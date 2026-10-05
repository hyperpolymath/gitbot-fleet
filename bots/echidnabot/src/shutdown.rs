// SPDX-License-Identifier: MPL-2.0
// Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//! Graceful-shutdown coordinator.
//!
//! On `SIGTERM` or `SIGINT` (Ctrl-C on POSIX) the daemon performs a
//! deterministic, bounded shutdown:
//!
//!   1. Flip a `CancellationToken` ("draining" mode). Webhooks stop
//!      accepting new requests; the scheduler loop stops dispatching new
//!      jobs.
//!   2. Stop the Axum HTTP server via `with_graceful_shutdown` — connections
//!      are drained, no new connections accepted.
//!   3. Wait for the scheduler's in-flight counter to reach 0, bounded by
//!      `shutdown_timeout_secs` (default 30s, env override
//!      `ECHIDNABOT_SHUTDOWN_TIMEOUT_SECS`).
//!   4. Call each registered shutdown hook in registration order
//!      (DB pool close, OTLP tracer flush, JSON-log buffer flush, ...).
//!   5. Return cleanly so the orchestrator never has to send `SIGKILL`.
//!
//! ## Why a coordinator pattern?
//!
//! Subsystems are created across multiple modules (`store`, `scheduler`,
//! observability — eventually the OpenTelemetry agent). A central
//! coordinator lets each subsystem register a hook at construction time
//! without the call site having to know the full ordering. The
//! coordinator owns the order: SCHEDULER drain → axum drain → hooks in
//! registration order → done.
//!
//! ## Coordination with other in-flight subsystems
//!
//! * **OpenTelemetry** — `main.rs` extracts an OTLP flush hook from
//!   [`crate::observability::TracerShutdown::into_coordinator_hook`] and
//!   registers it on the coordinator. When no OTLP endpoint is
//!   configured the hook is `None` and registration is skipped.
//! * **JSON-logging** — independent; will register its own buffer-flush
//!   hook here if it needs one (most JSON layers in
//!   `tracing-subscriber` are line-buffered and need no explicit flush).
//!
//! ## Why poll the scheduler instead of using a barrier?
//!
//! `JobScheduler` already tracks in-flight count via an `AtomicUsize`
//! (`running_count()`). Adding a `Notify` to wake on each completion is
//! cheap, but during shutdown a polling loop with a short interval is
//! simpler and avoids a new synchronisation surface. The poll is bounded
//! by the deadline so it always terminates.

use std::sync::Arc;
use std::time::Duration;

use tokio::sync::Notify;

use crate::scheduler::JobScheduler;

/// Default shutdown deadline (30s) — generous enough to let most proof
/// jobs finish a verification round but short enough that an orchestrator
/// (systemd, docker, k8s) won't escalate to SIGKILL first.
pub const DEFAULT_SHUTDOWN_TIMEOUT_SECS: u64 = 30;

/// Env var name for overriding the shutdown deadline at runtime.
pub const ENV_SHUTDOWN_TIMEOUT: &str = "ECHIDNABOT_SHUTDOWN_TIMEOUT_SECS";

/// Type-erased shutdown hook. Each subsystem (DB pool, tracer, log
/// buffer) registers one of these; the coordinator calls them in
/// registration order during the shutdown sequence.
///
/// Hooks must be `Send + 'static` and return a boxed future so the
/// coordinator can `await` them sequentially. They should be
/// idempotent — the coordinator guarantees one call per registration,
/// but a subsystem might be torn down twice in tests.
pub type ShutdownHook = Box<
    dyn FnOnce() -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>>
        + Send
        + 'static,
>;

/// Read the configured shutdown deadline.
///
/// Env (`ECHIDNABOT_SHUTDOWN_TIMEOUT_SECS`) wins over the config
/// argument so operators can repoint without redeploying. Negative or
/// unparseable values fall back to `cfg_secs`.
pub fn resolve_shutdown_timeout(cfg_secs: u64) -> Duration {
    let secs = std::env::var(ENV_SHUTDOWN_TIMEOUT)
        .ok()
        .and_then(|raw| raw.parse::<u64>().ok())
        .unwrap_or(cfg_secs);
    Duration::from_secs(secs)
}

/// Coordinator that owns the cancellation signal and the ordered list
/// of shutdown hooks. Construct once per process; share `signal()`
/// liberally; call `run()` from `main` on the signal future.
pub struct ShutdownCoordinator {
    /// Notified when shutdown begins. Subscribers should treat this as
    /// "stop accepting new work; existing work continues to completion
    /// until the deadline".
    notify: Arc<Notify>,
    /// Hooks run in registration order during the shutdown sequence.
    hooks: Vec<(String, ShutdownHook)>,
    /// Drain deadline. Polled when waiting for the scheduler to flush.
    timeout: Duration,
}

impl ShutdownCoordinator {
    /// Build a fresh coordinator. The deadline applies to the
    /// in-flight-job drain phase, not the full shutdown — hooks run
    /// after the drain regardless of how long the drain took.
    pub fn new(timeout: Duration) -> Self {
        Self {
            notify: Arc::new(Notify::new()),
            hooks: Vec::new(),
            timeout,
        }
    }

    /// Subscribe to the shutdown signal. Cheap to clone — multiple
    /// subsystems (axum, the scheduler dispatch loop, ad-hoc workers)
    /// can each `await` their own subscription independently.
    pub fn signal(&self) -> ShutdownSignal {
        ShutdownSignal {
            notify: self.notify.clone(),
        }
    }

    /// Register a subsystem hook. Hooks run in registration order
    /// during `run()`; pair the name with what the hook does so log
    /// output is greppable.
    pub fn register<F, Fut>(&mut self, name: impl Into<String>, hook: F)
    where
        F: FnOnce() -> Fut + Send + 'static,
        Fut: std::future::Future<Output = ()> + Send + 'static,
    {
        let boxed: ShutdownHook = Box::new(move || Box::pin(hook()));
        self.hooks.push((name.into(), boxed));
    }

    /// Trigger the shutdown signal without consuming the coordinator.
    /// Useful in tests; production normally relies on `wait_for_signal`.
    pub fn trigger(&self) {
        self.notify.notify_waiters();
    }

    /// Standalone trigger handle. Cheap to clone and move into a
    /// separate task (e.g. the signal-listener future) where holding a
    /// borrow on the coordinator would conflict with later `run()`.
    /// Calling `trigger()` on the returned handle fires the same
    /// signal that subscribers wake on.
    pub fn trigger_handle(&self) -> ShutdownTrigger {
        ShutdownTrigger {
            notify: self.notify.clone(),
        }
    }

    /// Drain the scheduler's in-flight job counter, bounded by the
    /// configured timeout. Returns `Ok(())` on clean drain or
    /// `Err(remaining)` when the deadline fires with jobs still running.
    pub async fn drain_scheduler(
        &self,
        scheduler: &JobScheduler,
    ) -> std::result::Result<(), usize> {
        let deadline = tokio::time::Instant::now() + self.timeout;
        let poll = Duration::from_millis(100);
        loop {
            let running = scheduler.running_count();
            if running == 0 {
                return Ok(());
            }
            if tokio::time::Instant::now() >= deadline {
                return Err(running);
            }
            tokio::time::sleep(poll).await;
        }
    }

    /// Execute the shutdown sequence: trigger the signal (so any
    /// subscriber that hasn't already noticed wakes up), drain the
    /// scheduler, then run all hooks in registration order.
    ///
    /// Returns the number of in-flight jobs that the drain timed out
    /// on (0 on a clean shutdown) so callers can log a warning.
    pub async fn run(mut self, scheduler: Option<Arc<JobScheduler>>) -> usize {
        // Step 1: notify all subscribers. axum's graceful_shutdown
        // future, the scheduler loop, webhook handlers — anyone holding
        // a ShutdownSignal — wakes up here.
        self.notify.notify_waiters();
        tracing::info!("Shutdown signal sent — entering drain phase");

        // Step 2: drain in-flight jobs (bounded). The scheduler is
        // optional because some entry points (CLI subcommands like
        // `check`) don't spawn one.
        let remaining = if let Some(s) = scheduler {
            match self.drain_scheduler(&s).await {
                Ok(()) => {
                    tracing::info!("Scheduler drained cleanly");
                    0
                }
                Err(left) => {
                    tracing::warn!(
                        in_flight = left,
                        timeout_secs = self.timeout.as_secs(),
                        "Shutdown drain deadline fired with jobs still in flight; proceeding to teardown anyway"
                    );
                    left
                }
            }
        } else {
            0
        };

        // Step 3: run hooks in registration order. We drain the Vec by
        // value so each FnOnce can be consumed; if a hook panics we log
        // and continue (a panicking tracer flush shouldn't prevent the
        // DB pool from closing).
        for (name, hook) in self.hooks.drain(..) {
            tracing::info!("Running shutdown hook: {}", name);
            let fut = hook();
            // Catch panics so one bad hook doesn't poison the rest.
            // AssertUnwindSafe is fine here — we don't reuse any state
            // after a panic, we just log and move on.
            let result = std::panic::AssertUnwindSafe(fut);
            // We can't use catch_unwind on a future directly without
            // futures-util's FutureExt; await first inside a
            // tokio::spawn so a panic gets isolated as a JoinError.
            let handle = tokio::spawn(async move {
                result.0.await;
            });
            if let Err(join_err) = handle.await {
                tracing::error!(hook = %name, error = %join_err, "Shutdown hook panicked");
            }
        }

        tracing::info!("Shutdown complete");
        remaining
    }
}

/// A clonable handle to the shutdown signal. Subscribers `await`
/// `triggered()` to be woken when shutdown begins.
#[derive(Clone)]
pub struct ShutdownSignal {
    notify: Arc<Notify>,
}

impl ShutdownSignal {
    /// Wait for the shutdown signal to fire. Returns immediately once
    /// `ShutdownCoordinator::run()` (or `trigger()`) has been called.
    pub async fn triggered(&self) {
        self.notify.notified().await;
    }
}

/// Standalone trigger handle. Use when the caller needs to fire the
/// shutdown signal from a context that cannot hold a borrow on the
/// `ShutdownCoordinator` itself (e.g. a moved-into-task future).
#[derive(Clone)]
pub struct ShutdownTrigger {
    notify: Arc<Notify>,
}

impl ShutdownTrigger {
    /// Wake every subscriber currently parked on `ShutdownSignal::triggered`.
    pub fn trigger(&self) {
        self.notify.notify_waiters();
    }
}

/// Wait for SIGTERM (Unix) or Ctrl-C (any platform) and return when
/// either fires. Used as the future passed to
/// `axum::serve(...).with_graceful_shutdown(...)` and as the await
/// point in `main` before `ShutdownCoordinator::run()`.
///
/// On non-Unix platforms only Ctrl-C is observed (SIGTERM isn't
/// portably available); echidnabot's deployment targets are Unix-only
/// in practice so this isn't a real gap.
pub async fn wait_for_termination() {
    #[cfg(unix)]
    {
        use tokio::signal::unix::{signal, SignalKind};

        // ctrl_c() under the hood installs a SIGINT handler — wire
        // both explicitly so the first one to fire wins.
        let mut term = match signal(SignalKind::terminate()) {
            Ok(s) => s,
            Err(e) => {
                tracing::warn!(
                    "Failed to install SIGTERM handler: {}; only SIGINT will trigger shutdown",
                    e
                );
                let _ = tokio::signal::ctrl_c().await;
                return;
            }
        };
        let mut int = match signal(SignalKind::interrupt()) {
            Ok(s) => s,
            Err(e) => {
                tracing::warn!(
                    "Failed to install SIGINT handler: {}; only SIGTERM will trigger shutdown",
                    e
                );
                let _ = term.recv().await;
                return;
            }
        };
        tokio::select! {
            _ = term.recv() => tracing::info!("SIGTERM received"),
            _ = int.recv() => tracing::info!("SIGINT received"),
        }
    }
    #[cfg(not(unix))]
    {
        let _ = tokio::signal::ctrl_c().await;
        tracing::info!("Ctrl-C received");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[tokio::test]
    async fn signal_fires_to_all_subscribers() {
        let coord = ShutdownCoordinator::new(Duration::from_secs(1));
        let sig_a = coord.signal();
        let sig_b = coord.signal();

        // Pre-arm both subscribers so notify_waiters reaches them.
        let a = tokio::spawn(async move { sig_a.triggered().await });
        let b = tokio::spawn(async move { sig_b.triggered().await });
        // Tiny yield so the spawned tasks register with the Notify.
        tokio::time::sleep(Duration::from_millis(10)).await;

        coord.trigger();
        tokio::time::timeout(Duration::from_millis(500), a)
            .await
            .expect("subscriber A must wake")
            .unwrap();
        tokio::time::timeout(Duration::from_millis(500), b)
            .await
            .expect("subscriber B must wake")
            .unwrap();
    }

    #[tokio::test]
    async fn hooks_run_in_registration_order() {
        let mut coord = ShutdownCoordinator::new(Duration::from_secs(1));
        let order = Arc::new(std::sync::Mutex::new(Vec::<u32>::new()));

        for i in 0..3u32 {
            let order = order.clone();
            coord.register(format!("hook-{}", i), move || async move {
                order.lock().unwrap().push(i);
            });
        }

        let timed_out = coord.run(None).await;
        assert_eq!(timed_out, 0);
        assert_eq!(*order.lock().unwrap(), vec![0, 1, 2]);
    }

    #[tokio::test]
    async fn drain_returns_immediately_when_no_jobs_in_flight() {
        let coord = ShutdownCoordinator::new(Duration::from_millis(500));
        let sched = Arc::new(JobScheduler::new(2, 10));
        let started = std::time::Instant::now();
        coord
            .drain_scheduler(&sched)
            .await
            .expect("drain must succeed on idle scheduler");
        assert!(
            started.elapsed() < Duration::from_millis(100),
            "drain on idle scheduler must be fast"
        );
    }

    #[tokio::test]
    async fn resolve_timeout_uses_env_when_set() {
        // Use a unique value so we can confirm env-source vs default.
        std::env::set_var(ENV_SHUTDOWN_TIMEOUT, "7");
        let t = resolve_shutdown_timeout(30);
        assert_eq!(t, Duration::from_secs(7));
        std::env::remove_var(ENV_SHUTDOWN_TIMEOUT);
    }

    #[tokio::test]
    async fn resolve_timeout_falls_back_when_env_unparseable() {
        std::env::set_var(ENV_SHUTDOWN_TIMEOUT, "not-a-number");
        let t = resolve_shutdown_timeout(42);
        assert_eq!(t, Duration::from_secs(42));
        std::env::remove_var(ENV_SHUTDOWN_TIMEOUT);
    }

    #[tokio::test]
    async fn panicking_hook_does_not_block_subsequent_hooks() {
        let mut coord = ShutdownCoordinator::new(Duration::from_secs(1));
        let after = Arc::new(AtomicUsize::new(0));

        coord.register("panic-hook", || async move {
            panic!("intentional test panic");
        });
        let after_clone = after.clone();
        coord.register("after-panic", move || async move {
            after_clone.fetch_add(1, Ordering::SeqCst);
        });

        let _ = coord.run(None).await;
        assert_eq!(
            after.load(Ordering::SeqCst),
            1,
            "hook after a panicking hook must still run"
        );
    }
}
