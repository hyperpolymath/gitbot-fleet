// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! echidnabot CLI and server entry point

use clap::{Parser, Subcommand};
use echidnabot::adapters::bitbucket::BitbucketAdapter;
use echidnabot::adapters::github::GitHubAdapter;
use echidnabot::adapters::gitlab::GitLabAdapter;
use echidnabot::adapters::{
    CheckConclusion, CheckRun, CheckStatus as AdapterCheckStatus, Platform, PlatformAdapter, PrId,
    RepoId,
};
use echidnabot::api::graphql::GraphQLState;
use echidnabot::api::{create_schema, webhook_router};
use echidnabot::dispatcher::echidna_client::ProverStatus;
use echidnabot::dispatcher::{EchidnaClient, ProofResult, ProofStatus, ProverKind};
use echidnabot::feedback::corpus_delta::{CorpusDelta, DeltaRow, DeltaSource};
use echidnabot::modes::{self, BotMode, ModeSelector};
use echidnabot::result_formatter;
use echidnabot::scheduler::{JobScheduler, ProofJob};
use echidnabot::shutdown::{
    resolve_shutdown_timeout, wait_for_termination, ShutdownCoordinator, ShutdownSignal,
};
use echidnabot::store::models::goal_fingerprint;
use echidnabot::store::models::{
    ProofResultRecord, Repository as StoreRepository, TacticOutcomeRecord,
};
use echidnabot::store::{SqliteStore, Store};
use echidnabot::{Config, Result};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;
use tokio::fs;
use tokio::time::{sleep, Duration};

#[derive(Parser)]
#[command(name = "echidnabot")]
#[command(about = "Proof-aware CI bot for theorem prover repositories")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Path to configuration file
    #[arg(short, long, default_value = "echidnabot.toml")]
    config: String,

    /// Enable verbose logging
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Start the webhook server
    Serve {
        /// Host to bind to. If omitted, reads `[server].host` from the TOML
        /// config (which itself defaults to `0.0.0.0` if unset there).
        #[arg(short = 'H', long)]
        host: Option<String>,

        /// Port to bind to. If omitted, reads `[server].port` from the TOML
        /// config (which itself defaults to `8080` if unset there).
        #[arg(short, long)]
        port: Option<u16>,
    },

    /// Register a repository for monitoring
    Register {
        /// Repository in format owner/name
        #[arg(short, long)]
        repo: String,

        /// Platform (github, gitlab, bitbucket)
        #[arg(short, long, default_value = "github")]
        platform: String,

        /// Provers to enable (comma-separated)
        #[arg(long, default_value = "metamath")]
        provers: String,

        /// Bot operating mode for this repo. Overrides the daemon-wide
        /// default but is itself overridden by a target-repo directive
        /// at `.machine_readable/bot_directives/echidnabot.a2ml`.
        ///
        /// One of: `verifier`, `advisor`, `consultant`, `regulator`.
        #[arg(short, long, default_value = "verifier")]
        mode: String,

        /// Regulator-mode coverage threshold (percent, 0..=100). The
        /// merge gate releases when proven_count * 100 / total_count >=
        /// this value. 100 = "every proof must pass" (strictest);
        /// lower values tolerate flake during incremental coverage growth.
        /// Ignored for non-Regulator modes. Default: 100.
        #[arg(long, default_value = "100", value_parser = clap::value_parser!(u8))]
        regulator_threshold: u8,
    },

    /// Manually trigger a proof check
    Check {
        /// Repository in format owner/name
        #[arg(short, long)]
        repo: String,

        /// Commit SHA (defaults to HEAD)
        #[arg(short, long)]
        commit: Option<String>,

        /// Specific prover to use
        #[arg(short, long)]
        prover: Option<String>,
    },

    /// Show status of a repository or job
    Status {
        /// Repository in format owner/name, or job ID
        #[arg(short, long)]
        target: String,
    },

    /// Initialize the database
    InitDb,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Load config first — the OTLP endpoint may come from [observability]
    // in echidnabot.toml. The env var (OTEL_EXPORTER_OTLP_ENDPOINT) wins
    // over the TOML value (see ObservabilityConfig::resolved_endpoint).
    //
    // We can't honour --verbose via tracing-subscriber's `EnvFilter::try_new`
    // *and* respect RUST_LOG simultaneously without ordering games; the
    // observability module honours RUST_LOG via EnvFilter::try_from_default_env
    // and we fall back to "info" / "debug" if unset.
    //
    // Output format is selected by `ECHIDNABOT_LOG_FORMAT` (text|json) —
    // see `observability::LogFormat::from_env`.
    if cli.verbose && std::env::var("RUST_LOG").is_err() {
        // Make --verbose meaningful when the operator hasn't explicitly
        // set RUST_LOG. Setting it before init_tracing lets EnvFilter
        // pick it up naturally. Safe on edition 2021 (the unsafe wrap
        // is a 2024-edition-only requirement); single-threaded startup
        // anyway — no tokio runtime yet, no other threads spawned.
        std::env::set_var("RUST_LOG", "debug");
    }

    let config = Config::load(&cli.config)?;

    // Initialise tracing via the observability module. Returns a
    // TracerShutdown handle we must keep alive until the application
    // exits — dropping it flushes pending spans.
    let otlp_endpoint = config.observability.resolved_endpoint();
    if let Some(ref endpoint) = otlp_endpoint {
        // Log via plain eprintln since the subscriber isn't installed yet.
        eprintln!("Initialising OpenTelemetry OTLP exporter → {endpoint}");
    }
    let mut tracer_guard = echidnabot::observability::init_tracing(otlp_endpoint, false)
        .map_err(|e| echidnabot::Error::Config(format!("tracing init failed: {e}")))?;

    let result = match cli.command {
        Commands::Serve { host, port } => {
            // CLI flag wins; otherwise honour the TOML [server] section.
            let host = host.unwrap_or_else(|| config.server.host.clone());
            let port = port.unwrap_or(config.server.port);
            tracing::info!("Starting echidnabot server on {}:{}", host, port);
            // Hand the OTLP flush over to the shutdown coordinator so that
            // signal-driven graceful shutdown flushes spans inside its
            // drain phase (closes echidnabot#71). When no OTLP endpoint
            // was configured, the hook is `None` and the coordinator
            // skips registration.
            let tracer_hook = tracer_guard.into_coordinator_hook();
            serve(&config, &host, port, tracer_hook).await
        }
        Commands::Register {
            repo,
            platform,
            provers,
            mode,
            regulator_threshold,
        } => {
            tracing::info!(
                "Registering {} on {} with provers: {} (mode: {}, regulator_threshold: {})",
                repo,
                platform,
                provers,
                mode,
                regulator_threshold,
            );
            register(
                &config,
                &repo,
                &platform,
                &provers,
                &mode,
                regulator_threshold,
            )
            .await
        }
        Commands::Check {
            repo,
            commit,
            prover,
        } => {
            tracing::info!("Triggering check for {} at {:?}", repo, commit);
            check(&config, &repo, commit.as_deref(), prover.as_deref()).await
        }
        Commands::Status { target } => {
            tracing::info!("Getting status for {}", target);
            status(&config, &target).await
        }
        Commands::InitDb => {
            tracing::info!("Initializing database");
            init_db(&config).await
        }
    };

    // Flush any in-flight OTel spans before the process exits.
    //
    // For `serve`, the OTLP provider was already moved into the shutdown
    // coordinator via `into_coordinator_hook()`, so this call is a no-op
    // (provider is `None`). For other CLI subcommands which do not run a
    // coordinator, this is the only flush — the explicit `shutdown()`
    // gives us a chance to surface errors that `Drop` would silently log.
    tracer_guard.shutdown();

    result
}

/// OTLP-flush coordinator hook type, as produced by
/// `TracerShutdown::into_coordinator_hook()`. Used by `serve` to wire
/// the OpenTelemetry flush into the graceful-shutdown drain phase.
type TracerFlushHook = Box<
    dyn FnOnce() -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send + 'static>>
        + Send
        + 'static,
>;

async fn serve(
    config: &Config,
    host: &str,
    port: u16,
    tracer_hook: Option<TracerFlushHook>,
) -> Result<()> {
    use async_graphql_axum::{GraphQLRequest, GraphQLResponse};
    use axum::{routing::get, routing::post, Extension, Router};

    // Webhook signature verification is per-integration (handled in
    // src/api/webhooks.rs). When no `webhook_secret` is configured for a
    // platform, the receiver still returns 200 on POST — fine for local
    // testing but unsafe in any deployment reachable from a network.
    // Surface the gap loudly at startup so an operator can't miss it.
    let gh_unsecured = config
        .github
        .as_ref()
        .is_some_and(|g| g.webhook_secret.is_none());
    let gl_unsecured = config
        .gitlab
        .as_ref()
        .is_some_and(|g| g.webhook_secret.is_none());
    if config.github.is_none() && config.gitlab.is_none() {
        tracing::warn!(
            "No [github] / [gitlab] integration configured. \
             /webhooks/* endpoints accept ANY payload with no signature \
             verification. Acceptable for local testing only."
        );
    } else {
        if gh_unsecured {
            tracing::warn!(
                "[github].webhook_secret not set — /webhooks/github accepts \
                 any POST without HMAC verification. Set webhook_secret in \
                 echidnabot.toml before exposing this daemon."
            );
        }
        if gl_unsecured {
            tracing::warn!(
                "[gitlab].webhook_secret not set — /webhooks/gitlab accepts \
                 any POST without signature verification."
            );
        }
    }

    let store = Arc::new(SqliteStore::new(&config.database.url).await?);
    let scheduler = Arc::new(JobScheduler::new(
        config.scheduler.max_concurrent,
        config.scheduler.queue_size,
    ));
    let echidna = Arc::new(EchidnaClient::new(&config.echidna));

    let graphql_state = GraphQLState {
        store: store.clone(),
        scheduler: scheduler.clone(),
        echidna: echidna.clone(),
    };
    let schema = create_schema(graphql_state);

    let rate_limiter = config.server.rate_limit_rpm.map(|rpm| {
        tracing::info!(
            "Webhook rate limiting enabled: {} requests/minute per IP",
            rpm
        );
        Arc::new(echidnabot::api::rate_limit::WebhookRateLimiter::new(rpm))
    });
    if rate_limiter.is_none() {
        tracing::warn!("Webhook rate limiting is disabled — set [server] rate_limit_rpm to enable");
    }

    let app_state = echidnabot::api::webhooks::AppState {
        config: Arc::new(config.clone()),
        store: store.clone(),
        scheduler: scheduler.clone(),
        rate_limiter,
        mode_selector: ModeSelector::new(config.bot.mode),
    };

    let app = Router::new()
        .route("/health", get(health))
        .route("/metrics", get(metrics))
        .route("/", get(root))
        .route(
            "/graphql",
            post(
                |Extension(schema): Extension<echidnabot::api::graphql::EchidnabotSchema>,
                 req: GraphQLRequest| async move {
                    GraphQLResponse::from(schema.execute(req.into_inner()).await)
                },
            )
            .get(graphql_playground),
        )
        .merge(webhook_router(app_state.clone()))
        .layer(Extension(schema))
        .with_state(app_state.clone());

    // ── Graceful-shutdown wiring ─────────────────────────────────────────
    //
    // The coordinator owns the shutdown sequence:
    //   1. Signal fires (SIGTERM / SIGINT).
    //   2. axum stops accepting new connections (its
    //      `with_graceful_shutdown` future returns).
    //   3. The scheduler loop sees the signal and stops dispatching.
    //   4. The coordinator drains in-flight jobs (bounded by
    //      `lifecycle.shutdown_timeout_secs` / env override).
    //   5. Registered hooks run in order: DB close → tracer flush.
    //
    // Env override (`ECHIDNABOT_SHUTDOWN_TIMEOUT_SECS`) wins over the TOML
    // value so operators can adjust without redeploying. See
    // `src/shutdown.rs` for the full design.
    let timeout = resolve_shutdown_timeout(config.lifecycle.shutdown_timeout_secs);
    let mut coordinator = ShutdownCoordinator::new(timeout);
    let scheduler_signal = coordinator.signal();
    let axum_signal = coordinator.signal();
    // Standalone trigger handle for the signal-listener task; using a
    // separate handle avoids capturing the coordinator by move (which
    // would conflict with the later `coordinator.run()` call).
    let signal_trigger = coordinator.trigger_handle();
    let error_trigger = coordinator.trigger_handle();

    // Register subsystem hooks. Order matters: DB closes after the
    // scheduler has drained (set up by the coordinator's run() phase
    // ordering), and tracer-flush runs last so any final shutdown logs
    // are emitted before the buffer is torn down.
    let store_for_hook = store.clone();
    coordinator.register("db-pool-close", move || async move {
        store_for_hook.close().await;
        tracing::info!("DB pool closed");
    });
    // Wire the real OTLP flush into the drain phase. When no endpoint
    // was configured at init time, `tracer_hook` is `None` and we skip
    // registration — no point burning a hook slot on a no-op.
    if let Some(hook) = tracer_hook {
        coordinator.register("tracer-flush", hook);
    }

    tokio::spawn(run_scheduler_loop(
        scheduler.clone(),
        store.clone(),
        echidna.clone(),
        app_state.config.clone(),
        scheduler_signal,
    ));

    let listener = tokio::net::TcpListener::bind(format!("{}:{}", host, port)).await?;
    tracing::info!(
        "Listening on http://{}:{} (shutdown timeout: {}s)",
        host,
        port,
        timeout.as_secs()
    );

    // Spawn the signal listener as a side task. When SIGTERM/SIGINT
    // arrives it pulls the coordinator's trigger; axum's
    // with_graceful_shutdown wakes; the scheduler loop wakes; we then
    // await the server's natural drain.
    tokio::spawn(async move {
        wait_for_termination().await;
        signal_trigger.trigger();
    });

    // Run axum with graceful-shutdown wired into the coordinator's
    // signal. `axum::serve(...).await` returns AFTER the shutdown
    // future has fired AND all in-flight HTTP connections have
    // drained — so by the time we get past this await the HTTP plane
    // is fully quiesced.
    let serve_result = axum::serve(
        listener,
        app.into_make_service_with_connect_info::<std::net::SocketAddr>(),
    )
    .with_graceful_shutdown(async move {
        axum_signal.triggered().await;
        tracing::info!("Axum graceful shutdown triggered — draining HTTP connections");
    })
    .await;
    if let Err(e) = serve_result {
        tracing::error!("axum::serve error: {}", e);
        // Server died without a signal — fire shutdown so hooks still
        // run and the process exits cleanly.
        error_trigger.trigger();
    } else {
        tracing::info!("HTTP server drained cleanly");
    }

    // Drain in-flight jobs + run subsystem hooks. `run()` consumes the
    // coordinator and returns the count of jobs that timed out (0 on
    // a clean shutdown).
    let remaining = coordinator.run(Some(scheduler.clone())).await;
    if remaining > 0 {
        tracing::warn!(
            "{} job(s) were still in flight when shutdown deadline fired",
            remaining
        );
    }
    Ok(())
}

async fn graphql_playground() -> &'static str {
    r#"<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>echidnabot GraphQL</title>
    <link rel="stylesheet" href="https://unpkg.com/@graphql-playground/react/build/static/css/index.css" />
    <link rel="shortcut icon" href="https://raw.githubusercontent.com/graphql/graphql-playground/master/packages/graphql-playground-react/public/favicon.png" />
    <script src="https://unpkg.com/@graphql-playground/react/build/static/js/middleware.js"></script>
  </head>
  <body>
    <div id="root"></div>
    <script>
      window.addEventListener("load", function () {
        GraphQLPlayground.init(document.getElementById("root"), { endpoint: "/graphql" });
      });
    </script>
  </body>
</html>"#
}

async fn health() -> &'static str {
    "OK"
}

/// Prometheus-compatible text exposition of key counters.
///
/// Exposes scheduler queue depth and build metadata. Full Prometheus
/// integration (using `prometheus` or `metrics-exporter-prometheus` crates)
/// is a future hardening item; this endpoint provides the shape and format
/// that operators expect so dashboards and alerts can be wired now.
async fn metrics(
    axum::extract::State(state): axum::extract::State<echidnabot::api::webhooks::AppState>,
) -> (axum::http::StatusCode, String) {
    let queued = state.scheduler.queue_depth();
    let running = state.scheduler.running_count();
    let body = format!(
        "# HELP echidnabot_jobs_queued Number of jobs waiting in the proof queue\n\
         # TYPE echidnabot_jobs_queued gauge\n\
         echidnabot_jobs_queued {queued}\n\
         # HELP echidnabot_jobs_running Number of jobs currently being verified\n\
         # TYPE echidnabot_jobs_running gauge\n\
         echidnabot_jobs_running {running}\n\
         # HELP echidnabot_build_info Static build metadata\n\
         # TYPE echidnabot_build_info gauge\n\
         echidnabot_build_info{{version=\"{version}\"}} 1\n",
        queued = queued,
        running = running,
        version = env!("CARGO_PKG_VERSION"),
    );
    (axum::http::StatusCode::OK, body)
}

async fn root() -> &'static str {
    "echidnabot - Proof-aware CI bot\n\nEndpoints:\n  GET  /health\n  GET  /graphql\n  POST /graphql\n  POST /webhooks/github\n  POST /webhooks/gitlab\n  POST /webhooks/bitbucket"
}

async fn register(
    config: &Config,
    repo: &str,
    platform: &str,
    provers: &str,
    mode: &str,
    regulator_threshold: u8,
) -> Result<()> {
    let store = SqliteStore::new(&config.database.url).await?;
    let platform = parse_platform(platform)?;
    let (owner, name) = split_repo_name(repo)?;

    let mut repo_record = StoreRepository::new(platform, owner, name);
    let enabled = parse_prover_list(provers)?;
    if !enabled.is_empty() {
        repo_record.enabled_provers = enabled;
    }

    // Parse the mode flag — accepts the four lowercase strings.
    repo_record.mode = serde_json::from_value(serde_json::Value::String(mode.to_lowercase()))
        .map_err(|_| {
            echidnabot::Error::Config(format!(
                "unknown mode '{}': expected one of verifier, advisor, consultant, regulator",
                mode
            ))
        })?;

    // Clamp threshold to 0..=100 (clap's u8 parser already enforces u8
    // bounds, but we don't want 200% to silently become valid here).
    repo_record.regulator_coverage_threshold = regulator_threshold.min(100);

    store.create_repository(&repo_record).await?;
    tracing::info!(
        "Registered repository {} on {:?} in {} mode (regulator threshold {}%)",
        repo_record.full_name(),
        repo_record.platform,
        repo_record.mode,
        repo_record.regulator_coverage_threshold,
    );
    Ok(())
}

async fn check(
    config: &Config,
    repo: &str,
    commit: Option<&str>,
    prover: Option<&str>,
) -> Result<()> {
    let client = EchidnaClient::new(&config.echidna);
    let health = client.health_check().await?;
    tracing::info!(
        "ECHIDNA health check: {}",
        if health { "ok" } else { "unhealthy" }
    );

    if !health {
        tracing::warn!("ECHIDNA reported unhealthy; results may be unreliable");
    }

    let repo_path = Path::new(repo);
    let (proof_content, inferred_prover) = if repo_path.is_file() {
        let content = fs::read_to_string(repo_path).await?;
        let detected = detect_prover_from_filename(repo_path);
        (Some(content), detected)
    } else {
        (None, None)
    };

    let selected_prover = prover.and_then(parse_prover_arg).or(inferred_prover);

    if let Some(ref kind) = selected_prover {
        let status = client.prover_status(kind).await?;
        tracing::info!(
            "Prover {} status: {}",
            kind.display_name(),
            format_prover_status(status)
        );
    }

    if let Some(content) = proof_content {
        let kind = selected_prover.unwrap_or_else(|| ProverKind::new("metamath"));
        let result = client.verify_proof(&kind, &content).await?;
        tracing::info!(
            "Proof result: {:?} ({} ms)",
            result.status,
            result.duration_ms
        );
        tracing::info!("Message: {}", result.message);
        if !result.prover_output.trim().is_empty() {
            tracing::info!("Prover output:\n{}", result.prover_output.trim());
        }
        if !result.artifacts.is_empty() {
            tracing::info!("Artifacts: {}", result.artifacts.join(", "));
        }
        if let Some(commit) = commit {
            tracing::info!("Checked commit {}", commit);
        }
    } else {
        tracing::warn!(
            "Repo '{}' is not a proof file; pass a local proof file path to run verification",
            repo
        );
    }

    Ok(())
}

fn parse_prover_arg(prover: &str) -> Option<ProverKind> {
    match prover.to_lowercase().as_str() {
        "agda" => Some(ProverKind::new("agda")),
        "coq" | "rocq" => Some(ProverKind::new("coq")),
        "lean" | "lean4" => Some(ProverKind::new("lean")),
        "isabelle" | "isabelle-hol" | "isabelle_hol" => Some(ProverKind::new("isabelle")),
        "z3" => Some(ProverKind::new("z3")),
        "cvc5" => Some(ProverKind::new("cvc5")),
        "metamath" => Some(ProverKind::new("metamath")),
        "hol-light" | "hol_light" | "hol" => Some(ProverKind::new("hol-light")),
        "mizar" => Some(ProverKind::new("mizar")),
        "pvs" => Some(ProverKind::new("pvs")),
        "acl2" => Some(ProverKind::new("acl2")),
        "hol4" => Some(ProverKind::new("hol4")),
        _ => None,
    }
}

fn detect_prover_from_filename(path: &Path) -> Option<ProverKind> {
    let filename = path.file_name()?.to_str()?.to_lowercase();
    ProverKind::all().find(|prover| {
        prover
            .file_extensions()
            .iter()
            .any(|ext| filename.ends_with(ext))
    })
}

fn format_prover_status(status: ProverStatus) -> &'static str {
    match status {
        ProverStatus::Available => "available",
        ProverStatus::Degraded => "degraded",
        ProverStatus::Unavailable => "unavailable",
        ProverStatus::Unknown => "unknown",
    }
}

fn parse_platform(platform: &str) -> Result<Platform> {
    match platform.to_lowercase().as_str() {
        "github" => Ok(Platform::GitHub),
        "gitlab" => Ok(Platform::GitLab),
        "bitbucket" => Ok(Platform::Bitbucket),
        "codeberg" => Ok(Platform::Codeberg),
        _ => Err(echidnabot::Error::Config(format!(
            "Unknown platform '{}'",
            platform
        ))),
    }
}

fn split_repo_name(repo: &str) -> Result<(String, String)> {
    let mut parts = repo.splitn(2, '/');
    let owner = parts.next().unwrap_or_default().to_string();
    let name = parts.next().unwrap_or_default().to_string();
    if owner.is_empty() || name.is_empty() {
        return Err(echidnabot::Error::Config(
            "Repo must be in owner/name format".to_string(),
        ));
    }
    Ok((owner, name))
}

fn parse_prover_list(provers: &str) -> Result<Vec<ProverKind>> {
    let mut results = Vec::new();
    for prover in provers.split(',').map(str::trim).filter(|p| !p.is_empty()) {
        match parse_prover_arg(prover) {
            Some(kind) => results.push(kind),
            None => {
                return Err(echidnabot::Error::InvalidProver(prover.to_string()));
            }
        }
    }
    Ok(results)
}

async fn status(config: &Config, target: &str) -> Result<()> {
    let store = SqliteStore::new(&config.database.url).await?;

    if let Ok(job_id) = uuid::Uuid::parse_str(target) {
        if let Some(job) = store.get_job(echidnabot::scheduler::JobId(job_id)).await? {
            tracing::info!(
                "Job {} repo={} commit={} prover={:?} status={:?}",
                job.id,
                job.repo_id,
                job.commit_sha,
                job.prover,
                job.status
            );
            return Ok(());
        }
    }

    if let Ok((owner, name)) = split_repo_name(target) {
        if let Some(repo) = store
            .get_repository_by_name(Platform::GitHub, &owner, &name)
            .await?
        {
            tracing::info!(
                "Repository {} enabled={} last_checked={:?}",
                repo.full_name(),
                repo.enabled,
                repo.last_checked_commit
            );
            let jobs = store.list_jobs_for_repo(repo.id, 20).await?;
            tracing::info!("Recent jobs: {}", jobs.len());
            return Ok(());
        }
    }

    tracing::warn!("No matching job or repository found for '{}'", target);
    Ok(())
}

async fn init_db(config: &Config) -> Result<()> {
    let _store = SqliteStore::new(&config.database.url).await?;
    tracing::info!("Database initialized");
    Ok(())
}

async fn run_scheduler_loop(
    scheduler: Arc<JobScheduler>,
    store: Arc<dyn Store>,
    echidna: Arc<EchidnaClient>,
    config: Arc<Config>,
    shutdown: ShutdownSignal,
) {
    // Pin a single shutdown future for the loop. Each iteration races
    // it against the next-poll wait so an idle scheduler exits promptly
    // when the signal fires. In-flight jobs are NOT cancelled — they
    // continue in their own task scope; the coordinator's drain phase
    // waits for the in-flight counter to reach 0 (bounded by the
    // configured deadline).
    let shutdown_fut = shutdown.triggered();
    tokio::pin!(shutdown_fut);
    loop {
        if let Some(job) = scheduler.try_start_next().await {
            if let Err(err) = mark_job_running(store.as_ref(), &job).await {
                tracing::warn!("Failed to mark job {} running: {}", job.id, err);
            }

            let result = match process_job(&job, store.as_ref(), echidna.as_ref(), &config).await {
                Ok(result) => result,
                Err(err) => {
                    tracing::error!("Job {} failed: {}", job.id, err);
                    echidnabot::scheduler::JobResult {
                        success: false,
                        message: err.to_string(),
                        prover_output: String::new(),
                        duration_ms: 0,
                        verified_files: vec![],
                        failed_files: vec![],
                        confidence: None,
                        axioms: None,
                    }
                }
            };

            if let Err(err) = finalize_job(store.as_ref(), &job, &result).await {
                tracing::warn!("Failed to finalize job {}: {}", job.id, err);
            }

            // Phase 2b: record double-loop feedback — tactic outcomes + corpus delta.
            // Best-effort: errors are logged and swallowed so they never stall the
            // scheduler. Both writes are gated by `config.corpus.enabled`.
            record_feedback(&job, &result, store.clone(), &config).await;

            // Phase 3: report the outcome back to the originating platform
            // (check run + optional PR comment) per the resolved bot mode.
            // Errors here are logged but never block the scheduler — the DB
            // is the source of truth, and a missing GitHub token / 503 from
            // the platform shouldn't cascade.
            if let Err(err) =
                report_to_platform(store.clone(), echidna.as_ref(), &config, &job, &result).await
            {
                tracing::warn!("Platform report skipped for job {}: {}", job.id, err);
            }

            scheduler.complete_job(job.id, result).await;
        } else {
            // Idle — wait briefly for either the next polling tick or
            // the shutdown signal. Whichever fires first wins; on
            // shutdown we return immediately rather than burning
            // another 250ms before noticing.
            tokio::select! {
                _ = sleep(Duration::from_millis(250)) => {}
                _ = &mut shutdown_fut => {
                    tracing::info!("Scheduler loop observed shutdown signal — stopping dispatch");
                    return;
                }
            }
        }
    }
}

/// Phase 3: post a job's outcome back to the originating platform.
///
/// Cascade:
///   1. Look up the repository row to recover platform + bot mode.
///   2. Resolve the effective mode via `modes::resolve_mode` (directive
///      content is None until the executor lands a clone-and-read step).
///   3. Build the platform-appropriate adapter.
///   4. Translate the `JobResult` into a `ProofResult` for the formatter,
///      then format per-mode.
///   5. Always create a check run; comment on the originating PR for
///      modes that opt in (Advisor / Consultant / Regulator).
///
/// All steps are best-effort. Errors are surfaced to the caller (which
/// logs but does not propagate them), so a 503 from GitHub or a missing
/// token never blocks the scheduler.
async fn report_to_platform(
    store: Arc<dyn Store>,
    echidna: &EchidnaClient,
    config: &Config,
    job: &ProofJob,
    job_result: &echidnabot::scheduler::JobResult,
) -> Result<()> {
    let repo = match store.get_repository(job.repo_id).await? {
        Some(r) => r,
        None => return Ok(()), // Repo deleted between enqueue + completion
    };

    // Cascade: target-repo directive (fetched via PlatformAdapter) →
    // DB column → Verifier default. Directive fetch is best-effort —
    // API errors return None and the cascade falls through.
    let directive_adapter = echidnabot::adapters::build_adapter(config, repo.platform).ok();
    let directive_content = if let Some(ref adapter) = directive_adapter {
        let api_repo_id = RepoId {
            platform: repo.platform,
            owner: repo.owner.clone(),
            name: repo.name.clone(),
        };
        modes::fetch_directive_via_adapter(adapter.as_ref(), &api_repo_id, None).await
    } else {
        None
    };
    let mode = modes::resolve_mode_with_daemon_default(
        &repo,
        directive_content.as_deref(),
        config.bot.mode,
    );

    // Verifier mode is silent on PRs but still posts a check run.
    let proof_result = ProofResult {
        status: if job_result.success {
            ProofStatus::Verified
        } else {
            ProofStatus::Failed
        },
        message: job_result.message.clone(),
        prover_output: job_result.prover_output.clone(),
        duration_ms: job_result.duration_ms,
        artifacts: vec![],
        confidence: job_result.confidence.clone(),
        axioms: job_result.axioms.clone(),
    };

    // Tactic suggestions for Advisor / Consultant / Regulator. Verifier
    // doesn't show suggestions, so we skip the network round-trip there.
    // Suggestions are reranked through the local feedback store
    // (Package 7b-2 Reranker) so historical success informs the order.
    let suggestions = if matches!(
        mode,
        BotMode::Advisor | BotMode::Consultant | BotMode::Regulator
    ) && !job_result.success
    {
        // Use prover_output as the goal-state proxy — it typically
        // contains the unproven goal in failure context. Imperfect
        // but the closest signal available without re-reading the
        // proof file. Truncate to keep ECHIDNA's prompt budget bounded.
        let goal_state = if job_result.prover_output.len() > 2000 {
            &job_result.prover_output[..2000]
        } else {
            &job_result.prover_output
        };
        match echidna.suggest_tactics(&job.prover, "", goal_state).await {
            Ok(raw) if !raw.is_empty() => {
                let reranker = echidnabot::feedback::Reranker::new(store.clone());
                match reranker.rerank(&job.prover, goal_state, raw).await {
                    Ok(reranked) => reranked.into_iter().take(5).collect(),
                    Err(e) => {
                        tracing::debug!("Reranker error ({}); using raw suggestions", e);
                        vec![]
                    }
                }
            }
            Ok(_) => vec![],
            Err(e) => {
                tracing::debug!(
                    "ECHIDNA suggest_tactics unavailable ({}); skipping suggestions",
                    e
                );
                vec![]
            }
        }
    } else {
        vec![]
    };

    let formatted =
        result_formatter::format_proof_result(mode, &proof_result, job.prover.clone(), suggestions);

    let repo_id = RepoId {
        platform: repo.platform,
        owner: repo.owner.clone(),
        name: repo.name.clone(),
    };

    // For Regulator mode, compute per-commit coverage now so the
    // threshold check (Bit 5b) can override the simple block-on-any-failure
    // path. Coverage is a running tally — each job that finalizes sees the
    // most recent counts, including its own contribution if save_result
    // already ran (it did, in finalize_job above).
    let coverage_for_regulator = if matches!(mode, BotMode::Regulator) {
        store.commit_coverage(repo.id, &job.commit_sha).await.ok()
    } else {
        None
    };

    let conclusion = match formatted.check_status {
        echidnabot::modes::CheckStatus::Success => CheckConclusion::Success,
        echidnabot::modes::CheckStatus::Failure => match mode {
            BotMode::Regulator => {
                // Block merge only when overall coverage is below the
                // configured threshold; tolerate single-job flake when
                // the rest of the commit is solid.
                if let Some(c) = coverage_for_regulator {
                    if c.percent() >= repo.regulator_coverage_threshold {
                        CheckConclusion::Neutral
                    } else {
                        CheckConclusion::Failure
                    }
                } else {
                    // Couldn't compute coverage — fall back to strict
                    // block-on-any-failure to be safe.
                    CheckConclusion::Failure
                }
            }
            _ => CheckConclusion::Neutral,
        },
        echidnabot::modes::CheckStatus::Neutral => CheckConclusion::Neutral,
    };

    // Augment the per-mode summary with coverage detail for Regulator,
    // so the GitHub Checks UI shows the threshold context inline.
    let mut summary = result_formatter::check_run_summary(&formatted, mode);
    if let Some(c) = coverage_for_regulator {
        summary.push_str(&format!(
            "\n\nCoverage: **{}/{}** ({}%) vs threshold **{}%** — {}",
            c.proven,
            c.total,
            c.percent(),
            repo.regulator_coverage_threshold,
            if c.percent() >= repo.regulator_coverage_threshold {
                "passing"
            } else {
                "below threshold; merge blocked"
            },
        ));
    }

    let check = CheckRun {
        name: format!("echidnabot/{:?}", job.prover),
        head_sha: job.commit_sha.clone(),
        status: AdapterCheckStatus::Completed {
            conclusion,
            summary,
        },
        details_url: None,
    };

    let adapter = echidnabot::adapters::build_adapter(config, repo.platform)?;

    if let Err(err) = adapter.create_check_run(&repo_id, check).await {
        tracing::warn!(
            "create_check_run failed for {} (mode {}): {}",
            repo.full_name(),
            mode,
            err
        );
        // Don't return — comment may still succeed.
    }

    // Modes that want PR comments: Advisor (suggestions), Consultant
    // (inline review comment on offending line), Regulator (block notice).
    // Verifier stays silent.
    let wants_comment = matches!(
        mode,
        BotMode::Advisor | BotMode::Consultant | BotMode::Regulator
    );
    if wants_comment {
        if let Some(pr_number) = job.pr_number {
            let mut body = result_formatter::generate_pr_comment(&formatted, mode);
            // For Regulator, append the coverage stanza so the PR comment
            // tells the reviewer exactly where the commit sits relative to
            // the configured threshold.
            if let Some(c) = coverage_for_regulator {
                body.push_str(&format!(
                    "\n\n### 🎯 Coverage gate\n\n\
                     Provers passing: **{}/{}** (**{}%**)  \n\
                     Threshold: **{}%**  \n\
                     Status: **{}**\n",
                    c.proven,
                    c.total,
                    c.percent(),
                    repo.regulator_coverage_threshold,
                    if c.percent() >= repo.regulator_coverage_threshold {
                        "✅ passing"
                    } else {
                        "🚫 below threshold — merge blocked"
                    },
                ));
            }
            let pr_id = PrId(pr_number.to_string());

            // Consultant mode: attempt an inline review comment on the first
            // failing proof file so the annotation lands next to the code.
            // Falls back to a general PR comment when the file is not in the
            // diff or when the adapter returns an error (e.g. GitLab/Bitbucket
            // stubs that always fall back internally).
            let comment_result = if mode == BotMode::Consultant {
                if let Some(failed_file) = job_result.failed_files.first() {
                    let location = echidnabot::adapters::ReviewCommentLocation {
                        commit_sha: job.commit_sha.clone(),
                        path: failed_file.clone(),
                        line: extract_error_line(&job_result.prover_output).unwrap_or(1),
                    };
                    match adapter
                        .create_review_comment(&repo_id, pr_id.clone(), &body, location)
                        .await
                    {
                        Ok(id) => Ok(id),
                        Err(review_err) => {
                            tracing::debug!(
                                "Review comment failed for {} PR #{} ({}); falling back to PR comment",
                                repo.full_name(), pr_number, review_err
                            );
                            adapter.create_comment(&repo_id, pr_id, &body).await
                        }
                    }
                } else {
                    adapter.create_comment(&repo_id, pr_id, &body).await
                }
            } else {
                adapter.create_comment(&repo_id, pr_id, &body).await
            };

            if let Err(err) = comment_result {
                tracing::warn!(
                    "create_comment failed for {} PR #{} (mode {}): {}",
                    repo.full_name(),
                    pr_number,
                    mode,
                    err
                );
            }
        }
    }

    Ok(())
}

async fn mark_job_running(store: &dyn Store, job: &ProofJob) -> Result<()> {
    let mut record = store
        .get_job(job.id)
        .await?
        .ok_or_else(|| echidnabot::Error::JobNotFound(job.id.0))?;
    record.status = echidnabot::scheduler::JobStatus::Running;
    record.started_at = Some(chrono::Utc::now());
    store.update_job(&record).await?;
    Ok(())
}

async fn finalize_job(
    store: &dyn Store,
    job: &ProofJob,
    result: &echidnabot::scheduler::JobResult,
) -> Result<()> {
    let mut record = store
        .get_job(job.id)
        .await?
        .ok_or_else(|| echidnabot::Error::JobNotFound(job.id.0))?;
    record.status = if result.success {
        echidnabot::scheduler::JobStatus::Completed
    } else {
        echidnabot::scheduler::JobStatus::Failed
    };
    record.completed_at = Some(chrono::Utc::now());
    record.error_message = if result.success {
        None
    } else {
        Some(result.message.clone())
    };
    store.update_job(&record).await?;

    let result_record = ProofResultRecord::new(job.id, result);
    store.save_result(&result_record).await?;

    if let Some(mut repo) = store.get_repository(job.repo_id).await? {
        repo.last_checked_commit = Some(job.commit_sha.clone());
        repo.updated_at = chrono::Utc::now();
        store.update_repository(&repo).await?;
    }
    Ok(())
}

/// Phase 2b: write double-loop feedback after a job finalizes.
///
/// Two writes:
///   1. `store.record_tactic_outcome` — per-file success/failure keyed by
///      prover + goal fingerprint. Uses prover_output as the goal-state
///      proxy (the first 2 kB, to cap DB row size). The "tactic" recorded
///      here is the string "proof_accepted" / "proof_rejected" — a CI-level
///      sentinel that captures the overall prover verdict, not a specific
///      in-proof tactic. Fine-grained per-tactic recording comes from the
///      GraphQL `recordTacticOutcome` mutation, called by LLM agents that
///      observe individual tactic steps.
///   2. `CorpusDelta.record` — appends a JSONL row to the echidna training
///      corpus for any job that succeeded. Only fires when
///      `config.corpus.enabled = true`. Errors are logged but never returned
///      so a misconfigured corpus path never breaks the scheduler.
async fn record_feedback(
    job: &ProofJob,
    result: &echidnabot::scheduler::JobResult,
    store: Arc<dyn Store>,
    config: &Config,
) {
    let goal_state_proxy = if result.prover_output.len() > 2048 {
        &result.prover_output[..2048]
    } else {
        &result.prover_output
    };
    let fingerprint = goal_fingerprint(goal_state_proxy);
    let tactic_label = if result.success {
        "proof_accepted"
    } else {
        "proof_rejected"
    };

    let outcome = TacticOutcomeRecord::new(
        Some(job.id.0),
        job.prover.clone(),
        fingerprint,
        tactic_label.to_string(),
        result.success,
        result.duration_ms as i64,
    );
    if let Err(e) = store.record_tactic_outcome(&outcome).await {
        tracing::debug!("record_tactic_outcome failed for job {}: {}", job.id, e);
    }

    if result.success && config.corpus.enabled {
        if let Some(ref dir) = config.corpus.training_data_dir {
            let mut cd = CorpusDelta::new(dir.clone());
            if let Some(ref root) = config.corpus.echidna_root {
                cd = cd.with_trigger(root.clone());
            }
            if let Some(t) = config.corpus.auto_trigger_threshold {
                cd = cd.with_auto_trigger(t);
            }
            let row = DeltaRow::new(
                job.prover.clone(),
                goal_state_proxy.to_string(),
                tactic_label.to_string(),
                true,
                result.duration_ms as i64,
                DeltaSource::Webhook,
            );
            if let Err(e) = cd.record(&row).await {
                tracing::warn!(
                    "corpus_delta.record failed for job {} ({}): {}",
                    job.id,
                    job.prover,
                    e
                );
            }
        }
    }
}

async fn process_job(
    job: &ProofJob,
    store: &dyn Store,
    echidna: &EchidnaClient,
    config: &Config,
) -> Result<echidnabot::scheduler::JobResult> {
    let start = Instant::now();
    let healthy = echidna.health_check().await?;
    if !healthy {
        return Err(echidnabot::Error::Echidna(
            "ECHIDNA core reported unhealthy status".to_string(),
        ));
    }

    let status = echidna.prover_status(&job.prover).await?;
    if status != ProverStatus::Available {
        return Err(echidnabot::Error::Echidna(format!(
            "Prover {} not available (status: {})",
            job.prover.display_name(),
            format_prover_status(status)
        )));
    }

    let repo = store
        .get_repository(job.repo_id)
        .await?
        .ok_or_else(|| echidnabot::Error::RepoNotFound(job.repo_id.to_string()))?;

    let repo_id = RepoId::new(repo.platform, repo.owner.clone(), repo.name.clone());
    let checkout = clone_repo(config, &repo_id, &job.commit_sha).await?;
    let repo_path = checkout.path().to_path_buf();

    let mut file_paths = job.file_paths.clone();
    if file_paths.is_empty() {
        let extensions: Vec<String> = job
            .prover
            .file_extensions()
            .iter()
            .map(|s| s.to_string())
            .collect();
        let repo_path_clone = repo_path.clone();
        file_paths = tokio::task::spawn_blocking(move || {
            collect_files_by_extension(&repo_path_clone, &extensions)
        })
        .await
        .unwrap_or_default()
        .into_iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect();

        if let Some(mut record) = store.get_job(job.id).await? {
            record.file_paths = file_paths.clone();
            store.update_job(&record).await?;
        }
    }

    if file_paths.is_empty() {
        return Ok(echidnabot::scheduler::JobResult {
            success: false,
            message: "No proof files found for prover".to_string(),
            prover_output: String::new(),
            duration_ms: start.elapsed().as_millis() as u64,
            verified_files: vec![],
            failed_files: vec![],
            confidence: None,
            axioms: None,
        });
    }

    const MAX_OUTPUT_BYTES: usize = 1024 * 1024; // 1 MiB cap on accumulated prover output

    let mut verified = Vec::new();
    let mut failed = Vec::new();
    let mut prover_output = String::new();

    // Build the local sandboxed executor once (only when configured).
    // When `executor.local_isolation = false` (default), proofs delegate
    // to ECHIDNA's REST API, which runs them in its own process. When
    // `true`, each proof runs in a Podman / bubblewrap sandbox locally
    // — needed for air-gapped or no-ECHIDNA setups.
    let local_executor = if config.executor.local_isolation {
        let mut ex = echidnabot::executor::container::PodmanExecutor::new().await;
        // Per-prover image fan-out — each prover gets the image
        // specialised for its binaries (smaller, faster cold-start,
        // narrower attack surface). Falls back to the default
        // container_image when no per-prover entry exists.
        if let Some(img) = config.executor.image_for(job.prover.clone()) {
            ex = ex.with_image(img);
        }
        if let Some(ref mem) = config.executor.memory_limit {
            ex = ex.with_memory_limit(mem.clone());
        }
        if let Some(cpus) = config.executor.cpu_limit {
            ex = ex.with_cpu_limit(cpus);
        }
        if let Some(secs) = config.executor.timeout_secs {
            ex = ex.with_timeout(std::time::Duration::from_secs(secs));
        }
        // Refuse to start if the operator opted in but neither podman
        // nor bubblewrap is available (fail-safe per SONNET-TASKS Task 1).
        if matches!(
            ex.backend(),
            echidnabot::executor::container::IsolationBackend::None
        ) {
            return Err(echidnabot::Error::Config(
                "executor.local_isolation = true but no isolation backend (podman or bubblewrap) was found on PATH. Refusing to run proofs without isolation.".to_string()
            ));
        }
        Some(ex)
    } else {
        None
    };

    for path in &file_paths {
        let full_path = if Path::new(path).is_absolute() {
            PathBuf::from(path)
        } else {
            repo_path.join(path)
        };
        let content = fs::read_to_string(&full_path).await?;

        let (verified_ok, output_chunk) = if let Some(ref ex) = local_executor {
            // Local sandboxed path. ExecutionResult is success on
            // exit_code == 0; non-zero (including timeout-kill) is
            // treated as failure with the captured stderr.
            match ex.execute_proof(job.prover.clone(), &content, None).await {
                Ok(exec) => {
                    let combined = if exec.stdout.trim().is_empty() {
                        exec.stderr.clone()
                    } else if exec.stderr.trim().is_empty() {
                        exec.stdout.clone()
                    } else {
                        format!("{}\n--- stderr ---\n{}", exec.stdout, exec.stderr)
                    };
                    (exec.exit_code == Some(0), combined)
                }
                Err(e) => (false, format!("Local executor error: {}", e)),
            }
        } else {
            // ECHIDNA-delegated path (default).
            let result = echidna.verify_proof(&job.prover, &content).await?;
            (
                result.status == echidnabot::dispatcher::ProofStatus::Verified,
                result.prover_output,
            )
        };

        if verified_ok {
            verified.push(path.to_string());
        } else {
            failed.push(path.to_string());
        }
        if !output_chunk.trim().is_empty() && prover_output.len() < MAX_OUTPUT_BYTES {
            let remaining = MAX_OUTPUT_BYTES - prover_output.len();
            let chunk = &output_chunk[..output_chunk.len().min(remaining)];
            prover_output.push_str(chunk);
            prover_output.push('\n');
        }
    }

    let success = failed.is_empty();
    let message = if success {
        format!("Verified {} file(s)", verified.len())
    } else {
        format!("Failed {} file(s)", failed.len())
    };

    let final_status = if success {
        echidnabot::dispatcher::ProofStatus::Verified
    } else {
        echidnabot::dispatcher::ProofStatus::Failed
    };
    let axioms = echidnabot::trust::axiom_tracker::AxiomTracker::scan(&job.prover, &prover_output);
    let confidence =
        echidnabot::trust::confidence::assess_confidence(&job.prover, final_status, false, 1);
    Ok(echidnabot::scheduler::JobResult {
        success,
        message,
        prover_output,
        duration_ms: start.elapsed().as_millis() as u64,
        verified_files: verified,
        failed_files: failed,
        confidence: Some(confidence),
        axioms: Some(axioms),
    })
}

async fn clone_repo(config: &Config, repo: &RepoId, commit: &str) -> Result<tempfile::TempDir> {
    match repo.platform {
        Platform::GitHub => {
            if let Some(ref gh) = config.github {
                if let Some(ref token) = gh.token {
                    let adapter = GitHubAdapter::new(token)?;
                    return adapter.clone_repo(repo, commit).await;
                }
            }
            clone_repo_via_git("https://github.com", repo, commit).await
        }
        Platform::GitLab => {
            let adapter = GitLabAdapter::new(config.gitlab.as_ref().map(|g| g.url.as_str()));
            adapter.clone_repo(repo, commit).await
        }
        Platform::Bitbucket => {
            let adapter = BitbucketAdapter::new(None);
            adapter.clone_repo(repo, commit).await
        }
        Platform::Codeberg => {
            let base_url = config
                .codeberg
                .as_ref()
                .map(|c| c.url.as_str())
                .unwrap_or("https://codeberg.org");
            clone_repo_via_git(base_url, repo, commit).await
        }
    }
}

async fn clone_repo_via_git(
    base_url: &str,
    repo: &RepoId,
    commit: &str,
) -> Result<tempfile::TempDir> {
    let url = format!(
        "{}/{}/{}.git",
        base_url.trim_end_matches('/'),
        repo.owner,
        repo.name
    );
    echidnabot::adapters::clone_revision(&url, commit).await
}

const MAX_PROOF_FILES: usize = 10_000;

fn collect_files_by_extension(root: &Path, extensions: &[String]) -> Vec<PathBuf> {
    let mut results = Vec::new();
    collect_files_inner(root, extensions, &mut results);
    results
}

/// Extract the first line number from a prover error message.
///
/// Tries common error-location patterns from major proof assistants:
/// - Coq/Rocq:    `File "X", line N`
/// - Lean 4:      `X:N:M:` (file:line:col)
/// - Agda:        `at X:N,M-N,M`
/// - Isabelle:    `line N of X`
///
/// Returns `None` when no pattern matches; callers default to line 1.
fn extract_error_line(prover_output: &str) -> Option<u32> {
    for line in prover_output.lines() {
        // Coq: File "path", line N, ...
        if let Some(rest) = line.find(", line ").map(|i| &line[i + 7..]) {
            if let Some(n) = rest.split([',', ' ']).next().and_then(|s| s.parse().ok()) {
                return Some(n);
            }
        }
        // Lean: path:N:M: ...
        let parts: Vec<&str> = line.splitn(4, ':').collect();
        if parts.len() >= 3 {
            if let (Ok(n), _) = (
                parts[1].trim().parse::<u32>(),
                parts[2].trim().parse::<u32>(),
            ) {
                if n > 0 {
                    return Some(n);
                }
            }
        }
        // Isabelle: line N of ...
        if let Some(rest) = line.find("line ").map(|i| &line[i + 5..]) {
            if let Some(n) = rest.split_whitespace().next().and_then(|s| s.parse().ok()) {
                return Some(n);
            }
        }
    }
    None
}

fn collect_files_inner(root: &Path, extensions: &[String], results: &mut Vec<PathBuf>) {
    if results.len() >= MAX_PROOF_FILES {
        return;
    }
    let Ok(entries) = std::fs::read_dir(root) else {
        return;
    };
    for entry in entries.flatten() {
        if results.len() >= MAX_PROOF_FILES {
            break;
        }
        let path = entry.path();
        if path.is_dir() {
            if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
                if name == ".git" || name == "target" {
                    continue;
                }
            }
            collect_files_inner(&path, extensions, results);
        } else if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
            if extensions.iter().any(|ext| name.ends_with(ext)) {
                results.push(path);
            }
        }
    }
}

#[cfg(test)]
mod clone_contract_tests {
    use super::*;
    use echidnabot::config::CodebergConfig;

    fn git(repo: &Path, args: &[&str]) -> String {
        let output = std::process::Command::new("git")
            .current_dir(repo)
            .args(args)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8(output.stdout).unwrap().trim().to_string()
    }

    #[tokio::test]
    async fn configured_forge_clones_exact_revision_and_rejects_missing_revision() {
        let forge = tempfile::tempdir().unwrap();
        let repo = forge.path().join("owner/proofs.git");
        std::fs::create_dir_all(&repo).unwrap();
        git(&repo, &["init", "--initial-branch=main"]);
        let commit_args = [
            "-c",
            "user.name=Contract Test",
            "-c",
            "user.email=contract@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--allow-empty",
            "-m",
            "fixture",
        ];
        git(&repo, &commit_args);
        let first = git(&repo, &["rev-parse", "HEAD"]);
        git(&repo, &commit_args);
        assert_ne!(first, git(&repo, &["rev-parse", "HEAD"]));
        let config = Config {
            codeberg: Some(CodebergConfig {
                url: format!("file://{}", forge.path().display()),
                token: None,
                webhook_secret: None,
            }),
            ..Default::default()
        };
        let id = RepoId::new(Platform::Codeberg, "owner", "proofs");
        let cloned = clone_repo(&config, &id, &first).await.unwrap();
        let path = cloned.path().to_path_buf();
        let actual = git(&path, &["rev-parse", "HEAD"]);
        drop(cloned);
        assert!(
            !path.exists(),
            "dropping the checkout must remove the repository"
        );
        assert_eq!(actual, first);
        assert!(clone_repo(&config, &id, "missing-contract-revision")
            .await
            .is_err());
        let missing = RepoId::new(Platform::Codeberg, "owner", "missing");
        assert!(clone_repo(&config, &missing, "HEAD").await.is_err());
    }
}
