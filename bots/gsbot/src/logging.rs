// SPDX-License-Identifier: PMPL-1.0-or-later
//! Logging setup — colored console with optional file output. Faithful
//! intent of `utils/logger.py` (colorlog) using `tracing`.

use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::EnvFilter;

/// Map the Python-style level name to a tracing filter directive.
fn level_directive(level: &str) -> &'static str {
    match level.to_ascii_uppercase().as_str() {
        "DEBUG" => "debug",
        "WARNING" | "WARN" => "warn",
        "ERROR" => "error",
        "CRITICAL" => "error",
        _ => "info",
    }
}

/// Initialise the global subscriber. Console output is colored; if
/// `log_file` is set, a plain non-blocking file layer is added. Returns
/// the file appender guard (kept alive for the process lifetime).
pub fn init(level: &str, log_file: Option<&str>) -> Option<tracing_appender::non_blocking::WorkerGuard> {
    let env = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new(level_directive(level)));

    let console = tracing_subscriber::fmt::layer()
        .with_ansi(true)
        .with_target(true);

    if let Some(path) = log_file {
        if let Some(parent) = std::path::Path::new(path).parent() {
            if !parent.as_os_str().is_empty() {
                let _ = std::fs::create_dir_all(parent);
            }
        }
        let file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path);
        if let Ok(file) = file {
            let (nb, guard) = tracing_appender::non_blocking(file);
            let file_layer = tracing_subscriber::fmt::layer()
                .with_ansi(false)
                .with_writer(nb);
            tracing_subscriber::registry()
                .with(env)
                .with(console)
                .with(file_layer)
                .init();
            return Some(guard);
        }
    }

    tracing_subscriber::registry().with(env).with(console).init();
    None
}
