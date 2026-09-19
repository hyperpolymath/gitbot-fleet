// SPDX-License-Identifier: MPL-2.0

//! Rhodibot - RSR Compliance Bot
//!
//! A GitHub bot for enforcing Rhodium Standard Repository guidelines
//! across the hyperpolymath organization.

use anyhow::Result;
use axum::{
    Json, Router,
    extract::State,
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
};
use clap::{Parser, Subcommand};
use serde::Serialize;
use std::sync::Arc;
use tokio::net::TcpListener;
use tower_http::trace::TraceLayer;
use tracing::{info, warn};

use rhodibot::canon::Canon;
use rhodibot::canon::local;
use rhodibot::canon::profile::GateTable;
use rhodibot::canon::report::{CanonReport, FailOn};
use rhodibot::config;
use rhodibot::github::GitHubClient;
use rhodibot::rsr;
use rhodibot::webhook;

use config::Config;

/// RSR Compliance Bot for repository management
#[derive(Parser, Debug)]
#[command(name = "rhodibot")]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Port to listen on
    #[arg(short, long, env = "PORT", default_value = "3000")]
    port: u16,

    /// Address to bind.
    ///
    /// Loopback by default: the intended deployment fronts this process with a
    /// Cloudflare Tunnel (or another reverse proxy) on the same host, so the
    /// webhook port has no business being reachable from the network. Set
    /// `--bind 0.0.0.0` (or `BIND_ADDR=0.0.0.0`) to serve directly, which is
    /// what earlier versions did unconditionally.
    #[arg(long, env = "BIND_ADDR", default_value = "127.0.0.1")]
    bind: String,

    /// GitHub App ID
    #[arg(long, env = "GITHUB_APP_ID")]
    app_id: Option<u64>,

    /// Path to GitHub App private key
    #[arg(long, env = "GITHUB_PRIVATE_KEY_PATH")]
    private_key_path: Option<String>,

    /// Webhook secret for verification
    #[arg(long, env = "GITHUB_WEBHOOK_SECRET")]
    webhook_secret: Option<String>,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Check a repository against the RSR canon's file-presence criteria.
    ///
    /// Reads the canon vendored beside the rules (never a hardcoded list),
    /// skips the criteria the repository's `.machine_readable/rsr-profile.a2ml`
    /// does not declare a capability for, and reports what is present where the
    /// canon records it, present elsewhere, present under a location the canon
    /// has retired, or absent.
    ///
    /// **Advisory by default**: exits 0 whatever it finds. The canon names
    /// hypatia's `rsr-conformance` the single normative checker, so a second
    /// opinion should not claim authority it does not have. Pass `--fail-on`
    /// to make it gate CI anyway.
    ///
    /// Works unauthenticated on public repositories; set `GITHUB_TOKEN` to
    /// check private ones or lift the rate limit.
    Canon {
        /// Repository owner (user or org), e.g. `hyperpolymath`
        #[arg(long)]
        owner: Option<String>,

        /// Repository name, e.g. `ubicity`
        #[arg(long)]
        repo: Option<String>,

        /// Check a local checkout instead of a GitHub repository
        #[arg(long, conflicts_with_all = ["owner", "repo"])]
        path: Option<String>,

        /// Output format
        #[arg(long, default_value = "pretty", value_parser = ["pretty", "json"])]
        format: String,

        /// Exit non-zero when a finding at or above this severity is present
        #[arg(long, default_value = "none", value_parser = ["none", "relocated", "deprecated", "missing"])]
        fail_on: String,
    },

    /// Run a one-shot RSR compliance check against a remote repository and exit.
    ///
    /// Uses the GitHub REST API (honours the `GITHUB_TOKEN` env var for rate
    /// limits / private repos; works unauthenticated on public repos). Exits
    /// non-zero when required checks fail, so it can gate CI directly.
    Check {
        /// Repository owner (user or org), e.g. `hyperpolymath`
        #[arg(long)]
        owner: String,

        /// Repository name, e.g. `ubicity`
        #[arg(long)]
        repo: String,

        /// Output format
        #[arg(long, default_value = "pretty", value_parser = ["pretty", "json"])]
        format: String,
    },
}

/// Application state shared across handlers
#[derive(Clone)]
struct AppState {
    config: Arc<Config>,
    /// How the bot authenticates, reported by `/health`. Never credential
    /// material -- a mode name, so an operator can see whether the App
    /// credentials were picked up.
    credential_mode: &'static str,
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "rhodibot=info,tower_http=info".into()),
        )
        .init();

    // Load environment variables
    dotenvy::dotenv().ok();

    // Parse CLI arguments
    let cli = Cli::parse();

    // Build configuration
    let config = Config::new(
        cli.app_id,
        cli.private_key_path.as_deref(),
        cli.webhook_secret.clone(),
    )?;

    // One-shot CLI mode: run a compliance check and exit with a CI-usable code.
    if let Some(Command::Check {
        owner,
        repo,
        format,
    }) = &cli.command
    {
        return run_check(&config, owner, repo, format).await;
    }

    // The canon check: the same rule set the estate is scored against, read
    // from the canon itself rather than from a list in this file.
    if let Some(Command::Canon {
        owner,
        repo,
        path,
        format,
        fail_on,
    }) = &cli.command
    {
        return run_canon(&config, owner, repo, path, format, fail_on).await;
    }

    info!("Starting Rhodibot v{}", env!("CARGO_PKG_VERSION"));

    // Refuse to start with credentials that cannot work. A half-configured App
    // must not quietly become an anonymous client: that turns into a
    // permissions puzzle at the first webhook instead of a clear failure now.
    let credentials = GitHubClient::new(&config);
    credentials.readiness()?;
    info!("GitHub credentials: {}", credentials.credential_mode());

    let state = AppState {
        config: Arc::new(config),
        credential_mode: credentials.credential_mode(),
    };

    // Build router
    let app = Router::new()
        .route("/", get(health_check))
        .route("/health", get(health_check))
        .route("/webhook", post(webhook_handler))
        .route("/api/check/{owner}/{repo}", get(check_repository))
        .layer(TraceLayer::new_for_http())
        .with_state(state);

    // Start server
    let addr = format!("{}:{}", cli.bind, cli.port);
    let listener = TcpListener::bind(&addr).await?;
    info!("Listening on {}", addr);

    axum::serve(listener, app).await?;

    Ok(())
}

/// One-shot RSR compliance check for CI. Prints a report and exits non-zero
/// when required checks fail (so the caller's job fails too).
/// The file paths a repository's profile may live at, in preference order.
const PROFILE_PATHS: [&str; 2] = [
    ".machine_readable/rsr-profile.a2ml",
    "machine-readable/rsr-profile.a2ml",
];

/// Check one repository against the canon.
///
/// The rules come from the vendored canon; the applicability comes from the
/// repository's own profile; the classification comes from the verdict module.
/// Nothing about the rule set is decided here.
async fn run_canon(
    config: &Config,
    owner: &Option<String>,
    repo: &Option<String>,
    path: &Option<String>,
    format: &str,
    fail_on: &str,
) -> Result<()> {
    let fail_on = FailOn::parse(fail_on)?;

    let (subject, files, profile_source, from_git) = match (owner, repo, path) {
        (Some(owner), Some(repo), None) => {
            rhodibot::sanitize::validate_owner_repo(owner, repo)?;
            let client = GitHubClient::new(config);
            let files = client.tree_paths(owner, repo).await?;

            // The profile decides which criteria apply, so a failure to read it
            // must not pass as "declares nothing".
            let mut profile = None;
            for candidate in PROFILE_PATHS {
                if let Some(text) = client
                    .get_file_content_if_present(owner, repo, candidate)
                    .await?
                {
                    profile = Some(text);
                    break;
                }
            }

            (format!("{owner}/{repo}"), files, profile, true)
        }
        (None, None, Some(path)) => {
            let source = local::read(std::path::Path::new(path))?;
            let from_git = source.from_git();
            (path.clone(), source.files, source.profile, from_git)
        }
        _ => anyhow::bail!("give either --owner and --repo, or --path: one repository, one way"),
    };

    let canon = Canon::vendored()?;
    let gates = GateTable::vendored()?;
    let report = CanonReport::build(&subject, &canon, &gates, &files, profile_source.as_deref())?;

    if format == "json" {
        println!("{}", serde_json::to_string_pretty(&report)?);
    } else {
        if !from_git && path.is_some() {
            // A walked list includes untracked files, which can satisfy a
            // criterion by accident. Say so rather than let the reader assume
            // the tracked set.
            println!(
                "warning: git was not available, so this is a walk of the working directory -- \
                 untracked and build files are included."
            );
        }
        print!("{}", report.render());
    }

    if fail_on.breached_by(&report) {
        let counts = report.findings_by_severity();
        println!(
            "\n--fail-on {}: {}",
            fail_on.as_str(),
            counts
                .iter()
                .map(|(severity, count)| format!("{count} {severity}"))
                .collect::<Vec<_>>()
                .join(", ")
        );
        std::process::exit(1);
    }

    Ok(())
}

async fn run_check(config: &Config, owner: &str, repo: &str, format: &str) -> Result<()> {
    rhodibot::sanitize::validate_owner_repo(owner, repo)?;

    let report = rsr::check_compliance(config, owner, repo).await?;

    if format == "json" {
        println!("{}", serde_json::to_string_pretty(&report)?);
    } else {
        println!(
            "RSR compliance — {}/{} (policy: {})",
            report.owner, report.repo, report.policy
        );
        println!("{}", rsr::policy_summary(report.policy));
        println!(
            "Score: {}/{} ({:.0}%)",
            report.score, report.max_score, report.percentage
        );
        println!();
        for check in &report.checks {
            println!(
                "  [{:?}] {} ({:?}, {}/{}) — {}",
                check.status,
                check.name,
                check.severity,
                check.points,
                check.max_points,
                check.message
            );
        }
        println!();
        println!("{}", report.summary);
        println!(
            "Required checks: {}",
            if report.required_passed {
                "PASSED"
            } else {
                "FAILED"
            }
        );
    }

    if !report.required_passed {
        std::process::exit(1);
    }
    Ok(())
}

/// Health check endpoint
async fn health_check(State(state): State<AppState>) -> impl IntoResponse {
    Json(HealthResponse {
        status: "healthy".to_string(),
        version: env!("CARGO_PKG_VERSION").to_string(),
        name: "rhodibot".to_string(),
        credentials: state.credential_mode.to_string(),
    })
}

#[derive(Serialize)]
struct HealthResponse {
    status: String,
    version: String,
    name: String,
    /// e.g. "GitHub App installation tokens". Named, never the credential.
    credentials: String,
}

/// Webhook handler for GitHub events
async fn webhook_handler(
    State(state): State<AppState>,
    headers: axum::http::HeaderMap,
    body: String,
) -> impl IntoResponse {
    // Verify webhook signature if secret is configured
    if let Some(ref secret) = state.config.webhook_secret {
        if let Some(signature) = headers.get("x-hub-signature-256") {
            if !webhook::verify_signature(secret, &body, signature.to_str().unwrap_or("")) {
                warn!("Invalid webhook signature");
                return (StatusCode::UNAUTHORIZED, "Invalid signature").into_response();
            }
        } else {
            warn!("Missing webhook signature");
            return (StatusCode::UNAUTHORIZED, "Missing signature").into_response();
        }
    }

    // Parse event type
    let event_type = headers
        .get("x-github-event")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("unknown");

    info!("Received webhook event: {}", event_type);

    // Process event
    match event_type {
        "push" => webhook::handle_push(&state.config, &body).await,
        "pull_request" => webhook::handle_pull_request(&state.config, &body).await,
        "repository" => webhook::handle_repository(&state.config, &body).await,
        "installation" | "installation_repositories" => {
            webhook::handle_installation(&state.config, &body).await
        }
        "ping" => {
            info!("Received ping event");
            Ok(())
        }
        _ => {
            info!("Ignoring event type: {}", event_type);
            Ok(())
        }
    }
    .map(|_| (StatusCode::OK, "OK").into_response())
    .unwrap_or_else(|e| {
        warn!("Error processing webhook: {}", e);
        (StatusCode::INTERNAL_SERVER_ERROR, "Error").into_response()
    })
}

/// Check a repository for RSR compliance
async fn check_repository(
    State(state): State<AppState>,
    axum::extract::Path((owner, repo)): axum::extract::Path<(String, String)>,
) -> impl IntoResponse {
    // Validate owner/repo names from URL path
    if let Err(e) = rhodibot::sanitize::validate_owner_repo(&owner, &repo) {
        warn!("Invalid owner/repo in request: {}", e);
        return (StatusCode::BAD_REQUEST, e.to_string()).into_response();
    }

    info!("Checking repository: {}/{}", owner, repo);

    match rsr::check_compliance(&state.config, &owner, &repo).await {
        Ok(report) => Json(report).into_response(),
        Err(e) => {
            warn!("Error checking repository: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()).into_response()
        }
    }
}
