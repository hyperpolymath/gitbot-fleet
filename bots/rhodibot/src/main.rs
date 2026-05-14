// SPDX-License-Identifier: PMPL-1.0-or-later

//! Rhodibot - RSR Compliance Bot
//!
//! A GitHub bot for enforcing Rhodium Standard Repository guidelines
//! across the hyperpolymath organization.

use anyhow::Result;
use axum::{
    extract::State,
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use clap::Parser;
use serde::Serialize;
use std::sync::Arc;
use tokio::net::TcpListener;
use tower_http::trace::TraceLayer;
use tracing::{info, warn};

use rhodibot::config;
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

    /// GitHub App ID
    #[arg(long, env = "GITHUB_APP_ID")]
    app_id: Option<u64>,

    /// Path to GitHub App private key
    #[arg(long, env = "GITHUB_PRIVATE_KEY_PATH")]
    private_key_path: Option<String>,

    /// Webhook secret for verification
    #[arg(long, env = "GITHUB_WEBHOOK_SECRET")]
    webhook_secret: Option<String>,
}

/// Application state shared across handlers
#[derive(Clone)]
struct AppState {
    config: Arc<Config>,
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

    info!("Starting Rhodibot v{}", env!("CARGO_PKG_VERSION"));

    // Build configuration
    let config = Config::new(
        cli.app_id,
        cli.private_key_path.as_deref(),
        cli.webhook_secret.clone(),
    )?;
    let state = AppState {
        config: Arc::new(config),
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
    let addr = format!("0.0.0.0:{}", cli.port);
    let listener = TcpListener::bind(&addr).await?;
    info!("Listening on {}", addr);

    axum::serve(listener, app).await?;

    Ok(())
}

/// Health check endpoint
async fn health_check() -> impl IntoResponse {
    Json(HealthResponse {
        status: "healthy".to_string(),
        version: env!("CARGO_PKG_VERSION").to_string(),
        name: "rhodibot".to_string(),
    })
}

#[derive(Serialize)]
struct HealthResponse {
    status: String,
    version: String,
    name: String,
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
