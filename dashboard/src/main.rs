// SPDX-License-Identifier: PMPL-1.0-or-later
//! Fleet Dashboard - Web interface for gitbot-fleet monitoring
//!
//! Provides real-time monitoring, health status, findings explorer,
//! and fleet management through a web interface.

use axum::{
    extract::{Path, Query, State, WebSocketUpgrade},
    http::StatusCode,
    response::{Html, IntoResponse, Json},
    routing::get,
    Router,
};
use gitbot_shared_context::{Context as FleetContext, FleetHealth, ReportFormat};
use serde::{Deserialize, Serialize};
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_http::{cors::CorsLayer, services::ServeDir};
use tracing::{info, warn};

/// Dashboard application state
#[derive(Clone)]
struct AppState {
    /// Repository being monitored
    repo_path: PathBuf,
    /// Repository name
    repo_name: String,
    /// Cached fleet context
    context: Arc<RwLock<Option<FleetContext>>>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter("fleet_dashboard=debug,tower_http=debug")
        .with_target(false)
        .init();

    // Get repository path from environment or use current directory
    let repo_path: PathBuf = std::env::var("FLEET_REPO_PATH")
        .unwrap_or_else(|_| ".".to_string())
        .into();

    let repo_name = std::env::var("FLEET_REPO_NAME").unwrap_or_else(|_| "unknown".to_string());

    info!("Starting Fleet Dashboard");
    info!("Repository: {} ({})", repo_name, repo_path.display());

    let state = AppState {
        repo_path,
        repo_name,
        context: Arc::new(RwLock::new(None)),
    };

    // Build router
    let app = Router::new()
        .route("/", get(index_handler))
        .route("/api/health", get(health_handler))
        .route("/api/status", get(status_handler))
        .route("/api/findings", get(findings_handler))
        .route("/api/report/:format", get(report_handler))
        .route("/api/bots", get(bots_handler))
        .route("/ws", get(websocket_handler))
        .nest_service("/static", ServeDir::new("dashboard/static"))
        .layer(CorsLayer::permissive())
        .with_state(state);

    // Bind server
    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));
    info!("Dashboard listening on http://{}", addr);
    info!("Open http://localhost:8080 in your browser");

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

/// Serve index.html
async fn index_handler() -> Html<&'static str> {
    Html(include_str!("../static/index.html"))
}

/// Get fleet health status
async fn health_handler(State(state): State<AppState>) -> Json<FleetHealth> {
    let ctx = get_or_create_context(&state).await;
    Json(ctx.health_check())
}

/// Get fleet status
async fn status_handler(State(state): State<AppState>) -> Json<StatusResponse> {
    let ctx = get_or_create_context(&state).await;
    let summary = ctx.summary();

    StatusResponse {
        session_id: summary.session_id.to_string(),
        repo_name: summary.repo_name,
        duration_ms: summary.duration_ms,
        bots_run: summary.bots_run,
        bots_failed: summary.bots_failed,
        total_findings: summary.total_findings,
        total_errors: summary.total_errors,
        total_warnings: summary.total_warnings,
        blocks_release: summary.blocks_release,
    }
    .into()
}

/// Get findings with optional filters
async fn findings_handler(
    State(state): State<AppState>,
    Query(params): Query<FindingsQuery>,
) -> Json<FindingsResponse> {
    let ctx = get_or_create_context(&state).await;

    let mut findings: Vec<_> = ctx.findings.findings.iter().collect();

    // Filter by bot if specified
    if let Some(bot_name) = params.bot {
        findings.retain(|f| format!("{:?}", f.source) == bot_name);
    }

    // Filter by severity if specified
    if let Some(severity) = params.severity {
        findings.retain(|f| format!("{:?}", f.severity).to_lowercase() == severity.to_lowercase());
    }

    // Limit results
    let limit = params.limit.unwrap_or(100).min(1000);
    findings.truncate(limit);

    FindingsResponse {
        total: ctx.findings.findings.len(),
        filtered: findings.len(),
        findings: findings
            .into_iter()
            .map(|f| FindingData {
                source: format!("{:?}", f.source),
                category: f.category.clone(),
                severity: format!("{:?}", f.severity),
                message: f.message.clone(),
                file: f.file.as_ref().map(|p| p.display().to_string()),
                line: f.line,
                blocks_release: f.severity.blocks_release(),
            })
            .collect(),
    }
    .into()
}

/// Get fleet report in specified format
async fn report_handler(
    State(state): State<AppState>,
    Path(format): Path<String>,
) -> impl IntoResponse {
    let ctx = get_or_create_context(&state).await;

    let report_format = match format.to_lowercase().as_str() {
        "json" => ReportFormat::Json,
        "html" => ReportFormat::Html,
        _ => ReportFormat::Markdown,
    };

    let report = ctx.generate_report(report_format);

    match format.to_lowercase().as_str() {
        "html" => (StatusCode::OK, [("content-type", "text/html")], report),
        "json" => (StatusCode::OK, [("content-type", "application/json")], report),
        _ => (StatusCode::OK, [("content-type", "text/plain")], report),
    }
}

/// Get list of all bots
async fn bots_handler(State(state): State<AppState>) -> Json<BotsResponse> {
    let ctx = get_or_create_context(&state).await;

    let bots: Vec<_> = ctx
        .executions
        .iter()
        .map(|(bot_id, exec)| BotData {
            id: format!("{:?}", bot_id),
            tier: format!("{:?}", bot_id.tier()),
            status: format!("{:?}", exec.status),
            findings_count: exec.findings_count,
            errors_count: exec.errors_count,
            files_analyzed: exec.files_analyzed,
            started_at: exec.started_at.map(|t| t.to_rfc3339()),
            completed_at: exec.completed_at.map(|t| t.to_rfc3339()),
        })
        .collect();

    BotsResponse { bots }.into()
}

/// WebSocket handler for real-time updates
async fn websocket_handler(
    ws: WebSocketUpgrade,
    State(state): State<AppState>,
) -> impl IntoResponse {
    ws.on_upgrade(|socket| websocket_connection(socket, state))
}

/// Handle WebSocket connection
async fn websocket_connection(
    mut socket: axum::extract::ws::WebSocket,
    state: AppState,
) {
    use axum::extract::ws::Message;
    use tokio::time::{interval, Duration};

    let mut interval = interval(Duration::from_secs(5));

    loop {
        tokio::select! {
            _ = interval.tick() => {
                let ctx = get_or_create_context(&state).await;
                let health = ctx.health_check();

                let json = serde_json::to_string(&health).unwrap_or_default();
                if socket.send(Message::Text(json)).await.is_err() {
                    break;
                }
            }
            msg = socket.recv() => {
                match msg {
                    Some(Ok(Message::Close(_))) | None => break,
                    Some(Err(e)) => {
                        warn!("WebSocket error: {}", e);
                        break;
                    }
                    _ => {}
                }
            }
        }
    }
}

/// Get or create fleet context
async fn get_or_create_context(state: &AppState) -> FleetContext {
    let mut context = state.context.write().await;

    if context.is_none() {
        let mut ctx = FleetContext::new(&state.repo_name, &state.repo_path);
        ctx.register_all_bots();
        *context = Some(ctx);
    }

    context.clone().unwrap()
}

// API Response Types

#[derive(Debug, Serialize, Deserialize)]
struct StatusResponse {
    session_id: String,
    repo_name: String,
    duration_ms: Option<u64>,
    bots_run: usize,
    bots_failed: usize,
    total_findings: usize,
    total_errors: usize,
    total_warnings: usize,
    blocks_release: bool,
}

#[derive(Debug, Deserialize)]
struct FindingsQuery {
    bot: Option<String>,
    severity: Option<String>,
    limit: Option<usize>,
}

#[derive(Debug, Serialize)]
struct FindingsResponse {
    total: usize,
    filtered: usize,
    findings: Vec<FindingData>,
}

#[derive(Debug, Serialize)]
struct FindingData {
    source: String,
    category: String,
    severity: String,
    message: String,
    file: Option<String>,
    line: Option<usize>,
    blocks_release: bool,
}

#[derive(Debug, Serialize)]
struct BotsResponse {
    bots: Vec<BotData>,
}

#[derive(Debug, Serialize)]
struct BotData {
    id: String,
    tier: String,
    status: String,
    findings_count: usize,
    errors_count: usize,
    files_analyzed: usize,
    started_at: Option<String>,
    completed_at: Option<String>,
}
