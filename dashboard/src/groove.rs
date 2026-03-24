// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! Groove capability discovery endpoint for gitbot-fleet.
//!
//! Serves GET `/.well-known/groove` with a JSON capability manifest and
//! GET `/health` so that groove-aware systems (Gossamer, PanLL, BoJ) can
//! discover and probe the fleet dashboard automatically.
//!
//! The canonical ABI definition lives in `Groove.idr` (`gitbotFleetManifest`).

use axum::response::Json;

/// Groove capability manifest for gitbot-fleet.
///
/// Advertises the fleet's bot-orchestration capability in the standard Groove
/// format.  Any groove-aware client can probe `GET /.well-known/groove` on
/// port 7500 to discover this service.
///
/// ## Capabilities offered
///
/// - **bot-orchestration** — Fleet of 6 bots (rhodibot, echidnabot,
///   sustainabot, glambot, seambot, finishbot) for automated repository
///   maintenance across 500+ hyperpolymath repos.
///
/// ## Capabilities consumed
///
/// - **octad-storage** (VeriSimDB) — Persist dispatch outcomes and fleet
///   metrics.
/// - **static-analysis** (Hypatia / CodeQL) — Receive findings that drive
///   bot dispatch decisions.
pub async fn groove_manifest() -> Json<serde_json::Value> {
    Json(serde_json::json!({
        "groove_version": "1",
        "service_id": "gitbot-fleet",
        "service_version": "0.1.0",
        "capabilities": {
            "bot_orchestration": {
                "type": "bot-orchestration",
                "description": "Fleet of 6 bots (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot) for automated repository maintenance",
                "protocol": "http",
                "endpoint": "/api/v1/dispatch",
                "requires_auth": false,
                "panel_compatible": true
            }
        },
        "consumes": ["octad-storage", "static-analysis"],
        "endpoints": {
            "api": "http://localhost:7500/api",
            "health": "http://localhost:7500/health"
        },
        "health": "/health",
        "applicability": ["individual", "team"]
    }))
}

/// Lightweight health probe for groove discovery.
///
/// Returns a minimal JSON object indicating the service is alive.  This is
/// the endpoint referenced by the `"health"` key in the groove manifest and
/// is intentionally separate from the richer `/api/health` handler which
/// returns full `FleetHealth` diagnostics.
pub async fn health() -> Json<serde_json::Value> {
    Json(serde_json::json!({
        "service_id": "gitbot-fleet",
        "status": "ok"
    }))
}
