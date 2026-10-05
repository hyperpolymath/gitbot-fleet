// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! BoJ-mediated LLM client for Consultant-mode Q&A.
//!
//! Per the Bit 6(b) decision (locked 2026-04-25 to option (a)): route
//! every LLM call through BoJ's `model-router-mcp` cartridge. This
//! preserves the estate's "BoJ-only MCP" canonical rule and means that
//! when BoJ revives + the V→Zig adapter ships, this client immediately
//! starts working without further code changes here.
//!
//! Today, BoJ is unreachable on most workstations (zig adapter
//! removed 2026-04-10, Zig replacement empty per ADR-001). Calls fall
//! through quickly with a clear error, and the Consultant handler
//! degrades to a local-data-only response.

use serde::{Deserialize, Serialize};
use std::time::Duration;

use crate::api::webhooks::AppState;
use crate::error::{Error, Result};
use crate::store::models::{ProofJobRecord, Repository};

/// BoJ cartridge invocation envelope. Matches
/// `boj-server/cartridges/model-router-mcp/invoke` shape.
#[derive(Serialize)]
struct BojInvocation<'a> {
    cartridge: &'a str,
    tool: &'a str,
    args: BojQAArgs<'a>,
}

#[derive(Serialize)]
struct BojQAArgs<'a> {
    repo: &'a str,
    pr_number: u64,
    question: &'a str,
    /// Compact context — recent job summaries flattened to text. The
    /// router cartridge passes this through to the model with an
    /// echidnabot-tuned system prompt that lives BoJ-side.
    context: String,
}

#[derive(Deserialize)]
struct BojResponse {
    /// Markdown-formatted answer ready for direct posting on the PR.
    answer: String,
    /// Free-form provenance — model name, latency, cache hit, etc.
    /// Surfaced in tracing logs but not in the user-facing comment.
    #[serde(default)]
    provenance: Option<String>,
}

/// Query BoJ for a Consultant-mode Q&A response. Returns a
/// markdown-formatted answer ready to embed in the PR comment.
///
/// Returns `Err(Error::Echidna)` (re-using the upstream-unreachable
/// error variant) when BoJ is down — callers should treat this as
/// "degrade to local response", not as a fatal error.
pub async fn query_boj_q_and_a(
    state: &AppState,
    repo: &Repository,
    pr_number: u64,
    question: &str,
    recent: &[ProofJobRecord],
) -> Result<String> {
    // Read BoJ endpoint from config. Honour an env-var override so the
    // operator can point at a non-default BoJ instance without a
    // restart of echidnabot's config-load path.
    let endpoint = std::env::var("BOJ_ENDPOINT")
        .ok()
        .or_else(|| state.config.boj.as_ref().map(|b| b.url.clone()))
        .unwrap_or_else(|| "http://127.0.0.1:7700".to_string());

    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(20))
        .build()
        .map_err(|e| Error::Echidna(format!("reqwest build: {}", e)))?;

    // Cheap health-check first. BoJ exposes /health on the loader; if
    // that's unreachable we abort before sending the cartridge call.
    let health_url = format!("{}/health", endpoint.trim_end_matches('/'));
    let health = client
        .get(&health_url)
        .timeout(Duration::from_secs(2))
        .send()
        .await
        .map_err(|e| Error::Echidna(format!("BoJ unreachable at {}: {}", endpoint, e)))?;
    if !health.status().is_success() {
        return Err(Error::Echidna(format!(
            "BoJ health check returned {} at {}",
            health.status(),
            endpoint
        )));
    }

    let context = build_context(recent);
    let invocation = BojInvocation {
        cartridge: "echidna-llm-mcp",
        tool: "consultant_qa",
        args: BojQAArgs {
            repo: &repo.full_name(),
            pr_number,
            question,
            context,
        },
    };

    let invoke_url = format!(
        "{}/cartridge/{}/invoke",
        endpoint.trim_end_matches('/'),
        invocation.cartridge
    );
    let resp = client
        .post(&invoke_url)
        .json(&invocation)
        .send()
        .await
        .map_err(|e| Error::Echidna(format!("BoJ invoke failed: {}", e)))?;

    if !resp.status().is_success() {
        return Err(Error::Echidna(format!(
            "BoJ cartridge returned {}",
            resp.status()
        )));
    }

    let parsed: BojResponse = resp
        .json()
        .await
        .map_err(|e| Error::Echidna(format!("BoJ response parse: {}", e)))?;

    if let Some(ref prov) = parsed.provenance {
        tracing::debug!("BoJ Q&A provenance: {}", prov);
    }

    Ok(parsed.answer)
}

/// Flatten recent job records into a compact context string for the
/// LLM. Format is human-readable to keep the model prompt simple;
/// volumes are bounded by the caller's filter (≤8 jobs) so we don't
/// blow the prompt budget.
fn build_context(recent: &[ProofJobRecord]) -> String {
    if recent.is_empty() {
        return "No recent verification jobs.".to_string();
    }
    let mut out = String::with_capacity(recent.len() * 80);
    out.push_str("Recent verification jobs (most recent first):\n");
    for job in recent {
        let detail = job
            .error_message
            .as_deref()
            .map(|s| {
                format!(
                    " — error: {}",
                    s.lines()
                        .next()
                        .unwrap_or("")
                        .chars()
                        .take(120)
                        .collect::<String>()
                )
            })
            .unwrap_or_default();
        out.push_str(&format!(
            "- {} · {:?} · status={:?}{}\n",
            &job.commit_sha[..8.min(job.commit_sha.len())],
            job.prover,
            job.status,
            detail
        ));
    }
    out
}
