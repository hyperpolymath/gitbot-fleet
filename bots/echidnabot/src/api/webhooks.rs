// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Webhook handlers for GitHub, GitLab, Bitbucket, and Codeberg/Forgejo

use axum::{
    body::Bytes,
    extract::State,
    http::{HeaderMap, StatusCode},
    middleware,
    response::IntoResponse,
    routing::post,
    Router,
};
use hmac::{Hmac, KeyInit, Mac};
use sha2::Sha256;
use std::sync::Arc;

use serde::Deserialize;

use crate::adapters::{Platform, PrId, RepoId};
use crate::api::rate_limit::{rate_limit_middleware, WebhookRateLimiter};
use crate::config::Config;
use crate::error::Result;
use crate::modes::{self, ModeSelector};
use crate::scheduler::{JobPriority, JobScheduler, ProofJob};
use crate::store::models::ProofJobRecord;
use crate::store::Store;

/// Application state shared across handlers
#[derive(Clone)]
pub struct AppState {
    pub config: Arc<Config>,
    pub store: Arc<dyn Store>,
    pub scheduler: Arc<JobScheduler>,
    /// Per-IP sliding-window rate limiter for webhook endpoints. `None` = unlimited.
    pub rate_limiter: Option<Arc<WebhookRateLimiter>>,
    /// Daemon-wide mode selector — the fallback when no per-repo directive
    /// or DB column setting is found. Populated from `[bot] mode` in the
    /// TOML config. Avoids a DB lookup for the common "no per-repo setting"
    /// case inside webhook handlers.
    pub mode_selector: ModeSelector,
}

/// Create webhook router with optional per-IP rate limiting.
///
/// `state` is cloned into the rate-limit middleware so it can access the
/// `rate_limiter` field without going through the router's state layer.
pub fn webhook_router(state: AppState) -> Router<AppState> {
    Router::new()
        .route("/webhooks/github", post(handle_github_webhook))
        .route("/webhooks/gitlab", post(handle_gitlab_webhook))
        .route("/webhooks/bitbucket", post(handle_bitbucket_webhook))
        .route("/webhooks/codeberg", post(handle_codeberg_webhook))
        .layer(middleware::from_fn_with_state(state, rate_limit_middleware))
}

/// GitHub webhook handler
#[tracing::instrument(
    name = "webhook.github",
    skip(state, headers, body),
    fields(
        payload_bytes = body.len(),
        event_type = tracing::field::Empty,
        delivery_id = tracing::field::Empty,
    )
)]
async fn handle_github_webhook(
    State(state): State<AppState>,
    headers: HeaderMap,
    body: Bytes,
) -> impl IntoResponse {
    tracing::info!("Received GitHub webhook");

    // Verify signature if secret is configured
    if let Some(ref gh_config) = state.config.github {
        if let Some(ref secret) = gh_config.webhook_secret {
            if let Err(e) = verify_github_signature(&headers, &body, secret) {
                tracing::warn!("GitHub webhook signature verification failed: {}", e);
                return (StatusCode::UNAUTHORIZED, "Invalid signature");
            }
        }
    }

    // Parse event type + traceability id
    let event_type = headers
        .get("X-GitHub-Event")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("unknown");
    let delivery_id = headers
        .get("X-GitHub-Delivery")
        .and_then(|v| v.to_str().ok())
        .map(String::from);

    // Record the event_type + delivery_id on the active span so distributed
    // traces can be grouped by event in the collector UI.
    let span = tracing::Span::current();
    span.record("event_type", event_type);
    if let Some(ref id) = delivery_id {
        span.record("delivery_id", id.as_str());
    }
    tracing::info!("GitHub event type: {}", event_type);

    match event_type {
        "push" => {
            tracing::info!("Received push event");
            if let Ok(payload) = serde_json::from_slice::<GitHubPushPayload>(&body) {
                let (owner, name) = split_full_name(&payload.repository.full_name);
                let _ = enqueue_repo_jobs(
                    &state,
                    Platform::GitHub,
                    &owner,
                    &name,
                    &payload.after,
                    JobPriority::Normal,
                    RepoEventKind::Push,
                    None,
                    delivery_id.clone(),
                )
                .await;
            }
        }
        "pull_request" => {
            tracing::info!("Received pull_request event");
            if let Ok(payload) = serde_json::from_slice::<GitHubPullRequestPayload>(&body) {
                let (owner, name) = split_full_name(&payload.repository.full_name);
                let _ = enqueue_repo_jobs(
                    &state,
                    Platform::GitHub,
                    &owner,
                    &name,
                    &payload.pull_request.head.sha,
                    JobPriority::High,
                    RepoEventKind::PullRequest,
                    Some(payload.pull_request.number),
                    delivery_id.clone(),
                )
                .await;
            }
        }
        "check_suite" => {
            tracing::info!("Received check_suite event");
            if let Ok(payload) = serde_json::from_slice::<GitHubCheckSuitePayload>(&body) {
                let (owner, name) = split_full_name(&payload.repository.full_name);
                let _ = enqueue_repo_jobs(
                    &state,
                    Platform::GitHub,
                    &owner,
                    &name,
                    &payload.check_suite.head_sha,
                    JobPriority::High,
                    RepoEventKind::PullRequest,
                    None, // check_suite payload doesn't carry the PR number directly
                    delivery_id.clone(),
                )
                .await;
            }
        }
        "issue_comment" => {
            // Consultant-mode trigger: any @echidnabot mention on a PR
            // comment surfaces a structured Q&A response. Bare comments
            // without a mention are ignored. Bot/system author comments
            // (echidnabot's own posts) are filtered to avoid loops.
            tracing::info!("Received issue_comment event");
            if let Ok(payload) = serde_json::from_slice::<GitHubIssueCommentPayload>(&body) {
                if !modes::is_any_mention(&payload.comment.body) {
                    return (StatusCode::OK, "OK");
                }
                if payload.comment.user.as_ref().is_some_and(|u| {
                    u.login.eq_ignore_ascii_case("echidnabot")
                        || matches!(u.user_type.as_deref(), Some("Bot"))
                }) {
                    tracing::debug!("Ignoring own comment / bot author");
                    return (StatusCode::OK, "OK");
                }
                let (owner, name) = split_full_name(&payload.repository.full_name);
                let _ = handle_consultant_mention(
                    &state,
                    Platform::GitHub,
                    &owner,
                    &name,
                    payload.issue.number,
                    &payload.comment.body,
                )
                .await;
            }
        }
        "ping" => {
            tracing::info!("Received ping event - webhook configured correctly");
        }
        _ => {
            tracing::debug!("Ignoring event type: {}", event_type);
        }
    }

    (StatusCode::OK, "OK")
}

/// GitLab webhook handler
#[tracing::instrument(
    name = "webhook.gitlab",
    skip(state, headers, body),
    fields(payload_bytes = body.len())
)]
async fn handle_gitlab_webhook(
    State(state): State<AppState>,
    headers: HeaderMap,
    body: Bytes,
) -> impl IntoResponse {
    tracing::info!("Received GitLab webhook");

    // Verify token if configured
    if let Some(ref gl_config) = state.config.gitlab {
        if let Some(ref secret) = gl_config.webhook_secret {
            let token = headers
                .get("X-Gitlab-Token")
                .and_then(|v| v.to_str().ok())
                .unwrap_or("");

            if token != secret {
                tracing::warn!("GitLab webhook token mismatch");
                return (StatusCode::UNAUTHORIZED, "Invalid token");
            }
        }
    }

    // Parse event type + traceability id
    let event_type = headers
        .get("X-Gitlab-Event")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("unknown");
    let delivery_id = headers
        .get("X-Gitlab-Webhook-UUID")
        .and_then(|v| v.to_str().ok())
        .map(String::from);

    tracing::info!("GitLab event type: {}", event_type);

    match event_type {
        "Push Hook" => {
            tracing::info!("Received push hook");
            if let Ok(payload) = serde_json::from_slice::<GitLabPushPayload>(&body) {
                let (owner, name) = split_full_name(&payload.project.path_with_namespace);
                let commit = payload.checkout_sha.unwrap_or(payload.after);
                let _ = enqueue_repo_jobs(
                    &state,
                    Platform::GitLab,
                    &owner,
                    &name,
                    &commit,
                    JobPriority::Normal,
                    RepoEventKind::Push,
                    None,
                    delivery_id.clone(),
                )
                .await;
            }
        }
        "Merge Request Hook" => {
            tracing::info!("Received merge request hook");
            if let Ok(payload) = serde_json::from_slice::<GitLabMergeRequestPayload>(&body) {
                let (owner, name) = split_full_name(&payload.project.path_with_namespace);
                let mr_iid = payload.object_attributes.iid;
                let commit = payload
                    .object_attributes
                    .last_commit
                    .map(|c| c.id)
                    .unwrap_or_else(|| payload.object_attributes.last_commit_id);
                let _ = enqueue_repo_jobs(
                    &state,
                    Platform::GitLab,
                    &owner,
                    &name,
                    &commit,
                    JobPriority::High,
                    RepoEventKind::PullRequest,
                    mr_iid,
                    delivery_id.clone(),
                )
                .await;
            }
        }
        "Note Hook" => {
            tracing::info!("Received GitLab note hook (Consultant trigger)");
            if let Ok(payload) = serde_json::from_slice::<GitLabNotePayload>(&body) {
                if !modes::is_any_mention(&payload.object_attributes.note) {
                    return (StatusCode::OK, "OK");
                }
                if payload
                    .user
                    .as_ref()
                    .is_some_and(|u| u.username.eq_ignore_ascii_case("echidnabot"))
                {
                    return (StatusCode::OK, "OK");
                }
                // Only respond on MR notes — Issue notes don't have a PR
                // to comment back on.
                if payload.object_attributes.noteable_type.as_deref() != Some("MergeRequest") {
                    return (StatusCode::OK, "OK");
                }
                let Some(mr) = payload.merge_request.as_ref() else {
                    return (StatusCode::OK, "OK");
                };
                let (owner, name) = split_full_name(&payload.project.path_with_namespace);
                let _ = handle_consultant_mention(
                    &state,
                    Platform::GitLab,
                    &owner,
                    &name,
                    mr.iid,
                    &payload.object_attributes.note,
                )
                .await;
            }
        }
        _ => {
            tracing::debug!("Ignoring event type: {}", event_type);
        }
    }

    (StatusCode::OK, "OK")
}

/// Bitbucket webhook handler
#[tracing::instrument(
    name = "webhook.bitbucket",
    skip(state, headers, body),
    fields(payload_bytes = body.len())
)]
async fn handle_bitbucket_webhook(
    State(state): State<AppState>,
    headers: HeaderMap,
    body: Bytes,
) -> impl IntoResponse {
    tracing::info!("Received Bitbucket webhook");

    let event_type = headers
        .get("X-Event-Key")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("unknown");
    let delivery_id = headers
        .get("X-Hook-UUID")
        .and_then(|v| v.to_str().ok())
        .map(String::from);

    tracing::info!("Bitbucket event type: {}", event_type);

    if event_type.starts_with("repo:push") {
        if let Ok(payload) = serde_json::from_slice::<BitbucketPushPayload>(&body) {
            let (owner, name) = split_full_name(&payload.repository.full_name);
            if let Some(commit) = payload
                .push
                .changes
                .first()
                .and_then(|c| c.new_target.as_ref())
                .map(|t| t.hash.clone())
            {
                let _ = enqueue_repo_jobs(
                    &state,
                    Platform::Bitbucket,
                    &owner,
                    &name,
                    &commit,
                    JobPriority::Normal,
                    RepoEventKind::Push,
                    None,
                    delivery_id.clone(),
                )
                .await;
            }
        }
    } else if event_type == "pullrequest:comment_created" {
        tracing::info!("Received Bitbucket pullrequest:comment_created (Consultant trigger)");
        if let Ok(payload) = serde_json::from_slice::<BitbucketPRCommentPayload>(&body) {
            if !modes::is_any_mention(&payload.comment.content.raw) {
                return (StatusCode::OK, "OK");
            }
            if payload
                .actor
                .as_ref()
                .is_some_and(|u| u.username.eq_ignore_ascii_case("echidnabot"))
            {
                return (StatusCode::OK, "OK");
            }
            let (owner, name) = split_full_name(&payload.repository.full_name);
            let _ = handle_consultant_mention(
                &state,
                Platform::Bitbucket,
                &owner,
                &name,
                payload.pullrequest.id,
                &payload.comment.content.raw,
            )
            .await;
        }
    }

    (StatusCode::OK, "OK")
}

/// Codeberg / Forgejo / Gitea webhook handler (issue #62 scaffold).
///
/// Codeberg runs Forgejo (a Gitea fork) and uses the Gitea webhook wire
/// format unchanged:
///
/// - `X-Gitea-Event` header carries the event name (e.g. `push`,
///   `pull_request`, `issue_comment`)
/// - `X-Gitea-Signature` header carries an HMAC-SHA256 hex digest of
///   the raw body, **without** the `sha256=` prefix GitHub uses
/// - `X-Gitea-Delivery` is the per-delivery UUID (traceability)
///
/// Payload shapes mirror GitHub closely enough that we can deserialize
/// the same `*PushPayload` / `*PullRequestPayload` shape with Gitea
/// field names. This handler is **scaffold only** — it dispatches the
/// three event types we already enqueue for other platforms (push, PR,
/// issue_comment) and leaves the rest as `tracing::debug!` no-ops.
async fn handle_codeberg_webhook(
    State(state): State<AppState>,
    headers: HeaderMap,
    body: Bytes,
) -> impl IntoResponse {
    tracing::info!("Received Codeberg/Forgejo webhook");

    // Verify HMAC-SHA256 signature if a secret is configured. Same
    // primitive as GitHub but a different header name and a raw-hex
    // (no `sha256=` prefix) value, hence its own helper.
    if let Some(ref cb_config) = state.config.codeberg {
        if let Some(ref secret) = cb_config.webhook_secret {
            if let Err(e) = verify_codeberg_signature(&headers, &body, secret) {
                tracing::warn!("Codeberg webhook signature verification failed: {}", e);
                return (StatusCode::UNAUTHORIZED, "Invalid signature");
            }
        }
    }

    let event_type = headers
        .get("X-Gitea-Event")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("unknown");
    let delivery_id = headers
        .get("X-Gitea-Delivery")
        .and_then(|v| v.to_str().ok())
        .map(String::from);

    tracing::info!("Codeberg event type: {}", event_type);

    match event_type {
        "push" => {
            if let Ok(payload) = serde_json::from_slice::<CodebergPushPayload>(&body) {
                let (owner, name) = split_full_name(&payload.repository.full_name);
                let _ = enqueue_repo_jobs(
                    &state,
                    Platform::Codeberg,
                    &owner,
                    &name,
                    &payload.after,
                    JobPriority::Normal,
                    RepoEventKind::Push,
                    None,
                    delivery_id.clone(),
                )
                .await;
            }
        }
        "pull_request" => {
            if let Ok(payload) = serde_json::from_slice::<CodebergPullRequestPayload>(&body) {
                let (owner, name) = split_full_name(&payload.repository.full_name);
                let _ = enqueue_repo_jobs(
                    &state,
                    Platform::Codeberg,
                    &owner,
                    &name,
                    &payload.pull_request.head.sha,
                    JobPriority::High,
                    RepoEventKind::PullRequest,
                    Some(payload.pull_request.number),
                    delivery_id.clone(),
                )
                .await;
            }
        }
        "issue_comment" => {
            // Consultant-mode trigger — mirrors GitHub's handler.
            // TODO(#62): full payload-shape audit against Forgejo
            // docs; the field set below covers the happy path but
            // may need extending for edge cases (review comments
            // dispatched as `issue_comment`, etc.).
            if let Ok(payload) = serde_json::from_slice::<CodebergIssueCommentPayload>(&body) {
                if !modes::is_any_mention(&payload.comment.body) {
                    return (StatusCode::OK, "OK");
                }
                if payload
                    .comment
                    .user
                    .as_ref()
                    .is_some_and(|u| u.login.eq_ignore_ascii_case("echidnabot"))
                {
                    return (StatusCode::OK, "OK");
                }
                let (owner, name) = split_full_name(&payload.repository.full_name);
                let _ = handle_consultant_mention(
                    &state,
                    Platform::Codeberg,
                    &owner,
                    &name,
                    payload.issue.number,
                    &payload.comment.body,
                )
                .await;
            }
        }
        _ => {
            tracing::debug!("Ignoring Codeberg event type: {}", event_type);
        }
    }

    (StatusCode::OK, "OK")
}

#[derive(Clone, Copy, Debug)]
enum RepoEventKind {
    Push,
    PullRequest,
}

/// Enqueue proof jobs for a registered repository.
///
/// `pr_number` is populated for pull_request events (None for push events).
/// Threads through to ProofJob so the result-reporter can comment on the
/// originating PR rather than the commit page.
///
/// `delivery_id` is the platform-specific webhook traceability id —
/// `X-GitHub-Delivery`, `X-Gitlab-Webhook-UUID`, or `X-Hook-UUID` — so a
/// stored job can be correlated back to the exact webhook that produced it.
#[tracing::instrument(
    name = "dispatch.job",
    skip(state),
    fields(
        platform = ?platform,
        repo = %format!("{owner}/{name}"),
        commit = commit,
        pr_number = pr_number,
        priority = ?priority,
    )
)]
// The arguments mirror the platform-neutral webhook payload fields. Keeping
// them explicit makes the tracing fields above and all platform call sites
// auditable side by side.
#[allow(clippy::too_many_arguments)]
async fn enqueue_repo_jobs(
    state: &AppState,
    platform: Platform,
    owner: &str,
    name: &str,
    commit: &str,
    priority: JobPriority,
    event_kind: RepoEventKind,
    pr_number: Option<u64>,
    delivery_id: Option<String>,
) -> Result<()> {
    let repo = match state
        .store
        .get_repository_by_name(platform, owner, name)
        .await?
    {
        Some(repo) => repo,
        None => {
            tracing::info!("Repository not registered: {}/{}", owner, name);
            return Ok(());
        }
    };

    if !repo.enabled {
        tracing::info!("Repository {} is disabled", repo.full_name());
        return Ok(());
    }

    // Determine bot mode via cascade:
    //   1. target-repo `.machine_readable/bot_directives/echidnabot.a2ml`
    //      (or `all.a2ml`) — fetched via PlatformAdapter::get_file_contents
    //   2. `repositories.mode` column (per-repo)
    //   3. `BotMode::default()` (= Verifier)
    //
    // Directive fetch is best-effort: an API error or missing file
    // returns None and the cascade falls through to the DB column.
    let directive_content = match crate::adapters::build_adapter(&state.config, repo.platform) {
        Ok(adapter) => {
            let api_repo_id = RepoId {
                platform: repo.platform,
                owner: repo.owner.clone(),
                name: repo.name.clone(),
            };
            modes::fetch_directive_via_adapter(adapter.as_ref(), &api_repo_id, None).await
        }
        Err(e) => {
            tracing::debug!("No adapter for directive fetch ({}); using DB cascade", e);
            None
        }
    };
    let mode = modes::resolve_mode_with_daemon_default(
        &repo,
        directive_content.as_deref(),
        state.mode_selector.default_mode,
    );
    let is_pr = matches!(event_kind, RepoEventKind::PullRequest);

    tracing::info!(
        "Bot mode: {} (repo: {}, event: {})",
        mode,
        repo.full_name(),
        if is_pr { "pull_request" } else { "push" },
    );

    // Consultant mode only triggers on explicit @echidnabot mentions
    if !modes::should_auto_trigger(mode, is_pr) {
        tracing::info!(
            "Mode {} does not auto-trigger for this event; skipping",
            mode,
        );
        return Ok(());
    }

    let should_enqueue = match event_kind {
        RepoEventKind::Push => repo.check_on_push,
        RepoEventKind::PullRequest => repo.check_on_pr,
    };

    if !should_enqueue {
        return Ok(());
    }

    for prover in &repo.enabled_provers {
        let job = ProofJob::new(repo.id, commit.to_string(), prover.clone(), Vec::new())
            .with_priority(priority)
            .with_context(pr_number, delivery_id.clone());
        let record = ProofJobRecord::from(job.clone());
        state.store.create_job(&record).await?;
        let _ = state.scheduler.enqueue(job).await?;
    }

    tracing::info!(
        "Enqueued {} job(s) for {} in {} mode",
        repo.enabled_provers.len(),
        repo.full_name(),
        mode,
    );

    Ok(())
}

/// Phase 6 — Consultant mode Q&A handler.
///
/// Triggered by `issue_comment` events that contain an `@echidnabot`
/// mention. Filters by mode (silent unless repo is in Consultant mode)
/// and posts a grounded response built from local DB state plus an
/// optional LLM enrichment via BoJ's model-router-mcp cartridge.
///
/// LLM source (per Bit 6(b) decision, locked 2026-04-25 to option (a)):
/// route through the BoJ cartridge. If BoJ is unreachable (currently
/// the case per echidnabot AGENTIC.a2ml [exceptions.boj-only-mcp]), the
/// handler degrades to the local-data response only and notes the
/// degraded state in the comment.
async fn handle_consultant_mention(
    state: &AppState,
    platform: Platform,
    owner: &str,
    name: &str,
    pr_number: u64,
    body: &str,
) -> Result<()> {
    let repo = match state
        .store
        .get_repository_by_name(platform, owner, name)
        .await?
    {
        Some(r) => r,
        None => {
            tracing::debug!(
                "issue_comment on unregistered repo {}/{} — ignoring",
                owner,
                name
            );
            return Ok(());
        }
    };

    // Phase 7: directive content lookup is still TODO (executor would
    // clone target repo). For now the cascade falls through to DB mode,
    // with the daemon-wide mode_selector as the next fallback.
    let mode =
        modes::resolve_mode_with_daemon_default(&repo, None, state.mode_selector.default_mode);
    if mode != modes::BotMode::Consultant {
        tracing::debug!(
            "@echidnabot mention on {} but mode is {} (not Consultant) — ignoring",
            repo.full_name(),
            mode
        );
        return Ok(());
    }

    let question = modes::extract_question(body);
    tracing::info!(
        "Consultant Q&A on {} PR #{}: {}",
        repo.full_name(),
        pr_number,
        if question.is_empty() {
            "(no question text — ping only)".to_string()
        } else {
            format!("{:.80}", question)
        }
    );

    // Local-data answer: most recent jobs for this PR (filter by
    // pr_number on the per-repo job list since the store doesn't index
    // by PR yet — fine for any reasonable PR-job volume).
    let recent = state
        .store
        .list_jobs_for_repo(repo.id, 50)
        .await
        .unwrap_or_default();
    let pr_jobs: Vec<_> = recent
        .into_iter()
        .filter(|j| j.pr_number == Some(pr_number))
        .take(8)
        .collect();

    let local_answer = build_consultant_summary(&repo, pr_number, &pr_jobs, &question);

    // Try BoJ for an LLM-enriched answer. When BoJ is up + the cartridge
    // is registered, the response includes the BoJ output above the
    // local-data summary. When BoJ is down (current state per the
    // documented exception) we surface that fact and ship local only.
    let final_body =
        match crate::llm::query_boj_q_and_a(state, &repo, pr_number, &question, &pr_jobs).await {
            Ok(boj_response) => format!(
                "{}\n\n---\n\n{}",
                boj_response.trim_end(),
                local_answer.trim_start()
            ),
            Err(err) => {
                tracing::warn!(
                    "BoJ Q&A unavailable ({}) — replying with local data only",
                    err
                );
                format!(
                    "{}\n\n> ℹ️ _LLM-enriched Q&A is currently unavailable \
                 (BoJ-only-MCP exception per AGENTIC.a2ml). Reply above is \
                 grounded in echidnabot's local job store; richer answers will \
                 unlock when BoJ revives._\n",
                    local_answer.trim_end()
                )
            }
        };

    let adapter = crate::adapters::build_adapter(&state.config, repo.platform)?;
    let repo_id = RepoId {
        platform: repo.platform,
        owner: repo.owner.clone(),
        name: repo.name.clone(),
    };
    let pr_id = PrId(pr_number.to_string());
    if let Err(err) = adapter.create_comment(&repo_id, pr_id, &final_body).await {
        tracing::warn!(
            "Consultant create_comment failed for {} PR #{}: {}",
            repo.full_name(),
            pr_number,
            err
        );
    }

    Ok(())
}

/// Build the grounded local-data section of a Consultant response.
fn build_consultant_summary(
    repo: &crate::store::models::Repository,
    pr_number: u64,
    pr_jobs: &[crate::store::models::ProofJobRecord],
    question: &str,
) -> String {
    let mut out = format!(
        "## 🦔 echidnabot · Consultant\n\n\
         **Repo:** `{}` · **PR:** #{}\n\n",
        repo.full_name(),
        pr_number
    );
    if !question.is_empty() {
        out.push_str(&format!(
            "> {}\n\n",
            question.lines().take(6).collect::<Vec<_>>().join("\n> ")
        ));
    }
    if pr_jobs.is_empty() {
        out.push_str(
            "I haven't yet run a verification job against any commit on this PR. \
             Push a change to a watched proof file (e.g. `*.v`, `*.lean`, `*.agda`, \
             `*.thy`, `*.smt2`, `*.mm`) and I'll trigger automatically.\n",
        );
        return out;
    }
    out.push_str("**Most recent verification jobs on this PR:**\n\n");
    for job in pr_jobs {
        let status_glyph = match job.status {
            crate::scheduler::JobStatus::Completed => "✅",
            crate::scheduler::JobStatus::Failed => "❌",
            crate::scheduler::JobStatus::Running => "🔄",
            crate::scheduler::JobStatus::Queued => "⏳",
            crate::scheduler::JobStatus::Cancelled => "⏹️",
        };
        let detail = match (&job.status, &job.error_message) {
            (crate::scheduler::JobStatus::Failed, Some(msg)) => {
                format!(
                    " — {}",
                    msg.lines()
                        .next()
                        .unwrap_or("")
                        .chars()
                        .take(80)
                        .collect::<String>()
                )
            }
            _ => String::new(),
        };
        out.push_str(&format!(
            "- `{:.8}` · **{:?}** · {} {:?}{}\n",
            job.commit_sha, job.prover, status_glyph, job.status, detail
        ));
    }
    out.push('\n');
    out
}

fn split_full_name(full_name: &str) -> (String, String) {
    let mut parts = full_name.splitn(2, '/');
    let owner = parts.next().unwrap_or_default().to_string();
    let name = parts.next().unwrap_or_default().to_string();
    (owner, name)
}

#[derive(Deserialize)]
struct GitHubPushPayload {
    after: String,
    repository: GitHubRepo,
}

#[derive(Deserialize)]
struct GitHubPullRequestPayload {
    pull_request: GitHubPullRequest,
    repository: GitHubRepo,
}

#[derive(Deserialize)]
struct GitHubCheckSuitePayload {
    check_suite: GitHubCheckSuite,
    repository: GitHubRepo,
}

#[derive(Deserialize)]
struct GitHubRepo {
    full_name: String,
}

#[derive(Deserialize)]
struct GitHubPullRequest {
    /// PR number — used to comment back on the originating PR rather
    /// than the commit page.
    number: u64,
    head: GitHubHead,
}

#[derive(Deserialize)]
struct GitHubIssueCommentPayload {
    issue: GitHubIssue,
    comment: GitHubComment,
    repository: GitHubRepo,
}

#[derive(Deserialize)]
struct GitHubIssue {
    number: u64,
}

#[derive(Deserialize)]
struct GitHubComment {
    body: String,
    #[serde(default)]
    user: Option<GitHubUser>,
}

#[derive(Deserialize)]
struct GitHubUser {
    login: String,
    /// `"Bot"` for app/bot authors. We use this to filter out our own
    /// comments before they cause a Consultant-mode self-loop.
    #[serde(rename = "type", default)]
    user_type: Option<String>,
}

#[derive(Deserialize)]
struct GitHubCheckSuite {
    head_sha: String,
}

#[derive(Deserialize)]
struct GitHubHead {
    sha: String,
}

#[derive(Deserialize)]
struct GitLabPushPayload {
    after: String,
    checkout_sha: Option<String>,
    project: GitLabProject,
}

#[derive(Deserialize)]
struct GitLabMergeRequestPayload {
    object_attributes: GitLabMergeAttributes,
    project: GitLabProject,
}

#[derive(Deserialize)]
struct GitLabMergeAttributes {
    last_commit_id: String,
    last_commit: Option<GitLabCommit>,
    /// GitLab's per-project MR identifier (the human-facing !N number).
    /// Equivalent to GitHub's PR number for plumbing purposes.
    iid: Option<u64>,
}

#[derive(Deserialize)]
struct GitLabCommit {
    id: String,
}

#[derive(Deserialize)]
struct GitLabProject {
    path_with_namespace: String,
}

#[derive(Deserialize)]
struct GitLabNotePayload {
    object_attributes: GitLabNoteAttributes,
    project: GitLabProject,
    #[serde(default)]
    user: Option<GitLabUser>,
    /// Present when the note is on a Merge Request. None for issue notes
    /// or commit notes (we only handle MR notes today).
    #[serde(default)]
    merge_request: Option<GitLabMR>,
}

#[derive(Deserialize)]
struct GitLabNoteAttributes {
    note: String,
    /// "MergeRequest" / "Issue" / "Commit" / "Snippet". Filter to
    /// MergeRequest only — that's the only one with a PR-equivalent
    /// to comment back on.
    #[serde(default)]
    noteable_type: Option<String>,
}

#[derive(Deserialize)]
struct GitLabUser {
    username: String,
}

#[derive(Deserialize)]
struct GitLabMR {
    iid: u64,
}

#[derive(Deserialize)]
struct BitbucketPushPayload {
    repository: BitbucketRepo,
    push: BitbucketPush,
}

#[derive(Deserialize)]
struct BitbucketRepo {
    full_name: String,
}

#[derive(Deserialize)]
struct BitbucketPush {
    changes: Vec<BitbucketChange>,
}

#[derive(Deserialize)]
struct BitbucketChange {
    #[serde(rename = "new")]
    new_target: Option<BitbucketTarget>,
}

#[derive(Deserialize)]
struct BitbucketTarget {
    hash: String,
}

#[derive(Deserialize)]
struct BitbucketPRCommentPayload {
    repository: BitbucketRepo,
    pullrequest: BitbucketPullRequestRef,
    comment: BitbucketComment,
    #[serde(default)]
    actor: Option<BitbucketActor>,
}

#[derive(Deserialize)]
struct BitbucketPullRequestRef {
    /// Bitbucket PR id — equivalent to GitHub PR number / GitLab MR iid.
    id: u64,
}

#[derive(Deserialize)]
struct BitbucketComment {
    content: BitbucketContent,
}

#[derive(Deserialize)]
struct BitbucketContent {
    raw: String,
}

#[derive(Deserialize)]
struct BitbucketActor {
    /// Bitbucket can also identify by `nickname` or `account_id`; we use
    /// username for the bot-self filter to be consistent with the other
    /// platforms' conventions.
    #[serde(default)]
    username: String,
}

// --- Codeberg / Forgejo / Gitea payload shapes (issue #62 scaffold) ---
//
// Gitea field naming overlaps GitHub by design — `repository.full_name`,
// `pull_request.head.sha`, `issue.number`, `comment.body` all match.
// Only the *envelope* differs (header names, signature format), so we
// can keep these structs minimal and Gitea-flavoured to make the
// dispatch path readable.

#[derive(Deserialize)]
struct CodebergRepo {
    full_name: String,
}

#[derive(Deserialize)]
struct CodebergPushPayload {
    /// The `after` SHA — same convention as GitHub's push hook.
    after: String,
    repository: CodebergRepo,
}

#[derive(Deserialize)]
struct CodebergPullRequestPayload {
    pull_request: CodebergPullRequest,
    repository: CodebergRepo,
}

#[derive(Deserialize)]
struct CodebergPullRequest {
    /// Per-repo PR index (Gitea's equivalent of GitHub's PR number).
    number: u64,
    head: CodebergPullRequestHead,
}

#[derive(Deserialize)]
struct CodebergPullRequestHead {
    sha: String,
}

#[derive(Deserialize)]
struct CodebergIssueCommentPayload {
    repository: CodebergRepo,
    issue: CodebergIssueRef,
    comment: CodebergComment,
}

#[derive(Deserialize)]
struct CodebergIssueRef {
    /// Per-repo issue/PR number (Gitea unifies PRs into the Issues
    /// surface — same payload shape on both).
    number: u64,
}

#[derive(Deserialize)]
struct CodebergComment {
    body: String,
    #[serde(default)]
    user: Option<CodebergUser>,
}

#[derive(Deserialize)]
struct CodebergUser {
    /// Username (Gitea's "login" field, kept as `login` for
    /// GitHub-payload-style readability).
    login: String,
}

/// Verify Codeberg / Forgejo / Gitea webhook signature.
///
/// Wire format: `X-Gitea-Signature: <hex>` — raw HMAC-SHA256 hex of
/// the request body keyed by the configured webhook secret. **No**
/// `sha256=` prefix (that's the GitHub format). The Codeberg fork
/// reuses Gitea's header name unchanged, hence the `X-Gitea-*` naming
/// even though we call this the Codeberg adapter.
///
/// TODO(#62): some Forgejo deployments also emit `X-Hub-Signature-256`
/// for GitHub compatibility — fall through to that when
/// `X-Gitea-Signature` is absent so we work against the widest set of
/// instances without per-instance config.
fn verify_codeberg_signature(
    headers: &HeaderMap,
    body: &Bytes,
    secret: &str,
) -> std::result::Result<(), String> {
    let signature = headers
        .get("X-Gitea-Signature")
        .and_then(|v| v.to_str().ok())
        .ok_or_else(|| "Missing X-Gitea-Signature header".to_string())?;

    let signature_bytes =
        hex::decode(signature).map_err(|_| "Invalid hex in signature".to_string())?;

    let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes())
        .map_err(|_| "Invalid secret key".to_string())?;
    mac.update(body);

    mac.verify_slice(&signature_bytes)
        .map_err(|_| "Signature mismatch".to_string())?;

    Ok(())
}

/// Verify GitHub webhook signature (HMAC-SHA256)
fn verify_github_signature(
    headers: &HeaderMap,
    body: &Bytes,
    secret: &str,
) -> std::result::Result<(), String> {
    let signature = headers
        .get("X-Hub-Signature-256")
        .and_then(|v| v.to_str().ok())
        .ok_or_else(|| "Missing X-Hub-Signature-256 header".to_string())?;

    // Signature format: "sha256=<hex>"
    let signature = signature
        .strip_prefix("sha256=")
        .ok_or_else(|| "Invalid signature format".to_string())?;

    let signature_bytes =
        hex::decode(signature).map_err(|_| "Invalid hex in signature".to_string())?;

    // Compute expected signature
    let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes())
        .map_err(|_| "Invalid secret key".to_string())?;
    mac.update(body);

    mac.verify_slice(&signature_bytes)
        .map_err(|_| "Signature mismatch".to_string())?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verify_github_signature() {
        let secret = "test-secret";
        let body = Bytes::from(r#"{"test": "payload"}"#);

        // Compute expected signature
        let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes()).unwrap();
        mac.update(&body);
        let expected = hex::encode(mac.finalize().into_bytes());

        let mut headers = HeaderMap::new();
        headers.insert(
            "X-Hub-Signature-256",
            format!("sha256={}", expected).parse().unwrap(),
        );

        assert!(verify_github_signature(&headers, &body, secret).is_ok());
    }

    #[test]
    fn test_verify_codeberg_signature() {
        let secret = "test-secret";
        let body = Bytes::from(r#"{"test": "payload"}"#);

        // Same HMAC-SHA256 primitive as GitHub, but Codeberg/Forgejo
        // sends the raw hex without the `sha256=` prefix.
        let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes()).unwrap();
        mac.update(&body);
        let expected = hex::encode(mac.finalize().into_bytes());

        let mut headers = HeaderMap::new();
        headers.insert("X-Gitea-Signature", expected.parse().unwrap());

        assert!(verify_codeberg_signature(&headers, &body, secret).is_ok());
    }

    #[test]
    fn test_verify_codeberg_signature_missing_header() {
        let body = Bytes::from(r#"{"test": "payload"}"#);
        let headers = HeaderMap::new();
        assert!(verify_codeberg_signature(&headers, &body, "secret").is_err());
    }

    #[test]
    fn test_verify_codeberg_signature_mismatch() {
        let body = Bytes::from(r#"{"test": "payload"}"#);
        let mut headers = HeaderMap::new();
        // 64 hex chars (valid SHA-256 length) but wrong key.
        headers.insert(
            "X-Gitea-Signature",
            "0000000000000000000000000000000000000000000000000000000000000000"
                .parse()
                .unwrap(),
        );
        assert!(verify_codeberg_signature(&headers, &body, "secret").is_err());
    }
}
