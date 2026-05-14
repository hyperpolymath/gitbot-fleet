// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! End-to-end integration tests for echidnabot's webhook flow.
//!
//! # What this exercises (the real chain)
//!
//! ```text
//! TestServer (axum-test) → POST /webhooks/github
//!   → real axum handler (handle_github_webhook)
//!   → real HMAC-SHA256 signature check
//!   → real JSON payload parsing (GitHubPushPayload / GitHubPullRequestPayload)
//!   → real event routing (push / pull_request / ping / unknown)
//!   → real store lookup (SqliteStore, sqlite::memory:)
//!   → real ProofJob construction and enqueue into JobScheduler
//!   → real ProofJobRecord persisted to in-memory SQLite
//! ```
//!
//! # What is mocked / bounded
//!
//! - SQLite: `sqlite::memory:` — real SQLite engine, no file I/O
//! - Echidna HTTP API: wiremock server (unused during webhook handling; present
//!   for URL injection correctness)
//! - GitHub API (octocrab): not wired into AppState — see limitation note below
//!
//! # Architectural limitation
//!
//! The **scheduler loop** (`run_scheduler_loop` in `main.rs`) that calls
//! `EchidnaClient::verify_proof` and subsequently posts a GitHub Check Run is
//! a private `async fn` spawned only inside `main::serve()`.  It is NOT part of
//! `AppState` and NOT reachable from a test without modifying production code.
//!
//! Consequence: these tests validate the webhook → dispatcher boundary
//! exhaustively but cannot exercise the proof execution or check-run posting
//! without a production-code refactor.  See the TODO block at the bottom.

use axum_test::TestServer;
use bytes::Bytes;
use echidnabot::adapters::Platform;
use echidnabot::api::webhook_router;
use echidnabot::api::webhooks::AppState;
use echidnabot::config::{Config, DatabaseConfig, EchidnaApiMode, EchidnaConfig, GitHubConfig, SchedulerConfig};
use echidnabot::dispatcher::ProverKind;
use echidnabot::scheduler::JobScheduler;
use echidnabot::store::models::Repository;
use echidnabot::store::{SqliteStore, Store};
use hmac::{Hmac, Mac};
use sha2::Sha256;
use std::sync::Arc;
use wiremock::matchers::{method, path};
use wiremock::{Mock, MockServer, ResponseTemplate};

// =============================================================================
// Helper: compute GitHub-format HMAC-SHA256 signature ("sha256=<hex>")
// =============================================================================

fn github_signature(secret: &str, body: &[u8]) -> String {
    let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes())
        .expect("HMAC accepts any key length");
    mac.update(body);
    let result = mac.finalize();
    format!("sha256={}", hex::encode(result.into_bytes()))
}

// =============================================================================
// Helper: build AppState + axum-test TestServer
//
// All dependencies are injectable:
//   - `sqlite::memory:` — no file I/O
//   - `EchidnaConfig.endpoint` / `rest_endpoint` — plain String fields
//   - webhook secret — via `GitHubConfig.webhook_secret`
// =============================================================================

async fn build_test_server(
    webhook_secret: &str,
    echidna_base_url: &str,
) -> (TestServer, Arc<dyn Store>, Arc<JobScheduler>) {
    let store: Arc<dyn Store> = Arc::new(
        SqliteStore::new("sqlite::memory:")
            .await
            .expect("in-memory SQLite should initialise"),
    );

    let scheduler = Arc::new(JobScheduler::new(
        /* max_concurrent */ 4,
        /* queue_size */ 64,
    ));

    let config = Config {
        database: DatabaseConfig {
            url: "sqlite::memory:".to_string(),
            max_connections: 1,
        },
        echidna: EchidnaConfig {
            // Both URL fields are injectable plain Strings — no global state
            endpoint: format!("{}/graphql", echidna_base_url),
            rest_endpoint: echidna_base_url.to_string(),
            mode: EchidnaApiMode::Rest,
            timeout_secs: 5,
        },
        github: Some(GitHubConfig {
            app_id: None,
            private_key_path: None,
            token: None,
            webhook_secret: Some(webhook_secret.to_string()),
        }),
        gitlab: None,
        scheduler: SchedulerConfig {
            max_concurrent: 4,
            queue_size: 64,
        },
        ..Config::default()
    };

    let app_state = AppState {
        config: Arc::new(config),
        store: store.clone(),
        scheduler: scheduler.clone(),
    };

    let router = webhook_router().with_state(app_state);
    let server = TestServer::new(router).expect("TestServer::new should succeed");

    (server, store, scheduler)
}

// =============================================================================
// Helper: register a repository in the store
//
// The webhook handler silently does nothing for repos not in the store, so
// registration is required for tests that expect a job to be enqueued.
// =============================================================================

async fn register_repo(
    store: &Arc<dyn Store>,
    owner: &str,
    name: &str,
    provers: Vec<ProverKind>,
) {
    let mut repo = Repository::new(Platform::GitHub, owner.to_string(), name.to_string());
    repo.enabled_provers = provers;
    repo.check_on_push = true;
    repo.check_on_pr = true;
    repo.enabled = true;
    store
        .create_repository(&repo)
        .await
        .expect("store should accept repository");
}

// =============================================================================
// Helper: GitHub push payload
// =============================================================================

fn push_payload_json(owner: &str, repo: &str, commit_sha: &str) -> serde_json::Value {
    serde_json::json!({
        "ref": "refs/heads/main",
        "before": "0000000000000000000000000000000000000000",
        "after": commit_sha,
        "repository": {
            "id": 123456789,
            "full_name": format!("{}/{}", owner, repo),
            "name": repo,
            "owner": { "login": owner }
        },
        "commits": [{
            "id": commit_sha,
            "message": "add theorem",
            "added": [],
            "removed": [],
            "modified": ["src/Theorem.lean"]
        }],
        "head_commit": {
            "id": commit_sha,
            "modified": ["src/Theorem.lean"]
        },
        "pusher": { "name": "hyperpolymath", "email": "test@example.com" }
    })
}

// =============================================================================
// E2E Test 1:
//   Valid push event, registered repo → 200 + job enqueued in scheduler + persisted
//
// Real components exercised:
//   - HMAC signature verification (real HMAC-SHA256)
//   - Push payload JSON parsing (GitHubPushPayload)
//   - Store lookup (SqliteStore, sqlite::memory:)
//   - ProofJob construction and JobScheduler.enqueue()
//   - ProofJobRecord INSERT in SQLite
// =============================================================================

#[tokio::test]
async fn webhook_push_enqueues_proof_job_and_persists_to_store() {
    // Echidna mock: needed for URL injection, not called by the webhook handler
    let echidna_mock = MockServer::start().await;
    Mock::given(method("GET"))
        .and(path("/api/health"))
        .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({"ok": true})))
        .mount(&echidna_mock)
        .await;

    let secret = "test-webhook-secret-echidnabot";
    let (server, store, scheduler) =
        build_test_server(secret, &echidna_mock.uri()).await;

    // Register the repository with Lean as the prover
    let owner = "hyperpolymath";
    let repo_name = "lean-proofs";
    register_repo(&store, owner, repo_name, vec![ProverKind::Lean]).await;

    // Construct payload and sign it
    let commit_sha = "deadbeefdeadbeefdeadbeefdeadbeef12345678";
    let payload = push_payload_json(owner, repo_name, commit_sha);
    let body_bytes: Bytes = serde_json::to_vec(&payload).unwrap().into();
    let signature = github_signature(secret, &body_bytes);

    // POST /webhooks/github with real HMAC signature
    let response = server
        .post("/webhooks/github")
        .add_header("Content-Type", "application/json")
        .add_header("X-GitHub-Event", "push")
        .add_header("X-Hub-Signature-256", signature.as_str())
        .bytes(body_bytes)
        .await;

    // Handler must return 200
    response.assert_status_ok();

    // One job should be in the scheduler (one prover configured: Lean)
    let stats = scheduler.stats().await;
    assert_eq!(
        stats.queued + stats.running,
        1,
        "exactly one ProofJob should be in the scheduler"
    );

    // Job should also be persisted to the in-memory SQLite store
    let db_repo = store
        .get_repository_by_name(Platform::GitHub, owner, repo_name)
        .await
        .expect("store query must not fail")
        .expect("repository must be present in store");

    let jobs = store
        .list_jobs_for_repo(db_repo.id, 10)
        .await
        .expect("job listing must not fail");

    assert_eq!(jobs.len(), 1, "one job should be persisted");
    assert_eq!(jobs[0].commit_sha, commit_sha, "job must reference the pushed commit");
    assert_eq!(jobs[0].prover, ProverKind::Lean, "prover must match repository config");
}

// =============================================================================
// E2E Test 2:
//   Wrong HMAC signature → 401, no job enqueued
// =============================================================================

#[tokio::test]
async fn webhook_push_invalid_hmac_returns_401_no_job() {
    let echidna_mock = MockServer::start().await;
    let secret = "correct-secret";
    let (server, store, scheduler) =
        build_test_server(secret, &echidna_mock.uri()).await;

    register_repo(&store, "hyperpolymath", "lean-proofs", vec![ProverKind::Lean]).await;

    let body_bytes: Bytes =
        serde_json::to_vec(&push_payload_json("hyperpolymath", "lean-proofs", "cafebabe"))
            .unwrap()
            .into();
    // Sign with the WRONG secret
    let bad_sig = github_signature("this-is-not-the-secret", &body_bytes);

    let response = server
        .post("/webhooks/github")
        .add_header("Content-Type", "application/json")
        .add_header("X-GitHub-Event", "push")
        .add_header("X-Hub-Signature-256", bad_sig.as_str())
        .bytes(body_bytes)
        .await;

    response.assert_status_unauthorized();

    let stats = scheduler.stats().await;
    assert_eq!(
        stats.queued + stats.running,
        0,
        "no job must be enqueued after signature rejection"
    );
}

// =============================================================================
// E2E Test 3:
//   Missing X-Hub-Signature-256 header → 401
//
// The handler requires a signature when `webhook_secret` is configured.
// =============================================================================

#[tokio::test]
async fn webhook_push_missing_signature_header_returns_401() {
    let echidna_mock = MockServer::start().await;
    let (server, store, _scheduler) =
        build_test_server("some-secret", &echidna_mock.uri()).await;

    register_repo(&store, "hyperpolymath", "lean-proofs", vec![ProverKind::Lean]).await;

    let body_bytes: Bytes =
        serde_json::to_vec(&push_payload_json("hyperpolymath", "lean-proofs", "abc123"))
            .unwrap()
            .into();

    let response = server
        .post("/webhooks/github")
        .add_header("Content-Type", "application/json")
        .add_header("X-GitHub-Event", "push")
        // Deliberately omit X-Hub-Signature-256
        .bytes(body_bytes)
        .await;

    response.assert_status_unauthorized();
}

// =============================================================================
// E2E Test 4:
//   Unregistered repository → 200 (GitHub expects 200), no job enqueued
//
// The handler must never return a non-2xx for a valid signed event even if the
// repo isn't registered — it just does nothing.
// =============================================================================

#[tokio::test]
async fn webhook_push_unregistered_repo_returns_200_no_job() {
    let echidna_mock = MockServer::start().await;
    let secret = "another-secret";
    let (server, _store, scheduler) =
        build_test_server(secret, &echidna_mock.uri()).await;
    // No repo registered in the store

    let payload = push_payload_json("unknown-owner", "unknown-repo", "aabbccddeeff");
    let body_bytes: Bytes = serde_json::to_vec(&payload).unwrap().into();
    let signature = github_signature(secret, &body_bytes);

    let response = server
        .post("/webhooks/github")
        .add_header("Content-Type", "application/json")
        .add_header("X-GitHub-Event", "push")
        .add_header("X-Hub-Signature-256", signature.as_str())
        .bytes(body_bytes)
        .await;

    response.assert_status_ok();

    let stats = scheduler.stats().await;
    assert_eq!(
        stats.queued + stats.running,
        0,
        "no job must be enqueued for an unregistered repository"
    );
}

// =============================================================================
// E2E Test 5:
//   pull_request event → 200, one job enqueued at High priority
//
// Uses the real GitHubPullRequestPayload parsing path.
// =============================================================================

#[tokio::test]
async fn webhook_pull_request_enqueues_job() {
    let echidna_mock = MockServer::start().await;
    let secret = "pr-secret";
    let (server, store, scheduler) =
        build_test_server(secret, &echidna_mock.uri()).await;

    register_repo(&store, "hyperpolymath", "lean-proofs", vec![ProverKind::Lean]).await;

    let head_sha = "fedcba9876543210fedcba9876543210fedcba98";
    let payload = serde_json::json!({
        "action": "opened",
        "number": 42,
        "pull_request": {
            "head": { "sha": head_sha },
            "base": { "sha": "0000000000000000000000000000000000000000" }
        },
        "repository": {
            "full_name": "hyperpolymath/lean-proofs",
            "name": "lean-proofs",
            "owner": { "login": "hyperpolymath" }
        }
    });
    let body_bytes: Bytes = serde_json::to_vec(&payload).unwrap().into();
    let signature = github_signature(secret, &body_bytes);

    let response = server
        .post("/webhooks/github")
        .add_header("Content-Type", "application/json")
        .add_header("X-GitHub-Event", "pull_request")
        .add_header("X-Hub-Signature-256", signature.as_str())
        .bytes(body_bytes)
        .await;

    response.assert_status_ok();

    let stats = scheduler.stats().await;
    assert_eq!(
        stats.queued + stats.running,
        1,
        "one job should be enqueued for the pull_request event"
    );
}

// =============================================================================
// E2E Test 6:
//   ping event → 200, no job enqueued (webhook configuration confirmation)
// =============================================================================

#[tokio::test]
async fn webhook_ping_returns_200_no_job() {
    let echidna_mock = MockServer::start().await;
    let secret = "ping-secret";
    let (server, _store, scheduler) =
        build_test_server(secret, &echidna_mock.uri()).await;

    let payload = serde_json::json!({ "zen": "Keep it logically simple.", "hook_id": 1 });
    let body_bytes: Bytes = serde_json::to_vec(&payload).unwrap().into();
    let signature = github_signature(secret, &body_bytes);

    let response = server
        .post("/webhooks/github")
        .add_header("Content-Type", "application/json")
        .add_header("X-GitHub-Event", "ping")
        .add_header("X-Hub-Signature-256", signature.as_str())
        .bytes(body_bytes)
        .await;

    response.assert_status_ok();

    let stats = scheduler.stats().await;
    assert_eq!(stats.queued + stats.running, 0, "ping must not enqueue any job");
}

// =============================================================================
// E2E Test 7:
//   Multi-prover repository → one job enqueued per prover
// =============================================================================

#[tokio::test]
async fn webhook_push_multi_prover_enqueues_one_job_per_prover() {
    let echidna_mock = MockServer::start().await;
    let secret = "multi-prover-secret";
    let (server, store, scheduler) =
        build_test_server(secret, &echidna_mock.uri()).await;

    // Register with two provers
    register_repo(
        &store,
        "hyperpolymath",
        "mixed-proofs",
        vec![ProverKind::Lean, ProverKind::Coq],
    )
    .await;

    let payload = push_payload_json("hyperpolymath", "mixed-proofs", "111aaa222bbb333ccc");
    let body_bytes: Bytes = serde_json::to_vec(&payload).unwrap().into();
    let signature = github_signature(secret, &body_bytes);

    let response = server
        .post("/webhooks/github")
        .add_header("Content-Type", "application/json")
        .add_header("X-GitHub-Event", "push")
        .add_header("X-Hub-Signature-256", signature.as_str())
        .bytes(body_bytes)
        .await;

    response.assert_status_ok();

    let stats = scheduler.stats().await;
    assert_eq!(
        stats.queued + stats.running,
        2,
        "two jobs should be enqueued — one for Lean, one for Coq"
    );
}

// =============================================================================
// E2E Test 8:
//   Unknown event type → 200, no job (graceful ignore)
// =============================================================================

#[tokio::test]
async fn webhook_unknown_event_returns_200_no_job() {
    let echidna_mock = MockServer::start().await;
    let secret = "unknown-event-secret";
    let (server, _store, scheduler) =
        build_test_server(secret, &echidna_mock.uri()).await;

    let payload = serde_json::json!({ "some": "data" });
    let body_bytes: Bytes = serde_json::to_vec(&payload).unwrap().into();
    let signature = github_signature(secret, &body_bytes);

    let response = server
        .post("/webhooks/github")
        .add_header("Content-Type", "application/json")
        .add_header("X-GitHub-Event", "star") // valid GitHub event, not handled
        .add_header("X-Hub-Signature-256", signature.as_str())
        .bytes(body_bytes)
        .await;

    response.assert_status_ok();

    let stats = scheduler.stats().await;
    assert_eq!(stats.queued + stats.running, 0, "unknown events must not enqueue jobs");
}

// =============================================================================
// TODO: Remaining gap — echidna HTTP call and GitHub Check Run posting
//
// The webhook handler's job is complete at job-enqueue.  The downstream path:
//
//   ProofJob → `run_scheduler_loop` → EchidnaClient::health_check()
//                                   → EchidnaClient::prover_status()
//                                   → EchidnaClient::verify_proof()
//                                   → GitHubAdapter::create_check_run()  (via octocrab)
//
// is in `main.rs::run_scheduler_loop` (a private `async fn`), which is spawned
// only from `main::serve()`.
//
// To exercise the full chain end-to-end in a test, the recommended refactoring
// (without breaking production code) would be to:
//
//   1. Make `run_scheduler_loop` a `pub async fn` in `lib.rs` (or a new module)
//      so tests can `tokio::spawn` it with injected `EchidnaClient` and `Config`.
//   2. Move the octocrab client construction into `AppState` (or a `GitHubClient`
//      trait similar to the `Store` trait) so the base URL can be overridden for
//      tests to point at a wiremock server instead of `api.github.com`.
//
// The wiremock stubs below are ready and correct; they just aren't reachable
// from the test without the above refactor.
//
// Untested today:
//   - EchidnaClient::health_check() path (POST /graphql { __typename })
//   - EchidnaClient::prover_status() path (query ProverStatus)
//   - EchidnaClient::verify_proof() path (mutation VerifyProof)
//   - GitHubAdapter::create_check_run() → octocrab → POST /repos/.../check-runs
//   - result_formatter output appearing in the GitHub Check Run body
//   - circuit breaker tripping on echidna HTTP failures
// =============================================================================
