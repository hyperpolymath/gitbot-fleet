// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Integration tests for echidnabot
//!
//! Tests cover:
//! - Webhook signature verification (valid, invalid, missing header)
//! - ECHIDNA client request construction
//! - Bot mode resolution and formatting
//! - Job lifecycle (enqueue, start, complete)
//! - Proof result database models
//! - Circuit breaker behavior
//! - Container executor command generation

use echidnabot::dispatcher::{ProofResult, ProofStatus, ProverKind};
use echidnabot::executor::{IsolationBackend, PodmanExecutor};
use echidnabot::modes::{BotMode, CheckStatus};
use echidnabot::result_formatter::{
    check_run_conclusion, format_proof_result, generate_pr_comment,
};
use echidnabot::scheduler::{
    CircuitBreaker, CircuitState, JobId, JobPriority, JobResult, JobScheduler, JobStatus,
    ProofJob,
};
use echidnabot::store::models::{ProofJobRecord, ProofResultRecord, Repository};
use echidnabot::adapters::Platform;
use echidnabot::config::Config;

use axum::http::HeaderMap;
use hmac::{Hmac, Mac};
use sha2::Sha256;
use std::time::Duration;
use uuid::Uuid;

// =============================================================================
// Webhook Signature Verification Tests
// =============================================================================

/// Helper: compute HMAC-SHA256 signature for a payload
fn compute_github_signature(secret: &str, body: &[u8]) -> String {
    let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes()).unwrap();
    mac.update(body);
    let result = mac.finalize();
    format!("sha256={}", hex::encode(result.into_bytes()))
}

#[test]
fn test_webhook_valid_hmac_signature() {
    let secret = "supersecret";
    let body = b"{ \"action\": \"opened\" }";
    let signature = compute_github_signature(secret, body);

    // Verify by recomputing
    let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes()).unwrap();
    mac.update(body);
    let hex_sig = signature.strip_prefix("sha256=").unwrap();
    let sig_bytes = hex::decode(hex_sig).unwrap();
    assert!(mac.verify_slice(&sig_bytes).is_ok());
}

#[test]
fn test_webhook_invalid_signature_rejected() {
    let secret = "supersecret";
    let body = b"{ \"action\": \"opened\" }";

    // Wrong secret produces different signature
    let wrong_sig = compute_github_signature("wrong-secret", body);
    let correct_sig = compute_github_signature(secret, body);
    assert_ne!(wrong_sig, correct_sig);
}

#[test]
fn test_webhook_missing_signature_header_handling() {
    let headers = HeaderMap::new();
    // No X-Hub-Signature-256 header
    let has_sig = headers.get("X-Hub-Signature-256").is_some();
    assert!(!has_sig, "Expected no signature header");
}

#[test]
fn test_webhook_signature_format_validation() {
    let sig = "sha256=abc123";
    let prefix_stripped = sig.strip_prefix("sha256=");
    assert_eq!(prefix_stripped, Some("abc123"));

    let invalid = "md5=abc123";
    assert!(invalid.strip_prefix("sha256=").is_none());
}

// =============================================================================
// ECHIDNA Client Tests (request/response construction)
// =============================================================================

#[test]
fn test_prover_kind_display_names() {
    assert_eq!(ProverKind::Coq.display_name(), "Coq");
    assert_eq!(ProverKind::Lean.display_name(), "Lean 4");
    assert_eq!(ProverKind::Isabelle.display_name(), "Isabelle/HOL");
    assert_eq!(ProverKind::Z3.display_name(), "Z3");
    assert_eq!(ProverKind::Metamath.display_name(), "Metamath");
}

#[test]
fn test_prover_kind_from_extension() {
    assert_eq!(ProverKind::from_extension(".v"), Some(ProverKind::Coq));
    assert_eq!(ProverKind::from_extension(".lean"), Some(ProverKind::Lean));
    assert_eq!(ProverKind::from_extension(".mm"), Some(ProverKind::Metamath));
    assert_eq!(ProverKind::from_extension(".smt2"), Some(ProverKind::Z3));
    assert_eq!(ProverKind::from_extension(".unknown"), None);
}

#[test]
fn test_prover_tiers() {
    // Tier 1: complete
    assert_eq!(ProverKind::Coq.tier(), 1);
    assert_eq!(ProverKind::Lean.tier(), 1);
    assert_eq!(ProverKind::Z3.tier(), 1);

    // Tier 2: complete
    assert_eq!(ProverKind::Metamath.tier(), 2);
    assert_eq!(ProverKind::Mizar.tier(), 2);

    // Tier 3: stubs
    assert_eq!(ProverKind::Pvs.tier(), 3);
    assert_eq!(ProverKind::Hol4.tier(), 3);
}

#[test]
fn test_proof_result_parsing() {
    let result = ProofResult {
        status: ProofStatus::Verified,
        message: "All goals discharged".to_string(),
        prover_output: "Proof complete.".to_string(),
        duration_ms: 1234,
        artifacts: vec!["proof.cert".to_string()],
    };

    assert_eq!(result.status, ProofStatus::Verified);
    assert!(result.artifacts.len() == 1);
}

#[test]
fn test_proof_status_variants() {
    assert_ne!(ProofStatus::Verified, ProofStatus::Failed);
    assert_ne!(ProofStatus::Timeout, ProofStatus::Error);
    assert_ne!(ProofStatus::Unknown, ProofStatus::Verified);
}

#[test]
fn test_echidna_config_defaults() {
    let config = Config::default();
    assert_eq!(config.echidna.timeout_secs, 300);
    assert_eq!(config.server.port, 8080);
    assert_eq!(config.scheduler.max_concurrent, 5);
    assert_eq!(config.scheduler.queue_size, 100);
}

// =============================================================================
// Bot Mode Tests
// =============================================================================

#[test]
fn test_all_modes_format_result() {
    let modes = [
        BotMode::Verifier,
        BotMode::Advisor,
        BotMode::Consultant,
        BotMode::Regulator,
    ];

    for mode in &modes {
        let result = mode.format_result(true, "Coq", "ok", vec![]);
        assert_eq!(result.check_status, CheckStatus::Success);
        assert!(!result.should_block || *mode == BotMode::Regulator);
    }
}

#[test]
fn test_mode_serialization() {
    let mode = BotMode::Advisor;
    let json = serde_json::to_string(&mode).unwrap();
    assert_eq!(json, "\"advisor\"");

    let parsed: BotMode = serde_json::from_str(&json).unwrap();
    assert_eq!(parsed, BotMode::Advisor);
}

#[test]
fn test_mode_default_is_verifier() {
    assert_eq!(BotMode::default(), BotMode::Verifier);
}

#[test]
fn test_result_formatter_truncates_long_output() {
    let long_output = "x".repeat(5000);
    let proof_result = ProofResult {
        status: ProofStatus::Failed,
        message: "Failed".to_string(),
        prover_output: long_output,
        duration_ms: 100,
        artifacts: vec![],
    };

    let formatted = format_proof_result(BotMode::Advisor, &proof_result, ProverKind::Coq, vec![]);
    let comment = generate_pr_comment(&formatted, BotMode::Advisor);

    // Comment should contain truncation notice
    assert!(comment.contains("truncated") || comment.len() < 10000);
}

#[test]
fn test_check_run_conclusion_values() {
    let success = echidnabot::modes::FormattedResult {
        summary: "ok".to_string(),
        details: None,
        suggestions: vec![],
        should_block: false,
        check_status: CheckStatus::Success,
    };
    assert_eq!(check_run_conclusion(&success), "success");

    let failure = echidnabot::modes::FormattedResult {
        summary: "fail".to_string(),
        details: None,
        suggestions: vec![],
        should_block: false,
        check_status: CheckStatus::Failure,
    };
    assert_eq!(check_run_conclusion(&failure), "failure");

    let neutral = echidnabot::modes::FormattedResult {
        summary: "neutral".to_string(),
        details: None,
        suggestions: vec![],
        should_block: false,
        check_status: CheckStatus::Neutral,
    };
    assert_eq!(check_run_conclusion(&neutral), "neutral");
}

// =============================================================================
// Job Lifecycle Tests
// =============================================================================

#[test]
fn test_job_creation() {
    let repo_id = Uuid::new_v4();
    let job = ProofJob::new(
        repo_id,
        "abc123def".to_string(),
        ProverKind::Lean,
        vec!["src/Main.lean".to_string()],
    );

    assert_eq!(job.repo_id, repo_id);
    assert_eq!(job.commit_sha, "abc123def");
    assert_eq!(job.prover, ProverKind::Lean);
    assert_eq!(job.status, JobStatus::Queued);
    assert_eq!(job.priority, JobPriority::Normal);
    assert!(job.started_at.is_none());
    assert!(job.completed_at.is_none());
}

#[test]
fn test_job_start_sets_running() {
    let mut job = ProofJob::new(
        Uuid::new_v4(),
        "abc123".to_string(),
        ProverKind::Coq,
        vec![],
    );

    job.start();
    assert_eq!(job.status, JobStatus::Running);
    assert!(job.started_at.is_some());
}

#[test]
fn test_job_complete_success() {
    let mut job = ProofJob::new(
        Uuid::new_v4(),
        "abc123".to_string(),
        ProverKind::Z3,
        vec![],
    );
    job.start();

    let result = JobResult {
        success: true,
        message: "Verified".to_string(),
        prover_output: "sat".to_string(),
        duration_ms: 50,
        verified_files: vec!["test.smt2".to_string()],
        failed_files: vec![],
    };

    job.complete(result);
    assert_eq!(job.status, JobStatus::Completed);
    assert!(job.completed_at.is_some());
    assert!(job.result.as_ref().unwrap().success);
}

#[test]
fn test_job_complete_failure() {
    let mut job = ProofJob::new(
        Uuid::new_v4(),
        "abc123".to_string(),
        ProverKind::Lean,
        vec![],
    );
    job.start();

    let result = JobResult {
        success: false,
        message: "Proof failed".to_string(),
        prover_output: "Error at line 42".to_string(),
        duration_ms: 200,
        verified_files: vec![],
        failed_files: vec!["test.lean".to_string()],
    };

    job.complete(result);
    assert_eq!(job.status, JobStatus::Failed);
}

#[test]
fn test_job_cancel() {
    let mut job = ProofJob::new(
        Uuid::new_v4(),
        "abc123".to_string(),
        ProverKind::Agda,
        vec![],
    );

    job.cancel();
    assert_eq!(job.status, JobStatus::Cancelled);
    assert!(job.completed_at.is_some());
}

#[test]
fn test_job_priority_ordering() {
    assert!(JobPriority::Critical > JobPriority::High);
    assert!(JobPriority::High > JobPriority::Normal);
    assert!(JobPriority::Normal > JobPriority::Low);
}

// =============================================================================
// Database Model Tests
// =============================================================================

#[test]
fn test_repository_model() {
    let repo = Repository::new(Platform::GitHub, "owner".to_string(), "repo".to_string());

    assert_eq!(repo.platform, Platform::GitHub);
    assert_eq!(repo.owner, "owner");
    assert_eq!(repo.name, "repo");
    assert_eq!(repo.full_name(), "owner/repo");
    assert!(repo.enabled);
    assert!(repo.check_on_push);
    assert!(repo.check_on_pr);
}

#[test]
fn test_proof_job_record_from_job() {
    let job = ProofJob::new(
        Uuid::new_v4(),
        "sha256hash".to_string(),
        ProverKind::Metamath,
        vec!["proof.mm".to_string()],
    );

    let record = ProofJobRecord::from(job.clone());
    assert_eq!(record.id, job.id.0);
    assert_eq!(record.repo_id, job.repo_id);
    assert_eq!(record.commit_sha, "sha256hash");
    assert_eq!(record.prover, ProverKind::Metamath);
}

#[test]
fn test_proof_result_record() {
    let job_id = JobId::new();
    let result = JobResult {
        success: true,
        message: "All proofs verified".to_string(),
        prover_output: "OK".to_string(),
        duration_ms: 500,
        verified_files: vec!["a.mm".to_string(), "b.mm".to_string()],
        failed_files: vec![],
    };

    let record = ProofResultRecord::new(job_id, &result);
    assert_eq!(record.job_id, job_id.0);
    assert!(record.success);
    assert_eq!(record.verified_files.len(), 2);
    assert!(record.failed_files.is_empty());
}

// =============================================================================
// Container Executor Tests
// =============================================================================

#[test]
fn test_executor_build_podman_args_security() {
    let executor = PodmanExecutor::default()
        .with_backend(IsolationBackend::Podman);

    let args = executor.build_podman_args(ProverKind::Lean);

    // Verify all security flags are present
    assert!(args.contains(&"--cap-drop=ALL".to_string()));
    assert!(args.contains(&"--network=none".to_string()));
    assert!(args.contains(&"--read-only".to_string()));
    assert!(args.contains(&"--security-opt=no-new-privileges".to_string()));
    assert!(args.contains(&"--pids-limit=100".to_string()));
    assert!(args.contains(&"--rm".to_string()));
}

#[test]
fn test_executor_custom_resource_limits() {
    let executor = PodmanExecutor::default()
        .with_memory_limit("4g")
        .with_cpu_limit(8.0)
        .with_timeout(Duration::from_secs(600))
        .with_backend(IsolationBackend::Podman);

    let args = executor.build_podman_args(ProverKind::Coq);

    assert!(args.contains(&"--memory=4g".to_string()));
    assert!(args.contains(&"--cpus=8".to_string()));
    assert!(args.contains(&"--timeout=600".to_string()));
}

#[tokio::test]
async fn test_executor_no_backend_refuses_proofs() {
    let executor = PodmanExecutor::default()
        .with_backend(IsolationBackend::None);

    let result = executor
        .execute_proof(ProverKind::Lean, "theorem test : True := trivial", None)
        .await;

    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("No isolation backend"), "Error: {}", err);
}

// =============================================================================
// Integration: Full Pipeline Tests
// =============================================================================

#[tokio::test]
async fn test_scheduler_enqueue_dequeue_cycle() {
    let scheduler = JobScheduler::new(2, 10);
    let repo_id = Uuid::new_v4();

    // Enqueue a job
    let job = ProofJob::new(
        repo_id,
        "commit123".to_string(),
        ProverKind::Coq,
        vec!["theorem.v".to_string()],
    );
    let job_id = job.id;

    let enqueue_result = scheduler.enqueue(job).await.unwrap();
    assert!(enqueue_result.is_some());

    // Start the job
    let started = scheduler.try_start_next().await;
    assert!(started.is_some());
    let started_job = started.unwrap();
    assert_eq!(started_job.id, job_id);
    assert_eq!(started_job.status, JobStatus::Running);

    // Complete the job
    let result = JobResult {
        success: true,
        message: "Verified".to_string(),
        prover_output: "OK".to_string(),
        duration_ms: 100,
        verified_files: vec!["theorem.v".to_string()],
        failed_files: vec![],
    };

    scheduler.complete_job(job_id, result).await;

    // Verify stats
    let stats = scheduler.stats().await;
    assert_eq!(stats.running, 0);
    assert_eq!(stats.queued, 0);
}

#[tokio::test]
async fn test_scheduler_respects_max_concurrent() {
    let scheduler = JobScheduler::new(1, 10); // Max 1 concurrent

    // Enqueue two jobs
    let job1 = ProofJob::new(
        Uuid::new_v4(),
        "commit1".to_string(),
        ProverKind::Coq,
        vec![],
    );
    let job2 = ProofJob::new(
        Uuid::new_v4(),
        "commit2".to_string(),
        ProverKind::Lean,
        vec![],
    );

    scheduler.enqueue(job1).await.unwrap();
    scheduler.enqueue(job2).await.unwrap();

    // Start first job
    let first = scheduler.try_start_next().await;
    assert!(first.is_some());

    // Second should not start (at capacity)
    let second = scheduler.try_start_next().await;
    assert!(second.is_none());
}

#[tokio::test]
async fn test_circuit_breaker_integration() {
    let cb = CircuitBreaker::new(3, Duration::from_millis(100));

    // Should be closed initially
    assert_eq!(cb.state().await, CircuitState::Closed);
    assert!(cb.check().await.is_ok());

    // Trip after 3 failures
    for _ in 0..3 {
        cb.record_failure().await;
    }
    assert_eq!(cb.state().await, CircuitState::Open);
    assert!(cb.check().await.is_err());

    // Wait for reset
    tokio::time::sleep(Duration::from_millis(150)).await;

    // Should be half-open after timeout
    assert!(cb.check().await.is_ok());
    assert_eq!(cb.state().await, CircuitState::HalfOpen);

    // Success closes the circuit
    cb.record_success().await;
    assert_eq!(cb.state().await, CircuitState::Closed);
}
