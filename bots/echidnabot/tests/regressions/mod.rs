// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Regression tests — one test per confirmed bug.
//!
//! Testing taxonomy category: Regression.
//! Add a new test here for every bug reported or discovered, keyed by
//! commit hash or issue reference. Never remove these.

use echidnabot::dispatcher::ProverKind;
use echidnabot::scheduler::{JobPriority, JobScheduler, ProofJob};
use echidnabot::store::models::goal_fingerprint;
use echidnabot::trust::solver_integrity::{IntegrityStatus, SolverIntegrity};
use std::collections::HashMap;
use uuid::Uuid;

/// Regression: commit 42e7bde — prover_key() used format!("{:?}", prover)
/// which on the ProverSlug newtype produced `ProverSlug("coq")` not `"coq"`,
/// causing all manifest lookups to return Unchecked instead of Verified/Tampered.
#[test]
fn regression_prover_key_debug_format_bug_42e7bde() {
    let mut manifest = HashMap::new();
    manifest.insert(
        "coq".to_string(),
        "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890".to_string(),
    );
    let integrity = SolverIntegrity::with_manifest(manifest);
    let report = integrity.verify(
        &ProverKind::new("coq"),
        "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890",
        "/usr/bin/coqc",
    );
    assert_eq!(
        report.status,
        IntegrityStatus::Verified,
        "prover_key must produce 'coq' not 'ProverSlug(\"coq\")' — regression 42e7bde"
    );
}

/// Regression: duplicate detection must consider prover, not just (repo, commit).
/// Different provers on the same commit are NOT duplicates.
#[tokio::test]
async fn regression_duplicate_detection_respects_prover() {
    let sched = JobScheduler::new(4, 20);
    let repo = Uuid::new_v4();

    let j1 = ProofJob::new(repo, "sha_dup".to_string(), ProverKind::new("coq"), vec![]);
    let j2 = ProofJob::new(repo, "sha_dup".to_string(), ProverKind::new("lean"), vec![]); // diff prover
    let j3 = ProofJob::new(repo, "sha_dup".to_string(), ProverKind::new("coq"), vec![]); // true duplicate

    assert!(sched.enqueue(j1).await.unwrap().is_some(), "first coq job must be accepted");
    assert!(sched.enqueue(j2).await.unwrap().is_some(), "lean job on same commit must be accepted");
    assert!(sched.enqueue(j3).await.unwrap().is_none(), "duplicate (coq, same sha) must be rejected");
}

/// Regression: goal_fingerprint must always be exactly 64 hex chars (SHA-256).
#[test]
fn regression_goal_fingerprint_always_64_chars() {
    for input in &["", "x", "forall n, n + 0 = n", &"a".repeat(10_000)] {
        let f = goal_fingerprint(input);
        assert_eq!(
            f.len(),
            64,
            "goal_fingerprint must be 64 hex chars for input len {}", input.len()
        );
    }
}

/// Regression: high-priority jobs must run before low-priority jobs regardless
/// of enqueue order.
#[tokio::test]
async fn regression_priority_queue_ordering() {
    let sched = JobScheduler::new(1, 10);
    let repo = Uuid::new_v4();

    let low = ProofJob::new(repo, "low_sha".to_string(), ProverKind::new("coq"), vec![])
        .with_priority(JobPriority::Low);
    let high = ProofJob::new(repo, "high_sha".to_string(), ProverKind::new("lean"), vec![])
        .with_priority(JobPriority::High);

    sched.enqueue(low).await.unwrap();
    sched.enqueue(high).await.unwrap();

    let started = sched.try_start_next().await.unwrap();
    assert_eq!(
        started.commit_sha, "high_sha",
        "high-priority must run first regardless of enqueue order"
    );
}

/// Regression: rate limiter must treat IPv4 and IPv6 as independent buckets.
#[test]
fn regression_rate_limiter_v4_v6_independent() {
    use echidnabot::api::rate_limit::WebhookRateLimiter;
    use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

    let limiter = WebhookRateLimiter::new(1);
    let v4 = IpAddr::V4(Ipv4Addr::new(1, 1, 1, 1));
    let v6 = IpAddr::V6(Ipv6Addr::new(0x2606, 0x4700, 0x4700, 0, 0, 0, 0, 0x1111));

    // Exhaust v4
    limiter.check_ip(v4);
    assert!(!limiter.check_ip(v4), "v4 should be exhausted");

    // v6 must still have capacity
    assert!(limiter.check_ip(v6), "v6 bucket must be independent of v4");
}
