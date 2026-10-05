// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Criterion benchmarks for echidnabot hot paths.
//!
//! Six Sigma classification thresholds (per standards repo):
//!   Unacceptable  > 50% regression from baseline → hard fail
//!   Acceptable    20–50% regression               → soft fail / reviewer approval
//!   Ordinary      within ±20% of baseline         → pass
//!   Extraordinary > 20% improvement               → investigate before updating baseline
//!
//! Baselines are the mean of the last 10 CI runs on main.
//! Run locally: `cargo bench`
//! Compare: `cargo bench -- --save-baseline main` then `cargo bench -- --baseline main`

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use echidnabot::api::rate_limit::WebhookRateLimiter;
use echidnabot::dispatcher::ProverKind;
use echidnabot::scheduler::ProofJob;
use echidnabot::store::models::goal_fingerprint;
use hmac::{Hmac, KeyInit, Mac};
use sha2::Sha256;
// criterion 0.8 deprecated its own `black_box` re-export in favour of the std
// one; CI runs clippy with `-D warnings`, so the re-export is an error here.
use std::hint::black_box;
use std::net::{IpAddr, Ipv4Addr};
use uuid::Uuid;

// ──────────────────────────────────────────────────────────────────────────────
// HMAC-SHA256 webhook signature verification
// Hot path: every inbound webhook call. Timing must be constant-time (see
// src/api/webhooks.rs verify_github_signature); this bench measures throughput.
// ──────────────────────────────────────────────────────────────────────────────

fn bench_hmac_verify(c: &mut Criterion) {
    let secret = b"super-secret-webhook-token-32b";
    let body_small = b"{}".as_ref();
    let body_medium = serde_json::json!({
        "action": "opened",
        "pull_request": { "number": 42, "title": "feat: add lean proof", "head": { "sha": "abc1234" } },
        "repository": { "id": 12345, "full_name": "owner/repo" }
    })
    .to_string();
    let body_large = body_medium.repeat(64); // ~16 KB — realistic large webhook

    let mut g = c.benchmark_group("hmac_verify");

    for (name, payload) in &[
        ("empty", body_small.to_vec()),
        ("typical_pr", body_medium.as_bytes().to_vec()),
        ("large_16kb", body_large.as_bytes().to_vec()),
    ] {
        g.throughput(Throughput::Bytes(payload.len() as u64));
        g.bench_with_input(BenchmarkId::from_parameter(name), payload, |b, body| {
            b.iter(|| {
                let mut mac = Hmac::<Sha256>::new_from_slice(secret).unwrap();
                mac.update(body);
                black_box(mac.finalize())
            });
        });
    }
    g.finish();
}

// ──────────────────────────────────────────────────────────────────────────────
// Per-IP rate limiter check
// Called on every webhook request. Must be fast enough that it doesn't
// become a bottleneck before the rate limit itself kicks in.
// ──────────────────────────────────────────────────────────────────────────────

fn bench_rate_limiter(c: &mut Criterion) {
    let limiter = WebhookRateLimiter::new(1000); // high limit so we don't actually block
    let ip_single = IpAddr::V4(Ipv4Addr::new(1, 2, 3, 4));

    let mut g = c.benchmark_group("rate_limiter");

    g.bench_function("single_ip_check", |b| {
        b.iter(|| black_box(limiter.check_ip(ip_single)));
    });

    // Contended: 256 distinct IPs already tracked
    let limiter_warm = WebhookRateLimiter::new(1000);
    for i in 0u8..=255 {
        limiter_warm.check_ip(IpAddr::V4(Ipv4Addr::new(10, 0, 0, i)));
    }
    g.bench_function("warm_cache_256_ips", |b| {
        b.iter(|| black_box(limiter_warm.check_ip(ip_single)));
    });

    g.finish();
}

// ──────────────────────────────────────────────────────────────────────────────
// ProverKind construction and lookup
// Called for every job creation and dispatcher call.
// ──────────────────────────────────────────────────────────────────────────────

fn bench_prover_kind(c: &mut Criterion) {
    let mut g = c.benchmark_group("prover_kind");

    g.bench_function("new_classic", |b| {
        b.iter(|| black_box(ProverKind::new("coq")));
    });

    g.bench_function("new_extended", |b| {
        b.iter(|| black_box(ProverKind::new("linear-agda")));
    });

    g.bench_function("display_name", |b| {
        let p = ProverKind::new("lean");
        b.iter(|| black_box(p.display_name()));
    });

    g.bench_function("from_extension_hit", |b| {
        b.iter(|| black_box(ProverKind::from_extension("v")));
    });

    g.bench_function("from_extension_miss", |b| {
        b.iter(|| black_box(ProverKind::from_extension("xyz")));
    });

    g.bench_function("tier_lookup", |b| {
        let p = ProverKind::new("isabelle");
        b.iter(|| black_box(p.tier()));
    });

    g.finish();
}

// ──────────────────────────────────────────────────────────────────────────────
// goal_fingerprint hashing
// Called for every tactic outcome record written to the feedback store.
// ──────────────────────────────────────────────────────────────────────────────

fn bench_goal_fingerprint(c: &mut Criterion) {
    let short_goal = "forall n : nat, n + 0 = n";
    let medium_goal = "forall (A B : Type) (f : A -> B) (l : list A), \
                       length (map f l) = length l";
    let long_goal = medium_goal.repeat(20); // ~1 KB proof obligation

    let mut g = c.benchmark_group("goal_fingerprint");
    g.bench_function("short", |b| {
        b.iter(|| black_box(goal_fingerprint(short_goal)));
    });
    g.bench_function("medium", |b| {
        b.iter(|| black_box(goal_fingerprint(medium_goal)));
    });
    g.bench_function("long_1kb", |b| {
        b.iter(|| black_box(goal_fingerprint(&long_goal)));
    });
    g.finish();
}

// ──────────────────────────────────────────────────────────────────────────────
// Job queue operations
// EnqueuedeJob is called per webhook push event; try_start_next is the
// scheduler hot loop tick.
// ──────────────────────────────────────────────────────────────────────────────

fn bench_proof_job_new(c: &mut Criterion) {
    let repo = Uuid::new_v4();
    let prover = ProverKind::new("coq");
    let files = vec![
        "theories/Main.v".to_string(),
        "theories/Lemmas.v".to_string(),
    ];

    c.bench_function("proof_job_new", |b| {
        b.iter(|| {
            black_box(ProofJob::new(
                repo,
                "abc123".to_string(),
                prover.clone(),
                files.clone(),
            ))
        });
    });
}

criterion_group!(
    benches,
    bench_hmac_verify,
    bench_rate_limiter,
    bench_prover_kind,
    bench_goal_fingerprint,
    bench_proof_job_new,
);
criterion_main!(benches);
