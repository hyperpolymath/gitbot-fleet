// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <6759885+hyperpolymath@users.noreply.github.com>
//! Benchmarks for echidnabot operations.
//!
//! Measures the hot paths in proof confidence assessment and prover kind
//! lookups — both are called per-file during proof scanning and must be fast.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use echidnabot::dispatcher::{ProofStatus, ProverKind};
use echidnabot::trust::confidence::assess_confidence;

// ---------------------------------------------------------------------------
// Confidence assessment benchmarks
// ---------------------------------------------------------------------------

/// Benchmark a single confidence assessment for a verified proof.
fn bench_assess_confidence_verified(c: &mut Criterion) {
    let mut group = c.benchmark_group("confidence_assessment");

    // Typical case: Lean4 proof with certificate, single checker
    group.bench_function("lean_verified_with_cert", |b| {
        b.iter(|| {
            assess_confidence(
                black_box(ProverKind::Lean),
                black_box(ProofStatus::Verified),
                black_box(true),  // has_certificate
                black_box(1),     // checker_count
            )
        })
    });

    // Cross-checked by multiple systems (highest confidence)
    group.bench_function("coq_cross_checked", |b| {
        b.iter(|| {
            assess_confidence(
                black_box(ProverKind::Coq),
                black_box(ProofStatus::Verified),
                black_box(true),
                black_box(2),  // two checkers → Level5
            )
        })
    });

    // Failed proof (early-exit path)
    group.bench_function("z3_failed", |b| {
        b.iter(|| {
            assess_confidence(
                black_box(ProverKind::Z3),
                black_box(ProofStatus::Failed),
                black_box(false),
                black_box(0),
            )
        })
    });

    group.finish();
}

/// Benchmark confidence assessment across all prover kinds.
///
/// The inner loop is kept intentionally simple so we measure only the
/// classification logic, not overhead from constructing complex test fixtures.
fn bench_assess_all_provers(c: &mut Criterion) {
    let provers = [
        ProverKind::Agda,
        ProverKind::Coq,
        ProverKind::Lean,
        ProverKind::Isabelle,
        ProverKind::Z3,
        ProverKind::Cvc5,
        ProverKind::Metamath,
        ProverKind::HolLight,
        ProverKind::Mizar,
        ProverKind::Pvs,
        ProverKind::Acl2,
        ProverKind::Hol4,
    ];

    let mut group = c.benchmark_group("all_provers_confidence");
    for prover in &provers {
        group.bench_with_input(
            BenchmarkId::new("assess", format!("{:?}", prover)),
            prover,
            |b, &p| {
                b.iter(|| {
                    assess_confidence(
                        black_box(p),
                        black_box(ProofStatus::Verified),
                        black_box(true),
                        black_box(1),
                    )
                })
            },
        );
    }
    group.finish();
}

// ---------------------------------------------------------------------------
// ProverKind metadata lookups
// ---------------------------------------------------------------------------

/// Benchmark file extension lookups for prover kind detection.
///
/// These are called when scanning repository files to identify proof files.
fn bench_prover_file_extensions(c: &mut Criterion) {
    let mut group = c.benchmark_group("prover_metadata");

    group.bench_function("lean_extensions", |b| {
        b.iter(|| {
            let exts = ProverKind::Lean.file_extensions();
            black_box(exts)
        })
    });

    group.bench_function("coq_extensions", |b| {
        b.iter(|| {
            let exts = ProverKind::Coq.file_extensions();
            black_box(exts)
        })
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_assess_confidence_verified,
    bench_assess_all_provers,
    bench_prover_file_extensions,
);
criterion_main!(benches);
