// SPDX-License-Identifier: PMPL-1.0-or-later
//! Benchmarks for finishingbot analyzers

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use finishingbot::analyzers::{
    license::LicenseAnalyzer, placeholder::PlaceholderAnalyzer, scm_files::ScmFilesAnalyzer,
    testing::TestingAnalyzer, tooling::ToolingAnalyzer, v1_readiness::V1ReadinessAnalyzer,
    Analyzer,
};
use finishingbot::config::Config;
use std::path::Path;

fn bench_license_analyzer(c: &mut Criterion) {
    let analyzer = LicenseAnalyzer::default();
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("license_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_scm_files_analyzer(c: &mut Criterion) {
    let analyzer = ScmFilesAnalyzer::default();
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("scm_files_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_testing_analyzer(c: &mut Criterion) {
    let analyzer = TestingAnalyzer::default();
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("testing_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_tooling_analyzer(c: &mut Criterion) {
    let analyzer = ToolingAnalyzer::default();
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("tooling_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_v1_readiness_analyzer(c: &mut Criterion) {
    let analyzer = V1ReadinessAnalyzer::default();
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("v1_readiness_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_full_audit(c: &mut Criterion) {
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("full_audit", |b| {
        b.iter(|| {
            let _license = LicenseAnalyzer::default().analyze(path, &config);
            let _placeholder = PlaceholderAnalyzer::default().analyze(path, &config);
            let _scm = ScmFilesAnalyzer::default().analyze(path, &config);
            let _testing = TestingAnalyzer::default().analyze(path, &config);
            let _tooling = ToolingAnalyzer::default().analyze(path, &config);
            let _v1 = V1ReadinessAnalyzer::default().analyze(path, &config);
        })
    });
}

criterion_group!(
    benches,
    bench_license_analyzer,
    bench_scm_files_analyzer,
    bench_testing_analyzer,
    bench_tooling_analyzer,
    bench_v1_readiness_analyzer,
    bench_full_audit
);
criterion_main!(benches);
