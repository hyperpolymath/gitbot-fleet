// SPDX-License-Identifier: MPL-2.0
//! Benchmarks for finishingbot analyzers

use criterion::{criterion_group, criterion_main, Criterion};
use finishing_bot::analyzers::{
    license::LicenseAnalyzer, placeholder::PlaceholderAnalyzer, scm_files::ScmFilesAnalyzer,
    testing::TestingAnalyzer, tooling::ToolingAnalyzer, v1_readiness::V1ReadinessAnalyzer,
    Analyzer,
};
use finishing_bot::config::Config;
use std::hint::black_box;
use std::path::Path;

fn bench_license_analyzer(c: &mut Criterion) {
    let analyzer = LicenseAnalyzer;
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("license_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_scm_files_analyzer(c: &mut Criterion) {
    let analyzer = ScmFilesAnalyzer;
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("scm_files_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_testing_analyzer(c: &mut Criterion) {
    let analyzer = TestingAnalyzer;
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("testing_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_tooling_analyzer(c: &mut Criterion) {
    let analyzer = ToolingAnalyzer;
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("tooling_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_v1_readiness_analyzer(c: &mut Criterion) {
    let analyzer = V1ReadinessAnalyzer;
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
            let _license = LicenseAnalyzer.analyze(path, &config);
            let _placeholder = PlaceholderAnalyzer.analyze(path, &config);
            let _scm = ScmFilesAnalyzer.analyze(path, &config);
            let _testing = TestingAnalyzer.analyze(path, &config);
            let _tooling = ToolingAnalyzer.analyze(path, &config);
            let _v1 = V1ReadinessAnalyzer.analyze(path, &config);
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
