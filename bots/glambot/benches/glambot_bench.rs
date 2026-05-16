// SPDX-License-Identifier: PMPL-1.0-or-later
//! Benchmarks for glambot analyzers

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use glambot::analyzers::{
    accessibility::AccessibilityAnalyzer, machine::MachineAnalyzer, seo::SeoAnalyzer,
    visual::VisualAnalyzer, Analyzer,
};
use glambot::config::Config;
use std::path::Path;

fn bench_visual_analyzer(c: &mut Criterion) {
    let analyzer = VisualAnalyzer::default();
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("visual_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_accessibility_analyzer(c: &mut Criterion) {
    let analyzer = AccessibilityAnalyzer::default();
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("accessibility_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_seo_analyzer(c: &mut Criterion) {
    let analyzer = SeoAnalyzer::default();
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("seo_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

fn bench_machine_analyzer(c: &mut Criterion) {
    let analyzer = MachineAnalyzer::default();
    let config = Config::default();
    let path = Path::new(".");

    c.bench_function("machine_analyzer", |b| {
        b.iter(|| analyzer.analyze(black_box(path), black_box(&config)))
    });
}

criterion_group!(
    benches,
    bench_visual_analyzer,
    bench_accessibility_analyzer,
    bench_seo_analyzer,
    bench_machine_analyzer
);
criterion_main!(benches);
