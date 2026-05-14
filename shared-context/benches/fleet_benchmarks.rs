// SPDX-License-Identifier: PMPL-1.0-or-later
//! Performance benchmarks for gitbot-fleet operations

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use gitbot_shared_context::{BotId, Context, Finding, Severity};
use std::path::PathBuf;

/// Benchmark context creation
fn bench_context_creation(c: &mut Criterion) {
    c.bench_function("context_new", |b| {
        b.iter(|| {
            let ctx = Context::new(
                black_box("test-repo"),
                black_box(PathBuf::from("/tmp/test")),
            );
            black_box(ctx);
        });
    });
}

/// Benchmark bot registration
fn bench_bot_registration(c: &mut Criterion) {
    let mut group = c.benchmark_group("bot_registration");

    group.bench_function("register_single", |b| {
        b.iter(|| {
            let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
            ctx.register_bot(black_box(BotId::Rhodibot));
            black_box(ctx);
        });
    });

    group.bench_function("register_all", |b| {
        b.iter(|| {
            let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
            ctx.register_all_bots();
            black_box(ctx);
        });
    });

    group.finish();
}

/// Benchmark finding operations
fn bench_finding_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("findings");

    // Single finding addition
    group.bench_function("add_single_finding", |b| {
        b.iter(|| {
            let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
            let finding = Finding::new(
                black_box(BotId::Rhodibot),
                black_box("TEST-001"),
                black_box(Severity::Warning),
                black_box("Test finding"),
            );
            ctx.add_finding(finding);
            black_box(ctx);
        });
    });

    // Bulk finding addition
    for count in [10, 100, 1000].iter() {
        group.throughput(Throughput::Elements(*count as u64));
        group.bench_with_input(BenchmarkId::new("add_bulk", count), count, |b, &count| {
            b.iter(|| {
                let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
                for i in 0..count {
                    let finding = Finding::new(
                        BotId::Rhodibot,
                        &format!("TEST-{:03}", i),
                        Severity::Warning,
                        "Test finding",
                    );
                    ctx.add_finding(finding);
                }
                black_box(ctx);
            });
        });
    }

    group.finish();
}

/// Benchmark finding queries
fn bench_finding_queries(c: &mut Criterion) {
    let mut group = c.benchmark_group("finding_queries");

    // Setup context with findings
    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
    for i in 0..1000 {
        let bot = match i % 3 {
            0 => BotId::Rhodibot,
            1 => BotId::Echidnabot,
            _ => BotId::Sustainabot,
        };
        let severity = match i % 4 {
            0 => Severity::Error,
            1 => Severity::Warning,
            2 => Severity::Info,
            _ => Severity::Suggestion,
        };
        ctx.add_finding(Finding::new(bot, &format!("TEST-{:03}", i), severity, "Test"));
    }

    group.bench_function("query_by_bot", |b| {
        b.iter(|| {
            let findings = ctx.findings_from(black_box(BotId::Rhodibot));
            black_box(findings);
        });
    });

    group.bench_function("query_by_category", |b| {
        b.iter(|| {
            let findings = ctx.findings_by_category(black_box("test-category"));
            black_box(findings);
        });
    });

    group.bench_function("check_has_errors", |b| {
        b.iter(|| {
            let has_errors = ctx.has_errors();
            black_box(has_errors);
        });
    });

    group.bench_function("check_blocks_release", |b| {
        b.iter(|| {
            let blocks = ctx.blocks_release();
            black_box(blocks);
        });
    });

    group.finish();
}

/// Benchmark bot execution tracking
fn bench_bot_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("bot_execution");

    group.bench_function("start_complete_cycle", |b| {
        b.iter(|| {
            let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
            ctx.register_bot(BotId::Rhodibot);
            ctx.start_bot(BotId::Rhodibot).unwrap();
            ctx.complete_bot(BotId::Rhodibot, 10, 2, 50).unwrap();
            black_box(ctx);
        });
    });

    group.bench_function("ready_bots_calculation", |b| {
        let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
        ctx.register_all_bots();
        // Complete some bots
        ctx.start_bot(BotId::Rhodibot).unwrap();
        ctx.complete_bot(BotId::Rhodibot, 5, 0, 20).unwrap();

        b.iter(|| {
            let ready = ctx.ready_bots();
            black_box(ready);
        });
    });

    group.finish();
}

/// Benchmark health checking
fn bench_health_check(c: &mut Criterion) {
    let mut group = c.benchmark_group("health");

    // Setup context with execution data
    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
    ctx.register_all_bots();
    ctx.start_bot(BotId::Rhodibot).unwrap();
    ctx.complete_bot(BotId::Rhodibot, 10, 2, 50).unwrap();
    ctx.start_bot(BotId::Echidnabot).unwrap();
    ctx.complete_bot(BotId::Echidnabot, 5, 0, 30).unwrap();

    // Add some findings
    for i in 0..50 {
        ctx.add_finding(Finding::new(
            BotId::Rhodibot,
            &format!("TEST-{:03}", i),
            if i < 5 { Severity::Error } else { Severity::Warning },
            "Test finding",
        ));
    }

    group.bench_function("full_health_check", |b| {
        b.iter(|| {
            let health = ctx.health_check();
            black_box(health);
        });
    });

    group.finish();
}

/// Benchmark report generation
fn bench_report_generation(c: &mut Criterion) {
    use gitbot_shared_context::ReportFormat;

    let mut group = c.benchmark_group("reporting");

    // Setup context with data
    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
    ctx.register_all_bots();

    for bot in [BotId::Rhodibot, BotId::Echidnabot, BotId::Sustainabot] {
        ctx.start_bot(bot).unwrap();
        ctx.complete_bot(bot, 20, 3, 100).unwrap();
    }

    for i in 0..100 {
        ctx.add_finding(Finding::new(
            BotId::Rhodibot,
            &format!("TEST-{:03}", i),
            if i % 4 == 0 { Severity::Error } else { Severity::Warning },
            "Test finding with some detail",
        ));
    }

    group.bench_function("generate_markdown", |b| {
        b.iter(|| {
            let report = ctx.generate_report(black_box(ReportFormat::Markdown));
            black_box(report);
        });
    });

    group.bench_function("generate_json", |b| {
        b.iter(|| {
            let report = ctx.generate_report(black_box(ReportFormat::Json));
            black_box(report);
        });
    });

    group.bench_function("generate_html", |b| {
        b.iter(|| {
            let report = ctx.generate_report(black_box(ReportFormat::Html));
            black_box(report);
        });
    });

    group.finish();
}

/// Benchmark summary generation
fn bench_summary(c: &mut Criterion) {
    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
    ctx.register_all_bots();

    for bot in BotId::all() {
        ctx.start_bot(bot).unwrap();
        ctx.complete_bot(bot, 15, 2, 75).unwrap();
    }

    c.bench_function("generate_summary", |b| {
        b.iter(|| {
            let summary = ctx.summary();
            black_box(summary);
        });
    });
}

/// Benchmark serialization
fn bench_serialization(c: &mut Criterion) {
    let mut group = c.benchmark_group("serialization");

    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
    ctx.register_all_bots();

    for i in 0..100 {
        ctx.add_finding(Finding::new(
            BotId::Rhodibot,
            &format!("TEST-{:03}", i),
            Severity::Warning,
            "Test finding",
        ));
    }

    group.bench_function("serialize_context", |b| {
        b.iter(|| {
            let json = serde_json::to_string(&ctx).unwrap();
            black_box(json);
        });
    });

    let json = serde_json::to_string(&ctx).unwrap();
    group.bench_function("deserialize_context", |b| {
        b.iter(|| {
            let ctx: Context = serde_json::from_str(&json).unwrap();
            black_box(ctx);
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_context_creation,
    bench_bot_registration,
    bench_finding_operations,
    bench_finding_queries,
    bench_bot_execution,
    bench_health_check,
    bench_report_generation,
    bench_summary,
    bench_serialization,
);
criterion_main!(benches);
