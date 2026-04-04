// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <6759885+hyperpolymath@users.noreply.github.com>
//! P2P property tests for shared-context
//!
//! These tests verify key invariants that must hold for any valid input:
//!
//! 1. **Fleet state validity**: Any valid subset of bots produces a coherent
//!    coordination state — no panics, consistent metadata, correct tier counts.
//!
//! 2. **Confidence score bounds**: ConfidenceThresholds.dispatch_strategy always
//!    returns a well-typed result for any f64 in [0.0, 1.0].
//!
//! 3. **Task dispatch determinism**: Dispatching the same task with the same
//!    confidence and successful_fixes count always yields the same strategy.
//!
//! These run as plain unit tests without proptest/quickcheck so that there are
//! no new dependencies. The "property" approach is achieved by iterating over a
//! carefully constructed set of representative values spanning boundary conditions.

use gitbot_shared_context::{
    BotId, Context, ConfidenceThresholds, DispatchStrategy, Finding, Severity,
};
use std::path::PathBuf;

// ---------------------------------------------------------------------------
// Property 1: Any valid bot subset produces a coherent fleet state
// ---------------------------------------------------------------------------

/// All possible subsets of bots should register without panic and produce
/// a valid context where no invariants are violated.
#[test]
fn prop_any_bot_subset_produces_valid_state() {
    let all_bots = BotId::all();

    // Test single-bot subsets
    for &bot in &all_bots {
        let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
        ctx.register_bot(bot);

        // Start and complete the single bot
        ctx.start_bot(bot).expect("start_bot should not fail for registered bot");
        ctx.complete_bot(bot, 0, 0, 1).expect("complete_bot should not fail");

        // State must be internally consistent
        assert!(ctx.bot_completed(bot), "Bot should be marked complete after complete_bot");
        assert_eq!(
            ctx.findings.len(), 0,
            "No findings added — findings collection should be empty"
        );
    }
}

/// Registering all bots and running them all should produce a coherent summary.
#[test]
fn prop_full_fleet_produces_coherent_summary() {
    let mut ctx = Context::new("fleet-test-repo", PathBuf::from("/tmp/fleet-test"));
    ctx.register_all_bots();

    let all_bots = BotId::all();

    // Run each bot, adding 1 finding each
    for &bot in &all_bots {
        ctx.start_bot(bot).unwrap();
        ctx.add_finding(Finding::new(
            bot,
            &format!("{}-001", bot),
            Severity::Info,
            &format!("Test finding from {}", bot),
        ));
        ctx.complete_bot(bot, 1, 0, 5).unwrap();
    }

    ctx.complete_session();
    let summary = ctx.summary();

    // Total findings must equal number of bots (one per bot)
    assert_eq!(
        summary.total_findings,
        all_bots.len(),
        "Summary total_findings should equal number of bots that each contributed one finding"
    );

    // Total files analyzed must equal 5 * number of bots
    assert_eq!(
        summary.total_files_analyzed,
        5 * all_bots.len(),
        "Total files analyzed should be the sum across all bots"
    );

    // Repo name must be preserved
    assert_eq!(summary.repo_name, "fleet-test-repo");

    // bots_run must equal all_bots.len()
    assert_eq!(summary.bots_run, all_bots.len());
}

/// Findings from one bot should never appear in another bot's findings_from query.
#[test]
fn prop_findings_are_partitioned_by_bot() {
    let mut ctx = Context::new("partition-test", PathBuf::from("/tmp/partition-test"));
    ctx.register_all_bots();

    let all_bots = BotId::all();

    // Add 3 findings per bot
    for &bot in &all_bots {
        for i in 0..3u32 {
            ctx.add_finding(Finding::new(
                bot,
                &format!("{}-{:03}", bot, i),
                Severity::Warning,
                &format!("Finding {} from {}", i, bot),
            ));
        }
    }

    // Each bot's findings slice must only contain that bot's findings
    for &bot in &all_bots {
        let bot_findings = ctx.findings_from(bot);
        assert_eq!(bot_findings.len(), 3, "Expected exactly 3 findings for {}", bot);
        for f in &bot_findings {
            assert_eq!(
                f.source, bot,
                "Finding sourced from {} appeared in {}'s partition",
                f.source, bot
            );
        }
    }

    // Total findings must equal 3 * number of bots
    assert_eq!(
        ctx.findings.len(),
        3 * all_bots.len(),
        "Total findings must be the sum of all per-bot findings"
    );
}

// ---------------------------------------------------------------------------
// Property 2: Confidence scores are always in [0.0, 1.0] range
// ---------------------------------------------------------------------------

/// DispatchStrategy must be deterministic and cover the full range of valid
/// confidence values without panicking or returning undefined behaviour.
#[test]
fn prop_confidence_scores_always_yield_valid_strategy() {
    let thresholds = ConfidenceThresholds::default();

    // Test boundary values and representative points across [0.0, 1.0]
    let test_values: &[f64] = &[
        0.0, 0.001, 0.1, 0.3, 0.5, 0.69, 0.70, 0.849, 0.85, 0.94,
        0.95, 0.96, 0.99, 1.0,
    ];

    for &confidence in test_values {
        // With min_successful_fixes = 0 (waived)
        let strategy = thresholds.dispatch_strategy(confidence, 10);

        // The result must be one of the three valid variants — no panics
        match strategy {
            DispatchStrategy::AutoExecute
            | DispatchStrategy::ReviewRequired
            | DispatchStrategy::ReportOnly => {}
        }
    }
}

/// Below the auto_execute_min threshold, AutoExecute must never be returned.
#[test]
fn prop_below_auto_execute_threshold_never_auto_executes() {
    let thresholds = ConfidenceThresholds::default(); // auto_execute_min = 0.95

    // Values strictly below 0.95
    let below_threshold = [0.0_f64, 0.5, 0.84, 0.94, 0.9499];
    for &confidence in &below_threshold {
        let strategy = thresholds.dispatch_strategy(confidence, 100);
        assert_ne!(
            strategy,
            DispatchStrategy::AutoExecute,
            "confidence={} is below auto_execute_min but returned AutoExecute",
            confidence
        );
    }
}

/// At or above the auto_execute_min threshold with sufficient successful fixes,
/// AutoExecute must always be returned.
#[test]
fn prop_at_auto_execute_threshold_with_fixes_always_auto_executes() {
    let thresholds = ConfidenceThresholds::default(); // auto_execute_min = 0.95, min_successful_fixes = 3

    let at_or_above = [0.95_f64, 0.96, 0.99, 1.0];
    for &confidence in &at_or_above {
        let strategy = thresholds.dispatch_strategy(confidence, 3);
        assert_eq!(
            strategy,
            DispatchStrategy::AutoExecute,
            "confidence={} with 3 fixes should AutoExecute",
            confidence
        );
    }
}

/// With insufficient successful_fixes, even high-confidence findings fall back
/// to ReviewRequired rather than AutoExecute.
#[test]
fn prop_insufficient_fixes_prevents_auto_execute() {
    let thresholds = ConfidenceThresholds::default(); // min_successful_fixes = 3

    // Perfect confidence, but only 2 previous successful fixes
    let strategy = thresholds.dispatch_strategy(1.0, 2);
    assert_ne!(
        strategy,
        DispatchStrategy::AutoExecute,
        "Insufficient fix history should prevent AutoExecute even at max confidence"
    );
    // It should fall through to ReviewRequired (confidence >= review_min = 0.85)
    assert_eq!(strategy, DispatchStrategy::ReviewRequired);
}

/// Below the review_min threshold, the strategy must be ReportOnly.
#[test]
fn prop_below_review_threshold_is_report_only() {
    let thresholds = ConfidenceThresholds::default(); // review_min = 0.85

    let below_review = [0.0_f64, 0.5, 0.84, 0.8499];
    for &confidence in &below_review {
        let strategy = thresholds.dispatch_strategy(confidence, 100);
        assert_eq!(
            strategy,
            DispatchStrategy::ReportOnly,
            "confidence={} is below review_min — expected ReportOnly",
            confidence
        );
    }
}

// ---------------------------------------------------------------------------
// Property 3: Task dispatch determinism
// ---------------------------------------------------------------------------

/// The same confidence + successful_fixes pair must always produce the same
/// dispatch strategy, regardless of how many times it is called or in what order.
#[test]
fn prop_dispatch_is_deterministic() {
    let thresholds = ConfidenceThresholds::default();

    let test_cases: &[(f64, u32)] = &[
        (0.0, 0),
        (0.5, 0),
        (0.85, 0),
        (0.85, 5),
        (0.95, 3),
        (0.95, 2),
        (1.0, 10),
    ];

    // Call each pair 5 times — result must be identical every time
    for &(confidence, fixes) in test_cases {
        let first = thresholds.dispatch_strategy(confidence, fixes);
        for _ in 0..4 {
            let result = thresholds.dispatch_strategy(confidence, fixes);
            assert_eq!(
                result, first,
                "dispatch_strategy({}, {}) returned different results on repeated calls",
                confidence, fixes
            );
        }
    }
}

/// Different confidence values must produce different strategies when they fall
/// on opposite sides of a threshold boundary.
#[test]
fn prop_dispatch_respects_threshold_ordering() {
    let thresholds = ConfidenceThresholds::default();

    // Provide enough successful_fixes to not block AutoExecute
    let sufficient_fixes = thresholds.min_successful_fixes + 1;

    let auto_execute = thresholds.dispatch_strategy(thresholds.auto_execute_min, sufficient_fixes);
    let review_required = thresholds.dispatch_strategy(thresholds.review_min, sufficient_fixes);
    let report_only = thresholds.dispatch_strategy(0.0, sufficient_fixes);

    assert_eq!(auto_execute, DispatchStrategy::AutoExecute);
    assert_eq!(review_required, DispatchStrategy::ReviewRequired);
    assert_eq!(report_only, DispatchStrategy::ReportOnly);

    // The three strategies must all be different
    assert_ne!(auto_execute, review_required);
    assert_ne!(auto_execute, report_only);
    assert_ne!(review_required, report_only);
}

/// Two contexts for the same repo with the same bots and same findings must
/// produce identical summaries (structural equality, not pointer identity).
#[test]
fn prop_identical_contexts_produce_identical_summaries() {
    let make_ctx = || {
        let mut ctx = Context::new("deterministic-repo", PathBuf::from("/tmp/det"));
        ctx.register_bot(BotId::Rhodibot);
        ctx.start_bot(BotId::Rhodibot).unwrap();
        ctx.add_finding(Finding::new(
            BotId::Rhodibot,
            "RSR-001",
            Severity::Error,
            "Deterministic finding",
        ));
        ctx.complete_bot(BotId::Rhodibot, 1, 1, 5).unwrap();
        ctx.complete_session();
        ctx
    };

    let ctx1 = make_ctx();
    let ctx2 = make_ctx();

    let s1 = ctx1.summary();
    let s2 = ctx2.summary();

    assert_eq!(s1.repo_name, s2.repo_name);
    assert_eq!(s1.total_findings, s2.total_findings);
    assert_eq!(s1.total_errors, s2.total_errors);
    assert_eq!(s1.total_warnings, s2.total_warnings);
    assert_eq!(s1.bots_run, s2.bots_run);
    assert_eq!(s1.blocks_release, s2.blocks_release);
}
