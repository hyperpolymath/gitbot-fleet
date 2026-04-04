// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <6759885+hyperpolymath@users.noreply.github.com>
//! End-to-end fleet coordination tests.
//!
//! These tests verify the full coordination lifecycle as observed through the
//! shared-context layer:
//!
//! - Single bot dispatch: task is dispatched → bot processes → result collected
//! - Multi-bot dispatch: all results aggregated correctly, no interleaving
//! - Bot failure isolation: one failed bot does not prevent others from completing
//! - Session lifecycle: context progresses correctly through all phases
//! - Findings pipeline: findings flow correctly from source bots to consumers
//!
//! These are end-to-end tests of the _shared-context coordination layer_. They
//! do not start real bot processes; they exercise the full state machine that
//! real bots drive.

use gitbot_shared_context::{
    BotId, Context, ContextStorage, Finding, ReportFormat, Severity,
};
use std::path::PathBuf;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// E2E Scenario 1: Single bot dispatch → process → collect
// ---------------------------------------------------------------------------

/// Dispatch a task to a single bot, let it process, and verify the result
/// is captured in the shared context.
#[test]
fn e2e_single_bot_dispatch_process_collect() {
    let mut ctx = Context::new("e2e-single-bot", PathBuf::from("/tmp/e2e-single-bot"));
    ctx.register_bot(BotId::Rhodibot);

    // --- DISPATCH phase ---
    ctx.start_bot(BotId::Rhodibot).expect("start_bot must succeed for registered bot");

    // Verify bot is running
    let execution = ctx.executions.get(&BotId::Rhodibot).unwrap();
    assert!(
        execution.started_at.is_some(),
        "Bot start time must be recorded after start_bot"
    );

    // --- PROCESS phase (bot adds findings) ---
    ctx.add_finding(
        Finding::new(BotId::Rhodibot, "RSR-MISSING-README", Severity::Error, "Missing README.adoc")
            .with_category("structure"),
    );
    ctx.add_finding(
        Finding::new(BotId::Rhodibot, "RSR-MISSING-LICENSE", Severity::Warning, "Missing LICENSE file")
            .with_category("legal"),
    );

    // --- COLLECT phase ---
    ctx.complete_bot(BotId::Rhodibot, 2, 1, 15)
        .expect("complete_bot must succeed");

    // Verify result collection
    let execution = ctx.executions.get(&BotId::Rhodibot).unwrap();
    assert_eq!(execution.findings_count, 2, "findings_count must match");
    assert_eq!(execution.errors_count, 1, "errors_count must match");
    assert_eq!(execution.files_analyzed, 15, "files_analyzed must match");
    assert!(execution.completed_at.is_some(), "completed_at must be set");

    // Verify findings are queryable
    let results = ctx.findings_from(BotId::Rhodibot);
    assert_eq!(results.len(), 2, "Must be able to retrieve all 2 findings");

    let readme_finding = results.iter().find(|f| f.rule_id == "RSR-MISSING-README");
    assert!(readme_finding.is_some(), "RSR-MISSING-README must be present");
    assert_eq!(readme_finding.unwrap().severity, Severity::Error);
}

// ---------------------------------------------------------------------------
// E2E Scenario 2: Multi-bot dispatch → aggregation
// ---------------------------------------------------------------------------

/// Run all standard verifier + finisher bots and verify all results are
/// aggregated correctly in the session summary.
#[test]
fn e2e_multi_bot_dispatch_all_results_aggregated() {
    let mut ctx = Context::new("e2e-multi-bot", PathBuf::from("/tmp/e2e-multi-bot"));
    ctx.register_all_bots();

    let verifiers = [BotId::Rhodibot, BotId::Echidnabot, BotId::Sustainabot, BotId::Panicbot];
    let finishers = [BotId::Glambot, BotId::Seambot, BotId::Finishbot];

    // Run verifiers first (no deps)
    for &bot in &verifiers {
        ctx.start_bot(bot).expect("start verifier");
        ctx.add_finding(Finding::new(bot, &format!("{}-001", bot), Severity::Warning, "Verifier finding"));
        ctx.complete_bot(bot, 1, 0, 10).expect("complete verifier");
    }

    // Verify all verifiers complete before finishers start
    assert!(ctx.verifiers_complete(), "All verifiers must be complete before running finishers");

    // Run finishers
    for &bot in &finishers {
        ctx.start_bot(bot).expect("start finisher");
        ctx.add_finding(Finding::new(bot, &format!("{}-001", bot), Severity::Info, "Finisher finding"));
        ctx.complete_bot(bot, 1, 0, 5).expect("complete finisher");
    }

    ctx.complete_session();
    let summary = ctx.summary();

    // All bots contributed one finding each
    let total_bots = verifiers.len() + finishers.len();
    assert_eq!(
        summary.total_findings, total_bots,
        "Summary must aggregate findings from all {} bots", total_bots
    );
    assert_eq!(
        summary.bots_run, total_bots,
        "bots_run must count all {} completing bots", total_bots
    );

    // No errors (only warnings and info)
    assert_eq!(summary.total_errors, 0, "No error-severity findings were added");
    assert_eq!(summary.total_warnings, verifiers.len(), "Verifiers each contributed one warning");

    // No release blocks (no error-severity findings)
    assert!(!summary.blocks_release, "Warnings and info should not block release");
}

// ---------------------------------------------------------------------------
// E2E Scenario 3: Bot failure isolation
// ---------------------------------------------------------------------------

/// When one bot fails, other bots must still be able to start, run, and
/// complete. The failed bot should not corrupt the session.
#[test]
fn e2e_bot_failure_does_not_prevent_other_bots() {
    let mut ctx = Context::new("e2e-failure-isolation", PathBuf::from("/tmp/e2e-failure-isolation"));
    ctx.register_all_bots();

    // Rhodibot fails
    ctx.start_bot(BotId::Rhodibot).unwrap();
    ctx.fail_bot(BotId::Rhodibot, "GitHub API rate limit exceeded")
        .expect("fail_bot must succeed");

    let rhodibot_exec = ctx.executions.get(&BotId::Rhodibot).unwrap();
    assert!(
        matches!(rhodibot_exec.status, gitbot_shared_context::bot::BotStatus::Failed),
        "Rhodibot execution status must be Failed"
    );

    // Echidnabot can still run independently
    ctx.start_bot(BotId::Echidnabot).unwrap();
    ctx.add_finding(Finding::new(BotId::Echidnabot, "PROOF-VERIFIED", Severity::Info, "Proof verified"));
    ctx.complete_bot(BotId::Echidnabot, 1, 0, 5).unwrap();

    // Sustainabot can still run independently
    ctx.start_bot(BotId::Sustainabot).unwrap();
    ctx.add_finding(Finding::new(BotId::Sustainabot, "ECO-001", Severity::Warning, "Outdated deps"));
    ctx.complete_bot(BotId::Sustainabot, 1, 0, 8).unwrap();

    // Panicbot can still run independently
    ctx.start_bot(BotId::Panicbot).unwrap();
    ctx.complete_bot(BotId::Panicbot, 0, 0, 3).unwrap();

    // Findings from successful bots are intact
    assert_eq!(ctx.findings_from(BotId::Echidnabot).len(), 1, "Echidnabot finding must be present");
    assert_eq!(ctx.findings_from(BotId::Sustainabot).len(), 1, "Sustainabot finding must be present");

    // Rhodibot findings are empty (it failed before adding any)
    assert_eq!(ctx.findings_from(BotId::Rhodibot).len(), 0, "Failed bot contributed no findings");

    // The session summary should reflect the failure
    ctx.complete_session();
    let summary = ctx.summary();
    assert_eq!(summary.total_findings, 2, "Only 2 findings from the successful bots");
}

// ---------------------------------------------------------------------------
// E2E Scenario 4: Session persistence and reload
// ---------------------------------------------------------------------------

/// A context can be saved to disk and reloaded, recovering all findings and
/// execution records intact.
#[test]
fn e2e_session_persistence_and_reload() {
    let temp = TempDir::new().unwrap();
    let storage = ContextStorage::new(temp.path());

    // Create and populate context
    let mut ctx = Context::new("persist-repo", PathBuf::from("/tmp/persist-repo"));
    ctx.register_bot(BotId::Rhodibot);
    ctx.start_bot(BotId::Rhodibot).unwrap();
    ctx.add_finding(
        Finding::new(BotId::Rhodibot, "RSR-001", Severity::Error, "Persistent error finding")
            .with_category("structure"),
    );
    ctx.complete_bot(BotId::Rhodibot, 1, 1, 20).unwrap();
    ctx.complete_session();

    let session_id = ctx.session_id;
    let expected_findings = ctx.findings.len();

    // Persist to disk
    storage.save_context(&ctx).expect("save_context must succeed");

    // Reload and verify
    let loaded = storage.load_context(&session_id).expect("load_context must succeed");

    assert_eq!(loaded.repo_name, "persist-repo", "repo_name must survive round-trip");
    assert_eq!(
        loaded.findings.len(), expected_findings,
        "findings count must survive round-trip"
    );
    assert_eq!(loaded.session_id, session_id, "session_id must be identical after reload");

    // Verify execution records are intact
    let exec = loaded.executions.get(&BotId::Rhodibot).unwrap();
    assert_eq!(exec.findings_count, 1);
    assert_eq!(exec.errors_count, 1);
}

// ---------------------------------------------------------------------------
// E2E Scenario 5: Report generation pipeline
// ---------------------------------------------------------------------------

/// A completed session must be able to generate well-formed reports in all
/// supported formats (Markdown, JSON, text). None of these must panic or
/// produce an empty result.
#[test]
fn e2e_report_generation_pipeline() {
    let mut ctx = Context::new("report-repo", PathBuf::from("/tmp/report-repo"));
    ctx.register_all_bots();

    // Run a subset of bots to populate the context
    ctx.start_bot(BotId::Rhodibot).unwrap();
    ctx.add_finding(
        Finding::new(BotId::Rhodibot, "RSR-001", Severity::Error, "Missing SPDX headers on 3 files")
            .with_category("licensing"),
    );
    ctx.complete_bot(BotId::Rhodibot, 1, 1, 30).unwrap();

    ctx.start_bot(BotId::Glambot).unwrap();
    ctx.add_finding(
        Finding::new(BotId::Glambot, "SEO-001", Severity::Warning, "README missing meta keywords")
            .with_category("seo"),
    );
    ctx.complete_bot(BotId::Glambot, 1, 0, 5).unwrap();

    ctx.complete_session();

    // Generate markdown report
    let md_report = ctx.generate_report(ReportFormat::Markdown);
    assert!(!md_report.is_empty(), "Markdown report must not be empty");
    assert!(
        md_report.contains("report-repo"),
        "Report must mention the repository name"
    );

    // Generate JSON report
    let json_report = ctx.generate_report(ReportFormat::Json);
    assert!(!json_report.is_empty(), "JSON report must not be empty");
    // Must be valid JSON
    let parsed: serde_json::Value = serde_json::from_str(&json_report)
        .expect("JSON report must be valid JSON");
    assert!(
        parsed.is_object() || parsed.is_array(),
        "JSON report must be an object or array at the root"
    );

    // Generate HTML report
    let html_report = ctx.generate_report(ReportFormat::Html);
    assert!(!html_report.is_empty(), "HTML report must not be empty");
}

// ---------------------------------------------------------------------------
// E2E Scenario 6: Findings severity filtering and release gate
// ---------------------------------------------------------------------------

/// Error-severity findings block release. Warning and Info do not.
/// This verifies the complete pipeline from finding ingestion to release decision.
#[test]
fn e2e_findings_severity_pipeline_release_gate() {
    // Case 1: No findings → no block
    {
        let mut ctx = Context::new("clean-repo", PathBuf::from("/tmp/clean-repo"));
        ctx.register_bot(BotId::Rhodibot);
        ctx.start_bot(BotId::Rhodibot).unwrap();
        ctx.complete_bot(BotId::Rhodibot, 0, 0, 10).unwrap();
        ctx.complete_session();
        assert!(!ctx.blocks_release(), "Clean repo must not block release");
    }

    // Case 2: Only warnings → no block
    {
        let mut ctx = Context::new("warning-repo", PathBuf::from("/tmp/warning-repo"));
        ctx.register_bot(BotId::Rhodibot);
        ctx.start_bot(BotId::Rhodibot).unwrap();
        ctx.add_finding(Finding::new(BotId::Rhodibot, "WARN-001", Severity::Warning, "Minor issue"));
        ctx.complete_bot(BotId::Rhodibot, 1, 0, 5).unwrap();
        ctx.complete_session();
        assert!(!ctx.blocks_release(), "Warnings alone must not block release");
    }

    // Case 3: Error present → blocks release
    {
        let mut ctx = Context::new("error-repo", PathBuf::from("/tmp/error-repo"));
        ctx.register_bot(BotId::Rhodibot);
        ctx.start_bot(BotId::Rhodibot).unwrap();
        ctx.add_finding(Finding::new(BotId::Rhodibot, "ERR-001", Severity::Error, "Critical missing file"));
        ctx.complete_bot(BotId::Rhodibot, 1, 1, 5).unwrap();
        ctx.complete_session();
        assert!(ctx.blocks_release(), "Error severity must block release");
        assert!(ctx.has_errors(), "has_errors must return true when Error findings present");
    }

    // Case 4: Findings marked as fixable are tracked by the pipeline
    // (demonstrates the finding pipeline supports fixability tracking)
    {
        let mut ctx = Context::new("fixable-repo", PathBuf::from("/tmp/fixable-repo"));
        ctx.register_bot(BotId::Rhodibot);
        ctx.start_bot(BotId::Rhodibot).unwrap();

        let finding = Finding::new(BotId::Rhodibot, "ERR-002", Severity::Error, "Can be auto-fixed")
            .fixable();
        let finding_id = finding.id;
        ctx.add_finding(finding);
        ctx.complete_bot(BotId::Rhodibot, 1, 1, 5).unwrap();

        // Before fixing: blocks release, has errors, has fixable finding
        assert!(ctx.blocks_release());
        assert!(ctx.has_errors());
        assert_eq!(ctx.findings.fixable().len(), 1, "One fixable finding must be tracked");

        // Mark the finding as fixed via the FindingSet mutation API
        if let Some(f) = ctx.findings.find_mut(finding_id) {
            f.mark_fixed();
        }

        // After fixing: fixable() returns only not-yet-fixed items (now 0),
        // and unfixed() returns 0 as well (the only finding is now marked fixed).
        assert_eq!(
            ctx.findings.fixable().len(), 0,
            "fixable() must return 0 after the finding is marked fixed"
        );
        assert_eq!(
            ctx.findings.unfixed().len(), 0,
            "unfixed() must return 0 after fixing"
        );
    }
}
