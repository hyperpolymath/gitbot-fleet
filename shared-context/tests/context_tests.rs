// SPDX-License-Identifier: PMPL-1.0
//! Integration tests for shared context

use gitbot_shared_context::{
    BotId, BotInfo, Context, ContextStorage, Finding, RepoState, Severity, Tier,
};
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_context_creation() {
    let ctx = Context::new("test-repo", "/path/to/repo");

    assert_eq!(ctx.repo_name, "test-repo");
    assert_eq!(ctx.repo_path, PathBuf::from("/path/to/repo"));
    assert!(ctx.executions.is_empty());
    assert!(ctx.findings.is_empty());
}

#[test]
fn test_bot_registration_and_execution() {
    let mut ctx = Context::new("test-repo", "/tmp/test");

    // Register bot
    ctx.register_bot(BotId::Glambot);
    assert!(ctx.executions.contains_key(&BotId::Glambot));

    // Start bot
    ctx.start_bot(BotId::Glambot).unwrap();
    assert!(!ctx.bot_completed(BotId::Glambot));

    // Complete bot
    ctx.complete_bot(BotId::Glambot, 5, 1, 10).unwrap();
    assert!(ctx.bot_completed(BotId::Glambot));

    // Check execution record
    let execution = ctx.executions.get(&BotId::Glambot).unwrap();
    assert_eq!(execution.findings_count, 5);
    assert_eq!(execution.errors_count, 1);
    assert_eq!(execution.files_analyzed, 10);
}

#[test]
fn test_register_all_bots() {
    let mut ctx = Context::new("test-repo", "/tmp/test");
    ctx.register_all_bots();

    assert!(ctx.executions.contains_key(&BotId::Rhodibot));
    assert!(ctx.executions.contains_key(&BotId::Echidnabot));
    assert!(ctx.executions.contains_key(&BotId::Sustainabot));
    assert!(ctx.executions.contains_key(&BotId::Glambot));
    assert!(ctx.executions.contains_key(&BotId::Seambot));
    assert!(ctx.executions.contains_key(&BotId::Finishbot));
}

#[test]
fn test_add_findings() {
    let mut ctx = Context::new("test-repo", "/tmp/test");

    let finding = Finding::new(
        BotId::Rhodibot,
        "RSR-001",
        Severity::Error,
        "Missing README",
    )
    .with_category("structure");

    ctx.add_finding(finding);

    assert_eq!(ctx.findings.len(), 1);
    assert!(ctx.has_errors());
    assert!(ctx.blocks_release());
}

#[test]
fn test_findings_by_source() {
    let mut ctx = Context::new("test-repo", "/tmp/test");

    ctx.add_finding(Finding::new(
        BotId::Rhodibot,
        "RSR-001",
        Severity::Error,
        "Issue 1",
    ));
    ctx.add_finding(Finding::new(
        BotId::Glambot,
        "WCAG-001",
        Severity::Warning,
        "Issue 2",
    ));
    ctx.add_finding(Finding::new(
        BotId::Rhodibot,
        "RSR-002",
        Severity::Info,
        "Issue 3",
    ));

    let rhodibot_findings = ctx.findings_from(BotId::Rhodibot);
    assert_eq!(rhodibot_findings.len(), 2);

    let glambot_findings = ctx.findings_from(BotId::Glambot);
    assert_eq!(glambot_findings.len(), 1);
}

#[test]
fn test_findings_by_tier() {
    let mut ctx = Context::new("test-repo", "/tmp/test");

    // Add verifier findings
    ctx.add_finding(Finding::new(
        BotId::Rhodibot,
        "RSR-001",
        Severity::Error,
        "Verifier issue 1",
    ));
    ctx.add_finding(Finding::new(
        BotId::Echidnabot,
        "MATH-001",
        Severity::Warning,
        "Verifier issue 2",
    ));

    // Add finisher findings
    ctx.add_finding(Finding::new(
        BotId::Glambot,
        "WCAG-001",
        Severity::Info,
        "Finisher issue 1",
    ));

    let verifier_findings = ctx.findings_from_tier(Tier::Verifier);
    assert_eq!(verifier_findings.len(), 2);

    let finisher_findings = ctx.findings_from_tier(Tier::Finisher);
    assert_eq!(finisher_findings.len(), 1);
}

#[test]
fn test_shared_data() {
    let mut ctx = Context::new("test-repo", "/tmp/test");

    ctx.set_data("key1", serde_json::json!("value1"));
    ctx.set_data("key2", serde_json::json!(42));
    ctx.set_data("key3", serde_json::json!({"nested": "object"}));

    assert_eq!(ctx.get_data("key1"), Some(&serde_json::json!("value1")));
    assert_eq!(ctx.get_data("key2"), Some(&serde_json::json!(42)));
    assert!(ctx.get_data("nonexistent").is_none());

    // Test typed retrieval
    let val: Option<i32> = ctx.get_data_as("key2");
    assert_eq!(val, Some(42));
}

#[test]
fn test_bot_dependencies() {
    let mut ctx = Context::new("test-repo", "/tmp/test");
    ctx.register_all_bots();

    // Initially, only bots with no dependencies should be ready
    let ready = ctx.ready_bots();

    // Verifiers (no deps) should be ready
    assert!(ready.contains(&BotId::Rhodibot));
    assert!(ready.contains(&BotId::Echidnabot));
    assert!(ready.contains(&BotId::Sustainabot));

    // Finishers (have deps) should NOT be ready
    assert!(!ready.contains(&BotId::Glambot)); // depends on rhodibot
    assert!(!ready.contains(&BotId::Finishbot)); // depends on rhodibot, glambot
}

#[test]
fn test_verifiers_complete() {
    let mut ctx = Context::new("test-repo", "/tmp/test");
    ctx.register_all_bots();

    assert!(!ctx.verifiers_complete());

    // Complete all verifiers
    ctx.start_bot(BotId::Rhodibot).unwrap();
    ctx.complete_bot(BotId::Rhodibot, 0, 0, 5).unwrap();

    ctx.start_bot(BotId::Echidnabot).unwrap();
    ctx.complete_bot(BotId::Echidnabot, 0, 0, 5).unwrap();

    ctx.start_bot(BotId::Sustainabot).unwrap();
    ctx.complete_bot(BotId::Sustainabot, 0, 0, 5).unwrap();

    assert!(ctx.verifiers_complete());
}

#[test]
fn test_context_summary() {
    let mut ctx = Context::new("test-repo", "/tmp/test");
    ctx.register_all_bots();

    // Run some bots
    ctx.start_bot(BotId::Rhodibot).unwrap();
    ctx.add_finding(Finding::new(
        BotId::Rhodibot,
        "RSR-001",
        Severity::Warning,
        "Warning",
    ));
    ctx.complete_bot(BotId::Rhodibot, 1, 0, 10).unwrap();

    ctx.start_bot(BotId::Glambot).unwrap();
    ctx.add_finding(Finding::new(
        BotId::Glambot,
        "WCAG-001",
        Severity::Error,
        "Error",
    ));
    ctx.complete_bot(BotId::Glambot, 1, 1, 5).unwrap();

    ctx.complete_session();

    let summary = ctx.summary();
    assert_eq!(summary.repo_name, "test-repo");
    assert_eq!(summary.bots_run, 2);
    assert_eq!(summary.total_findings, 2);
    assert_eq!(summary.total_errors, 1);
    assert_eq!(summary.total_warnings, 1);
    assert_eq!(summary.total_files_analyzed, 15);
    assert!(summary.blocks_release);
}

#[test]
fn test_bot_info() {
    let info = BotInfo::standard(BotId::Glambot);

    assert_eq!(info.id, BotId::Glambot);
    assert_eq!(info.name, "Glambot");
    assert!(info.categories.contains(&"accessibility".to_string()));
    assert!(info.categories.contains(&"seo".to_string()));
    assert!(info.can_fix);
    assert!(info.depends_on.contains(&BotId::Rhodibot));
}

#[test]
fn test_bot_tier() {
    assert_eq!(BotId::Rhodibot.tier(), Tier::Verifier);
    assert_eq!(BotId::Echidnabot.tier(), Tier::Verifier);
    assert_eq!(BotId::Sustainabot.tier(), Tier::Verifier);

    assert_eq!(BotId::Glambot.tier(), Tier::Finisher);
    assert_eq!(BotId::Seambot.tier(), Tier::Finisher);
    assert_eq!(BotId::Finishbot.tier(), Tier::Finisher);

    assert_eq!(BotId::Custom(1).tier(), Tier::Custom);
}

#[test]
fn test_finding_builder() {
    let finding = Finding::new(BotId::Glambot, "WCAG-1.1.1", Severity::Error, "Missing alt text")
        .with_rule_name("Image Alternative Text")
        .with_category("accessibility")
        .with_file(PathBuf::from("index.html"))
        .with_location(42, 15)
        .with_element("<img src=\"logo.png\">")
        .with_suggestion("Add alt attribute to describe the image")
        .fixable();

    assert_eq!(finding.rule_id, "WCAG-1.1.1");
    assert_eq!(finding.rule_name, "Image Alternative Text");
    assert_eq!(finding.category, "accessibility");
    assert_eq!(finding.file, Some(PathBuf::from("index.html")));
    assert_eq!(finding.line, Some(42));
    assert_eq!(finding.column, Some(15));
    assert!(finding.fixable);
    assert!(!finding.fixed);
}

#[test]
fn test_finding_location_string() {
    let finding1 = Finding::new(BotId::Glambot, "TEST-001", Severity::Info, "Test")
        .with_file(PathBuf::from("test.html"))
        .with_location(10, 5);
    assert_eq!(finding1.location_string(), Some("test.html:10:5".to_string()));

    let finding2 = Finding::new(BotId::Glambot, "TEST-002", Severity::Info, "Test")
        .with_file(PathBuf::from("test.html"))
        .with_line(10);
    assert_eq!(finding2.location_string(), Some("test.html:10".to_string()));

    let finding3 = Finding::new(BotId::Glambot, "TEST-003", Severity::Info, "Test")
        .with_file(PathBuf::from("test.html"));
    assert_eq!(finding3.location_string(), Some("test.html".to_string()));

    let finding4 = Finding::new(BotId::Glambot, "TEST-004", Severity::Info, "Test");
    assert_eq!(finding4.location_string(), None);
}

#[tokio::test]
async fn test_context_storage() {
    let temp_dir = TempDir::new().unwrap();
    let storage = ContextStorage::new(temp_dir.path());

    // Create and save context
    let mut ctx = Context::new("test-repo", "/tmp/test");
    ctx.add_finding(Finding::new(
        BotId::Rhodibot,
        "TEST-001",
        Severity::Info,
        "Test finding",
    ));

    storage.save_context(&ctx).unwrap();

    // Load context
    let loaded = storage.load_context("test-repo").unwrap();
    assert_eq!(loaded.repo_name, "test-repo");
    assert_eq!(loaded.findings.len(), 1);
}

#[tokio::test]
async fn test_repo_state() {
    let temp_dir = TempDir::new().unwrap();
    let storage = ContextStorage::new(temp_dir.path());

    // Create repo state
    let mut state = RepoState::new("test-repo");
    state.add_session_summary(
        uuid::Uuid::new_v4(),
        5,
        2,
        1,
    );

    storage.save_repo_state(&state).unwrap();

    // Load repo state
    let loaded = storage.load_repo_state("test-repo").unwrap();
    assert_eq!(loaded.repo_name, "test-repo");
    assert_eq!(loaded.session_count, 1);
}
