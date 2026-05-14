// SPDX-License-Identifier: PMPL-1.0-or-later
//! Fleet coordination integration tests

use gitbot_shared_context::{BotId, Context, Finding, Severity};
use std::path::PathBuf;

#[test]
fn test_fleet_context_creation() {
    let ctx = Context::new("test-repo", PathBuf::from("/tmp/test-repo"));
    assert_eq!(ctx.repo_name, "test-repo");
}

#[test]
fn test_bot_registration() {
    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test-repo"));
    ctx.register_all_bots();

    let bots = [BotId::Rhodibot, BotId::Echidnabot, BotId::Sustainabot, BotId::Glambot, BotId::Seambot, BotId::Finishbot, BotId::RobotRepoAutomaton, BotId::Hypatia];

    for bot in &bots {
        assert!(ctx.executions.contains_key(bot));
    }
}

#[test]
fn test_finding_publication() {
    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test-repo"));
    ctx.register_all_bots();
    ctx.start_bot(BotId::RobotRepoAutomaton).unwrap();

    let finding = Finding::new(BotId::RobotRepoAutomaton, "TEST-FINDING", Severity::Warning, "Test finding description");
    ctx.add_finding(finding);

    let findings = ctx.findings_from(BotId::RobotRepoAutomaton);
    assert_eq!(findings.len(), 1);
    assert_eq!(findings[0].rule_id, "TEST-FINDING");
}

#[test]
fn test_cross_bot_findings() {
    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test-repo"));
    ctx.register_all_bots();
    ctx.start_bot(BotId::Echidnabot).unwrap();
    ctx.start_bot(BotId::RobotRepoAutomaton).unwrap();

    ctx.add_finding(Finding::new(BotId::Echidnabot, "PROOF-VERIFIED", Severity::Info, "Contract verified successfully"));
    ctx.add_finding(Finding::new(BotId::RobotRepoAutomaton, "COMPLIANCE-VIOLATION", Severity::Error, "Missing LICENSE file"));

    assert_eq!(ctx.findings_from(BotId::Echidnabot).len(), 1);
    assert_eq!(ctx.findings_from(BotId::RobotRepoAutomaton).len(), 1);
    assert_eq!(ctx.findings.findings.len(), 2);
}

#[test]
fn test_session_lifecycle() {
    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test-repo"));
    ctx.register_all_bots();

    ctx.start_bot(BotId::Seambot).unwrap();
    assert!(ctx.executions.get(&BotId::Seambot).unwrap().started_at.is_some());

    ctx.complete_bot(BotId::Seambot, 5, 2, 10).unwrap();
    let exec = ctx.executions.get(&BotId::Seambot).unwrap();
    assert!(exec.completed_at.is_some());
    assert_eq!(exec.findings_count, 5);
}

#[test]
fn test_tier_hierarchy() {
    let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test-repo"));
    ctx.register_all_bots();

    ctx.start_bot(BotId::Hypatia).unwrap();
    ctx.start_bot(BotId::Echidnabot).unwrap();
    ctx.start_bot(BotId::Seambot).unwrap();
    ctx.start_bot(BotId::RobotRepoAutomaton).unwrap();

    assert!(ctx.executions.get(&BotId::Hypatia).unwrap().started_at.is_some());
    assert!(ctx.executions.get(&BotId::RobotRepoAutomaton).unwrap().started_at.is_some());
}
