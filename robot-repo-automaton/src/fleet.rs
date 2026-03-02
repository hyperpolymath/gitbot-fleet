// SPDX-License-Identifier: PMPL-1.0-or-later
//! Fleet integration for gitbot-fleet coordination
//!
//! This module provides the bridge between robot-repo-automaton and the gitbot-fleet
//! shared-context coordination layer. It handles:
//!
//! - Registering robot-repo-automaton as a Tier 3 Executor
//! - Publishing findings after compliance scanning and fixing
//! - Coordinating with other bots in the fleet
//! - Consuming rules from hypatia

use crate::detector::DetectedIssue;
use crate::error::{Error, Result};
use gitbot_shared_context::{BotId, Context, Finding, Severity as FleetSeverity};
use std::path::PathBuf;
use tracing::{debug, info};

/// Fleet coordinator for robot-repo-automaton
pub struct FleetCoordinator {
    /// Shared context (if connected to fleet)
    context: Option<Context>,
}

impl FleetCoordinator {
    /// Create a new fleet coordinator (not yet connected)
    pub fn new() -> Self {
        Self { context: None }
    }

    /// Connect to fleet for a repository analysis session
    ///
    /// This creates a shared context and registers robot-repo-automaton as a Tier 3 Executor.
    pub fn connect(&mut self, repo_name: &str, repo_path: impl Into<PathBuf>) -> Result<()> {
        info!("Connecting to gitbot-fleet for repo: {}", repo_name);

        let mut ctx = Context::new(repo_name, repo_path);
        ctx.register_all_bots();

        // Mark robot-repo-automaton as started
        ctx.start_bot(BotId::RobotRepoAutomaton)
            .map_err(|e| Error::Internal(format!("Failed to start bot: {}", e)))?;

        self.context = Some(ctx);
        Ok(())
    }

    /// Disconnect from fleet (mark robot-repo-automaton as complete)
    pub fn disconnect(&mut self, findings_count: usize, errors_count: usize, files_analyzed: usize) -> Result<()> {
        if let Some(ref mut ctx) = self.context {
            info!("Disconnecting from gitbot-fleet (findings: {}, errors: {}, files: {})",
                  findings_count, errors_count, files_analyzed);

            ctx.complete_bot(BotId::RobotRepoAutomaton, findings_count, errors_count, files_analyzed)
                .map_err(|e| Error::Internal(format!("Failed to complete bot: {}", e)))?;

            // TODO: Persist context to ~/.gitbot-fleet/sessions/
        }

        self.context = None;
        Ok(())
    }

    /// Publish findings from detected compliance issues
    ///
    /// This publishes compliance violations found during scanning.
    pub fn publish_detections(&mut self, issues: &[DetectedIssue]) -> Result<()> {
        if let Some(ref mut ctx) = self.context {
            debug!("Publishing {} compliance findings", issues.len());

            for issue in issues {
                let severity = convert_severity(&issue.severity);

                for file in &issue.affected_files {
                    let finding = Finding::new(
                        BotId::RobotRepoAutomaton,
                        "COMPLIANCE-VIOLATION",
                        severity,
                        &issue.description,
                    )
                    .with_rule_name(&issue.error_name)
                    .with_category("compliance")
                    .with_file(file.clone())
                    .with_suggestion(&format!(
                        "Compliance issue detected: {}. Run 'robot-repo-automaton fix' to auto-fix.",
                        issue.error_name
                    ))
                    .with_metadata(serde_json::json!({
                        "error_type_id": issue.error_type_id,
                        "severity": format!("{:?}", issue.severity),
                        "confidence": issue.confidence,
                        "suggested_fix": issue.suggested_fix,
                    }));

                    ctx.add_finding(finding);
                }
            }

            Ok(())
        } else {
            // Not connected to fleet - skip publishing
            debug!("Not connected to fleet, skipping finding publication");
            Ok(())
        }
    }

    /// Publish fix results after applying automated fixes
    ///
    /// This publishes successful fixes applied by robot-repo-automaton.
    pub fn publish_fixes(&mut self, fixed_count: usize, files: &[PathBuf]) -> Result<()> {
        if let Some(ref mut ctx) = self.context {
            debug!("Publishing {} successful fixes", fixed_count);

            for file in files {
                let finding = Finding::new(
                    BotId::RobotRepoAutomaton,
                    "COMPLIANCE-FIXED",
                    FleetSeverity::Info,
                    &format!("Compliance issues automatically fixed in {}", file.display()),
                )
                .with_rule_name("Automated Compliance Fix")
                .with_category("compliance-fix")
                .with_file(file.clone())
                .with_metadata(serde_json::json!({
                    "auto_fixed": true,
                    "fixes_applied": fixed_count,
                }));

                ctx.add_finding(finding);
            }

            Ok(())
        } else {
            debug!("Not connected to fleet, skipping fix publication");
            Ok(())
        }
    }

    /// Report fix outcomes to Hypatia's neurosymbolic learning loop.
    ///
    /// When robot-repo-automaton applies a fix (or fails to apply one),
    /// this method records the outcome so the learning engine can adjust
    /// confidence thresholds and propose new rules based on real-world
    /// success/failure rates.
    pub fn report_fix_outcome(
        &mut self,
        pattern: &str,
        success: bool,
        fix_type: &str,
        confidence: &str,
    ) -> Result<()> {
        if let Some(ref mut ctx) = self.context {
            let outcome_label = if success { "success" } else { "failure" };
            info!(
                "Reporting fix outcome to learning loop: pattern={}, result={}, confidence={}",
                pattern, outcome_label, confidence
            );

            let finding = Finding::new(
                BotId::RobotRepoAutomaton,
                "FIX-OUTCOME",
                if success { FleetSeverity::Info } else { FleetSeverity::Warning },
                &format!(
                    "Fix {} for pattern '{}' (confidence: {})",
                    outcome_label, pattern, confidence
                ),
            )
            .with_rule_name(pattern)
            .with_category("fix-outcome")
            .with_metadata(serde_json::json!({
                "pattern": pattern,
                "success": success,
                "fix_type": fix_type,
                "confidence": confidence,
                "learning_loop": true,
            }));

            ctx.add_finding(finding);
            Ok(())
        } else {
            debug!("Not connected to fleet, skipping fix outcome report");
            Ok(())
        }
    }

    /// Get current context (for inspection)
    pub fn context(&self) -> Option<&Context> {
        self.context.as_ref()
    }

    /// Check if connected to fleet
    pub fn is_connected(&self) -> bool {
        self.context.is_some()
    }

    /// Get session ID (if connected)
    pub fn session_id(&self) -> Option<uuid::Uuid> {
        self.context.as_ref().map(|ctx| ctx.session_id)
    }
}

impl Default for FleetCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert robot-repo-automaton severity to fleet severity
fn convert_severity(severity: &crate::catalog::Severity) -> FleetSeverity {
    use crate::catalog::Severity;
    match severity {
        Severity::Critical => FleetSeverity::Error,
        Severity::High => FleetSeverity::Error,
        Severity::Medium => FleetSeverity::Warning,
        Severity::Low => FleetSeverity::Info,
        Severity::Info => FleetSeverity::Info,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::catalog::Severity;
    use std::path::PathBuf;

    #[test]
    fn test_fleet_coordinator_lifecycle() {
        let mut coordinator = FleetCoordinator::new();
        assert!(!coordinator.is_connected());

        // Connect
        coordinator.connect("test-repo", "/tmp/test-repo").unwrap();
        assert!(coordinator.is_connected());
        assert!(coordinator.session_id().is_some());

        // Disconnect
        coordinator.disconnect(5, 2, 10).unwrap();
        assert!(!coordinator.is_connected());
    }

    #[test]
    fn test_publish_detections() {
        let mut coordinator = FleetCoordinator::new();
        coordinator.connect("test-repo", "/tmp/test-repo").unwrap();

        let issues = vec![
            DetectedIssue {
                error_type_id: "MISSING-LICENSE".to_string(),
                error_name: "Missing License File".to_string(),
                severity: Severity::High,
                description: "Repository missing LICENSE file".to_string(),
                affected_files: vec![PathBuf::from(".")],
                confidence: 1.0,
                suggested_fix: "Add LICENSE file".to_string(),
                commit_message: "Add LICENSE file".to_string(),
            },
        ];

        coordinator.publish_detections(&issues).unwrap();

        // Verify findings were added to context
        let ctx = coordinator.context().unwrap();
        assert!(!ctx.findings_from(BotId::RobotRepoAutomaton).is_empty());
    }

    #[test]
    fn test_publish_fixes() {
        let mut coordinator = FleetCoordinator::new();
        coordinator.connect("test-repo", "/tmp/test-repo").unwrap();

        let files = vec![PathBuf::from("LICENSE"), PathBuf::from("README.md")];
        coordinator.publish_fixes(2, &files).unwrap();

        // Verify fix findings were added
        let ctx = coordinator.context().unwrap();
        assert!(!ctx.findings_from(BotId::RobotRepoAutomaton).is_empty());
    }

    #[test]
    fn test_severity_conversion() {
        assert_eq!(convert_severity(&Severity::Critical), FleetSeverity::Error);
        assert_eq!(convert_severity(&Severity::High), FleetSeverity::Error);
        assert_eq!(convert_severity(&Severity::Medium), FleetSeverity::Warning);
        assert_eq!(convert_severity(&Severity::Low), FleetSeverity::Info);
        assert_eq!(convert_severity(&Severity::Info), FleetSeverity::Info);
    }
}
