// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Fleet integration for finishing-bot
//!
//! Provides integration with gitbot-fleet's shared context layer,
//! allowing finishing-bot to:
//! - Report findings to the shared context
//! - Access findings from verifier bots (rhodibot, glambot)
//! - Participate in coordinated release validation sessions
//! - Receive release rules from cicd-hyper-a

#[cfg(feature = "fleet")]
use gitbot_shared_context::{
    BotId, BotInfo, Context, Finding as FleetFinding, Severity as FleetSeverity, Tier,
};

/// cicd-hyper-a integration for release rules
pub mod cicd_hyper_a {
    use serde::{Deserialize, Serialize};

    /// Release rule from cicd-hyper-a
    #[derive(Debug, Clone, Deserialize, Serialize)]
    pub struct ReleaseRule {
        /// Rule identifier
        pub id: String,
        /// Rule category (license, placeholder, claims, release)
        pub category: String,
        /// Check to perform
        pub check: ReleaseCheck,
        /// Severity level
        pub severity: String,
        /// Whether auto-fix is available
        pub fixable: bool,
        /// Block release if violated
        pub blocks_release: bool,
    }

    /// Release check types
    #[derive(Debug, Clone, Deserialize, Serialize)]
    #[serde(tag = "type", rename_all = "kebab-case")]
    pub enum ReleaseCheck {
        /// License validation
        License { spdx_id: Option<String>, require_header: bool },
        /// Placeholder detection
        Placeholder { patterns: Vec<String>, allow_in_tests: bool },
        /// Claim verification
        Claim { claim_type: String, verify_method: String },
        /// Release artifact check
        Artifact { required_files: Vec<String> },
        /// Version consistency
        Version { files_to_check: Vec<String> },
    }

    /// cicd-hyper-a configuration for finishing-bot
    #[derive(Debug, Clone, Default, Deserialize, Serialize)]
    pub struct CicdHyperAConfig {
        /// Enable cicd-hyper-a rule fetching
        #[serde(default)]
        pub enabled: bool,
        /// API endpoint for rules
        #[serde(default = "default_endpoint")]
        pub endpoint: String,
        /// Rulesets to fetch
        #[serde(default)]
        pub rulesets: Vec<String>,
        /// Strict mode - block release on any violation
        #[serde(default)]
        pub strict_mode: bool,
    }

    fn default_endpoint() -> String {
        "http://localhost:8080/api/v1".to_string()
    }

    /// Fetch release rules from cicd-hyper-a
    pub async fn fetch_release_rules(
        _config: &CicdHyperAConfig,
    ) -> Result<Vec<ReleaseRule>, String> {
        // In a real implementation, this would call cicd-hyper-a API
        // For now, return default rules
        Ok(vec![
            ReleaseRule {
                id: "LIC-SPDX".to_string(),
                category: "license".to_string(),
                check: ReleaseCheck::License {
                    spdx_id: None,
                    require_header: true,
                },
                severity: "error".to_string(),
                fixable: true,
                blocks_release: true,
            },
            ReleaseRule {
                id: "PH-TODO".to_string(),
                category: "placeholder".to_string(),
                check: ReleaseCheck::Placeholder {
                    patterns: vec!["TODO".to_string(), "FIXME".to_string(), "XXX".to_string()],
                    allow_in_tests: true,
                },
                severity: "warning".to_string(),
                fixable: true,
                blocks_release: false,
            },
            ReleaseRule {
                id: "REL-VERSION".to_string(),
                category: "release".to_string(),
                check: ReleaseCheck::Version {
                    files_to_check: vec![
                        "Cargo.toml".to_string(),
                        "package.json".to_string(),
                        "VERSION".to_string(),
                    ],
                },
                severity: "error".to_string(),
                fixable: false,
                blocks_release: true,
            },
        ])
    }

    /// Report finishing-bot findings to cicd-hyper-a learning pipeline
    pub async fn report_to_learning_pipeline(
        _findings: &[super::Finding],
        _release_blocked: bool,
        _config: &CicdHyperAConfig,
    ) -> Result<(), String> {
        // In a real implementation, POST findings to cicd-hyper-a
        Ok(())
    }

    /// Check if release should be blocked based on cicd-hyper-a rules
    pub fn should_block_release(
        findings: &[super::Finding],
        rules: &[ReleaseRule],
    ) -> bool {
        for finding in findings {
            for rule in rules {
                if finding.id == rule.id && rule.blocks_release {
                    if matches!(
                        finding.severity,
                        super::Severity::Error
                    ) {
                        return true;
                    }
                }
            }
        }
        false
    }
}

use crate::analyzers::{AuditResult, Finding, Severity};
use crate::config::Config;

/// Convert finishing-bot Severity to fleet Severity
#[cfg(feature = "fleet")]
pub fn to_fleet_severity(severity: Severity) -> FleetSeverity {
    match severity {
        Severity::Error => FleetSeverity::Error,
        Severity::Warning => FleetSeverity::Warning,
        Severity::Info => FleetSeverity::Info,
        Severity::Suggestion => FleetSeverity::Suggestion,
    }
}

/// Convert fleet Severity to finishing-bot Severity
#[cfg(feature = "fleet")]
pub fn from_fleet_severity(severity: FleetSeverity) -> Severity {
    match severity {
        FleetSeverity::Error => Severity::Error,
        FleetSeverity::Warning => Severity::Warning,
        FleetSeverity::Info => Severity::Info,
        FleetSeverity::Suggestion => Severity::Suggestion,
    }
}

/// Convert finishing-bot Finding to fleet Finding
#[cfg(feature = "fleet")]
pub fn to_fleet_finding(finding: &Finding, category: &str) -> FleetFinding {
    let mut fleet_finding = FleetFinding::new(
        BotId::Finishbot,
        &finding.id,
        to_fleet_severity(finding.severity),
        &finding.message,
    )
    .with_rule_name(&finding.name)
    .with_category(category);

    if let Some(ref file) = finding.file {
        fleet_finding = fleet_finding.with_file(file.clone());
    }

    if let (Some(line), Some(col)) = (finding.line, finding.column) {
        fleet_finding = fleet_finding.with_location(line, col);
    } else if let Some(line) = finding.line {
        fleet_finding = fleet_finding.with_line(line);
    }

    if let Some(ref suggestion) = finding.suggestion {
        fleet_finding = fleet_finding.with_suggestion(suggestion);
    }

    if finding.fixable {
        fleet_finding = fleet_finding.fixable();
    }

    fleet_finding
}

/// Convert fleet Finding to finishing-bot Finding
#[cfg(feature = "fleet")]
pub fn from_fleet_finding(finding: &FleetFinding) -> Finding {
    let mut local = Finding::new(
        &finding.rule_id,
        &finding.rule_name,
        from_fleet_severity(finding.severity),
        &finding.message,
    );

    if let Some(ref file) = finding.file {
        local = local.with_file(file.clone());
    }

    if let (Some(line), Some(col)) = (finding.line, finding.column) {
        local = local.with_location(line, col);
    } else if let Some(line) = finding.line {
        local = local.with_line(line);
    }

    if let Some(ref suggestion) = finding.suggestion {
        local = local.with_suggestion(suggestion);
    }

    if finding.fixable {
        local = local.fixable();
    }

    local
}

/// Run finishing-bot within a fleet context
#[cfg(feature = "fleet")]
pub fn run_in_context(ctx: &mut Context, config: &Config) -> AuditResult {
    use crate::analyzers::{
        claims::ClaimsAnalyzer, license::LicenseAnalyzer, placeholder::PlaceholderAnalyzer,
        release::ReleaseAnalyzer, Analyzer,
    };
    use tracing::{error, info, warn};

    // Register and start finishing-bot
    ctx.register_bot(BotId::Finishbot);
    if let Err(e) = ctx.start_bot(BotId::Finishbot) {
        error!("Failed to start finishing-bot in context: {}", e);
    }

    // Check dependencies - finishing-bot depends on rhodibot and glambot
    let rhodibot_count = ctx.findings_from(BotId::Rhodibot).len();
    let glambot_count = ctx.findings_from(BotId::Glambot).len();

    info!(
        "Received {} findings from rhodibot, {} from glambot",
        rhodibot_count, glambot_count
    );

    // Store dependency info in context
    ctx.set_data(
        "finishing-bot.rhodibot_findings",
        serde_json::json!(rhodibot_count),
    );
    ctx.set_data(
        "finishing-bot.glambot_findings",
        serde_json::json!(glambot_count),
    );

    // Check if we should warn about missing dependencies
    if !ctx.bot_completed(BotId::Rhodibot) {
        warn!("Rhodibot has not completed - some checks may be incomplete");
    }
    if !ctx.bot_completed(BotId::Glambot) {
        warn!("Glambot has not completed - some checks may be incomplete");
    }

    let mut result = AuditResult::default();
    let mut findings_count = 0;
    let mut errors_count = 0;
    let mut files_checked = 0;

    // Run license analyzer
    let license = LicenseAnalyzer::default();
    match license.analyze(&ctx.repo_path, config) {
        Ok(analysis) => {
            files_checked += analysis.files_checked;
            for finding in &analysis.findings {
                let fleet_finding = to_fleet_finding(finding, "license");
                if finding.severity == Severity::Error {
                    errors_count += 1;
                }
                ctx.add_finding(fleet_finding);
                findings_count += 1;
            }
            result.license = analysis;
        }
        Err(e) => {
            error!("License analysis error: {}", e);
        }
    }

    // Run placeholder analyzer
    let placeholder = PlaceholderAnalyzer::default();
    match placeholder.analyze(&ctx.repo_path, config) {
        Ok(analysis) => {
            files_checked += analysis.files_checked;
            for finding in &analysis.findings {
                let fleet_finding = to_fleet_finding(finding, "placeholder");
                if finding.severity == Severity::Error {
                    errors_count += 1;
                }
                ctx.add_finding(fleet_finding);
                findings_count += 1;
            }
            result.placeholder = analysis;
        }
        Err(e) => {
            error!("Placeholder analysis error: {}", e);
        }
    }

    // Run claims analyzer
    let claims = ClaimsAnalyzer::default();
    match claims.analyze(&ctx.repo_path, config) {
        Ok(analysis) => {
            files_checked += analysis.files_checked;
            for finding in &analysis.findings {
                let fleet_finding = to_fleet_finding(finding, "claims");
                if finding.severity == Severity::Error {
                    errors_count += 1;
                }
                ctx.add_finding(fleet_finding);
                findings_count += 1;
            }
            result.claims = analysis;
        }
        Err(e) => {
            error!("Claims analysis error: {}", e);
        }
    }

    // Run release analyzer
    let release = ReleaseAnalyzer::default();
    match release.analyze(&ctx.repo_path, config) {
        Ok(analysis) => {
            files_checked += analysis.files_checked;
            for finding in &analysis.findings {
                let fleet_finding = to_fleet_finding(finding, "release");
                if finding.severity == Severity::Error {
                    errors_count += 1;
                }
                ctx.add_finding(fleet_finding);
                findings_count += 1;
            }
            result.release = analysis;
        }
        Err(e) => {
            error!("Release analysis error: {}", e);
        }
    }

    // Complete bot execution
    if let Err(e) = ctx.complete_bot(BotId::Finishbot, findings_count, errors_count, files_checked)
    {
        error!("Failed to complete finishing-bot in context: {}", e);
    }

    // Store release readiness in context for other bots
    ctx.set_data(
        "finishing-bot.release_ready",
        serde_json::json!(!result.should_block_release()),
    );

    result
}

/// Get relevant findings from verifier bots
#[cfg(feature = "fleet")]
pub fn get_verifier_findings(ctx: &Context) -> Vec<Finding> {
    ctx.findings_from_tier(Tier::Verifier)
        .into_iter()
        .map(from_fleet_finding)
        .collect()
}

/// Get findings from rhodibot specifically (finishing-bot's dependency)
#[cfg(feature = "fleet")]
pub fn get_rhodibot_findings(ctx: &Context) -> Vec<Finding> {
    ctx.findings_from(BotId::Rhodibot)
        .into_iter()
        .map(from_fleet_finding)
        .collect()
}

/// Get findings from glambot specifically (finishing-bot's dependency)
#[cfg(feature = "fleet")]
pub fn get_glambot_findings(ctx: &Context) -> Vec<Finding> {
    ctx.findings_from(BotId::Glambot)
        .into_iter()
        .map(from_fleet_finding)
        .collect()
}

/// Get finishing-bot's BotInfo for fleet registration
#[cfg(feature = "fleet")]
pub fn bot_info() -> BotInfo {
    BotInfo::standard(BotId::Finishbot)
}

/// Check if finishing-bot's dependencies are satisfied in the context
#[cfg(feature = "fleet")]
pub fn dependencies_satisfied(ctx: &Context) -> bool {
    let info = BotInfo::standard(BotId::Finishbot);
    info.depends_on.iter().all(|dep| ctx.bot_completed(*dep))
}

/// Check if repository is ready for release based on all fleet findings
#[cfg(feature = "fleet")]
pub fn is_release_ready(ctx: &Context) -> bool {
    !ctx.blocks_release()
}

#[cfg(test)]
#[cfg(feature = "fleet")]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_severity_conversion_roundtrip() {
        for severity in [
            Severity::Error,
            Severity::Warning,
            Severity::Info,
            Severity::Suggestion,
        ] {
            let fleet = to_fleet_severity(severity);
            let back = from_fleet_severity(fleet);
            assert_eq!(severity, back);
        }
    }

    #[test]
    fn test_finding_conversion() {
        let finding = Finding::new("LIC-001", "Missing License", Severity::Error, "No LICENSE file")
            .with_file(PathBuf::from("LICENSE"))
            .with_suggestion("Add a LICENSE file");

        let fleet_finding = to_fleet_finding(&finding, "license");

        assert_eq!(fleet_finding.rule_id, "LIC-001");
        assert_eq!(fleet_finding.category, "license");
        assert_eq!(fleet_finding.source, BotId::Finishbot);
    }

    #[test]
    fn test_bot_info() {
        let info = bot_info();
        assert_eq!(info.id, BotId::Finishbot);
        assert!(info.categories.contains(&"license".to_string()));
        assert!(info.categories.contains(&"release".to_string()));
        assert!(info.depends_on.contains(&BotId::Rhodibot));
        assert!(info.depends_on.contains(&BotId::Glambot));
    }
}
