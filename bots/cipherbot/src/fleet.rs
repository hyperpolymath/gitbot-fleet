// SPDX-License-Identifier: PMPL-1.0-or-later
//! Fleet Integration — cipherbot's integration with the gitbot-fleet ecosystem.
//!
//! Registers as `BotId::Custom(100)` in gitbot-shared-context.
//!
//! Finding categories:
//! - `"crypto/deprecated"` — deprecated algorithms
//! - `"crypto/weak"` — weak but not broken
//! - `"crypto/pq-vulnerable"` — classical-only (not PQ-safe)
//! - `"crypto/config"` — configuration issues
//! - `"crypto/dependency"` — dependency vulnerabilities
//! - `"crypto/protocol"` — protocol-level issues

use gitbot_shared_context::bot::{BotExecution, BotInfo};
use gitbot_shared_context::finding::FindingSet;
use crate::analyzers::{self, CIPHERBOT_ID};
use crate::pq_readiness;
use std::path::Path;

/// Cipherbot's BotInfo registration for the fleet.
pub fn bot_info() -> BotInfo {
    BotInfo {
        id: CIPHERBOT_ID,
        name: "Cipherbot".to_string(),
        description: "Cryptographic hygiene, protocol compliance, and post-quantum readiness"
            .to_string(),
        version: env!("CARGO_PKG_VERSION").to_string(),
        categories: vec![
            "crypto/deprecated".to_string(),
            "crypto/weak".to_string(),
            "crypto/pq-vulnerable".to_string(),
            "crypto/config".to_string(),
            "crypto/dependency".to_string(),
            "crypto/protocol".to_string(),
        ],
        can_fix: false,
        depends_on: vec![],
    }
}

/// Bot operating mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BotMode {
    /// Report findings, suggest migrations, don't block.
    Advisor,
    /// Only analyze when @cipherbot mentioned.
    Consultant,
    /// Block PRs with REJECT-level findings.
    Regulator,
    /// Enforce organization-wide crypto policy.
    Policy,
}

impl BotMode {
    /// Parse mode from a string identifier.
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "advisor" => Some(BotMode::Advisor),
            "consultant" => Some(BotMode::Consultant),
            "regulator" => Some(BotMode::Regulator),
            "policy" => Some(BotMode::Policy),
            _ => None,
        }
    }

    /// Whether this mode blocks on errors.
    pub fn blocks_on_error(&self) -> bool {
        matches!(self, BotMode::Regulator | BotMode::Policy)
    }
}

/// Run cipherbot as a fleet member against a directory.
///
/// Returns a `FindingSet` containing all detected crypto issues,
/// plus a PQ readiness assessment finding.
pub fn run_fleet_scan(dir: &Path, mode: BotMode) -> (FindingSet, BotExecution) {
    let mut execution = BotExecution::new(CIPHERBOT_ID);
    execution.start();

    // Run all analyzers
    let all_findings = analyzers::run_all_analyzers(dir);

    // Get raw usages for PQ assessment
    let all_analyzers: Vec<Box<dyn analyzers::Analyzer>> = vec![
        Box::new(analyzers::hashing::HashingAnalyzer),
        Box::new(analyzers::symmetric::SymmetricAnalyzer),
        Box::new(analyzers::key_exchange::KeyExchangeAnalyzer),
        Box::new(analyzers::signatures::SignatureAnalyzer),
        Box::new(analyzers::password::PasswordAnalyzer),
        Box::new(analyzers::protocol::ProtocolAnalyzer),
        Box::new(analyzers::rng::RngAnalyzer),
    ];

    let mut all_usages = Vec::new();
    for analyzer in &all_analyzers {
        all_usages.extend(analyzer.analyze_directory(dir));
    }

    // PQ readiness assessment
    let pq_assessment = pq_readiness::assess(&all_usages);
    let pq_finding = pq_readiness::assessment_to_finding(&pq_assessment);

    // Build finding set
    let mut finding_set = FindingSet::new();
    let findings_count = all_findings.len() + 1; // +1 for PQ assessment

    for finding in all_findings {
        finding_set.add(finding);
    }
    finding_set.add(pq_finding);

    // Count files analyzed (approximate via unique files in usages)
    let files_analyzed: std::collections::HashSet<_> = all_usages
        .iter()
        .map(|u| u.file.clone())
        .collect();

    let error_count = finding_set.errors().len();
    execution.complete(findings_count, error_count, files_analyzed.len());

    // In regulator/policy mode, mark as failed if there are errors
    if mode.blocks_on_error() && finding_set.has_errors() {
        execution.fail(&format!(
            "Cipherbot found {} blocking error(s) in {} mode",
            error_count,
            match mode {
                BotMode::Regulator => "regulator",
                BotMode::Policy => "policy",
                _ => "unknown",
            }
        ));
    }

    (finding_set, execution)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bot_info() {
        let info = bot_info();
        assert_eq!(info.id, CIPHERBOT_ID);
        assert_eq!(info.name, "Cipherbot");
        assert_eq!(info.categories.len(), 6);
        assert!(!info.can_fix);
    }

    #[test]
    fn test_bot_mode_from_str() {
        assert_eq!(BotMode::parse("advisor"), Some(BotMode::Advisor));
        assert_eq!(BotMode::parse("consultant"), Some(BotMode::Consultant));
        assert_eq!(BotMode::parse("regulator"), Some(BotMode::Regulator));
        assert_eq!(BotMode::parse("policy"), Some(BotMode::Policy));
        assert_eq!(BotMode::parse("invalid"), None);
    }

    #[test]
    fn test_blocks_on_error() {
        assert!(!BotMode::Advisor.blocks_on_error());
        assert!(!BotMode::Consultant.blocks_on_error());
        assert!(BotMode::Regulator.blocks_on_error());
        assert!(BotMode::Policy.blocks_on_error());
    }
}
