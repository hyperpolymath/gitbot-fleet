// SPDX-License-Identifier: PMPL-1.0-or-later
//! Fleet Integration — panicbot's integration with the gitbot-fleet ecosystem.
//!
//! Registers as `BotId::Panicbot` in gitbot-shared-context. Provides the
//! standard fleet entry point (`run_fleet_scan`) that:
//! 1. Reads per-repo directives (`.machine_readable/bot_directives/panicbot.scm`)
//! 2. Invokes `panic-attack assail` via the scanner module
//! 3. Translates WeakPoints into Finding structs
//! 4. Classifies fixable vs unfixable
//! 5. Writes unfixable to `.panicbot/PANICBOT-FINDINGS.a2ml`
//! 6. Returns FindingSet for the fleet pipeline

use crate::a2ml_writer;
use crate::directives;
use crate::scanner;
use crate::translator;
use gitbot_shared_context::bot::{BotExecution, BotId, BotInfo};
use gitbot_shared_context::finding::FindingSet;
use std::path::Path;

/// Panicbot's BotInfo registration for the fleet.
pub fn bot_info() -> BotInfo {
    BotInfo::standard(BotId::Panicbot)
}

/// Bot operating mode.
///
/// Controls scan depth and whether findings can block PRs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BotMode {
    /// Report findings, don't block. Runs `assail` only.
    Advisor,
    /// Full suite: `assail` + `adjudicate` + `diagnostics`.
    Auditor,
    /// Block PRs on critical findings. Runs `assail` + strict thresholds.
    Guardian,
}

impl BotMode {
    /// Parse mode from a string identifier.
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "advisor" => Some(BotMode::Advisor),
            "auditor" => Some(BotMode::Auditor),
            "guardian" => Some(BotMode::Guardian),
            _ => None,
        }
    }

    /// Whether this mode blocks on critical findings.
    pub fn blocks_on_error(&self) -> bool {
        matches!(self, BotMode::Guardian)
    }
}

/// Run panicbot as a fleet member against a repository directory.
///
/// Performs the complete scan pipeline:
/// 1. Load per-repo directives
/// 2. Run panic-attack assail
/// 3. Translate findings
/// 4. Classify fixable/unfixable
/// 5. Write A2ML debt register
/// 6. Return FindingSet + BotExecution
pub fn run_fleet_scan(dir: &Path, mode: BotMode) -> (FindingSet, BotExecution) {
    let mut execution = BotExecution::new(BotId::Panicbot);
    execution.start();

    // Step 1: Load per-repo directives
    let config = match directives::load_directives(dir) {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!("Failed to load directives: {}, using defaults", e);
            crate::config::PanicbotConfig::default()
        }
    };

    // Step 2: Run panic-attack assail
    if !config.allowed_commands.assail {
        tracing::info!("assail command disabled by directives, skipping scan");
        execution.skip("assail disabled by bot directives");
        return (FindingSet::new(), execution);
    }

    let assail_result = scanner::run_assail(dir, config.timeout);
    let report = match assail_result {
        Ok(r) => r,
        Err(e) => {
            let msg = format!("panic-attack assail failed: {}", e);
            tracing::error!("{}", msg);
            execution.fail(&msg);
            return (FindingSet::new(), execution);
        }
    };

    // Step 3: Translate WeakPoints into Findings
    let findings = translator::translate_all(&report.weak_points, &config);
    let findings_count = findings.len();

    // Step 4: Classify fixable vs unfixable
    let (fixable, unfixable) = translator::classify_fixability(&findings);

    // Step 5: Write A2ML debt register (if there are unfixable findings)
    let repo_name = dir
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    // Try to get panic-attack version for A2ML metadata
    let pa_version = scanner::run_diagnostics(config.timeout)
        .map(|d| d.version)
        .unwrap_or_else(|_| "unknown".to_string());

    if !unfixable.is_empty() || !fixable.is_empty() {
        let a2ml_doc = a2ml_writer::generate_a2ml(
            &fixable,
            &unfixable,
            &format!("hyperpolymath/{}", repo_name),
            &pa_version,
        );

        if let Err(e) = a2ml_writer::write_a2ml(dir, &a2ml_doc) {
            tracing::warn!("Failed to write A2ML debt register: {}", e);
        }
    }

    // Step 6: Build FindingSet for fleet pipeline
    let mut finding_set = FindingSet::new();
    finding_set.extend(findings);

    // Count files from weak point locations
    let files_analyzed: std::collections::HashSet<_> = report
        .weak_points
        .iter()
        .filter_map(|wp| wp.location.as_ref())
        .filter_map(|loc| loc.split(':').next())
        .collect();

    let error_count = finding_set.errors().len();
    execution.complete(findings_count, error_count, files_analyzed.len());

    // In guardian mode, mark as failed if there are critical findings
    if mode.blocks_on_error() && finding_set.has_errors() {
        execution.fail(&format!(
            "Panicbot found {} blocking error(s) in guardian mode",
            error_count
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
        assert_eq!(info.id, BotId::Panicbot);
        assert_eq!(info.name, "Panicbot");
        assert_eq!(info.categories.len(), 10);
        assert!(!info.can_fix);
        assert_eq!(info.depends_on, vec![BotId::Rhodibot]);
    }

    #[test]
    fn test_bot_mode_parse() {
        assert_eq!(BotMode::parse("advisor"), Some(BotMode::Advisor));
        assert_eq!(BotMode::parse("auditor"), Some(BotMode::Auditor));
        assert_eq!(BotMode::parse("guardian"), Some(BotMode::Guardian));
        assert_eq!(BotMode::parse("ADVISOR"), Some(BotMode::Advisor));
        assert_eq!(BotMode::parse("invalid"), None);
    }

    #[test]
    fn test_blocks_on_error() {
        assert!(!BotMode::Advisor.blocks_on_error());
        assert!(!BotMode::Auditor.blocks_on_error());
        assert!(BotMode::Guardian.blocks_on_error());
    }
}
