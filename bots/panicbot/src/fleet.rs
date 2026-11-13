// SPDX-License-Identifier: MPL-2.0
//! Fleet Integration — panicbot's integration with the gitbot-fleet ecosystem.
//!
//! Registers as `BotId::Panicbot` in gitbot-shared-context. Provides the
//! standard fleet entry point (`run_fleet_scan`) that:
//! 1. Reads per-repo directives (`.machine_readable/bot_directives/panicbot.scm`)
//! 2. Runs the allowed panic-attack suite (full suite by default, see `suite`)
//! 3. Translates each part's results into Finding structs (assail WeakPoints,
//!    assay candidates, dynamic-mode results when a target is given)
//! 4. Classifies fixable vs unfixable
//! 5. Writes unfixable to `.panicbot/PANICBOT-FINDINGS.a2ml`
//! 6. Returns FindingSet for the fleet pipeline

use crate::a2ml_writer;
use crate::directives;
use crate::suite::{self, SuiteOptions, SuiteRun};
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
    /// Report findings, don't block. Runs the allowed suite.
    Advisor,
    /// Report findings, don't block. Same scope as Advisor since the full-suite
    /// ruling (2026-11-13); the name is kept for callers.
    Auditor,
    /// Block PRs on critical findings. Runs the allowed suite + strict thresholds.
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
/// 2. Run the allowed panic-attack suite (see [`crate::suite`])
/// 3. Classify fixable/unfixable
/// 4. Write A2ML debt register
/// 5. Return FindingSet + BotExecution
pub fn run_fleet_scan(dir: &Path, mode: BotMode) -> (FindingSet, BotExecution) {
    let (set, execution, _) = run_fleet_suite(dir, mode, &SuiteOptions::from_env(None));
    (set, execution)
}

/// [`run_fleet_scan`] that also returns the per-part results and takes the
/// suite options (target for the dynamic modes).
pub fn run_fleet_suite(
    dir: &Path,
    mode: BotMode,
    opts: &SuiteOptions,
) -> (FindingSet, BotExecution, Option<SuiteRun>) {
    let mut execution = BotExecution::new(BotId::Panicbot);
    execution.start();

    // Step 1: Load per-repo directives. A directive file that exists but does
    // not parse is a failure, never the (full-suite) default: failing open
    // would widen the scope of a repository whose directive is broken.
    let config = match directives::load_directives(dir) {
        Ok(c) => c,
        Err(e) => {
            let msg = format!("bot directives unreadable, nothing run: {e:#}");
            tracing::error!("{}", msg);
            execution.fail(&msg);
            return (FindingSet::new(), execution, None);
        }
    };

    // Step 2: Run the allowed suite.
    let run = suite::run_suite(dir, &config, opts);
    for line in run.table() {
        tracing::info!("{}", line);
    }
    if !run.analysed() && run.errored().is_empty() {
        execution.skip("no panic-attack part ran: all denied by bot directives or not applicable");
        return (FindingSet::new(), execution, Some(run));
    }

    // Step 3: Classify fixable vs unfixable
    let findings = run.findings.clone();
    let findings_count = findings.len();
    let (fixable, unfixable) = translator::classify_fixability(&findings);

    // Step 4: Write A2ML debt register (if there are findings)
    let repo_name = dir
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    // The version for the A2ML metadata comes from the suite's own diagnostics
    // part: no second, separately-timed diagnostics run.
    let pa_version = run.diagnostics_version().unwrap_or("unknown").to_string();

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

    // Step 5: Build FindingSet for fleet pipeline
    let mut finding_set = FindingSet::new();
    finding_set.extend(findings);

    // Count files from weak point locations
    let files_analyzed: std::collections::HashSet<_> = run
        .assail
        .iter()
        .flat_map(|r| r.weak_points.iter())
        .filter_map(|wp| wp.location.as_ref())
        .filter_map(|loc| loc.split(':').next())
        .collect();

    let error_count = finding_set.errors().len();
    execution.complete(findings_count, error_count, files_analyzed.len());

    // A failed part is a failed run, whatever the other parts found.
    if let Some(summary) = run.error_summary() {
        execution.fail(&summary);
    }

    // In guardian mode, mark as failed if there are critical findings
    if mode.blocks_on_error() && finding_set.has_errors() {
        execution.fail(&format!(
            "Panicbot found {} blocking error(s) in guardian mode",
            error_count
        ));
    }

    (finding_set, execution, Some(run))
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
