// SPDX-License-Identifier: PMPL-1.0-or-later

//! Fleet integration module for gitbot-fleet shared context.
//!
//! Converts RSR compliance results into `Finding` structs compatible with
//! the gitbot-fleet shared context layer. This enables other bots (glambot,
//! seambot, finishbot, robot-repo-automaton) to consume rhodibot's findings.

use std::path::{Path, PathBuf};

use gitbot_shared_context::{BotId, Context, Finding, Severity as FleetSeverity};
use tracing::info;

use crate::rsr::{CheckCategory, CheckStatus, ComplianceReport, Severity};

/// RSR finding rule ID prefix
const RULE_PREFIX: &str = "RSR";

/// Map RSR check category to fleet finding category string
fn category_string(category: CheckCategory) -> &'static str {
    match category {
        CheckCategory::Documentation => "rsr/documentation",
        CheckCategory::Security => "rsr/security",
        CheckCategory::Governance => "rsr/governance",
        CheckCategory::Structure => "rsr/structure",
        CheckCategory::LanguagePolicy => "rsr/language-policy",
    }
}

/// Map RSR severity to fleet severity
fn map_severity(severity: Severity, status: CheckStatus) -> FleetSeverity {
    match (severity, status) {
        (Severity::Required, CheckStatus::Fail) => FleetSeverity::Error,
        (Severity::Recommended, CheckStatus::Warn) => FleetSeverity::Warning,
        (Severity::Recommended, CheckStatus::Fail) => FleetSeverity::Warning,
        (_, CheckStatus::Warn) => FleetSeverity::Info,
        (_, CheckStatus::Pass) => FleetSeverity::Info,
        (_, CheckStatus::Skip) => FleetSeverity::Suggestion,
        _ => FleetSeverity::Info,
    }
}

/// Generate a rule ID from a check name
///
/// Converts check names like "README.adoc" to rule IDs like "RSR-001"
/// and "no-go.mod" to "RSR-BAN-001".
fn rule_id(check_name: &str) -> String {
    // Map known checks to stable rule IDs
    match check_name {
        "README.adoc" => format!("{}-001", RULE_PREFIX),
        "LICENSE.txt" => format!("{}-002", RULE_PREFIX),
        "SECURITY.md" => format!("{}-003", RULE_PREFIX),
        "CONTRIBUTING.md" => format!("{}-004", RULE_PREFIX),
        "CODE_OF_CONDUCT.md" => format!("{}-005", RULE_PREFIX),
        ".claude/CLAUDE.md" => format!("{}-006", RULE_PREFIX),
        "STATE.scm" => format!("{}-007", RULE_PREFIX),
        "META.scm" => format!("{}-008", RULE_PREFIX),
        "ECOSYSTEM.scm" => format!("{}-009", RULE_PREFIX),
        ".github/workflows" => format!("{}-010", RULE_PREFIX),
        ".editorconfig" => format!("{}-011", RULE_PREFIX),
        ".gitattributes" => format!("{}-012", RULE_PREFIX),
        ".gitignore" => format!("{}-013", RULE_PREFIX),
        "justfile" => format!("{}-014", RULE_PREFIX),
        ".bot_directives" => format!("{}-015", RULE_PREFIX),
        "license-type" => format!("{}-LIC-001", RULE_PREFIX),
        name if name.starts_with("no-") => {
            format!("{}-BAN-{}", RULE_PREFIX, name.strip_prefix("no-").unwrap_or(name))
        }
        _ => format!("{}-CUSTOM", RULE_PREFIX),
    }
}

/// Suggestion text for a failed check
fn suggestion_for(check_name: &str) -> Option<String> {
    match check_name {
        "README.adoc" => Some("Create a README.adoc file with project documentation".to_string()),
        "LICENSE.txt" => Some("Add a LICENSE.txt file with PMPL-1.0-or-later".to_string()),
        "SECURITY.md" => Some("Add a SECURITY.md with vulnerability reporting instructions".to_string()),
        "CONTRIBUTING.md" => Some("Add a CONTRIBUTING.md with contribution guidelines".to_string()),
        "CODE_OF_CONDUCT.md" => Some("Add a CODE_OF_CONDUCT.md (Contributor Covenant recommended)".to_string()),
        ".claude/CLAUDE.md" => Some("Create .claude/CLAUDE.md with AI assistant instructions".to_string()),
        "STATE.scm" => Some("Add .machine_readable/STATE.scm with project state".to_string()),
        "META.scm" => Some("Add .machine_readable/META.scm with meta information".to_string()),
        "ECOSYSTEM.scm" => Some("Add .machine_readable/ECOSYSTEM.scm with ecosystem position".to_string()),
        ".github/workflows" => Some("Add GitHub Actions workflows in .github/workflows/".to_string()),
        ".editorconfig" => Some("Add .editorconfig for consistent formatting".to_string()),
        ".gitattributes" => Some("Add .gitattributes for line ending and diff config".to_string()),
        ".gitignore" => Some("Add .gitignore for build artifacts".to_string()),
        "justfile" => Some("Add a justfile as the primary build system".to_string()),
        ".bot_directives" => Some("Create .bot_directives/ directory for bot configs".to_string()),
        "license-type" => Some("Set repository license to an approved type (PMPL-1.0-or-later recommended)".to_string()),
        name if name.starts_with("no-") => {
            let banned_file = name.strip_prefix("no-").unwrap_or(name);
            Some(format!("Remove {} - this file violates language policy", banned_file))
        }
        _ => None,
    }
}

/// Whether a failed check can be auto-fixed by robot-repo-automaton
fn is_fixable(check_name: &str) -> bool {
    matches!(check_name,
        "SECURITY.md" | "CODE_OF_CONDUCT.md" | "CONTRIBUTING.md" |
        ".editorconfig" | ".gitattributes" | ".gitignore" |
        ".claude/CLAUDE.md" | ".bot_directives"
    )
}

/// Convert an RSR compliance report into fleet findings.
///
/// Only failed/warned checks are converted to findings. Passing checks
/// are not emitted since the fleet context focuses on issues to address.
pub fn report_to_findings(report: &ComplianceReport) -> Vec<Finding> {
    let mut findings = Vec::new();

    for check in &report.checks {
        // Only emit findings for non-passing checks
        match check.status {
            CheckStatus::Pass | CheckStatus::Skip => continue,
            CheckStatus::Fail | CheckStatus::Warn => {}
        }

        let fleet_severity = map_severity(check.severity, check.status);
        let rid = rule_id(&check.name);
        let category = category_string(check.category);

        let mut finding = Finding::new(
            BotId::Rhodibot,
            &rid,
            fleet_severity,
            &check.message,
        )
        .with_rule_name(&check.name)
        .with_category(category);

        // Set file path for file-based checks
        if !check.name.starts_with("license-type") && !check.name.starts_with("no-") {
            finding = finding.with_file(PathBuf::from(&check.name));
        } else if check.name.starts_with("no-") {
            let banned_file = check.name.strip_prefix("no-").unwrap_or(&check.name);
            finding = finding.with_file(PathBuf::from(banned_file));
        }

        // Add suggestion if available
        if let Some(suggestion) = suggestion_for(&check.name) {
            finding = finding.with_suggestion(&suggestion);
        }

        // Mark fixable checks
        if is_fixable(&check.name) {
            finding = finding.fixable();
        }

        findings.push(finding);
    }

    findings
}

/// Publish RSR compliance findings to a fleet shared context.
///
/// Creates or updates the context file at the given path with rhodibot's findings.
pub fn publish_to_context(
    report: &ComplianceReport,
    context_path: &Path,
) -> Result<Context, Box<dyn std::error::Error>> {
    let repo_full_name = format!("{}/{}", report.owner, report.repo);

    // Load existing context or create new
    let mut context = if context_path.exists() {
        let data = std::fs::read_to_string(context_path)?;
        serde_json::from_str(&data)?
    } else {
        let mut ctx = Context::new(&repo_full_name, context_path.parent().unwrap_or(Path::new(".")));
        ctx.register_bot(BotId::Rhodibot);
        ctx
    };

    // Start rhodibot execution
    let _ = context.start_bot(BotId::Rhodibot);

    // Convert and add findings
    let findings = report_to_findings(report);
    let findings_count = findings.len();
    let errors_count = findings.iter().filter(|f| f.severity == FleetSeverity::Error).count();

    context.add_findings(findings);

    // Store RSR-specific metadata
    context.set_data("rsr:score", serde_json::json!(report.score));
    context.set_data("rsr:max_score", serde_json::json!(report.max_score));
    context.set_data("rsr:percentage", serde_json::json!(report.percentage));
    context.set_data("rsr:policy", serde_json::json!(format!("{}", report.policy)));
    context.set_data("rsr:required_passed", serde_json::json!(report.required_passed));

    // Complete rhodibot execution
    let _ = context.complete_bot(
        BotId::Rhodibot,
        findings_count,
        errors_count,
        report.checks.len(),
    );

    // Write context to file
    let json = serde_json::to_string_pretty(&context)?;
    if let Some(parent) = context_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(context_path, json)?;

    info!(
        "Published {} findings to fleet context at {}",
        findings_count,
        context_path.display()
    );

    Ok(context)
}
