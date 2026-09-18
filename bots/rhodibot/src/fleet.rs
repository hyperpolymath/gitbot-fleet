// SPDX-License-Identifier: MPL-2.0

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
        "LICENSE.txt" | "LICENSE" => format!("{}-002", RULE_PREFIX),
        "SECURITY.md" | "SECURITY.adoc" => format!("{}-003", RULE_PREFIX),
        "CONTRIBUTING.md" | "CONTRIBUTING.adoc" => format!("{}-004", RULE_PREFIX),
        "CODE_OF_CONDUCT.md" | "CODE_OF_CONDUCT.adoc" => format!("{}-005", RULE_PREFIX),
        ".claude/CLAUDE.md" => format!("{}-006", RULE_PREFIX),
        ".machine_readable/STATE.scm" | ".machine_readable/STATE.a2ml" => format!("{}-007", RULE_PREFIX),
        ".machine_readable/META.scm" | ".machine_readable/META.a2ml" => format!("{}-008", RULE_PREFIX),
        ".machine_readable/ECOSYSTEM.scm" | ".machine_readable/ECOSYSTEM.a2ml" => format!("{}-009", RULE_PREFIX),
        ".github/workflows" => format!("{}-010", RULE_PREFIX),
        ".editorconfig" => format!("{}-011", RULE_PREFIX),
        ".gitattributes" => format!("{}-012", RULE_PREFIX),
        ".gitignore" => format!("{}-013", RULE_PREFIX),
        "justfile" | "Justfile" => format!("{}-014", RULE_PREFIX),
        ".machine_readable/bot_directives" => format!("{}-015", RULE_PREFIX),
        "0-AI-MANIFEST.a2ml" => format!("{}-016", RULE_PREFIX),
        "www/.well-known/security.txt" => format!("{}-017", RULE_PREFIX),
        ".machine_readable/root-allow.txt" => format!("{}-018", RULE_PREFIX),
        "no-.bot_directives" => format!("{}-LEGACY-001", RULE_PREFIX),
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
        "LICENSE.txt" | "LICENSE" => Some("Add a LICENSE file with MPL-2.0 (LICENSE is the current spelling)".to_string()),
        "SECURITY.adoc" | "SECURITY.md" => Some("Add a SECURITY.adoc with vulnerability reporting instructions".to_string()),
        "CONTRIBUTING.adoc" | "CONTRIBUTING.md" => Some("Add a CONTRIBUTING.adoc with contribution guidelines".to_string()),
        "CODE_OF_CONDUCT.adoc" | "CODE_OF_CONDUCT.md" => Some("Add a CODE_OF_CONDUCT.adoc (Contributor Covenant recommended)".to_string()),
        ".claude/CLAUDE.md" => Some("Create .claude/CLAUDE.md with AI assistant instructions".to_string()),
        ".machine_readable/STATE.a2ml" | ".machine_readable/STATE.scm" => Some("Add .machine_readable/STATE.a2ml with project state".to_string()),
        ".machine_readable/META.a2ml" | ".machine_readable/META.scm" => Some("Add .machine_readable/META.a2ml with meta information".to_string()),
        ".machine_readable/ECOSYSTEM.a2ml" | ".machine_readable/ECOSYSTEM.scm" => Some("Add .machine_readable/ECOSYSTEM.a2ml with ecosystem position".to_string()),
        ".github/workflows" => Some("Add GitHub Actions workflows in .github/workflows/".to_string()),
        ".editorconfig" => Some("Add .editorconfig for consistent formatting".to_string()),
        ".gitattributes" => Some("Add .gitattributes for line ending and diff config".to_string()),
        ".gitignore" => Some("Add .gitignore for build artifacts".to_string()),
        "Justfile" | "justfile" => Some("Add a Justfile as the primary build system".to_string()),
        ".machine_readable/bot_directives" => Some("Create .machine_readable/bot_directives/ for bot configs".to_string()),
        "no-.bot_directives" => Some("Migrate legacy .bot_directives/ to .machine_readable/bot_directives/".to_string()),
        "0-AI-MANIFEST.a2ml" => Some("Add a 0-AI-MANIFEST.a2ml machine-readable manifest at the repository root".to_string()),
        "www/.well-known/security.txt" => Some("Create www/.well-known/security.txt; run scripts/migrate-wellknown-to-www.sh to move an existing root .well-known/".to_string()),
        ".machine_readable/root-allow.txt" => Some("Add .machine_readable/root-allow.txt declaring the permitted root entries".to_string()),
        // Move, do not delete: the generic no- arm below would advise
        // removing the file, which loses live security-contact metadata.
        "no-.well-known/security.txt" => Some("Migrate the root .well-known/ to www/.well-known/ with scripts/migrate-wellknown-to-www.sh - move it, do not delete it".to_string()),
        "license-type" => Some("Set repository license to an approved type (MPL-2.0 recommended)".to_string()),
        name if name.starts_with("no-") => {
            let banned_file = name.strip_prefix("no-").unwrap_or(name);
            Some(format!("Remove {} - this file violates language policy", banned_file))
        }
        _ => None,
    }
}

/// Whether a failed check can be auto-fixed by robot-repo-automaton
/// Whether a failed check can be auto-fixed by robot-repo-automaton.
///
/// Only files with proper templates in robot-repo-automaton/templates/ are
/// fixable. Files without templates (CONTRIBUTING.md, CODE_OF_CONDUCT.md)
/// would produce empty boilerplate and are excluded.
fn is_fixable(check_name: &str) -> bool {
    // Must match the arms of robot-repo-automaton's get_template_content()
    // exactly. That function falls through to String::new() for anything it
    // does not recognise, so advertising a check as fixable without a
    // template behind it makes the automaton open a PR that creates the file
    // EMPTY - and inbox-steward auto-merges PRs that pass CI.
    //
    // This list was previously four entries wider than the templates
    // (.gitattributes, .gitignore, .claude/CLAUDE.md and
    // .machine_readable/bot_directives), which is how an empty-file PR could
    // reach a repository and be merged without anyone reading it.
    matches!(check_name,
        "LICENSE" | "LICENSE.txt" |
        "SECURITY.adoc" | "SECURITY.md" |
        ".editorconfig"
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
