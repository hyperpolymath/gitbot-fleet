// SPDX-License-Identifier: PMPL-1.0-or-later
//! A2ML Writer — generates `.panicbot/PANICBOT-FINDINGS.a2ml` debt registers.
//!
//! The A2ML debt register is the canonical record of unfixable findings in a
//! repository. It is **overwritten** on each scan (not appended) because it
//! represents the *current* debt state. Historical tracking is handled by the
//! shared-context findings pipeline.
//!
//! ## Output Format
//!
//! ```json
//! {
//!   "a2ml": { "schema": "panicbot.findings", "version": 1, ... },
//!   "payload": {
//!     "summary": { "total": N, "fixable": M, "unfixable": K, ... },
//!     "unfixable_findings": [ ... ],
//!     "fixable_dispatched": [ ... ]
//!   }
//! }
//! ```

use anyhow::{Context, Result};
use chrono::Utc;
use gitbot_shared_context::finding::Finding;
use serde::Serialize;
use std::collections::HashMap;
use std::path::Path;

/// Summary statistics for the A2ML debt register.
#[derive(Debug, Clone, Serialize)]
pub struct FindingsSummary {
    /// Total number of findings from this scan.
    pub total: usize,
    /// Number of fixable findings (dispatched to fleet pipeline).
    pub fixable: usize,
    /// Number of unfixable findings (recorded in debt register).
    pub unfixable: usize,
    /// Breakdown by severity level.
    pub by_severity: HashMap<String, usize>,
    /// Breakdown by triangle tier.
    pub by_tier: HashMap<String, usize>,
}

/// A single finding entry in the A2ML output.
///
/// Simplified representation of a Finding for the A2ML format.
/// Contains only the fields needed for debt tracking.
#[derive(Debug, Clone, Serialize)]
pub struct A2mlFinding {
    /// Canonical rule ID (e.g., "PA001").
    pub rule_id: String,
    /// Human-readable rule name.
    pub rule_name: String,
    /// Fleet category (e.g., "static-analysis/unsafe-code").
    pub category: String,
    /// Severity level (error, warning, info, suggestion).
    pub severity: String,
    /// Description of the issue.
    pub message: String,
    /// File location (if known).
    pub file: Option<String>,
    /// Line number (if known).
    pub line: Option<usize>,
    /// Triangle tier (eliminate, substitute, control).
    pub triangle_tier: Option<String>,
    /// Confidence score (0.0 - 1.0).
    pub confidence: Option<f64>,
}

/// The complete A2ML debt register document.
#[derive(Debug, Clone, Serialize)]
pub struct A2mlDocument {
    /// A2ML metadata envelope.
    pub a2ml: A2mlMetadata,
    /// Payload containing findings and summary.
    pub payload: A2mlPayload,
}

/// A2ML metadata section.
#[derive(Debug, Clone, Serialize)]
pub struct A2mlMetadata {
    /// Schema identifier.
    pub schema: String,
    /// Schema version.
    pub version: u32,
    /// Timestamp of generation.
    pub generated_at: String,
    /// Generator tool and version.
    pub generator: String,
    /// Repository identifier (owner/name).
    pub repo: String,
    /// panic-attack version used for scanning.
    pub panic_attack_version: String,
}

/// A2ML payload section.
#[derive(Debug, Clone, Serialize)]
pub struct A2mlPayload {
    /// Aggregate summary statistics.
    pub summary: FindingsSummary,
    /// Unfixable findings (the debt register proper).
    pub unfixable_findings: Vec<A2mlFinding>,
    /// Fixable findings that were dispatched to the fleet pipeline.
    pub fixable_dispatched: Vec<A2mlFinding>,
}

/// Convert a fleet Finding into an A2ML-compatible finding entry.
fn finding_to_a2ml(finding: &Finding) -> A2mlFinding {
    A2mlFinding {
        rule_id: finding.rule_id.clone(),
        rule_name: finding.rule_name.clone(),
        category: finding.category.clone(),
        severity: format!("{:?}", finding.severity).to_lowercase(),
        message: finding.message.clone(),
        file: finding.file.as_ref().map(|f| f.display().to_string()),
        line: finding.line,
        triangle_tier: finding.triangle_tier.map(|t| t.to_string()),
        confidence: finding.confidence,
    }
}

/// Build the summary statistics from fixable and unfixable findings.
fn build_summary(
    fixable: &[&Finding],
    unfixable: &[&Finding],
) -> FindingsSummary {
    let total = fixable.len() + unfixable.len();
    let all_findings: Vec<&&Finding> = fixable.iter().chain(unfixable.iter()).collect();

    let mut by_severity: HashMap<String, usize> = HashMap::new();
    let mut by_tier: HashMap<String, usize> = HashMap::new();

    for finding in &all_findings {
        let severity_key = format!("{:?}", finding.severity).to_lowercase();
        *by_severity.entry(severity_key).or_insert(0) += 1;

        if let Some(tier) = finding.triangle_tier {
            *by_tier.entry(tier.to_string()).or_insert(0) += 1;
        }
    }

    FindingsSummary {
        total,
        fixable: fixable.len(),
        unfixable: unfixable.len(),
        by_severity,
        by_tier,
    }
}

/// Generate an A2ML debt register document from classified findings.
///
/// `repo_name` should be in "owner/repo" format (e.g., "hyperpolymath/some-repo").
/// `panic_attack_version` is the version string from diagnostics (or "unknown").
pub fn generate_a2ml(
    fixable: &[&Finding],
    unfixable: &[&Finding],
    repo_name: &str,
    panic_attack_version: &str,
) -> A2mlDocument {
    let summary = build_summary(fixable, unfixable);

    A2mlDocument {
        a2ml: A2mlMetadata {
            schema: "panicbot.findings".to_string(),
            version: 1,
            generated_at: Utc::now().to_rfc3339(),
            generator: format!("panicbot/{}", env!("CARGO_PKG_VERSION")),
            repo: repo_name.to_string(),
            panic_attack_version: panic_attack_version.to_string(),
        },
        payload: A2mlPayload {
            summary,
            unfixable_findings: unfixable.iter().map(|f| finding_to_a2ml(f)).collect(),
            fixable_dispatched: fixable.iter().map(|f| finding_to_a2ml(f)).collect(),
        },
    }
}

/// Write the A2ML debt register to disk.
///
/// Creates the `.panicbot/` directory in the target repo if it doesn't exist,
/// then writes `PANICBOT-FINDINGS.a2ml` (overwriting any previous version).
pub fn write_a2ml(repo_path: &Path, document: &A2mlDocument) -> Result<std::path::PathBuf> {
    let panicbot_dir = repo_path.join(".panicbot");
    std::fs::create_dir_all(&panicbot_dir)
        .with_context(|| format!("Failed to create {}", panicbot_dir.display()))?;

    let output_path = panicbot_dir.join("PANICBOT-FINDINGS.a2ml");
    let json = serde_json::to_string_pretty(document)
        .context("Failed to serialize A2ML document")?;

    std::fs::write(&output_path, &json)
        .with_context(|| format!("Failed to write {}", output_path.display()))?;

    tracing::info!("A2ML debt register written to {}", output_path.display());
    Ok(output_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use gitbot_shared_context::bot::BotId;
    use gitbot_shared_context::finding::Severity;
    use gitbot_shared_context::triangle::TriangleTier;

    fn make_finding(rule_id: &str, severity: Severity, fixable: bool, tier: TriangleTier) -> Finding {
        let mut f = Finding::new(BotId::Panicbot, rule_id, severity, "test finding")
            .with_category("static-analysis/test")
            .with_triangle_tier(tier)
            .with_confidence(0.80);
        if fixable {
            f = f.fixable();
        }
        f
    }

    #[test]
    fn test_finding_to_a2ml() {
        let finding = Finding::new(BotId::Panicbot, "PA001", Severity::Warning, "unsafe block")
            .with_rule_name("Unsafe code block")
            .with_category("static-analysis/unsafe-code")
            .with_file(std::path::PathBuf::from("src/lib.rs"))
            .with_line(42)
            .with_triangle_tier(TriangleTier::Control)
            .with_confidence(0.70);

        let a2ml = finding_to_a2ml(&finding);
        assert_eq!(a2ml.rule_id, "PA001");
        assert_eq!(a2ml.rule_name, "Unsafe code block");
        assert_eq!(a2ml.category, "static-analysis/unsafe-code");
        assert_eq!(a2ml.severity, "warning");
        assert_eq!(a2ml.file, Some("src/lib.rs".to_string()));
        assert_eq!(a2ml.line, Some(42));
        assert_eq!(a2ml.triangle_tier, Some("control".to_string()));
        assert!((a2ml.confidence.unwrap() - 0.70).abs() < f64::EPSILON);
    }

    #[test]
    fn test_build_summary() {
        let f1 = make_finding("PA001", Severity::Error, false, TriangleTier::Control);
        let f2 = make_finding("PA004", Severity::Warning, true, TriangleTier::Eliminate);
        let f3 = make_finding("PA008", Severity::Warning, false, TriangleTier::Control);

        let fixable = vec![&f2];
        let unfixable = vec![&f1, &f3];

        let summary = build_summary(&fixable, &unfixable);
        assert_eq!(summary.total, 3);
        assert_eq!(summary.fixable, 1);
        assert_eq!(summary.unfixable, 2);
        assert_eq!(*summary.by_severity.get("error").unwrap_or(&0), 1);
        assert_eq!(*summary.by_severity.get("warning").unwrap_or(&0), 2);
        assert_eq!(*summary.by_tier.get("control").unwrap_or(&0), 2);
        assert_eq!(*summary.by_tier.get("eliminate").unwrap_or(&0), 1);
    }

    #[test]
    fn test_generate_a2ml_structure() {
        let f1 = make_finding("PA001", Severity::Warning, false, TriangleTier::Control);
        let f2 = make_finding("PA004", Severity::Error, true, TriangleTier::Eliminate);

        let fixable = vec![&f2];
        let unfixable = vec![&f1];

        let doc = generate_a2ml(&fixable, &unfixable, "hyperpolymath/test-repo", "2.1.0");

        assert_eq!(doc.a2ml.schema, "panicbot.findings");
        assert_eq!(doc.a2ml.version, 1);
        assert_eq!(doc.a2ml.repo, "hyperpolymath/test-repo");
        assert_eq!(doc.a2ml.panic_attack_version, "2.1.0");
        assert!(doc.a2ml.generator.starts_with("panicbot/"));

        assert_eq!(doc.payload.summary.total, 2);
        assert_eq!(doc.payload.unfixable_findings.len(), 1);
        assert_eq!(doc.payload.fixable_dispatched.len(), 1);
        assert_eq!(doc.payload.unfixable_findings[0].rule_id, "PA001");
        assert_eq!(doc.payload.fixable_dispatched[0].rule_id, "PA004");
    }

    #[test]
    fn test_generate_a2ml_empty() {
        let doc = generate_a2ml(&[], &[], "hyperpolymath/empty-repo", "unknown");
        assert_eq!(doc.payload.summary.total, 0);
        assert!(doc.payload.unfixable_findings.is_empty());
        assert!(doc.payload.fixable_dispatched.is_empty());
    }

    #[test]
    fn test_a2ml_serializes_to_json() {
        let doc = generate_a2ml(&[], &[], "test/repo", "1.0.0");
        let json = serde_json::to_string_pretty(&doc).unwrap();
        assert!(json.contains("\"schema\": \"panicbot.findings\""));
        assert!(json.contains("\"version\": 1"));
        assert!(json.contains("\"total\": 0"));
    }
}
