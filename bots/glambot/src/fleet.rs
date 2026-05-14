// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Fleet integration module for gitbot-shared-context interoperability
//!
//! Converts glambot internal findings to the shared-context Finding format
//! used by the gitbot-fleet ecosystem. Requires the `fleet` feature.

use crate::analyzers::{Finding as GlambotFinding, Severity as GlambotSeverity};
use gitbot_shared_context::bot::BotId;
use gitbot_shared_context::finding::{
    Finding as FleetFinding, FindingSet, Severity as FleetSeverity,
};

/// Map glambot severity to fleet severity
fn map_severity(severity: GlambotSeverity) -> FleetSeverity {
    match severity {
        GlambotSeverity::Error => FleetSeverity::Error,
        GlambotSeverity::Warning => FleetSeverity::Warning,
        GlambotSeverity::Info => FleetSeverity::Info,
        GlambotSeverity::Suggestion => FleetSeverity::Suggestion,
    }
}

/// Determine the presentation category for a finding based on its ID prefix
fn category_for_id(id: &str) -> &'static str {
    if id.starts_with("VIS-") {
        "presentation/visual"
    } else if id.starts_with("ACC-") {
        "presentation/accessibility"
    } else if id.starts_with("SEO-") {
        "presentation/seo"
    } else if id.starts_with("MACH-") {
        "presentation/machine-readability"
    } else if id.starts_with("GS-") {
        "presentation/git-seo"
    } else {
        "presentation"
    }
}

/// Convert a single glambot finding to a fleet finding
pub fn to_fleet_finding(finding: &GlambotFinding) -> FleetFinding {
    let mut fleet_finding = FleetFinding::new(
        BotId::Glambot,
        &finding.id,
        map_severity(finding.severity),
        &finding.message,
    )
    .with_rule_name(&finding.name)
    .with_category(category_for_id(&finding.id));

    if let Some(ref file) = finding.file {
        fleet_finding = fleet_finding.with_file(file.clone());
    }

    if let Some(line) = finding.line {
        if let Some(col) = finding.column {
            fleet_finding = fleet_finding.with_location(line, col);
        } else {
            fleet_finding = fleet_finding.with_line(line);
        }
    }

    if let Some(ref suggestion) = finding.suggestion {
        fleet_finding = fleet_finding.with_suggestion(suggestion);
    }

    if finding.fixable {
        fleet_finding = fleet_finding.fixable();
    }

    fleet_finding
}

/// Convert a collection of glambot findings to a fleet FindingSet
pub fn to_fleet_finding_set(findings: &[GlambotFinding]) -> FindingSet {
    let mut set = FindingSet::new();
    for finding in findings {
        set.add(to_fleet_finding(finding));
    }
    set
}

/// Serialize findings to shared-context JSON format for publishing
pub fn serialize_to_shared_context(findings: &[GlambotFinding]) -> serde_json::Result<String> {
    let set = to_fleet_finding_set(findings);
    serde_json::to_string_pretty(&set)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analyzers::{Finding as GlambotFinding, Severity as GlambotSeverity};
    use std::path::PathBuf;

    #[test]
    fn test_severity_mapping() {
        assert!(matches!(
            map_severity(GlambotSeverity::Error),
            FleetSeverity::Error
        ));
        assert!(matches!(
            map_severity(GlambotSeverity::Warning),
            FleetSeverity::Warning
        ));
        assert!(matches!(
            map_severity(GlambotSeverity::Info),
            FleetSeverity::Info
        ));
        assert!(matches!(
            map_severity(GlambotSeverity::Suggestion),
            FleetSeverity::Suggestion
        ));
    }

    #[test]
    fn test_category_mapping() {
        assert_eq!(category_for_id("VIS-001"), "presentation/visual");
        assert_eq!(category_for_id("ACC-001"), "presentation/accessibility");
        assert_eq!(category_for_id("SEO-001"), "presentation/seo");
        assert_eq!(
            category_for_id("MACH-001"),
            "presentation/machine-readability"
        );
        assert_eq!(category_for_id("GS-001"), "presentation/git-seo");
        assert_eq!(category_for_id("UNKNOWN"), "presentation");
    }

    #[test]
    fn test_to_fleet_finding_full() {
        let glambot_finding = GlambotFinding::new(
            "VIS-001",
            "Missing README",
            GlambotSeverity::Error,
            "No README file found in repository root.",
        )
        .with_file(PathBuf::from("README.md"))
        .with_line(1)
        .with_suggestion("Add a README.md file")
        .fixable();

        let fleet_finding = to_fleet_finding(&glambot_finding);

        assert_eq!(fleet_finding.source, BotId::Glambot);
        assert_eq!(fleet_finding.rule_id, "VIS-001");
        assert_eq!(fleet_finding.rule_name, "Missing README");
        assert!(matches!(fleet_finding.severity, FleetSeverity::Error));
        assert_eq!(fleet_finding.category, "presentation/visual");
        assert_eq!(
            fleet_finding.file.as_ref().unwrap().display().to_string(),
            "README.md"
        );
        assert_eq!(fleet_finding.line, Some(1));
        assert_eq!(
            fleet_finding.suggestion.as_deref(),
            Some("Add a README.md file")
        );
        assert!(fleet_finding.fixable);
    }

    #[test]
    fn test_round_trip_serialization() {
        let findings = vec![
            GlambotFinding::new(
                "ACC-001",
                "Missing alt text",
                GlambotSeverity::Error,
                "Image has no alt text",
            )
            .with_file(PathBuf::from("docs.md"))
            .with_line(5),
            GlambotFinding::new(
                "SEO-001",
                "Missing description",
                GlambotSeverity::Warning,
                "README lacks description",
            ),
        ];

        // Serialize to JSON
        let json = serialize_to_shared_context(&findings).unwrap();

        // Deserialize back to FindingSet
        let deserialized: FindingSet = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.len(), 2);
        assert!(deserialized.has_errors());

        // Verify the findings match
        let errors = deserialized.errors();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].rule_id, "ACC-001");
        assert_eq!(errors[0].source, BotId::Glambot);
        assert_eq!(errors[0].category, "presentation/accessibility");

        let warnings = deserialized.warnings();
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].rule_id, "SEO-001");
    }

    #[test]
    fn test_finding_set_aggregation() {
        let findings = vec![
            GlambotFinding::new("VIS-001", "A", GlambotSeverity::Error, "err"),
            GlambotFinding::new("ACC-001", "B", GlambotSeverity::Warning, "warn"),
            GlambotFinding::new("SEO-001", "C", GlambotSeverity::Info, "info"),
            GlambotFinding::new("MACH-001", "D", GlambotSeverity::Suggestion, "sug"),
        ];

        let set = to_fleet_finding_set(&findings);

        assert_eq!(set.len(), 4);
        assert!(set.blocks_release());
        assert_eq!(set.errors().len(), 1);
        assert_eq!(set.warnings().len(), 1);
        assert_eq!(set.by_source(BotId::Glambot).len(), 4);
        assert_eq!(set.by_category("presentation/visual").len(), 1);
        assert_eq!(set.by_category("presentation/accessibility").len(), 1);
    }
}
