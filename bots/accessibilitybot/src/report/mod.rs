// SPDX-License-Identifier: PMPL-1.0-or-later
//! Report generation for accessibility findings.
//!
//! Supports multiple output formats:
//! - Text: human-readable findings with WCAG criterion references
//! - JSON: structured findings for programmatic consumption
//! - SARIF: Static Analysis Results Interchange Format for IDE/CI integration

use crate::fleet::{FindingSet, Severity};
use serde::Serialize;

/// Output format for reports
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Human-readable text
    Text,
    /// Structured JSON
    Json,
    /// SARIF for IDE/CI integration
    Sarif,
}

impl std::fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputFormat::Text => write!(f, "text"),
            OutputFormat::Json => write!(f, "json"),
            OutputFormat::Sarif => write!(f, "sarif"),
        }
    }
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "text" => Ok(OutputFormat::Text),
            "json" => Ok(OutputFormat::Json),
            "sarif" => Ok(OutputFormat::Sarif),
            other => Err(format!("Unknown output format: {}", other)),
        }
    }
}

/// Generate a report from findings
pub fn generate_report(findings: &FindingSet, format: OutputFormat) -> String {
    match format {
        OutputFormat::Text => generate_text_report(findings),
        OutputFormat::Json => generate_json_report(findings),
        OutputFormat::Sarif => generate_sarif_report(findings),
    }
}

/// Generate human-readable text report
fn generate_text_report(findings: &FindingSet) -> String {
    let mut output = String::new();

    output.push_str("=== Accessibilitybot WCAG Analysis Report ===\n\n");

    if findings.is_empty() {
        output.push_str("No accessibility issues found. All checks passed.\n");
        return output;
    }

    let errors = findings.errors().len();
    let warnings = findings.warnings().len();
    let total = findings.len();

    output.push_str(&format!(
        "Found {} issue(s): {} error(s), {} warning(s), {} info/suggestion(s)\n\n",
        total, errors, warnings, total - errors - warnings
    ));

    // Group by severity
    for severity in &[Severity::Error, Severity::Warning, Severity::Info, Severity::Suggestion] {
        let sev_findings = findings.by_severity(*severity);
        if sev_findings.is_empty() {
            continue;
        }

        output.push_str(&format!("--- {} ({}) ---\n", severity, sev_findings.len()));

        for finding in sev_findings {
            output.push_str(&format!(
                "[{}] {}\n",
                finding.rule_id,
                finding.message
            ));

            if let Some(ref file) = finding.file {
                if let Some(line) = finding.line {
                    output.push_str(&format!("  Location: {}:{}\n", file.display(), line));
                } else {
                    output.push_str(&format!("  Location: {}\n", file.display()));
                }
            }

            if let Some(ref criterion) = finding.wcag_criterion {
                if let Some(ref level) = finding.wcag_level {
                    output.push_str(&format!("  WCAG: {} (Level {})\n", criterion, level));
                }
            }

            if let Some(ref suggestion) = finding.suggestion {
                output.push_str(&format!("  Fix: {}\n", suggestion));
            }

            if let Some(ref impact) = finding.impact {
                let groups = impact.affected_groups();
                if !groups.is_empty() {
                    output.push_str(&format!("  Affects: {}\n", groups.join(", ")));
                }
            }

            output.push('\n');
        }
    }

    if findings.blocks_release() {
        output.push_str("RESULT: RELEASE BLOCKED (errors found)\n");
    } else if warnings > 0 {
        output.push_str("RESULT: PASS WITH WARNINGS\n");
    } else {
        output.push_str("RESULT: PASS\n");
    }

    output
}

/// Generate JSON report
fn generate_json_report(findings: &FindingSet) -> String {
    serde_json::to_string_pretty(findings).unwrap_or_else(|e| {
        format!("{{\"error\": \"Failed to serialize findings: {}\"}}", e)
    })
}

/// SARIF report structure (simplified)
#[derive(Debug, Serialize)]
struct SarifReport {
    #[serde(rename = "$schema")]
    schema: String,
    version: String,
    runs: Vec<SarifRun>,
}

#[derive(Debug, Serialize)]
struct SarifRun {
    tool: SarifTool,
    results: Vec<SarifResult>,
}

#[derive(Debug, Serialize)]
struct SarifTool {
    driver: SarifDriver,
}

#[derive(Debug, Serialize)]
struct SarifDriver {
    name: String,
    version: String,
    #[serde(rename = "informationUri")]
    information_uri: String,
}

#[derive(Debug, Serialize)]
struct SarifResult {
    #[serde(rename = "ruleId")]
    rule_id: String,
    level: String,
    message: SarifMessage,
    locations: Vec<SarifLocation>,
}

#[derive(Debug, Serialize)]
struct SarifMessage {
    text: String,
}

#[derive(Debug, Serialize)]
struct SarifLocation {
    #[serde(rename = "physicalLocation")]
    physical_location: SarifPhysicalLocation,
}

#[derive(Debug, Serialize)]
struct SarifPhysicalLocation {
    #[serde(rename = "artifactLocation")]
    artifact_location: SarifArtifactLocation,
    region: Option<SarifRegion>,
}

#[derive(Debug, Serialize)]
struct SarifArtifactLocation {
    uri: String,
}

#[derive(Debug, Serialize)]
struct SarifRegion {
    #[serde(rename = "startLine")]
    start_line: usize,
}

/// Generate SARIF report
fn generate_sarif_report(findings: &FindingSet) -> String {
    let results: Vec<SarifResult> = findings.findings.iter().map(|f| {
        let level = match f.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "note",
            Severity::Suggestion => "note",
        };

        let mut locations = Vec::new();
        if let Some(ref file) = f.file {
            locations.push(SarifLocation {
                physical_location: SarifPhysicalLocation {
                    artifact_location: SarifArtifactLocation {
                        uri: file.display().to_string(),
                    },
                    region: f.line.map(|l| SarifRegion { start_line: l }),
                },
            });
        }

        SarifResult {
            rule_id: f.rule_id.clone(),
            level: level.to_string(),
            message: SarifMessage { text: f.message.clone() },
            locations,
        }
    }).collect();

    let report = SarifReport {
        schema: "https://json.schemastore.org/sarif-2.1.0.json".to_string(),
        version: "2.1.0".to_string(),
        runs: vec![SarifRun {
            tool: SarifTool {
                driver: SarifDriver {
                    name: "accessibilitybot".to_string(),
                    version: env!("CARGO_PKG_VERSION").to_string(),
                    information_uri: "https://github.com/hyperpolymath/accessibilitybot".to_string(),
                },
            },
            results,
        }],
    };

    serde_json::to_string_pretty(&report).unwrap_or_else(|e| {
        format!("{{\"error\": \"Failed to serialize SARIF report: {}\"}}", e)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fleet::{Finding, WcagLevel};
    use std::path::PathBuf;

    fn sample_finding() -> Finding {
        Finding::new("WCAG-1.1.1-missing-alt", Severity::Error, "Missing alt text")
            .with_wcag("1.1.1", WcagLevel::A)
            .with_file(PathBuf::from("index.html"))
            .with_line(10)
    }

    #[test]
    fn test_text_report_empty() {
        let findings = FindingSet::new();
        let report = generate_report(&findings, OutputFormat::Text);
        assert!(report.contains("No accessibility issues found"));
    }

    #[test]
    fn test_text_report_with_findings() {
        let mut findings = FindingSet::new();
        findings.add(sample_finding());
        let report = generate_report(&findings, OutputFormat::Text);
        assert!(report.contains("WCAG-1.1.1-missing-alt"));
        assert!(report.contains("RELEASE BLOCKED"));
    }

    #[test]
    fn test_json_report() {
        let mut findings = FindingSet::new();
        findings.add(sample_finding());
        let report = generate_report(&findings, OutputFormat::Json);
        let parsed: serde_json::Value = serde_json::from_str(&report).expect("valid JSON");
        assert!(parsed["findings"].is_array());
    }

    #[test]
    fn test_sarif_report() {
        let mut findings = FindingSet::new();
        findings.add(sample_finding());
        let report = generate_report(&findings, OutputFormat::Sarif);
        let parsed: serde_json::Value = serde_json::from_str(&report).expect("valid JSON");
        assert_eq!(parsed["version"], "2.1.0");
        assert!(parsed["runs"][0]["results"].is_array());
    }

    #[test]
    fn test_output_format_parse() {
        assert_eq!("text".parse::<OutputFormat>().unwrap(), OutputFormat::Text);
        assert_eq!("json".parse::<OutputFormat>().unwrap(), OutputFormat::Json);
        assert_eq!("sarif".parse::<OutputFormat>().unwrap(), OutputFormat::Sarif);
        assert!("xml".parse::<OutputFormat>().is_err());
    }
}
