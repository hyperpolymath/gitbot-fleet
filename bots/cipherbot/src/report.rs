// SPDX-License-Identifier: PMPL-1.0-or-later
//! SARIF Report Generation — outputs findings in the SARIF 2.1.0 format
//! for integration with GitHub Code Scanning, VS Code, and other tools.

use gitbot_shared_context::finding::{Finding, Severity};
use serde::Serialize;

/// SARIF 2.1.0 report structure (simplified).
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifReport {
    /// SARIF schema URI.
    #[serde(rename = "$schema")]
    pub schema: String,
    /// SARIF version.
    pub version: String,
    /// Analysis runs.
    pub runs: Vec<SarifRun>,
}

/// A single analysis run.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifRun {
    /// Tool information.
    pub tool: SarifTool,
    /// Results (findings).
    pub results: Vec<SarifResult>,
}

/// Tool description.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifTool {
    /// Tool driver.
    pub driver: SarifDriver,
}

/// Tool driver information.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifDriver {
    /// Tool name.
    pub name: String,
    /// Tool version.
    pub version: String,
    /// Semantic version.
    pub semantic_version: String,
    /// Information URI.
    pub information_uri: String,
    /// Rules.
    pub rules: Vec<SarifRule>,
}

/// A SARIF rule definition.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifRule {
    /// Rule ID.
    pub id: String,
    /// Rule short description.
    pub short_description: SarifMessage,
    /// Default severity.
    pub default_configuration: SarifDefaultConfig,
}

/// SARIF default configuration.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifDefaultConfig {
    /// Severity level.
    pub level: String,
}

/// A SARIF result (finding).
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifResult {
    /// Rule ID reference.
    pub rule_id: String,
    /// Severity level.
    pub level: String,
    /// Message.
    pub message: SarifMessage,
    /// Locations.
    pub locations: Vec<SarifLocation>,
    /// Fix suggestions.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub fixes: Vec<SarifFix>,
}

/// SARIF message.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifMessage {
    /// Message text.
    pub text: String,
}

/// SARIF location.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifLocation {
    /// Physical location.
    pub physical_location: SarifPhysicalLocation,
}

/// SARIF physical location.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifPhysicalLocation {
    /// Artifact location.
    pub artifact_location: SarifArtifactLocation,
    /// Region in the file.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub region: Option<SarifRegion>,
}

/// SARIF artifact location.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifArtifactLocation {
    /// File URI.
    pub uri: String,
}

/// SARIF region in a file.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifRegion {
    /// Start line.
    pub start_line: usize,
    /// Start column.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_column: Option<usize>,
}

/// SARIF fix suggestion.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifFix {
    /// Fix description.
    pub description: SarifMessage,
}

/// Convert severity to SARIF level string.
fn severity_to_level(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "note",
        Severity::Suggestion => "note",
    }
}

/// Generate a SARIF 2.1.0 report from a list of findings.
pub fn generate_sarif(findings: &[Finding]) -> SarifReport {
    // Collect unique rules
    let mut rules_map = std::collections::HashMap::new();
    for finding in findings {
        rules_map.entry(finding.rule_id.clone()).or_insert_with(|| SarifRule {
            id: finding.rule_id.clone(),
            short_description: SarifMessage {
                text: finding.rule_name.clone(),
            },
            default_configuration: SarifDefaultConfig {
                level: severity_to_level(finding.severity).to_string(),
            },
        });
    }

    let rules: Vec<SarifRule> = rules_map.into_values().collect();

    let results: Vec<SarifResult> = findings
        .iter()
        .map(|f| {
            let locations = if let Some(ref file) = f.file {
                vec![SarifLocation {
                    physical_location: SarifPhysicalLocation {
                        artifact_location: SarifArtifactLocation {
                            uri: file.to_string_lossy().to_string(),
                        },
                        region: f.line.map(|line| SarifRegion {
                            start_line: line,
                            start_column: f.column,
                        }),
                    },
                }]
            } else {
                vec![]
            };

            let fixes = if let Some(ref suggestion) = f.suggestion {
                vec![SarifFix {
                    description: SarifMessage {
                        text: suggestion.clone(),
                    },
                }]
            } else {
                vec![]
            };

            SarifResult {
                rule_id: f.rule_id.clone(),
                level: severity_to_level(f.severity).to_string(),
                message: SarifMessage {
                    text: f.message.clone(),
                },
                locations,
                fixes,
            }
        })
        .collect();

    SarifReport {
        schema: "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/main/sarif-2.1/schema/sarif-schema-2.1.0.json".to_string(),
        version: "2.1.0".to_string(),
        runs: vec![SarifRun {
            tool: SarifTool {
                driver: SarifDriver {
                    name: "cipherbot".to_string(),
                    version: env!("CARGO_PKG_VERSION").to_string(),
                    semantic_version: env!("CARGO_PKG_VERSION").to_string(),
                    information_uri: "https://github.com/hyperpolymath/cipherbot".to_string(),
                    rules,
                },
            },
            results,
        }],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analyzers::CIPHERBOT_ID;
    use std::path::PathBuf;

    #[test]
    fn test_generate_sarif_empty() {
        let report = generate_sarif(&[]);
        assert_eq!(report.version, "2.1.0");
        assert_eq!(report.runs.len(), 1);
        assert!(report.runs[0].results.is_empty());
    }

    #[test]
    fn test_generate_sarif_with_findings() {
        let findings = vec![
            Finding::new(CIPHERBOT_ID, "CIPHER-MD5", Severity::Error, "MD5 is broken")
                .with_file(PathBuf::from("src/lib.rs"))
                .with_line(42)
                .with_suggestion("Use SHAKE3-512"),
            Finding::new(CIPHERBOT_ID, "CIPHER-SHA256", Severity::Warning, "SHA-256 not PQ-safe")
                .with_file(PathBuf::from("src/crypto.rs"))
                .with_line(10),
        ];
        let report = generate_sarif(&findings);
        assert_eq!(report.runs[0].results.len(), 2);
        assert_eq!(report.runs[0].results[0].level, "error");
        assert_eq!(report.runs[0].results[1].level, "warning");
        assert!(!report.runs[0].results[0].fixes.is_empty());
    }

    #[test]
    fn test_sarif_serialization() {
        let findings = vec![Finding::new(
            CIPHERBOT_ID,
            "CIPHER-TEST",
            Severity::Info,
            "Test finding",
        )];
        let report = generate_sarif(&findings);
        let json = serde_json::to_string_pretty(&report);
        assert!(json.is_ok(), "SARIF report should serialize to JSON");
        let json_str = json.unwrap();
        assert!(json_str.contains("\"version\": \"2.1.0\""));
        assert!(json_str.contains("cipherbot"));
    }
}
