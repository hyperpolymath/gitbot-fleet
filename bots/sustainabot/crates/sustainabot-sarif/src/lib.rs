// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! SARIF 2.1.0 output for sustainabot analysis results.
//!
//! Produces machine-readable SARIF that GitHub/IDEs render as inline annotations.

use serde::{Deserialize, Serialize};
use sustainabot_metrics::AnalysisResult;

/// SARIF schema version
const SARIF_SCHEMA: &str = "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/main/sarif-2.1/schema/sarif-schema-2.1.0.json";
const SARIF_VERSION: &str = "2.1.0";
const TOOL_NAME: &str = "sustainabot";
const TOOL_INFO_URI: &str = "https://github.com/hyperpolymath/sustainabot";

/// Top-level SARIF log
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifLog {
    #[serde(rename = "$schema")]
    pub schema: String,
    pub version: String,
    pub runs: Vec<Run>,
}

/// A single SARIF run
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Run {
    pub tool: Tool,
    pub results: Vec<SarifResult>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub invocations: Option<Vec<Invocation>>,
}

/// Tool description
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Tool {
    pub driver: ToolComponent,
}

/// Tool component with rules
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolComponent {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub semantic_version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub information_uri: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub rules: Vec<ReportingDescriptor>,
}

/// Rule definition
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReportingDescriptor {
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    pub short_description: Message,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub full_description: Option<Message>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub help: Option<MultiformatMessage>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_configuration: Option<ReportingConfiguration>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<serde_json::Value>,
}

/// Reporting configuration for severity
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReportingConfiguration {
    pub level: SarifLevel,
}

/// A single finding result
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifResult {
    pub rule_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rule_index: Option<usize>,
    pub level: SarifLevel,
    pub message: Message,
    pub locations: Vec<Location>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub fixes: Vec<Fix>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<serde_json::Value>,
}

/// SARIF severity levels
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SarifLevel {
    Error,
    Warning,
    Note,
    None,
}

/// Simple message
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Message {
    pub text: String,
}

/// Multiformat message (text + markdown)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MultiformatMessage {
    pub text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub markdown: Option<String>,
}

/// Code location
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Location {
    pub physical_location: PhysicalLocation,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub logical_locations: Option<Vec<LogicalLocation>>,
}

/// Physical file location
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PhysicalLocation {
    pub artifact_location: ArtifactLocation,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub region: Option<Region>,
}

/// File path reference
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ArtifactLocation {
    pub uri: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uri_base_id: Option<String>,
}

/// Line/column region
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Region {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_line: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_column: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_line: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_column: Option<usize>,
}

/// Logical location (function name, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LogicalLocation {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<String>,
}

/// Suggested fix
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Fix {
    pub description: Message,
}

/// Invocation metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Invocation {
    pub execution_successful: bool,
}

/// Known sustainabot rules
fn builtin_rules() -> Vec<ReportingDescriptor> {
    vec![
        rule("sustainabot/general", "General sustainability finding",
             "Code unit analyzed for ecological and economic efficiency.",
             SarifLevel::Note),
        rule("sustainabot/nested-loops", "Deeply nested loops",
             "Deeply nested loops create O(n^k) complexity, wasting CPU and energy.",
             SarifLevel::Warning),
        rule("sustainabot/busy-wait", "Busy-wait loop",
             "Loop without sleep/await/yield burns CPU continuously.",
             SarifLevel::Warning),
        rule("sustainabot/string-concat-in-loop", "String concatenation in loop",
             "String concatenation inside loops causes repeated heap allocation.",
             SarifLevel::Warning),
        rule("sustainabot/clone-in-loop", "Clone in loop",
             ".clone() inside loop body causes repeated deep copies.",
             SarifLevel::Note),
        rule("sustainabot/unbuffered-io", "Unbuffered I/O",
             "File I/O without buffering causes excessive system calls.",
             SarifLevel::Warning),
        rule("sustainabot/large-allocation", "Large allocation",
             "Heap allocation exceeding 1MB detected.",
             SarifLevel::Note),
        rule("sustainabot/redundant-allocation", "Redundant allocation",
             "Excessive .to_string()/.to_owned() where borrows suffice.",
             SarifLevel::Note),
        rule("sustainabot/eco-threshold", "Below eco threshold",
             "Function's ecological score is below the configured threshold.",
             SarifLevel::Warning),
        rule("sustainabot/carbon-intensity", "High carbon intensity",
             "Estimated carbon emissions exceed sustainable baseline.",
             SarifLevel::Warning),
        rule("sustainabot/security-sustainability", "Security-sustainability correlation",
             "Security weak point with sustainability impact detected.",
             SarifLevel::Warning),
    ]
}

fn rule(id: &str, name: &str, desc: &str, level: SarifLevel) -> ReportingDescriptor {
    ReportingDescriptor {
        id: id.to_string(),
        name: Some(name.to_string()),
        short_description: Message { text: desc.to_string() },
        full_description: None,
        help: None,
        default_configuration: Some(ReportingConfiguration { level }),
        properties: None,
    }
}

/// Convert sustainabot analysis results into a SARIF log.
pub fn to_sarif(results: &[AnalysisResult], version: &str) -> SarifLog {
    let rules = builtin_rules();
    let rule_ids: Vec<&str> = rules.iter().map(|r| r.id.as_str()).collect();

    let sarif_results: Vec<SarifResult> = results
        .iter()
        .map(|r| convert_result(r, &rule_ids))
        .collect();

    SarifLog {
        schema: SARIF_SCHEMA.to_string(),
        version: SARIF_VERSION.to_string(),
        runs: vec![Run {
            tool: Tool {
                driver: ToolComponent {
                    name: TOOL_NAME.to_string(),
                    semantic_version: Some(version.to_string()),
                    information_uri: Some(TOOL_INFO_URI.to_string()),
                    rules,
                },
            },
            results: sarif_results,
            invocations: Some(vec![Invocation {
                execution_successful: true,
            }]),
        }],
    }
}

/// Convert sustainabot analysis results to SARIF JSON string.
pub fn to_sarif_json(results: &[AnalysisResult], version: &str) -> Result<String, serde_json::Error> {
    let log = to_sarif(results, version);
    serde_json::to_string_pretty(&log)
}

fn convert_result(result: &AnalysisResult, rule_ids: &[&str]) -> SarifResult {
    let rule_id = &result.rule_id;
    let rule_index = rule_ids.iter().position(|&id| id == rule_id);

    // Determine severity from health scores
    let level = if result.health.eco_score.0 < 30.0 {
        SarifLevel::Error
    } else if result.health.eco_score.0 < 60.0 {
        SarifLevel::Warning
    } else {
        SarifLevel::Note
    };

    // Build message
    let func_name = result
        .location
        .name
        .as_deref()
        .unwrap_or("<anonymous>");
    let message_text = if result.recommendations.is_empty() {
        format!(
            "{}: eco={:.0}/100, energy={:.2}J, carbon={:.4}gCO2e",
            func_name,
            result.health.eco_score.0,
            result.resources.energy.0,
            result.resources.carbon.0,
        )
    } else {
        format!(
            "{}: eco={:.0}/100, energy={:.2}J, carbon={:.4}gCO2e. {}",
            func_name,
            result.health.eco_score.0,
            result.resources.energy.0,
            result.resources.carbon.0,
            result.recommendations.join("; "),
        )
    };

    // Build location
    let location = Location {
        physical_location: PhysicalLocation {
            artifact_location: ArtifactLocation {
                uri: result.location.file.clone(),
                uri_base_id: Some("%SRCROOT%".to_string()),
            },
            region: Some(Region {
                start_line: Some(result.location.line),
                start_column: Some(result.location.column),
                end_line: result.location.end_line,
                end_column: result.location.end_column,
            }),
        },
        logical_locations: result.location.name.as_ref().map(|name| {
            vec![LogicalLocation {
                name: name.clone(),
                kind: Some("function".to_string()),
            }]
        }),
    };

    // Build fixes from suggestion
    let fixes = result
        .suggestion
        .as_ref()
        .map(|s| {
            vec![Fix {
                description: Message { text: s.clone() },
            }]
        })
        .unwrap_or_default();

    // Custom properties with eco/econ scores and resource profile
    let properties = serde_json::json!({
        "eco_score": result.health.eco_score.0,
        "econ_score": result.health.econ_score.0,
        "quality_score": result.health.quality_score,
        "overall_health": result.health.overall,
        "energy_joules": result.resources.energy.0,
        "carbon_gco2e": result.resources.carbon.0,
        "duration_ms": result.resources.duration.0,
        "memory_bytes": result.resources.memory.0,
        "confidence": format!("{:?}", result.confidence),
    });

    SarifResult {
        rule_id: rule_id.clone(),
        rule_index,
        level,
        message: Message { text: message_text },
        locations: vec![location],
        fixes,
        properties: Some(properties),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sustainabot_metrics::*;

    fn sample_result() -> AnalysisResult {
        AnalysisResult {
            location: CodeLocation {
                file: "src/main.rs".to_string(),
                line: 10,
                column: 1,
                end_line: Some(25),
                end_column: Some(2),
                name: Some("process_data".to_string()),
            },
            resources: ResourceProfile {
                energy: Energy::joules(5.0),
                duration: Duration::milliseconds(25.0),
                carbon: Carbon::grams_co2e(0.0007),
                memory: Memory::kilobytes(100),
            },
            health: HealthIndex::compute(
                EcoScore::new(75.0),
                EconScore::new(80.0),
                85.0,
            ),
            recommendations: vec!["Code looks efficient".to_string()],
            rule_id: "sustainabot/general".to_string(),
            suggestion: None,
            end_location: Some((25, 2)),
            confidence: Confidence::Estimated,
        }
    }

    #[test]
    fn test_sarif_structure() {
        let results = vec![sample_result()];
        let log = to_sarif(&results, "0.1.0");

        assert_eq!(log.version, "2.1.0");
        assert_eq!(log.runs.len(), 1);
        assert_eq!(log.runs[0].tool.driver.name, "sustainabot");
        assert!(!log.runs[0].tool.driver.rules.is_empty());
        assert_eq!(log.runs[0].results.len(), 1);
    }

    #[test]
    fn test_sarif_json_valid() {
        let results = vec![sample_result()];
        let json = to_sarif_json(&results, "0.1.0").unwrap();

        // Should be valid JSON
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["version"], "2.1.0");
        assert!(parsed["runs"][0]["results"][0]["locations"][0]["physicalLocation"]["region"]["startLine"]
            .as_u64()
            .unwrap() == 10);
    }

    #[test]
    fn test_sarif_with_suggestion() {
        let mut result = sample_result();
        result.rule_id = "sustainabot/nested-loops".to_string();
        result.suggestion = Some("Use hash map for O(1) lookup".to_string());

        let log = to_sarif(&[result], "0.1.0");
        let sarif_result = &log.runs[0].results[0];

        assert_eq!(sarif_result.rule_id, "sustainabot/nested-loops");
        assert_eq!(sarif_result.fixes.len(), 1);
        assert_eq!(sarif_result.fixes[0].description.text, "Use hash map for O(1) lookup");
    }

    #[test]
    fn test_sarif_severity_mapping() {
        // Low eco score → Error
        let mut result = sample_result();
        result.health = HealthIndex::compute(EcoScore::new(20.0), EconScore::new(80.0), 85.0);
        let log = to_sarif(&[result], "0.1.0");
        assert!(matches!(log.runs[0].results[0].level, SarifLevel::Error));

        // Medium eco score → Warning
        let mut result = sample_result();
        result.health = HealthIndex::compute(EcoScore::new(45.0), EconScore::new(80.0), 85.0);
        let log = to_sarif(&[result], "0.1.0");
        assert!(matches!(log.runs[0].results[0].level, SarifLevel::Warning));

        // High eco score → Note
        let mut result = sample_result();
        result.health = HealthIndex::compute(EcoScore::new(85.0), EconScore::new(80.0), 85.0);
        let log = to_sarif(&[result], "0.1.0");
        assert!(matches!(log.runs[0].results[0].level, SarifLevel::Note));
    }
}
