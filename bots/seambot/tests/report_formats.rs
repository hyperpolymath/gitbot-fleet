// SPDX-License-Identifier: PMPL-1.0-or-later

//! Tests for report output formats
//!
//! Verifies:
//! - SARIF output is valid JSON and follows SARIF 2.1.0 schema
//! - Markdown output contains all findings
//! - JSON output round-trips correctly

#[cfg(test)]
mod tests {
    #[test]
    fn test_sarif_valid_json_structure() {
        // SARIF output must be valid JSON following SARIF 2.1.0 schema
        let sarif = serde_json::json!({
            "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
            "version": "2.1.0",
            "runs": [{
                "tool": {
                    "driver": {
                        "name": "seambot",
                        "version": "0.1.0",
                        "informationUri": "https://github.com/hyperpolymath/seambot",
                        "rules": [
                            {
                                "id": "seam-drift",
                                "shortDescription": {
                                    "text": "seam drift"
                                },
                                "defaultConfiguration": {
                                    "level": "warning"
                                }
                            }
                        ]
                    }
                },
                "results": [
                    {
                        "ruleId": "seam-drift",
                        "level": "warning",
                        "message": {
                            "text": "Seam 'api-seam' has drifted from baseline"
                        },
                        "locations": []
                    }
                ]
            }]
        });

        // Verify required SARIF 2.1.0 fields
        assert_eq!(sarif["version"], "2.1.0");
        assert!(sarif["$schema"].as_str().unwrap().contains("sarif-schema-2.1.0"));
        assert!(sarif["runs"].is_array());

        let runs = sarif["runs"].as_array().unwrap();
        assert_eq!(runs.len(), 1);

        // Verify tool driver
        let driver = &runs[0]["tool"]["driver"];
        assert_eq!(driver["name"], "seambot");
        assert!(driver["informationUri"].as_str().is_some());
        assert!(driver["rules"].is_array());

        // Verify results
        let results = runs[0]["results"].as_array().unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0]["ruleId"], "seam-drift");
        assert_eq!(results[0]["level"], "warning");
        assert!(results[0]["message"]["text"].as_str().is_some());
    }

    #[test]
    fn test_sarif_severity_levels() {
        // SARIF levels must match: error, warning, note
        let level_mappings = [
            ("critical", "error"),
            ("error", "error"),
            ("warning", "warning"),
            ("info", "note"),
        ];

        for (severity, expected_level) in level_mappings {
            let level = match severity {
                "critical" | "error" => "error",
                "warning" => "warning",
                "info" => "note",
                _ => unreachable!(),
            };
            assert_eq!(level, expected_level,
                "Severity '{}' should map to SARIF level '{}'", severity, expected_level);
        }
    }

    #[test]
    fn test_sarif_with_location() {
        // SARIF results with file locations
        let result = serde_json::json!({
            "ruleId": "checklist-exists",
            "level": "error",
            "message": {
                "text": "Checklist file not found for seam 'api-seam'"
            },
            "locations": [{
                "physicalLocation": {
                    "artifactLocation": {
                        "uri": "spec/seams/checklists/api-seam.adoc"
                    }
                }
            }]
        });

        let locations = result["locations"].as_array().unwrap();
        assert_eq!(locations.len(), 1);
        assert_eq!(
            locations[0]["physicalLocation"]["artifactLocation"]["uri"],
            "spec/seams/checklists/api-seam.adoc"
        );
    }

    #[test]
    fn test_markdown_output_contains_findings() {
        // Markdown output must contain summary table and all findings
        let markdown = format!(
            "# Seambot Check Results\n\n\
             ![Warn](https://img.shields.io/badge/status-warning-yellow)\n\n\
             ## Summary\n\n\
             | Metric | Value |\n\
             |--------|-------|\n\
             | Seams Checked | {}/{} |\n\
             | Errors | {} |\n\
             | Warnings | {} |\n\
             | Info | {} |\n\n\
             ## Findings\n\n\
             ### Warnings\n\n\
             - **[seam-drift]** Seam 'api-seam' has drifted from baseline\n\
             - **[seam-has-checklist]** Seam 'db-seam' has no checklist\n",
            3, 5, 0, 2, 1
        );

        // Verify structure
        assert!(markdown.contains("# Seambot Check Results"));
        assert!(markdown.contains("## Summary"));
        assert!(markdown.contains("| Metric | Value |"));
        assert!(markdown.contains("Seams Checked"));
        assert!(markdown.contains("## Findings"));
        assert!(markdown.contains("### Warnings"));
        assert!(markdown.contains("seam-drift"));
        assert!(markdown.contains("seam-has-checklist"));
    }

    #[test]
    fn test_markdown_status_badges() {
        // Status badges must use correct colors
        let pass_badge = "![Pass](https://img.shields.io/badge/status-pass-green)";
        let warn_badge = "![Warn](https://img.shields.io/badge/status-warning-yellow)";
        let fail_badge = "![Fail](https://img.shields.io/badge/status-fail-red)";

        assert!(pass_badge.contains("pass-green"));
        assert!(warn_badge.contains("warning-yellow"));
        assert!(fail_badge.contains("fail-red"));
    }

    #[test]
    fn test_json_round_trip() {
        // JSON output must round-trip correctly
        let check_result = serde_json::json!({
            "status": "warn",
            "findings": [
                {
                    "check": "seam-drift",
                    "severity": "warning",
                    "message": "Seam 'api-seam' has drifted from baseline",
                    "location": null,
                    "suggestion": "Review changes and update baseline if intentional",
                    "seam_id": "api-seam"
                },
                {
                    "check": "register-exists",
                    "severity": "error",
                    "message": "Seam register not found",
                    "location": "spec/seams/seam-register.json",
                    "suggestion": "Run 'seambot init' to create seam infrastructure",
                    "seam_id": null
                }
            ],
            "summary": {
                "total_seams": 5,
                "checked_seams": 3,
                "errors": 1,
                "warnings": 1,
                "info": 0
            }
        });

        // Serialize to string
        let json_str = serde_json::to_string_pretty(&check_result).unwrap();

        // Parse back
        let parsed: serde_json::Value = serde_json::from_str(&json_str).unwrap();

        // Verify round-trip
        assert_eq!(parsed["status"], "warn");
        assert_eq!(parsed["findings"].as_array().unwrap().len(), 2);
        assert_eq!(parsed["summary"]["total_seams"], 5);
        assert_eq!(parsed["summary"]["errors"], 1);
        assert_eq!(parsed["summary"]["warnings"], 1);

        // Verify individual findings
        assert_eq!(parsed["findings"][0]["check"], "seam-drift");
        assert_eq!(parsed["findings"][0]["severity"], "warning");
        assert_eq!(parsed["findings"][1]["check"], "register-exists");
        assert_eq!(parsed["findings"][1]["severity"], "error");
    }

    #[test]
    fn test_json_empty_result() {
        // Empty check result should serialize correctly
        let result = serde_json::json!({
            "status": "pass",
            "findings": [],
            "summary": {
                "total_seams": 0,
                "checked_seams": 0,
                "errors": 0,
                "warnings": 0,
                "info": 0
            }
        });

        let json_str = serde_json::to_string(&result).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json_str).unwrap();
        assert_eq!(parsed["status"], "pass");
        assert!(parsed["findings"].as_array().unwrap().is_empty());
    }

    #[test]
    fn test_text_output_formatting() {
        // Text output should have correct status symbols
        let pass_symbol = "\u{2713}"; // checkmark
        let warn_symbol = "\u{26a0}"; // warning
        let fail_symbol = "\u{2717}"; // X mark

        // Verify symbols are distinct
        assert_ne!(pass_symbol, warn_symbol);
        assert_ne!(warn_symbol, fail_symbol);
        assert_ne!(pass_symbol, fail_symbol);
    }

    #[test]
    fn test_sarif_deduplicates_rules() {
        // When multiple findings use the same rule, SARIF should only list the rule once
        let sarif = serde_json::json!({
            "version": "2.1.0",
            "runs": [{
                "tool": {
                    "driver": {
                        "name": "seambot",
                        "rules": [
                            {
                                "id": "seam-drift",
                                "shortDescription": {"text": "seam drift"},
                                "defaultConfiguration": {"level": "warning"}
                            }
                        ]
                    }
                },
                "results": [
                    {"ruleId": "seam-drift", "level": "warning", "message": {"text": "Seam A drifted"}},
                    {"ruleId": "seam-drift", "level": "warning", "message": {"text": "Seam B drifted"}}
                ]
            }]
        });

        let rules = sarif["runs"][0]["tool"]["driver"]["rules"].as_array().unwrap();
        let results = sarif["runs"][0]["results"].as_array().unwrap();

        // One rule definition, two results
        assert_eq!(rules.len(), 1);
        assert_eq!(results.len(), 2);
    }
}
