// SPDX-License-Identifier: PMPL-1.0-or-later

//! Report generation and output formatting

use anyhow::Result;
use std::path::Path;
use clap::ValueEnum;

use crate::seam::{CheckResult, CheckStatus, Finding, Severity};

/// Output format options
#[derive(Debug, Clone, Copy, Default, ValueEnum)]
pub enum OutputFormat {
    /// Human-readable text
    #[default]
    Text,
    /// JSON format
    Json,
    /// Markdown format
    Markdown,
    /// SARIF format (for GitHub Code Scanning)
    Sarif,
}

/// Reporter for outputting check results
pub struct Reporter {
    format: OutputFormat,
}

impl Reporter {
    pub fn new(format: OutputFormat) -> Self {
        Self { format }
    }

    /// Output the check result to stdout or file
    pub fn output(&self, result: &CheckResult, path: Option<&Path>) -> Result<()> {
        let output = match self.format {
            OutputFormat::Text => self.format_text(result),
            OutputFormat::Json => self.format_json(result)?,
            OutputFormat::Markdown => self.format_markdown(result),
            OutputFormat::Sarif => self.format_sarif(result)?,
        };

        if let Some(path) = path {
            std::fs::write(path, &output)?;
        } else {
            print!("{}", output);
        }

        Ok(())
    }

    fn format_text(&self, result: &CheckResult) -> String {
        let mut output = String::new();

        // Header
        output.push_str(&format!(
            "\n{} Seambot Check Results\n",
            match result.status {
                CheckStatus::Pass => "✓",
                CheckStatus::Warn => "⚠",
                CheckStatus::Fail => "✗",
            }
        ));
        output.push_str(&"─".repeat(50));
        output.push('\n');

        // Summary
        output.push_str(&format!(
            "\nStatus: {:?}\n",
            result.status
        ));
        output.push_str(&format!(
            "Seams: {}/{} checked\n",
            result.summary.checked_seams,
            result.summary.total_seams
        ));
        output.push_str(&format!(
            "Findings: {} errors, {} warnings, {} info\n",
            result.summary.errors,
            result.summary.warnings,
            result.summary.info
        ));

        // Findings by severity
        if !result.findings.is_empty() {
            output.push('\n');
            output.push_str(&"─".repeat(50));
            output.push_str("\nFindings:\n\n");

            // Errors first
            for finding in result.findings.iter().filter(|f| f.severity == Severity::Critical || f.severity == Severity::Error) {
                output.push_str(&self.format_finding(finding));
            }

            // Then warnings
            for finding in result.findings.iter().filter(|f| f.severity == Severity::Warning) {
                output.push_str(&self.format_finding(finding));
            }

            // Then info
            for finding in result.findings.iter().filter(|f| f.severity == Severity::Info) {
                output.push_str(&self.format_finding(finding));
            }
        }

        output.push('\n');
        output
    }

    fn format_finding(&self, finding: &Finding) -> String {
        let icon = match finding.severity {
            Severity::Critical => "🔴",
            Severity::Error => "❌",
            Severity::Warning => "⚠️",
            Severity::Info => "ℹ️",
        };

        let mut line = format!("  {} [{}] {}\n", icon, finding.check, finding.message);

        if let Some(ref location) = finding.location {
            line.push_str(&format!("     Location: {}\n", location));
        }

        if let Some(ref seam_id) = finding.seam_id {
            line.push_str(&format!("     Seam: {}\n", seam_id));
        }

        if let Some(ref suggestion) = finding.suggestion {
            line.push_str(&format!("     Suggestion: {}\n", suggestion));
        }

        line.push('\n');
        line
    }

    fn format_json(&self, result: &CheckResult) -> Result<String> {
        Ok(serde_json::to_string_pretty(result)?)
    }

    fn format_markdown(&self, result: &CheckResult) -> String {
        let mut output = String::new();

        // Header
        output.push_str("# Seambot Check Results\n\n");

        let status_badge = match result.status {
            CheckStatus::Pass => "![Pass](https://img.shields.io/badge/status-pass-green)",
            CheckStatus::Warn => "![Warn](https://img.shields.io/badge/status-warning-yellow)",
            CheckStatus::Fail => "![Fail](https://img.shields.io/badge/status-fail-red)",
        };
        output.push_str(&format!("{}\n\n", status_badge));

        // Summary table
        output.push_str("## Summary\n\n");
        output.push_str("| Metric | Value |\n");
        output.push_str("|--------|-------|\n");
        output.push_str(&format!("| Seams Checked | {}/{} |\n",
            result.summary.checked_seams,
            result.summary.total_seams
        ));
        output.push_str(&format!("| Errors | {} |\n", result.summary.errors));
        output.push_str(&format!("| Warnings | {} |\n", result.summary.warnings));
        output.push_str(&format!("| Info | {} |\n", result.summary.info));

        // Findings
        if !result.findings.is_empty() {
            output.push_str("\n## Findings\n\n");

            // Errors
            let errors: Vec<_> = result.findings.iter()
                .filter(|f| f.severity == Severity::Critical || f.severity == Severity::Error)
                .collect();
            if !errors.is_empty() {
                output.push_str("### Errors\n\n");
                for finding in errors {
                    output.push_str(&self.format_finding_md(finding));
                }
            }

            // Warnings
            let warnings: Vec<_> = result.findings.iter()
                .filter(|f| f.severity == Severity::Warning)
                .collect();
            if !warnings.is_empty() {
                output.push_str("### Warnings\n\n");
                for finding in warnings {
                    output.push_str(&self.format_finding_md(finding));
                }
            }

            // Info
            let info: Vec<_> = result.findings.iter()
                .filter(|f| f.severity == Severity::Info)
                .collect();
            if !info.is_empty() {
                output.push_str("### Info\n\n");
                for finding in info {
                    output.push_str(&self.format_finding_md(finding));
                }
            }
        }

        output
    }

    fn format_finding_md(&self, finding: &Finding) -> String {
        let mut output = format!("- **[{}]** {}\n", finding.check, finding.message);

        if let Some(ref location) = finding.location {
            output.push_str(&format!("  - Location: `{}`\n", location));
        }

        if let Some(ref seam_id) = finding.seam_id {
            output.push_str(&format!("  - Seam: `{}`\n", seam_id));
        }

        if let Some(ref suggestion) = finding.suggestion {
            output.push_str(&format!("  - *Suggestion: {}*\n", suggestion));
        }

        output.push('\n');
        output
    }

    fn format_sarif(&self, result: &CheckResult) -> Result<String> {
        // SARIF 2.1.0 format for GitHub Code Scanning
        let sarif = serde_json::json!({
            "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
            "version": "2.1.0",
            "runs": [{
                "tool": {
                    "driver": {
                        "name": "seambot",
                        "version": env!("CARGO_PKG_VERSION"),
                        "informationUri": "https://github.com/hyperpolymath/seambot",
                        "rules": self.extract_rules(result)
                    }
                },
                "results": self.extract_sarif_results(result)
            }]
        });

        Ok(serde_json::to_string_pretty(&sarif)?)
    }

    fn extract_rules(&self, result: &CheckResult) -> Vec<serde_json::Value> {
        let mut seen = std::collections::HashSet::new();
        let mut rules = Vec::new();

        for finding in &result.findings {
            if seen.insert(&finding.check) {
                rules.push(serde_json::json!({
                    "id": finding.check,
                    "shortDescription": {
                        "text": finding.check.replace('-', " ")
                    },
                    "defaultConfiguration": {
                        "level": match finding.severity {
                            Severity::Critical | Severity::Error => "error",
                            Severity::Warning => "warning",
                            Severity::Info => "note",
                        }
                    }
                }));
            }
        }

        rules
    }

    fn extract_sarif_results(&self, result: &CheckResult) -> Vec<serde_json::Value> {
        result.findings.iter().map(|finding| {
            serde_json::json!({
                "ruleId": finding.check,
                "level": match finding.severity {
                    Severity::Critical | Severity::Error => "error",
                    Severity::Warning => "warning",
                    Severity::Info => "note",
                },
                "message": {
                    "text": finding.message
                },
                "locations": finding.location.as_ref().map(|loc| vec![
                    serde_json::json!({
                        "physicalLocation": {
                            "artifactLocation": {
                                "uri": loc
                            }
                        }
                    })
                ]).unwrap_or_default()
            })
        }).collect()
    }
}
