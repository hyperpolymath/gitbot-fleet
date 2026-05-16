// SPDX-License-Identifier: PMPL-1.0-or-later
//! Fleet-wide reporting and analytics
//!
//! Generates reports about fleet execution, findings, and bot performance
//! across all registered bots. Supports multiple output formats.

use crate::{BotId, Context, Finding, Severity, Tier};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Report format
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReportFormat {
    Markdown,
    Json,
    Html,
}

/// Fleet-wide report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FleetReport {
    pub generated_at: DateTime<Utc>,
    pub session_id: String,
    pub repo_name: String,
    pub summary: FleetSummary,
    pub bot_executions: Vec<BotExecutionReport>,
    pub findings_by_severity: HashMap<String, usize>,
    pub findings_by_bot: HashMap<String, usize>,
    pub tier_performance: HashMap<String, TierPerformance>,
}

/// Fleet execution summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FleetSummary {
    pub total_bots: usize,
    pub bots_completed: usize,
    pub bots_in_progress: usize,
    pub bots_pending: usize,
    pub total_findings: usize,
    pub critical_findings: usize,
    pub errors: usize,
    pub warnings: usize,
    pub overall_health: f64,
}

/// Individual bot execution report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BotExecutionReport {
    pub bot_id: String,
    pub tier: String,
    pub status: String,
    pub findings_count: usize,
    pub errors_count: usize,
    pub duration_ms: Option<u64>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
}

/// Tier performance metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TierPerformance {
    pub tier: String,
    pub bots_count: usize,
    pub completed_count: usize,
    pub total_findings: usize,
    pub avg_duration_ms: f64,
}

impl Context {
    /// Generate a fleet-wide report
    pub fn generate_report(&self, format: ReportFormat) -> String {
        let report = self.build_report();

        match format {
            ReportFormat::Markdown => self.format_markdown(&report),
            ReportFormat::Json => serde_json::to_string_pretty(&report).unwrap_or_default(),
            ReportFormat::Html => self.format_html(&report),
        }
    }

    /// Build the fleet report structure
    fn build_report(&self) -> FleetReport {
        let summary = self.build_summary();
        let bot_executions = self.build_bot_executions();
        let findings_by_severity = self.count_findings_by_severity();
        let findings_by_bot = self.count_findings_by_bot();
        let tier_performance = self.calculate_tier_performance();

        FleetReport {
            generated_at: Utc::now(),
            session_id: self.session_id.to_string(),
            repo_name: self.repo_name.clone(),
            summary,
            bot_executions,
            findings_by_severity,
            findings_by_bot,
            tier_performance,
        }
    }

    /// Build fleet summary
    fn build_summary(&self) -> FleetSummary {
        let total_bots = self.executions.len();
        let bots_completed = self.executions.values().filter(|e| e.completed_at.is_some()).count();
        let bots_in_progress = self
            .executions
            .values()
            .filter(|e| e.started_at.is_some() && e.completed_at.is_none())
            .count();
        let bots_pending = total_bots - bots_completed - bots_in_progress;

        let total_findings = self.findings.findings.len();
        let critical_findings = 0; // No Critical severity in current schema
        let errors = self
            .findings
            .findings
            .iter()
            .filter(|f| matches!(f.severity, Severity::Error))
            .count();
        let warnings = self
            .findings
            .findings
            .iter()
            .filter(|f| matches!(f.severity, Severity::Warning))
            .count();

        // Calculate health score (0-100)
        let overall_health = if total_bots > 0 {
            let completion_score = (bots_completed as f64 / total_bots as f64) * 50.0;
            let severity_penalty = (critical_findings as f64 * 10.0)
                + (errors as f64 * 5.0)
                + (warnings as f64 * 1.0);
            let finding_score = (50.0 - severity_penalty.min(50.0)).max(0.0);
            completion_score + finding_score
        } else {
            100.0
        };

        FleetSummary {
            total_bots,
            bots_completed,
            bots_in_progress,
            bots_pending,
            total_findings,
            critical_findings,
            errors,
            warnings,
            overall_health,
        }
    }

    /// Build bot execution reports
    fn build_bot_executions(&self) -> Vec<BotExecutionReport> {
        self.executions
            .iter()
            .map(|(bot_id, exec)| {
                let status = if exec.completed_at.is_some() {
                    "completed"
                } else if exec.started_at.is_some() {
                    "in_progress"
                } else {
                    "pending"
                }
                .to_string();

                let duration_ms = if let (Some(start), Some(end)) = (exec.started_at, exec.completed_at) {
                    Some((end.timestamp_millis() - start.timestamp_millis()) as u64)
                } else {
                    None
                };

                BotExecutionReport {
                    bot_id: format!("{:?}", bot_id),
                    tier: format!("{:?}", bot_id.tier()),
                    status,
                    findings_count: exec.findings_count,
                    errors_count: exec.errors_count,
                    duration_ms,
                    started_at: exec.started_at,
                    completed_at: exec.completed_at,
                }
            })
            .collect()
    }

    /// Count findings by severity
    fn count_findings_by_severity(&self) -> HashMap<String, usize> {
        let mut counts = HashMap::new();
        for finding in &self.findings.findings {
            let severity = format!("{:?}", finding.severity);
            *counts.entry(severity).or_insert(0) += 1;
        }
        counts
    }

    /// Count findings by bot
    fn count_findings_by_bot(&self) -> HashMap<String, usize> {
        let mut counts = HashMap::new();
        for finding in &self.findings.findings {
            let bot = format!("{:?}", finding.source);
            *counts.entry(bot).or_insert(0) += 1;
        }
        counts
    }

    /// Calculate tier performance metrics
    fn calculate_tier_performance(&self) -> HashMap<String, TierPerformance> {
        let mut tier_stats: HashMap<Tier, (usize, usize, usize, Vec<u64>)> = HashMap::new();

        for (bot_id, exec) in &self.executions {
            let tier = bot_id.tier();
            let entry = tier_stats.entry(tier).or_insert((0, 0, 0, vec![]));

            entry.0 += 1; // bots_count
            if exec.completed_at.is_some() {
                entry.1 += 1; // completed_count
            }
            entry.2 += exec.findings_count; // total_findings

            if let (Some(start), Some(end)) = (exec.started_at, exec.completed_at) {
                let duration = (end.timestamp_millis() - start.timestamp_millis()) as u64;
                entry.3.push(duration);
            }
        }

        tier_stats
            .into_iter()
            .map(|(tier, (bots_count, completed_count, total_findings, durations))| {
                let avg_duration_ms = if !durations.is_empty() {
                    durations.iter().sum::<u64>() as f64 / durations.len() as f64
                } else {
                    0.0
                };

                (
                    format!("{:?}", tier),
                    TierPerformance {
                        tier: format!("{:?}", tier),
                        bots_count,
                        completed_count,
                        total_findings,
                        avg_duration_ms,
                    },
                )
            })
            .collect()
    }

    /// Format report as Markdown
    fn format_markdown(&self, report: &FleetReport) -> String {
        let mut md = String::new();

        // Header
        md.push_str(&format!("# Fleet Report: {}\n\n", report.repo_name));
        md.push_str(&format!(
            "**Generated:** {}\n",
            report.generated_at.format("%Y-%m-%d %H:%M:%S UTC")
        ));
        md.push_str(&format!("**Session ID:** `{}`\n\n", report.session_id));

        // Summary
        md.push_str("## Summary\n\n");
        md.push_str(&format!("**Overall Health:** {:.1}/100\n\n", report.summary.overall_health));
        md.push_str("| Metric | Value |\n");
        md.push_str("|--------|-------|\n");
        md.push_str(&format!("| Total Bots | {} |\n", report.summary.total_bots));
        md.push_str(&format!("| Completed | {} |\n", report.summary.bots_completed));
        md.push_str(&format!("| In Progress | {} |\n", report.summary.bots_in_progress));
        md.push_str(&format!("| Pending | {} |\n", report.summary.bots_pending));
        md.push_str(&format!("| Total Findings | {} |\n", report.summary.total_findings));
        md.push_str(&format!("| Critical | {} |\n", report.summary.critical_findings));
        md.push_str(&format!("| Errors | {} |\n", report.summary.errors));
        md.push_str(&format!("| Warnings | {} |\n\n", report.summary.warnings));

        // Bot Executions
        md.push_str("## Bot Executions\n\n");
        md.push_str("| Bot | Tier | Status | Findings | Errors | Duration (ms) |\n");
        md.push_str("|-----|------|--------|----------|--------|---------------|\n");
        for exec in &report.bot_executions {
            md.push_str(&format!(
                "| {} | {} | {} | {} | {} | {} |\n",
                exec.bot_id,
                exec.tier,
                exec.status,
                exec.findings_count,
                exec.errors_count,
                exec.duration_ms
                    .map(|d| d.to_string())
                    .unwrap_or_else(|| "-".to_string())
            ));
        }
        md.push_str("\n");

        // Tier Performance
        md.push_str("## Tier Performance\n\n");
        md.push_str("| Tier | Bots | Completed | Findings | Avg Duration (ms) |\n");
        md.push_str("|------|------|-----------|----------|-------------------|\n");
        for (_, perf) in &report.tier_performance {
            md.push_str(&format!(
                "| {} | {} | {} | {} | {:.0} |\n",
                perf.tier, perf.bots_count, perf.completed_count, perf.total_findings, perf.avg_duration_ms
            ));
        }

        md
    }

    /// Format report as HTML
    fn format_html(&self, report: &FleetReport) -> String {
        format!(
            r#"<!DOCTYPE html>
<html>
<head>
    <title>Fleet Report: {}</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        h1 {{ color: #333; }}
        table {{ border-collapse: collapse; width: 100%; margin: 20px 0; }}
        th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
        th {{ background-color: #4CAF50; color: white; }}
        .health-score {{ font-size: 24px; font-weight: bold; }}
        .health-good {{ color: #4CAF50; }}
        .health-warning {{ color: #ff9800; }}
        .health-critical {{ color: #f44336; }}
    </style>
</head>
<body>
    <h1>Fleet Report: {}</h1>
    <p><strong>Generated:</strong> {}</p>
    <p><strong>Session ID:</strong> {}</p>

    <h2>Summary</h2>
    <p class="health-score {}">Overall Health: {:.1}/100</p>

    <h2>Findings</h2>
    <p>Total: {} | Critical: {} | Errors: {} | Warnings: {}</p>

    <p><em>Full report data available in JSON format</em></p>
</body>
</html>"#,
            report.repo_name,
            report.repo_name,
            report.generated_at.format("%Y-%m-%d %H:%M:%S UTC"),
            report.session_id,
            if report.summary.overall_health >= 80.0 {
                "health-good"
            } else if report.summary.overall_health >= 50.0 {
                "health-warning"
            } else {
                "health-critical"
            },
            report.summary.overall_health,
            report.summary.total_findings,
            report.summary.critical_findings,
            report.summary.errors,
            report.summary.warnings
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_fleet_report_generation() {
        let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
        ctx.register_all_bots();

        // Start and complete some bots
        ctx.start_bot(BotId::Rhodibot).unwrap();
        ctx.complete_bot(BotId::Rhodibot, 2, 1, 0).unwrap();

        let report = ctx.build_report();

        assert_eq!(report.repo_name, "test-repo");
        assert!(report.summary.total_bots > 0);
        assert_eq!(report.summary.bots_completed, 1);
    }

    #[test]
    fn test_markdown_report_format() {
        let ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
        let markdown = ctx.generate_report(ReportFormat::Markdown);

        assert!(markdown.contains("# Fleet Report"));
        assert!(markdown.contains("test-repo"));
        assert!(markdown.contains("Overall Health"));
    }

    #[test]
    fn test_json_report_format() {
        let ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
        let json = ctx.generate_report(ReportFormat::Json);

        assert!(json.contains("\"repo_name\""));
        assert!(json.contains("\"session_id\""));
        assert!(serde_json::from_str::<FleetReport>(&json).is_ok());
    }

    #[test]
    fn test_html_report_format() {
        let ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
        let html = ctx.generate_report(ReportFormat::Html);

        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("Fleet Report"));
        assert!(html.contains("Overall Health"));
    }
}
