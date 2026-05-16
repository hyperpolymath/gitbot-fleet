// SPDX-License-Identifier: PMPL-1.0-or-later
//! Fleet health monitoring and checks
//!
//! Monitors the health and performance of the bot fleet, detecting
//! anomalies, tracking resource usage, and providing alerts.

use crate::bot::BotStatus;
use crate::{BotId, Context, Severity, Tier};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Overall fleet health status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum HealthStatus {
    /// All systems operational
    Healthy,
    /// Minor issues detected
    Degraded,
    /// Significant issues requiring attention
    Unhealthy,
    /// Critical failures, immediate action required
    Critical,
}

/// Health check result for the entire fleet
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FleetHealth {
    /// Overall status
    pub status: HealthStatus,
    /// Timestamp of health check
    pub checked_at: DateTime<Utc>,
    /// Individual bot health statuses
    pub bot_health: HashMap<String, BotHealth>,
    /// Tier-level health metrics
    pub tier_health: HashMap<String, TierHealth>,
    /// System-level metrics
    pub system_metrics: SystemMetrics,
    /// Active alerts
    pub alerts: Vec<HealthAlert>,
    /// Health score (0-100)
    pub health_score: f64,
}

/// Health status for an individual bot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BotHealth {
    /// Bot identifier
    pub bot_id: String,
    /// Current status
    pub status: HealthStatus,
    /// Execution state
    pub execution_status: String,
    /// Number of findings produced
    pub findings_count: usize,
    /// Number of errors
    pub errors_count: usize,
    /// Execution duration (if completed)
    pub duration_ms: Option<u64>,
    /// Average duration over last N runs
    pub avg_duration_ms: Option<f64>,
    /// Success rate over last N runs
    pub success_rate: f64,
    /// Detected anomalies
    pub anomalies: Vec<String>,
}

/// Health metrics for a tier
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TierHealth {
    /// Tier name
    pub tier: String,
    /// Tier health status
    pub status: HealthStatus,
    /// Number of bots in tier
    pub bot_count: usize,
    /// Number of healthy bots
    pub healthy_count: usize,
    /// Number of failed bots
    pub failed_count: usize,
    /// Average tier completion time
    pub avg_completion_ms: f64,
    /// Tier-level findings
    pub total_findings: usize,
}

/// System-level metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemMetrics {
    /// Total session duration
    pub session_duration_ms: Option<u64>,
    /// Total bots registered
    pub total_bots: usize,
    /// Bots completed
    pub bots_completed: usize,
    /// Bots in progress
    pub bots_in_progress: usize,
    /// Bots failed
    pub bots_failed: usize,
    /// Total findings
    pub total_findings: usize,
    /// Error rate (errors / total findings)
    pub error_rate: f64,
    /// Average bot duration
    pub avg_bot_duration_ms: f64,
}

/// Health alert
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthAlert {
    /// Alert severity
    pub severity: AlertSeverity,
    /// Alert category
    pub category: AlertCategory,
    /// Alert message
    pub message: String,
    /// Bot affected (if applicable)
    pub bot: Option<String>,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
}

/// Alert severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AlertSeverity {
    Info,
    Warning,
    Error,
    Critical,
}

/// Alert category
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AlertCategory {
    BotFailure,
    SlowExecution,
    HighErrorRate,
    DependencyFailure,
    ResourceExhaustion,
    AnomalyDetected,
}

impl Context {
    /// Perform comprehensive health check
    pub fn health_check(&self) -> FleetHealth {
        let bot_health = self.check_bot_health();
        let tier_health = self.check_tier_health();
        let system_metrics = self.calculate_system_metrics();
        let alerts = self.generate_alerts(&bot_health, &tier_health, &system_metrics);
        let health_score = self.calculate_health_score(&bot_health, &tier_health);
        let status = determine_overall_status(health_score, &alerts);

        FleetHealth {
            status,
            checked_at: Utc::now(),
            bot_health,
            tier_health,
            system_metrics,
            alerts,
            health_score,
        }
    }

    /// Check health of individual bots
    fn check_bot_health(&self) -> HashMap<String, BotHealth> {
        let mut health = HashMap::new();

        for (bot_id, execution) in &self.executions {
            let anomalies = self.detect_bot_anomalies(*bot_id, execution);
            let bot_status = determine_bot_health_status(execution, &anomalies);

            let duration_ms = if let (Some(start), Some(end)) =
                (execution.started_at, execution.completed_at)
            {
                Some((end - start).num_milliseconds() as u64)
            } else {
                None
            };

            health.insert(
                format!("{:?}", bot_id),
                BotHealth {
                    bot_id: format!("{:?}", bot_id),
                    status: bot_status,
                    execution_status: format!("{:?}", execution.status),
                    findings_count: execution.findings_count,
                    errors_count: execution.errors_count,
                    duration_ms,
                    avg_duration_ms: None, // Would need historical data
                    success_rate: 1.0,     // Would need historical data
                    anomalies,
                },
            );
        }

        health
    }

    /// Check health of tiers
    fn check_tier_health(&self) -> HashMap<String, TierHealth> {
        let mut tier_health = HashMap::new();
        let tiers = [
            Tier::Engine,
            Tier::Verifier,
            Tier::Finisher,
            Tier::Executor,
        ];

        for tier in tiers {
            let tier_bots: Vec<_> = self
                .executions
                .iter()
                .filter(|(bot, _)| bot.tier() == tier)
                .collect();

            let bot_count = tier_bots.len();
            let healthy_count = tier_bots
                .iter()
                .filter(|(_, exec)| exec.status == BotStatus::Completed)
                .count();
            let failed_count = tier_bots
                .iter()
                .filter(|(_, exec)| exec.status == BotStatus::Failed)
                .count();

            let mut total_duration = 0u64;
            let mut completed_count = 0usize;
            for (_, exec) in &tier_bots {
                if let (Some(start), Some(end)) = (exec.started_at, exec.completed_at) {
                    total_duration += (end - start).num_milliseconds() as u64;
                    completed_count += 1;
                }
            }

            let avg_completion_ms = if completed_count > 0 {
                total_duration as f64 / completed_count as f64
            } else {
                0.0
            };

            let total_findings: usize = tier_bots.iter().map(|(_, exec)| exec.findings_count).sum();

            let status = if failed_count > 0 && failed_count == bot_count {
                HealthStatus::Critical
            } else if failed_count > 0 {
                HealthStatus::Unhealthy
            } else if healthy_count < bot_count {
                HealthStatus::Degraded
            } else {
                HealthStatus::Healthy
            };

            tier_health.insert(
                format!("{:?}", tier),
                TierHealth {
                    tier: format!("{:?}", tier),
                    status,
                    bot_count,
                    healthy_count,
                    failed_count,
                    avg_completion_ms,
                    total_findings,
                },
            );
        }

        tier_health
    }

    /// Calculate system-level metrics
    fn calculate_system_metrics(&self) -> SystemMetrics {
        let total_bots = self.executions.len();
        let bots_completed = self
            .executions
            .values()
            .filter(|e| e.status == BotStatus::Completed)
            .count();
        let bots_in_progress = self
            .executions
            .values()
            .filter(|e| e.status == BotStatus::Running)
            .count();
        let bots_failed = self
            .executions
            .values()
            .filter(|e| e.status == BotStatus::Failed)
            .count();

        let total_findings = self.findings.findings.len();
        let total_errors = self
            .findings
            .findings
            .iter()
            .filter(|f| matches!(f.severity, Severity::Error))
            .count();
        let error_rate = if total_findings > 0 {
            total_errors as f64 / total_findings as f64
        } else {
            0.0
        };

        let mut total_duration = 0u64;
        let mut completed_count = 0usize;
        for exec in self.executions.values() {
            if let (Some(start), Some(end)) = (exec.started_at, exec.completed_at) {
                total_duration += (end - start).num_milliseconds() as u64;
                completed_count += 1;
            }
        }
        let avg_bot_duration_ms = if completed_count > 0 {
            total_duration as f64 / completed_count as f64
        } else {
            0.0
        };

        SystemMetrics {
            session_duration_ms: self.duration_ms(),
            total_bots,
            bots_completed,
            bots_in_progress,
            bots_failed,
            total_findings,
            error_rate,
            avg_bot_duration_ms,
        }
    }

    /// Detect anomalies in bot execution
    fn detect_bot_anomalies(
        &self,
        bot_id: BotId,
        execution: &crate::bot::BotExecution,
    ) -> Vec<String> {
        let mut anomalies = Vec::new();

        // Check for abnormally high error rate
        if execution.findings_count > 0 {
            let error_rate = execution.errors_count as f64 / execution.findings_count as f64;
            if error_rate > 0.5 {
                anomalies.push(format!(
                    "High error rate: {:.1}% ({}/{})",
                    error_rate * 100.0,
                    execution.errors_count,
                    execution.findings_count
                ));
            }
        }

        // Check for zero findings on completed bot
        if execution.status == BotStatus::Completed && execution.findings_count == 0 {
            // Some bots like Hypatia might legitimately have zero findings
            if !matches!(bot_id, BotId::Hypatia) {
                anomalies.push("No findings produced by completed bot".to_string());
            }
        }

        // Check for abnormally long execution (>5 minutes)
        if let (Some(start), Some(end)) = (execution.started_at, execution.completed_at) {
            let duration = (end - start).num_milliseconds();
            if duration > 300_000 {
                // 5 minutes
                anomalies.push(format!(
                    "Long execution time: {:.1}s",
                    duration as f64 / 1000.0
                ));
            }
        }

        anomalies
    }

    /// Generate health alerts
    fn generate_alerts(
        &self,
        bot_health: &HashMap<String, BotHealth>,
        tier_health: &HashMap<String, TierHealth>,
        system_metrics: &SystemMetrics,
    ) -> Vec<HealthAlert> {
        let mut alerts = Vec::new();

        // Check for failed bots
        for (bot_name, health) in bot_health {
            if health.execution_status == "Failed" {
                alerts.push(HealthAlert {
                    severity: AlertSeverity::Error,
                    category: AlertCategory::BotFailure,
                    message: format!("Bot {} has failed", bot_name),
                    bot: Some(bot_name.clone()),
                    timestamp: Utc::now(),
                });
            }

            // Check for anomalies
            for anomaly in &health.anomalies {
                alerts.push(HealthAlert {
                    severity: AlertSeverity::Warning,
                    category: AlertCategory::AnomalyDetected,
                    message: format!("{}: {}", bot_name, anomaly),
                    bot: Some(bot_name.clone()),
                    timestamp: Utc::now(),
                });
            }
        }

        // Check for tier-level failures
        for (tier_name, health) in tier_health {
            if health.failed_count > 0 && health.failed_count == health.bot_count {
                alerts.push(HealthAlert {
                    severity: AlertSeverity::Critical,
                    category: AlertCategory::DependencyFailure,
                    message: format!("All bots in tier {} have failed", tier_name),
                    bot: None,
                    timestamp: Utc::now(),
                });
            }
        }

        // Check for high system error rate
        if system_metrics.error_rate > 0.3 {
            alerts.push(HealthAlert {
                severity: AlertSeverity::Warning,
                category: AlertCategory::HighErrorRate,
                message: format!(
                    "High error rate: {:.1}% ({} errors / {} findings)",
                    system_metrics.error_rate * 100.0,
                    (system_metrics.error_rate * system_metrics.total_findings as f64) as usize,
                    system_metrics.total_findings
                ),
                bot: None,
                timestamp: Utc::now(),
            });
        }

        alerts
    }

    /// Calculate overall health score (0-100)
    fn calculate_health_score(
        &self,
        bot_health: &HashMap<String, BotHealth>,
        tier_health: &HashMap<String, TierHealth>,
    ) -> f64 {
        if bot_health.is_empty() {
            return 100.0;
        }

        let mut score = 100.0;

        // Deduct for failed bots
        let failed_count = bot_health
            .values()
            .filter(|h| h.execution_status == "Failed")
            .count();
        score -= (failed_count as f64 / bot_health.len() as f64) * 30.0;

        // Deduct for degraded tiers
        let degraded_tiers = tier_health
            .values()
            .filter(|t| t.status == HealthStatus::Degraded || t.status == HealthStatus::Unhealthy)
            .count();
        score -= (degraded_tiers as f64 / tier_health.len() as f64) * 20.0;

        // Deduct for anomalies
        let anomaly_count: usize = bot_health.values().map(|h| h.anomalies.len()).sum();
        score -= (anomaly_count as f64).min(20.0);

        score.max(0.0)
    }
}

/// Determine overall health status from score and alerts
fn determine_overall_status(score: f64, alerts: &[HealthAlert]) -> HealthStatus {
    let has_critical = alerts
        .iter()
        .any(|a| a.severity == AlertSeverity::Critical);
    let has_errors = alerts.iter().any(|a| a.severity == AlertSeverity::Error);

    if has_critical || score < 30.0 {
        HealthStatus::Critical
    } else if has_errors || score < 60.0 {
        HealthStatus::Unhealthy
    } else if score < 85.0 {
        HealthStatus::Degraded
    } else {
        HealthStatus::Healthy
    }
}

/// Determine bot health status
fn determine_bot_health_status(
    execution: &crate::bot::BotExecution,
    anomalies: &[String],
) -> HealthStatus {
    match execution.status {
        BotStatus::Failed => HealthStatus::Critical,
        BotStatus::Completed => {
            if !anomalies.is_empty() {
                HealthStatus::Degraded
            } else {
                HealthStatus::Healthy
            }
        }
        BotStatus::Running => HealthStatus::Healthy,
        BotStatus::Pending => HealthStatus::Healthy,
        BotStatus::Skipped => HealthStatus::Degraded,
    }
}

impl FleetHealth {
    /// Print health report to stdout
    pub fn print(&self) {
        let status_symbol = match self.status {
            HealthStatus::Healthy => "✅",
            HealthStatus::Degraded => "⚠️",
            HealthStatus::Unhealthy => "❌",
            HealthStatus::Critical => "🔴",
        };

        println!();
        println!("╔════════════════════════════════════════════════════════════════╗");
        println!("║                     FLEET HEALTH STATUS                        ║");
        println!("╠════════════════════════════════════════════════════════════════╣");
        println!(
            "║  Status:  {} {:?} (Score: {:.1}/100)                         ║",
            status_symbol,
            self.status,
            self.health_score
        );
        println!(
            "║  Checked: {}                                   ║",
            self.checked_at.format("%Y-%m-%d %H:%M:%S UTC")
        );
        println!("╠════════════════════════════════════════════════════════════════╣");

        // System metrics
        println!("║  System Metrics:                                               ║");
        println!(
            "║    Bots: {} total, {} completed, {} in-progress, {} failed   ║",
            self.system_metrics.total_bots,
            self.system_metrics.bots_completed,
            self.system_metrics.bots_in_progress,
            self.system_metrics.bots_failed
        );
        println!(
            "║    Findings: {} total, error rate: {:.1}%                    ║",
            self.system_metrics.total_findings,
            self.system_metrics.error_rate * 100.0
        );
        println!("╠════════════════════════════════════════════════════════════════╣");

        // Alerts
        if !self.alerts.is_empty() {
            println!("║  Active Alerts: {}                                            ║", self.alerts.len());
            for alert in self.alerts.iter().take(5) {
                let severity_str = match alert.severity {
                    AlertSeverity::Info => "ℹ️ ",
                    AlertSeverity::Warning => "⚠️ ",
                    AlertSeverity::Error => "❌",
                    AlertSeverity::Critical => "🔴",
                };
                let msg = if alert.message.len() > 52 {
                    format!("{}...", &alert.message[..49])
                } else {
                    alert.message.clone()
                };
                println!("║    {} {}                                            ║", severity_str, msg);
            }
            if self.alerts.len() > 5 {
                println!("║    ... and {} more alerts                                 ║", self.alerts.len() - 5);
            }
            println!("╠════════════════════════════════════════════════════════════════╣");
        }

        println!("╚════════════════════════════════════════════════════════════════╝");
        println!();
    }

    /// Export as JSON
    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Finding;
    use std::path::PathBuf;

    #[test]
    fn test_health_check_healthy_fleet() {
        let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
        // Only register the bot we're testing (not all bots)
        ctx.register_bot(BotId::Rhodibot);

        // Simulate successful execution
        ctx.start_bot(BotId::Rhodibot).unwrap();
        ctx.complete_bot(BotId::Rhodibot, 5, 0, 10).unwrap();

        let health = ctx.health_check();

        assert_eq!(health.status, HealthStatus::Healthy);
        assert!(health.health_score > 80.0);
        assert_eq!(health.alerts.len(), 0);
    }

    #[test]
    fn test_health_check_with_failures() {
        let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
        ctx.register_all_bots();

        // Simulate failure
        ctx.start_bot(BotId::Rhodibot).unwrap();
        ctx.fail_bot(BotId::Rhodibot, "Test failure").unwrap();

        let health = ctx.health_check();

        assert!(matches!(
            health.status,
            HealthStatus::Unhealthy | HealthStatus::Critical
        ));
        assert!(health.health_score < 100.0);
        assert!(!health.alerts.is_empty());
    }

    #[test]
    fn test_anomaly_detection_high_error_rate() {
        let mut ctx = Context::new("test-repo", PathBuf::from("/tmp/test"));
        ctx.register_bot(BotId::Rhodibot);

        // Add findings with high error rate
        ctx.start_bot(BotId::Rhodibot).unwrap();
        for i in 0..10 {
            let severity = if i < 8 {
                Severity::Error
            } else {
                Severity::Warning
            };
            ctx.add_finding(Finding::new(
                BotId::Rhodibot,
                &format!("TEST-{}", i),
                severity,
                "Test finding",
            ));
        }
        ctx.complete_bot(BotId::Rhodibot, 10, 8, 10).unwrap();

        let health = ctx.health_check();
        let bot_health = health.bot_health.get("Rhodibot").unwrap();

        assert!(!bot_health.anomalies.is_empty());
        assert!(bot_health
            .anomalies
            .iter()
            .any(|a| a.contains("High error rate")));
    }
}
