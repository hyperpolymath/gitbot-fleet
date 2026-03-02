// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//! Migration Health Tracking for SustainaBot
//!
//! Extends sustainabot's analysis with ReScript migration health monitoring.
//! Tracks migration health scores over time and alerts on regressions.
//!
//! Integration points:
//! - Reads panic-attack migration-snapshot JSON outputs
//! - Produces SARIF-compatible findings for migration regressions
//! - Feeds health deltas into the fleet dispatch pipeline

use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Migration health snapshot (parsed from panic-attack output)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationHealthSnapshot {
    /// Repository or target path
    pub target: PathBuf,
    /// Snapshot label
    pub label: String,
    /// ISO 8601 timestamp
    pub timestamp: String,
    /// Overall health score (0.0 - 1.0)
    pub health_score: f64,
    /// Count of deprecated API usages
    pub deprecated_count: usize,
    /// Count of modern API usages
    pub modern_count: usize,
    /// Config format: bsconfig, rescript_json, both, none
    pub config_format: String,
    /// Detected version bracket
    pub version_bracket: String,
}

/// A migration health regression finding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationRegression {
    /// Repository affected
    pub target: PathBuf,
    /// Previous health score
    pub previous_score: f64,
    /// Current health score
    pub current_score: f64,
    /// Health score delta (negative = regression)
    pub delta: f64,
    /// What regressed
    pub reason: RegressionReason,
    /// SARIF-compatible rule ID
    pub rule_id: String,
    /// Human-readable description
    pub description: String,
}

/// Reason for a migration regression
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RegressionReason {
    /// Health score dropped
    HealthScoreDrop,
    /// Deprecated API count increased
    DeprecatedCountIncrease,
    /// Config format reverted (e.g., rescript.json → bsconfig.json)
    ConfigFormatRevert,
    /// Version bracket downgrade
    VersionBracketDowngrade,
}

impl std::fmt::Display for RegressionReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HealthScoreDrop => write!(f, "Migration health score dropped"),
            Self::DeprecatedCountIncrease => write!(f, "Deprecated API usage increased"),
            Self::ConfigFormatRevert => write!(f, "Config format reverted"),
            Self::VersionBracketDowngrade => write!(f, "Version bracket downgraded"),
        }
    }
}

/// Migration health tracker
///
/// Compares successive snapshots to detect regressions and track
/// migration progress over time.
pub struct MigrationHealthTracker;

impl MigrationHealthTracker {
    /// Compare two snapshots and detect regressions.
    ///
    /// Returns a list of regression findings. An empty list means
    /// migration health is stable or improving.
    pub fn detect_regressions(
        previous: &MigrationHealthSnapshot,
        current: &MigrationHealthSnapshot,
    ) -> Vec<MigrationRegression> {
        let mut regressions = Vec::new();

        // Health score regression (threshold: -0.05 = 5% drop)
        let health_delta = current.health_score - previous.health_score;
        if health_delta < -0.05 {
            regressions.push(MigrationRegression {
                target: current.target.clone(),
                previous_score: previous.health_score,
                current_score: current.health_score,
                delta: health_delta,
                reason: RegressionReason::HealthScoreDrop,
                rule_id: "sustainabot/migration-health-regression".to_string(),
                description: format!(
                    "Migration health score dropped from {:.2} to {:.2} ({:+.2})",
                    previous.health_score, current.health_score, health_delta
                ),
            });
        }

        // Deprecated count regression
        if current.deprecated_count > previous.deprecated_count {
            let increase = current.deprecated_count - previous.deprecated_count;
            regressions.push(MigrationRegression {
                target: current.target.clone(),
                previous_score: previous.health_score,
                current_score: current.health_score,
                delta: -(increase as f64),
                reason: RegressionReason::DeprecatedCountIncrease,
                rule_id: "sustainabot/migration-deprecated-increase".to_string(),
                description: format!(
                    "Deprecated API usage increased by {} (from {} to {})",
                    increase, previous.deprecated_count, current.deprecated_count
                ),
            });
        }

        // Config format regression (rescript_json → bsconfig is a downgrade)
        if previous.config_format == "rescript_json" && current.config_format == "bsconfig" {
            regressions.push(MigrationRegression {
                target: current.target.clone(),
                previous_score: previous.health_score,
                current_score: current.health_score,
                delta: -0.2, // Config revert is a significant regression
                reason: RegressionReason::ConfigFormatRevert,
                rule_id: "sustainabot/migration-config-revert".to_string(),
                description: "Config format reverted from rescript.json to bsconfig.json"
                    .to_string(),
            });
        }

        // Version bracket downgrade
        let prev_rank = version_bracket_rank(&previous.version_bracket);
        let curr_rank = version_bracket_rank(&current.version_bracket);
        if curr_rank < prev_rank {
            regressions.push(MigrationRegression {
                target: current.target.clone(),
                previous_score: previous.health_score,
                current_score: current.health_score,
                delta: (curr_rank as f64 - prev_rank as f64) * 0.1,
                reason: RegressionReason::VersionBracketDowngrade,
                rule_id: "sustainabot/migration-version-downgrade".to_string(),
                description: format!(
                    "Version bracket downgraded from {} to {}",
                    previous.version_bracket, current.version_bracket
                ),
            });
        }

        regressions
    }

    /// Compute a migration velocity metric.
    ///
    /// Given a series of snapshots ordered by time, computes the average
    /// health improvement per snapshot interval.
    pub fn compute_velocity(snapshots: &[MigrationHealthSnapshot]) -> f64 {
        if snapshots.len() < 2 {
            return 0.0;
        }

        let total_delta =
            snapshots.last().unwrap().health_score - snapshots.first().unwrap().health_score;
        total_delta / (snapshots.len() - 1) as f64
    }

    /// Load a migration health snapshot from a JSON file.
    ///
    /// Reads the panic-attack migration-snapshot output format.
    pub fn load_snapshot(path: &Path) -> anyhow::Result<MigrationHealthSnapshot> {
        let content = std::fs::read_to_string(path)?;
        let raw: serde_json::Value = serde_json::from_str(&content)?;

        Ok(MigrationHealthSnapshot {
            target: PathBuf::from(raw["target_path"].as_str().unwrap_or("unknown")),
            label: raw["label"].as_str().unwrap_or("unknown").to_string(),
            timestamp: raw["timestamp"].as_str().unwrap_or("unknown").to_string(),
            health_score: raw["health_score"].as_f64().unwrap_or(0.0),
            deprecated_count: raw["deprecated_api_count"].as_u64().unwrap_or(0) as usize,
            modern_count: raw["modern_api_count"].as_u64().unwrap_or(0) as usize,
            config_format: raw["config_format"].as_str().unwrap_or("none").to_string(),
            version_bracket: raw["version_bracket"]
                .as_str()
                .unwrap_or("unknown")
                .to_string(),
        })
    }
}

/// Rank version brackets for comparison (higher = more modern)
fn version_bracket_rank(bracket: &str) -> u8 {
    match bracket {
        "BuckleScript" => 1,
        "V11" => 2,
        "V12Alpha" => 3,
        "V12Stable" => 4,
        "V12Current" => 5,
        "V13PreRelease" => 6,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_snapshot(
        health: f64,
        deprecated: usize,
        config: &str,
        version: &str,
    ) -> MigrationHealthSnapshot {
        MigrationHealthSnapshot {
            target: PathBuf::from("/tmp/test-repo"),
            label: "test".to_string(),
            timestamp: "2026-03-01T10:00:00Z".to_string(),
            health_score: health,
            deprecated_count: deprecated,
            modern_count: 50,
            config_format: config.to_string(),
            version_bracket: version.to_string(),
        }
    }

    #[test]
    fn test_no_regression_on_improvement() {
        let prev = make_snapshot(0.5, 40, "bsconfig", "V11");
        let curr = make_snapshot(0.7, 20, "rescript_json", "V12Stable");
        let regressions = MigrationHealthTracker::detect_regressions(&prev, &curr);
        assert!(regressions.is_empty());
    }

    #[test]
    fn test_health_score_regression() {
        let prev = make_snapshot(0.8, 10, "rescript_json", "V12Current");
        let curr = make_snapshot(0.6, 10, "rescript_json", "V12Current");
        let regressions = MigrationHealthTracker::detect_regressions(&prev, &curr);
        assert!(regressions
            .iter()
            .any(|r| r.reason == RegressionReason::HealthScoreDrop));
    }

    #[test]
    fn test_deprecated_count_regression() {
        let prev = make_snapshot(0.7, 10, "rescript_json", "V12Stable");
        let curr = make_snapshot(0.7, 25, "rescript_json", "V12Stable");
        let regressions = MigrationHealthTracker::detect_regressions(&prev, &curr);
        assert!(regressions
            .iter()
            .any(|r| r.reason == RegressionReason::DeprecatedCountIncrease));
    }

    #[test]
    fn test_config_format_regression() {
        let prev = make_snapshot(0.8, 5, "rescript_json", "V12Current");
        let curr = make_snapshot(0.8, 5, "bsconfig", "V12Current");
        let regressions = MigrationHealthTracker::detect_regressions(&prev, &curr);
        assert!(regressions
            .iter()
            .any(|r| r.reason == RegressionReason::ConfigFormatRevert));
    }

    #[test]
    fn test_version_bracket_regression() {
        let prev = make_snapshot(0.8, 5, "rescript_json", "V12Stable");
        let curr = make_snapshot(0.8, 5, "rescript_json", "V11");
        let regressions = MigrationHealthTracker::detect_regressions(&prev, &curr);
        assert!(regressions
            .iter()
            .any(|r| r.reason == RegressionReason::VersionBracketDowngrade));
    }

    #[test]
    fn test_small_health_drop_not_flagged() {
        // 3% drop should not trigger (threshold is 5%)
        let prev = make_snapshot(0.8, 10, "rescript_json", "V12Current");
        let curr = make_snapshot(0.77, 10, "rescript_json", "V12Current");
        let regressions = MigrationHealthTracker::detect_regressions(&prev, &curr);
        assert!(!regressions
            .iter()
            .any(|r| r.reason == RegressionReason::HealthScoreDrop));
    }

    #[test]
    fn test_migration_velocity() {
        let snapshots = vec![
            make_snapshot(0.3, 50, "bsconfig", "V11"),
            make_snapshot(0.5, 30, "bsconfig", "V11"),
            make_snapshot(0.7, 15, "rescript_json", "V12Stable"),
            make_snapshot(0.85, 5, "rescript_json", "V12Current"),
        ];
        let velocity = MigrationHealthTracker::compute_velocity(&snapshots);
        // (0.85 - 0.3) / 3 = 0.183...
        assert!(velocity > 0.15);
        assert!(velocity < 0.25);
    }

    #[test]
    fn test_migration_velocity_single_snapshot() {
        let snapshots = vec![make_snapshot(0.5, 20, "bsconfig", "V11")];
        let velocity = MigrationHealthTracker::compute_velocity(&snapshots);
        assert_eq!(velocity, 0.0);
    }

    #[test]
    fn test_version_bracket_rank() {
        assert!(version_bracket_rank("V12Current") > version_bracket_rank("V11"));
        assert!(version_bracket_rank("V13PreRelease") > version_bracket_rank("V12Current"));
        assert!(version_bracket_rank("BuckleScript") < version_bracket_rank("V11"));
    }
}
