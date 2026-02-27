// SPDX-License-Identifier: PMPL-1.0-or-later
//! Configuration — attack selection, thresholds, and timeouts for panicbot.
//!
//! All confidence values are configurable to allow tuning based on real scan
//! data. Defaults are conservative — static analysis rarely achieves >0.90
//! true-positive rates, so values here reflect honest assessments of
//! `panic-attack`'s regex/AST-based detection accuracy.

use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Top-level panicbot configuration.
///
/// Controls which `panic-attack` subcommands run, severity thresholds,
/// and subprocess timeout behaviour.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PanicbotConfig {
    /// Which panic-attack subcommands are allowed to execute.
    pub allowed_commands: AllowedCommands,

    /// Minimum severity a finding must have to be reported.
    /// Findings below this threshold are silently dropped.
    pub min_severity: MinSeverity,

    /// Maximum wall-clock time for a single `panic-attack` invocation.
    pub timeout: Duration,

    /// Per-category confidence overrides.
    /// If absent, `translator::default_confidence()` is used.
    pub confidence_overrides: Vec<ConfidenceOverride>,
}

/// Which panic-attack subcommands panicbot is allowed to invoke.
///
/// Dynamic attack modes (`attack`, `assault`, `ambush`, `amuck`, `abduct`,
/// `axial`) are **denied by default** — they execute the target binary and
/// can cause side effects. Static analysis (`assail`, `adjudicate`,
/// `diagnostics`) is safe and enabled by default.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AllowedCommands {
    /// `panic-attack assail` — core static analysis (safe, default: true).
    pub assail: bool,
    /// `panic-attack adjudicate` — multi-report verdict (safe, default: true).
    pub adjudicate: bool,
    /// `panic-attack diagnostics` — preflight health check (safe, default: true).
    pub diagnostics: bool,
}

/// Minimum severity filter for reported findings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MinSeverity {
    /// Report everything.
    Low,
    /// Skip Low-severity findings.
    Medium,
    /// Only High and Critical.
    High,
    /// Only Critical.
    Critical,
}

/// Per-category confidence override.
///
/// Allows tuning confidence values for specific `WeakPointCategory` types
/// without recompiling. Useful when scan data reveals that a particular
/// pattern has higher/lower false-positive rates than the default.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfidenceOverride {
    /// The WeakPointCategory name (e.g., "UnsafeCode", "HardcodedSecret").
    pub category: String,
    /// Override confidence value (0.0 - 1.0).
    pub confidence: f64,
}

impl Default for PanicbotConfig {
    fn default() -> Self {
        Self {
            allowed_commands: AllowedCommands::default(),
            min_severity: MinSeverity::Low,
            timeout: Duration::from_secs(300),
            confidence_overrides: Vec::new(),
        }
    }
}

impl Default for AllowedCommands {
    fn default() -> Self {
        Self {
            assail: true,
            adjudicate: true,
            diagnostics: true,
        }
    }
}

impl MinSeverity {
    /// Returns the numeric threshold for this severity level.
    /// Findings with a severity value >= this threshold are reported.
    pub fn threshold(&self) -> u8 {
        match self {
            MinSeverity::Low => 0,
            MinSeverity::Medium => 1,
            MinSeverity::High => 2,
            MinSeverity::Critical => 3,
        }
    }

    /// Parse from string (case-insensitive).
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "low" => Some(MinSeverity::Low),
            "medium" => Some(MinSeverity::Medium),
            "high" => Some(MinSeverity::High),
            "critical" => Some(MinSeverity::Critical),
            _ => None,
        }
    }
}

impl PanicbotConfig {
    /// Look up a confidence override for a given category name.
    /// Returns `None` if no override is configured.
    pub fn confidence_for(&self, category: &str) -> Option<f64> {
        self.confidence_overrides
            .iter()
            .find(|o| o.category == category)
            .map(|o| o.confidence)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = PanicbotConfig::default();
        assert!(config.allowed_commands.assail);
        assert!(config.allowed_commands.adjudicate);
        assert!(config.allowed_commands.diagnostics);
        assert_eq!(config.min_severity, MinSeverity::Low);
        assert_eq!(config.timeout, Duration::from_secs(300));
        assert!(config.confidence_overrides.is_empty());
    }

    #[test]
    fn test_min_severity_threshold() {
        assert_eq!(MinSeverity::Low.threshold(), 0);
        assert_eq!(MinSeverity::Medium.threshold(), 1);
        assert_eq!(MinSeverity::High.threshold(), 2);
        assert_eq!(MinSeverity::Critical.threshold(), 3);
    }

    #[test]
    fn test_min_severity_from_str() {
        assert_eq!(MinSeverity::from_str("low"), Some(MinSeverity::Low));
        assert_eq!(MinSeverity::from_str("HIGH"), Some(MinSeverity::High));
        assert_eq!(MinSeverity::from_str("invalid"), None);
    }

    #[test]
    fn test_confidence_override_lookup() {
        let config = PanicbotConfig {
            confidence_overrides: vec![
                ConfidenceOverride {
                    category: "UnsafeCode".to_string(),
                    confidence: 0.60,
                },
            ],
            ..Default::default()
        };
        assert_eq!(config.confidence_for("UnsafeCode"), Some(0.60));
        assert_eq!(config.confidence_for("PanicPath"), None);
    }
}
