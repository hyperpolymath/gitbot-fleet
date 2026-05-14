// SPDX-License-Identifier: PMPL-1.0
//! Safety Triangle types for the gitbot-fleet remediation pipeline.
//!
//! The Safety Triangle prioritizes remediation actions:
//! 1. Eliminate — Remove the hazard entirely (highest priority)
//! 2. Substitute — Replace with a proven-safe alternative
//! 3. Control — Add engineering controls (lowest priority)

use serde::{Deserialize, Serialize};

/// Safety Triangle remediation tier.
///
/// Determines the approach to fixing a finding, ordered by preference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TriangleTier {
    /// Remove the hazard entirely (dead code, TODO markers, unused unsafe).
    Eliminate,
    /// Replace with a proven-safe alternative (unsafe SQL -> SafeSQL from proven repo).
    Substitute,
    /// Add engineering controls (bounds checks, documentation, error handling).
    Control,
}

impl TriangleTier {
    /// Priority order (lower = higher priority).
    pub fn priority(&self) -> u8 {
        match self {
            TriangleTier::Eliminate => 1,
            TriangleTier::Substitute => 2,
            TriangleTier::Control => 3,
        }
    }

    /// Whether this tier can potentially be auto-executed.
    pub fn can_auto_execute(&self) -> bool {
        matches!(self, TriangleTier::Eliminate)
    }

    /// Human-readable description of the tier action.
    pub fn description(&self) -> &'static str {
        match self {
            TriangleTier::Eliminate => "Remove the hazard entirely",
            TriangleTier::Substitute => "Replace with proven-safe alternative",
            TriangleTier::Control => "Add engineering controls",
        }
    }
}

impl std::fmt::Display for TriangleTier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TriangleTier::Eliminate => write!(f, "eliminate"),
            TriangleTier::Substitute => write!(f, "substitute"),
            TriangleTier::Control => write!(f, "control"),
        }
    }
}

/// Confidence thresholds for dispatch decisions.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfidenceThresholds {
    /// Minimum confidence for auto-execution (robot-repo-automaton).
    pub auto_execute_min: f64,
    /// Minimum confidence for review-required execution (rhodibot PR).
    pub review_min: f64,
    /// Minimum successful fixes before auto-execution is allowed.
    pub min_successful_fixes: u32,
}

impl Default for ConfidenceThresholds {
    fn default() -> Self {
        Self {
            auto_execute_min: 0.95,
            review_min: 0.85,
            min_successful_fixes: 3,
        }
    }
}

impl ConfidenceThresholds {
    /// Determine the dispatch strategy for a given confidence level.
    pub fn dispatch_strategy(&self, confidence: f64, successful_fixes: u32) -> DispatchStrategy {
        if confidence >= self.auto_execute_min && successful_fixes >= self.min_successful_fixes {
            DispatchStrategy::AutoExecute
        } else if confidence >= self.review_min {
            DispatchStrategy::ReviewRequired
        } else {
            DispatchStrategy::ReportOnly
        }
    }
}

/// How a finding should be dispatched based on confidence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DispatchStrategy {
    /// Apply fix automatically via robot-repo-automaton.
    AutoExecute,
    /// Create PR for human review via rhodibot.
    ReviewRequired,
    /// Log advisory only via sustainabot.
    ReportOnly,
}
