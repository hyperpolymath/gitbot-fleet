// SPDX-License-Identifier: MPL-2.0
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
/// and subprocess timeout and budget behaviour.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PanicbotConfig {
    /// Which panic-attack subcommands are allowed to execute.
    pub allowed_commands: AllowedCommands,

    /// Minimum severity a finding must have to be reported.
    /// Findings below this threshold are silently dropped.
    pub min_severity: MinSeverity,

    /// Maximum wall-clock time for a single `panic-attack` invocation.
    pub timeout: Duration,

    /// Wall-clock budget for the WHOLE suite. Each invocation runs for at most
    /// the smaller of `timeout` and what is left of the budget; a part that
    /// cannot start inside the budget reports `error`, never a silent skip.
    /// Kept under the report-back driver's 300 s per-adapter limit.
    pub budget: Duration,

    /// Things the directive loader wants the operator to see (unknown command
    /// names, a deny that overrides an allow). Printed by the CLI.
    #[serde(default)]
    pub notes: Vec<String>,

    /// Per-category confidence overrides.
    /// If absent, `translator::default_confidence()` is used.
    pub confidence_overrides: Vec<ConfidenceOverride>,
}

/// A panic-attack subcommand panicbot can run (class (a) of the inventory in
/// [`crate::suite`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Subcommand {
    /// Static analysis of a directory (weak points).
    Assail,
    /// Proven-library substitution survey of a directory.
    Assay,
    /// Tool preflight: binary present, version, health.
    Diagnostics,
    /// Verdict over dynamic-mode reports.
    Adjudicate,
    /// Single-axis attack on a built program (covered by `assault`).
    Attack,
    /// Assail plus multi-axis attack on a built program.
    Assault,
    /// Program run under ambient stressors.
    Ambush,
    /// File mutation with an execution check.
    Amuck,
    /// File isolation with an execution check.
    Abduct,
    /// Reaction observation from an execution or reports.
    Axial,
}

impl Subcommand {
    /// Every class (a) subcommand, in the order the suite runs them.
    pub const ALL: [Subcommand; 10] = [
        Subcommand::Assail,
        Subcommand::Assay,
        Subcommand::Assault,
        Subcommand::Ambush,
        Subcommand::Attack,
        Subcommand::Amuck,
        Subcommand::Abduct,
        Subcommand::Axial,
        Subcommand::Adjudicate,
        Subcommand::Diagnostics,
    ];

    /// The subcommand as panic-attack and the directive files spell it.
    pub fn name(self) -> &'static str {
        match self {
            Subcommand::Assail => "assail",
            Subcommand::Assay => "assay",
            Subcommand::Diagnostics => "diagnostics",
            Subcommand::Adjudicate => "adjudicate",
            Subcommand::Attack => "attack",
            Subcommand::Assault => "assault",
            Subcommand::Ambush => "ambush",
            Subcommand::Amuck => "amuck",
            Subcommand::Abduct => "abduct",
            Subcommand::Axial => "axial",
        }
    }

    /// Parse a directive spelling. Unknown names are `None`, never a guess.
    pub fn parse(s: &str) -> Option<Subcommand> {
        Subcommand::ALL.into_iter().find(|c| c.name() == s)
    }

    /// Whether the subcommand executes code from the scanned repository
    /// (or a program built from it). Dynamic modes need an explicit target
    /// and are refused under privileged CI triggers (see [`crate::suite`]).
    pub fn is_dynamic(self) -> bool {
        matches!(
            self,
            Subcommand::Attack
                | Subcommand::Assault
                | Subcommand::Ambush
                | Subcommand::Amuck
                | Subcommand::Abduct
                | Subcommand::Axial
        )
    }
}

/// Which panic-attack subcommands panicbot is allowed to invoke.
///
/// The default is the FULL SUITE: every class (a) subcommand (owner ruling
/// 2026-11-13). A repository narrows it with its directive file, which can
/// deny any of them or replace the set with an explicit allow list (see
/// [`crate::directives`]). Allowed is not the same as run: a dynamic mode
/// that is allowed but has no target reports `not_applicable` with its reason.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AllowedCommands {
    /// `panic-attack assail` — core static analysis.
    pub assail: bool,
    /// `panic-attack assay` — proven-library substitution survey.
    pub assay: bool,
    /// `panic-attack adjudicate` — verdict over dynamic-mode reports.
    pub adjudicate: bool,
    /// `panic-attack diagnostics` — preflight health check.
    pub diagnostics: bool,
    /// `panic-attack attack` — single-axis attack (covered by `assault`).
    pub attack: bool,
    /// `panic-attack assault` — assail plus multi-axis attack on a program.
    pub assault: bool,
    /// `panic-attack ambush` — program under ambient stressors.
    pub ambush: bool,
    /// `panic-attack amuck` — file mutation with an execution check.
    pub amuck: bool,
    /// `panic-attack abduct` — file isolation with an execution check.
    pub abduct: bool,
    /// `panic-attack axial` — reaction observation.
    pub axial: bool,
}

impl AllowedCommands {
    /// Every class (a) subcommand allowed: the default.
    pub fn all() -> Self {
        Self {
            assail: true,
            assay: true,
            adjudicate: true,
            diagnostics: true,
            attack: true,
            assault: true,
            ambush: true,
            amuck: true,
            abduct: true,
            axial: true,
        }
    }

    /// Nothing allowed.
    pub fn none() -> Self {
        Self {
            assail: false,
            assay: false,
            adjudicate: false,
            diagnostics: false,
            attack: false,
            assault: false,
            ambush: false,
            amuck: false,
            abduct: false,
            axial: false,
        }
    }

    /// What a directive file granted before the full-suite ruling: the static
    /// trio. A directive file that says nothing about `allow` keeps exactly
    /// this, so a repository's scope is never widened behind its back.
    pub fn legacy_static() -> Self {
        let mut a = Self::none();
        a.assail = true;
        a.adjudicate = true;
        a.diagnostics = true;
        a
    }

    /// Whether `cmd` is allowed.
    pub fn allows(&self, cmd: Subcommand) -> bool {
        match cmd {
            Subcommand::Assail => self.assail,
            Subcommand::Assay => self.assay,
            Subcommand::Diagnostics => self.diagnostics,
            Subcommand::Adjudicate => self.adjudicate,
            Subcommand::Attack => self.attack,
            Subcommand::Assault => self.assault,
            Subcommand::Ambush => self.ambush,
            Subcommand::Amuck => self.amuck,
            Subcommand::Abduct => self.abduct,
            Subcommand::Axial => self.axial,
        }
    }

    /// Allow or deny one subcommand.
    pub fn set(&mut self, cmd: Subcommand, allowed: bool) {
        match cmd {
            Subcommand::Assail => self.assail = allowed,
            Subcommand::Assay => self.assay = allowed,
            Subcommand::Diagnostics => self.diagnostics = allowed,
            Subcommand::Adjudicate => self.adjudicate = allowed,
            Subcommand::Attack => self.attack = allowed,
            Subcommand::Assault => self.assault = allowed,
            Subcommand::Ambush => self.ambush = allowed,
            Subcommand::Amuck => self.amuck = allowed,
            Subcommand::Abduct => self.abduct = allowed,
            Subcommand::Axial => self.axial = allowed,
        }
    }
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

impl Default for AllowedCommands {
    fn default() -> Self {
        Self::all()
    }
}

/// Default wall-clock budget for the whole suite (see [`PanicbotConfig::budget`]).
pub const DEFAULT_BUDGET: Duration = Duration::from_secs(240);

impl Default for PanicbotConfig {
    fn default() -> Self {
        Self {
            allowed_commands: AllowedCommands::default(),
            min_severity: MinSeverity::Low,
            timeout: Duration::from_secs(300),
            budget: DEFAULT_BUDGET,
            notes: Vec::new(),
            confidence_overrides: Vec::new(),
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
}

/// Parse from string (case-insensitive).
impl std::str::FromStr for MinSeverity {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "low" => Ok(MinSeverity::Low),
            "medium" => Ok(MinSeverity::Medium),
            "high" => Ok(MinSeverity::High),
            "critical" => Ok(MinSeverity::Critical),
            other => Err(format!("unknown severity: {other}")),
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
        assert_eq!(config.budget, DEFAULT_BUDGET);
        assert!(
            config.budget < Duration::from_secs(300),
            "must stay under the driver's 300 s adapter limit"
        );
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
        assert_eq!("low".parse::<MinSeverity>().ok(), Some(MinSeverity::Low));
        assert_eq!("HIGH".parse::<MinSeverity>().ok(), Some(MinSeverity::High));
        assert_eq!("invalid".parse::<MinSeverity>().ok(), None);
    }

    #[test]
    fn test_confidence_override_lookup() {
        let config = PanicbotConfig {
            confidence_overrides: vec![ConfidenceOverride {
                category: "UnsafeCode".to_string(),
                confidence: 0.60,
            }],
            ..Default::default()
        };
        assert_eq!(config.confidence_for("UnsafeCode"), Some(0.60));
        assert_eq!(config.confidence_for("PanicPath"), None);
    }
}

#[cfg(test)]
mod suite_tests {
    use super::*;

    #[test]
    fn default_is_the_full_suite() {
        let allowed = PanicbotConfig::default().allowed_commands;
        for cmd in Subcommand::ALL {
            assert!(
                allowed.allows(cmd),
                "{} must be allowed by default",
                cmd.name()
            );
        }
        assert_eq!(allowed, AllowedCommands::all());
    }

    #[test]
    fn all_lists_each_subcommand_once_and_names_round_trip() {
        let mut seen = std::collections::HashSet::new();
        for cmd in Subcommand::ALL {
            assert!(seen.insert(cmd), "{} listed twice", cmd.name());
            assert_eq!(Subcommand::parse(cmd.name()), Some(cmd));
        }
        assert_eq!(Subcommand::parse("tui"), None);
        assert_eq!(Subcommand::parse("Assail"), None);
    }

    #[test]
    fn set_and_allows_agree_for_every_subcommand() {
        for cmd in Subcommand::ALL {
            let mut a = AllowedCommands::none();
            assert!(!a.allows(cmd));
            a.set(cmd, true);
            assert!(a.allows(cmd));
            for other in Subcommand::ALL.into_iter().filter(|o| *o != cmd) {
                assert!(
                    !a.allows(other),
                    "{} leaked into {}",
                    cmd.name(),
                    other.name()
                );
            }
        }
    }

    #[test]
    fn legacy_static_is_exactly_the_old_default() {
        let a = AllowedCommands::legacy_static();
        let on: Vec<&str> = Subcommand::ALL
            .into_iter()
            .filter(|c| a.allows(*c))
            .map(|c| c.name())
            .collect();
        assert_eq!(on, ["assail", "adjudicate", "diagnostics"]);
    }
}
