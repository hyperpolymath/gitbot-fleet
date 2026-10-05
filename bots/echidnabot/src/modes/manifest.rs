// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Per-repo manifest schema (`schema_version = "2.0"`).
//!
//! Extends the v1.0 directive (which carried only `[bot] mode`) to cover
//! the surfaces required for estate-scale opt-in:
//!
//!   * which provers apply (whitelist / blacklist)
//!   * proof-file globs (include / exclude)
//!   * per-prover timeout and flags
//!   * axiom policy (forbid list + severity)
//!   * merge-block thresholds (confidence + axiom severity)
//!   * blocked-on labels (upstream gating)
//!
//! Canonical path: `.machine_readable/bot_directives/echidnabot.a2ml`.
//! v1.0 directives (mode-only) continue to parse via [`directives::parse_a2ml_directive`]
//! and the resolver cascade — see `directives.rs` for the lookup order.
//!
//! Backwards compatibility: missing fields fall back to documented
//! defaults; unknown fields are ignored so future extensions don't break
//! older `echidnabot` builds reading newer manifests.
//!
//! Estate-side examples live under `tests/fixtures/manifest/`.

use crate::modes::BotMode;
use serde::{Deserialize, Serialize};

/// Top-level repo manifest, parsed from A2ML / TOML.
///
/// Use [`RepoManifest::parse`] to load from a string. All fields are
/// optional at the wire format; defaults match the v1.0 behaviour where
/// possible.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RepoManifest {
    /// Schema version. Currently `"1.0"` (mode-only) or `"2.0"` (extended).
    #[serde(default)]
    pub schema_version: Option<String>,

    #[serde(default)]
    pub bot: BotSection,

    #[serde(default)]
    pub provers: ProversSection,

    #[serde(default)]
    pub proofs: ProofsSection,

    #[serde(default)]
    pub axioms: AxiomsSection,

    #[serde(default)]
    pub merge_block: MergeBlockSection,

    #[serde(default)]
    pub blocked_on: BlockedOnSection,
}

/// `[bot]` table: operating mode and master enable flag.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BotSection {
    /// Operating mode (verifier/advisor/consultant/regulator).
    /// Defaults to `verifier` (matches [`BotMode::default`]).
    #[serde(default)]
    pub mode: Option<BotMode>,

    /// Master switch. When `false`, the bot skips this repo entirely.
    /// Defaults to `true`.
    #[serde(default = "default_true")]
    pub enabled: bool,
}

impl Default for BotSection {
    fn default() -> Self {
        Self {
            mode: None,
            enabled: true,
        }
    }
}

/// `[provers]` table: which provers run for this repo.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ProversSection {
    /// Whitelist. If non-empty, only these provers run.
    /// If empty, every prover whose file globs match is tried.
    #[serde(default)]
    pub enabled: Vec<String>,

    /// Blacklist. These provers never run, even if files match.
    /// Wins over `enabled` on overlap.
    #[serde(default)]
    pub disabled: Vec<String>,

    /// Per-prover overrides, keyed by prover slug (`coq`, `lean4`, ...).
    /// Flattened in TOML as `[provers.coq]`, `[provers.lean4]`, ...
    #[serde(flatten)]
    pub per_prover: std::collections::BTreeMap<String, ProverConfig>,
}

/// Per-prover knobs (`[provers.<slug>]`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ProverConfig {
    /// CLI flags appended to the prover invocation.
    #[serde(default)]
    pub flags: Vec<String>,

    /// Per-prover timeout. If unset, falls back to the daemon default
    /// (`[scheduler] job_timeout_seconds`).
    #[serde(default)]
    pub timeout_seconds: Option<u64>,

    /// Lean4-specific: use `lake build` instead of bare `lean`.
    /// Ignored for non-Lean provers.
    #[serde(default)]
    pub lake: Option<bool>,
}

/// `[proofs]` table: file globs.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ProofsSection {
    /// Glob patterns the bot considers proof-bearing.
    /// If empty, extension-based auto-detection (`.v`, `.lean`, ...) is used.
    #[serde(default)]
    pub include: Vec<String>,

    /// Glob patterns the bot ignores. Wins over `include`.
    #[serde(default)]
    pub exclude: Vec<String>,
}

/// `[axioms]` table: forbidden constructs and severity.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AxiomsSection {
    /// Tokens that, if present in a checked proof, trigger the rule.
    /// Examples: `Admitted`, `admit`, `sorry`, `postulate`, `--type-in-type`.
    #[serde(default)]
    pub forbid: Vec<String>,

    /// Reaction severity. Mapped to [`AxiomSeverity`].
    #[serde(default)]
    pub severity: Option<AxiomSeverity>,
}

/// Axiom-policy severity, ordered low → high.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AxiomSeverity {
    /// Log only; never reported.
    Info,
    /// Reported as a check-run warning.
    #[default]
    Warning,
    /// Reported as a check-run error. Combined with `[merge_block]`
    /// this can gate PR merges.
    Error,
}

/// `[merge_block]` table: gates for Regulator mode.
///
/// Has no effect outside Regulator mode. The thresholds combine with
/// AND semantics: a merge is blocked when **any** configured gate fails.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct MergeBlockSection {
    /// Minimum [`crate::trust`] confidence level (1–5).
    /// Below this, the merge is blocked.
    #[serde(default)]
    pub min_confidence: Option<u8>,

    /// Block when any axiom of this severity (or worse) appears.
    #[serde(default)]
    pub axiom_severity: Option<AxiomSeverity>,
}

/// `[blocked_on]` table: upstream gating labels.
///
/// These are advisory — surfaced in PR comments / check-run summaries so
/// reviewers know that a red signal is upstream-caused.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BlockedOnSection {
    /// Free-form labels. Convention: `blocked-on:<owner>/<repo>#<issue>`.
    #[serde(default)]
    pub labels: Vec<String>,
}

fn default_true() -> bool {
    true
}

impl RepoManifest {
    /// Parse an A2ML / TOML manifest. Returns `None` if the input is not
    /// valid TOML; unknown fields are tolerated.
    pub fn parse(content: &str) -> Option<Self> {
        toml::from_str(content).ok()
    }

    /// True when no field is set — i.e. parsing succeeded but the file
    /// carries no actionable directive.
    pub fn is_empty(&self) -> bool {
        self.bot.mode.is_none()
            && self.bot.enabled
            && self.provers.enabled.is_empty()
            && self.provers.disabled.is_empty()
            && self.provers.per_prover.is_empty()
            && self.proofs.include.is_empty()
            && self.proofs.exclude.is_empty()
            && self.axioms.forbid.is_empty()
            && self.axioms.severity.is_none()
            && self.merge_block.min_confidence.is_none()
            && self.merge_block.axiom_severity.is_none()
            && self.blocked_on.labels.is_empty()
    }

    /// Resolve the effective mode using the manifest's `[bot] mode`
    /// field, falling back to `default`.
    ///
    /// This is the v2.0 mirror of [`crate::modes::directives::parse_a2ml_directive`],
    /// kept separate so v1.0 directives keep their narrow parser.
    pub fn effective_mode(&self, default: BotMode) -> BotMode {
        self.bot.mode.unwrap_or(default)
    }

    /// True when a given prover slug should run for this repo, taking
    /// both `enabled` and `disabled` into account.
    ///
    /// Convention: slugs are lowercase ASCII (`coq`, `lean4`, `agda`,
    /// `isabelle`, `z3`, `cvc5`, `metamath`, `hollight`, `mizar`).
    pub fn prover_runs(&self, slug: &str) -> bool {
        if self.provers.disabled.iter().any(|p| p == slug) {
            return false;
        }
        if self.provers.enabled.is_empty() {
            return true;
        }
        self.provers.enabled.iter().any(|p| p == slug)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_minimal_v1_directive() {
        // A v1.0 directive (mode-only) parses cleanly into v2.0 shape.
        let content = r#"
            schema_version = "1.0"
            [bot]
            mode = "advisor"
        "#;
        let m = RepoManifest::parse(content).expect("parses");
        assert_eq!(m.schema_version.as_deref(), Some("1.0"));
        assert_eq!(m.bot.mode, Some(BotMode::Advisor));
        assert!(m.bot.enabled, "defaults to enabled");
    }

    #[test]
    fn parses_full_v2_directive() {
        let content = r#"
            schema_version = "2.0"

            [bot]
            mode = "regulator"
            enabled = true

            [provers]
            enabled = ["coq", "lean4"]
            disabled = ["agda"]

            [provers.coq]
            flags = ["-R", "formal", "Ephapax"]
            timeout_seconds = 300

            [provers.lean4]
            lake = true
            timeout_seconds = 600

            [proofs]
            include = ["formal/**/*.v", "src/**/*.lean"]
            exclude = ["vendor/**"]

            [axioms]
            forbid = ["Admitted", "sorry", "postulate"]
            severity = "error"

            [merge_block]
            min_confidence = 4
            axiom_severity = "warning"

            [blocked_on]
            labels = ["blocked-on:verisimdb#3"]
        "#;
        let m = RepoManifest::parse(content).expect("parses");
        assert_eq!(m.bot.mode, Some(BotMode::Regulator));
        assert_eq!(m.provers.enabled, vec!["coq", "lean4"]);
        assert_eq!(m.provers.disabled, vec!["agda"]);
        assert_eq!(
            m.provers.per_prover.get("coq").unwrap().timeout_seconds,
            Some(300)
        );
        assert_eq!(m.provers.per_prover.get("lean4").unwrap().lake, Some(true));
        assert_eq!(m.proofs.include.len(), 2);
        assert_eq!(m.axioms.severity, Some(AxiomSeverity::Error));
        assert_eq!(m.merge_block.min_confidence, Some(4));
        assert_eq!(m.merge_block.axiom_severity, Some(AxiomSeverity::Warning));
        assert_eq!(m.blocked_on.labels.len(), 1);
    }

    #[test]
    fn tolerates_unknown_fields() {
        // Future extensions must not break older builds.
        let content = r#"
            schema_version = "2.0"
            future_field = "ignored"
            [bot]
            mode = "advisor"
            [future_section]
            stuff = 42
        "#;
        let m = RepoManifest::parse(content).expect("parses despite unknowns");
        assert_eq!(m.bot.mode, Some(BotMode::Advisor));
    }

    #[test]
    fn empty_manifest_is_empty() {
        let m = RepoManifest::parse("").expect("empty parses");
        assert!(m.is_empty());
    }

    #[test]
    fn prover_runs_whitelist() {
        let content = r#"
            [provers]
            enabled = ["coq", "lean4"]
        "#;
        let m = RepoManifest::parse(content).unwrap();
        assert!(m.prover_runs("coq"));
        assert!(m.prover_runs("lean4"));
        assert!(!m.prover_runs("agda"));
    }

    #[test]
    fn prover_runs_blacklist_wins() {
        let content = r#"
            [provers]
            enabled = ["coq", "agda"]
            disabled = ["agda"]
        "#;
        let m = RepoManifest::parse(content).unwrap();
        assert!(m.prover_runs("coq"));
        assert!(!m.prover_runs("agda"), "disabled wins over enabled");
    }

    #[test]
    fn prover_runs_empty_whitelist_means_all() {
        let content = r#"
            [provers]
            disabled = ["mizar"]
        "#;
        let m = RepoManifest::parse(content).unwrap();
        assert!(m.prover_runs("coq"));
        assert!(m.prover_runs("anything-else"));
        assert!(!m.prover_runs("mizar"));
    }

    #[test]
    fn effective_mode_falls_back_to_default() {
        let content = r#"
            [provers]
            enabled = ["coq"]
        "#;
        let m = RepoManifest::parse(content).unwrap();
        assert_eq!(m.effective_mode(BotMode::Advisor), BotMode::Advisor);
    }

    #[test]
    fn effective_mode_uses_manifest_when_set() {
        let content = r#"
            [bot]
            mode = "regulator"
        "#;
        let m = RepoManifest::parse(content).unwrap();
        assert_eq!(m.effective_mode(BotMode::Advisor), BotMode::Regulator);
    }

    #[test]
    fn axiom_severity_ordering() {
        assert!(AxiomSeverity::Error > AxiomSeverity::Warning);
        assert!(AxiomSeverity::Warning > AxiomSeverity::Info);
    }

    #[test]
    fn parses_disabled_bot() {
        let content = r#"
            [bot]
            enabled = false
        "#;
        let m = RepoManifest::parse(content).unwrap();
        assert!(!m.bot.enabled);
    }

    #[test]
    fn invalid_toml_returns_none() {
        assert!(RepoManifest::parse("this is not toml [[[").is_none());
    }

    #[test]
    fn fixture_ephapax_parses() {
        let content = include_str!("../../tests/fixtures/manifest/ephapax.a2ml");
        let m = RepoManifest::parse(content).expect("ephapax fixture parses");
        assert_eq!(m.bot.mode, Some(BotMode::Regulator));
        assert!(m.prover_runs("coq"));
        assert!(!m.prover_runs("lean4"));
        assert!(!m.proofs.include.is_empty());
    }

    #[test]
    fn fixture_valence_shell_parses() {
        let content = include_str!("../../tests/fixtures/manifest/valence-shell.a2ml");
        let m = RepoManifest::parse(content).expect("valence-shell fixture parses");
        assert_eq!(m.bot.mode, Some(BotMode::Advisor));
        assert!(m.prover_runs("coq"));
        assert!(m.prover_runs("idris2"));
    }
}
