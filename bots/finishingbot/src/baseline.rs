// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Acknowledged findings from the repository's `.hypatia-baseline.json`.
//!
//! The estate's documented exemption mechanism is the per-repo
//! `.hypatia-baseline.json` (`docs/upstream-proposals/standards--hypatia-baseline-consumer/`,
//! `docs/EXEMPTION-MECHANISMS.adoc`, "Layer 2"). `.hypatia-ignore` is an
//! anti-pattern there ("never read by anything"), so finishing-bot does not read
//! it. Format (`docs/HYPATIA-BASELINE-FORMAT.adoc`): a JSON array of entries.
//!
//! ```json
//! { "severity": "high", "rule_module": "finishing_bot", "type": "PH-001",
//!   "file_pattern": "build/just/**", "note": "scanner names the tokens",
//!   "tracking_issue": "owner/repo#1", "expires_at": "2027-06-30" }
//! ```
//!
//! An entry acknowledges a finding iff `severity`, `rule_module` and `type` match
//! EXACTLY and `file` equals (or `file_pattern` globs) the finding's repo-relative
//! path. This bot's `rule_module` is `finishing_bot`; its `type` is the finding id
//! (`PH-001`); entries of other modules (`cicd_rules`, ...) belong to Hypatia and
//! are not read here. `severity` uses the estate words, mapped from this bot's own
//! as the report-back driver does: error = `high`, warning = `medium`,
//! info = `info`, suggestion = `low`.
//!
//! Deliberate limits, so that nothing is silently half-honoured:
//! - matching is exact, so a rule family needs one entry per finding id (and per
//!   severity the id can take); there is no `PH` prefix or `*` wildcard;
//! - an entry past its `expires_at` (or with an unparseable one) acknowledges
//!   nothing, which makes the finding reappear;
//! - an entry with `severity_override` is not applied: this bot does not downgrade
//!   findings, so the finding stays visible at its own severity;
//! - a baseline that is not a JSON array, or an entry that is malformed, exempts
//!   nothing and is logged.

use crate::analyzers::Severity;
use chrono::NaiveDate;
use serde_json::Value;
use std::path::Path;
use tracing::warn;

/// The `rule_module` under which finishing-bot's rules are addressed.
pub const MODULE: &str = "finishing_bot";

/// The baseline file name, at the repository root.
pub const FILE_NAME: &str = ".hypatia-baseline.json";

#[derive(Debug, Default, Clone)]
pub struct Baseline {
    entries: Vec<Entry>,
}

#[derive(Debug, Clone)]
struct Entry {
    severity: String,
    rule: String,
    target: Target,
}

#[derive(Debug, Clone)]
enum Target {
    File(String),
    Pattern(glob::Pattern),
}

/// The estate severity word for one of this bot's severities.
fn estate_severity(s: Severity) -> &'static str {
    match s {
        Severity::Error => "high",
        Severity::Warning => "medium",
        Severity::Info => "info",
        Severity::Suggestion => "low",
    }
}

fn text_of<'a>(entry: &'a Value, key: &str) -> Option<&'a str> {
    entry.get(key).and_then(Value::as_str)
}

impl Baseline {
    /// Load `<root>/.hypatia-baseline.json`; an absent or unreadable file
    /// acknowledges nothing.
    pub fn load(root: &Path) -> Self {
        match std::fs::read_to_string(root.join(FILE_NAME)) {
            Ok(t) => Self::parse(&t, chrono::Utc::now().date_naive()),
            Err(_) => Self::default(),
        }
    }

    /// Parse baseline text as of `today` (entries expire after their date).
    pub fn parse(text: &str, today: NaiveDate) -> Self {
        let value: Value = match serde_json::from_str(text) {
            Ok(v) => v,
            Err(e) => {
                warn!("{FILE_NAME} is not valid JSON ({e}); acknowledging nothing");
                return Self::default();
            }
        };
        let Some(array) = value.as_array() else {
            warn!("{FILE_NAME} is not a JSON array; acknowledging nothing");
            return Self::default();
        };
        let mut entries = Vec::new();
        for entry in array {
            if text_of(entry, "rule_module") != Some(MODULE) {
                continue; // Hypatia's own entry (or another module's), not ours
            }
            if entry.get("severity_override").is_some() {
                warn!("{FILE_NAME}: severity_override is not supported by finishing-bot; entry not applied");
                continue;
            }
            if let Some(expires) = entry.get("expires_at") {
                match expires
                    .as_str()
                    .and_then(|d| NaiveDate::parse_from_str(d, "%Y-%m-%d").ok())
                {
                    Some(date) if date >= today => {}
                    Some(_) => continue, // expired: the finding reappears
                    None => {
                        warn!("{FILE_NAME}: unparseable expires_at; entry not applied");
                        continue;
                    }
                }
            }
            let (Some(severity), Some(rule)) = (text_of(entry, "severity"), text_of(entry, "type"))
            else {
                warn!("{FILE_NAME}: entry lacks severity or type; not applied");
                continue;
            };
            let target = match (text_of(entry, "file"), text_of(entry, "file_pattern")) {
                (Some(file), None) => Target::File(file.to_string()),
                (None, Some(pattern)) => match glob::Pattern::new(pattern) {
                    Ok(p) => Target::Pattern(p),
                    Err(e) => {
                        warn!("{FILE_NAME}: bad file_pattern {pattern:?} ({e}); not applied");
                        continue;
                    }
                },
                _ => {
                    warn!(
                        "{FILE_NAME}: entry needs exactly one of file or file_pattern; not applied"
                    );
                    continue;
                }
            };
            entries.push(Entry {
                severity: severity.to_string(),
                rule: rule.to_string(),
                target,
            });
        }
        Self { entries }
    }

    /// Does an entry acknowledge finding `rule_id` of `severity` at repo-relative
    /// path `rel`?
    pub fn acknowledges(&self, rule_id: &str, severity: Severity, rel: &str) -> bool {
        let rel = rel.strip_prefix("./").unwrap_or(rel);
        let severity = estate_severity(severity);
        let options = glob::MatchOptions {
            case_sensitive: true,
            require_literal_separator: true,
            require_literal_leading_dot: false,
        };
        self.entries.iter().any(|e| {
            e.severity == severity
                && e.rule == rule_id
                && match &e.target {
                    Target::File(f) => f == rel,
                    Target::Pattern(p) => p.matches_with(rel, options),
                }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn day(s: &str) -> NaiveDate {
        NaiveDate::parse_from_str(s, "%Y-%m-%d").unwrap()
    }

    const FILE: &str = r#"[
      {"severity":"critical","rule_module":"cicd_rules","type":"banned_language_file","file":"a.py"},
      {"severity":"high","rule_module":"finishing_bot","type":"PH-001","file_pattern":"build/just/**"},
      {"severity":"medium","rule_module":"finishing_bot","type":"PH-002","file":"Justfile","note":"why"},
      {"severity":"high","rule_module":"finishing_bot","type":"REL-003","file_pattern":"scripts/check-*.sh"}
    ]"#;

    #[test]
    fn exact_rule_severity_and_path_or_pattern_are_acknowledged() {
        let b = Baseline::parse(FILE, day("2026-11-13"));
        assert!(b.acknowledges("PH-001", Severity::Error, "build/just/a.just"));
        assert!(b.acknowledges("PH-001", Severity::Error, "./build/just/deep/a.just"));
        assert!(b.acknowledges("PH-002", Severity::Warning, "Justfile"));
        assert!(b.acknowledges("REL-003", Severity::Error, "scripts/check-x.sh"));
    }

    #[test]
    fn nothing_but_an_exact_match_is_acknowledged() {
        let b = Baseline::parse(FILE, day("2026-11-13"));
        assert!(!b.acknowledges("PH-001", Severity::Error, "src/main.rs")); // other path
        assert!(!b.acknowledges("PH-001", Severity::Warning, "build/just/a.just")); // severity
        assert!(!b.acknowledges("PH-003", Severity::Error, "build/just/a.just")); // exact id
        assert!(!b.acknowledges("PH", Severity::Error, "build/just/a.just")); // no prefix form
        assert!(!b.acknowledges("PH-002", Severity::Warning, "sub/Justfile")); // exact file
        assert!(!b.acknowledges("REL-003", Severity::Error, "scripts/sub/check-x.sh")); // * stops at /
        assert!(!b.acknowledges("banned_language_file", Severity::Error, "a.py"));
        // Hypatia's entry
    }

    #[test]
    fn expired_and_unsupported_entries_acknowledge_nothing() {
        let text = r#"[
          {"severity":"high","rule_module":"finishing_bot","type":"PH-001","file":"a","expires_at":"2026-11-12"},
          {"severity":"high","rule_module":"finishing_bot","type":"PH-002","file":"a","expires_at":"2026-11-13"},
          {"severity":"high","rule_module":"finishing_bot","type":"PH-003","file":"a","expires_at":"soon"},
          {"severity":"high","rule_module":"finishing_bot","type":"PH-004","file":"a","severity_override":"advisory"},
          {"severity":"high","rule_module":"finishing_bot","type":"PH-005"},
          {"severity":"high","rule_module":"finishing_bot","type":"PH-006","file":"a","file_pattern":"*"}
        ]"#;
        let b = Baseline::parse(text, day("2026-11-13"));
        assert!(!b.acknowledges("PH-001", Severity::Error, "a"), "expired");
        assert!(
            b.acknowledges("PH-002", Severity::Error, "a"),
            "last valid day"
        );
        for id in ["PH-003", "PH-004", "PH-005", "PH-006"] {
            assert!(!b.acknowledges(id, Severity::Error, "a"), "{id}");
        }
    }

    #[test]
    fn malformed_or_absent_baseline_acknowledges_nothing() {
        for text in ["", "{", "{}", "\"x\"", "[1, null]"] {
            let b = Baseline::parse(text, day("2026-11-13"));
            assert!(!b.acknowledges("PH-001", Severity::Error, "a"), "{text}");
        }
        let d = tempfile::TempDir::new().unwrap();
        assert!(!Baseline::load(d.path()).acknowledges("PH-001", Severity::Error, "a"));
    }

    #[test]
    fn estate_severity_words_follow_the_report_back_mapping() {
        assert_eq!(estate_severity(Severity::Error), "high");
        assert_eq!(estate_severity(Severity::Warning), "medium");
        assert_eq!(estate_severity(Severity::Info), "info");
        assert_eq!(estate_severity(Severity::Suggestion), "low");
    }
}
