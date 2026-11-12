// SPDX-License-Identifier: MPL-2.0
//! Translate pons findings into the fleet's finding shape, carrying the tier
//! and the evidence class.
//!
//! Owner ruling D1 (2026-11-12): SPECULATIVE findings are carried, never
//! dropped, and the grid must never grade one as blocking. The grid reads only
//! `severity`, so a SPECULATIVE (or unrecognised) evidence class is capped at
//! `info` here; the original severity stays in `pons_severity`. Every other
//! pons `ERROR` keeps its blocking meaning (`error`), `WARN` is `warning`.
//! A heuristic is labelled as one in the message: it must never read as a
//! proof (pons ADR-0004).

use serde::Serialize;

/// A pons finding after validation (see `scanner`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PonsFinding {
    pub rule_id: String,
    pub severity: String, // INFO | WARN | ERROR, upper-cased
    pub tier: String,
    pub evidence: String, // HEURISTIC | DATAFLOW | PROTOCOL | SPECULATIVE (pons' words)
    pub file: String,
    pub line: u64,
    pub message: String,
    pub evidence_note: Option<String>,
    pub counter_condition: Option<String>,
}

/// One finding as printed on stdout.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct Finding {
    pub rule_id: String,
    /// Fleet word the grid grades: `error`, `warning` or `info`.
    pub severity: String,
    /// pons' own severity, unchanged (`ERROR`, `WARN`, `INFO`).
    pub pons_severity: String,
    pub tier: String,
    pub evidence_class: String,
    pub file: String,
    pub line: u64,
    pub message: String,
    /// True only when the finding may block: pons `ERROR` with a graded class.
    pub blocking: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub evidence_note: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub counter_condition: Option<String>,
}

/// Evidence classes the grid may grade at their mapped severity.
fn is_graded_class(evidence: &str) -> bool {
    matches!(evidence, "HEURISTIC" | "DATAFLOW" | "PROTOCOL")
}

/// Translate one finding. Unknown evidence classes are treated like
/// SPECULATIVE (carried, capped at `info`): an unrecognised class must not be
/// able to block, and must not be dropped.
pub fn translate(f: &PonsFinding) -> Finding {
    let graded = is_graded_class(&f.evidence);
    let mapped = match f.severity.as_str() {
        "ERROR" => "error",
        "WARN" => "warning",
        _ => "info",
    };
    let severity = if graded { mapped } else { "info" };
    let label = match f.evidence.as_str() {
        "HEURISTIC" => format!("{}, {}; a heuristic, not proof", f.tier, f.evidence),
        "SPECULATIVE" => format!(
            "{}, {}; carried, never graded as blocking",
            f.tier, f.evidence
        ),
        e if !graded => format!(
            "{}, {}; unrecognised evidence class, never graded as blocking",
            f.tier, e
        ),
        _ => format!("{}, {}", f.tier, f.evidence),
    };
    Finding {
        rule_id: f.rule_id.clone(),
        severity: severity.to_string(),
        pons_severity: f.severity.clone(),
        tier: f.tier.clone(),
        evidence_class: f.evidence.clone(),
        file: f.file.clone(),
        line: f.line,
        message: format!("{} ({})", f.message, label),
        blocking: graded && severity == "error",
        evidence_note: f.evidence_note.clone(),
        counter_condition: f.counter_condition.clone(),
    }
}

pub fn translate_all(fs: &[PonsFinding]) -> Vec<Finding> {
    fs.iter().map(translate).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pf(sev: &str, ev: &str) -> PonsFinding {
        PonsFinding {
            rule_id: "r".into(),
            severity: sev.into(),
            tier: "T0".into(),
            evidence: ev.into(),
            file: "a.rs".into(),
            line: 3,
            message: "m".into(),
            evidence_note: None,
            counter_condition: None,
        }
    }

    #[test]
    fn error_keeps_its_blocking_meaning() {
        let f = translate(&pf("ERROR", "DATAFLOW"));
        assert_eq!((f.severity.as_str(), f.blocking), ("error", true));
    }

    #[test]
    fn warn_and_info_do_not_block() {
        assert_eq!(translate(&pf("WARN", "HEURISTIC")).severity, "warning");
        assert_eq!(translate(&pf("INFO", "PROTOCOL")).severity, "info");
        assert!(!translate(&pf("WARN", "HEURISTIC")).blocking);
    }

    #[test]
    fn speculative_is_carried_never_blocking() {
        let f = translate(&pf("ERROR", "SPECULATIVE"));
        assert_eq!(f.severity, "info");
        assert_eq!(f.pons_severity, "ERROR");
        assert_eq!(f.evidence_class, "SPECULATIVE");
        assert!(!f.blocking);
        assert!(f.message.contains("never graded as blocking"));
    }

    #[test]
    fn unknown_evidence_class_cannot_block() {
        let f = translate(&pf("ERROR", "MYSTERY"));
        assert_eq!((f.severity.as_str(), f.blocking), ("info", false));
        assert_eq!(f.evidence_class, "MYSTERY");
    }

    #[test]
    fn heuristic_is_labelled_as_one() {
        assert!(translate(&pf("WARN", "HEURISTIC"))
            .message
            .contains("a heuristic, not proof"));
    }
}
