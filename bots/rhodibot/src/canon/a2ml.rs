// SPDX-License-Identifier: MPL-2.0

//! Any `.a2ml` file is a priority-1 finding.
//!
//! A2ML is retired (owner rulings D99, D269c, D312, D313): no `.a2ml` file
//! should exist anywhere in a repository. Rhodibot used to *expect* some of
//! them; now every tracked `*.a2ml` is reported as [`Severity::Retired`], the
//! worst a canon report holds, one finding per file so a grid can place each
//! one against a change.
//!
//! Each finding names the form the file's facts belong in, from the standards
//! mapping specs (`metadatastician/standards` `1-formats/deed/mappings/`), and
//! says how it gets there: conversion is kyX's front end (D313), never a hand
//! conversion, and a family with no mapping spec waits for one. Rhodibot only
//! reports; the fleet's `fix-a2ml-via-kyx.sh` is what tries the conversion.
//!
//! This is the report, not the read path: the legacy read fallback in
//! [`super::overlay`] (a legacy file read as "migrate" when its deed form is
//! absent) is unchanged.

use super::report::Finding;
use super::verdict::Severity;

/// The finding id. Not a canon criterion: a rhodibot rule on top of it.
pub const NO_A2ML_ID: &str = "rhodibot.no-a2ml";

/// Where one A2ML family's facts go, and whether kyX can take them there.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Destination {
    /// The deed form, as a report names it.
    pub form: &'static str,
    /// The mapping spec that decides it, or `None` when there is none yet.
    pub spec: Option<&'static str>,
    /// Whether kyX has a front end for the family today.
    pub kyx: bool,
}

/// The family a path belongs to, by its file name (and, for bot directives and
/// contractiles, its directory).
pub fn destination(path: &str) -> Destination {
    let lower = path.to_ascii_lowercase();
    let name = lower.rsplit('/').next().unwrap_or(&lower);
    let d = |form, spec, kyx| Destination { form, spec, kyx };
    match name {
        "ecosystem.a2ml" => d(
            ".machine_readable/descriptiles/ECOSYSTEM.deed, or the chora (ecosystem ...) clause",
            Some("meta-ecosystem-to-repo-deed.adoc"),
            true,
        ),
        "meta.a2ml" => d(
            ".machine_readable/descriptiles/META.deed, or the chora (meta ...) clause",
            Some("meta-ecosystem-to-repo-deed.adoc"),
            false,
        ),
        "state.a2ml" => d(
            ".machine_readable/descriptiles/STATE.deed, or the chora (status ...) clause",
            Some("state-v1-decision.adoc"),
            false,
        ),
        "agentic.a2ml" | "neurosym.a2ml" | "playbook.a2ml" => d(
            "the chora (agentic ...) / (neurosym ...) / (playbook ...) clause",
            Some("agentic-neurosym-playbook-to-repo-deed.adoc"),
            false,
        ),
        "clade.a2ml" => d(
            "the chora (identity ...) (clade ...) (forges ...) (lineage ...) clauses",
            Some("clade-to-repo-deed.adoc"),
            false,
        ),
        "0-ai-manifest.a2ml" => d(
            "the chora (manifest ...) clause",
            Some("ai-manifest-decision.adoc"),
            false,
        ),
        "anchor.a2ml" => d("the chora (anchor ...) clause", None, false),
        "rsr-profile.a2ml" => d("the chora (rsr-profile ...) clause", None, false),
        "rsr-criteria-v2.a2ml" => d(
            "a criteria praxis deed",
            Some("rsr-criteria-to-praxis-deed.adoc"),
            false,
        ),
        _ if lower.contains("bot_directives/") => {
            d("the chora (bot-directives ...) clause", None, false)
        }
        _ if name.ends_with("file.a2ml") => d(
            "a contractile deed (<Verb>file.deed) attesting its .k9.ncl contract",
            None,
            false,
        ),
        _ => d("a deed form a mapping spec has yet to decide", None, false),
    }
}

/// Whether a tracked path is an A2ML file.
pub fn is_a2ml(path: &str) -> bool {
    path.to_ascii_lowercase().ends_with(".a2ml")
}

/// One [`Severity::Retired`] finding per `.a2ml` file, in path order.
pub fn findings(files: &[String]) -> Vec<Finding> {
    let mut paths: Vec<&String> = files.iter().filter(|p| is_a2ml(p)).collect();
    paths.sort();
    paths.dedup();
    paths
        .into_iter()
        .map(|path| {
            let dest = destination(path);
            let how = match (dest.spec, dest.kyx) {
                (Some(spec), true) => format!(
                    "convert it with kyX under {spec} (D313); a refusal is a P1 issue, never a \
                     hand conversion"
                ),
                (Some(spec), false) => format!(
                    "{spec} decides it, but kyX has no front end for it yet: it waits for kyX \
                     (D313); do not hand-convert"
                ),
                (None, _) => "no mapping spec decides it yet: it waits for one and for kyX \
                              (D313); do not hand-convert"
                    .to_string(),
            };
            Finding {
                id: NO_A2ML_ID.to_string(),
                name: "A2ML is retired (D99, D269c, D312, D313)".to_string(),
                tier: "P1".to_string(),
                severity: Severity::Retired,
                canon_keeps_it_at: None,
                expected: vec![dest.form.to_string()],
                found: vec![path.clone()],
                deprecated_copies: Vec::new(),
                details: vec![format!(
                    "{path}: retired A2ML; its facts belong in {}; {how}",
                    dest.form
                )],
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn files(paths: &[&str]) -> Vec<String> {
        paths.iter().map(|p| p.to_string()).collect()
    }

    #[test]
    fn every_a2ml_is_one_retired_finding_and_nothing_else_is() {
        let got = findings(&files(&[
            "README.adoc",
            ".machine_readable/descriptiles/STATE.a2ml",
            "0-AI-MANIFEST.a2ml",
            "x_chora.deed",
            "deep/nested/Thing.A2ML",
            ".machine_readable/descriptiles/STATE.a2ml",
        ]));
        let paths: Vec<&str> = got.iter().map(|f| f.found[0].as_str()).collect();
        assert_eq!(
            paths,
            [
                ".machine_readable/descriptiles/STATE.a2ml",
                "0-AI-MANIFEST.a2ml",
                "deep/nested/Thing.A2ML"
            ]
        );
        for f in &got {
            assert_eq!(f.severity, Severity::Retired);
            assert_eq!(f.id, NO_A2ML_ID);
            assert_eq!(f.tier, "P1");
            // Never asks for an .a2ml, never suggests a hand conversion.
            assert!(f.expected.iter().all(|e| !e.ends_with(".a2ml")), "{f:?}");
            assert!(f.details[0].contains("D313"), "{f:?}");
        }
    }

    #[test]
    fn only_ecosystem_is_routed_to_kyx_today() {
        assert!(destination(".machine_readable/descriptiles/ECOSYSTEM.a2ml").kyx);
        assert!(destination(".machine_readable/6a2/ECOSYSTEM.a2ml").kyx);
        for path in [
            "STATE.a2ml",
            "META.a2ml",
            "AGENTIC.a2ml",
            "CLADE.a2ml",
            "0-AI-MANIFEST.a2ml",
            ".machine_readable/bot_directives/coverage.a2ml",
            "contractiles/must/Mustfile.a2ml",
            "audits/assail-classifications.a2ml",
        ] {
            assert!(!destination(path).kyx, "{path}");
        }
    }

    #[test]
    fn families_name_their_deed_form() {
        assert!(
            destination("x/bot_directives/debt.a2ml")
                .form
                .contains("(bot-directives")
        );
        assert!(
            destination("contractiles/trust/Trustfile.a2ml")
                .form
                .contains("contractile deed")
        );
        assert!(destination("STATE.a2ml").form.contains("STATE.deed"));
        assert_eq!(destination("odd.a2ml").spec, None);
    }

    #[test]
    fn a_finding_says_ecosystem_goes_through_kyx_and_others_wait() {
        let got = findings(&files(&["ECOSYSTEM.a2ml", "META.a2ml", "odd.a2ml"]));
        assert!(
            got[0].details[0].contains("convert it with kyX"),
            "{:?}",
            got[0]
        );
        assert!(got[1].details[0].contains("waits for kyX"), "{:?}", got[1]);
        assert!(
            got[2].details[0].contains("no mapping spec"),
            "{:?}",
            got[2]
        );
    }
}
