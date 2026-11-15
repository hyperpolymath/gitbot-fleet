// SPDX-License-Identifier: MPL-2.0

//! Rhodibot's overlay on the vendored criteria canon.
//!
//! The vendored `rsr-criteria-v2.a2ml` still names `.a2ml` files for the
//! machine-readable repo facts, and `CONTRIBUTING.md` / `CODE_OF_CONDUCT.md`
//! for the community documents. The estate has retired A2ML (those facts are
//! clauses of `<repo>_chora.deed`), and the community documents are almost
//! always `.adoc`. Converting the canon itself to a deed is not this crate's
//! to do -- that is an A7 mapping-spec item with a parity gate -- so the canon
//! is left byte-for-byte as it is, its digest stays honest, and this module
//! re-targets the affected criteria **as requirements**. The overlay is
//! recorded in `canon/pin.toml` under `[overlay]`, and a lockstep test fails
//! when the two disagree. When the canon gains its deed form, this module is
//! deleted in the same commit that re-pins.
//!
//! # What changes
//!
//! | criterion | now satisfied by |
//! |---|---|
//! | 2.1.4 `coc-md`, 2.1.5 `contributing-md` | `.adoc` **or** `.md`, never both required; the repo's `(bot :name rhodibot (config :community-doc-format ...))` only picks which one a report names first |
//! | 2.3.1 `ai-manifest` | the chora deed's `(manifest ...)` clause |
//! | 3.1.2-3.1.5 state, meta, ecosystem, agentic | the descriptile `.deed`, or (fallback) the chora deed's clause of that name; state has no clause of its own: `(status ...)` or `(maturity ...)` |
//! | 3.1.6-3.1.8 neurosym, playbook, anchor | the chora deed's clause of that name |
//! | 3.2.2 `rsr-profile` | the chora deed's `(rsr-profile ...)` clause |
//! | 3.1.9 `clade`, 3.2.1 `a2ml-valid` | suspended: not scored as file questions |
//!
//! Plus one check the canon does not carry: the chora deed itself must exist.
//!
//! # Legacy `.a2ml` is a read fallback, never a demand
//!
//! A repository that has not migrated yet still carries the old file. Where
//! the new form is absent and the old file is present, the verdict is
//! `elsewhere` ("present at X -- not Y"), i.e. *migrate*, and the expected
//! form it names is always the deed. No message here asks for an `.a2ml`.

use std::collections::BTreeSet;

use super::requirement::Requirement;
use super::{Criterion, Tier};
use crate::deed::{Deed, DocFormat};

/// The criteria the overlay re-targets, for `pin.toml` to agree with.
pub const RETARGETED: &[&str] = &[
    "2.1.4", "2.1.5", "2.3.1", "3.1.2", "3.1.3", "3.1.4", "3.1.5", "3.1.6", "3.1.7", "3.1.8",
    "3.2.2",
];

/// Criteria the overlay stops scoring as file questions.
///
/// 3.1.9 asks for `CLADE.a2ml`, and the deed form of CLADE has not landed (A7
/// M3), so there is nothing current to ask for. 3.2.1 is already not a file
/// question; it is listed so the intent is recorded rather than accidental.
pub const SUSPENDED: &[&str] = &["3.1.9", "3.2.1"];

/// The id of the check this overlay adds.
pub const CHORA_DEED_ID: &str = "rhodibot.chora-deed";

/// What a criterion is checked against instead of its own description.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Replacement {
    pub requirement: Requirement,
    /// Where a report says the canon keeps it. `-` for none: the canon's own
    /// answer is the retired one, so it is never quoted.
    pub template_ref: String,
    /// Whether the criterion's own, original requirement is consulted as a
    /// read fallback when the new form is absent.
    pub legacy_fallback: bool,
}

/// The overlay for one repository.
#[derive(Debug, Clone)]
pub struct Overlay {
    chora_name: String,
    format: DocFormat,
    clauses: BTreeSet<String>,
}

impl Overlay {
    /// `chora_name` is the root file name of the repository's chora deed, the
    /// one that is expected even when it is missing (`<repo>_chora.deed`).
    pub fn new(chora_name: &str, deed: Option<&Deed>, format: DocFormat) -> Self {
        Self {
            chora_name: chora_name.to_string(),
            format,
            clauses: deed.map(Deed::clauses).unwrap_or_default(),
        }
    }

    pub fn chora_name(&self) -> &str {
        &self.chora_name
    }

    /// One virtual path per top-level clause the deed carries.
    ///
    /// A clause is not a file, but the verdict machinery answers "is this
    /// there?" over paths. `<chora deed>#meta` is a path no file can have, so
    /// it cannot be satisfied by accident, and it reads as what it is in a
    /// report.
    pub fn virtual_files(&self) -> Vec<String> {
        self.clauses
            .iter()
            .map(|clause| clause_path(&self.chora_name, clause))
            .collect()
    }

    pub fn is_suspended(id: &str) -> bool {
        SUSPENDED.contains(&id)
    }

    /// The replacement requirement for a criterion, when it is re-targeted.
    pub fn replacement(&self, criterion: &Criterion) -> Option<Replacement> {
        let clause = |name: &str| clause_path(&self.chora_name, name);
        let descriptile = |name: &str| format!(".machine_readable/descriptiles/{name}.deed");
        let doc = |stem: &str| self.doc_alternatives(stem);

        let (groups, legacy_fallback): (Vec<Vec<String>>, bool) = match criterion.id.as_str() {
            "2.1.4" => (vec![doc("CODE_OF_CONDUCT")], false),
            "2.1.5" => (vec![doc("CONTRIBUTING")], false),
            "2.3.1" => (vec![vec![clause("manifest")]], true),
            "3.1.2" => (
                vec![vec![
                    descriptile("STATE"),
                    clause("status"),
                    clause("maturity"),
                ]],
                true,
            ),
            "3.1.3" => (vec![vec![descriptile("META"), clause("meta")]], true),
            "3.1.4" => (
                vec![vec![descriptile("ECOSYSTEM"), clause("ecosystem")]],
                true,
            ),
            "3.1.5" => (vec![vec![descriptile("AGENTIC"), clause("agentic")]], true),
            "3.1.6" => (vec![vec![clause("neurosym")]], true),
            "3.1.7" => (vec![vec![clause("playbook")]], true),
            "3.1.8" => (vec![vec![clause("anchor")]], true),
            "3.2.2" => (vec![vec![clause("rsr-profile")]], true),
            _ => return None,
        };

        Some(Replacement {
            requirement: Requirement { all_of: groups },
            template_ref: "-".to_string(),
            legacy_fallback,
        })
    }

    /// `CONTRIBUTING.adoc` and `CONTRIBUTING.md`, the preferred spelling first.
    ///
    /// One group, so either satisfies it and neither is ever required in
    /// addition to the other. GitHub honours both the root and `.github/`.
    fn doc_alternatives(&self, stem: &str) -> Vec<String> {
        let first = self.format.extension();
        let second = self.format.other().extension();
        vec![
            format!("{stem}.{first}"),
            format!("{stem}.{second}"),
            format!(".github/{stem}.{first}"),
            format!(".github/{stem}.{second}"),
        ]
    }

    /// Checks the canon does not carry: the chora deed has to exist.
    pub fn extra_checks(&self) -> Vec<(Criterion, Replacement)> {
        let criterion = Criterion {
            id: CHORA_DEED_ID.to_string(),
            name: "chora-deed".to_string(),
            desc: format!(
                "{} -- the repository's identity and facts (manifest, meta, ecosystem, agentic, \
                 neurosym, playbook, anchor, rsr-profile, bot-directives)",
                self.chora_name
            ),
            tier: Tier::Silver,
            gate: "universal".to_string(),
            detect: "rhodibot/chora-deed".to_string(),
            template_ref: "-".to_string(),
        };
        let replacement = Replacement {
            requirement: Requirement {
                all_of: vec![vec![self.chora_name.clone()]],
            },
            template_ref: "-".to_string(),
            legacy_fallback: false,
        };
        vec![(criterion, replacement)]
    }
}

/// The virtual path of one clause of the chora deed.
pub fn clause_path(chora_name: &str, clause: &str) -> String {
    format!("{chora_name}#{clause}")
}

/// Pick the chora deed out of a file list: the root file ending `_chora.deed`.
///
/// More than one is an error, because the estate has one per repository.
pub fn find_chora_deed(files: &[String]) -> anyhow::Result<Option<String>> {
    let mut found: Vec<&String> = files
        .iter()
        .filter(|file| !file.contains('/') && file.ends_with("_chora.deed"))
        .collect();
    found.sort();
    anyhow::ensure!(
        found.len() <= 1,
        "more than one chora deed at the root ({}); a repository has exactly one",
        found
            .iter()
            .map(|file| file.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    );
    Ok(found.first().map(|file| (*file).clone()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::canon::Canon;

    fn criterion(id: &str) -> Criterion {
        Canon::vendored()
            .expect("canon")
            .criteria()
            .find(|criterion| criterion.id == id)
            .expect("criterion")
            .clone()
    }

    #[test]
    fn no_replacement_names_an_a2ml_file() {
        let overlay = Overlay::new("x_chora.deed", None, DocFormat::Adoc);
        for id in RETARGETED {
            let replacement = overlay
                .replacement(&criterion(id))
                .unwrap_or_else(|| panic!("{id} is re-targeted"));
            for path in replacement.requirement.paths() {
                assert!(!path.ends_with(".a2ml"), "{id} still demands {path}");
            }
        }
    }

    #[test]
    fn every_retargeted_id_has_a_replacement_and_nothing_else_does() {
        let overlay = Overlay::new("x_chora.deed", None, DocFormat::Adoc);
        let canon = Canon::vendored().unwrap();
        for criterion in canon.criteria() {
            assert_eq!(
                overlay.replacement(criterion).is_some(),
                RETARGETED.contains(&criterion.id.as_str()),
                "{}",
                criterion.id
            );
        }
    }

    #[test]
    fn the_default_prefers_adoc_and_the_override_prefers_md() {
        let adoc = Overlay::new("x_chora.deed", None, DocFormat::Adoc);
        let md = Overlay::new("x_chora.deed", None, DocFormat::Md);
        let contributing = criterion("2.1.5");
        let first = |overlay: &Overlay| {
            overlay
                .replacement(&contributing)
                .unwrap()
                .requirement
                .all_of[0][0]
                .clone()
        };
        assert_eq!(first(&adoc), "CONTRIBUTING.adoc");
        assert_eq!(first(&md), "CONTRIBUTING.md");
    }

    #[test]
    fn a_document_is_one_group_so_both_forms_are_never_required() {
        let overlay = Overlay::new("x_chora.deed", None, DocFormat::Adoc);
        let replacement = overlay.replacement(&criterion("2.1.4")).unwrap();
        assert_eq!(replacement.requirement.all_of.len(), 1);
    }

    #[test]
    fn virtual_files_come_from_the_deed_clauses() {
        let deed = Deed::parse("(repo-deed (meta :version \"1\") (anchor :id \"a\"))").unwrap();
        let overlay = Overlay::new("x_chora.deed", Some(&deed), DocFormat::Adoc);
        assert_eq!(
            overlay.virtual_files(),
            vec![
                "x_chora.deed#anchor".to_string(),
                "x_chora.deed#meta".to_string()
            ]
        );
    }

    #[test]
    fn the_chora_deed_is_found_at_the_root_only() {
        let files = |paths: &[&str]| paths.iter().map(|p| p.to_string()).collect::<Vec<_>>();
        assert_eq!(
            find_chora_deed(&files(&["a_chora.deed", "README.adoc"])).unwrap(),
            Some("a_chora.deed".to_string())
        );
        assert_eq!(
            find_chora_deed(&files(&["sub/a_chora.deed"])).unwrap(),
            None
        );
        assert!(find_chora_deed(&files(&["a_chora.deed", "b_chora.deed"])).is_err());
    }
}
