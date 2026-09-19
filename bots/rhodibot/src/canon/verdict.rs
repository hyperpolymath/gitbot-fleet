// SPDX-License-Identifier: MPL-2.0

//! What a check found, and what it is worth.
//!
//! A criterion that names files has four possible answers, and they are not
//! interchangeable:
//!
//! | verdict | meaning | worth |
//! |---|---|---|
//! | present at the path | the file is where the canon records it | nothing to do |
//! | present under a deprecated location | the file exists, but in a place the canon has retired | move it |
//! | present elsewhere | the file exists in neither place | check it |
//! | absent | the file is not there | add it |
//!
//! # The rule this module exists to hold
//!
//! **The description decides what is required. `template_ref` only says where
//! the canon's own template keeps it.**
//!
//! The pilot read `template_ref` as the requirement and reported repositories
//! that satisfied the criterion in another reasonable place as missing it:
//! criterion 1.2.2 asks for a `.pre-commit-config.yaml` with real hooks, the
//! template keeps its copy under `ci/`, and a repository with one at the root
//! was failed. Both of that pilot's false positives were this one mistake.
//!
//! So a bare filename is satisfied in one of exactly two places: **the
//! repository root**, or **the location the canon records for it**. Everywhere
//! else it is `Elsewhere` -- a finding, with the canon's location as a note.
//!
//! That second rule came out of running this against real trees. "Anywhere"
//! was the first attempt, and it passed `1.2.4`, which asks for
//! `.tool-versions`, on a copy living at
//! `robot-repo-automaton/templates/skeleton/.tool-versions` -- another
//! project's template skeleton, not this repository's file. Restricting to the
//! root alone would have been too strict the other way: it would have failed
//! the `.machine_readable/descriptiles/*.a2ml` files that satisfy 3.1.2-3.1.7
//! exactly where the canon keeps them.
//!
//! When the description gives a path -- ".well-known/security.txt" -- the
//! location is part of the requirement and a copy somewhere else is
//! `Elsewhere`.
//!
//! # Deprecated locations
//!
//! The canon states that `.machine_readable/6a2/` is deprecated, but it states
//! it in prose, inside criterion 3.1.1's description: "NOT 6a2/, which is
//! deprecated 2026-06-30". There is no machine-readable deprecation table, so
//! this module reads that sentence rather than hardcoding its conclusion. A
//! lockstep test asserts the sentence is still there and still parsed, and that
//! a canon which stops saying it fails the test instead of quietly reclassifying
//! every retired path as ordinary.

use anyhow::{Result, bail, ensure};
use serde::Serialize;

use super::Canon;
use super::Criterion;
use super::requirement::requirement_from;

/// A location the canon has retired, and the sentence that says so.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Deprecation {
    /// The retired location, resolved to a full path: `.machine_readable/6a2/`.
    pub location: String,
    /// When it was retired, as the canon records it: `2026-06-30`.
    pub since: String,
    /// Which criterion states it: `3.1.1`.
    pub stated_by: String,
    /// The sentence, so a report can quote the canon rather than paraphrase it.
    pub quote: String,
}

impl Deprecation {
    /// Read the canon's retirements out of its own descriptions.
    ///
    /// The shape being read is `NOT <location>, which is deprecated <date>`,
    /// relative to the criterion's own `template_ref`: 3.1.1 is about
    /// `.machine_readable/descriptiles/` and names `6a2/` beside it, which
    /// resolves to `.machine_readable/6a2/`.
    pub fn from_canon(canon: &Canon) -> Result<Vec<Self>> {
        let mut found = Vec::new();

        for criterion in canon.criteria() {
            let Some((token, since)) = parse_deprecation_sentence(&criterion.desc) else {
                continue;
            };

            ensure!(
                !since.is_empty(),
                "criterion {} retires {:?} without saying when; the date is what a report quotes",
                criterion.id,
                token
            );

            let location = resolve_location(&token, &criterion.template_ref).map_err(|error| {
                anyhow::anyhow!(
                    "criterion {} retires {:?}, but it cannot be located: {error:#}. A \
                     deprecation this module cannot place is one a report cannot apply, and \
                     dropping it would turn a retired path into an ordinary one.",
                    criterion.id,
                    token
                )
            })?;

            found.push(Self {
                location,
                since,
                stated_by: criterion.id.clone(),
                quote: criterion.desc.clone(),
            });
        }

        Ok(found)
    }

    /// Is this repository path inside the retired location?
    pub fn matches(&self, path: &str) -> bool {
        if self.location.ends_with('/') {
            path.starts_with(&self.location)
        } else {
            path == self.location
        }
    }
}

/// Read `NOT <location>, which is deprecated <date>` out of a description.
fn parse_deprecation_sentence(desc: &str) -> Option<(String, String)> {
    let after_not = desc.split_once("NOT ")?.1;
    let (token, rest) = after_not.split_once(',')?;
    // The phrase, then the date. The space after "deprecated" is not required:
    // a sentence that states the retirement but omits the date must reach the
    // check below rather than vanish as "no retirement".
    let date = rest.trim_start().strip_prefix("which is deprecated")?;
    let date = date.split(')').next().unwrap_or(date).trim();
    Some((token.trim().to_string(), date.to_string()))
}

/// Turn a location named beside `.machine_readable/descriptiles/` into a path.
fn resolve_location(token: &str, template_ref: &str) -> Result<String> {
    ensure!(!token.is_empty(), "the location is empty");

    if token.trim_end_matches('/').contains('/') {
        return Ok(token.to_string());
    }

    // A bare name is a sibling of whatever the criterion is about. The
    // directory of `template_ref` gives the parent to resolve against.
    let directory = template_ref
        .rsplit_once('/')
        .map(|(directory, _)| directory);
    let Some(directory) = directory else {
        bail!(
            "{token:?} is named without a directory, and this criterion's template_ref \
             ({template_ref:?}) gives no directory to resolve it against"
        );
    };
    let parent = directory.rsplit_once('/').map_or("", |(parent, _)| parent);

    Ok(if parent.is_empty() {
        token.to_string()
    } else {
        format!("{parent}/{token}")
    })
}

/// The verdict for one required group of alternative locations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GroupVerdict {
    /// Satisfied, at this path.
    AtPath {
        path: String,
        /// Copies of the same file sitting under a retired location.
        deprecated_copies: Vec<String>,
        /// Where the canon's own template keeps it, when that differs. A note,
        /// never a fault: see the module docs.
        canon_keeps_it_at: Option<String>,
    },
    /// Present, but only under a location the canon has retired.
    Deprecated {
        found: String,
        location: String,
        since: String,
    },
    /// Present, but not at any location the description names, nor at the
    /// canon's recorded location for it, nor at the repository root.
    Elsewhere {
        expected: Vec<String>,
        found: Vec<String>,
        /// Copies of the same file under a retired location, when the same
        /// repository is carrying both -- the shape of a move that stopped
        /// halfway.
        deprecated_copies: Vec<String>,
    },
    /// Not present.
    Absent {
        expected: Vec<String>,
        /// Files that differ only in their extension: `CODE_OF_CONDUCT.adoc`
        /// where the criterion asks for `CODE_OF_CONDUCT.md`. Not a
        /// satisfaction -- the canon named a file, and this is a different one
        /// -- but a report that said only "absent" would send a reader looking
        /// for a file that is sitting there under another name.
        near_misses: Vec<String>,
    },
}

impl GroupVerdict {
    /// The word a report uses.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::AtPath { .. } => "at path",
            Self::Deprecated { .. } => "deprecated location",
            Self::Elsewhere { .. } => "elsewhere",
            Self::Absent { .. } => "absent",
        }
    }
}

/// How much the canon objects to what was found.
///
/// Ordered by how far the repository is from what the canon asks: a file that
/// is missing is worse than one in a retired place, which is worse than one
/// somewhere the canon did not name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Severity {
    /// Every group is present where the canon records it.
    Satisfied,
    /// Present, but somewhere the description does not name.
    Relocated,
    /// Present only under a location the canon has retired.
    Deprecated,
    /// Not present.
    Missing,
}

impl Severity {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Satisfied => "satisfied",
            Self::Relocated => "relocated",
            Self::Deprecated => "deprecated",
            Self::Missing => "missing",
        }
    }

    /// Does this belong in a list of things to act on?
    pub fn is_finding(self) -> bool {
        self != Self::Satisfied
    }
}

/// Every required group's outcome for one criterion.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Verdict {
    pub groups: Vec<GroupVerdict>,
    /// The criterion's own `template_ref`, carried so a report can say where
    /// the canon keeps the file without re-reading the canon.
    pub template_ref: String,
}

impl Verdict {
    /// Classify one criterion against the files a repository contains.
    ///
    /// `None` when the description is not a file-presence question -- the same
    /// answer `requirement_from` gives, for the same reason.
    pub fn of(
        criterion: &Criterion,
        files: &[String],
        deprecations: &[Deprecation],
    ) -> Option<Self> {
        let requirement = requirement_from(&criterion.desc)?;
        let groups = requirement
            .all_of
            .iter()
            .map(|alternatives| {
                classify_group(alternatives, files, deprecations, &criterion.template_ref)
            })
            .collect();
        Some(Self {
            groups,
            template_ref: criterion.template_ref.clone(),
        })
    }

    /// How much the canon objects, taking the worst group.
    pub fn severity(&self) -> Severity {
        if self
            .groups
            .iter()
            .any(|group| matches!(group, GroupVerdict::Absent { .. }))
        {
            return Severity::Missing;
        }
        if self
            .groups
            .iter()
            .any(|group| matches!(group, GroupVerdict::Deprecated { .. }))
        {
            return Severity::Deprecated;
        }
        if self
            .groups
            .iter()
            .any(|group| matches!(group, GroupVerdict::Elsewhere { .. }))
        {
            return Severity::Relocated;
        }
        Severity::Satisfied
    }

    /// The word a report uses for the verdict as a whole.
    pub fn as_str(&self) -> &'static str {
        match self.severity() {
            Severity::Satisfied => "at path",
            Severity::Relocated => "elsewhere",
            Severity::Deprecated => "deprecated location",
            Severity::Missing => "absent",
        }
    }

    /// Files the same criterion's copies leave behind in retired places, even
    /// where the verdict is satisfied -- a repository mid-move has both.
    pub fn deprecated_copies(&self) -> Vec<&String> {
        self.groups
            .iter()
            .flat_map(|group| match group {
                GroupVerdict::AtPath {
                    deprecated_copies, ..
                }
                | GroupVerdict::Elsewhere {
                    deprecated_copies, ..
                } => deprecated_copies.as_slice(),
                _ => &[],
            })
            .collect()
    }
}

fn classify_group(
    alternatives: &[String],
    files: &[String],
    deprecations: &[Deprecation],
    template_ref: &str,
) -> GroupVerdict {
    // 1. Present at one of the named locations.
    for alternative in alternatives {
        let matches = matching_paths(alternative, files);
        if let Some(found) = matches.first() {
            return GroupVerdict::AtPath {
                path: found.clone(),
                deprecated_copies: deprecated_copies_for(found, files, deprecations),
                canon_keeps_it_at: canon_location_note(template_ref, found),
            };
        }
    }

    // 2. A bare filename: the description named no directory, so the file is
    //    accepted in the two places that are unambiguously the repository's
    //    own -- where the canon records it, and the repository root. This runs
    //    before the retirement check below, so a repository holding the file in
    //    both the new and the retired place is satisfied with a leftover rather
    //    than reported as living in the retired one.
    let unanchored = alternatives
        .iter()
        .filter(|path| !path.ends_with('/'))
        .any(|path| !super::requirement::Requirement::is_anchored(path));
    if unanchored {
        for alternative in alternatives.iter().filter(|path| !path.ends_with('/')) {
            let basename = basename(alternative);

            // The canon's own recorded location.
            if template_ref != "-"
                && !template_ref.is_empty()
                && let Some(found) = files
                    .iter()
                    .find(|file| *file == template_ref && !is_retired(file, deprecations))
            {
                return GroupVerdict::AtPath {
                    path: found.clone(),
                    deprecated_copies: deprecated_copies_for(found, files, deprecations),
                    canon_keeps_it_at: None,
                };
            }

            // The repository's own root.
            if let Some(found) = files.iter().find(|file| {
                !file.contains('/')
                    && basename_of(file) == basename
                    && !is_retired(file, deprecations)
            }) {
                return GroupVerdict::AtPath {
                    path: found.clone(),
                    deprecated_copies: deprecated_copies_for(found, files, deprecations),
                    canon_keeps_it_at: canon_location_note(template_ref, found),
                };
            }
        }
    }

    // 3. Present somewhere else. Split by whether the copy is in a retired
    //    place, because a repository with the file in a live location *and* a
    //    leftover under a retired one is moving, not living in the retired one
    //    -- calling that "deprecated location" would overstate it.
    let mut live: Vec<String> = Vec::new();
    let mut retired: Vec<String> = Vec::new();
    for alternative in alternatives.iter().filter(|path| !path.ends_with('/')) {
        let basename = basename(alternative);
        for file in files.iter().filter(|file| basename_of(file) == basename) {
            if is_retired(file, deprecations) {
                if !retired.contains(file) {
                    retired.push(file.clone());
                }
            } else if !live.contains(file) {
                live.push(file.clone());
            }
        }
    }

    if !live.is_empty() {
        return GroupVerdict::Elsewhere {
            expected: alternatives.to_vec(),
            found: live,
            deprecated_copies: retired,
        };
    }

    // 4. Present only under a location the canon has retired.
    if let Some(found) = retired.first() {
        let deprecated = deprecations
            .iter()
            .find(|deprecated| deprecated.matches(found))
            .expect("a retired path came from a deprecation");
        return GroupVerdict::Deprecated {
            found: found.clone(),
            location: deprecated.location.clone(),
            since: deprecated.since.clone(),
        };
    }

    GroupVerdict::Absent {
        expected: alternatives.to_vec(),
        near_misses: near_misses_for(alternatives, files),
    }
}

/// Files whose name differs from an expected one only by extension.
fn near_misses_for(alternatives: &[String], files: &[String]) -> Vec<String> {
    let mut found: Vec<String> = Vec::new();
    for alternative in alternatives.iter().filter(|path| !path.ends_with('/')) {
        let stem = stem_of(basename(alternative));
        for file in files {
            if alternatives.iter().any(|expected| expected == file) {
                continue;
            }
            if basename_of(file) == basename(alternative) {
                continue;
            }
            if stem_of(basename_of(file)) == stem && !found.contains(file) {
                found.push(file.clone());
            }
        }
    }
    found.sort();
    found
}

/// The name without its extension. A leading dot is part of the name, not an
/// extension separator: `.gitignore` has no extension to strip.
fn stem_of(name: &str) -> &str {
    match name.rfind('.') {
        Some(index) if index > 0 => &name[..index],
        _ => name,
    }
}

/// Paths satisfying one alternative: an exact file, or anything under it when
/// the alternative names a directory.
fn matching_paths(alternative: &str, files: &[String]) -> Vec<String> {
    if alternative.ends_with('/') {
        files
            .iter()
            .filter(|file| file.starts_with(alternative) && file.len() > alternative.len())
            .cloned()
            .collect()
    } else {
        files
            .iter()
            .filter(|file| *file == alternative)
            .cloned()
            .collect()
    }
}

fn basename(path: &str) -> &str {
    path.trim_end_matches('/')
        .rsplit('/')
        .next()
        .unwrap_or(path)
}

fn basename_of(path: &str) -> &str {
    basename(path)
}

fn is_retired(path: &str, deprecations: &[Deprecation]) -> bool {
    deprecations
        .iter()
        .any(|deprecated| deprecated.matches(path))
}

fn deprecated_copies_for(
    found: &str,
    files: &[String],
    deprecations: &[Deprecation],
) -> Vec<String> {
    let name = basename_of(found);
    files
        .iter()
        .filter(|file| {
            deprecations
                .iter()
                .any(|deprecated| deprecated.matches(file))
        })
        .filter(|file| basename_of(file) == name)
        .cloned()
        .collect()
}

/// Where the canon's template keeps this file, when that is somewhere else.
pub fn canon_location_note(template_ref: &str, found: &str) -> Option<String> {
    if template_ref == "-" || template_ref.is_empty() || template_ref.ends_with('/') {
        return None;
    }
    if template_ref == found || basename_of(template_ref) != basename_of(found) {
        return None;
    }
    Some(template_ref.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn canon() -> Canon {
        Canon::vendored().expect("the vendored canon parses")
    }

    fn criterion(id: &str) -> Criterion {
        canon()
            .criteria()
            .find(|criterion| criterion.id == id)
            .unwrap_or_else(|| panic!("the canon has a criterion {id}"))
            .clone()
    }

    fn files(paths: &[&str]) -> Vec<String> {
        paths.iter().map(|path| path.to_string()).collect()
    }

    fn deprecations() -> Vec<Deprecation> {
        Deprecation::from_canon(&canon()).expect("the canon's retirements are readable")
    }

    #[test]
    fn the_canon_states_one_retirement_and_it_is_locatable() {
        let found = deprecations();
        assert_eq!(
            found.len(),
            1,
            "the canon is expected to state exactly one retirement; if it moved to a \
             machine-readable form, or stopped saying it, this module needs revisiting \
             rather than silently classifying retired paths as ordinary ones: {found:?}"
        );
        let deprecation = &found[0];
        assert_eq!(deprecation.location, ".machine_readable/6a2/");
        assert_eq!(deprecation.since, "2026-06-30");
        assert_eq!(deprecation.stated_by, "3.1.1");
        assert!(
            deprecation.quote.contains("NOT 6a2/"),
            "the quote is what a report shows the reader: {}",
            deprecation.quote
        );
    }

    #[test]
    fn a_canon_that_stops_saying_it_yields_no_retirement() {
        // The point of reading the sentence instead of hardcoding it: this is
        // what the lockstep test above is guarding.
        let rephrased = Canon::parse(&super::super::VENDORED_CRITERIA.replace(
            "(NOT 6a2/, which is deprecated 2026-06-30)",
            "(use the new layout)",
        ))
        .expect("a rephrased canon still parses");
        assert!(
            Deprecation::from_canon(&rephrased)
                .expect("no retirement is not an error")
                .is_empty()
        );
    }

    #[test]
    fn a_retirement_with_no_date_is_refused() {
        let undated = Canon::parse(
            &super::super::VENDORED_CRITERIA
                .replace("which is deprecated 2026-06-30", "which is deprecated"),
        )
        .expect("an undated canon still parses");
        let error = Deprecation::from_canon(&undated)
            .expect_err("a retirement without a date cannot be quoted honestly");
        assert!(
            format!("{error:#}").contains("without saying when"),
            "{error:#}"
        );
    }

    #[test]
    fn a_retirement_that_cannot_be_located_is_refused() {
        let unanchored = Canon::parse(&super::super::VENDORED_CRITERIA.replacen(
            "template_ref = \".machine_readable/descriptiles/\"",
            "template_ref = \"-\"",
            1,
        ))
        .expect("a canon with a root-level template_ref still parses");
        let error = Deprecation::from_canon(&unanchored)
            .expect_err("a retirement with no directory to resolve against must not be dropped");
        assert!(
            format!("{error:#}").contains("cannot be located"),
            "{error:#}"
        );
    }

    #[test]
    fn no_other_not_clause_is_read_as_a_retirement() {
        // 2.1.2 says "(NOT LICENSE.txt)". That is an exclusion, not a
        // retirement, and reading it as one would retire a filename.
        assert!(
            parse_deprecation_sentence("LICENSE present + LICENSES/ REUSE texts (NOT LICENSE.txt)")
                .is_none()
        );
        assert!(parse_deprecation_sentence("All .a2ml files parse + validate").is_none());
    }

    #[test]
    fn a_file_at_the_canon_path_is_at_path() {
        let verdict = Verdict::of(
            &criterion("3.1.2"),
            &files(&[".machine_readable/descriptiles/STATE.a2ml", "README.adoc"]),
            &deprecations(),
        )
        .expect("3.1.2 names a file");

        assert_eq!(verdict.severity(), Severity::Satisfied);
        assert_eq!(verdict.as_str(), "at path");
        match &verdict.groups[0] {
            GroupVerdict::AtPath {
                path,
                deprecated_copies,
                canon_keeps_it_at,
            } => {
                assert_eq!(path, ".machine_readable/descriptiles/STATE.a2ml");
                assert!(deprecated_copies.is_empty());
                assert_eq!(canon_keeps_it_at, &None);
            }
            other => panic!("expected AtPath, got {other:?}"),
        }
    }

    #[test]
    fn a_file_only_under_the_retired_location_is_reported_as_retired() {
        // The pilot's largest class: 23 of its 39 findings were files sitting
        // under .machine_readable/6a2/.
        let verdict = Verdict::of(
            &criterion("3.1.2"),
            &files(&[".machine_readable/6a2/STATE.a2ml"]),
            &deprecations(),
        )
        .expect("3.1.2 names a file");

        assert_eq!(verdict.severity(), Severity::Deprecated);
        assert_eq!(verdict.as_str(), "deprecated location");
        match &verdict.groups[0] {
            GroupVerdict::Deprecated {
                found,
                location,
                since,
            } => {
                assert_eq!(found, ".machine_readable/6a2/STATE.a2ml");
                assert_eq!(location, ".machine_readable/6a2/");
                assert_eq!(since, "2026-06-30");
            }
            other => panic!("expected Deprecated, got {other:?}"),
        }
    }

    #[test]
    fn a_repository_mid_move_shows_both_copies() {
        // nesy-solver was moving when the pilot ran: the file was in both
        // places. The move is satisfied, and the leftover is worth naming.
        let verdict = Verdict::of(
            &criterion("3.1.5"),
            &files(&[
                ".machine_readable/descriptiles/AGENTIC.a2ml",
                ".machine_readable/6a2/AGENTIC.a2ml",
            ]),
            &deprecations(),
        )
        .expect("3.1.5 names a file");

        assert_eq!(verdict.severity(), Severity::Satisfied);
        assert_eq!(
            verdict.deprecated_copies(),
            vec![&".machine_readable/6a2/AGENTIC.a2ml".to_string()]
        );
    }

    #[test]
    fn a_live_copy_beside_a_retired_leftover_is_relocated_not_deprecated() {
        // nesy-solver's shape for 3.1.2: the file has moved to
        // .machine_readable/ but not yet to the canon's descriptiles/
        // directory, and the retired copy is still there. The repository is
        // closer to compliant than "deprecated location" would suggest.
        let verdict = Verdict::of(
            &criterion("3.1.2"),
            &files(&[
                ".machine_readable/STATE.a2ml",
                ".machine_readable/6a2/STATE.a2ml",
            ]),
            &deprecations(),
        )
        .expect("3.1.2 names a file");

        assert_eq!(verdict.severity(), Severity::Relocated);
        match &verdict.groups[0] {
            GroupVerdict::Elsewhere {
                found,
                deprecated_copies,
                ..
            } => {
                assert_eq!(found, &vec![".machine_readable/STATE.a2ml".to_string()]);
                assert_eq!(
                    deprecated_copies,
                    &vec![".machine_readable/6a2/STATE.a2ml".to_string()]
                );
            }
            other => panic!("expected Elsewhere, got {other:?}"),
        }
    }

    #[test]
    fn a_bare_filename_at_the_canons_location_is_at_path() {
        // 3.1.8's description says "ANCHOR.a2ml" and no directory; the canon
        // records where its own template keeps it, and that counts.
        let verdict = Verdict::of(
            &criterion("3.1.8"),
            &files(&[".machine_readable/descriptiles/anchors/ANCHOR.a2ml"]),
            &deprecations(),
        )
        .expect("3.1.8 names a file");

        assert_eq!(verdict.severity(), Severity::Satisfied);
        match &verdict.groups[0] {
            GroupVerdict::AtPath {
                path,
                canon_keeps_it_at,
                ..
            } => {
                assert_eq!(path, ".machine_readable/descriptiles/anchors/ANCHOR.a2ml");
                assert_eq!(canon_keeps_it_at, &None);
            }
            other => panic!("expected AtPath, got {other:?}"),
        }
    }

    #[test]
    fn a_bare_filename_neither_at_the_canons_location_nor_at_the_root_is_a_finding() {
        // nesy-solver keeps ANCHOR.a2ml at .machine_readable/anchors/, where
        // the canon's template does not, and neither does it keep it at the
        // root. The pilot called this a near-miss; it is worth a look either
        // way, so it is a finding that names both locations.
        let verdict = Verdict::of(
            &criterion("3.1.8"),
            &files(&[".machine_readable/anchors/ANCHOR.a2ml"]),
            &deprecations(),
        )
        .expect("3.1.8 names a file");

        assert_eq!(verdict.severity(), Severity::Relocated);
        match &verdict.groups[0] {
            GroupVerdict::Elsewhere {
                expected, found, ..
            } => {
                assert_eq!(expected, &vec!["ANCHOR.a2ml".to_string()]);
                assert_eq!(
                    found,
                    &vec![".machine_readable/anchors/ANCHOR.a2ml".to_string()]
                );
            }
            other => panic!("expected Elsewhere, got {other:?}"),
        }
        assert_eq!(
            verdict.template_ref, ".machine_readable/descriptiles/anchors/ANCHOR.a2ml",
            "the report can name where the canon keeps it"
        );
    }

    #[test]
    fn another_projects_template_skeleton_does_not_satisfy_a_root_file() {
        // Found by running the classifier over gitbot-fleet itself: 1.2.4 wants
        // `.tool-versions`, and the only copy in the tree belongs to a template
        // skeleton two directories down. Counting that as satisfied would be
        // exactly the kind of quiet pass this module exists to prevent.
        let verdict = Verdict::of(
            &criterion("1.2.4"),
            &files(&["robot-repo-automaton/templates/skeleton/.tool-versions"]),
            &deprecations(),
        )
        .expect("1.2.4 names a file");

        assert_eq!(
            verdict.severity(),
            Severity::Relocated,
            "a nested copy is a finding, not a pass"
        );

        // And the repository's own copy does satisfy it.
        let own = Verdict::of(
            &criterion("1.2.4"),
            &files(&[".tool-versions"]),
            &deprecations(),
        )
        .expect("1.2.4 names a file");
        assert_eq!(own.severity(), Severity::Satisfied);
    }

    #[test]
    fn the_pre_commit_false_positive_does_not_come_back() {
        // The pilot's false positive: 1.2.2 wants "a .pre-commit-config.yaml
        // with real hooks"; the template keeps its copy in ci/. A root copy is
        // compliant, and reporting it as missing is the mistake this whole
        // module is arranged to prevent.
        let verdict = Verdict::of(
            &criterion("1.2.2"),
            &files(&[".pre-commit-config.yaml"]),
            &deprecations(),
        )
        .expect("1.2.2 names a file");

        assert_eq!(
            verdict.severity(),
            Severity::Satisfied,
            "a root .pre-commit-config.yaml satisfies 1.2.2"
        );
        match &verdict.groups[0] {
            GroupVerdict::AtPath {
                canon_keeps_it_at, ..
            } => assert_eq!(
                canon_keeps_it_at.as_deref(),
                Some("ci/.pre-commit-config.yaml")
            ),
            other => panic!("expected AtPath, got {other:?}"),
        }
    }

    #[test]
    fn a_path_the_description_gives_is_required_where_it_says() {
        // 2.2.1 names ".well-known/{security.txt,ai.txt,humans.txt}": three
        // files, each with a directory. A copy outside it is not the file the
        // canon asks for.
        let verdict = Verdict::of(
            &criterion("2.2.1"),
            &files(&["security.txt", "ai.txt", "humans.txt"]),
            &deprecations(),
        )
        .expect("2.2.1 names files");

        assert_eq!(verdict.severity(), Severity::Relocated);
        assert_eq!(verdict.as_str(), "elsewhere");
        assert_eq!(
            verdict.groups.len(),
            1,
            "the braces offer three files of one requirement, not three requirements"
        );
        match &verdict.groups[0] {
            GroupVerdict::Elsewhere {
                expected, found, ..
            } => {
                assert_eq!(
                    expected,
                    &vec![
                        ".well-known/security.txt".to_string(),
                        ".well-known/ai.txt".to_string(),
                        ".well-known/humans.txt".to_string()
                    ]
                );
                assert_eq!(
                    found,
                    &vec![
                        "security.txt".to_string(),
                        "ai.txt".to_string(),
                        "humans.txt".to_string()
                    ]
                );
            }
            other => panic!("expected Elsewhere, got {other:?}"),
        }
    }

    #[test]
    fn a_directory_requirement_is_satisfied_by_anything_inside_it() {
        let verdict = Verdict::of(
            &criterion("3.1.1"),
            &files(&[".machine_readable/descriptiles/STATE.a2ml"]),
            &deprecations(),
        )
        .expect("3.1.1 names a directory");
        assert_eq!(verdict.severity(), Severity::Satisfied);

        // The retired directory does not satisfy it: that is what the
        // description's "NOT 6a2/" says.
        let retired = Verdict::of(
            &criterion("3.1.1"),
            &files(&[".machine_readable/6a2/STATE.a2ml"]),
            &deprecations(),
        )
        .expect("3.1.1 names a directory");
        assert_eq!(
            retired.severity(),
            Severity::Missing,
            "a file under the retired directory does not make the new one present"
        );
    }

    #[test]
    fn a_plus_makes_both_required() {
        // 2.1.2 is "LICENSE present + LICENSES/ REUSE texts (NOT LICENSE.txt)".
        // The `+` makes both required; only a licence file is half of it.
        let licensing = criterion("2.1.2");

        let with_license_only = Verdict::of(&licensing, &files(&["LICENSE"]), &deprecations())
            .expect("2.1.2 names files");
        assert_eq!(with_license_only.groups.len(), 2);
        assert_eq!(with_license_only.severity(), Severity::Missing);

        let with_reuse_only = Verdict::of(
            &licensing,
            &files(&["LICENSES/MPL-2.0.txt"]),
            &deprecations(),
        )
        .expect("2.1.2 names files");
        assert_eq!(with_reuse_only.severity(), Severity::Missing);

        let with_both = Verdict::of(
            &licensing,
            &files(&["LICENSE", "LICENSES/MPL-2.0.txt"]),
            &deprecations(),
        )
        .expect("2.1.2 names files");
        assert_eq!(with_both.severity(), Severity::Satisfied);
    }

    #[test]
    fn both_required_files_must_be_present() {
        // 2.1.10: ".gitignore and .gitattributes present". One is not enough.
        let neither = Verdict::of(&criterion("2.1.10"), &files(&[]), &deprecations())
            .expect("2.1.10 names files");
        assert_eq!(neither.severity(), Severity::Missing);

        let one = Verdict::of(
            &criterion("2.1.10"),
            &files(&[".gitignore"]),
            &deprecations(),
        )
        .expect("2.1.10 names files");
        assert_eq!(one.groups.len(), 2);
        assert_eq!(one.severity(), Severity::Missing);

        let both = Verdict::of(
            &criterion("2.1.10"),
            &files(&[".gitignore", ".gitattributes"]),
            &deprecations(),
        )
        .expect("2.1.10 names files");
        assert_eq!(both.severity(), Severity::Satisfied);
    }

    #[test]
    fn an_absent_file_says_what_is_sitting_there_instead() {
        // gitbot-fleet has CODE_OF_CONDUCT.adoc; 2.1.4 asks for .md. The
        // verdict is absent, and the report says what it found instead.
        let verdict = Verdict::of(
            &criterion("2.1.4"),
            &files(&["CODE_OF_CONDUCT.adoc", "README.adoc"]),
            &deprecations(),
        )
        .expect("2.1.4 names a file");

        assert_eq!(verdict.severity(), Severity::Missing);
        match &verdict.groups[0] {
            GroupVerdict::Absent {
                expected,
                near_misses,
            } => {
                assert_eq!(expected, &vec!["CODE_OF_CONDUCT.md".to_string()]);
                assert_eq!(near_misses, &vec!["CODE_OF_CONDUCT.adoc".to_string()]);
            }
            other => panic!("expected Absent, got {other:?}"),
        }
    }

    #[test]
    fn a_dotfile_has_no_extension_to_near_miss_on() {
        let verdict = Verdict::of(
            &criterion("1.1.2"),
            &files(&["README.adoc"]),
            &deprecations(),
        )
        .expect("1.1.2 names a file");
        match &verdict.groups[0] {
            GroupVerdict::Absent { near_misses, .. } => {
                assert!(near_misses.is_empty(), "unexpected: {near_misses:?}");
            }
            other => panic!("expected Absent, got {other:?}"),
        }
    }

    #[test]
    fn a_criterion_that_names_no_files_has_no_verdict() {
        // An honest "this is not a file question", not an empty verdict that a
        // caller might count as satisfied.
        let manual = canon()
            .criteria()
            .find(|criterion| requirement_from(&criterion.desc).is_none())
            .expect("the canon has criteria that name no files")
            .clone();
        assert!(Verdict::of(&manual, &files(&[]), &deprecations()).is_none());
    }

    #[test]
    fn severity_orders_what_the_canon_objects_to_most() {
        assert!(Severity::Missing > Severity::Deprecated);
        assert!(Severity::Deprecated > Severity::Relocated);
        assert!(Severity::Relocated > Severity::Satisfied);
        assert!(!Severity::Satisfied.is_finding());
        assert!(Severity::Missing.is_finding());
    }
}
