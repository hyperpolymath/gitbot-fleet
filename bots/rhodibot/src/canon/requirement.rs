// SPDX-License-Identifier: MPL-2.0

//! What a criterion requires, read from its own description.
//!
//! # Why not `template_ref`
//!
//! The pilot read `template_ref` as the path a criterion requires. Over five
//! repositories that invented violations: criterion 1.2.2 asks for a
//! ".pre-commit-config.yaml with real hooks", the template keeps its copy in
//! `ci/`, and repositories that keep one at the root were reported as missing
//! it. `template_ref` records where the *template* satisfies a criterion; it is
//! a traceability pointer, not a requirement.
//!
//! The description is what the criterion asks for, so requirements are read
//! from there.
//!
//! # Deliberately conservative
//!
//! This is a parser for prose, which is a good reason to keep it narrow. It
//! handles the shapes the canon actually uses and gives up on everything else:
//!
//! - `README.adoc present` -> one requirement
//! - `.gitignore and .gitattributes present` -> both, because one file does not
//!   satisfy it
//! - `FUNDING.yml / .github/FUNDING.yml` -> either location
//! - `.well-known/{security.txt,ai.txt,humans.txt}` -> three paths, braces
//!   expanded
//! - `LICENSE present + LICENSES/ REUSE texts (NOT LICENSE.txt)` -> two, with
//!   the parenthesised exclusion dropped and `NOT` never read as a requirement
//! - `CHANGELOG.adoc, or .md (Keep a Changelog)` -> **no requirement at all**
//!
//! Giving up matters as much as parsing. That last one offers `.md` as an
//! alternative, and a checker that keeps only `CHANGELOG.adoc` would report
//! every repository using `CHANGELOG.md` as non-compliant. Where a description
//! names an alternative this parser cannot turn into a path, the criterion is
//! left to a human rather than guessed at.

/// Groups of paths, every group of which must be satisfied.
///
/// A group is satisfied when any one of its paths exists: the canon offers
/// alternative locations, not alternatives to the requirement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Requirement {
    pub all_of: Vec<Vec<String>>,
}

impl Requirement {
    /// Every path the requirement names, in order.
    pub fn paths(&self) -> impl Iterator<Item = &String> {
        self.all_of.iter().flatten()
    }

    /// How many files have to be present, at minimum.
    pub fn group_count(&self) -> usize {
        self.all_of.len()
    }

    /// Is this path anchored to a directory by the description?
    ///
    /// The canon names some files by their bare name -- "STATE.a2ml", "Justfile"
    /// -- and gives the directory only in a neighbouring criterion. A check that
    /// reads a bare basename as a root path reports every repository as missing
    /// it, which is the mistake the pilot was built to find. Callers that care
    /// about location should treat an unanchored path as "this file, anywhere"
    /// and say so in the report rather than assert a path the canon never gave.
    pub fn is_anchored(path: &str) -> bool {
        path.contains('/')
    }
}

/// Read a requirement from a criterion description.
///
/// `None` means the description does not name files this can resolve, which is
/// an answer: the criterion is not a file-presence question.
pub fn requirement_from(desc: &str) -> Option<Requirement> {
    // Parentheses hold explanation or exclusion -- "(NOT LICENSE.txt)",
    // "(Keep a Changelog)", "(.adoc primary per estate doc policy)". None of it
    // names something that must exist.
    let bare = strip_parenthetical(desc);

    // " and " and " + " join things that are all required. " or ", " / " and
    // "," offer alternatives.
    let mut all_of = Vec::new();
    for group_text in split_on(&bare, &[" and ", " + "]) {
        let mut any_of = Vec::new();
        let mut gave_up = false;
        for alternative in split_on(group_text, &[" or ", " / ", ","]) {
            let paths = expand(alternative);
            if paths.is_empty() {
                // A fragment that is not a path, in a position where the
                // description was offering one. `.md` in
                // "CHANGELOG.adoc, or .md" is the case this exists for: keeping
                // only the sibling would fault every repository that chose the
                // other option.
                if looks_like_a_partial_path(alternative) {
                    gave_up = true;
                    break;
                }
                continue;
            }
            any_of.extend(paths);
        }

        if gave_up {
            return None;
        }
        if any_of.is_empty() {
            continue;
        }
        all_of.push(any_of);
    }

    (!all_of.is_empty()).then_some(Requirement { all_of })
}

/// Remove text inside parentheses, including the parentheses.
fn strip_parenthetical(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut depth = 0usize;
    for ch in text.chars() {
        match ch {
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            _ if depth == 0 => out.push(ch),
            _ => {}
        }
    }
    out
}

/// Split on the given separators, but never inside `{...}`.
///
/// The brace form is how the canon lists the files of a directory
/// (`.well-known/{security.txt,ai.txt,humans.txt}`). Splitting on the commas
/// first would tear that into `.well-known/{security.txt`, `ai.txt` and
/// `humans.txt}`, and two of the three would be lost.
fn split_on<'a>(text: &'a str, separators: &[&str]) -> Vec<&'a str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut depth = 0usize;
    let mut index = 0;

    // Iterated by character, not by byte: the canon's descriptions carry em
    // dashes, and stepping through bytes would cut them in half.
    while index < text.len() {
        let Some(ch) = text[index..].chars().next() else {
            break;
        };

        match ch {
            '{' => depth += 1,
            '}' => depth = depth.saturating_sub(1),
            _ => {}
        }

        if depth == 0
            && let Some(separator) = separators
                .iter()
                .find(|separator| text[index..].starts_with(**separator))
        {
            parts.push(&text[start..index]);
            index += separator.len();
            start = index;
            continue;
        }

        index += ch.len_utf8();
    }

    parts.push(&text[start..]);
    parts
}

/// The paths a fragment names, with `{a,b}` expanded.
fn expand(fragment: &str) -> Vec<String> {
    // "No Makefile (Mustfile/justfile only)" is a criterion about the absence
    // of a file. Reading the filename out of it and requiring the file would
    // invert the criterion -- the worst kind of wrong, because the report would
    // confidently ask for the opposite of what the canon says.
    let opening = fragment.trim_start().to_lowercase();
    if opening.starts_with("no ") || opening.starts_with("not ") || opening.starts_with("never ") {
        return Vec::new();
    }

    let words: Vec<&str> = fragment
        .split_whitespace()
        .map(|word| {
            word.trim_start_matches(['`', '"', '\''])
                .trim_end_matches(['`', '"', ',', ';', '.', ':'])
        })
        .filter(|word| !word.is_empty())
        .collect();

    let mut paths = Vec::new();
    for word in words {
        if let Some((prefix, group, suffix)) = split_braces(word) {
            for option in group.split(',') {
                let candidate = format!("{prefix}{}{suffix}", option.trim());
                if is_path(&candidate) {
                    paths.push(candidate);
                }
            }
            continue;
        }
        if is_path(word) {
            paths.push(word.to_string());
        }
    }
    paths
}

/// `a/{x,y}.txt` -> (`a/`, `x,y`, `.txt`)
fn split_braces(word: &str) -> Option<(&str, &str, &str)> {
    let open = word.find('{')?;
    let close = word.find('}')?;
    (close > open).then(|| (&word[..open], &word[open + 1..close], &word[close + 1..]))
}

/// Files the canon names without an extension.
///
/// Kept as a list rather than inferred from capitalisation, so that prose
/// ("present", "texts", "dir") cannot qualify by being short and shouty.
const EXTENSIONLESS: &[&str] = &[
    "LICENSE",
    "LICENCES",
    "NOTICE",
    "README",
    "Justfile",
    "Mustfile",
    "Makefile",
    "Dockerfile",
    "CODEOWNERS",
    "AUTHORS",
    "CONTRIBUTORS",
];

/// Does this look like a path a repository could be checked for?
///
/// A directory ends in a slash. Anything else needs an extension, or to be one
/// of the handful of files the canon names without one. This is what separates
/// `descriptiles/STATE.a2ml` from the prose `state/progress`, and `README.adoc`
/// from the version number `7.0` -- a description such as "Scorecard >= 7.0"
/// must not become a requirement for a file called `7.0`.
fn is_path(word: &str) -> bool {
    if word.is_empty() || word == "/" || word.starts_with("NOT") {
        return false;
    }
    // A bare number, or a dotted one: a version, a score, a threshold.
    if word
        .split('.')
        .all(|part| !part.is_empty() && part.chars().all(|c| c.is_ascii_digit()))
    {
        return false;
    }
    match word.rsplit_once('/') {
        // A directory.
        Some((_, "")) => true,
        // A path: judge its last segment, which carries the extension.
        Some((_, last)) => has_extension(last) || EXTENSIONLESS.contains(&last),
        None => has_extension(word) || EXTENSIONLESS.contains(&word),
    }
}

/// Extensions that are extensions, not filenames.
///
/// Criterion 3.2.1 says "All .a2ml files parse + validate …". `.a2ml` there is a
/// bare extension, and treating it as a filename asks every repository for a
/// file called `.a2ml`. A leading-dot token is read as a file only when what
/// follows is not one of these.
const EXTENSIONS_ONLY: &[&str] = &[
    "a2ml", "adoc", "k9", "md", "ncl", "json", "scm", "sh", "toml", "txt", "yaml", "yml",
];

/// `README.adoc` yes; `progress` no.
///
/// A leading dot splits two ways: `.gitignore` and `.editorconfig` are files,
/// while `.md` and `.a2ml` are extensions with no name of their own. Treating
/// `.md` as a file would turn "CHANGELOG.adoc, or .md" into a requirement for a
/// file called `.md`.
fn has_extension(word: &str) -> bool {
    match word.rsplit_once('.') {
        Some(("", rest)) => {
            rest.len() >= 3
                && !EXTENSIONS_ONLY.contains(&rest)
                && rest.chars().all(|c| c.is_ascii_alphanumeric() || c == '-')
        }
        Some((stem, extension)) => {
            !stem.is_empty()
                && (1..=8).contains(&extension.len())
                && extension.chars().all(|c| c.is_ascii_alphanumeric())
        }
        None => false,
    }
}

/// A fragment that was offered as an alternative but is not a usable path.
///
/// `.md` is the case: an extension with no stem, meaningful only next to the
/// sibling it modifies.
fn looks_like_a_partial_path(fragment: &str) -> bool {
    fragment
        .split_whitespace()
        .any(|word| word.starts_with('.') && word.len() > 1 && !word.contains('/'))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn paths(desc: &str) -> Vec<String> {
        requirement_from(desc)
            .map(|r| r.paths().cloned().collect())
            .unwrap_or_default()
    }

    #[test]
    fn a_named_file_is_a_requirement() {
        assert_eq!(
            paths("README.adoc present (.adoc primary per estate doc policy)"),
            ["README.adoc"]
        );
        assert_eq!(
            paths("SECURITY.md with a vulnerability-disclosure policy"),
            ["SECURITY.md"]
        );
    }

    #[test]
    fn a_named_path_keeps_its_directories() {
        assert_eq!(
            paths(".machine_readable/descriptiles/STATE.a2ml — current state/progress"),
            [".machine_readable/descriptiles/STATE.a2ml"]
        );
    }

    #[test]
    fn braces_expand_into_the_files_the_criterion_lists() {
        assert_eq!(
            paths(".well-known/{security.txt,ai.txt,humans.txt}"),
            [
                ".well-known/security.txt",
                ".well-known/ai.txt",
                ".well-known/humans.txt"
            ]
        );
    }

    #[test]
    fn both_sides_of_an_and_are_required() {
        let requirement =
            requirement_from(".gitignore and .gitattributes present").expect("a requirement");
        assert_eq!(requirement.group_count(), 2);
        assert_eq!(
            paths(".gitignore and .gitattributes present"),
            [".gitignore", ".gitattributes"]
        );
    }

    #[test]
    fn a_slash_offers_locations_not_a_choice_of_requirement() {
        // FUNDING.yml satisfies it as surely as .github/FUNDING.yml does.
        let requirement =
            requirement_from("FUNDING.yml / .github/FUNDING.yml").expect("a requirement");
        assert_eq!(
            requirement.group_count(),
            1,
            "one requirement, two locations"
        );
        assert_eq!(requirement.all_of[0].len(), 2);
    }

    #[test]
    fn the_template_directory_convention_is_not_read_as_a_requirement() {
        // The description names the file and no directory; that is the
        // criterion. Anything about `ci/` is the template's business.
        let requirement =
            requirement_from(".pre-commit-config.yaml with real hooks").expect("a requirement");
        assert_eq!(
            requirement.all_of,
            vec![vec![".pre-commit-config.yaml".to_string()]]
        );
    }

    #[test]
    fn an_excluded_name_is_not_a_requirement() {
        // "LICENSE present + LICENSES/ REUSE texts (NOT LICENSE.txt)"
        let requirement =
            requirement_from("LICENSE present + LICENSES/ REUSE texts").expect("a requirement");
        assert_eq!(requirement.group_count(), 2);
        assert!(
            !requirement.paths().any(|p| p == "LICENSE.txt"),
            "an excluded spelling must never be required"
        );
    }

    #[test]
    fn a_directory_is_a_requirement_when_the_description_names_one() {
        assert_eq!(
            paths("REUSE-style LICENSES/ dir with full texts"),
            ["LICENSES/"]
        );
    }

    #[test]
    fn an_alternative_this_cannot_resolve_gives_up_rather_than_guessing() {
        // Keeping only CHANGELOG.adoc here would fault every repository that
        // chose CHANGELOG.md, which the description allows.
        assert!(requirement_from("CHANGELOG.adoc, or .md (Keep a Changelog)").is_none());
    }

    #[test]
    fn a_criterion_about_absence_does_not_become_a_requirement_for_the_file() {
        // 1.1.3 is "no-makefile". Reading `Makefile` out of it would require
        // every repository to have the file the canon forbids.
        assert!(requirement_from("No Makefile (Mustfile/justfile only)").is_none());
        assert!(requirement_from("No hardcoded secrets (secret scanner clean)").is_none());
        assert!(requirement_from("No plaintext HTTP URLs; HTTPS only").is_none());
    }

    #[test]
    fn prose_with_a_slash_is_not_a_path() {
        assert!(requirement_from("STATE.a2ml — current state/progress").is_some());
        let requirement =
            requirement_from("STATE.a2ml — current state/progress").expect("a requirement");
        assert_eq!(
            requirement.all_of,
            vec![vec!["STATE.a2ml".to_string()]],
            "`state/progress` is prose, not a directory"
        );
    }

    #[test]
    fn a_bare_extension_is_not_a_filename() {
        // "All .a2ml files parse + validate against their record-dialect profile"
        assert!(
            requirement_from(
                "All .a2ml files parse + validate against their record-dialect profile"
            )
            .is_none(),
            "`.a2ml` is an extension; requiring a file of that name would be nonsense"
        );
    }

    #[test]
    fn a_bare_basename_is_reported_as_unanchored() {
        let requirement =
            requirement_from("STATE.a2ml — current state/progress").expect("a requirement");
        let paths: Vec<&String> = requirement.paths().collect();
        assert_eq!(paths, [&"STATE.a2ml".to_string()]);
        assert!(
            !Requirement::is_anchored(paths[0]),
            "the description names no directory"
        );

        let anchored = requirement_from(".well-known/{security.txt}").expect("a requirement");
        assert!(Requirement::is_anchored(
            anchored.paths().next().expect("a path")
        ));
    }

    #[test]
    fn a_description_that_names_no_files_has_no_requirement() {
        for desc in [
            "No Python (fully banned)",
            "Repository canonical on GitHub (GitLab is mirror-only)",
            "OpenSSF Scorecard >= 7.0",
            "TLS 1.3 only for deployed endpoints",
            "No believe_me / sorry / Admitted in load-bearing proofs",
        ] {
            assert!(
                requirement_from(desc).is_none(),
                "{desc:?} names no file a repository can be checked for"
            );
        }
    }

    #[test]
    fn the_deprecated_location_is_read_as_an_exclusion_not_a_requirement() {
        // ".machine_readable/descriptiles/ present (NOT 6a2/, which is deprecated 2026-06-30)"
        let requirement =
            requirement_from(".machine_readable/descriptiles/ present").expect("a requirement");
        assert_eq!(
            requirement.all_of,
            vec![vec![".machine_readable/descriptiles/".to_string()]]
        );
        assert!(!requirement.paths().any(|p| p.contains("6a2")));
    }
}
