// SPDX-License-Identifier: MPL-2.0
//! Which of your package names are worth searching for, and how confident a
//! match is.

use serde::Serialize;

/// Internal test/bench/ABI-shim packages nobody else would depend on by name.
pub fn is_distinctive(name: &str) -> bool {
    let n = name.to_ascii_lowercase();
    let internal = matches!(
        n.as_str(),
        "abi"
            | "benchmark"
            | "benchmarks"
            | "test"
            | "tests"
            | "coreonly"
            | "ffi-test"
            | "integration-tests"
            | "property-tests"
    ) || (n.starts_with("test-") && n.len() > 5)
        || ["-test", "-tests", "-bench", "-suite"]
            .iter()
            .any(|suffix| n.len() > suffix.len() && n.ends_with(suffix));
    !internal
}

/// Names that are ordinary words, so a bare match may be someone else's package.
pub const AMBIGUOUS: &[&str] = &["proven", "trope"];

/// One search term per package family: `proven-ffi`, `proven-dns`… collapse to
/// `proven`. Stems shorter than five characters are too noisy to search, so
/// those names are kept whole.
pub fn search_stems(names: &[String]) -> Vec<String> {
    let mut stems: Vec<String> = names
        .iter()
        .filter(|n| is_distinctive(n))
        .map(|n| {
            let stem = n.split('-').next().unwrap_or(n);
            if stem.chars().count() >= 5 {
                stem.to_string()
            } else {
                n.clone()
            }
        })
        .collect();
    stems.sort();
    stems.dedup();
    stems
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum Confidence {
    /// `pack.toml` or `.gitmodules` points at your GitHub owners (unambiguous).
    High,
    /// `.ipkg` depends on a distinctive package name of yours.
    Medium,
    /// Only ambiguous names (`proven`, `trope`) with no corroboration.
    Low,
}

pub fn classify(deps: &[String], pack_refs: usize, sub_refs: usize) -> Option<Confidence> {
    if pack_refs > 0 || sub_refs > 0 {
        Some(Confidence::High)
    } else if deps.iter().any(|d| !AMBIGUOUS.contains(&d.as_str())) {
        Some(Confidence::Medium)
    } else if !deps.is_empty() {
        Some(Confidence::Low)
    } else {
        None
    }
}
