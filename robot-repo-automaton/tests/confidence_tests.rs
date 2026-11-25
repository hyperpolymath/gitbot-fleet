// SPDX-License-Identifier: MPL-2.0
//! Regression tests for the confidence guard on protected directories.

use robot_repo_automaton::catalog::{Fix, FixAction, Severity};
use robot_repo_automaton::{ConfidenceLevel, DetectedIssue, ThresholdConfig};

fn deletion_confidence(target: &str) -> ConfidenceLevel {
    let issue = DetectedIssue {
        error_type_id: "TEST".into(),
        error_name: "Obsolete file".into(),
        severity: Severity::Medium,
        description: "Detected an obsolete file".into(),
        affected_files: vec![],
        confidence: 1.0,
        suggested_fix: "Delete the file".into(),
        commit_message: "fix: remove obsolete file".into(),
    };
    let fix = Fix {
        action: FixAction::Delete,
        target: target.into(),
        reason: None,
        modification: None,
        fallback: None,
    };
    ThresholdConfig::default().classify_fix(&issue, &fix)
}

#[test]
fn test_high_confidence_deletes_are_downgraded_in_protected_directories() {
    // No protected basename or extension can account for these results.
    for target in [
        ".machine_readable/metadata.json",
        "crates/engine/src/abi/exports.rs",
        "docs/getting-started.txt",
        "packages/engine/spec/grammar.txt",
        "verification/run.sh",
    ] {
        assert_eq!(
            deletion_confidence(target),
            ConfidenceLevel::Low,
            "{target}"
        );
    }
}

#[test]
fn test_high_confidence_deletes_keep_their_rating_for_directory_near_misses() {
    for target in [
        "src/docs", // A filename, with no trailing directory separator.
        "docs-old/notes.txt",
        "packages/engine/Spec/grammar.txt", // Directory markers are case sensitive.
        "crates/engine/src/abigail/exports.rs",
    ] {
        assert_eq!(
            deletion_confidence(target),
            ConfidenceLevel::High,
            "{target}"
        );
    }
}
