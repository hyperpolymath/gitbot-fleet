// SPDX-License-Identifier: MPL-2.0
//! Canonical RSR skeleton generator (hyperpolymath/rsr-template-repo#48).
//!
//! `robot-repo-automaton` is the single source of truth for the RSR
//! *required-files set*. `rsr-template-repo` is **derived** from this: its
//! drift-CI runs `robot-repo-automaton skeleton check .` and fails if the
//! checked-in template diverges from [`SKELETON`]. This makes hand-maintenance
//! of the template structurally impossible to drift (#45/#47 closed the
//! immediate sanitisation; this closes the stronger structural fix #48).
//!
//! `emit` reproduces the **uninstantiated template repository**. Project
//! identity, purpose, kind, author and licence placeholders are intentional:
//! this command is not a project initializer. `check` compares that same
//! template, not a downstream project after its owner has customized it.
//!
//! Source of truth = the **post-#47 canonical layout**, not the prose in
//! `standards` `REQUIRED-FILES.md` (which is stale: it still lists root
//! `*.scm` + `Mustfile`, predating the estate-wide A2ML retirement and the
//! deed-first layout). Updating that doc to match is a
//! separate `standards`-repo change, deliberately out of scope here.

use anyhow::{Context, Result};
use std::path::Path;

/// `(repo-relative path, canonical content)`, embedded at compile time so the
/// generator is hermetic and reproducible. The embedded copies under
/// `templates/skeleton/` are themselves the canonical artefacts; on day one
/// they are byte-identical to the post-#47 `rsr-template-repo`, so the drift
/// check passes immediately and any future divergence fails CI.
pub const SKELETON: &[(&str, &str)] = &[
    (
        ".gitignore",
        include_str!("../templates/skeleton/.gitignore"),
    ),
    (
        ".gitattributes",
        include_str!("../templates/skeleton/.gitattributes"),
    ),
    (
        ".editorconfig",
        include_str!("../templates/skeleton/.editorconfig"),
    ),
    (
        ".tool-versions",
        include_str!("../templates/skeleton/.tool-versions"),
    ),
    ("Justfile", include_str!("../templates/skeleton/Justfile")),
    // A2ML is retired (owner rulings D99, D312, D313): no `.a2ml` is ever
    // emitted. Repo facts are clauses of `<repo>_chora.deed`; the playbook moves
    // to `PLAYBOOK.deed` (A7 ruling), with empty sections for now.
    (
        ".machine_readable/descriptiles/PLAYBOOK.deed",
        include_str!("../templates/skeleton/.machine_readable/descriptiles/PLAYBOOK.deed"),
    ),
];

/// Write the uninstantiated canonical template into `out`, creating parents.
/// Project-specific placeholders remain for the downstream initialization step.
pub fn emit(out: &Path) -> Result<()> {
    for (rel, content) in SKELETON {
        let dst = out.join(rel);
        if let Some(parent) = dst.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("creating {}", parent.display()))?;
        }
        std::fs::write(&dst, content).with_context(|| format!("writing {}", dst.display()))?;
    }
    Ok(())
}

/// A single required file that has drifted from canonical.
#[derive(Debug, PartialEq, Eq)]
pub enum Drift {
    /// Required file absent in the target repo.
    Missing(String),
    /// Required file present but content differs from canonical.
    Differs(String),
}

/// Compare `repo`'s required files against [`SKELETON`]. Empty result == in
/// sync. Comparison ignores only CRLF and trailing-final-newline noise so the
/// check is robust across platforms/editors without being lax about content.
pub fn check(repo: &Path) -> Result<Vec<Drift>> {
    let mut drift = Vec::new();
    for (rel, content) in SKELETON {
        let path = repo.join(rel);
        match std::fs::read_to_string(&path) {
            Err(_) => drift.push(Drift::Missing((*rel).to_string())),
            Ok(actual) => {
                if normalize(&actual) != normalize(content) {
                    drift.push(Drift::Differs((*rel).to_string()));
                }
            }
        }
    }
    Ok(drift)
}

fn normalize(s: &str) -> String {
    let mut t = s.replace("\r\n", "\n");
    while t.ends_with('\n') {
        t.pop();
    }
    t
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn skeleton_is_non_empty_and_well_formed() {
        assert!(SKELETON.len() >= 6, "expected the full RSR required set");
        for (rel, content) in SKELETON {
            assert!(!rel.is_empty(), "empty skeleton path");
            assert!(!rel.starts_with('/'), "{rel} must be repo-relative");
            assert!(!content.is_empty(), "{rel} canonical content is empty");
        }
    }

    #[test]
    fn skeleton_emits_no_a2ml() {
        for (rel, content) in SKELETON {
            assert!(!rel.ends_with(".a2ml"), "{rel} is retired A2ML");
            assert!(
                !content.contains(".a2ml"),
                "{rel} still names a .a2ml file"
            );
        }
        let dir = tempfile::tempdir().unwrap();
        emit(dir.path()).unwrap();
        let mut stack = vec![dir.path().to_path_buf()];
        while let Some(d) = stack.pop() {
            for e in std::fs::read_dir(d).unwrap() {
                let p = e.unwrap().path();
                if p.is_dir() {
                    stack.push(p);
                } else {
                    assert_ne!(
                        p.extension().and_then(|x| x.to_str()),
                        Some("a2ml"),
                        "emitted {}",
                        p.display()
                    );
                }
            }
        }
    }

    /// Every emitted `.deed` is structurally a deed (SPDX header, `(`-form,
    /// no `key = value` / `[section]`). When `DEED_LINT` points at
    /// standards' `1-formats/deed/tools/deed_lint.js`, the real linter runs.
    #[test]
    fn emitted_deeds_lint() {
        let dir = tempfile::tempdir().unwrap();
        emit(dir.path()).unwrap();
        let mut n = 0;
        for (rel, content) in SKELETON.iter().filter(|(r, _)| r.ends_with(".deed")) {
            n += 1;
            assert!(content.starts_with(";; SPDX-"), "{rel}: SPDX header");
            for line in content.lines().filter(|l| !l.trim_start().starts_with(';')) {
                assert!(!line.trim_start().starts_with('['), "{rel}: [section]");
            }
            assert!(content.contains("(descriptile-deed"), "{rel}: doc-head");
            assert!(
                content.matches(":schema-version").count() == 1,
                "{rel}: :schema-version exactly once"
            );
            if let Ok(lint) = std::env::var("DEED_LINT") {
                let out = std::process::Command::new("bun")
                    .arg(&lint)
                    .arg(dir.path().join(rel))
                    .output()
                    .expect("run bun deed_lint.js");
                assert!(
                    out.status.success(),
                    "{rel} fails deed_lint: {}",
                    String::from_utf8_lossy(&out.stdout)
                );
            }
        }
        assert!(n >= 1, "skeleton must emit the deed form");
    }

    #[test]
    fn emit_then_check_roundtrips_clean() {
        let dir = tempfile::tempdir().unwrap();
        emit(dir.path()).unwrap();
        let drift = check(dir.path()).unwrap();
        assert!(
            drift.is_empty(),
            "freshly emitted skeleton drifted: {drift:?}"
        );
    }

    #[test]
    fn check_flags_missing_and_differing() {
        let dir = tempfile::tempdir().unwrap();
        emit(dir.path()).unwrap();
        // Mutate one file and delete another.
        std::fs::write(dir.path().join(".tool-versions"), "tampered\n").unwrap();
        std::fs::remove_file(dir.path().join(".gitignore")).unwrap();
        let drift = check(dir.path()).unwrap();
        assert!(drift.contains(&Drift::Differs(".tool-versions".into())));
        assert!(drift.contains(&Drift::Missing(".gitignore".into())));
    }

    #[test]
    fn normalize_ignores_only_eol_noise() {
        assert_eq!(normalize("a\r\nb\n\n"), normalize("a\nb"));
        assert_ne!(normalize("a b"), normalize("a  b"));
    }
}
