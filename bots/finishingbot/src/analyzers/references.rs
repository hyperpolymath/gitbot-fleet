// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Resolve local prose references without using generated basename fallbacks.

use std::path::{Path, PathBuf};

const GENERATED_DIRS: &[&str] = &[
    ".git",
    "target",
    "node_modules",
    "build",
    "_build",
    "_site",
    ".ddraig-ssg",
    ".echo-proof-tools",
    ".echo-standards-references",
    "vendor",
    "deps",
];

fn local_file(root: &Path, candidate: &Path) -> Option<PathBuf> {
    let resolved = candidate.canonicalize().ok()?;
    (resolved.starts_with(root) && resolved.is_file()).then_some(resolved)
}

/// `Some(file name)` when `path` is a bare `*.yml`/`*.yaml` name or
/// `workflows/<name>`, the two spellings prose uses for a GitHub workflow.
fn workflow_file_name(path: &Path) -> Option<&std::ffi::OsStr> {
    let name = path.file_name()?;
    let is_yaml = path
        .extension()
        .is_some_and(|e| e == "yml" || e == "yaml");
    let parts: Vec<_> = path.components().collect();
    let qualified_ok = parts.len() == 1
        || (parts.len() == 2 && parts[0].as_os_str() == "workflows");
    (is_yaml && qualified_ok).then_some(name)
}

/// Prose can name a document-relative path, a repository-relative path, or an
/// unanchored module basename. The last form is accepted only when unique.
/// This resolves a file's identity; it does not verify its contents or claims.
pub(super) fn resolve_local_reference(
    repo: &Path,
    document: &Path,
    reference: &str,
) -> Option<PathBuf> {
    let root = repo.canonicalize().ok()?;
    let path = Path::new(reference);
    if path.is_absolute() {
        return None;
    }
    if let Some(parent) = document.parent() {
        if let Some(found) = local_file(&root, &parent.join(path)) {
            return Some(found);
        }
    }
    if let Some(found) = local_file(&root, &root.join(path)) {
        return Some(found);
    }
    // A workflow is named by its file name in prose (`estate-rules.yml`, or
    // `workflows/estate-rules.yml`) but lives under .github/workflows/. Look
    // there first, so the reference resolves even when the same basename also
    // exists elsewhere (which would make the basename search ambiguous).
    if let Some(name) = workflow_file_name(path) {
        if let Some(found) = local_file(&root, &root.join(".github/workflows").join(name)) {
            return Some(found);
        }
    }
    // A qualified missing path must not be "repaired" by a same-named file
    // elsewhere. Nor may generated output stand in for a source module.
    if path.components().count() != 1 {
        return None;
    }
    let name = path.file_name()?;
    let mut pending = vec![root.clone()];
    let mut found = None;
    while let Some(directory) = pending.pop() {
        for entry in std::fs::read_dir(directory).ok()? {
            let entry = entry.ok()?;
            let kind = entry.file_type().ok()?;
            if kind.is_dir() {
                if !GENERATED_DIRS
                    .iter()
                    .any(|excluded| entry.file_name() == *excluded)
                {
                    pending.push(entry.path());
                }
            } else if kind.is_file() && entry.file_name() == name {
                let candidate = local_file(&root, &entry.path())?;
                if found.is_some() {
                    return None; // Ambiguous basenames still need an explicit path.
                }
                found = Some(candidate);
            }
        }
    }
    found
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    static NEXT: AtomicUsize = AtomicUsize::new(0);

    struct Scratch(PathBuf);
    impl Scratch {
        fn new() -> Self {
            let path = std::env::temp_dir().join(format!(
                "finishing-reference-{}-{}",
                std::process::id(),
                NEXT.fetch_add(1, Ordering::Relaxed)
            ));
            std::fs::create_dir_all(&path).unwrap();
            Self(path)
        }
        fn file(&self, path: &str) -> PathBuf {
            let path = self.0.join(path);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(&path, "source fixture").unwrap();
            path
        }
        fn resolves(&self, document: &str, reference: &str) -> bool {
            resolve_local_reference(&self.0, &self.0.join(document), reference).is_some()
        }
    }
    impl Drop for Scratch {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn accepts_document_relative_and_repository_relative_paths() {
        let s = Scratch::new();
        s.file("docs/theory/Example.agda");
        s.file("proofs/agda/Echo.agda");
        assert!(s.resolves("docs/theory/note.adoc", "Example.agda"));
        assert!(s.resolves("docs/theory/note.adoc", "../../proofs/agda/Echo.agda"));
        assert!(s.resolves("docs/theory/note.adoc", "proofs/agda/Echo.agda"));
    }

    #[test]
    fn accepts_only_unique_unqualified_module_names() {
        let s = Scratch::new();
        s.file("proofs/agda/Echo.agda");
        assert!(s.resolves("docs/note.adoc", "Echo.agda"));
        s.file("examples/Echo.agda");
        assert!(!s.resolves("docs/note.adoc", "Echo.agda"));
    }

    #[test]
    fn workflow_names_resolve_under_dot_github_workflows() {
        let s = Scratch::new();
        s.file(".github/workflows/estate-rules.yml");
        assert!(s.resolves("docs/note.adoc", "estate-rules.yml"));
        assert!(s.resolves("docs/note.adoc", "workflows/estate-rules.yml"));
        assert!(s.resolves("docs/note.adoc", ".github/workflows/estate-rules.yml"));
        // Ambiguous basenames elsewhere do not defeat the workflow directory.
        s.file("tools/estate-rules.yml");
        assert!(s.resolves("docs/note.adoc", "estate-rules.yml"));
    }

    #[test]
    fn a_workflow_name_with_no_such_workflow_is_still_unresolved() {
        let s = Scratch::new();
        s.file(".github/workflows/other.yml");
        assert!(!s.resolves("docs/note.adoc", "estate-rules.yml"));
        assert!(!s.resolves("docs/note.adoc", "workflows/estate-rules.yml"));
    }

    #[test]
    fn a_missing_qualified_path_is_not_satisfied_by_another_basename() {
        let s = Scratch::new();
        s.file("proofs/agda/Echo.agda");
        assert!(!s.resolves("docs/note.adoc", "missing/Echo.agda"));
        assert!(!s.resolves("docs/note.adoc", "NeverExisted.agda"));
    }

    #[test]
    fn build_output_and_directories_do_not_satisfy_basename_references() {
        let s = Scratch::new();
        s.file("build/generated/Echo.agda");
        std::fs::create_dir_all(s.0.join("proofs/Echo.agda")).unwrap();
        assert!(!s.resolves("docs/note.adoc", "Echo.agda"));
    }

    #[test]
    fn absolute_and_outside_paths_do_not_count_as_repository_files() {
        let s = Scratch::new();
        let outside = Scratch::new();
        let file = outside.file("outside.agda");
        assert!(!s.resolves("docs/note.adoc", file.to_str().unwrap()));
        let parent_ref = format!(
            "../{}/outside.agda",
            outside.0.file_name().unwrap().to_str().unwrap()
        );
        assert!(!s.resolves("note.adoc", &parent_ref));
    }
}
