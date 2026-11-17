// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Repo-deed analyzer (formerly the SCM documentation files analyzer).
//!
//! The six descriptive documents that used to be `STATE.scm`, `META.scm`,
//! `ECOSYSTEM.scm`, `PLAYBOOK.scm`, `AGENTIC.scm` and `NEUROSYM.scm` are
//! clauses of the repository's one `<repo>_chora.deed` (owner rulings D312,
//! D313), and the project state is its `(status …)` or `(maturity …)` clause, or
//! `.machine_readable/descriptiles/STATE.deed` once standards' A7 migration
//! moves it there (J15). This analyzer therefore reads the deed, through
//! `gitbot-deed-reader`, and treats any surviving `.scm` document as legacy:
//! it says "migrate", never "move" or "complete" (the earlier
//! direct-under-`.machine_readable/` layout is retired and must not be
//! restored).
//!
//! The type and the report key keep their old names (`ScmFilesAnalyzer`,
//! `scm_files`) so the library API and the JSON report shape do not change.
//!
//! Finding ids:
//! - `DEED-001` (warning): no `<repo>_chora.deed` at the root.
//! - `DEED-002` (error): more than one chora deed (D312 allows exactly one).
//! - `DEED-003` (error): the chora deed cannot be read or parsed, or its
//!   doc-head is not `repo-deed`.
//! - `DEED-004` (warning): no project state: neither a `(status …)` nor a `(maturity …)` clause in
//!   the chora deed nor a `.machine_readable/descriptiles/STATE.deed`.
//! - `SCM-006-<NAME>.scm` (warning): a retired `.scm` document is present.
//!
//! Retired: `SCM-001` (missing `.scm`), `SCM-002`/`003`/`004` (validating a
//! retired file's content) and `SCM-005` (which advised MOVING a root `.scm`
//! into `.machine_readable/`, i.e. restoring a retired layout).

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use deed_reader::{load_chora, ChoraError};
use std::path::Path;
use tracing::debug;

/// The retired descriptive documents and the deed clause each one became.
const LEGACY_SCM: [(&str, &str); 6] = [
    ("STATE.scm", "status"),
    ("META.scm", "meta"),
    ("ECOSYSTEM.scm", "ecosystem"),
    ("PLAYBOOK.scm", "playbook"),
    ("AGENTIC.scm", "agentic"),
    ("NEUROSYM.scm", "neurosym"),
];

/// Where a legacy `.scm` document may still be found: the root, the retired
/// direct-under-`.machine_readable/` layout and the retired `6scm/` layout.
const LEGACY_DIRS: [&str; 3] = ["", ".machine_readable", ".machine_readable/6scm"];

/// Repo-deed analyzer (historical name kept for API stability).
pub struct ScmFilesAnalyzer;

impl Default for ScmFilesAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for ScmFilesAnalyzer {
    fn name(&self) -> &str {
        "Repo Deed (chora)"
    }

    fn analyze(&self, path: &Path, _config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        let has_state_deed = path
            .join(".machine_readable/descriptiles/STATE.deed")
            .is_file();

        match load_chora(path) {
            Ok(Some(chora)) => {
                result.files_checked += 1;
                debug!("read {}", chora.path.display());
                if !chora.root.has("status") && !chora.root.has("maturity") && !has_state_deed {
                    result.add(
                        Finding::new(
                            "DEED-004",
                            "Project state not recorded",
                            Severity::Warning,
                            "The chora deed has neither a (status …) nor a (maturity …) clause and there is no \
                             .machine_readable/descriptiles/STATE.deed, so the project's \
                             phase is not recorded anywhere a bot can read.",
                        )
                        .with_file(chora.path.clone())
                        .with_suggestion(
                            "Add a (status …) clause to the chora deed (standards \
                             1-formats/deed/mappings/clade-to-repo-deed.adoc §3), and check it \
                             with `bun 1-formats/deed/tools/deed_lint.js`",
                        ),
                    );
                }
            }
            Ok(None) => {
                result.add(
                    Finding::new(
                        "DEED-001",
                        "No chora deed",
                        Severity::Warning,
                        "No <repo>_chora.deed at the repository root. Repo facts (state, meta, \
                         ecosystem, agentic, neurosym, playbook) are clauses of that one deed \
                         (D312).",
                    )
                    .with_file(path.to_path_buf())
                    .with_suggestion(
                        "Mint the deed from rsr-template-repo's model \
                         (rsr-template-repo_chora.deed); bots read deeds but never write them",
                    ),
                );
            }
            Err(ChoraError::Multiple(paths)) => {
                let names: Vec<String> = paths
                    .iter()
                    .filter_map(|p| p.file_name().map(|n| n.to_string_lossy().into_owned()))
                    .collect();
                result.add(
                    Finding::new(
                        "DEED-002",
                        "More than one chora deed",
                        Severity::Error,
                        &format!(
                            "D312 allows exactly one chora deed per repository; found {}.",
                            names.join(", ")
                        ),
                    )
                    .with_file(path.to_path_buf())
                    .with_suggestion("Keep the deed whose stem is the repository's name"),
                );
            }
            Err(e) => {
                let (file, line) = match &e {
                    ChoraError::Parse(p, de) => (p.clone(), Some(de.line)),
                    ChoraError::Io(p, _) | ChoraError::WrongHead(p, _) => (p.clone(), None),
                    ChoraError::Multiple(_) => unreachable!("handled above"),
                };
                let mut f = Finding::new(
                    "DEED-003",
                    "Chora deed cannot be read",
                    Severity::Error,
                    &e.to_string(),
                )
                .with_file(file)
                .with_suggestion(
                    "Check it with standards' `bun 1-formats/deed/tools/deed_lint.js FILE`",
                );
                if let Some(l) = line {
                    f = f.with_line(l);
                }
                result.add(f);
            }
        }

        for dir in LEGACY_DIRS {
            for (name, clause) in LEGACY_SCM {
                let file = if dir.is_empty() {
                    path.join(name)
                } else {
                    path.join(dir).join(name)
                };
                if !file.is_file() {
                    continue;
                }
                result.files_checked += 1;
                result.add(
                    Finding::new(
                        &format!("SCM-006-{name}"),
                        &format!("Legacy {name} should be migrated"),
                        Severity::Warning,
                        &format!(
                            "{name} is retired: its facts belong in the ({clause} …) clause of \
                             the chora deed. It is not validated, and it must not be moved or \
                             extended."
                        ),
                    )
                    .with_file(file)
                    .with_suggestion(&format!(
                        "Migrate it into ({clause} …) under the standards mapping specs \
                         (1-formats/deed/mappings/), then delete it"
                    )),
                );
            }
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, _path: &Path, _config: &Config, _findings: &[Finding]) -> Result<Vec<String>> {
        // Bots read deeds; they never write one or hand-convert a legacy file.
        Ok(vec![
            "The chora deed is owner-authored: finishingbot does not write or convert it."
                .to_string(),
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    const DEED: &str = ";; SPDX-License-Identifier: MPL-2.0\n(repo-deed\n  :schema-version \"1.0.0\"\n  :canonical-name \"x\"\n  (status :phase beta))\n";

    fn ids(dir: &Path) -> Vec<String> {
        ScmFilesAnalyzer
            .analyze(dir, &Config::default())
            .unwrap()
            .findings
            .into_iter()
            .map(|f| f.id)
            .collect()
    }

    #[test]
    fn a_deed_with_status_and_no_scm_is_clean() {
        let d = TempDir::new().unwrap();
        std::fs::write(d.path().join("x_chora.deed"), DEED).unwrap();
        assert!(ids(d.path()).is_empty(), "{:?}", ids(d.path()));
    }

    #[test]
    fn no_deed_is_deed_001_and_absent_scm_is_not_a_finding() {
        let d = TempDir::new().unwrap();
        assert_eq!(ids(d.path()), vec!["DEED-001".to_string()]);
    }

    #[test]
    fn two_deeds_is_deed_002() {
        let d = TempDir::new().unwrap();
        std::fs::write(d.path().join("x_chora.deed"), DEED).unwrap();
        std::fs::write(d.path().join("y_chora.deed"), DEED).unwrap();
        assert_eq!(ids(d.path()), vec!["DEED-002".to_string()]);
    }

    #[test]
    fn an_unparseable_or_wrong_head_deed_is_deed_003_with_a_line() {
        let d = TempDir::new().unwrap();
        std::fs::write(
            d.path().join("x_chora.deed"),
            ";; SPDX-License-Identifier: MPL-2.0\n(repo-deed\n\n :a",
        )
        .unwrap();
        let r = ScmFilesAnalyzer
            .analyze(d.path(), &Config::default())
            .unwrap();
        let f = r.findings.iter().find(|f| f.id == "DEED-003").unwrap();
        assert_eq!(f.severity, Severity::Error);
        assert_eq!(f.line, Some(4));

        let d = TempDir::new().unwrap();
        std::fs::write(
            d.path().join("x_chora.deed"),
            ";; SPDX-License-Identifier: MPL-2.0\n(praxis-deed :a \"b\")\n",
        )
        .unwrap();
        assert_eq!(ids(d.path()), vec!["DEED-003".to_string()]);
    }

    #[test]
    fn missing_status_is_deed_004_unless_state_deed_exists() {
        let d = TempDir::new().unwrap();
        std::fs::write(
            d.path().join("x_chora.deed"),
            ";; SPDX-License-Identifier: MPL-2.0\n(repo-deed :canonical-name \"x\")\n",
        )
        .unwrap();
        assert_eq!(ids(d.path()), vec!["DEED-004".to_string()]);
        let dd = d.path().join(".machine_readable/descriptiles");
        std::fs::create_dir_all(&dd).unwrap();
        std::fs::write(dd.join("STATE.deed"), "(descriptile-deed)").unwrap();
        assert!(ids(d.path()).is_empty());
    }

    #[test]
    fn a_maturity_clause_alone_records_the_project_state() {
        // rhodibot's canon overlay lists STATE.deed, (status) and (maturity) as
        // the three alternatives; none of them is DEED-004.
        for clause in ["(maturity :level experimental)", "(status :phase beta)"] {
            let d = TempDir::new().unwrap();
            std::fs::write(
                d.path().join("x_chora.deed"),
                format!(";; SPDX-License-Identifier: MPL-2.0\n(repo-deed :canonical-name \"x\" {clause})\n"),
            )
            .unwrap();
            assert!(ids(d.path()).is_empty(), "{clause}");
        }
    }

    #[test]
    fn legacy_scm_is_reported_as_migrate_wherever_it_is_and_never_validated() {
        let d = TempDir::new().unwrap();
        std::fs::write(d.path().join("x_chora.deed"), DEED).unwrap();
        std::fs::write(d.path().join("STATE.scm"), ";; x\n").unwrap();
        std::fs::create_dir_all(d.path().join(".machine_readable/6scm")).unwrap();
        std::fs::write(d.path().join(".machine_readable/META.scm"), "(meta)").unwrap();
        std::fs::write(d.path().join(".machine_readable/6scm/ECOSYSTEM.scm"), "(e)").unwrap();
        let mut got = ids(d.path());
        got.sort();
        assert_eq!(
            got,
            vec![
                "SCM-006-ECOSYSTEM.scm".to_string(),
                "SCM-006-META.scm".to_string(),
                "SCM-006-STATE.scm".to_string()
            ]
        );
        let r = ScmFilesAnalyzer
            .analyze(d.path(), &Config::default())
            .unwrap();
        for f in &r.findings {
            assert_eq!(f.severity, Severity::Warning);
            let s = f.suggestion.as_deref().unwrap_or("");
            assert!(s.contains("Migrate") && !s.contains("Move"), "{s}");
        }
    }
}
