// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! V1 release readiness analyzer

use super::{first_existing, AnalysisResult, Analyzer, Finding, Severity, SECURITY_POLICY};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use tracing::debug;

/// V1 release readiness analyzer
pub struct V1ReadinessAnalyzer;

impl Default for V1ReadinessAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for V1ReadinessAnalyzer {
    fn name(&self) -> &str {
        "V1 Release Readiness"
    }

    fn analyze(&self, path: &Path, _config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Core documentation files
        self.check_core_docs(path, &mut result);

        // Security files
        self.check_security_files(path, &mut result);

        // Language compliance (no banned languages)
        self.check_language_compliance(path, &mut result);

        // Community files
        self.check_community_files(path, &mut result);

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, _path: &Path, _config: &Config, _findings: &[Finding]) -> Result<Vec<String>> {
        Ok(vec![
            "V1 readiness issues should be addressed manually.".to_string(),
            "Follow hyperpolymath v1 release checklist.".to_string(),
        ])
    }
}

impl V1ReadinessAnalyzer {
    /// Check for core documentation files
    fn check_core_docs(&self, path: &Path, result: &mut AnalysisResult) {
        // (name, description, accepted locations in preference order). The
        // first location is the canonical one and is the path reported when
        // the document is missing. Estate canon keeps the roadmap at
        // docs/status/ROADMAP.adoc (the STATE journal).
        let required_docs: [(&str, &str, &[&str]); 3] = [
            (
                "README.adoc",
                "Main project documentation",
                &["README.adoc"],
            ),
            (
                "ROADMAP.adoc",
                "Project roadmap",
                &["docs/status/ROADMAP.adoc", "ROADMAP.adoc"],
            ),
            ("LICENSE", "License file", &["LICENSE"]),
        ];

        for (filename, description, locations) in &required_docs {
            if first_existing(path, locations).is_none() {
                result.add(
                    Finding::new(
                        &format!("V1-DOC-{}", filename),
                        &format!("Missing {}", filename),
                        Severity::Error,
                        &format!("Required file {} not found ({}).", filename, description),
                    )
                    .with_file(path.join(locations[0]))
                    .with_suggestion(&format!("Create {} file", locations[0])),
                );
            } else {
                debug!("Found {}", filename);
            }
        }
    }

    /// Check for security files
    fn check_security_files(&self, path: &Path, result: &mut AnalysisResult) {
        if first_existing(path, SECURITY_POLICY).is_none() {
            result.add(
                Finding::new(
                    "V1-SEC-001",
                    "Missing SECURITY.md",
                    Severity::Warning,
                    "No security policy (SECURITY.md or .github/SECURITY.md) found for responsible disclosure.",
                )
                .with_file(path.join(SECURITY_POLICY[0]))
                .with_suggestion("Add SECURITY.md with security policy and contact information"),
            );
        } else {
            debug!("Found security policy");
        }
    }

    /// Check language compliance (no TypeScript, Python, Go, npm)
    fn check_language_compliance(&self, path: &Path, result: &mut AnalysisResult) {
        use walkdir::WalkDir;

        // Check for banned language files
        let banned_patterns = [
            ("*.ts", "TypeScript files found (use AffineScript instead)"),
            (
                "*.py",
                "Python files found (use Julia/Rust/AffineScript instead)",
            ),
            ("*.go", "Go files found (use Rust instead)"),
        ];

        for (pattern, message) in &banned_patterns {
            let ext = pattern.trim_start_matches("*.");
            let mut found_files = Vec::new();

            for entry in WalkDir::new(path)
                .into_iter()
                .filter_entry(|e| {
                    let name = e.file_name().to_string_lossy();
                    !name.starts_with('.') && name != "target" && name != "node_modules"
                })
                .filter_map(|e| e.ok())
            {
                if entry.path().extension().is_some_and(|e| e == ext) {
                    found_files.push(entry.path().to_path_buf());
                    if found_files.len() >= 5 {
                        break; // Stop after finding 5 examples
                    }
                }
            }

            if !found_files.is_empty() {
                result.add(
                    Finding::new(
                        &format!("V1-LANG-{}", ext),
                        &format!("Banned language: {}", pattern),
                        Severity::Error,
                        &format!("{}. Found {} file(s).", message, found_files.len()),
                    )
                    .with_file(found_files[0].clone())
                    .with_suggestion(&format!(
                        "Remove or convert {} files to allowed languages",
                        pattern
                    )),
                );
            }
        }

        // Check for node_modules (should use Deno only)
        let node_modules = path.join("node_modules");
        if node_modules.exists() && path.join("deno.json").exists() {
            result.add(
                Finding::new(
                    "V1-LANG-npm",
                    "node_modules directory found",
                    Severity::Error,
                    "node_modules/ found in Deno project (violates Deno-only policy).",
                )
                .with_file(node_modules)
                .with_suggestion("Remove node_modules/ and use Deno's dependency caching"),
            );
        }
    }

    /// Check for community files
    fn check_community_files(&self, path: &Path, result: &mut AnalysisResult) {
        // The rule id keeps the historical .md spelling; the estate writes
        // these as .adoc, and GitHub reads them from the root or .github/.
        let community_files: [(&str, &str, &[&str]); 2] = [
            (
                "CODE_OF_CONDUCT.md",
                "Code of conduct",
                &[
                    "CODE_OF_CONDUCT.md",
                    "CODE_OF_CONDUCT.adoc",
                    ".github/CODE_OF_CONDUCT.md",
                    ".github/CODE_OF_CONDUCT.adoc",
                ],
            ),
            (
                "CONTRIBUTING.md",
                "Contribution guidelines",
                &[
                    "CONTRIBUTING.md",
                    "CONTRIBUTING.adoc",
                    ".github/CONTRIBUTING.md",
                    ".github/CONTRIBUTING.adoc",
                ],
            ),
        ];

        for (filename, description, locations) in &community_files {
            if first_existing(path, locations).is_none() {
                result.add(
                    Finding::new(
                        &format!("V1-COM-{}", filename),
                        &format!("Missing {}", filename),
                        Severity::Warning,
                        &format!("{} file not found ({}).", filename, description),
                    )
                    .with_file(path.join(locations[0]))
                    .with_suggestion(&format!("Add {} for community health", filename)),
                );
            } else {
                debug!("Found {}", filename);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn run(dir: &Path) -> Vec<Finding> {
        V1ReadinessAnalyzer
            .analyze(dir, &Config::default())
            .unwrap()
            .findings
    }

    fn ids(dir: &Path) -> Vec<String> {
        run(dir).into_iter().map(|f| f.id).collect()
    }

    fn touch(dir: &Path, rel: &str) {
        let p = dir.join(rel);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, "x").unwrap();
    }

    #[test]
    fn roadmap_is_missing_when_absent_and_names_the_canonical_path() {
        let d = TempDir::new().unwrap();
        let f = run(d.path())
            .into_iter()
            .find(|f| f.id == "V1-DOC-ROADMAP.adoc")
            .expect("missing roadmap is reported");
        assert_eq!(
            f.file.as_deref(),
            Some(d.path().join("docs/status/ROADMAP.adoc").as_path())
        );
    }

    #[test]
    fn roadmap_accepts_docs_status_and_root() {
        for rel in ["docs/status/ROADMAP.adoc", "ROADMAP.adoc"] {
            let d = TempDir::new().unwrap();
            touch(d.path(), rel);
            assert!(
                !ids(d.path()).contains(&"V1-DOC-ROADMAP.adoc".to_string()),
                "{rel}"
            );
        }
    }

    #[test]
    fn community_files_accept_adoc_and_dot_github() {
        let none = TempDir::new().unwrap();
        let got = ids(none.path());
        assert!(got.contains(&"V1-COM-CONTRIBUTING.md".to_string()));
        assert!(got.contains(&"V1-COM-CODE_OF_CONDUCT.md".to_string()));
        for rel in [
            "CONTRIBUTING.md",
            "CONTRIBUTING.adoc",
            ".github/CONTRIBUTING.md",
            ".github/CONTRIBUTING.adoc",
        ] {
            let d = TempDir::new().unwrap();
            touch(d.path(), rel);
            assert!(
                !ids(d.path()).contains(&"V1-COM-CONTRIBUTING.md".to_string()),
                "{rel}"
            );
        }
        for rel in ["CODE_OF_CONDUCT.adoc", ".github/CODE_OF_CONDUCT.md"] {
            let d = TempDir::new().unwrap();
            touch(d.path(), rel);
            assert!(
                !ids(d.path()).contains(&"V1-COM-CODE_OF_CONDUCT.md".to_string()),
                "{rel}"
            );
        }
        // Accepting one document does not excuse the other.
        let d = TempDir::new().unwrap();
        touch(d.path(), "CONTRIBUTING.adoc");
        assert!(ids(d.path()).contains(&"V1-COM-CODE_OF_CONDUCT.md".to_string()));
    }

    #[test]
    fn security_policy_accepts_dot_github() {
        let none = TempDir::new().unwrap();
        assert!(ids(none.path()).contains(&"V1-SEC-001".to_string()));
        let d = TempDir::new().unwrap();
        touch(d.path(), ".github/SECURITY.md");
        assert!(!ids(d.path()).contains(&"V1-SEC-001".to_string()));
    }
}
