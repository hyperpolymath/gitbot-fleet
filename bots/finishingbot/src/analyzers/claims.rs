// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Claim verification analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use pulldown_cmark::{Event, Parser, Tag};
use regex::Regex;
use std::path::Path;
use tracing::debug;
use walkdir::WalkDir;

/// Claim verification analyzer
pub struct ClaimsAnalyzer;

impl Default for ClaimsAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for ClaimsAnalyzer {
    fn name(&self) -> &str {
        "Claim Verification"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Check for README
        if config.claims.require_readme {
            self.check_readme(path, &mut result);
        }

        // Check for CHANGELOG
        if config.claims.require_changelog {
            self.check_changelog(path, &mut result);
        }

        // Verify documentation claims
        if config.claims.verify_docs {
            self.verify_documentation_claims(path, config, &mut result);
        }

        // Verify test claims
        if config.claims.verify_tests {
            self.verify_test_claims(path, config, &mut result);
        }

        // Verify dependencies
        if config.claims.verify_dependencies {
            self.verify_dependencies(path, &mut result);
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, _path: &Path, _config: &Config, _findings: &[Finding]) -> Result<Vec<String>> {
        // Most claim issues require manual intervention
        Ok(Vec::new())
    }
}

impl ClaimsAnalyzer {
    /// Check for README file
    fn check_readme(&self, path: &Path, result: &mut AnalysisResult) {
        let readme_variants = [
            "README.md",
            "README.adoc",
            "README.asciidoc",
            "README.txt",
            "README.rst",
            "README",
            "readme.md",
        ];

        let found = readme_variants.iter().find(|name| path.join(name).exists());

        match found {
            None => {
                result.add(
                    Finding::new(
                        "CLM-001",
                        "Missing README",
                        Severity::Error,
                        "No README file found in repository root",
                    )
                    .with_suggestion("Create a README.md or README.adoc describing the project"),
                );
            }
            Some(readme_name) => {
                let readme_path = path.join(readme_name);
                if let Ok(content) = std::fs::read_to_string(&readme_path) {
                    self.validate_readme_content(&content, &readme_path, result);
                }
            }
        }
    }

    /// Validate README content quality
    fn validate_readme_content(&self, content: &str, path: &Path, result: &mut AnalysisResult) {
        result.files_checked += 1;

        let word_count = content.split_whitespace().count();

        // Check minimum content
        if word_count < 50 {
            result.add(
                Finding::new(
                    "CLM-002",
                    "Sparse README",
                    Severity::Warning,
                    &format!("README is very short ({} words)", word_count),
                )
                .with_file(path.to_path_buf())
                .with_suggestion("Add more documentation including installation, usage, and examples"),
            );
        }

        // Check for essential sections
        let content_lower = content.to_lowercase();
        let essential_sections = [
            ("installation", "Installation instructions"),
            ("usage", "Usage examples"),
            ("license", "License information"),
        ];

        for (section, description) in &essential_sections {
            if !content_lower.contains(section) {
                result.add(
                    Finding::new(
                        "CLM-003",
                        "Missing README Section",
                        Severity::Info,
                        &format!("README missing '{}' section", description),
                    )
                    .with_file(path.to_path_buf())
                    .with_suggestion(&format!("Add a section about: {}", description)),
                );
            }
        }

        // Check for broken internal links in markdown
        if path.extension().map(|e| e == "md").unwrap_or(false) {
            self.check_markdown_links(content, path, result);
        }
    }

    /// Check for CHANGELOG file
    fn check_changelog(&self, path: &Path, result: &mut AnalysisResult) {
        let changelog_variants = [
            "CHANGELOG.md",
            "CHANGELOG.adoc",
            "CHANGELOG.txt",
            "CHANGELOG",
            "HISTORY.md",
            "CHANGES.md",
            "changelog.md",
        ];

        if !changelog_variants.iter().any(|name| path.join(name).exists()) {
            result.add(
                Finding::new(
                    "CLM-004",
                    "Missing CHANGELOG",
                    Severity::Warning,
                    "No CHANGELOG file found for release tracking",
                )
                .with_suggestion("Create a CHANGELOG.md following Keep a Changelog format"),
            );
        }
    }

    /// Check markdown links for broken references
    fn check_markdown_links(&self, content: &str, path: &Path, result: &mut AnalysisResult) {
        let parser = Parser::new(content);
        let root = path.parent().unwrap_or(Path::new("."));

        for event in parser {
            if let Event::Start(Tag::Link { dest_url, .. }) = event {
                let url = dest_url.to_string();

                // Skip external URLs
                if url.starts_with("http://") || url.starts_with("https://") {
                    continue;
                }

                // Skip anchors
                if url.starts_with('#') {
                    continue;
                }

                // Check if local file exists
                let link_path = root.join(&url);
                if !link_path.exists() {
                    result.add(
                        Finding::new(
                            "CLM-005",
                            "Broken Link",
                            Severity::Warning,
                            &format!("Documentation references non-existent file: {}", url),
                        )
                        .with_file(path.to_path_buf())
                        .with_suggestion("Update the link or create the referenced file"),
                    );
                }
            }
        }
    }

    /// Verify documentation claims match reality
    fn verify_documentation_claims(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        // Find all documentation files
        for entry in WalkDir::new(path)
            .follow_links(false)
            .max_depth(3)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            let entry_path = entry.path();

            if !entry_path.is_file() {
                continue;
            }

            let is_doc = entry_path
                .extension()
                .and_then(|e| e.to_str())
                .map(|ext| matches!(ext, "md" | "adoc" | "asciidoc" | "rst"))
                .unwrap_or(false);

            if !is_doc {
                continue;
            }

            if let Ok(content) = std::fs::read_to_string(entry_path) {
                result.files_checked += 1;

                // Check for code blocks that reference files
                self.verify_code_file_claims(&content, entry_path, path, result);

                // Check for feature claims
                self.verify_feature_claims(&content, entry_path, path, result);
            }
        }
    }

    /// Verify that code examples reference existing files
    fn verify_code_file_claims(
        &self,
        content: &str,
        doc_path: &Path,
        repo_root: &Path,
        result: &mut AnalysisResult,
    ) {
        // Look for patterns like "see src/main.rs" or "in `lib.rs`"
        let file_ref_patterns = [
            Regex::new(r"see\s+`?([a-zA-Z0-9_./\-]+\.[a-z]+)`?").unwrap(),
            Regex::new(r"in\s+`([a-zA-Z0-9_./\-]+\.[a-z]+)`").unwrap(),
            Regex::new(r"file\s+`([a-zA-Z0-9_./\-]+\.[a-z]+)`").unwrap(),
        ];

        for pattern in &file_ref_patterns {
            for caps in pattern.captures_iter(content) {
                let file_ref = &caps[1];
                let ref_path = repo_root.join(file_ref);

                if !ref_path.exists() {
                    debug!(doc = %doc_path.display(), reference = file_ref, "Broken file reference");
                    result.add(
                        Finding::new(
                            "CLM-006",
                            "Broken File Reference",
                            Severity::Warning,
                            &format!("Documentation references non-existent file: {}", file_ref),
                        )
                        .with_file(doc_path.to_path_buf())
                        .with_suggestion("Update reference or create the missing file"),
                    );
                }
            }
        }
    }

    /// Verify feature claims have corresponding implementation
    fn verify_feature_claims(
        &self,
        content: &str,
        doc_path: &Path,
        _repo_root: &Path,
        result: &mut AnalysisResult,
    ) {
        // Look for feature list patterns
        let feature_indicators = [
            "- [ ]",     // Incomplete checkbox
            "* [ ]",     // Incomplete checkbox variant
            "[WIP]",     // Work in progress
            "[PLANNED]", // Planned feature
        ];

        for indicator in &feature_indicators {
            if content.contains(indicator) {
                let count = content.matches(indicator).count();
                result.add(
                    Finding::new(
                        "CLM-007",
                        "Incomplete Features Listed",
                        Severity::Info,
                        &format!(
                            "Documentation contains {} incomplete/planned feature markers ({})",
                            count, indicator
                        ),
                    )
                    .with_file(doc_path.to_path_buf())
                    .with_suggestion("Review feature list before release"),
                );
            }
        }
    }

    /// Verify test claims and coverage
    fn verify_test_claims(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        // Look for test directories and files
        let test_indicators = ["tests", "test", "spec", "__tests__"];
        let test_file_patterns = ["_test.rs", "_test.go", "_test.py", ".test.js", ".test.ts", ".spec.js", ".spec.ts"];

        let mut has_tests = false;
        let mut test_file_count = 0;

        for entry in WalkDir::new(path)
            .follow_links(false)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            let entry_path = entry.path();
            let name = entry.file_name().to_string_lossy();

            // Check for test directories
            if entry_path.is_dir() && test_indicators.contains(&name.as_ref()) {
                has_tests = true;
            }

            // Check for test files
            if entry_path.is_file() {
                let name_str = name.to_string();
                if test_file_patterns.iter().any(|p| name_str.ends_with(p)) {
                    has_tests = true;
                    test_file_count += 1;
                }

                // Rust: check for #[test] in source files
                if name_str.ends_with(".rs") {
                    if let Ok(content) = std::fs::read_to_string(entry_path) {
                        if content.contains("#[test]") || content.contains("#[cfg(test)]") {
                            has_tests = true;
                            test_file_count += 1;
                        }
                    }
                }
            }
        }

        if !has_tests {
            result.add(
                Finding::new(
                    "CLM-008",
                    "No Tests Found",
                    Severity::Warning,
                    "No test files or test directories found in repository",
                )
                .with_suggestion("Add tests to verify code functionality"),
            );
        } else {
            debug!(test_files = test_file_count, "Tests found");
        }

        // Check for CI workflow that runs tests
        self.check_ci_tests(path, result);
    }

    /// Check if CI runs tests
    fn check_ci_tests(&self, path: &Path, result: &mut AnalysisResult) {
        let ci_paths = [
            ".github/workflows",
            ".gitlab-ci.yml",
            ".travis.yml",
            "Jenkinsfile",
            ".circleci",
        ];

        let has_ci = ci_paths.iter().any(|p| path.join(p).exists());

        if !has_ci {
            result.add(
                Finding::new(
                    "CLM-009",
                    "No CI Configuration",
                    Severity::Info,
                    "No continuous integration configuration found",
                )
                .with_suggestion("Add CI/CD configuration to automatically run tests"),
            );
        }
    }

    /// Verify declared dependencies exist
    fn verify_dependencies(&self, path: &Path, result: &mut AnalysisResult) {
        // Check Cargo.toml for Rust
        let cargo_toml = path.join("Cargo.toml");
        if cargo_toml.exists() {
            self.verify_cargo_dependencies(&cargo_toml, result);
        }

        // Check package.json for JS/TS
        let package_json = path.join("package.json");
        if package_json.exists() {
            self.verify_npm_dependencies(&package_json, result);
        }

        // Check deno.json
        let deno_json = path.join("deno.json");
        if deno_json.exists() {
            self.verify_deno_dependencies(&deno_json, result);
        }
    }

    fn verify_cargo_dependencies(&self, cargo_path: &Path, result: &mut AnalysisResult) {
        if let Ok(content) = std::fs::read_to_string(cargo_path) {
            // Simple check: look for workspace members that don't exist
            if content.contains("[workspace]") {
                let member_re = Regex::new(r#"members\s*=\s*\[([\s\S]*?)\]"#).unwrap();
                if let Some(caps) = member_re.captures(&content) {
                    let members_str = &caps[1];
                    let path_re = Regex::new(r#""([^"]+)""#).unwrap();
                    for cap in path_re.captures_iter(members_str) {
                        let member_path = cargo_path.parent().unwrap().join(&cap[1]);
                        if !member_path.exists() {
                            result.add(
                                Finding::new(
                                    "CLM-010",
                                    "Missing Workspace Member",
                                    Severity::Error,
                                    &format!("Workspace member does not exist: {}", &cap[1]),
                                )
                                .with_file(cargo_path.to_path_buf())
                                .with_suggestion("Remove from workspace or create the directory"),
                            );
                        }
                    }
                }
            }
        }
    }

    fn verify_npm_dependencies(&self, _package_path: &Path, _result: &mut AnalysisResult) {
        // Could verify package-lock.json matches package.json
        // Left as basic check for now
    }

    fn verify_deno_dependencies(&self, _deno_path: &Path, _result: &mut AnalysisResult) {
        // Could verify import map entries
        // Left as basic check for now
    }
}
