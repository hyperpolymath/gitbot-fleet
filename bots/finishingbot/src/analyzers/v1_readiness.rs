// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! V1 release readiness analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
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
        let required_docs = [
            ("README.adoc", "Main project documentation"),
            ("ROADMAP.adoc", "Project roadmap"),
            ("LICENSE", "License file"),
        ];

        for (filename, description) in &required_docs {
            let file_path = path.join(filename);
            if !file_path.exists() {
                result.add(
                    Finding::new(
                        &format!("V1-DOC-{}", filename),
                        &format!("Missing {}", filename),
                        Severity::Error,
                        &format!("Required file {} not found ({}).", filename, description),
                    )
                    .with_suggestion(&format!("Create {} file", filename)),
                );
            } else {
                debug!("Found {}", filename);
            }
        }
    }

    /// Check for security files
    fn check_security_files(&self, path: &Path, result: &mut AnalysisResult) {
        let security_file = path.join("SECURITY.md");
        if !security_file.exists() {
            result.add(
                Finding::new(
                    "V1-SEC-001",
                    "Missing SECURITY.md",
                    Severity::Warning,
                    "SECURITY.md not found for responsible disclosure.",
                )
                .with_suggestion("Add SECURITY.md with security policy and contact information"),
            );
        } else {
            debug!("Found SECURITY.md");
        }
    }

    /// Check language compliance (no TypeScript, Python, Go, npm)
    fn check_language_compliance(&self, path: &Path, result: &mut AnalysisResult) {
        use walkdir::WalkDir;

        // Check for banned language files
        let banned_patterns = [
            ("*.ts", "TypeScript files found (use ReScript instead)"),
            ("*.py", "Python files found (use Julia/Rust/ReScript instead)"),
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
                if entry.path().extension().map_or(false, |e| e == ext) {
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
                    .with_suggestion(&format!("Remove or convert {} files to allowed languages", pattern)),
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
        let community_files = [
            ("CODE_OF_CONDUCT.md", "Code of conduct"),
            ("CONTRIBUTING.md", "Contribution guidelines"),
        ];

        for (filename, description) in &community_files {
            let file_path = path.join(filename);
            if !file_path.exists() {
                result.add(
                    Finding::new(
                        &format!("V1-COM-{}", filename),
                        &format!("Missing {}", filename),
                        Severity::Warning,
                        &format!("{} file not found ({}).", filename, description),
                    )
                    .with_suggestion(&format!("Add {} for community health", filename)),
                );
            } else {
                debug!("Found {}", filename);
            }
        }
    }
}
