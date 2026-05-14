// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Tooling and version management analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use tracing::debug;

/// Tooling and version management analyzer
pub struct ToolingAnalyzer;

impl Default for ToolingAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for ToolingAnalyzer {
    fn name(&self) -> &str {
        "Tooling & Version Management"
    }

    fn analyze(&self, path: &Path, _config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Check for .tool-versions (asdf/mise)
        let tool_versions = path.join(".tool-versions");
        if !tool_versions.exists() {
            result.add(
                Finding::new(
                    "TOOL-001",
                    "Missing .tool-versions",
                    Severity::Error,
                    ".tool-versions file not found for asdf/mise version management.",
                )
                .with_suggestion("Create .tool-versions file with required tool versions (rust, just, etc.)"),
            );
        } else {
            debug!("Found .tool-versions");

            // Validate .tool-versions content
            if let Ok(content) = std::fs::read_to_string(&tool_versions) {
                self.validate_tool_versions(&content, &tool_versions, &mut result);
            }
        }

        // Check for editor config
        let editorconfig = path.join(".editorconfig");
        if !editorconfig.exists() {
            result.add(
                Finding::new(
                    "TOOL-002",
                    "Missing .editorconfig",
                    Severity::Warning,
                    ".editorconfig not found for consistent code formatting.",
                )
                .with_suggestion("Add .editorconfig for consistent editor settings"),
            );
        } else {
            debug!("Found .editorconfig");
        }

        // Check for CI workflows
        let workflows_dir = path.join(".github/workflows");
        if !workflows_dir.exists() {
            result.add(
                Finding::new(
                    "TOOL-003",
                    "Missing CI workflows",
                    Severity::Error,
                    ".github/workflows/ directory not found.",
                )
                .with_suggestion("Add CI/CD workflows for automated testing and quality checks"),
            );
        } else {
            debug!("Found .github/workflows/");

            // Count workflows
            if let Ok(entries) = std::fs::read_dir(&workflows_dir) {
                let workflow_count = entries
                    .filter_map(|e| e.ok())
                    .filter(|e| e.path().extension().map_or(false, |ext| ext == "yml" || ext == "yaml"))
                    .count();

                if workflow_count < 10 {
                    result.add(
                        Finding::new(
                            "TOOL-004",
                            "Few CI workflows",
                            Severity::Warning,
                            &format!("Only {} workflows found. V1-ready repos typically have 15-35 workflows.", workflow_count),
                        )
                        .with_file(workflows_dir.clone())
                        .with_suggestion("Add comprehensive CI workflows (CodeQL, security scanning, tests, linting)"),
                    );
                } else {
                    debug!("Found {} workflows", workflow_count);
                }
            }
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, _path: &Path, _config: &Config, _findings: &[Finding]) -> Result<Vec<String>> {
        Ok(vec![
            "Tooling files should be manually created.".to_string(),
            "Use templates from hyperpolymath repos.".to_string(),
        ])
    }
}

impl ToolingAnalyzer {
    /// Validate .tool-versions content
    fn validate_tool_versions(
        &self,
        content: &str,
        file_path: &Path,
        result: &mut AnalysisResult,
    ) {
        let is_rust = file_path
            .parent()
            .and_then(|p| Some(p.join("Cargo.toml").exists()))
            .unwrap_or(false);

        // Check for rust version if Rust project
        if is_rust && !content.contains("rust ") {
            result.add(
                Finding::new(
                    "TOOL-005",
                    "Missing rust version in .tool-versions",
                    Severity::Warning,
                    "Rust project should specify rust version in .tool-versions.",
                )
                .with_file(file_path.to_path_buf())
                .with_suggestion("Add 'rust <version>' to .tool-versions"),
            );
        }

        // Check for just (task runner)
        if is_rust && !content.contains("just ") {
            result.add(
                Finding::new(
                    "TOOL-006",
                    "Missing just version in .tool-versions",
                    Severity::Info,
                    "Consider adding 'just' task runner to .tool-versions.",
                )
                .with_file(file_path.to_path_buf())
                .with_suggestion("Add 'just <version>' to .tool-versions if using justfile"),
            );
        }

        // Check for empty or very short file
        if content.trim().is_empty() {
            result.add(
                Finding::new(
                    "TOOL-007",
                    "Empty .tool-versions",
                    Severity::Error,
                    ".tool-versions file is empty.",
                )
                .with_file(file_path.to_path_buf())
                .with_suggestion("Add tool versions (e.g., 'rust 1.83.0')"),
            );
        }
    }
}
