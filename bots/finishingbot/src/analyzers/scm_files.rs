// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! SCM documentation files analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use tracing::debug;

/// SCM documentation files analyzer
pub struct ScmFilesAnalyzer;

impl Default for ScmFilesAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for ScmFilesAnalyzer {
    fn name(&self) -> &str {
        "SCM Documentation Files"
    }

    fn analyze(&self, path: &Path, _config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Required SCM files for v1.0
        let required_scm_files = [
            ("STATE.scm", "Project state and progress tracking"),
            ("META.scm", "Meta-level information and architectural decisions"),
            ("ECOSYSTEM.scm", "Project's place in the ecosystem"),
            ("PLAYBOOK.scm", "Operational playbooks and procedures"),
            ("AGENTIC.scm", "Agentic AI integration documentation"),
            ("NEUROSYM.scm", "Neurosymbolic AI documentation"),
        ];

        let machine_readable_dir = path.join(".machine_readable");

        for (filename, description) in &required_scm_files {
            let correct_path = machine_readable_dir.join(filename);
            let root_path = path.join(filename);

            if correct_path.exists() {
                debug!("Found {} in .machine_readable/", filename);
                result.files_checked += 1;

                // Validate SCM file content
                if let Ok(content) = std::fs::read_to_string(&correct_path) {
                    self.validate_scm_content(filename, &content, &correct_path, &mut result);
                }
            } else if root_path.exists() {
                // File is in root instead of .machine_readable/
                result.files_checked += 1;
                result.add(
                    Finding::new(
                        &format!("SCM-005-{}", filename),
                        &format!("{} in wrong location", filename),
                        Severity::Warning,
                        &format!(
                            "{} found in repository root but should be in .machine_readable/",
                            filename
                        ),
                    )
                    .with_file(root_path)
                    .with_suggestion(&format!(
                        "Move {} to .machine_readable/{}", filename, filename
                    )),
                );
            } else {
                result.add(
                    Finding::new(
                        &format!("SCM-001-{}", filename),
                        &format!("Missing {}", filename),
                        Severity::Error,
                        &format!("Required SCM file {} not found ({}).", filename, description),
                    )
                    .with_suggestion(&format!(
                        "Create .machine_readable/{} with proper metadata and content. See hyperpolymath SCM file specs.",
                        filename
                    )),
                );
            }
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, _path: &Path, _config: &Config, _findings: &[Finding]) -> Result<Vec<String>> {
        // SCM files should be manually created with proper content
        Ok(vec![
            "SCM files must be manually created with appropriate content.".to_string(),
            "Use templates from existing hyperpolymath repos.".to_string(),
        ])
    }
}

impl ScmFilesAnalyzer {
    /// Validate SCM file content
    fn validate_scm_content(
        &self,
        filename: &str,
        content: &str,
        file_path: &Path,
        result: &mut AnalysisResult,
    ) {
        // Check for SPDX header
        if !content.starts_with(";; SPDX-License-Identifier:") {
            result.add(
                Finding::new(
                    &format!("SCM-002-{}", filename),
                    &format!("{} missing SPDX header", filename),
                    Severity::Warning,
                    &format!("{} should start with SPDX license identifier.", filename),
                )
                .with_file(file_path.to_path_buf())
                .with_suggestion("Add ;; SPDX-License-Identifier: PMPL-1.0-or-later at the top"),
            );
        }

        // Check minimum content length (should have substantial content)
        if content.len() < 200 {
            result.add(
                Finding::new(
                    &format!("SCM-003-{}", filename),
                    &format!("{} appears incomplete", filename),
                    Severity::Warning,
                    &format!("{} is very short ({}  bytes). May be a placeholder.", filename, content.len()),
                )
                .with_file(file_path.to_path_buf())
                .with_suggestion("Ensure file has complete documentation content"),
            );
        }

        // Check for specific required sections based on file type
        match filename {
            "STATE.scm" => {
                if !content.contains("metadata") || !content.contains("current-position") {
                    result.add(
                        Finding::new(
                            "SCM-004-STATE",
                            "STATE.scm missing required sections",
                            Severity::Warning,
                            "STATE.scm should contain 'metadata' and 'current-position' sections.",
                        )
                        .with_file(file_path.to_path_buf())
                        .with_suggestion("Add required sections following hyperpolymath STATE.scm format"),
                    );
                }
            }
            "META.scm" => {
                if !content.contains("architecture-decisions") || !content.contains("development-practices") {
                    result.add(
                        Finding::new(
                            "SCM-004-META",
                            "META.scm missing required sections",
                            Severity::Warning,
                            "META.scm should contain 'architecture-decisions' and 'development-practices'.",
                        )
                        .with_file(file_path.to_path_buf())
                        .with_suggestion("Add ADRs and development practices following hyperpolymath META.scm spec"),
                    );
                }
            }
            "ECOSYSTEM.scm" => {
                if !content.contains("position-in-ecosystem") || !content.contains("related-projects") {
                    result.add(
                        Finding::new(
                            "SCM-004-ECOSYSTEM",
                            "ECOSYSTEM.scm missing required sections",
                            Severity::Warning,
                            "ECOSYSTEM.scm should contain 'position-in-ecosystem' and 'related-projects'.",
                        )
                        .with_file(file_path.to_path_buf())
                        .with_suggestion("Define ecosystem position and relationships"),
                    );
                }
            }
            _ => {}
        }
    }
}
