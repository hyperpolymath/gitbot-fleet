// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Visual polish analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use tracing::debug;
use walkdir::WalkDir;

/// Visual polish analyzer
pub struct VisualAnalyzer;

impl Default for VisualAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for VisualAnalyzer {
    fn name(&self) -> &str {
        "Visual Polish"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Check README formatting
        self.check_readme(path, config, &mut result);

        // Check for badges in README
        if config.visual.enforce_badges {
            self.check_badges(path, &mut result);
        }

        // Check markdown files for line length
        if config.visual.check_formatting {
            self.check_markdown_formatting(path, config, &mut result);
        }

        // Check for logo/branding
        if config.visual.require_logo {
            self.check_logo(path, &mut result);
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, path: &Path, _config: &Config, findings: &[Finding]) -> Result<Vec<String>> {
        let mut fixes = Vec::new();

        for finding in findings {
            match finding.id.as_str() {
                "VIS-001" => {
                    // Add missing README.adoc template
                    let readme_path = path.join("README.adoc");
                    if !readme_path.exists() {
                        let repo_name = path
                            .file_name()
                            .and_then(|n| n.to_str())
                            .unwrap_or("project");
                        let template = format!(
                            "= {}\n\n== Overview\n\nTODO: Add project description.\n\n== Installation\n\nTODO: Add installation instructions.\n\n== Usage\n\nTODO: Add usage examples.\n\n== License\n\nPMPL-1.0-or-later\n",
                            repo_name
                        );
                        std::fs::write(&readme_path, template)?;
                        fixes.push(format!("Created README.adoc template at {}", readme_path.display()));
                    }
                }
                "VIS-004" => {
                    // Add badges section to README if missing
                    let readme_files = ["README.adoc", "README.md"];
                    for readme in &readme_files {
                        let readme_path = path.join(readme);
                        if readme_path.exists() {
                            if let Ok(content) = std::fs::read_to_string(&readme_path) {
                                if !content.contains("![") && !content.contains("image:") {
                                    let badge_section = if *readme == "README.adoc" {
                                        "\n// Badges\nimage:https://img.shields.io/badge/license-PMPL--1.0-blue.svg[License]\n\n"
                                    } else {
                                        "\n<!-- Badges -->\n![License](https://img.shields.io/badge/license-PMPL--1.0-blue.svg)\n\n"
                                    };
                                    // Insert badges after the first heading
                                    let updated = if let Some(pos) = content.find('\n') {
                                        format!("{}{}{}", &content[..pos + 1], badge_section, &content[pos + 1..])
                                    } else {
                                        format!("{}{}", content, badge_section)
                                    };
                                    std::fs::write(&readme_path, updated)?;
                                    fixes.push(format!("Added badges section to {}", readme_path.display()));
                                }
                            }
                            break;
                        }
                    }
                }
                "VIS-006" => {
                    // Add screenshot placeholder if repo has UI components
                    let docs_dir = path.join("docs");
                    if !docs_dir.exists() {
                        std::fs::create_dir_all(&docs_dir)?;
                    }
                    let placeholder_path = docs_dir.join("SCREENSHOTS.md");
                    if !placeholder_path.exists() {
                        let content = "# Screenshots\n\nTODO: Add screenshots or visual documentation here.\n\n## UI Preview\n\n<!-- Add screenshot: ![Description](screenshot.png) -->\n";
                        std::fs::write(&placeholder_path, content)?;
                        fixes.push(format!("Created screenshot placeholder at {}", placeholder_path.display()));
                    }
                }
                _ => {}
            }
        }

        if fixes.is_empty() {
            fixes.push("No visual fixes applicable".to_string());
        }

        Ok(fixes)
    }
}

impl VisualAnalyzer {
    fn check_readme(&self, path: &Path, _config: &Config, result: &mut AnalysisResult) {
        let readme_files = ["README.md", "README.adoc", "README.rst", "README.txt"];
        let found = readme_files
            .iter()
            .find(|name| path.join(name).exists());

        match found {
            None => {
                result.add(
                    Finding::new(
                        "VIS-001",
                        "Missing README",
                        Severity::Error,
                        "No README file found in repository root.",
                    )
                    .with_suggestion("Add a README.md or README.adoc file"),
                );
            }
            Some(readme) => {
                result.files_checked += 1;
                debug!("Found README: {}", readme);

                let readme_path = path.join(readme);
                if let Ok(content) = std::fs::read_to_string(&readme_path) {
                    // Check if README is too short
                    if content.len() < 100 {
                        result.add(
                            Finding::new(
                                "VIS-002",
                                "README too short",
                                Severity::Warning,
                                &format!("README is only {} characters. Should be more descriptive.", content.len()),
                            )
                            .with_file(readme_path.clone())
                            .with_suggestion("Expand README with project description, usage, installation"),
                        );
                    }

                    // Check for title/heading
                    let has_title = content.lines().any(|line| {
                        line.starts_with("# ") || line.starts_with("= ")
                    });

                    if !has_title {
                        result.add(
                            Finding::new(
                                "VIS-003",
                                "README missing title",
                                Severity::Warning,
                                "README should start with a clear title.",
                            )
                            .with_file(readme_path)
                            .with_suggestion("Add # Project Name or = Project Name at the top"),
                        );
                    }
                }
            }
        }
    }

    fn check_badges(&self, path: &Path, result: &mut AnalysisResult) {
        let readme_files = ["README.md", "README.adoc"];
        let found = readme_files
            .iter()
            .find(|name| path.join(name).exists());

        if let Some(readme) = found {
            let readme_path = path.join(readme);
            if let Ok(content) = std::fs::read_to_string(&readme_path) {
                let has_badges = content.contains("![") || content.contains("image:");

                if !has_badges {
                    result.add(
                        Finding::new(
                            "VIS-004",
                            "No badges found",
                            Severity::Info,
                            "README does not contain any badges (build status, coverage, etc.).",
                        )
                        .with_file(readme_path)
                        .with_suggestion("Add badges for build status, test coverage, license, etc."),
                    );
                }
            }
        }
    }

    fn check_markdown_formatting(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            if entry.path().extension().and_then(|s| s.to_str()) == Some("md")
                || entry.path().extension().and_then(|s| s.to_str()) == Some("adoc")
            {
                result.files_checked += 1;

                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    for (line_num, line) in content.lines().enumerate() {
                        if line.len() > config.visual.max_line_length {
                            result.add(
                                Finding::new(
                                    "VIS-005",
                                    "Line too long",
                                    Severity::Info,
                                    &format!(
                                        "Line {} characters exceeds max length of {}.",
                                        line.len(),
                                        config.visual.max_line_length
                                    ),
                                )
                                .with_file(entry.path().to_path_buf())
                                .with_line(line_num + 1)
                                .with_suggestion("Break long lines for better readability"),
                            );
                            break; // Only report first long line per file
                        }
                    }
                }
            }
        }
    }

    fn check_logo(&self, path: &Path, result: &mut AnalysisResult) {
        let logo_files = [
            "logo.png",
            "logo.svg",
            "Logo.png",
            "Logo.svg",
            "docs/logo.png",
            "docs/logo.svg",
            "assets/logo.png",
            "assets/logo.svg",
        ];

        let has_logo = logo_files.iter().any(|name| path.join(name).exists());

        if !has_logo {
            result.add(
                Finding::new(
                    "VIS-006",
                    "Missing logo",
                    Severity::Info,
                    "No logo file found. Consider adding branding.",
                )
                .with_suggestion("Add logo.png or logo.svg to repository root or docs/ directory"),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn default_config() -> Config {
        Config::default()
    }

    #[test]
    fn test_well_presented_repo() {
        let dir = TempDir::new().unwrap();
        // Create a well-formatted README with title, badges, and sufficient content
        let readme_content = "# My Project\n\n\
            ![Build](https://img.shields.io/badge/build-passing-green)\n\n\
            This is a well-documented project with a clear description.\n\
            It provides comprehensive functionality for demonstration.\n\
            Installation instructions and usage examples follow below.\n\n\
            ## Installation\n\nRun `cargo install myproject`.\n\n\
            ## Usage\n\nRun `myproject --help` for options.\n";
        std::fs::write(dir.path().join("README.md"), readme_content).unwrap();

        let analyzer = VisualAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        // Well-presented repo should have no errors
        assert!(
            !result.has_errors(),
            "Well-presented repo should have no errors, got: {:?}",
            result.errors()
        );
    }

    #[test]
    fn test_bare_repo_produces_findings() {
        let dir = TempDir::new().unwrap();
        // Empty directory (bare repo) - no README at all

        let analyzer = VisualAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        // Should find missing README (VIS-001) which is an error
        assert!(result.has_errors(), "Bare repo should have errors");
        let error_ids: Vec<&str> = result
            .errors()
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            error_ids.contains(&"VIS-001"),
            "Should report missing README"
        );
    }

    #[test]
    fn test_short_readme_produces_warning() {
        let dir = TempDir::new().unwrap();
        // Very short README
        std::fs::write(dir.path().join("README.md"), "# Hello\nShort.").unwrap();

        let analyzer = VisualAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        let warning_ids: Vec<&str> = result
            .warnings()
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            warning_ids.contains(&"VIS-002"),
            "Short README should trigger VIS-002 warning"
        );
    }

    #[test]
    fn test_fix_creates_readme() {
        let dir = TempDir::new().unwrap();

        let analyzer = VisualAnalyzer::default();
        let config = default_config();

        // Create a VIS-001 finding
        let findings = vec![Finding::new(
            "VIS-001",
            "Missing README",
            Severity::Error,
            "No README found",
        )];

        let fixes = analyzer.fix(dir.path(), &config, &findings).unwrap();

        assert!(
            fixes.iter().any(|f| f.contains("Created README.adoc")),
            "Should create README.adoc"
        );
        assert!(
            dir.path().join("README.adoc").exists(),
            "README.adoc should exist after fix"
        );
    }
}
