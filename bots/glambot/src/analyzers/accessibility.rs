// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Accessibility (WCAG) analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use walkdir::WalkDir;

/// Accessibility analyzer
pub struct AccessibilityAnalyzer;

impl Default for AccessibilityAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for AccessibilityAnalyzer {
    fn name(&self) -> &str {
        "Accessibility (WCAG)"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Check images for alt text in markdown
        if config.accessibility.require_alt_text {
            self.check_image_alt_text(path, config, &mut result);
        }

        // Check heading hierarchy in markdown
        if config.accessibility.check_heading_hierarchy {
            self.check_heading_hierarchy(path, config, &mut result);
        }

        // Check link text descriptiveness
        if config.accessibility.check_link_text {
            self.check_link_text(path, config, &mut result);
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, path: &Path, _config: &Config, findings: &[Finding]) -> Result<Vec<String>> {
        let mut fixes = Vec::new();

        for finding in findings {
            match finding.id.as_str() {
                "ACC-001" => {
                    // Add alt text placeholders to images in markdown files
                    if let Some(ref file) = finding.file {
                        if let Ok(content) = std::fs::read_to_string(file) {
                            let updated = content
                                .replace("![](", "![TODO: Add alt text](");
                            if updated != content {
                                std::fs::write(file, &updated)?;
                                fixes.push(format!(
                                    "Added alt text placeholders to images in {}",
                                    file.display()
                                ));
                            }
                        }
                    }
                }
                "ACC-005" => {
                    // Add language attribute to HTML files if missing
                    if let Some(ref file) = finding.file {
                        if let Ok(content) = std::fs::read_to_string(file) {
                            if content.contains("<html") && !content.contains("lang=") {
                                let updated = content.replace("<html", "<html lang=\"en\"");
                                std::fs::write(file, &updated)?;
                                fixes.push(format!(
                                    "Added lang=\"en\" attribute to {}",
                                    file.display()
                                ));
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Generate WCAG compliance checklist if any accessibility findings exist
        if !findings.is_empty() {
            let checklist_path = path.join("WCAG-CHECKLIST.md");
            if !checklist_path.exists() {
                let checklist = "# WCAG Compliance Checklist\n\n\
                    ## Level A\n\
                    - [ ] All images have alt text (1.1.1)\n\
                    - [ ] All content is available without color alone (1.4.1)\n\
                    - [ ] Page language is identified (3.1.1)\n\n\
                    ## Level AA\n\
                    - [ ] Color contrast ratio at least 4.5:1 (1.4.3)\n\
                    - [ ] Text can be resized up to 200% (1.4.4)\n\
                    - [ ] Headings and labels are descriptive (2.4.6)\n\
                    - [ ] Link purpose is clear from context (2.4.4)\n\n\
                    ## Level AAA\n\
                    - [ ] Color contrast ratio at least 7:1 (1.4.6)\n\
                    - [ ] Link purpose is unambiguous (2.4.9)\n\
                    - [ ] Section headings organize content (2.4.10)\n";
                std::fs::write(&checklist_path, checklist)?;
                fixes.push(format!(
                    "Generated WCAG compliance checklist at {}",
                    checklist_path.display()
                ));
            }
        }

        if fixes.is_empty() {
            fixes.push("No accessibility fixes applicable".to_string());
        }

        Ok(fixes)
    }
}

impl AccessibilityAnalyzer {
    fn check_image_alt_text(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        use regex::Regex;

        let md_image_re = Regex::new(r"!\[(.*?)\]\((.*?)\)").unwrap();
        let adoc_image_re = Regex::new(r"image::(.*?)\[(.*?)\]").unwrap();

        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            let ext = entry.path().extension().and_then(|s| s.to_str());
            if ext == Some("md") || ext == Some("adoc") {
                result.files_checked += 1;

                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    for (line_num, line) in content.lines().enumerate() {
                        // Check markdown images
                        for cap in md_image_re.captures_iter(line) {
                            let alt_text = &cap[1];
                            if alt_text.is_empty() {
                                result.add(
                                    Finding::new(
                                        "ACC-001",
                                        "Image missing alt text",
                                        Severity::Error,
                                        "Image has empty alt text. Required for accessibility.",
                                    )
                                    .with_file(entry.path().to_path_buf())
                                    .with_line(line_num + 1)
                                    .with_suggestion("Add descriptive alt text: ![Description](image.png)"),
                                );
                            } else if alt_text.len() < 3 {
                                result.add(
                                    Finding::new(
                                        "ACC-002",
                                        "Alt text too short",
                                        Severity::Warning,
                                        "Alt text should be more descriptive (at least 3 characters).",
                                    )
                                    .with_file(entry.path().to_path_buf())
                                    .with_line(line_num + 1)
                                    .with_suggestion("Provide meaningful description of the image"),
                                );
                            }
                        }

                        // Check asciidoc images
                        for cap in adoc_image_re.captures_iter(line) {
                            let alt_text = &cap[2];
                            if alt_text.is_empty() {
                                result.add(
                                    Finding::new(
                                        "ACC-001",
                                        "Image missing alt text",
                                        Severity::Error,
                                        "Image has empty alt text. Required for accessibility.",
                                    )
                                    .with_file(entry.path().to_path_buf())
                                    .with_line(line_num + 1)
                                    .with_suggestion("Add descriptive alt text: image::file[Description]"),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    fn check_heading_hierarchy(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            if entry.path().extension().and_then(|s| s.to_str()) == Some("md") {
                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    let mut prev_level = 0;

                    for (line_num, line) in content.lines().enumerate() {
                        if line.starts_with('#') {
                            let level = line.chars().take_while(|&c| c == '#').count();

                            // Check for skipped heading levels (e.g., h1 -> h3)
                            if prev_level > 0 && level > prev_level + 1 {
                                result.add(
                                    Finding::new(
                                        "ACC-003",
                                        "Skipped heading level",
                                        Severity::Warning,
                                        &format!(
                                            "Heading jumps from h{} to h{}. Should not skip levels.",
                                            prev_level, level
                                        ),
                                    )
                                    .with_file(entry.path().to_path_buf())
                                    .with_line(line_num + 1)
                                    .with_suggestion("Use sequential heading levels (h1 -> h2 -> h3)"),
                                );
                            }

                            prev_level = level;
                        }
                    }
                }
            }
        }
    }

    fn check_link_text(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        use regex::Regex;

        let md_link_re = Regex::new(r"\[(.*?)\]\((.*?)\)").unwrap();

        let non_descriptive = ["click here", "here", "read more", "link", "more"];

        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            if entry.path().extension().and_then(|s| s.to_str()) == Some("md") {
                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    for (line_num, line) in content.lines().enumerate() {
                        for cap in md_link_re.captures_iter(line) {
                            let link_text = cap[1].to_lowercase();

                            if non_descriptive.iter().any(|&nd| link_text == nd) {
                                result.add(
                                    Finding::new(
                                        "ACC-004",
                                        "Non-descriptive link text",
                                        Severity::Warning,
                                        &format!("Link text '{}' is not descriptive. Use meaningful text.", &cap[1]),
                                    )
                                    .with_file(entry.path().to_path_buf())
                                    .with_line(line_num + 1)
                                    .with_suggestion("Use descriptive link text that explains the destination"),
                                );
                            }
                        }
                    }
                }
            }
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
    fn test_accessible_repo() {
        let dir = TempDir::new().unwrap();
        // Create a well-structured markdown file with proper alt text
        let content = "# Project\n\n## Overview\n\n\
            ![Project logo showing a gear icon](logo.png)\n\n\
            This project does [useful things](https://example.com/docs).\n";
        std::fs::write(dir.path().join("README.md"), content).unwrap();

        let analyzer = AccessibilityAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        assert!(
            !result.has_errors(),
            "Accessible repo should have no errors, got: {:?}",
            result.errors()
        );
    }

    #[test]
    fn test_missing_alt_text() {
        let dir = TempDir::new().unwrap();
        // Image with empty alt text
        let content = "# Project\n\n![](image.png)\n";
        std::fs::write(dir.path().join("docs.md"), content).unwrap();

        let analyzer = AccessibilityAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        assert!(result.has_errors(), "Missing alt text should be an error");
        let error_ids: Vec<&str> = result
            .errors()
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            error_ids.contains(&"ACC-001"),
            "Should report ACC-001 for missing alt text"
        );
    }

    #[test]
    fn test_skipped_heading_levels() {
        let dir = TempDir::new().unwrap();
        // Skips from h1 to h3
        let content = "# Title\n\n### Subsection\n\nContent here.\n";
        std::fs::write(dir.path().join("guide.md"), content).unwrap();

        let analyzer = AccessibilityAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        let warning_ids: Vec<&str> = result
            .warnings()
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            warning_ids.contains(&"ACC-003"),
            "Should report ACC-003 for skipped heading level"
        );
    }

    #[test]
    fn test_non_descriptive_links() {
        let dir = TempDir::new().unwrap();
        let content = "# Docs\n\n[click here](https://example.com) to learn more.\n";
        std::fs::write(dir.path().join("docs.md"), content).unwrap();

        let analyzer = AccessibilityAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        let warning_ids: Vec<&str> = result
            .warnings()
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            warning_ids.contains(&"ACC-004"),
            "Should report ACC-004 for non-descriptive link text"
        );
    }

    #[test]
    fn test_fix_generates_wcag_checklist() {
        let dir = TempDir::new().unwrap();

        let analyzer = AccessibilityAnalyzer::default();
        let config = default_config();

        let findings = vec![Finding::new(
            "ACC-001",
            "Image missing alt text",
            Severity::Error,
            "Test finding",
        )];

        let fixes = analyzer.fix(dir.path(), &config, &findings).unwrap();

        assert!(
            fixes.iter().any(|f| f.contains("WCAG compliance checklist")),
            "Should generate WCAG checklist"
        );
        assert!(
            dir.path().join("WCAG-CHECKLIST.md").exists(),
            "WCAG-CHECKLIST.md should exist after fix"
        );
    }
}
