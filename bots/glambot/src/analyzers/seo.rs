// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! SEO analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use walkdir::WalkDir;

/// SEO analyzer
pub struct SeoAnalyzer;

impl Default for SeoAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for SeoAnalyzer {
    fn name(&self) -> &str {
        "SEO"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Check for repository description in README
        self.check_repo_description(path, &mut result);

        // Check HTML files for meta tags
        if config.seo.require_meta_tags {
            self.check_meta_tags(path, config, &mut result);
        }

        // Check for OpenGraph tags
        if config.seo.require_opengraph {
            self.check_opengraph(path, config, &mut result);
        }

        // Check README for keywords
        self.check_keywords(path, &mut result);

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, path: &Path, _config: &Config, findings: &[Finding]) -> Result<Vec<String>> {
        let mut fixes = Vec::new();

        for finding in findings {
            match finding.id.as_str() {
                "SEO-001" => {
                    // Improve repository description in README
                    let readme_files = ["README.md", "README.adoc"];
                    for readme in &readme_files {
                        let readme_path = path.join(readme);
                        if readme_path.exists() {
                            if let Ok(content) = std::fs::read_to_string(&readme_path) {
                                let first_lines: String =
                                    content.lines().take(10).collect::<Vec<_>>().join("\n");
                                if first_lines.len() < 50 {
                                    let repo_name = path
                                        .file_name()
                                        .and_then(|n| n.to_str())
                                        .unwrap_or("project");
                                    let suggestion = format!(
                                        "\n> TODO: Add a clear, concise description of {} here.\n> This helps search engines and users understand your project.\n\n",
                                        repo_name
                                    );
                                    // Insert after first line
                                    let updated = if let Some(pos) = content.find('\n') {
                                        format!(
                                            "{}{}{}",
                                            &content[..pos + 1],
                                            suggestion,
                                            &content[pos + 1..]
                                        )
                                    } else {
                                        format!("{}{}", content, suggestion)
                                    };
                                    std::fs::write(&readme_path, updated)?;
                                    fixes.push(format!(
                                        "Added description placeholder to {}",
                                        readme_path.display()
                                    ));
                                }
                            }
                            break;
                        }
                    }
                }
                "SEO-006" => {
                    // Add topics/tags suggestion based on repo content
                    let topics_path = path.join(".github").join("topics.txt");
                    if !topics_path.exists() {
                        if let Some(parent) = topics_path.parent() {
                            std::fs::create_dir_all(parent)?;
                        }
                        let repo_name = path
                            .file_name()
                            .and_then(|n| n.to_str())
                            .unwrap_or("project");
                        let topics = format!(
                            "# Suggested topics/tags for {}\n# Add these to your GitHub repository settings\n{}\nopen-source\nautomation\n",
                            repo_name, repo_name
                        );
                        std::fs::write(&topics_path, topics)?;
                        fixes.push(format!(
                            "Created topic suggestions at {}",
                            topics_path.display()
                        ));
                    }
                }
                "SEO-004" | "SEO-005" => {
                    // Add Open Graph metadata template to HTML files
                    if let Some(ref file) = finding.file {
                        if let Ok(content) = std::fs::read_to_string(file) {
                            if content.contains("<head>") && !content.contains("og:title") {
                                let og_template = "\n    <!-- Open Graph Metadata -->\n    \
                                    <meta property=\"og:title\" content=\"TODO: Page Title\">\n    \
                                    <meta property=\"og:description\" content=\"TODO: Page Description\">\n    \
                                    <meta property=\"og:type\" content=\"website\">\n    \
                                    <meta property=\"og:image\" content=\"TODO: Image URL\">\n";
                                let updated =
                                    content.replace("<head>", &format!("<head>{}", og_template));
                                std::fs::write(file, &updated)?;
                                fixes.push(format!(
                                    "Added OpenGraph metadata template to {}",
                                    file.display()
                                ));
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        if fixes.is_empty() {
            fixes.push("No SEO fixes applicable".to_string());
        }

        Ok(fixes)
    }
}

impl SeoAnalyzer {
    fn check_repo_description(&self, path: &Path, result: &mut AnalysisResult) {
        let readme_files = ["README.md", "README.adoc"];
        let found = readme_files
            .iter()
            .find(|name| path.join(name).exists());

        if let Some(readme) = found {
            let readme_path = path.join(readme);
            if let Ok(content) = std::fs::read_to_string(&readme_path) {
                result.files_checked += 1;

                // Check if there's a clear description in the first few lines
                let first_lines: String = content.lines().take(10).collect::<Vec<_>>().join("\n");

                if first_lines.len() < 50 {
                    result.add(
                        Finding::new(
                            "SEO-001",
                            "Missing repository description",
                            Severity::Warning,
                            "README should start with a clear project description for SEO.",
                        )
                        .with_file(readme_path)
                        .with_suggestion("Add 1-2 paragraph description at the top of README"),
                    );
                }
            }
        }
    }

    fn check_meta_tags(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            if entry.path().extension().and_then(|s| s.to_str()) == Some("html") {
                result.files_checked += 1;

                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    let has_description = content.contains("<meta name=\"description\"");
                    let has_keywords = content.contains("<meta name=\"keywords\"");

                    if !has_description {
                        result.add(
                            Finding::new(
                                "SEO-002",
                                "Missing meta description",
                                Severity::Warning,
                                "HTML file missing meta description tag.",
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion("Add <meta name=\"description\" content=\"...\"> in <head>"),
                        );
                    }

                    if !has_keywords {
                        result.add(
                            Finding::new(
                                "SEO-003",
                                "Missing meta keywords",
                                Severity::Info,
                                "HTML file missing meta keywords tag.",
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion("Add <meta name=\"keywords\" content=\"...\"> in <head>"),
                        );
                    }
                }
            }
        }
    }

    fn check_opengraph(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            if entry.path().extension().and_then(|s| s.to_str()) == Some("html") {
                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    let has_og_title = content.contains("og:title");
                    let has_og_description = content.contains("og:description");
                    let has_og_image = content.contains("og:image");

                    if !has_og_title || !has_og_description {
                        result.add(
                            Finding::new(
                                "SEO-004",
                                "Missing OpenGraph tags",
                                Severity::Info,
                                "HTML file missing OpenGraph tags for social media sharing.",
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion("Add og:title, og:description, og:image meta tags"),
                        );
                    }

                    if !has_og_image {
                        result.add(
                            Finding::new(
                                "SEO-005",
                                "Missing OpenGraph image",
                                Severity::Info,
                                "HTML file missing og:image tag. Improves social media previews.",
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion("Add <meta property=\"og:image\" content=\"...\">"),
                        );
                    }
                }
            }
        }
    }

    fn check_keywords(&self, path: &Path, result: &mut AnalysisResult) {
        let readme_files = ["README.md", "README.adoc"];
        let found = readme_files
            .iter()
            .find(|name| path.join(name).exists());

        if let Some(readme) = found {
            let readme_path = path.join(readme);
            if let Ok(content) = std::fs::read_to_string(&readme_path) {
                // Check if README mentions common project keywords
                let keywords = ["install", "usage", "example", "documentation", "license"];
                let missing_keywords: Vec<_> = keywords
                    .iter()
                    .filter(|&kw| !content.to_lowercase().contains(kw))
                    .collect();

                if missing_keywords.len() > 3 {
                    let keywords_str = missing_keywords
                        .iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    result.add(
                        Finding::new(
                            "SEO-006",
                            "Few standard keywords",
                            Severity::Info,
                            &format!(
                                "README missing common keywords: {}. Improves searchability.",
                                keywords_str
                            ),
                        )
                        .with_file(readme_path)
                        .with_suggestion("Include sections: Installation, Usage, Examples, Documentation, License"),
                    );
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
    fn test_well_seo_optimized_repo() {
        let dir = TempDir::new().unwrap();
        // README with good description and keywords
        let content = "# My Awesome Project\n\n\
            This is a comprehensive tool for installation and usage of widgets.\n\
            See the documentation for examples and license information.\n\n\
            ## Installation\n\nRun `cargo install awesome`.\n\n\
            ## Usage\n\nCheck the examples directory.\n\n\
            ## Documentation\n\nFull docs at https://example.com.\n\n\
            ## License\n\nPMPL-1.0-or-later\n";
        std::fs::write(dir.path().join("README.md"), content).unwrap();

        let analyzer = SeoAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        // Well-optimized repo should have no warnings or errors
        assert!(
            !result.has_errors(),
            "Well-optimized repo should have no errors"
        );
        assert!(
            result.warnings().is_empty(),
            "Well-optimized repo should have no warnings"
        );
    }

    #[test]
    fn test_bare_repo_seo_findings() {
        let dir = TempDir::new().unwrap();
        // Minimal README
        let content = "# Hi\n";
        std::fs::write(dir.path().join("README.md"), content).unwrap();

        let analyzer = SeoAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        // Should find missing description (SEO-001)
        let ids: Vec<&str> = result
            .findings
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            ids.contains(&"SEO-001"),
            "Should report SEO-001 for short description"
        );
    }

    #[test]
    fn test_html_missing_meta_tags() {
        let dir = TempDir::new().unwrap();
        let html = "<html><head><title>Test</title></head><body>Hello</body></html>";
        std::fs::write(dir.path().join("index.html"), html).unwrap();

        let mut config = default_config();
        config.seo.require_meta_tags = true;

        let analyzer = SeoAnalyzer::default();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        let ids: Vec<&str> = result
            .findings
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            ids.contains(&"SEO-002"),
            "Should report SEO-002 for missing meta description"
        );
    }

    #[test]
    fn test_fix_creates_topic_suggestions() {
        let dir = TempDir::new().unwrap();
        std::fs::create_dir_all(dir.path().join(".github")).unwrap();

        let analyzer = SeoAnalyzer::default();
        let config = default_config();

        let findings = vec![Finding::new(
            "SEO-006",
            "Few standard keywords",
            Severity::Info,
            "Missing keywords",
        )];

        let fixes = analyzer.fix(dir.path(), &config, &findings).unwrap();

        assert!(
            fixes.iter().any(|f| f.contains("topic suggestions")),
            "Should create topic suggestions file"
        );
    }
}
