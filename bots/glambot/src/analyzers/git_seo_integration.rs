// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Git-SEO integration analyzer
//!
//! Shells out to git-seo CLI for comprehensive repository SEO analysis.

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use std::process::Command;
use tracing::{debug, warn};

/// Git-SEO integration analyzer
pub struct GitSeoAnalyzer;

impl Default for GitSeoAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for GitSeoAnalyzer {
    fn name(&self) -> &str {
        "Git-SEO"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Check if git-seo is installed
        if !is_git_seo_installed() {
            result.add(
                Finding::new(
                    "GS-001",
                    "git-seo not installed",
                    Severity::Info,
                    "Install git-seo for comprehensive SEO analysis: https://github.com/hyperpolymath/git-seo",
                )
                .with_suggestion("Install: git clone https://github.com/hyperpolymath/git-seo && cd git-seo && julia --project=. -e 'using Pkg; Pkg.instantiate()'"),
            );
            result.duration_ms = start.elapsed().as_millis() as u64;
            return Ok(result);
        }

        // Detect repository URL from git remote
        if let Some(repo_url) = get_repo_url(path) {
            debug!("Running git-seo on {}", repo_url);

            // Run git-seo analyze with JSON output
            match run_git_seo_analyze(&repo_url, config) {
                Ok(seo_report) => {
                    result.files_checked += 1;
                    process_seo_report(&seo_report, &mut result);
                }
                Err(e) => {
                    warn!("git-seo analyze failed: {}", e);
                    result.add(
                        Finding::new(
                            "GS-002",
                            "git-seo analysis failed",
                            Severity::Warning,
                            &format!("Failed to run git-seo: {}", e),
                        )
                        .with_suggestion("Verify git-seo installation and GITHUB_TOKEN"),
                    );
                }
            }
        } else {
            result.add(
                Finding::new(
                    "GS-003",
                    "No git remote found",
                    Severity::Info,
                    "Repository has no git remote. git-seo requires a forge URL (GitHub/GitLab/Bitbucket).",
                )
                .with_suggestion("Add remote: git remote add origin <url>"),
            );
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, path: &Path, config: &Config, findings: &[Finding]) -> Result<Vec<String>> {
        let mut fixes = Vec::new();

        // Check if any git-seo findings exist
        let has_git_seo_findings = findings.iter().any(|f| f.id.starts_with("GS-"));

        if !has_git_seo_findings {
            return Ok(vec!["No git-seo findings to fix".to_string()]);
        }

        // Optimize .gitattributes for search indexing
        let gitattributes_path = path.join(".gitattributes");
        if !gitattributes_path.exists() {
            let content = "# SPDX-License-Identifier: PMPL-1.0-or-later\n\
                # .gitattributes for search indexing\n\n\
                * text=auto eol=lf\n\n\
                # Documentation (indexed by search)\n\
                *.md    text eol=lf diff=markdown linguist-documentation\n\
                *.adoc  text eol=lf linguist-documentation\n\
                *.rst   text eol=lf linguist-documentation\n\n\
                # Source code\n\
                *.rs    text eol=lf diff=rust\n\n\
                # Data\n\
                *.json  text eol=lf\n\
                *.yaml  text eol=lf\n\
                *.yml   text eol=lf\n\
                *.toml  text eol=lf\n\n\
                # Binary\n\
                *.png   binary\n\
                *.jpg   binary\n\
                *.gif   binary\n\n\
                # Lock files\n\
                Cargo.lock  text eol=lf -diff\n";
            std::fs::write(&gitattributes_path, content)?;
            fixes.push(format!(
                "Created optimized .gitattributes at {}",
                gitattributes_path.display()
            ));
        }

        // Add .github/ metadata files
        let github_dir = path.join(".github");
        if !github_dir.exists() {
            std::fs::create_dir_all(&github_dir)?;
        }

        let funding_path = github_dir.join("FUNDING.yml");
        if !funding_path.exists() {
            let content = "# SPDX-License-Identifier: PMPL-1.0-or-later\n# Funding information\n# github: hyperpolymath\n";
            std::fs::write(&funding_path, content)?;
            fixes.push(format!(
                "Created FUNDING.yml at {}",
                funding_path.display()
            ));
        }

        // If git-seo is installed, also run the tool
        if is_git_seo_installed() {
            if let Some(repo_url) = get_repo_url(path) {
                if config.seo.enable_auto_apply.unwrap_or(false) {
                    debug!("Running git-seo optimize --apply on {}", repo_url);

                    match run_git_seo_apply(&repo_url) {
                        Ok(output) => {
                            fixes.push(format!("Applied git-seo fixes:\n{}", output));
                        }
                        Err(e) => {
                            warn!("git-seo --apply failed: {}", e);
                            fixes.push(format!("Failed to apply git-seo fixes: {}", e));
                        }
                    }
                } else {
                    fixes.push(format!(
                        "To apply SEO fixes interactively, run: git-seo optimize {} --apply",
                        repo_url
                    ));
                }
            }
        }

        if fixes.is_empty() {
            fixes.push("No git-seo fixes applicable".to_string());
        }

        Ok(fixes)
    }
}

/// Check if git-seo is installed
fn is_git_seo_installed() -> bool {
    Command::new("julia")
        .args(["-e", "using GitSEO"])
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false)
}

/// Get repository URL from git remote
fn get_repo_url(path: &Path) -> Option<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(path)
        .args(["remote", "get-url", "origin"])
        .output()
        .ok()?;

    if output.status.success() {
        String::from_utf8(output.stdout)
            .ok()
            .map(|s| s.trim().to_string())
    } else {
        None
    }
}

/// Run git-seo analyze and parse JSON output
fn run_git_seo_analyze(repo_url: &str, _config: &Config) -> Result<serde_json::Value> {
    let output = Command::new("git-seo")
        .args(["analyze", repo_url, "--json"])
        .output()?;

    if !output.status.success() {
        return Err(crate::error::GlambotError::Analysis(format!(
            "git-seo failed: {}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    let json_str = String::from_utf8(output.stdout)
        .map_err(|e| crate::error::GlambotError::Analysis(format!("Invalid UTF-8: {}", e)))?;

    Ok(serde_json::from_str(&json_str)?)
}

/// Run git-seo optimize --apply
fn run_git_seo_apply(repo_url: &str) -> Result<String> {
    let output = Command::new("git-seo")
        .args(["optimize", repo_url, "--apply"])
        .output()?;

    if !output.status.success() {
        return Err(crate::error::GlambotError::Analysis(format!(
            "git-seo --apply failed: {}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

/// Process git-seo JSON report into glambot findings
fn process_seo_report(report: &serde_json::Value, result: &mut AnalysisResult) {
    // Extract overall score
    let score = report["scores"]["total"]
        .as_f64()
        .unwrap_or(0.0);
    let percentage = report["scores"]["percentage"]
        .as_f64()
        .unwrap_or(0.0);

    // Create finding based on score
    let severity = if percentage >= 80.0 {
        Severity::Info
    } else if percentage >= 60.0 {
        Severity::Warning
    } else {
        Severity::Error
    };

    result.add(
        Finding::new(
            "GS-100",
            &format!("SEO Score: {:.1}/100 ({:.1}%)", score, percentage),
            severity,
            &format!(
                "Repository SEO score is {:.1}%. {}",
                percentage,
                if percentage >= 80.0 {
                    "Excellent discoverability!"
                } else if percentage >= 60.0 {
                    "Good, but room for improvement."
                } else {
                    "Needs significant SEO improvements."
                }
            ),
        )
        .with_suggestion("Run: git-seo optimize <url> for detailed recommendations"),
    );

    // Process category scores
    if let Some(categories) = report["scores"]["categories"].as_array() {
        for category in categories {
            let name = category["name"].as_str().unwrap_or("unknown");
            let cat_score = category["score"].as_f64().unwrap_or(0.0);
            let max = category["max"].as_f64().unwrap_or(100.0);
            let percentage = (cat_score / max) * 100.0;

            if percentage < 60.0 {
                result.add(
                    Finding::new(
                        &format!("GS-1{}", categories.len()),
                        &format!("{} score low: {:.1}/{}", name, cat_score, max),
                        Severity::Warning,
                        &format!("{} category needs improvement", name),
                    )
                    .with_suggestion(&format!("Focus on {} SEO factors", name)),
                );
            }
        }
    }

    // Process recommendations
    if let Some(recommendations) = report["recommendations"].as_array() {
        for (i, rec) in recommendations.iter().enumerate() {
            if let Some(rec_str) = rec.as_str() {
                result.add(
                    Finding::new(
                        &format!("GS-2{:02}", i),
                        "SEO recommendation",
                        Severity::Info,
                        rec_str,
                    )
                    .with_suggestion("Run: git-seo optimize <url> --apply to fix interactively"),
                );
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
    fn test_repo_without_git_remote() {
        let dir = TempDir::new().unwrap();
        // Initialize a git repo without a remote
        std::process::Command::new("git")
            .arg("init")
            .arg(dir.path())
            .output()
            .unwrap();

        let analyzer = GitSeoAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        // Should find no remote (GS-003) or git-seo not installed (GS-001)
        let ids: Vec<&str> = result
            .findings
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            ids.contains(&"GS-001") || ids.contains(&"GS-003"),
            "Should report GS-001 (no git-seo) or GS-003 (no remote), got: {:?}",
            ids
        );
    }

    #[test]
    fn test_process_seo_report_high_score() {
        let report: serde_json::Value = serde_json::json!({
            "scores": {
                "total": 85.0,
                "percentage": 85.0,
                "categories": []
            },
            "recommendations": []
        });

        let mut result = AnalysisResult::new();
        process_seo_report(&report, &mut result);

        // High score should produce Info severity, not Error
        assert!(
            !result.has_errors(),
            "High score should not produce errors"
        );
        let finding = &result.findings[0];
        assert_eq!(finding.severity, Severity::Info);
    }

    #[test]
    fn test_process_seo_report_low_score() {
        let report: serde_json::Value = serde_json::json!({
            "scores": {
                "total": 30.0,
                "percentage": 30.0,
                "categories": []
            },
            "recommendations": ["Add README", "Add description"]
        });

        let mut result = AnalysisResult::new();
        process_seo_report(&report, &mut result);

        // Low score should produce Error severity
        assert!(result.has_errors(), "Low score should produce errors");
        // Should also produce recommendation findings
        assert!(
            result.findings.len() >= 3,
            "Should have score finding plus 2 recommendations, got {}",
            result.findings.len()
        );
    }

    #[test]
    fn test_fix_creates_funding_yml() {
        let dir = TempDir::new().unwrap();
        std::process::Command::new("git")
            .arg("init")
            .arg(dir.path())
            .output()
            .unwrap();

        let analyzer = GitSeoAnalyzer::default();
        let config = default_config();

        let findings = vec![Finding::new(
            "GS-100",
            "SEO Score low",
            Severity::Warning,
            "Repository SEO needs work",
        )];

        let fixes = analyzer.fix(dir.path(), &config, &findings).unwrap();

        assert!(
            fixes.iter().any(|f| f.contains("FUNDING.yml")),
            "Should create FUNDING.yml"
        );
        assert!(
            dir.path().join(".github").join("FUNDING.yml").exists(),
            "FUNDING.yml should exist after fix"
        );
    }
}
