// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Machine-readability analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use tracing::debug;
use walkdir::WalkDir;

/// Machine-readability analyzer
pub struct MachineAnalyzer;

impl Default for MachineAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for MachineAnalyzer {
    fn name(&self) -> &str {
        "Machine-Readability"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Validate JSON files
        if config.machine.validate_json {
            self.check_json_validity(path, config, &mut result);
        }

        // Validate YAML files
        if config.machine.validate_yaml {
            self.check_yaml_validity(path, config, &mut result);
        }

        // Check for structured data
        if config.machine.check_structured_data {
            self.check_structured_data(path, config, &mut result);
        }

        // Check for robots.txt
        self.check_robots_txt(path, &mut result);

        // Check for .machine_readable/ directory
        self.check_machine_readable_dir(path, &mut result);

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, path: &Path, _config: &Config, findings: &[Finding]) -> Result<Vec<String>> {
        let mut fixes = Vec::new();

        for finding in findings {
            match finding.id.as_str() {
                "MACH-006" => {
                    // Create missing .machine_readable/ directory structure
                    let mr_dir = path.join(".machine_readable");
                    if !mr_dir.exists() {
                        std::fs::create_dir_all(&mr_dir)?;
                        fixes.push(format!(
                            "Created .machine_readable/ directory at {}",
                            mr_dir.display()
                        ));
                    }

                    // Generate template SCM files
                    let repo_name = path
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("project");

                    let state_path = mr_dir.join("STATE.scm");
                    if !state_path.exists() {
                        let state_content = format!(
                            ";; SPDX-License-Identifier: PMPL-1.0-or-later\n\
                            ;; STATE.scm - Project state for {name}\n\n\
                            (state\n\
                            \x20 (metadata\n\
                            \x20   (version \"0.1.0\")\n\
                            \x20   (schema-version \"1.0\")\n\
                            \x20   (created \"TODO\")\n\
                            \x20   (updated \"TODO\")\n\
                            \x20   (project \"{name}\")\n\
                            \x20   (repo \"hyperpolymath/{name}\"))\n\n\
                            \x20 (project-context\n\
                            \x20   (name \"{name}\")\n\
                            \x20   (tagline \"TODO: Add tagline\")\n\
                            \x20   (tech-stack (\"TODO\"))))\n",
                            name = repo_name
                        );
                        std::fs::write(&state_path, state_content)?;
                        fixes.push(format!(
                            "Generated STATE.scm template at {}",
                            state_path.display()
                        ));
                    }

                    let meta_path = mr_dir.join("META.scm");
                    if !meta_path.exists() {
                        let meta_content = format!(
                            ";; SPDX-License-Identifier: PMPL-1.0-or-later\n\
                            ;; META.scm - Meta information for {name}\n\n\
                            (meta\n\
                            \x20 (architecture-decisions ())\n\
                            \x20 (development-practices ())\n\
                            \x20 (design-rationale ()))\n",
                            name = repo_name
                        );
                        std::fs::write(&meta_path, meta_content)?;
                        fixes.push(format!(
                            "Generated META.scm template at {}",
                            meta_path.display()
                        ));
                    }

                    let ecosystem_path = mr_dir.join("ECOSYSTEM.scm");
                    if !ecosystem_path.exists() {
                        let eco_content = format!(
                            ";; SPDX-License-Identifier: PMPL-1.0-or-later\n\
                            ;; ECOSYSTEM.scm - Ecosystem context for {name}\n\n\
                            (ecosystem\n\
                            \x20 (version \"1.0\")\n\
                            \x20 (name \"{name}\")\n\
                            \x20 (type \"application\")\n\
                            \x20 (purpose \"TODO\")\n\
                            \x20 (related-projects ()))\n",
                            name = repo_name
                        );
                        std::fs::write(&ecosystem_path, eco_content)?;
                        fixes.push(format!(
                            "Generated ECOSYSTEM.scm template at {}",
                            ecosystem_path.display()
                        ));
                    }
                }
                "MACH-003" => {
                    // Add structured data (schema.org JSON-LD) template
                    if let Some(ref file) = finding.file {
                        if let Ok(content) = std::fs::read_to_string(file) {
                            if content.contains("<head>")
                                && !content.contains("application/ld+json")
                            {
                                let json_ld = "\n    <script type=\"application/ld+json\">\n    {\n      \
                                    \"@context\": \"https://schema.org\",\n      \
                                    \"@type\": \"SoftwareSourceCode\",\n      \
                                    \"name\": \"TODO: Project Name\",\n      \
                                    \"description\": \"TODO: Project Description\",\n      \
                                    \"license\": \"https://spdx.org/licenses/PMPL-1.0-or-later\",\n      \
                                    \"codeRepository\": \"TODO: Repository URL\"\n    }\n    </script>\n";
                                let updated =
                                    content.replace("</head>", &format!("{}</head>", json_ld));
                                std::fs::write(file, &updated)?;
                                fixes.push(format!(
                                    "Added schema.org JSON-LD template to {}",
                                    file.display()
                                ));
                            }
                        }
                    }
                }
                "MACH-005" => {
                    // Add robots.txt
                    let robots_path = path.join("robots.txt");
                    if !robots_path.exists() {
                        let content =
                            "# robots.txt for repository documentation\nUser-agent: *\nAllow: /\n\nSitemap: TODO\n";
                        std::fs::write(&robots_path, content)?;
                        fixes.push(format!(
                            "Created robots.txt at {}",
                            robots_path.display()
                        ));
                    }
                }
                _ => {}
            }
        }

        if fixes.is_empty() {
            fixes.push("No machine-readability fixes applicable".to_string());
        }

        Ok(fixes)
    }
}

impl MachineAnalyzer {
    fn check_json_validity(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            if entry.path().extension().and_then(|s| s.to_str()) == Some("json") {
                result.files_checked += 1;

                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    if let Err(e) = serde_json::from_str::<serde_json::Value>(&content) {
                        result.add(
                            Finding::new(
                                "MACH-001",
                                "Invalid JSON",
                                Severity::Error,
                                &format!("JSON file is not valid: {}", e),
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion("Fix JSON syntax errors"),
                        );
                    } else {
                        debug!("Valid JSON: {}", entry.path().display());
                    }
                }
            }
        }
    }

    fn check_yaml_validity(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            let ext = entry.path().extension().and_then(|s| s.to_str());
            if ext == Some("yaml") || ext == Some("yml") {
                result.files_checked += 1;

                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    if let Err(e) = serde_yaml::from_str::<serde_yaml::Value>(&content) {
                        result.add(
                            Finding::new(
                                "MACH-002",
                                "Invalid YAML",
                                Severity::Error,
                                &format!("YAML file is not valid: {}", e),
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion("Fix YAML syntax errors"),
                        );
                    } else {
                        debug!("Valid YAML: {}", entry.path().display());
                    }
                }
            }
        }
    }

    fn check_structured_data(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
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
                    let has_json_ld = content.contains("application/ld+json");
                    let has_microdata = content.contains("itemscope") || content.contains("itemprop");
                    let has_schema_org = content.contains("schema.org");

                    if !has_json_ld && !has_microdata && config.machine.require_schema_org {
                        result.add(
                            Finding::new(
                                "MACH-003",
                                "Missing structured data",
                                Severity::Warning,
                                "HTML file has no structured data (JSON-LD or microdata).",
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion("Add schema.org structured data for better machine readability"),
                        );
                    }

                    if has_json_ld {
                        // Try to validate JSON-LD
                        if let Some(json_ld_start) = content.find("<script type=\"application/ld+json\">") {
                            if let Some(json_ld_end) = content[json_ld_start..].find("</script>") {
                                let json_ld = &content[json_ld_start + 36..json_ld_start + json_ld_end];
                                if serde_json::from_str::<serde_json::Value>(json_ld).is_err() {
                                    result.add(
                                        Finding::new(
                                            "MACH-004",
                                            "Invalid JSON-LD",
                                            Severity::Error,
                                            "JSON-LD structured data is not valid JSON.",
                                        )
                                        .with_file(entry.path().to_path_buf())
                                        .with_suggestion("Fix JSON-LD syntax"),
                                    );
                                }
                            }
                        }
                    }

                    if has_schema_org {
                        debug!("Found schema.org markup in {}", entry.path().display());
                    }
                }
            }
        }
    }

    fn check_robots_txt(&self, path: &Path, result: &mut AnalysisResult) {
        let robots_path = path.join("robots.txt");

        if !robots_path.exists() {
            result.add(
                Finding::new(
                    "MACH-005",
                    "Missing robots.txt",
                    Severity::Info,
                    "No robots.txt found. Consider adding one for crawler control.",
                )
                .with_suggestion("Add robots.txt to repository root if hosting documentation"),
            );
        } else {
            result.files_checked += 1;
            debug!("Found robots.txt");
        }
    }

    fn check_machine_readable_dir(&self, path: &Path, result: &mut AnalysisResult) {
        let mr_dir = path.join(".machine_readable");

        if !mr_dir.exists() {
            result.add(
                Finding::new(
                    "MACH-006",
                    "Missing .machine_readable/ directory",
                    Severity::Warning,
                    "No .machine_readable/ directory found. SCM files (STATE.scm, META.scm, ECOSYSTEM.scm) should be placed here.",
                )
                .fixable()
                .with_suggestion("Create .machine_readable/ directory with STATE.scm, META.scm, ECOSYSTEM.scm"),
            );
            return;
        }

        result.files_checked += 1;

        // Check for required SCM files
        let required_scm = ["STATE.scm", "META.scm", "ECOSYSTEM.scm"];
        for scm_file in &required_scm {
            // Check both direct path and 6scm subdirectory
            let direct_path = mr_dir.join(scm_file);
            let sub_path = mr_dir.join("6scm").join(scm_file);
            if !direct_path.exists() && !sub_path.exists() {
                result.add(
                    Finding::new(
                        "MACH-007",
                        &format!("Missing {}", scm_file),
                        Severity::Info,
                        &format!("{} not found in .machine_readable/ directory.", scm_file),
                    )
                    .with_suggestion(&format!("Add {} to .machine_readable/", scm_file)),
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
    fn test_well_structured_repo() {
        let dir = TempDir::new().unwrap();
        // Create valid JSON file
        std::fs::write(
            dir.path().join("package.json"),
            r#"{"name": "test", "version": "1.0.0"}"#,
        )
        .unwrap();
        // Create valid YAML file
        std::fs::write(
            dir.path().join("config.yml"),
            "name: test\nversion: 1.0.0\n",
        )
        .unwrap();
        // Create .machine_readable/ with SCM files
        let mr_dir = dir.path().join(".machine_readable");
        std::fs::create_dir_all(&mr_dir).unwrap();
        std::fs::write(mr_dir.join("STATE.scm"), "(state)").unwrap();
        std::fs::write(mr_dir.join("META.scm"), "(meta)").unwrap();
        std::fs::write(mr_dir.join("ECOSYSTEM.scm"), "(ecosystem)").unwrap();
        // Create robots.txt
        std::fs::write(dir.path().join("robots.txt"), "User-agent: *\nAllow: /\n").unwrap();

        let analyzer = MachineAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        assert!(
            !result.has_errors(),
            "Well-structured repo should have no errors, got: {:?}",
            result.errors()
        );
    }

    #[test]
    fn test_invalid_json_detected() {
        let dir = TempDir::new().unwrap();
        // Create invalid JSON
        std::fs::write(dir.path().join("broken.json"), "{invalid json}").unwrap();

        let analyzer = MachineAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        assert!(result.has_errors(), "Invalid JSON should produce errors");
        let error_ids: Vec<&str> = result
            .errors()
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            error_ids.contains(&"MACH-001"),
            "Should report MACH-001 for invalid JSON"
        );
    }

    #[test]
    fn test_missing_machine_readable_dir() {
        let dir = TempDir::new().unwrap();
        // No .machine_readable/ directory

        let analyzer = MachineAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        let ids: Vec<&str> = result
            .findings
            .iter()
            .map(|f| f.id.as_str())
            .collect();
        assert!(
            ids.contains(&"MACH-006"),
            "Should report MACH-006 for missing .machine_readable/"
        );
    }

    #[test]
    fn test_fix_creates_machine_readable_structure() {
        let dir = TempDir::new().unwrap();

        let analyzer = MachineAnalyzer::default();
        let config = default_config();

        let findings = vec![Finding::new(
            "MACH-006",
            "Missing .machine_readable/",
            Severity::Warning,
            "No .machine_readable/ directory found",
        )];

        let fixes = analyzer.fix(dir.path(), &config, &findings).unwrap();

        assert!(
            fixes.iter().any(|f| f.contains(".machine_readable")),
            "Should create .machine_readable directory"
        );
        assert!(
            dir.path().join(".machine_readable").exists(),
            ".machine_readable/ should exist after fix"
        );
        assert!(
            dir.path().join(".machine_readable").join("STATE.scm").exists(),
            "STATE.scm should exist after fix"
        );
        assert!(
            dir.path().join(".machine_readable").join("META.scm").exists(),
            "META.scm should exist after fix"
        );
        assert!(
            dir.path().join(".machine_readable").join("ECOSYSTEM.scm").exists(),
            "ECOSYSTEM.scm should exist after fix"
        );
    }
}
