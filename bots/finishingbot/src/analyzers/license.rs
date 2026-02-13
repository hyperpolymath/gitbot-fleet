// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! License validation analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use regex::Regex;
use std::path::Path;
use tracing::debug;
use walkdir::WalkDir;

/// License validation analyzer
pub struct LicenseAnalyzer;

impl Default for LicenseAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for LicenseAnalyzer {
    fn name(&self) -> &str {
        "License Validation"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Check for LICENSE file
        self.check_license_file(path, config, &mut result);

        // Check SPDX headers in source files
        if config.licenses.require_spdx_headers {
            self.check_spdx_headers(path, config, &mut result);
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, _path: &Path, config: &Config, findings: &[Finding]) -> Result<Vec<String>> {
        let mut fixed = Vec::new();

        for finding in findings.iter().filter(|f| f.fixable) {
            if finding.id.starts_with("LIC-002") {
                // Add SPDX header
                if let Some(ref file_path) = finding.file {
                    if !config.dry_run {
                        if let Ok(()) = self.add_spdx_header(file_path, &config.licenses.allowed) {
                            fixed.push(format!("Added SPDX header to {}", file_path.display()));
                        }
                    } else {
                        fixed.push(format!(
                            "[DRY RUN] Would add SPDX header to {}",
                            file_path.display()
                        ));
                    }
                }
            }
        }

        Ok(fixed)
    }
}

impl LicenseAnalyzer {
    /// Check for LICENSE file in repository root
    fn check_license_file(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        let license_files = [
            "LICENSE",
            "LICENSE.md",
            "LICENSE.txt",
            "LICENCE",
            "LICENCE.md",
            "LICENCE.txt",
            "COPYING",
            "COPYING.md",
        ];

        let found_license = license_files
            .iter()
            .find(|name| path.join(name).exists());

        match found_license {
            None => {
                result.add(
                    Finding::new(
                        "LIC-001",
                        "Missing License",
                        Severity::Error,
                        "No LICENSE file found in repository root",
                    )
                    .with_suggestion("Add a LICENSE file with your chosen license"),
                );
            }
            Some(license_file) => {
                let license_path = path.join(license_file);
                if let Ok(content) = std::fs::read_to_string(&license_path) {
                    self.validate_license_content(&content, &license_path, config, result);
                }
            }
        }
    }

    /// Validate license file content
    fn validate_license_content(
        &self,
        content: &str,
        path: &Path,
        config: &Config,
        result: &mut AnalysisResult,
    ) {
        let detected = self.detect_license(content);

        match detected {
            Some(license) => {
                if config.licenses.strict && !config.licenses.allowed.contains(&license) {
                    result.add(
                        Finding::new(
                            "LIC-003",
                            "Disallowed License",
                            Severity::Error,
                            &format!(
                                "License '{}' is not in the allowed list: {}",
                                license,
                                config.licenses.allowed.join(", ")
                            ),
                        )
                        .with_file(path.to_path_buf()),
                    );
                } else {
                    debug!("Detected license: {}", license);
                }
            }
            None => {
                if config.licenses.strict {
                    result.add(
                        Finding::new(
                            "LIC-004",
                            "Unknown License",
                            Severity::Warning,
                            "Could not detect license type from LICENSE file",
                        )
                        .with_file(path.to_path_buf())
                        .with_suggestion("Use a standard license format or add SPDX identifier"),
                    );
                }
            }
        }
    }

    /// Detect license type from content
    fn detect_license(&self, content: &str) -> Option<String> {
        let content_lower = content.to_lowercase();

        // Check for SPDX identifier first
        let spdx_re = Regex::new(r"SPDX-License-Identifier:\s*([A-Za-z0-9.-]+)").ok()?;
        if let Some(caps) = spdx_re.captures(content) {
            return Some(caps[1].to_string());
        }

        // Check for common license patterns
        if content_lower.contains("gnu affero general public license")
            || content_lower.contains("agpl-3.0")
        {
            return Some("AGPL-3.0-or-later".to_string());
        }

        if content_lower.contains("gnu general public license")
            && content_lower.contains("version 3")
        {
            return Some("GPL-3.0-or-later".to_string());
        }

        if content_lower.contains("mit license") || content_lower.contains("permission is hereby granted, free of charge") {
            return Some("MIT".to_string());
        }

        if content_lower.contains("apache license") && content_lower.contains("version 2.0") {
            return Some("Apache-2.0".to_string());
        }

        if content_lower.contains("bsd 3-clause") || content_lower.contains("redistribution and use in source and binary forms") {
            return Some("BSD-3-Clause".to_string());
        }

        if content_lower.contains("palimpsest") {
            return Some("PMPL-1.0".to_string());
        }

        None
    }

    /// Check for SPDX headers in source files
    fn check_spdx_headers(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        let spdx_pattern =
            Regex::new(r"(?i)SPDX-License-Identifier:\s*[A-Za-z0-9.-]+").unwrap();

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

            if !entry_path.is_file() {
                continue;
            }

            // Check if this is a source file that needs SPDX header
            let should_check = entry_path
                .extension()
                .and_then(|e| e.to_str())
                .map(|ext| config.licenses.source_extensions.contains(&ext.to_string()))
                .unwrap_or(false);

            if !should_check {
                continue;
            }

            result.files_checked += 1;

            // Read first few lines to check for SPDX header
            if let Ok(content) = std::fs::read_to_string(entry_path) {
                let first_lines: String = content.lines().take(10).collect::<Vec<_>>().join("\n");

                if !spdx_pattern.is_match(&first_lines) {
                    result.add(
                        Finding::new(
                            "LIC-002",
                            "Missing SPDX Header",
                            Severity::Warning,
                            "Source file missing SPDX license identifier",
                        )
                        .with_file(entry_path.to_path_buf())
                        .with_suggestion(&format!(
                            "Add '// SPDX-License-Identifier: {}' at the start",
                            config.licenses.allowed.first().unwrap_or(&"<LICENSE>".to_string())
                        ))
                        .fixable(),
                    );
                }
            }
        }
    }

    /// Add SPDX header to a file
    fn add_spdx_header(&self, path: &Path, allowed_licenses: &[String]) -> Result<()> {
        let content = std::fs::read_to_string(path)?;
        let default_license = "PMPL-1.0-or-later".to_string();
        let license = allowed_licenses.first().unwrap_or(&default_license);

        // Determine comment style based on extension
        let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
        let header = match ext {
            "rs" | "js" | "ts" | "java" | "kt" | "swift" | "go" | "c" | "cpp" | "h" | "hpp" => {
                format!("// SPDX-License-Identifier: {}\n", license)
            }
            "py" | "rb" | "sh" | "bash" | "yml" | "yaml" => {
                format!("# SPDX-License-Identifier: {}\n", license)
            }
            "hs" | "ml" => {
                format!("-- SPDX-License-Identifier: {}\n", license)
            }
            "html" | "xml" => {
                format!("<!-- SPDX-License-Identifier: {} -->\n", license)
            }
            "css" => {
                format!("/* SPDX-License-Identifier: {} */\n", license)
            }
            _ => {
                format!("// SPDX-License-Identifier: {}\n", license)
            }
        };

        // Handle shebang lines
        let new_content = if content.starts_with("#!") {
            let mut lines = content.lines();
            let shebang = lines.next().unwrap_or("");
            format!("{}\n{}{}", shebang, header, lines.collect::<Vec<_>>().join("\n"))
        } else {
            format!("{}{}", header, content)
        };

        std::fs::write(path, new_content)?;
        Ok(())
    }
}
