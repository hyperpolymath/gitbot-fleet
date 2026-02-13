// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Release readiness analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use regex::Regex;
use sha2::{Digest, Sha256, Sha512};
use std::path::Path;
use tracing::debug;
use walkdir::WalkDir;

/// Release readiness analyzer
pub struct ReleaseAnalyzer;

impl Default for ReleaseAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for ReleaseAnalyzer {
    fn name(&self) -> &str {
        "Release Readiness"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Check version consistency
        self.check_version_consistency(path, &mut result);

        // Check for debug artifacts
        self.check_debug_artifacts(path, config, &mut result);

        // Check security files
        self.check_security_files(path, &mut result);

        // Check hash files if required
        if config.release.require_hashes {
            self.verify_release_hashes(path, config, &mut result);
        }

        // Check for sensitive data
        self.check_sensitive_data(path, config, &mut result);

        // Check build configuration
        self.check_build_config(path, &mut result);

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, _path: &Path, _config: &Config, _findings: &[Finding]) -> Result<Vec<String>> {
        // Most release issues require manual intervention
        Ok(Vec::new())
    }
}

impl ReleaseAnalyzer {
    /// Check version consistency across files
    fn check_version_consistency(&self, path: &Path, result: &mut AnalysisResult) {
        let mut versions: Vec<(String, String)> = Vec::new();

        // Check Cargo.toml
        let cargo_toml = path.join("Cargo.toml");
        if cargo_toml.exists() {
            if let Ok(content) = std::fs::read_to_string(&cargo_toml) {
                if let Some(version) = self.extract_toml_version(&content) {
                    versions.push(("Cargo.toml".to_string(), version));
                }
            }
        }

        // Check package.json
        let package_json = path.join("package.json");
        if package_json.exists() {
            if let Ok(content) = std::fs::read_to_string(&package_json) {
                if let Some(version) = self.extract_json_version(&content) {
                    versions.push(("package.json".to_string(), version));
                }
            }
        }

        // Check deno.json
        let deno_json = path.join("deno.json");
        if deno_json.exists() {
            if let Ok(content) = std::fs::read_to_string(&deno_json) {
                if let Some(version) = self.extract_json_version(&content) {
                    versions.push(("deno.json".to_string(), version));
                }
            }
        }

        // Check VERSION file
        let version_file = path.join("VERSION");
        if version_file.exists() {
            if let Ok(content) = std::fs::read_to_string(&version_file) {
                versions.push(("VERSION".to_string(), content.trim().to_string()));
            }
        }

        // Compare versions
        if versions.len() > 1 {
            let first_version = &versions[0].1;
            let inconsistent: Vec<_> = versions
                .iter()
                .filter(|(_, v)| v != first_version)
                .collect();

            if !inconsistent.is_empty() {
                let version_list: Vec<String> = versions
                    .iter()
                    .map(|(f, v)| format!("{}: {}", f, v))
                    .collect();

                result.add(
                    Finding::new(
                        "REL-001",
                        "Version Inconsistency",
                        Severity::Error,
                        &format!("Version numbers are inconsistent:\n  {}", version_list.join("\n  ")),
                    )
                    .with_suggestion("Ensure all version files have the same version number"),
                );
            }
        }
    }

    fn extract_toml_version(&self, content: &str) -> Option<String> {
        let re = Regex::new(r#"version\s*=\s*"([^"]+)""#).ok()?;
        re.captures(content).map(|c| c[1].to_string())
    }

    fn extract_json_version(&self, content: &str) -> Option<String> {
        let re = Regex::new(r#""version"\s*:\s*"([^"]+)""#).ok()?;
        re.captures(content).map(|c| c[1].to_string())
    }

    /// Check for debug artifacts that shouldn't be in release
    fn check_debug_artifacts(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        let debug_patterns = [
            "console.log",
            "console.debug",
            "debugger;",
            "binding.pry",
            "import pdb",
            "pdb.set_trace",
            "dbg!",
            "println!",  // In Rust release builds
            "#[cfg(debug_assertions)]",
        ];

        let debug_files = [
            ".env.local",
            ".env.development",
            "debug.log",
            "*.log",
        ];

        // Check for debug files
        for pattern in &debug_files {
            if pattern.contains('*') {
                let glob_pattern = path.join(pattern);
                if let Ok(entries) = glob::glob(&glob_pattern.to_string_lossy()) {
                    for entry in entries.filter_map(|e| e.ok()) {
                        result.add(
                            Finding::new(
                                "REL-002",
                                "Debug File",
                                Severity::Warning,
                                &format!("Debug file found: {}", entry.display()),
                            )
                            .with_file(entry)
                            .with_suggestion("Remove debug files before release"),
                        );
                    }
                }
            } else if path.join(pattern).exists() {
                result.add(
                    Finding::new(
                        "REL-002",
                        "Debug File",
                        Severity::Warning,
                        &format!("Debug file found: {}", pattern),
                    )
                    .with_suggestion("Remove debug files before release"),
                );
            }
        }

        // Check source files for debug statements (limited scan)
        for entry in WalkDir::new(path)
            .follow_links(false)
            .max_depth(5)
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

            let is_source = entry_path
                .extension()
                .and_then(|e| e.to_str())
                .map(|ext| matches!(ext, "rs" | "js" | "ts" | "py" | "rb" | "go"))
                .unwrap_or(false);

            if !is_source {
                continue;
            }

            if let Ok(content) = std::fs::read_to_string(entry_path) {
                result.files_checked += 1;

                for pattern in &debug_patterns {
                    if content.contains(pattern) {
                        // Count occurrences
                        let count = content.matches(pattern).count();
                        result.add(
                            Finding::new(
                                "REL-003",
                                "Debug Statement",
                                Severity::Info,
                                &format!("Found {} occurrence(s) of '{}' in source", count, pattern),
                            )
                            .with_file(entry_path.to_path_buf())
                            .with_suggestion("Review and remove debug statements for release"),
                        );
                        break; // Only report once per file
                    }
                }
            }
        }
    }

    /// Check for required security files
    fn check_security_files(&self, path: &Path, result: &mut AnalysisResult) {
        let security_files = [
            ("SECURITY.md", "Security policy", Severity::Warning),
            ("SECURITY.adoc", "Security policy", Severity::Warning),
        ];

        let has_security = security_files
            .iter()
            .any(|(name, _, _)| path.join(name).exists());

        if !has_security {
            result.add(
                Finding::new(
                    "REL-004",
                    "Missing Security Policy",
                    Severity::Warning,
                    "No SECURITY.md file found",
                )
                .with_suggestion("Add a SECURITY.md describing how to report vulnerabilities"),
            );
        }

        // Check for .github/SECURITY.md
        let github_security = path.join(".github/SECURITY.md");
        if !github_security.exists() && !has_security {
            debug!("No security policy found in .github either");
        }
    }

    /// Verify release artifact hashes
    fn verify_release_hashes(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        // Look for common release directories
        let release_dirs = ["dist", "release", "build", "target/release"];

        for dir in &release_dirs {
            let release_path = path.join(dir);
            if !release_path.exists() {
                continue;
            }

            // Look for hash files
            let hash_extensions = ["sha256", "sha512", "sha256sum", "sha512sum"];
            let mut found_hashes = false;

            for entry in WalkDir::new(&release_path)
                .max_depth(2)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                let entry_path = entry.path();
                if entry_path.is_file() {
                    if let Some(ext) = entry_path.extension().and_then(|e| e.to_str()) {
                        if hash_extensions.contains(&ext) {
                            found_hashes = true;
                            // Verify hash matches file
                            self.verify_hash_file(entry_path, config, result);
                        }
                    }
                }
            }

            if !found_hashes && release_path.exists() {
                result.add(
                    Finding::new(
                        "REL-005",
                        "Missing Release Hashes",
                        Severity::Warning,
                        &format!("No hash files found in {}", dir),
                    )
                    .with_suggestion("Generate SHA256/SHA512 hashes for release artifacts"),
                );
            }
        }
    }

    /// Verify a hash file matches its target
    fn verify_hash_file(&self, hash_path: &Path, config: &Config, result: &mut AnalysisResult) {
        let hash_content = match std::fs::read_to_string(hash_path) {
            Ok(c) => c,
            Err(_) => return,
        };

        // Parse hash file (format: "hash  filename" or "hash *filename")
        let parts: Vec<&str> = hash_content.split_whitespace().collect();
        if parts.len() < 2 {
            return;
        }

        let expected_hash = parts[0];
        let filename = parts[1].trim_start_matches('*');
        let target_path = hash_path.parent().unwrap().join(filename);

        if !target_path.exists() {
            result.add(
                Finding::new(
                    "REL-006",
                    "Hash Target Missing",
                    Severity::Error,
                    &format!("Hash file references missing file: {}", filename),
                )
                .with_file(hash_path.to_path_buf()),
            );
            return;
        }

        // Calculate actual hash
        let file_content = match std::fs::read(&target_path) {
            Ok(c) => c,
            Err(_) => return,
        };

        let actual_hash = if config.release.hash_algorithms.contains(&"SHA512".to_string())
            && hash_path.to_string_lossy().contains("512")
        {
            let mut hasher = Sha512::new();
            hasher.update(&file_content);
            format!("{:x}", hasher.finalize())
        } else {
            let mut hasher = Sha256::new();
            hasher.update(&file_content);
            format!("{:x}", hasher.finalize())
        };

        if actual_hash.to_lowercase() != expected_hash.to_lowercase() {
            result.add(
                Finding::new(
                    "REL-007",
                    "Hash Mismatch",
                    Severity::Error,
                    &format!("Hash mismatch for {}: expected {}, got {}", filename, expected_hash, actual_hash),
                )
                .with_file(hash_path.to_path_buf())
                .with_suggestion("Regenerate hash file or investigate file corruption"),
            );
        }
    }

    /// Check for sensitive data that shouldn't be released
    fn check_sensitive_data(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        let sensitive_patterns = [
            (r#"(?i)api[_-]?key\s*[:=]\s*['"]?[a-zA-Z0-9]{20,}"#, "API Key"),
            (r#"(?i)secret[_-]?key\s*[:=]\s*['"]?[a-zA-Z0-9]{20,}"#, "Secret Key"),
            (r#"(?i)password\s*[:=]\s*['"]?[^\s'"]{8,}"#, "Password"),
            (r"(?i)private[_-]?key", "Private Key Reference"),
            (r"-----BEGIN (RSA |DSA |EC |OPENSSH )?PRIVATE KEY-----", "Private Key"),
            (r"(?i)aws[_-]?access[_-]?key", "AWS Access Key"),
            (r"(?i)aws[_-]?secret", "AWS Secret"),
            (r"ghp_[a-zA-Z0-9]{36}", "GitHub Personal Access Token"),
            (r"gho_[a-zA-Z0-9]{36}", "GitHub OAuth Token"),
        ];

        let compiled: Vec<(Regex, &str)> = sensitive_patterns
            .iter()
            .filter_map(|(p, name)| Regex::new(p).ok().map(|r| (r, *name)))
            .collect();

        for entry in WalkDir::new(path)
            .follow_links(false)
            .max_depth(5)
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

            // Skip binary files
            let is_text = entry_path
                .extension()
                .and_then(|e| e.to_str())
                .map(|ext| {
                    matches!(
                        ext,
                        "rs" | "js" | "ts" | "py" | "rb" | "go" | "java" | "kt" | "swift"
                            | "c" | "cpp" | "h" | "hpp" | "yml" | "yaml" | "json" | "toml"
                            | "xml" | "env" | "conf" | "cfg" | "ini" | "sh" | "bash"
                    )
                })
                .unwrap_or(false);

            if !is_text {
                continue;
            }

            if let Ok(content) = std::fs::read_to_string(entry_path) {
                for (pattern, name) in &compiled {
                    if pattern.is_match(&content) {
                        result.add(
                            Finding::new(
                                "REL-008",
                                "Sensitive Data",
                                Severity::Error,
                                &format!("Potential {} found in source file", name),
                            )
                            .with_file(entry_path.to_path_buf())
                            .with_suggestion("Remove sensitive data and rotate any exposed credentials"),
                        );
                        break; // One warning per file
                    }
                }
            }
        }
    }

    /// Check build configuration for release readiness
    fn check_build_config(&self, path: &Path, result: &mut AnalysisResult) {
        // Check Cargo.toml for release profile
        let cargo_toml = path.join("Cargo.toml");
        if cargo_toml.exists() {
            if let Ok(content) = std::fs::read_to_string(&cargo_toml) {
                if !content.contains("[profile.release]") {
                    result.add(
                        Finding::new(
                            "REL-009",
                            "Missing Release Profile",
                            Severity::Info,
                            "No [profile.release] configuration in Cargo.toml",
                        )
                        .with_file(cargo_toml)
                        .with_suggestion("Consider adding release optimizations (lto, codegen-units)"),
                    );
                }
            }
        }

        // Check for .github/workflows release workflow
        let release_workflow = path.join(".github/workflows/release.yml");
        let release_workflow_alt = path.join(".github/workflows/release.yaml");
        if !release_workflow.exists() && !release_workflow_alt.exists() {
            result.add(
                Finding::new(
                    "REL-010",
                    "No Release Workflow",
                    Severity::Info,
                    "No GitHub release workflow found",
                )
                .with_suggestion("Consider adding .github/workflows/release.yml for automated releases"),
            );
        }
    }
}
