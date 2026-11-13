// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Release readiness analyzer

use super::{first_existing, AnalysisResult, Analyzer, Finding, Severity, SECURITY_POLICY};
use crate::config::Config;
use crate::error::Result;
use regex::Regex;
use sha2::{Digest, Sha256, Sha512};
use std::path::Path;
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
                        &format!(
                            "Version numbers are inconsistent:\n  {}",
                            version_list.join("\n  ")
                        ),
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
            "println!", // In Rust release builds
            "#[cfg(debug_assertions)]",
        ];

        let debug_files = [".env.local", ".env.development", "debug.log", "*.log"];

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
                                &format!(
                                    "Found {} occurrence(s) of '{}' in source",
                                    count, pattern
                                ),
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
        // SECURITY.md or .adoc, at the root or in .github/ (GitHub reads both).
        if first_existing(path, SECURITY_POLICY).is_none() {
            result.add(
                Finding::new(
                    "REL-004",
                    "Missing Security Policy",
                    Severity::Warning,
                    "No SECURITY.md found (checked the root and .github/)",
                )
                .with_file(path.join(SECURITY_POLICY[0]))
                .with_suggestion("Add a SECURITY.md describing how to report vulnerabilities"),
            );
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
        let hash_dir = match hash_path.parent() {
            Some(dir) => dir,
            None => return,
        };
        let target_path = hash_dir.join(filename);

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

        let actual_hash = if config
            .release
            .hash_algorithms
            .contains(&"SHA512".to_string())
            && hash_path.to_string_lossy().contains("512")
        {
            let mut hasher = Sha512::new();
            hasher.update(&file_content);
            hex::encode(hasher.finalize())
        } else {
            let mut hasher = Sha256::new();
            hasher.update(&file_content);
            hex::encode(hasher.finalize())
        };

        if actual_hash.to_lowercase() != expected_hash.to_lowercase() {
            result.add(
                Finding::new(
                    "REL-007",
                    "Hash Mismatch",
                    Severity::Error,
                    &format!(
                        "Hash mismatch for {}: expected {}, got {}",
                        filename, expected_hash, actual_hash
                    ),
                )
                .with_file(hash_path.to_path_buf())
                .with_suggestion("Regenerate hash file or investigate file corruption"),
            );
        }
    }

    /// Check for sensitive data that shouldn't be released
    fn check_sensitive_data(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
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
                        "rs" | "js"
                            | "ts"
                            | "py"
                            | "rb"
                            | "go"
                            | "java"
                            | "kt"
                            | "swift"
                            | "c"
                            | "cpp"
                            | "h"
                            | "hpp"
                            | "yml"
                            | "yaml"
                            | "json"
                            | "toml"
                            | "xml"
                            | "env"
                            | "conf"
                            | "cfg"
                            | "ini"
                            | "sh"
                            | "bash"
                    )
                })
                .unwrap_or(false);

            if !is_text {
                continue;
            }

            if let Ok(content) = std::fs::read_to_string(entry_path) {
                if let Some(name) = sensitive_kind(&content) {
                    result.add(
                        Finding::new(
                            "REL-008",
                            "Sensitive Data",
                            Severity::Error,
                            &format!("Potential {} found in source file", name),
                        )
                        .with_file(entry_path.to_path_buf())
                        .with_suggestion(
                            "Remove sensitive data and rotate any exposed credentials",
                        ),
                    );
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
                        .with_suggestion(
                            "Consider adding release optimizations (lto, codegen-units)",
                        ),
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
                .with_suggestion(
                    "Consider adding .github/workflows/release.yml for automated releases",
                ),
            );
        }
    }
}

/// Key material patterns for REL-008. These match credentials themselves
/// (PEM headers, assignments of a long literal, vendor token shapes), never
/// the WORDS "private key" or "aws secret": a config label such as
/// `trust.no-private-keys = "probe-deferred"` or a document describing key
/// hygiene is not a leaked key.
static SENSITIVE_PATTERNS: std::sync::LazyLock<Vec<(Regex, &'static str)>> =
    std::sync::LazyLock::new(|| {
        [
            (
                r#"(?i)api[_-]?key\s*[:=]\s*['"]?[a-zA-Z0-9]{20,}"#,
                "API Key",
            ),
            (
                r#"(?i)secret[_-]?key\s*[:=]\s*['"]?[a-zA-Z0-9]{20,}"#,
                "Secret Key",
            ),
            (
                r#"(?i)aws[_-]?secret[_-]?(access[_-]?)?key\s*[:=]\s*['"]?[A-Za-z0-9/+=]{40}(?:$|[^A-Za-z0-9/+=])"#,
                "AWS Secret Access Key",
            ),
            (r#"(?i)password\s*[:=]\s*['"]?[^\s'"]{8,}"#, "Password"),
            (
                r"-----BEGIN ([A-Z0-9]+ )*PRIVATE KEY( BLOCK)?-----",
                "Private Key",
            ),
            (r"\b(AKIA|ASIA)[0-9A-Z]{16}\b", "AWS Access Key ID"),
            (r"ghp_[a-zA-Z0-9]{36}", "GitHub Personal Access Token"),
            (r"gho_[a-zA-Z0-9]{36}", "GitHub OAuth Token"),
        ]
        .iter()
        .map(|(p, name)| {
            (
                Regex::new(p).expect("REL-008 sensitive pattern is a valid constant regex"),
                *name,
            )
        })
        .collect()
    });

/// The kind of key material `content` contains, if any (first match wins).
fn sensitive_kind(content: &str) -> Option<&'static str> {
    SENSITIVE_PATTERNS
        .iter()
        .find(|(re, _)| re.is_match(content))
        .map(|(_, name)| *name)
}

#[cfg(test)]
mod sensitive_tests {
    use super::*;

    #[test]
    fn words_about_keys_are_not_key_material() {
        for text in [
            r#"trust.no-private-keys = "probe-deferred""#,
            "Never commit a private key or an AWS secret.",
            "aws_access_key and aws-secret are documented here",
            "private_key_path = \"/etc/ssl/key\"",
        ] {
            assert_eq!(sensitive_kind(text), None, "{text}");
        }
    }

    #[test]
    fn aws_secret_access_key_value_is_key_material() {
        for text in [
            r#"aws_secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY""#,
            "AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
        ] {
            assert_eq!(
                sensitive_kind(text),
                Some("AWS Secret Access Key"),
                "{text}"
            );
        }
        assert_eq!(sensitive_kind("aws_secret_access_key = \"$SECRET\""), None);
        assert_eq!(
            sensitive_kind("The aws_secret_access_key is rotated monthly"),
            None
        );
    }

    #[test]
    fn aws_secret_ending_in_base64_punctuation_is_key_material() {
        // The 40th character may be `/`, `+` or `=`: `\b` cannot terminate those.
        for last in ['/', '+', '=', 'A', '9'] {
            let key = format!("{}{}", "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKE", last);
            assert_eq!(key.len(), 40);
            for text in [
                format!("aws_secret_access_key = \"{key}\""),
                format!("AWS_SECRET_ACCESS_KEY={key}"),
                format!("AWS_SECRET_ACCESS_KEY={key}\n"),
                format!("aws_secret_access_key: {key} # note"),
            ] {
                assert_eq!(
                    sensitive_kind(&text),
                    Some("AWS Secret Access Key"),
                    "{text}"
                );
            }
        }
        // A run longer than 40 key characters is not a 40-character key.
        let long = format!("AWS_SECRET_ACCESS_KEY={}", "A".repeat(41));
        assert_eq!(sensitive_kind(&long), None);
        let long_slash = format!("AWS_SECRET_ACCESS_KEY={}/", "A".repeat(40));
        assert_eq!(sensitive_kind(&long_slash), None);
    }

    #[test]
    fn pem_headers_are_key_material() {
        for kind in [
            "RSA PRIVATE KEY",
            "PRIVATE KEY",
            "EC PRIVATE KEY",
            "OPENSSH PRIVATE KEY",
            "ENCRYPTED PRIVATE KEY",
        ] {
            let text = format!("{}{}{}\nMIIB\n", "-----BEGIN ", kind, "-----");
            assert_eq!(sensitive_kind(&text), Some("Private Key"), "{kind}");
        }
        let pgp = format!("{}{}{}", "-----BEGIN ", "PGP PRIVATE KEY BLOCK", "-----");
        assert_eq!(sensitive_kind(&pgp), Some("Private Key"));
    }

    #[test]
    fn key_literals_are_key_material() {
        let aws = format!("id = {}{}", "AKIA", "IOSFODNN7EXAMPLE");
        assert_eq!(sensitive_kind(&aws), Some("AWS Access Key ID"));
        let ghp = format!("t = {}{}", "ghp_", "a".repeat(36));
        assert_eq!(sensitive_kind(&ghp), Some("GitHub Personal Access Token"));
        assert_eq!(
            sensitive_kind("api_key = \"abcdefghij0123456789xyz\""),
            Some("API Key")
        );
    }

    #[test]
    fn rel_008_ignores_a_label_but_reports_a_key_file() {
        let dir = tempfile::TempDir::new().unwrap();
        std::fs::write(
            dir.path().join("trust.toml"),
            "trust.no-private-keys = \"probe-deferred\"\n",
        )
        .unwrap();
        let mut result = AnalysisResult::new();
        ReleaseAnalyzer.check_sensitive_data(dir.path(), &Config::default(), &mut result);
        assert!(result.findings.is_empty(), "{:?}", result.findings);

        std::fs::write(
            dir.path().join("leak.toml"),
            format!(
                "k = \"\"\"\n-----BEGIN {} KEY-----\nAAAA\n\"\"\"\n",
                "PRIVATE"
            ),
        )
        .unwrap();
        ReleaseAnalyzer.check_sensitive_data(dir.path(), &Config::default(), &mut result);
        assert_eq!(result.findings.len(), 1);
        assert_eq!(result.findings[0].id, "REL-008");
    }

    #[test]
    fn security_policy_is_found_in_dot_github() {
        let dir = tempfile::TempDir::new().unwrap();
        let mut result = AnalysisResult::new();
        ReleaseAnalyzer.check_security_files(dir.path(), &mut result);
        assert_eq!(result.findings.len(), 1);
        assert_eq!(result.findings[0].id, "REL-004");

        std::fs::create_dir_all(dir.path().join(".github")).unwrap();
        std::fs::write(dir.path().join(".github/SECURITY.md"), "x").unwrap();
        let mut again = AnalysisResult::new();
        ReleaseAnalyzer.check_security_files(dir.path(), &mut again);
        assert!(again.findings.is_empty());
    }
}
