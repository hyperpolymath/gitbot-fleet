// SPDX-License-Identifier: PMPL-1.0-or-later
//! Cryptographic analyzers for detecting deprecated, weak, and non-PQ-safe algorithms.
//!
//! Each analyzer scans source code, configuration files, and dependency manifests
//! for usage of cryptographic primitives and reports findings with severity levels.

pub mod config;
pub mod deps;
pub mod dns;
pub mod hashing;
pub mod infra;
pub mod key_exchange;
pub mod password;
pub mod protocol;
pub mod rng;
pub mod signatures;
pub mod symmetric;

use gitbot_shared_context::finding::{Finding, Severity};
use gitbot_shared_context::bot::BotId;
use std::path::{Path, PathBuf};

/// The BotId used for all cipherbot findings.
/// Uses Custom(100) since cipherbot is not yet in the upstream BotId enum.
pub const CIPHERBOT_ID: BotId = BotId::Custom(100);

/// Classification of a cryptographic algorithm's security status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CryptoStatus {
    /// Algorithm is broken — immediate rejection required.
    Reject,
    /// Algorithm has known weaknesses — migration recommended.
    Warn,
    /// Algorithm is acceptable for current use.
    Accept,
    /// Algorithm is the preferred/ideal choice.
    Prefer,
    /// Algorithm is a conservative post-quantum backup.
    Fallback,
}

impl CryptoStatus {
    /// Convert to gitbot-fleet severity level.
    pub fn to_severity(self) -> Severity {
        match self {
            CryptoStatus::Reject => Severity::Error,
            CryptoStatus::Warn => Severity::Warning,
            CryptoStatus::Accept => Severity::Info,
            CryptoStatus::Prefer => Severity::Suggestion,
            CryptoStatus::Fallback => Severity::Info,
        }
    }
}

/// A detected usage of a cryptographic algorithm in source code or configuration.
#[derive(Debug, Clone)]
pub struct CryptoUsage {
    /// The algorithm detected (e.g., "MD5", "SHA-256", "AES-ECB").
    pub algorithm: String,
    /// Security status of the algorithm.
    pub status: CryptoStatus,
    /// File where the usage was found.
    pub file: PathBuf,
    /// Line number (1-indexed).
    pub line: usize,
    /// The matched text fragment.
    pub matched_text: String,
    /// Category for fleet findings (e.g., "crypto/deprecated").
    pub category: String,
    /// Human-readable description of the issue.
    pub message: String,
    /// Suggested replacement algorithm.
    pub suggestion: Option<String>,
}

impl CryptoUsage {
    /// Convert this usage into a gitbot-fleet Finding.
    pub fn to_finding(&self) -> Finding {
        let mut finding = Finding::new(
            CIPHERBOT_ID,
            &format!("CIPHER-{}", self.algorithm.to_uppercase().replace(['-', ' '], "_")),
            self.status.to_severity(),
            &self.message,
        )
        .with_category(&self.category)
        .with_file(self.file.clone())
        .with_line(self.line)
        .with_element(&self.matched_text);

        if let Some(ref suggestion) = self.suggestion {
            finding = finding.with_suggestion(suggestion);
        }

        finding
    }
}

/// Trait for all cryptographic analyzers.
///
/// Each analyzer scans files for specific categories of cryptographic usage
/// (hashing, symmetric encryption, key exchange, etc.) and returns a list
/// of detected usages with their security classifications.
pub trait Analyzer {
    /// Human-readable name of this analyzer.
    fn name(&self) -> &str;

    /// Finding category prefix (e.g., "crypto/deprecated").
    fn category(&self) -> &str;

    /// Analyze a single file's contents and return detected crypto usages.
    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage>;

    /// Analyze a directory tree, scanning all relevant files.
    fn analyze_directory(&self, dir: &Path) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();
        let walker = walkdir::WalkDir::new(dir)
            .follow_links(false)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file());

        for entry in walker {
            let path = entry.path();
            // Skip binary files, .git, node_modules, target
            if should_skip_path(path) {
                continue;
            }
            if let Ok(content) = std::fs::read_to_string(path) {
                usages.extend(self.analyze_content(path, &content));
            }
        }
        usages
    }
}

/// Check if a path should be skipped during analysis.
fn should_skip_path(path: &Path) -> bool {
    let path_str = path.to_string_lossy();
    let skip_dirs = [
        ".git/",
        "node_modules/",
        "target/",
        ".cargo/",
        "vendor/",
        "__pycache__/",
        ".tox/",
        "dist/",
        "build/",
        ".next/",
    ];
    for dir in &skip_dirs {
        if path_str.contains(dir) {
            return true;
        }
    }
    // Skip binary-looking extensions
    let skip_exts = [
        "png", "jpg", "jpeg", "gif", "ico", "woff", "woff2", "ttf", "eot",
        "pdf", "zip", "tar", "gz", "bz2", "xz", "exe", "dll", "so", "dylib",
        "o", "a", "class", "pyc", "wasm",
    ];
    if let Some(ext) = path.extension() {
        let ext_str = ext.to_string_lossy().to_lowercase();
        if skip_exts.contains(&ext_str.as_str()) {
            return true;
        }
    }
    false
}

/// Run all analyzers against a directory and collect findings.
pub fn run_all_analyzers(dir: &Path) -> Vec<Finding> {
    let analyzers: Vec<Box<dyn Analyzer>> = vec![
        Box::new(hashing::HashingAnalyzer),
        Box::new(symmetric::SymmetricAnalyzer),
        Box::new(key_exchange::KeyExchangeAnalyzer),
        Box::new(signatures::SignatureAnalyzer),
        Box::new(password::PasswordAnalyzer),
        Box::new(protocol::ProtocolAnalyzer),
        Box::new(rng::RngAnalyzer),
        Box::new(deps::DependencyAnalyzer),
        Box::new(config::ConfigAnalyzer),
        Box::new(dns::DnsAnalyzer),
        Box::new(infra::InfraAnalyzer),
    ];

    let mut findings = Vec::new();
    for analyzer in &analyzers {
        let usages = analyzer.analyze_directory(dir);
        findings.extend(usages.iter().map(|u| u.to_finding()));
    }
    findings
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_crypto_status_to_severity() {
        assert_eq!(CryptoStatus::Reject.to_severity(), Severity::Error);
        assert_eq!(CryptoStatus::Warn.to_severity(), Severity::Warning);
        assert_eq!(CryptoStatus::Accept.to_severity(), Severity::Info);
        assert_eq!(CryptoStatus::Prefer.to_severity(), Severity::Suggestion);
        assert_eq!(CryptoStatus::Fallback.to_severity(), Severity::Info);
    }

    #[test]
    fn test_should_skip_path() {
        assert!(should_skip_path(Path::new("project/.git/config")));
        assert!(should_skip_path(Path::new("project/node_modules/foo/bar.js")));
        assert!(should_skip_path(Path::new("project/target/debug/main")));
        assert!(should_skip_path(Path::new("image.png")));
        assert!(!should_skip_path(Path::new("src/main.rs")));
        assert!(!should_skip_path(Path::new("config.toml")));
    }

    #[test]
    fn test_crypto_usage_to_finding() {
        let usage = CryptoUsage {
            algorithm: "MD5".to_string(),
            status: CryptoStatus::Reject,
            file: PathBuf::from("src/lib.rs"),
            line: 42,
            matched_text: "md5::compute".to_string(),
            category: "crypto/deprecated".to_string(),
            message: "MD5 is cryptographically broken".to_string(),
            suggestion: Some("Use SHAKE3-512 or BLAKE3".to_string()),
        };
        let finding = usage.to_finding();
        assert_eq!(finding.severity, Severity::Error);
        assert_eq!(finding.category, "crypto/deprecated");
        assert_eq!(finding.line, Some(42));
        assert!(finding.suggestion.is_some());
    }
}
