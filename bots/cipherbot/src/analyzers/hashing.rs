// SPDX-License-Identifier: PMPL-1.0-or-later

//! Hash Function Analyzer — Cryptographic Security Audit.
//!
//! This module implements a static analyzer that detects the usage of
//! deprecated or insecure hashing algorithms. It enforces the project's
//! "Danger Zone" policy, which mandates the termination of MD5 and
//! SHA-1 usage.
//!
//! AUDIT TIERS:
//! - **REJECT**: MD5, SHA-1 (Cryptographically broken).
//! - **WARN**: SHA-256 (Non-PQ safe, prefer SHAKE3).
//! - **ACCEPT**: BLAKE3 (High performance), SHA-512.
//! - **PREFER**: SHAKE3-512 (FIPS 202, Post-Quantum ready).

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct HashPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
}

/// PATTERN REGISTRY: A collection of regex-based detectors for various
/// language-specific crypto APIs (Node.js crypto, OpenSSL, etc.).
static HASH_PATTERNS: LazyLock<Vec<HashPattern>> = LazyLock::new(|| {
    vec![
        // MD5 DETECTOR: Rejects both standalone and library-specific calls.
        HashPattern {
            regex: Regex::new(r#"(?i)\bmd5(?:::|\.|_|\s*\()"#).expect("static regex is valid"),
            algorithm: "MD5",
            status: CryptoStatus::Reject,
            message: "MD5 is broken. Replace with SHAKE3-512 or BLAKE3.",
        },
        // SHA-1 DETECTOR: Rejects collision-vulnerable SHA-1 usage.
        HashPattern {
            regex: Regex::new(r#"(?i)\bsha[_-]?1(?:::|\.|_|\s*\(|\b)"#).expect("static regex is valid"),
            algorithm: "SHA-1",
            status: CryptoStatus::Reject,
            message: "SHA-1 is broken (SHAttered collision). Replace with SHAKE3-512 or BLAKE3.",
        },
        // SHA-256 DETECTOR: Acceptable today, but not post-quantum preferred.
        HashPattern {
            regex: Regex::new(r#"(?i)\bsha[_-]?256\b"#).expect("static regex is valid"),
            algorithm: "SHA-256",
            status: CryptoStatus::Warn,
            message: "SHA-256 is sound but not post-quantum preferred. Prefer SHAKE3-512.",
        },
        // SHA-512 DETECTOR: Acceptable.
        HashPattern {
            regex: Regex::new(r#"(?i)\bsha[_-]?512\b"#).expect("static regex is valid"),
            algorithm: "SHA-512",
            status: CryptoStatus::Accept,
            message: "SHA-512 is acceptable. Consider SHAKE3-512 for post-quantum margin.",
        },
        // BLAKE3 DETECTOR: High-performance, acceptable.
        HashPattern {
            regex: Regex::new(r#"(?i)\bblake3\b"#).expect("static regex is valid"),
            algorithm: "BLAKE3",
            status: CryptoStatus::Accept,
            message: "BLAKE3 is a strong, high-performance hash.",
        },
        // SHAKE3-512 DETECTOR: FIPS 202, post-quantum preferred.
        HashPattern {
            regex: Regex::new(r#"(?i)\bshake3[_-]?512\b"#).expect("static regex is valid"),
            algorithm: "SHAKE3-512",
            status: CryptoStatus::Prefer,
            message: "SHAKE3-512 is the preferred post-quantum-ready hash (FIPS 202).",
        },
    ]
});

/// Analyzer for hash function usage.
pub struct HashingAnalyzer;

impl Analyzer for HashingAnalyzer {
    fn name(&self) -> &str {
        "Hash Function Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/deprecated"
    }

    /// AUDIT: Scans the content of a file line-by-line, skipping
    /// comments to minimize false positives.
    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.starts_with('#') || trimmed.starts_with("/*") {
                continue;
            }

            for pattern in HASH_PATTERNS.iter() {
                if pattern.regex.is_match(line) {
                    usages.push(CryptoUsage {
                        algorithm: pattern.algorithm.to_string(),
                        status: pattern.status,
                        file: path.to_path_buf(),
                        line: line_num + 1,
                        matched_text: line.trim().to_string(),
                        category: self.category().to_string(),
                        message: pattern.message.to_string(),
                        suggestion: None,
                    });
                }
            }
        }
        usages
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_md5() {
        let analyzer = HashingAnalyzer;
        let content = "let digest = md5::compute(&data);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect MD5");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_sha1() {
        let analyzer = HashingAnalyzer;
        let content = "let digest = sha1::Sha1::new();";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect SHA-1");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_sha256_warn() {
        let analyzer = HashingAnalyzer;
        let content = "let digest = sha256(&data);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect SHA-256");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_shake3_512_prefer() {
        let analyzer = HashingAnalyzer;
        let content = "let digest = shake3_512(&data);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect SHAKE3-512");
        assert_eq!(usages[0].status, CryptoStatus::Prefer);
    }

    #[test]
    fn test_clean_file() {
        let analyzer = HashingAnalyzer;
        let content = "fn main() { println!(\"hello\"); }";
        let usages = analyzer.analyze_content(Path::new("clean.rs"), content);
        assert!(usages.is_empty(), "Clean file should have no findings");
    }
}
