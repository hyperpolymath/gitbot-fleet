// SPDX-License-Identifier: PMPL-1.0-or-later
//! Hash Function Analyzer — detects usage of deprecated/weak hash functions.
//!
//! | Status  | Algorithm   | Action                                   |
//! |---------|-------------|------------------------------------------|
//! | REJECT  | MD5         | Error — broken, no use ever               |
//! | REJECT  | SHA-1       | Error — broken, terminate immediately     |
//! | WARN    | SHA-256     | Warning — use SHAKE3-512 or BLAKE3        |
//! | WARN    | SHA-384     | Warning — prefer SHAKE3-512               |
//! | ACCEPT  | SHA-512     | Note — acceptable but prefer SHAKE3-512   |
//! | ACCEPT  | BLAKE3      | OK for speed-critical paths               |
//! | PREFER  | SHAKE3-512  | Ideal — FIPS 202, post-quantum            |

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

/// Pattern definition for hash algorithm detection.
struct HashPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static HASH_PATTERNS: LazyLock<Vec<HashPattern>> = LazyLock::new(|| {
    vec![
        // MD5 — REJECT
        HashPattern {
            regex: Regex::new(r#"(?i)\bmd5(?:::|\.|_|\s*\(|_digest|_hash|_sum|::compute|::Digest)"#).unwrap(),
            algorithm: "MD5",
            status: CryptoStatus::Reject,
            message: "MD5 is cryptographically broken — collision attacks are trivial. No valid use case exists.",
            suggestion: Some("Replace with SHAKE3-512 (FIPS 202) or BLAKE3 for non-cryptographic hashing"),
        },
        HashPattern {
            regex: Regex::new(r#"(?i)crypto\.createHash\s*\(\s*['"]md5['"]\s*\)"#).unwrap(),
            algorithm: "MD5",
            status: CryptoStatus::Reject,
            message: "MD5 is cryptographically broken — collision attacks are trivial.",
            suggestion: Some("Replace with SHAKE3-512 via @noble/hashes or BLAKE3"),
        },
        HashPattern {
            regex: Regex::new(r#"(?i)MessageDigest::md5\s*\(\s*\)"#).unwrap(),
            algorithm: "MD5",
            status: CryptoStatus::Reject,
            message: "MD5 via OpenSSL is cryptographically broken.",
            suggestion: Some("Replace with SHAKE3-512 or BLAKE3"),
        },
        HashPattern {
            regex: Regex::new(r#"(?i)(?:hash_function|algorithm|hash_algo|digest)\s*[:=]\s*['"]?md5['"]?"#).unwrap(),
            algorithm: "MD5",
            status: CryptoStatus::Reject,
            message: "MD5 configured as hash algorithm — cryptographically broken.",
            suggestion: Some("Replace with shake3-512 or blake3"),
        },
        // SHA-1 — REJECT
        HashPattern {
            regex: Regex::new(r#"(?i)\bsha1(?:::|\.|_|\s*\(|_digest|_hash|_sum|::Digest)"#).unwrap(),
            algorithm: "SHA-1",
            status: CryptoStatus::Reject,
            message: "SHA-1 is cryptographically broken — SHAttered attack demonstrated practical collisions.",
            suggestion: Some("Replace with SHAKE3-512 (FIPS 202) or BLAKE3"),
        },
        HashPattern {
            regex: Regex::new(r#"(?i)crypto\.createHash\s*\(\s*['"]sha1?['"]\s*\)"#).unwrap(),
            algorithm: "SHA-1",
            status: CryptoStatus::Reject,
            message: "SHA-1 is cryptographically broken — SHAttered attack.",
            suggestion: Some("Replace with SHAKE3-512 via @noble/hashes"),
        },
        HashPattern {
            regex: Regex::new(r#"(?i)MessageDigest::sha1\s*\(\s*\)"#).unwrap(),
            algorithm: "SHA-1",
            status: CryptoStatus::Reject,
            message: "SHA-1 via OpenSSL is cryptographically broken.",
            suggestion: Some("Replace with SHAKE3-512 or BLAKE3"),
        },
        HashPattern {
            regex: Regex::new(r#"(?i)(?:hash_function|algorithm|hash_algo|digest)\s*[:=]\s*['"]?sha-?1['"]?"#).unwrap(),
            algorithm: "SHA-1",
            status: CryptoStatus::Reject,
            message: "SHA-1 configured as hash algorithm — cryptographically broken.",
            suggestion: Some("Replace with shake3-512 or blake3"),
        },
        // SHA-256 — WARN
        HashPattern {
            regex: Regex::new(r#"(?i)\bsha2?(?:::Sha)?256(?:::|\.|_|\s*\(|_digest|_hash)"#).unwrap(),
            algorithm: "SHA-256",
            status: CryptoStatus::Warn,
            message: "SHA-256 alone is not post-quantum safe. Prefer SHAKE3-512 or BLAKE3.",
            suggestion: Some("Migrate to SHAKE3-512 (FIPS 202) for PQ readiness"),
        },
        HashPattern {
            regex: Regex::new(r#"(?i)crypto\.createHash\s*\(\s*['"]sha256['"]\s*\)"#).unwrap(),
            algorithm: "SHA-256",
            status: CryptoStatus::Warn,
            message: "SHA-256 is not post-quantum safe.",
            suggestion: Some("Replace with SHAKE3-512 via @noble/hashes"),
        },
        HashPattern {
            regex: Regex::new(r#"(?i)ring::digest::SHA256"#).unwrap(),
            algorithm: "SHA-256",
            status: CryptoStatus::Warn,
            message: "SHA-256 via ring is not post-quantum safe.",
            suggestion: Some("Migrate to SHAKE3-512 for PQ readiness"),
        },
        // SHA-384 — WARN
        HashPattern {
            regex: Regex::new(r#"(?i)\bsha2?(?:::Sha)?384(?:::|\.|_|\s*\()"#).unwrap(),
            algorithm: "SHA-384",
            status: CryptoStatus::Warn,
            message: "SHA-384 is acceptable but SHAKE3-512 is preferred for PQ readiness.",
            suggestion: Some("Migrate to SHAKE3-512 (FIPS 202)"),
        },
        // SHA-512 — ACCEPT
        HashPattern {
            regex: Regex::new(r#"(?i)\bsha2?(?:::Sha)?512(?:::|\.|_|\s*\()"#).unwrap(),
            algorithm: "SHA-512",
            status: CryptoStatus::Accept,
            message: "SHA-512 is acceptable but SHAKE3-512 is preferred for FIPS 202 compliance.",
            suggestion: Some("Consider migrating to SHAKE3-512 (FIPS 202)"),
        },
        // BLAKE3 — ACCEPT
        HashPattern {
            regex: Regex::new(r#"(?i)\bblake3(?:::|\.|_|\s*\()"#).unwrap(),
            algorithm: "BLAKE3",
            status: CryptoStatus::Accept,
            message: "BLAKE3 is fast and secure — acceptable for speed-critical paths.",
            suggestion: None,
        },
        // SHAKE3-512 — PREFER
        HashPattern {
            regex: Regex::new(r#"(?i)\bshake(?:3[_-])?512(?:::|\.|_|\s*\()"#).unwrap(),
            algorithm: "SHAKE3-512",
            status: CryptoStatus::Prefer,
            message: "SHAKE3-512 (FIPS 202) — ideal hash function for post-quantum readiness.",
            suggestion: None,
        },
    ]
});

/// Analyzer for hash function usage in source code and configuration files.
pub struct HashingAnalyzer;

impl Analyzer for HashingAnalyzer {
    fn name(&self) -> &str {
        "Hash Function Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/deprecated"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            // Skip comment-only lines to reduce false positives
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
                        suggestion: pattern.suggestion.map(String::from),
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
    fn test_detect_md5_rust() {
        let analyzer = HashingAnalyzer;
        let content = r#"
use md5;
let digest = md5::compute(b"hello");
"#;
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect MD5 usage");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
        assert_eq!(usages[0].algorithm, "MD5");
    }

    #[test]
    fn test_detect_sha1_javascript() {
        let analyzer = HashingAnalyzer;
        let content = r#"
const hash = crypto.createHash('sha1');
hash.update('data');
"#;
        let usages = analyzer.analyze_content(Path::new("test.js"), content);
        assert!(!usages.is_empty(), "Should detect SHA-1 usage");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_sha256_warn() {
        let analyzer = HashingAnalyzer;
        let content = r#"
use sha2::Sha256;
let mut hasher = sha2::Sha256::new();
"#;
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect SHA-256 usage");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_blake3_accept() {
        let analyzer = HashingAnalyzer;
        let content = r#"
let hash = blake3::hash(b"data");
"#;
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect BLAKE3 usage");
        assert_eq!(usages[0].status, CryptoStatus::Accept);
    }

    #[test]
    fn test_detect_shake3_prefer() {
        let analyzer = HashingAnalyzer;
        let content = r#"
let hash = shake3_512::digest(b"data");
"#;
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect SHAKE3-512 usage");
        assert_eq!(usages[0].status, CryptoStatus::Prefer);
    }

    #[test]
    fn test_skip_comments() {
        let analyzer = HashingAnalyzer;
        let content = r#"
// md5::compute is deprecated
# sha1 is broken
"#;
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(usages.is_empty(), "Should skip comments: {:?}", usages);
    }

    #[test]
    fn test_config_md5() {
        let analyzer = HashingAnalyzer;
        let content = r#"
algorithm = "md5"
hash_function: sha1
"#;
        let usages = analyzer.analyze_content(Path::new("config.yml"), content);
        assert!(usages.len() >= 2, "Should detect MD5 and SHA-1 in config");
    }

    #[test]
    fn test_clean_file_no_findings() {
        let analyzer = HashingAnalyzer;
        let content = r#"
fn main() {
    println!("Hello, world!");
    let x = 42;
}
"#;
        let usages = analyzer.analyze_content(Path::new("clean.rs"), content);
        assert!(usages.is_empty(), "Clean file should have no findings");
    }
}
