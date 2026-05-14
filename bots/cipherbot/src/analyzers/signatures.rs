// SPDX-License-Identifier: PMPL-1.0-or-later
//! Signature Analyzer — detects weak or non-PQ-safe digital signature algorithms.
//!
//! | Status   | Algorithm                       | Action                      |
//! |----------|----------------------------------|------------------------------|
//! | REJECT   | RSA-SHA1                         | Error — SHA1 broken          |
//! | REJECT   | DSA                              | Error — deprecated           |
//! | WARN     | RSA-2048                         | Warning — not PQ-safe        |
//! | WARN     | Ed25519                          | Warning — classical only     |
//! | ACCEPT   | Ed448                            | OK for classical             |
//! | PREFER   | Dilithium5-AES + Ed448 hybrid    | Ideal — ML-DSA-87, FIPS 204  |
//! | FALLBACK | SPHINCS+                         | Conservative PQ backup       |

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct SignaturePattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static SIG_PATTERNS: LazyLock<Vec<SignaturePattern>> = LazyLock::new(|| {
    vec![
        // RSA-SHA1 — REJECT
        SignaturePattern {
            regex: Regex::new(r#"(?i)\b(?:rsa[_-]?sha1|sha1[_-]?rsa|sha1WithRSA)\b"#).unwrap(),
            algorithm: "RSA-SHA1",
            status: CryptoStatus::Reject,
            message: "RSA-SHA1 signatures use broken SHA-1 — completely insecure.",
            suggestion: Some("Migrate to Dilithium5 + Ed448 hybrid (ML-DSA-87, FIPS 204)"),
        },
        // DSA — REJECT
        SignaturePattern {
            regex: Regex::new(r#"(?i)\b(?:dsa(?:::|\.|_|[_-]sign)|DSA[_-]?(?:sign|verify|key))\b"#).unwrap(),
            algorithm: "DSA",
            status: CryptoStatus::Reject,
            message: "DSA is deprecated (NIST SP 800-131A Rev.2) — do not use for new applications.",
            suggestion: Some("Migrate to Dilithium5 + Ed448 hybrid (ML-DSA-87, FIPS 204)"),
        },
        // RSA-2048 signatures — WARN
        SignaturePattern {
            regex: Regex::new(r#"(?i)\b(?:rsa[_-]?2048[_-]?(?:sign|verify|pss)|RSA2048)\b"#).unwrap(),
            algorithm: "RSA-2048-sig",
            status: CryptoStatus::Warn,
            message: "RSA-2048 signatures are not post-quantum safe — vulnerable to Shor's algorithm.",
            suggestion: Some("Migrate to Dilithium5 + Ed448 hybrid for PQ readiness"),
        },
        // Ed25519 — WARN
        SignaturePattern {
            regex: Regex::new(r#"(?i)\b(?:ed25519|Ed25519)\b"#).unwrap(),
            algorithm: "Ed25519",
            status: CryptoStatus::Warn,
            message: "Ed25519 is classical-only — not post-quantum safe. Prefer Ed448 for classical use.",
            suggestion: Some("Migrate to Ed448 + Dilithium5 hybrid for PQ readiness"),
        },
        // Ed448 — ACCEPT
        SignaturePattern {
            regex: Regex::new(r#"(?i)\b(?:ed448|Ed448)\b"#).unwrap(),
            algorithm: "Ed448",
            status: CryptoStatus::Accept,
            message: "Ed448 is strong for classical signatures — 224-bit security level.",
            suggestion: Some("Consider hybrid Ed448 + Dilithium5 for PQ readiness"),
        },
        // Dilithium / ML-DSA — PREFER
        SignaturePattern {
            regex: Regex::new(r#"(?i)\b(?:dilithium[_-]?5|ml[_-]?dsa[_-]?87|ML-DSA-87)\b"#).unwrap(),
            algorithm: "Dilithium5",
            status: CryptoStatus::Prefer,
            message: "Dilithium5 (ML-DSA-87, FIPS 204) — ideal post-quantum signature algorithm.",
            suggestion: None,
        },
        // SPHINCS+ — FALLBACK
        SignaturePattern {
            regex: Regex::new(r#"(?i)\b(?:sphincs\+?|SPHINCS|slh[_-]?dsa)\b"#).unwrap(),
            algorithm: "SPHINCS+",
            status: CryptoStatus::Fallback,
            message: "SPHINCS+ (SLH-DSA) — conservative hash-based PQ backup signature scheme.",
            suggestion: None,
        },
    ]
});

/// Analyzer for digital signature algorithm usage.
pub struct SignatureAnalyzer;

impl Analyzer for SignatureAnalyzer {
    fn name(&self) -> &str {
        "Signature Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/pq-vulnerable"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.starts_with('#') || trimmed.starts_with("/*") {
                continue;
            }

            for pattern in SIG_PATTERNS.iter() {
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
    fn test_detect_rsa_sha1() {
        let analyzer = SignatureAnalyzer;
        let content = "algorithm: sha1WithRSA";
        let usages = analyzer.analyze_content(Path::new("config.yml"), content);
        assert!(!usages.is_empty(), "Should detect RSA-SHA1");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_dsa() {
        let analyzer = SignatureAnalyzer;
        let content = "let sig = dsa::sign(&key, &message);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect DSA");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_ed25519_warn() {
        let analyzer = SignatureAnalyzer;
        let content = "let sig = Ed25519::sign(&sk, &msg);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect Ed25519");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_ed448_accept() {
        let analyzer = SignatureAnalyzer;
        let content = "let sig = Ed448::sign(&sk, &msg);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect Ed448");
        assert_eq!(usages[0].status, CryptoStatus::Accept);
    }

    #[test]
    fn test_detect_dilithium5_prefer() {
        let analyzer = SignatureAnalyzer;
        let content = "let sig = ml_dsa_87::sign(&sk, &msg);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect Dilithium5/ML-DSA-87");
        assert_eq!(usages[0].status, CryptoStatus::Prefer);
    }

    #[test]
    fn test_detect_sphincs_fallback() {
        let analyzer = SignatureAnalyzer;
        let content = "let sig = sphincs::sign(&sk, &msg);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect SPHINCS+");
        assert_eq!(usages[0].status, CryptoStatus::Fallback);
    }
}
