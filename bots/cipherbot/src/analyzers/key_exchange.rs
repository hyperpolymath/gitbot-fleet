// SPDX-License-Identifier: PMPL-1.0-or-later
//! Key Exchange Analyzer — detects weak or non-PQ-safe key exchange algorithms.
//!
//! | Status  | Algorithm                  | Action                        |
//! |---------|----------------------------|-------------------------------|
//! | REJECT  | RSA-1024                   | Error — factorable            |
//! | REJECT  | DH-1024                    | Error — logjam attack         |
//! | WARN    | RSA-2048                   | Warning — not PQ-safe         |
//! | WARN    | ECDH (P-256)               | Warning — not PQ-safe         |
//! | ACCEPT  | X25519                     | OK for classical, not PQ      |
//! | ACCEPT  | X448                       | OK for classical, not PQ      |
//! | PREFER  | Kyber-1024 + SHAKE256-KDF  | Ideal — ML-KEM-1024, FIPS 203 |

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct KeyExchangePattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static KE_PATTERNS: LazyLock<Vec<KeyExchangePattern>> = LazyLock::new(|| {
    vec![
        // RSA-1024 — REJECT
        KeyExchangePattern {
            regex: Regex::new(r#"(?i)\brsa[_-]?1024\b"#).unwrap(),
            algorithm: "RSA-1024",
            status: CryptoStatus::Reject,
            message: "RSA-1024 is factorable with current hardware — completely insecure.",
            suggestion: Some("Migrate to Kyber-1024 + SHAKE256-KDF (ML-KEM-1024, FIPS 203)"),
        },
        // DH-1024 — REJECT
        KeyExchangePattern {
            regex: Regex::new(r#"(?i)\b(?:dh[_-]?1024|diffie.?hellman.?1024)\b"#).unwrap(),
            algorithm: "DH-1024",
            status: CryptoStatus::Reject,
            message: "DH-1024 is vulnerable to the Logjam attack — insecure.",
            suggestion: Some("Migrate to Kyber-1024 + SHAKE256-KDF (ML-KEM-1024, FIPS 203)"),
        },
        // RSA-2048 — WARN
        KeyExchangePattern {
            regex: Regex::new(r#"(?i)\brsa[_-]?2048\b"#).unwrap(),
            algorithm: "RSA-2048",
            status: CryptoStatus::Warn,
            message: "RSA-2048 is not post-quantum safe — vulnerable to Shor's algorithm on quantum computers.",
            suggestion: Some("Migrate to Kyber-1024 + SHAKE256-KDF hybrid for PQ safety"),
        },
        // ECDH P-256 — WARN
        KeyExchangePattern {
            regex: Regex::new(r#"(?i)\b(?:ecdh[_-]?p[_-]?256|P-?256|secp256r1|prime256v1|NistP256)\b"#).unwrap(),
            algorithm: "ECDH-P256",
            status: CryptoStatus::Warn,
            message: "ECDH with P-256 is not post-quantum safe — vulnerable to quantum attacks.",
            suggestion: Some("Migrate to Kyber-1024 + X448 hybrid for PQ safety"),
        },
        // X25519 — ACCEPT
        KeyExchangePattern {
            regex: Regex::new(r#"(?i)\b(?:x25519|curve25519)\b"#).unwrap(),
            algorithm: "X25519",
            status: CryptoStatus::Accept,
            message: "X25519 is strong for classical key exchange but not post-quantum safe.",
            suggestion: Some("Consider hybrid X25519 + Kyber-1024 for PQ readiness"),
        },
        // X448 — ACCEPT
        KeyExchangePattern {
            regex: Regex::new(r#"(?i)\b(?:x448|curve448)\b"#).unwrap(),
            algorithm: "X448",
            status: CryptoStatus::Accept,
            message: "X448 is strong for classical key exchange but not post-quantum safe.",
            suggestion: Some("Consider hybrid X448 + Kyber-1024 for PQ readiness"),
        },
        // Kyber — PREFER
        KeyExchangePattern {
            regex: Regex::new(r#"(?i)\b(?:kyber[_-]?1024|ml[_-]?kem[_-]?1024)\b"#).unwrap(),
            algorithm: "Kyber-1024",
            status: CryptoStatus::Prefer,
            message: "Kyber-1024 (ML-KEM-1024, FIPS 203) — ideal post-quantum key encapsulation.",
            suggestion: None,
        },
        // Kyber-768 — ACCEPT (lower security level)
        KeyExchangePattern {
            regex: Regex::new(r#"(?i)\b(?:kyber[_-]?768|ml[_-]?kem[_-]?768)\b"#).unwrap(),
            algorithm: "Kyber-768",
            status: CryptoStatus::Accept,
            message: "Kyber-768 (ML-KEM-768) provides PQ safety but Kyber-1024 is preferred.",
            suggestion: Some("Consider upgrading to Kyber-1024 (ML-KEM-1024) for maximum security"),
        },
    ]
});

/// Analyzer for key exchange algorithm usage.
pub struct KeyExchangeAnalyzer;

impl Analyzer for KeyExchangeAnalyzer {
    fn name(&self) -> &str {
        "Key Exchange Analyzer"
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

            for pattern in KE_PATTERNS.iter() {
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
    fn test_detect_rsa_1024() {
        let analyzer = KeyExchangeAnalyzer;
        let content = "let key = rsa_1024::generate();";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect RSA-1024");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_dh_1024() {
        let analyzer = KeyExchangeAnalyzer;
        let content = "key_size: dh_1024";
        let usages = analyzer.analyze_content(Path::new("config.yml"), content);
        assert!(!usages.is_empty(), "Should detect DH-1024");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_ecdh_p256_warn() {
        let analyzer = KeyExchangeAnalyzer;
        let content = "let kex = NistP256::key_exchange();";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect ECDH P-256");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_x25519_accept() {
        let analyzer = KeyExchangeAnalyzer;
        let content = "let shared = x25519::diffie_hellman(&secret, &public);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect X25519");
        assert_eq!(usages[0].status, CryptoStatus::Accept);
    }

    #[test]
    fn test_detect_kyber_1024_prefer() {
        let analyzer = KeyExchangeAnalyzer;
        let content = "let kem = ml_kem_1024::encapsulate(&pk);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect Kyber-1024");
        assert_eq!(usages[0].status, CryptoStatus::Prefer);
    }

    #[test]
    fn test_clean_file() {
        let analyzer = KeyExchangeAnalyzer;
        let content = "fn main() { let x = 1024; }";
        let usages = analyzer.analyze_content(Path::new("clean.rs"), content);
        assert!(usages.is_empty(), "Clean file should have no findings");
    }
}
