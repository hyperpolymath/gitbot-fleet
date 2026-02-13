// SPDX-License-Identifier: PMPL-1.0-or-later
//! Symmetric Encryption Analyzer — detects weak or deprecated symmetric ciphers.
//!
//! | Status  | Algorithm            | Action                                        |
//! |---------|----------------------|-----------------------------------------------|
//! | REJECT  | DES, 3DES            | Error — broken                                |
//! | REJECT  | RC4                  | Error — broken                                |
//! | REJECT  | AES-ECB              | Error — no integrity, patterns visible         |
//! | WARN    | AES-CBC              | Warning — prefer authenticated encryption      |
//! | WARN    | AES-GCM (128-bit)    | Warning — prefer 256-bit for quantum margin    |
//! | ACCEPT  | AES-GCM (256-bit)    | OK                                            |
//! | PREFER  | XChaCha20-Poly1305   | Ideal — larger nonce space, 256-bit           |

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct SymmetricPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static SYMMETRIC_PATTERNS: LazyLock<Vec<SymmetricPattern>> = LazyLock::new(|| {
    vec![
        // DES — REJECT
        SymmetricPattern {
            regex: Regex::new(r#"(?i)\b(?:des(?:::|-|_)|DES(?:_CBC|_ECB|_CFB|_OFB)|des_ede|triple.?des|3des)\b"#).unwrap(),
            algorithm: "DES/3DES",
            status: CryptoStatus::Reject,
            message: "DES/3DES is cryptographically broken — brute-force is trivial with modern hardware.",
            suggestion: Some("Replace with XChaCha20-Poly1305 (256-bit) or AES-256-GCM"),
        },
        SymmetricPattern {
            regex: Regex::new(r#"(?i)crypto\.createCipher(?:iv)?\s*\(\s*['"](?:des|des3|des-ede3)['"]\s*"#).unwrap(),
            algorithm: "DES/3DES",
            status: CryptoStatus::Reject,
            message: "DES/3DES via Node.js crypto is broken.",
            suggestion: Some("Replace with XChaCha20-Poly1305 via @noble/ciphers"),
        },
        // RC4 — REJECT
        SymmetricPattern {
            regex: Regex::new(r#"(?i)\b(?:rc4|arcfour|arc4)\b"#).unwrap(),
            algorithm: "RC4",
            status: CryptoStatus::Reject,
            message: "RC4 is cryptographically broken — biases in keystream are well-documented.",
            suggestion: Some("Replace with XChaCha20-Poly1305 (256-bit)"),
        },
        // AES-ECB — REJECT
        SymmetricPattern {
            regex: Regex::new(r#"(?i)\b(?:aes[_-]?ecb|ECB[_-]?mode|mode\s*[:=]\s*['"]?ecb)\b"#).unwrap(),
            algorithm: "AES-ECB",
            status: CryptoStatus::Reject,
            message: "AES-ECB mode reveals patterns in ciphertext — the famous ECB penguin attack.",
            suggestion: Some("Replace with XChaCha20-Poly1305 or AES-GCM"),
        },
        SymmetricPattern {
            regex: Regex::new(r#"(?i)crypto\.createCipher(?:iv)?\s*\(\s*['"]aes-\d+-ecb['"]\s*"#).unwrap(),
            algorithm: "AES-ECB",
            status: CryptoStatus::Reject,
            message: "AES-ECB mode via Node.js is insecure.",
            suggestion: Some("Replace with AES-256-GCM or XChaCha20-Poly1305"),
        },
        // AES-CBC — WARN
        SymmetricPattern {
            regex: Regex::new(r#"(?i)\b(?:aes[_-]?\d*[_-]?cbc|CBC[_-]?mode|mode\s*[:=]\s*['"]?cbc)\b"#).unwrap(),
            algorithm: "AES-CBC",
            status: CryptoStatus::Warn,
            message: "AES-CBC does not provide authenticated encryption — vulnerable to padding oracle attacks.",
            suggestion: Some("Replace with XChaCha20-Poly1305 or AES-256-GCM (AEAD)"),
        },
        // AES-GCM 128-bit — WARN
        SymmetricPattern {
            regex: Regex::new(r#"(?i)\b(?:aes[_-]?128[_-]?gcm|Aes128Gcm)\b"#).unwrap(),
            algorithm: "AES-GCM-128",
            status: CryptoStatus::Warn,
            message: "AES-GCM with 128-bit key — prefer 256-bit for post-quantum safety margin.",
            suggestion: Some("Upgrade to AES-256-GCM or XChaCha20-Poly1305"),
        },
        // AES-GCM 256-bit — ACCEPT
        SymmetricPattern {
            regex: Regex::new(r#"(?i)\b(?:aes[_-]?256[_-]?gcm|Aes256Gcm)\b"#).unwrap(),
            algorithm: "AES-GCM-256",
            status: CryptoStatus::Accept,
            message: "AES-256-GCM is acceptable — strong AEAD with 256-bit key.",
            suggestion: Some("Consider XChaCha20-Poly1305 for larger nonce space"),
        },
        // XChaCha20-Poly1305 — PREFER
        SymmetricPattern {
            regex: Regex::new(r#"(?i)\b(?:xchacha20[_-]?poly1305|XChaCha20Poly1305)\b"#).unwrap(),
            algorithm: "XChaCha20-Poly1305",
            status: CryptoStatus::Prefer,
            message: "XChaCha20-Poly1305 — ideal AEAD cipher with 256-bit key and 192-bit nonce.",
            suggestion: None,
        },
        // ChaCha20-Poly1305 (regular, not extended nonce) — ACCEPT
        SymmetricPattern {
            regex: Regex::new(r#"(?i)\b(?:chacha20[_-]?poly1305|ChaCha20Poly1305)\b"#).unwrap(),
            algorithm: "ChaCha20-Poly1305",
            status: CryptoStatus::Accept,
            message: "ChaCha20-Poly1305 is strong but has a smaller nonce space than XChaCha20.",
            suggestion: Some("Consider XChaCha20-Poly1305 for 192-bit nonce space"),
        },
    ]
});

/// Analyzer for symmetric encryption algorithm usage.
pub struct SymmetricAnalyzer;

impl Analyzer for SymmetricAnalyzer {
    fn name(&self) -> &str {
        "Symmetric Encryption Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/deprecated"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.starts_with('#') || trimmed.starts_with("/*") {
                continue;
            }

            for pattern in SYMMETRIC_PATTERNS.iter() {
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
    fn test_detect_des() {
        let analyzer = SymmetricAnalyzer;
        let content = "let cipher = des::Des::new(&key);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect DES");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_rc4() {
        let analyzer = SymmetricAnalyzer;
        let content = "let stream = rc4::Rc4::new(&key);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect RC4");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_aes_ecb() {
        let analyzer = SymmetricAnalyzer;
        let content = r#"mode = "ecb""#;
        let usages = analyzer.analyze_content(Path::new("config.toml"), content);
        assert!(!usages.is_empty(), "Should detect AES-ECB mode");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_aes_cbc_warn() {
        let analyzer = SymmetricAnalyzer;
        let content = "let cipher = aes_256_cbc::encrypt(&key, &iv, &data);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect AES-CBC");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_xchacha20_prefer() {
        let analyzer = SymmetricAnalyzer;
        let content = "let cipher = XChaCha20Poly1305::new(&key);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect XChaCha20-Poly1305");
        assert_eq!(usages[0].status, CryptoStatus::Prefer);
    }

    #[test]
    fn test_clean_file() {
        let analyzer = SymmetricAnalyzer;
        let content = "fn main() { println!(\"hello\"); }";
        let usages = analyzer.analyze_content(Path::new("clean.rs"), content);
        assert!(usages.is_empty(), "Clean file should have no findings");
    }
}
