// SPDX-License-Identifier: PMPL-1.0-or-later
//! Password Hashing Analyzer — detects weak password hashing algorithms.
//!
//! | Status  | Algorithm                          | Action                           |
//! |---------|------------------------------------|----------------------------------|
//! | REJECT  | MD5 (plaintext)                    | Error — catastrophic             |
//! | REJECT  | SHA-1/SHA-256 (unsalted)           | Error — rainbow tables           |
//! | WARN    | bcrypt                             | Warning — limited to 72 bytes    |
//! | WARN    | scrypt                             | Warning — Argon2id preferred     |
//! | ACCEPT  | Argon2id (default params)          | OK                               |
//! | PREFER  | Argon2id (512 MiB, 8 iter, 4 lanes)| Ideal — max GPU/ASIC resistance  |

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct PasswordPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static PASSWORD_PATTERNS: LazyLock<Vec<PasswordPattern>> = LazyLock::new(|| {
    vec![
        // MD5 for passwords — REJECT
        PasswordPattern {
            regex: Regex::new(r#"(?i)\b(?:md5[_-]?(?:hash|password|pass|pwd|crypt)\w*|password[_-]?md5)\b"#).unwrap(),
            algorithm: "MD5-password",
            status: CryptoStatus::Reject,
            message: "MD5 for password hashing is catastrophic — no stretching, trivially reversible with rainbow tables.",
            suggestion: Some("Replace with Argon2id (512 MiB memory, 8 iterations, 4 lanes)"),
        },
        // SHA-1/SHA-256 unsalted passwords — REJECT
        PasswordPattern {
            regex: Regex::new(r#"(?i)\b(?:sha[_-]?(?:1|256)[_-]?(?:hash|password|pass|pwd|crypt)|password[_-]?sha)"#).unwrap(),
            algorithm: "SHA-password",
            status: CryptoStatus::Reject,
            message: "SHA-1/SHA-256 for password hashing is insecure — no key stretching, vulnerable to rainbow tables.",
            suggestion: Some("Replace with Argon2id (512 MiB memory, 8 iterations, 4 lanes)"),
        },
        // bcrypt — WARN
        PasswordPattern {
            regex: Regex::new(r#"(?i)\b(?:bcrypt(?:::|\.|_|\s*\()|BCrypt|password_hash.*bcrypt)\b"#).unwrap(),
            algorithm: "bcrypt",
            status: CryptoStatus::Warn,
            message: "bcrypt is limited to 72 bytes input — truncates longer passwords. GPU-crackable at 4GB scale.",
            suggestion: Some("Migrate to Argon2id (512 MiB memory, 8 iterations, 4 lanes)"),
        },
        // scrypt — WARN
        PasswordPattern {
            regex: Regex::new(r#"(?i)\b(?:scrypt(?:::|\.|_|\s*\()|Scrypt)\b"#).unwrap(),
            algorithm: "scrypt",
            status: CryptoStatus::Warn,
            message: "scrypt is acceptable but Argon2id is preferred — better GPU/ASIC resistance with tunable parameters.",
            suggestion: Some("Migrate to Argon2id (512 MiB memory, 8 iterations, 4 lanes)"),
        },
        // Argon2id — ACCEPT/PREFER
        PasswordPattern {
            regex: Regex::new(r#"(?i)\b(?:argon2id|Argon2id|argon2[_-]id)\b"#).unwrap(),
            algorithm: "Argon2id",
            status: CryptoStatus::Accept,
            message: "Argon2id detected — verify parameters: 512 MiB memory, 8 iterations, 4 lanes for ideal security.",
            suggestion: Some("Verify: m=524288 (512 MiB), t=8, p=4 for maximum GPU/ASIC resistance"),
        },
        // Argon2i (data-independent) — ACCEPT but note
        // Note: we cannot use negative lookahead in the regex crate,
        // so we match argon2i and filter in analyze_content if argon2id is also present.
        PasswordPattern {
            regex: Regex::new(r#"(?i)\bargon2i\b"#).unwrap(),
            algorithm: "Argon2i",
            status: CryptoStatus::Accept,
            message: "Argon2i is data-independent — use Argon2id instead for combined side-channel and GPU resistance.",
            suggestion: Some("Switch to Argon2id for better combined protection"),
        },
        // PBKDF2 — WARN
        PasswordPattern {
            regex: Regex::new(r#"(?i)\b(?:pbkdf2|PBKDF2)\b"#).unwrap(),
            algorithm: "PBKDF2",
            status: CryptoStatus::Warn,
            message: "PBKDF2 is outdated for password hashing — GPU-parallelizable, no memory-hardness.",
            suggestion: Some("Migrate to Argon2id (512 MiB memory, 8 iterations, 4 lanes)"),
        },
    ]
});

/// Analyzer for password hashing algorithm usage.
pub struct PasswordAnalyzer;

impl Analyzer for PasswordAnalyzer {
    fn name(&self) -> &str {
        "Password Hashing Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/weak"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.starts_with('#') || trimmed.starts_with("/*") {
                continue;
            }

            for pattern in PASSWORD_PATTERNS.iter() {
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
    fn test_detect_md5_password() {
        let analyzer = PasswordAnalyzer;
        let content = "let hash = md5_hash_password(password);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect MD5 password hashing");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_bcrypt_warn() {
        let analyzer = PasswordAnalyzer;
        let content = "let hash = bcrypt::hash(password, cost);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect bcrypt");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_scrypt_warn() {
        let analyzer = PasswordAnalyzer;
        let content = "let key = scrypt::scrypt(&password, &salt, &params, &mut output);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect scrypt");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_argon2id_accept() {
        let analyzer = PasswordAnalyzer;
        let content = "let hash = Argon2id::hash_password(password, &salt);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect Argon2id");
        assert_eq!(usages[0].status, CryptoStatus::Accept);
    }

    #[test]
    fn test_detect_pbkdf2_warn() {
        let analyzer = PasswordAnalyzer;
        let content = "let key = PBKDF2::derive(&password, &salt, iterations);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect PBKDF2");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }
}
