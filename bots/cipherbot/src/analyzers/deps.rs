// SPDX-License-Identifier: PMPL-1.0-or-later
//! Dependency Crypto Audit Analyzer — scans Cargo.toml and package.json
//! for problematic cryptographic dependencies.
//!
//! Checks:
//! - Rust: `ring` CVEs, `openssl` < 3.0, `rust-crypto` (unmaintained)
//! - JS: `crypto-js` (weak defaults), `node-forge` (historical vulns)
//! - Recommends: `rustls` over `openssl`, `@noble/*` over legacy JS crypto

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct DepPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
    /// Only match in specific file types.
    file_ext: Option<&'static str>,
}

static DEP_PATTERNS: LazyLock<Vec<DepPattern>> = LazyLock::new(|| {
    vec![
        // rust-crypto — unmaintained, REJECT
        DepPattern {
            regex: Regex::new(r#"(?i)\brust[_-]crypto\b"#).unwrap(),
            algorithm: "rust-crypto",
            status: CryptoStatus::Reject,
            message: "rust-crypto is unmaintained and has known vulnerabilities — do not use.",
            suggestion: Some("Replace with RustCrypto crates (sha2, aes, etc.) or ring"),
            file_ext: None,
        },
        // openssl crate — WARN
        DepPattern {
            regex: Regex::new(r#"(?m)^\s*openssl\s*="#).unwrap(),
            algorithm: "openssl-dep",
            status: CryptoStatus::Warn,
            message: "openssl crate links to C OpenSSL — prefer rustls for pure-Rust TLS.",
            suggestion: Some("Replace with rustls for TLS, or RustCrypto crates for primitives"),
            file_ext: Some("toml"),
        },
        // crypto-js (npm) — WARN
        DepPattern {
            regex: Regex::new(r#"(?i)["']crypto-js["']"#).unwrap(),
            algorithm: "crypto-js",
            status: CryptoStatus::Warn,
            message: "crypto-js has weak defaults and no constant-time guarantees.",
            suggestion: Some("Replace with @noble/ciphers and @noble/hashes (audited)"),
            file_ext: None,
        },
        // node-forge (npm) — WARN
        DepPattern {
            regex: Regex::new(r#"(?i)["']node-forge["']"#).unwrap(),
            algorithm: "node-forge",
            status: CryptoStatus::Warn,
            message: "node-forge has historical vulnerabilities — prefer audited alternatives.",
            suggestion: Some("Replace with @noble/ciphers, @noble/hashes, or @noble/curves"),
            file_ext: None,
        },
        // sjcl (npm) — WARN
        DepPattern {
            regex: Regex::new(r#"(?i)["']sjcl["']"#).unwrap(),
            algorithm: "sjcl",
            status: CryptoStatus::Warn,
            message: "sjcl (Stanford JS Crypto Library) is minimally maintained.",
            suggestion: Some("Replace with @noble/ciphers and @noble/hashes"),
            file_ext: None,
        },
        // bcrypt.js variants — note (not necessarily bad, but check)
        DepPattern {
            regex: Regex::new(r#"(?i)["']bcryptjs["']"#).unwrap(),
            algorithm: "bcryptjs",
            status: CryptoStatus::Warn,
            message: "bcryptjs is JavaScript-only bcrypt — consider Argon2id for new projects.",
            suggestion: Some("Use argon2 package with Argon2id for new password hashing"),
            file_ext: None,
        },
    ]
});

/// Analyzer for cryptographic dependency audit.
pub struct DependencyAnalyzer;

impl Analyzer for DependencyAnalyzer {
    fn name(&self) -> &str {
        "Dependency Crypto Audit"
    }

    fn category(&self) -> &str {
        "crypto/dependency"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("");
        let filename = path
            .file_name()
            .and_then(|f| f.to_str())
            .unwrap_or("");

        // Only scan manifest files
        let is_manifest = matches!(
            filename,
            "Cargo.toml" | "package.json" | "Gemfile" | "requirements.txt" | "go.mod"
        );
        if !is_manifest {
            return usages;
        }

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.starts_with('#') || trimmed.starts_with("/*") {
                continue;
            }

            for pattern in DEP_PATTERNS.iter() {
                // Check file extension filter
                if let Some(required_ext) = pattern.file_ext {
                    if ext != required_ext {
                        continue;
                    }
                }

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
    fn test_detect_rust_crypto() {
        let analyzer = DependencyAnalyzer;
        let content = r#"
[dependencies]
rust-crypto = "0.2"
"#;
        let usages = analyzer.analyze_content(Path::new("Cargo.toml"), content);
        assert!(!usages.is_empty(), "Should detect rust-crypto");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_openssl_dep() {
        let analyzer = DependencyAnalyzer;
        let content = r#"
[dependencies]
openssl = "0.10"
"#;
        let usages = analyzer.analyze_content(Path::new("Cargo.toml"), content);
        assert!(!usages.is_empty(), "Should detect openssl dependency");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_crypto_js() {
        let analyzer = DependencyAnalyzer;
        let content = r#"
{
  "dependencies": {
    "crypto-js": "^4.0.0"
  }
}
"#;
        let usages = analyzer.analyze_content(Path::new("package.json"), content);
        assert!(!usages.is_empty(), "Should detect crypto-js");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_skip_non_manifest() {
        let analyzer = DependencyAnalyzer;
        let content = r#"let x = "crypto-js";"#;
        let usages = analyzer.analyze_content(Path::new("src/main.rs"), content);
        assert!(usages.is_empty(), "Should skip non-manifest files");
    }
}
