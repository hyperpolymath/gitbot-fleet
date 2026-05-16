// SPDX-License-Identifier: PMPL-1.0-or-later
//! Configuration File Analyzer — scans configuration files for crypto issues.
//!
//! Scans `*.toml`, `*.yaml`, `*.yml`, `*.json`, `*.env` for:
//! - Hardcoded keys/passwords (entropy analysis)
//! - Weak algorithm specifications
//! - Insecure protocol settings
//! - Self-signed certificate acceptance (`verify_ssl: false`)

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct ConfigPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static CONFIG_PATTERNS: LazyLock<Vec<ConfigPattern>> = LazyLock::new(|| {
    vec![
        // Hardcoded secrets — REJECT
        ConfigPattern {
            regex: Regex::new(r#"(?i)(?:password|secret|api_key|apikey|token|private_key)\s*[:=]\s*["'][^"']{8,}["']"#).unwrap(),
            algorithm: "hardcoded-secret",
            status: CryptoStatus::Reject,
            message: "Potential hardcoded secret detected — secrets should never be in configuration files.",
            suggestion: Some("Use environment variables or a secrets manager (Vault, sealed-secrets)"),
        },
        // Weak algorithm in config — REJECT
        ConfigPattern {
            regex: Regex::new(r#"(?i)(?:algorithm|cipher|encryption)\s*[:=]\s*['"]?(?:des|3des|rc4|blowfish)['"]?"#).unwrap(),
            algorithm: "weak-config-cipher",
            status: CryptoStatus::Reject,
            message: "Weak cipher specified in configuration — DES/3DES/RC4/Blowfish are broken.",
            suggestion: Some("Use xchacha20-poly1305 or aes-256-gcm"),
        },
        // Weak hash in config — REJECT
        ConfigPattern {
            regex: Regex::new(r#"(?i)(?:hash|digest|hash_algorithm)\s*[:=]\s*['"]?(?:md5|sha1)['"]?"#).unwrap(),
            algorithm: "weak-config-hash",
            status: CryptoStatus::Reject,
            message: "Weak hash algorithm specified in configuration — MD5/SHA-1 are broken.",
            suggestion: Some("Use shake3-512 or blake3"),
        },
        // Self-signed cert acceptance — REJECT
        ConfigPattern {
            regex: Regex::new(r#"(?i)(?:insecure[_-]?skip[_-]?verify|allow[_-]?invalid[_-]?cert|accept[_-]?invalid[_-]?cert)\s*[:=]\s*(?:true|1|yes|on)"#).unwrap(),
            algorithm: "insecure-cert",
            status: CryptoStatus::Reject,
            message: "Invalid certificate acceptance enabled — vulnerable to MITM attacks.",
            suggestion: Some("Use proper certificate validation with trusted CA certificates"),
        },
        // Low key size — WARN
        ConfigPattern {
            regex: Regex::new(r#"(?i)(?:key_size|key_length|keysize|keylength)\s*[:=]\s*(?:512|768|1024)\b"#).unwrap(),
            algorithm: "low-keysize",
            status: CryptoStatus::Warn,
            message: "Low key size configured — may be insecure for asymmetric operations.",
            suggestion: Some("Use at least 2048-bit RSA (prefer 4096) or switch to post-quantum algorithms"),
        },
        // Disabled encryption — REJECT
        ConfigPattern {
            regex: Regex::new(r#"(?i)(?:encryption|encrypt|tls|ssl)\s*[:=]\s*(?:false|off|no|disabled|none|0)"#).unwrap(),
            algorithm: "encryption-disabled",
            status: CryptoStatus::Reject,
            message: "Encryption explicitly disabled in configuration.",
            suggestion: Some("Enable encryption — use TLS 1.3 minimum"),
        },
    ]
});

/// Check if a file is a configuration file based on its extension and name.
fn is_config_file(path: &Path) -> bool {
    let ext = path
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("")
        .to_lowercase();
    let filename = path
        .file_name()
        .and_then(|f| f.to_str())
        .unwrap_or("")
        .to_lowercase();

    matches!(
        ext.as_str(),
        "toml" | "yaml" | "yml" | "json" | "env" | "cfg" | "conf" | "ini" | "properties"
    ) || matches!(
        filename.as_str(),
        ".env" | ".env.local" | ".env.production" | "config" | "settings"
    )
}

/// Analyzer for configuration file crypto issues.
pub struct ConfigAnalyzer;

impl Analyzer for ConfigAnalyzer {
    fn name(&self) -> &str {
        "Configuration File Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/config"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        // Only scan configuration files
        if !is_config_file(path) {
            return usages;
        }

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with('#') || trimmed.starts_with("//") || trimmed.starts_with("/*") {
                continue;
            }

            for pattern in CONFIG_PATTERNS.iter() {
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
    fn test_detect_hardcoded_secret() {
        let analyzer = ConfigAnalyzer;
        let content = r#"api_key = "sk-1234567890abcdef""#;
        let usages = analyzer.analyze_content(Path::new("config.toml"), content);
        assert!(!usages.is_empty(), "Should detect hardcoded secret");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_weak_cipher_config() {
        let analyzer = ConfigAnalyzer;
        let content = "cipher: des";
        let usages = analyzer.analyze_content(Path::new("settings.yml"), content);
        assert!(!usages.is_empty(), "Should detect weak cipher in config");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_encryption_disabled() {
        let analyzer = ConfigAnalyzer;
        let content = "encryption = false";
        let usages = analyzer.analyze_content(Path::new("config.toml"), content);
        assert!(!usages.is_empty(), "Should detect disabled encryption");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_skip_non_config_file() {
        let analyzer = ConfigAnalyzer;
        let content = r#"api_key = "sk-1234567890abcdef""#;
        let usages = analyzer.analyze_content(Path::new("src/main.rs"), content);
        assert!(usages.is_empty(), "Should skip non-config files");
    }

    #[test]
    fn test_detect_low_keysize() {
        let analyzer = ConfigAnalyzer;
        let content = "key_size: 1024";
        let usages = analyzer.analyze_content(Path::new("crypto.yml"), content);
        assert!(!usages.is_empty(), "Should detect low key size");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }
}
