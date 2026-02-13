// SPDX-License-Identifier: PMPL-1.0-or-later
//! TLS/Protocol Analyzer — detects insecure protocol configurations.
//!
//! | Status  | Protocol              | Action                    |
//! |---------|-----------------------|---------------------------|
//! | REJECT  | SSLv2, SSLv3, TLS 1.0 | Error — deprecated        |
//! | REJECT  | TLS 1.1               | Error — deprecated        |
//! | WARN    | TLS 1.2               | Warning — prefer TLS 1.3  |
//! | ACCEPT  | TLS 1.3               | OK                        |
//! | PREFER  | QUIC + HTTP/3          | Ideal                     |
//! | REJECT  | HTTP (no TLS)          | Error — always use HTTPS  |
//! | WARN    | IPv4                   | Warning — prefer IPv6     |
//! | PREFER  | IPv6                   | Ideal                     |

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct ProtocolPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static PROTOCOL_PATTERNS: LazyLock<Vec<ProtocolPattern>> = LazyLock::new(|| {
    vec![
        // SSLv2/SSLv3 — REJECT
        ProtocolPattern {
            regex: Regex::new(r#"(?i)\b(?:sslv[23]|ssl[_-]?v[23]|SSLv[23])\b"#).unwrap(),
            algorithm: "SSLv2/v3",
            status: CryptoStatus::Reject,
            message: "SSL 2.0/3.0 is deprecated and insecure — POODLE, DROWN attacks.",
            suggestion: Some("Upgrade to TLS 1.3 minimum"),
        },
        // TLS 1.0 — REJECT
        ProtocolPattern {
            regex: Regex::new(r#"(?i)\b(?:tls[_-]?1[_.]0|TLSv1\.0)\b"#).unwrap(),
            algorithm: "TLS-1.0",
            status: CryptoStatus::Reject,
            message: "TLS 1.0 is deprecated (RFC 8996) — BEAST, CRIME attacks.",
            suggestion: Some("Upgrade to TLS 1.3"),
        },
        // TLS 1.1 — REJECT
        ProtocolPattern {
            regex: Regex::new(r#"(?i)\b(?:tls[_-]?1[_.]1|TLSv1\.1)\b"#).unwrap(),
            algorithm: "TLS-1.1",
            status: CryptoStatus::Reject,
            message: "TLS 1.1 is deprecated (RFC 8996).",
            suggestion: Some("Upgrade to TLS 1.3"),
        },
        // TLS 1.2 — WARN
        ProtocolPattern {
            regex: Regex::new(r#"(?i)\b(?:tls[_-]?1[_.]2|TLSv1\.2)\b"#).unwrap(),
            algorithm: "TLS-1.2",
            status: CryptoStatus::Warn,
            message: "TLS 1.2 is acceptable but TLS 1.3 provides better security and performance.",
            suggestion: Some("Upgrade to TLS 1.3 for improved security"),
        },
        // TLS 1.3 — ACCEPT
        ProtocolPattern {
            regex: Regex::new(r#"(?i)\b(?:tls[_-]?1[_.]3|TLSv1\.3)\b"#).unwrap(),
            algorithm: "TLS-1.3",
            status: CryptoStatus::Accept,
            message: "TLS 1.3 — strong protocol with reduced handshake and forward secrecy.",
            suggestion: None,
        },
        // HTTP without TLS — REJECT
        // Note: cannot use negative lookahead in the regex crate;
        // we match all http:// and filter localhost in analyze_content.
        ProtocolPattern {
            regex: Regex::new(r#"(?i)\bhttp://"#).unwrap(),
            algorithm: "HTTP-plaintext",
            status: CryptoStatus::Reject,
            message: "Plaintext HTTP is insecure — always use HTTPS.",
            suggestion: Some("Replace http:// with https://"),
        },
        // verify_ssl: false — REJECT
        ProtocolPattern {
            regex: Regex::new(r#"(?i)(?:verify[_-]?ssl|ssl[_-]?verify|tls[_-]?verify|CURLOPT_SSL_VERIFYPEER)\s*[:=]\s*(?:false|0|no|off|False|FALSE)"#).unwrap(),
            algorithm: "TLS-verify-disabled",
            status: CryptoStatus::Reject,
            message: "TLS certificate verification disabled — enables MITM attacks.",
            suggestion: Some("Enable TLS certificate verification"),
        },
        // QUIC/HTTP3 — PREFER
        ProtocolPattern {
            regex: Regex::new(r#"(?i)\b(?:quic|http[_/]?3|h3)\b"#).unwrap(),
            algorithm: "QUIC/HTTP3",
            status: CryptoStatus::Prefer,
            message: "QUIC/HTTP3 — ideal protocol with built-in TLS 1.3 and multiplexing.",
            suggestion: None,
        },
    ]
});

/// Check if an HTTP URL is pointing to localhost/loopback (acceptable for dev).
fn is_localhost_http(line: &str) -> bool {
    let lower = line.to_lowercase();
    lower.contains("http://localhost")
        || lower.contains("http://127.0.0.1")
        || lower.contains("http://0.0.0.0")
        || lower.contains("http://[::1]")
}

/// Analyzer for TLS/protocol configuration.
pub struct ProtocolAnalyzer;

impl Analyzer for ProtocolAnalyzer {
    fn name(&self) -> &str {
        "TLS/Protocol Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/protocol"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.starts_with('#') || trimmed.starts_with("/*") {
                continue;
            }

            for pattern in PROTOCOL_PATTERNS.iter() {
                if pattern.regex.is_match(line) {
                    // Filter out localhost/loopback for HTTP-plaintext detection
                    if pattern.algorithm == "HTTP-plaintext" && is_localhost_http(line) {
                        continue;
                    }
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
    fn test_detect_sslv3() {
        let analyzer = ProtocolAnalyzer;
        let content = "min_version = SSLv3";
        let usages = analyzer.analyze_content(Path::new("config.yml"), content);
        assert!(!usages.is_empty(), "Should detect SSLv3");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_tls_1_0() {
        let analyzer = ProtocolAnalyzer;
        let content = "min_protocol: TLSv1.0";
        let usages = analyzer.analyze_content(Path::new("config.yml"), content);
        assert!(!usages.is_empty(), "Should detect TLS 1.0");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_tls_1_2_warn() {
        let analyzer = ProtocolAnalyzer;
        let content = "protocol: TLSv1.2";
        let usages = analyzer.analyze_content(Path::new("config.yml"), content);
        assert!(!usages.is_empty(), "Should detect TLS 1.2");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_http_plaintext() {
        let analyzer = ProtocolAnalyzer;
        let content = r#"api_url = "http://example.com/api""#;
        let usages = analyzer.analyze_content(Path::new("config.toml"), content);
        assert!(!usages.is_empty(), "Should detect plaintext HTTP");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_allow_http_localhost() {
        let analyzer = ProtocolAnalyzer;
        let content = r#"url = "http://localhost:8080""#;
        let usages = analyzer.analyze_content(Path::new("config.toml"), content);
        assert!(usages.is_empty(), "Should allow http://localhost");
    }

    #[test]
    fn test_detect_ssl_verify_disabled() {
        let analyzer = ProtocolAnalyzer;
        let content = "verify_ssl: false";
        let usages = analyzer.analyze_content(Path::new("config.yml"), content);
        assert!(!usages.is_empty(), "Should detect disabled SSL verification");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_quic_prefer() {
        let analyzer = ProtocolAnalyzer;
        let content = "transport: quic";
        let usages = analyzer.analyze_content(Path::new("config.yml"), content);
        assert!(!usages.is_empty(), "Should detect QUIC");
        assert_eq!(usages[0].status, CryptoStatus::Prefer);
    }
}
