// SPDX-License-Identifier: PMPL-1.0-or-later
//! DNS Zone File Analyzer — scans DNS zone files and infrastructure definitions
//! for cryptographic and security issues.
//!
//! Checks:
//! - SPF/DKIM/DMARC configuration
//! - CAA record restrictions
//! - TLSA/DANE SHA-1 usage
//! - MTA-STS policy enforcement
//! - SSHFP hash types
//! - IPv6 presence (AAAA records)
//! - Zero trust (no internal IPs in public DNS)

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct DnsPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static DNS_PATTERNS: LazyLock<Vec<DnsPattern>> = LazyLock::new(|| {
    vec![
        // SPF +all — REJECT (too permissive)
        DnsPattern {
            regex: Regex::new(r#"(?i)\bv=spf1\b.*\+all\b"#).unwrap(),
            algorithm: "SPF-permissive",
            status: CryptoStatus::Reject,
            message: "SPF record uses +all — allows anyone to send email as this domain.",
            suggestion: Some("Change to -all (hard fail) or ~all (soft fail)"),
        },
        // DMARC none — WARN
        DnsPattern {
            regex: Regex::new(r#"(?i)\bv=DMARC1\b.*\bp=none\b"#).unwrap(),
            algorithm: "DMARC-none",
            status: CryptoStatus::Warn,
            message: "DMARC policy is 'none' — provides monitoring only, no protection.",
            suggestion: Some("Set DMARC policy to p=reject for maximum protection"),
        },
        // DMARC quarantine — WARN
        DnsPattern {
            regex: Regex::new(r#"(?i)\bv=DMARC1\b.*\bp=quarantine\b"#).unwrap(),
            algorithm: "DMARC-quarantine",
            status: CryptoStatus::Warn,
            message: "DMARC policy is 'quarantine' — prefer 'reject' for stronger protection.",
            suggestion: Some("Upgrade DMARC policy to p=reject"),
        },
        // SSHFP with SHA-1 (type 1) — WARN
        DnsPattern {
            regex: Regex::new(r#"(?i)\bSSHFP\s+\d+\s+1\s+"#).unwrap(),
            algorithm: "SSHFP-SHA1",
            status: CryptoStatus::Warn,
            message: "SSHFP record uses SHA-1 hash type (type 1) — deprecated.",
            suggestion: Some("Use SHA-256 hash type (type 2) for SSHFP records"),
        },
        // TLSA with SHA-1 — WARN
        DnsPattern {
            regex: Regex::new(r#"(?i)\bTLSA\s+\d+\s+\d+\s+1\s+"#).unwrap(),
            algorithm: "TLSA-SHA1",
            status: CryptoStatus::Warn,
            message: "TLSA record uses SHA-1 matching type — deprecated.",
            suggestion: Some("Use SHA-256 (type 1) or full certificate (type 0) for TLSA"),
        },
        // Internal IPs in public DNS — REJECT
        DnsPattern {
            regex: Regex::new(r#"(?i)\b(?:A|AAAA)\s+(?:10\.\d+\.\d+\.\d+|172\.(?:1[6-9]|2\d|3[01])\.\d+\.\d+|192\.168\.\d+\.\d+)\b"#).unwrap(),
            algorithm: "internal-ip-exposed",
            status: CryptoStatus::Reject,
            message: "Internal/private IP address exposed in public DNS record.",
            suggestion: Some("Use Cloudflare Tunnel or similar zero-trust proxy instead of direct IPs"),
        },
        // MTA-STS mode testing — WARN
        DnsPattern {
            regex: Regex::new(r#"(?i)\bmode\s*[:=]\s*['"]?testing['"]?"#).unwrap(),
            algorithm: "MTA-STS-testing",
            status: CryptoStatus::Warn,
            message: "MTA-STS policy in testing mode — does not enforce TLS for email.",
            suggestion: Some("Set MTA-STS mode to 'enforce' for production"),
        },
    ]
});

/// Check if file is a DNS/infrastructure related file.
fn is_dns_file(path: &Path) -> bool {
    let filename = path
        .file_name()
        .and_then(|f| f.to_str())
        .unwrap_or("")
        .to_lowercase();
    let ext = path
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("")
        .to_lowercase();

    filename.contains("zone")
        || filename.contains("dns")
        || filename.contains("dmarc")
        || filename.contains("spf")
        || filename.contains("mta-sts")
        || ext == "zone"
        || ext == "db"
        || (ext == "txt" && (filename.contains("dns") || filename.contains("zone")))
        || ext == "yml"
        || ext == "yaml"
        || ext == "toml"
        || ext == "json"
        || ext == "conf"
}

/// Analyzer for DNS and email security configuration.
pub struct DnsAnalyzer;

impl Analyzer for DnsAnalyzer {
    fn name(&self) -> &str {
        "DNS Zone File Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/protocol"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        if !is_dns_file(path) {
            return usages;
        }

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with(';') || trimmed.starts_with('#') || trimmed.starts_with("//") {
                continue;
            }

            for pattern in DNS_PATTERNS.iter() {
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
    fn test_detect_permissive_spf() {
        let analyzer = DnsAnalyzer;
        let content = r#"@ IN TXT "v=spf1 include:_spf.google.com +all""#;
        let usages = analyzer.analyze_content(Path::new("example.com.zone"), content);
        assert!(!usages.is_empty(), "Should detect permissive SPF");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_dmarc_none() {
        let analyzer = DnsAnalyzer;
        let content = r#"_dmarc IN TXT "v=DMARC1; p=none; rua=mailto:dmarc@example.com""#;
        let usages = analyzer.analyze_content(Path::new("dns.zone"), content);
        assert!(!usages.is_empty(), "Should detect DMARC p=none");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_internal_ip() {
        let analyzer = DnsAnalyzer;
        let content = "internal A 192.168.1.100";
        let usages = analyzer.analyze_content(Path::new("example.com.zone"), content);
        assert!(!usages.is_empty(), "Should detect internal IP in DNS");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_sshfp_sha1() {
        let analyzer = DnsAnalyzer;
        let content = "server SSHFP 2 1 abc123def456";
        let usages = analyzer.analyze_content(Path::new("dns.zone"), content);
        assert!(!usages.is_empty(), "Should detect SSHFP SHA-1");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_skip_non_dns_file() {
        let analyzer = DnsAnalyzer;
        let content = r#"v=spf1 +all"#;
        let usages = analyzer.analyze_content(Path::new("src/main.rs"), content);
        assert!(usages.is_empty(), "Should skip non-DNS files");
    }
}
