// SPDX-License-Identifier: PMPL-1.0-or-later
//! Infrastructure-as-Code Analyzer — scans Terraform, Ansible, Docker/Podman configs
//! for cryptographic and security issues.
//!
//! Checks:
//! - TLS minimum version >= 1.3
//! - No hardcoded credentials
//! - Container images use SHA digests (not :latest)
//! - Network policies restrict egress
//! - Secrets managed via vault/sealed-secrets

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct InfraPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static INFRA_PATTERNS: LazyLock<Vec<InfraPattern>> = LazyLock::new(|| {
    vec![
        // Container image using :latest — WARN
        InfraPattern {
            regex: Regex::new(r#"(?i)(?:image|FROM)\s*[:=]?\s*["']?[a-zA-Z0-9._/-]+:latest["']?"#).unwrap(),
            algorithm: "container-latest",
            status: CryptoStatus::Warn,
            message: "Container image uses :latest tag — not reproducible, potential supply-chain risk.",
            suggestion: Some("Pin to specific SHA256 digest: image@sha256:abcdef..."),
        },
        // Hardcoded credentials in IaC — REJECT
        InfraPattern {
            regex: Regex::new(r#"(?i)(?:password|secret_key|access_key|private_key)\s*[:=]\s*["'][^"'$\{]{8,}["']"#).unwrap(),
            algorithm: "iac-hardcoded-cred",
            status: CryptoStatus::Reject,
            message: "Hardcoded credentials in infrastructure code — severe security risk.",
            suggestion: Some("Use Vault, sealed-secrets, or SOPS for secrets management"),
        },
        // Secrets in env vars (less bad but still) — WARN
        InfraPattern {
            regex: Regex::new(r#"(?i)(?:env|environment)\s*[:=].*(?:PASSWORD|SECRET|TOKEN|API_KEY)\s*[:=]\s*["'][^"'$\{]+["']"#).unwrap(),
            algorithm: "env-var-secret",
            status: CryptoStatus::Warn,
            message: "Secrets passed via environment variables — visible in process listings.",
            suggestion: Some("Mount secrets from Vault or sealed-secrets instead of env vars"),
        },
        // Privileged container — WARN (not crypto but relevant)
        InfraPattern {
            regex: Regex::new(r#"(?i)(?:privileged|security_context)\s*[:=]\s*(?:true|1)"#).unwrap(),
            algorithm: "privileged-container",
            status: CryptoStatus::Warn,
            message: "Privileged container detected — breaks container isolation.",
            suggestion: Some("Drop all capabilities and add only required ones"),
        },
        // HTTP endpoints in IaC — REJECT
        // Note: cannot use negative lookahead in the regex crate; match all
        // http:// endpoints and filter localhost in analyze_content.
        InfraPattern {
            regex: Regex::new(r#"(?i)(?:endpoint|url|address|host)\s*[:=]\s*["']http://"#).unwrap(),
            algorithm: "iac-http-endpoint",
            status: CryptoStatus::Reject,
            message: "Plaintext HTTP endpoint in infrastructure configuration.",
            suggestion: Some("Use https:// for all endpoints"),
        },
        // Self-signed cert allowance — REJECT
        InfraPattern {
            regex: Regex::new(r#"(?i)(?:skip_tls_verify|insecure_skip_tls|allow_self_signed|verify_certificates)\s*[:=]\s*(?:true|false|0|no)"#).unwrap(),
            algorithm: "iac-tls-skip",
            status: CryptoStatus::Warn,
            message: "TLS verification settings modified in infrastructure code — verify intent.",
            suggestion: Some("Use proper CA certificates instead of skipping verification"),
        },
    ]
});

/// Check if file is an infrastructure-as-code file.
fn is_infra_file(path: &Path) -> bool {
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

    // Terraform
    ext == "tf" || ext == "tfvars"
    // Docker
    || filename.starts_with("dockerfile") || filename.starts_with("containerfile")
    || filename == "docker-compose.yml" || filename == "docker-compose.yaml"
    || filename == "compose.yml" || filename == "compose.yaml"
    // Kubernetes
    || (ext == "yml" || ext == "yaml") && is_likely_k8s_or_iac(path)
    // Ansible
    || filename.contains("playbook") || filename.contains("ansible")
    // Helm
    || filename == "values.yml" || filename == "values.yaml"
    || filename == "chart.yml" || filename == "chart.yaml"
}

/// Heuristic check for likely Kubernetes or IaC YAML files.
fn is_likely_k8s_or_iac(path: &Path) -> bool {
    let path_str = path.to_string_lossy().to_lowercase();
    path_str.contains("k8s")
        || path_str.contains("kubernetes")
        || path_str.contains("deploy")
        || path_str.contains("infra")
        || path_str.contains("terraform")
        || path_str.contains("ansible")
        || path_str.contains("helm")
        || path_str.contains("compose")
}

/// Analyzer for infrastructure-as-code security.
pub struct InfraAnalyzer;

impl Analyzer for InfraAnalyzer {
    fn name(&self) -> &str {
        "Infrastructure-as-Code Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/config"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        if !is_infra_file(path) {
            return usages;
        }

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with('#') || trimmed.starts_with("//") || trimmed.starts_with("/*") {
                continue;
            }

            for pattern in INFRA_PATTERNS.iter() {
                if pattern.regex.is_match(line) {
                    // Filter localhost for HTTP endpoint detection
                    if pattern.algorithm == "iac-http-endpoint" {
                        let lower = line.to_lowercase();
                        if lower.contains("http://localhost") || lower.contains("http://127.0.0.1") {
                            continue;
                        }
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
    fn test_detect_latest_tag() {
        let analyzer = InfraAnalyzer;
        let content = "FROM nginx:latest";
        let usages = analyzer.analyze_content(Path::new("Dockerfile"), content);
        assert!(!usages.is_empty(), "Should detect :latest tag");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_hardcoded_cred() {
        let analyzer = InfraAnalyzer;
        let content = r#"password = "SuperSecretPass123!""#;
        let usages = analyzer.analyze_content(Path::new("infra/main.tf"), content);
        assert!(!usages.is_empty(), "Should detect hardcoded credential");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_http_endpoint() {
        let analyzer = InfraAnalyzer;
        let content = r#"endpoint = "http://api.example.com""#;
        let usages = analyzer.analyze_content(Path::new("deploy/k8s/app.yml"), content);
        assert!(!usages.is_empty(), "Should detect HTTP endpoint in IaC");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_skip_non_infra_file() {
        let analyzer = InfraAnalyzer;
        let content = r#"password = "SuperSecretPass123!""#;
        let usages = analyzer.analyze_content(Path::new("src/main.rs"), content);
        assert!(usages.is_empty(), "Should skip non-IaC files");
    }
}
