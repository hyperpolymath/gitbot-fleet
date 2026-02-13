// SPDX-License-Identifier: PMPL-1.0-or-later

//! RSR (Rhodium Standard Repository) compliance checking module
//!
//! Supports policy packs and opt-in severity levels for flexible compliance.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::config::Config;
use crate::github::GitHubClient;

/// Severity levels for compliance checks
#[derive(Debug, Default, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    /// Must pass for RSR compliance
    Required,
    /// Should pass, counts toward score but doesn't block
    #[default]
    Recommended,
    /// Nice to have, informational only
    Optional,
}

/// Policy pack identifiers
#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Default)]
#[serde(rename_all = "lowercase")]
pub enum PolicyPack {
    /// Minimal requirements for small/experimental repos
    Minimal,
    /// Standard requirements for most repos
    #[default]
    Standard,
    /// Strict requirements for production repos
    Strict,
    /// Enterprise requirements with full compliance
    Enterprise,
    /// Custom policy defined in repo config
    Custom,
}

impl std::fmt::Display for PolicyPack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minimal => write!(f, "minimal"),
            Self::Standard => write!(f, "standard"),
            Self::Strict => write!(f, "strict"),
            Self::Enterprise => write!(f, "enterprise"),
            Self::Custom => write!(f, "custom"),
        }
    }
}

/// Repository-specific RSR configuration (from .rsr.toml)
#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct RepoConfig {
    /// Policy pack to use
    #[serde(default)]
    pub policy: PolicyPack,
    /// Override severity for specific checks
    #[serde(default)]
    pub severity_overrides: HashMap<String, Severity>,
    /// Checks to skip entirely
    #[serde(default)]
    pub skip: Vec<String>,
    /// Additional required files beyond the policy
    #[serde(default)]
    pub require: Vec<String>,
    /// Custom banned patterns
    #[serde(default)]
    pub ban: Vec<String>,
}

/// Check definition with severity per policy pack
#[derive(Debug, Clone)]
pub struct CheckDef {
    pub name: &'static str,
    pub description: &'static str,
    pub category: CheckCategory,
    pub points: u8,
    /// Severity by policy pack: (minimal, standard, strict, enterprise)
    pub severity: (Severity, Severity, Severity, Severity),
}

impl CheckDef {
    pub fn severity_for(&self, policy: PolicyPack) -> Severity {
        match policy {
            PolicyPack::Minimal => self.severity.0,
            PolicyPack::Standard => self.severity.1,
            PolicyPack::Strict => self.severity.2,
            PolicyPack::Enterprise => self.severity.3,
            PolicyPack::Custom => self.severity.1, // Default to standard for custom
        }
    }
}

/// Required files for RSR compliance with policy-based severity
pub const REQUIRED_FILES: &[CheckDef] = &[
    CheckDef {
        name: "README.adoc",
        description: "AsciiDoc README",
        category: CheckCategory::Documentation,
        points: 5,
        // (minimal, standard, strict, enterprise)
        severity: (Severity::Required, Severity::Required, Severity::Required, Severity::Required),
    },
    CheckDef {
        name: "LICENSE.txt",
        description: "License file",
        category: CheckCategory::Governance,
        points: 5,
        severity: (Severity::Required, Severity::Required, Severity::Required, Severity::Required),
    },
    CheckDef {
        name: "SECURITY.md",
        description: "Security policy",
        category: CheckCategory::Security,
        points: 5,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    CheckDef {
        name: "CONTRIBUTING.md",
        description: "Contributing guidelines",
        category: CheckCategory::Documentation,
        points: 3,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    CheckDef {
        name: "CODE_OF_CONDUCT.md",
        description: "Code of conduct",
        category: CheckCategory::Governance,
        points: 3,
        severity: (Severity::Optional, Severity::Optional, Severity::Recommended, Severity::Required),
    },
    CheckDef {
        name: ".claude/CLAUDE.md",
        description: "AI assistant instructions",
        category: CheckCategory::Structure,
        points: 2,
        severity: (Severity::Optional, Severity::Optional, Severity::Recommended, Severity::Required),
    },
    CheckDef {
        name: "STATE.scm",
        description: "Project state file",
        category: CheckCategory::Structure,
        points: 3,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    CheckDef {
        name: "META.scm",
        description: "Meta information",
        category: CheckCategory::Structure,
        points: 3,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    CheckDef {
        name: "ECOSYSTEM.scm",
        description: "Ecosystem position",
        category: CheckCategory::Structure,
        points: 3,
        severity: (Severity::Optional, Severity::Optional, Severity::Recommended, Severity::Required),
    },
    // Additional RSR template files
    CheckDef {
        name: ".editorconfig",
        description: "EditorConfig for consistent formatting",
        category: CheckCategory::Structure,
        points: 2,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    CheckDef {
        name: ".gitattributes",
        description: "Git attributes for line endings and diffs",
        category: CheckCategory::Structure,
        points: 2,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    CheckDef {
        name: ".gitignore",
        description: "Git ignore patterns",
        category: CheckCategory::Structure,
        points: 2,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    CheckDef {
        name: "justfile",
        description: "Just task runner (primary build system)",
        category: CheckCategory::Structure,
        points: 2,
        severity: (Severity::Optional, Severity::Optional, Severity::Recommended, Severity::Required),
    },
    CheckDef {
        name: ".bot_directives",
        description: "Bot directives directory",
        category: CheckCategory::Structure,
        points: 2,
        severity: (Severity::Optional, Severity::Optional, Severity::Recommended, Severity::Required),
    },
];

/// Banned patterns definition
#[derive(Debug, Clone)]
pub struct BannedPattern {
    pub name: &'static str,
    pub description: &'static str,
    pub category: CheckCategory,
    /// Severity by policy pack: (minimal, standard, strict, enterprise)
    pub severity: (Severity, Severity, Severity, Severity),
}

impl BannedPattern {
    pub fn severity_for(&self, policy: PolicyPack) -> Severity {
        match policy {
            PolicyPack::Minimal => self.severity.0,
            PolicyPack::Standard => self.severity.1,
            PolicyPack::Strict => self.severity.2,
            PolicyPack::Enterprise => self.severity.3,
            PolicyPack::Custom => self.severity.1,
        }
    }
}

/// Banned file patterns (CCCP language policy)
pub const BANNED_PATTERNS: &[BannedPattern] = &[
    BannedPattern {
        name: "package-lock.json",
        description: "npm lock file (use Deno)",
        category: CheckCategory::LanguagePolicy,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    BannedPattern {
        name: "yarn.lock",
        description: "Yarn lock file (use Deno)",
        category: CheckCategory::LanguagePolicy,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    BannedPattern {
        name: "pnpm-lock.yaml",
        description: "pnpm lock file (use Deno)",
        category: CheckCategory::LanguagePolicy,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    BannedPattern {
        name: "bun.lockb",
        description: "Bun lock file (use Deno)",
        category: CheckCategory::LanguagePolicy,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    BannedPattern {
        name: "go.mod",
        description: "Go module (use Rust)",
        category: CheckCategory::LanguagePolicy,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
    BannedPattern {
        name: "go.sum",
        description: "Go checksum (use Rust)",
        category: CheckCategory::LanguagePolicy,
        severity: (Severity::Optional, Severity::Recommended, Severity::Required, Severity::Required),
    },
];

/// RSR Compliance Report
#[derive(Debug, Serialize)]
pub struct ComplianceReport {
    pub owner: String,
    pub repo: String,
    pub policy: PolicyPack,
    pub score: u8,
    pub max_score: u8,
    pub percentage: f32,
    pub required_passed: bool,
    pub checks: Vec<Check>,
    pub summary: String,
}

/// Individual compliance check
#[derive(Debug, Serialize)]
pub struct Check {
    pub name: String,
    pub category: CheckCategory,
    pub severity: Severity,
    pub status: CheckStatus,
    pub points: u8,
    pub max_points: u8,
    pub message: String,
}

/// Check categories
#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq)]
pub enum CheckCategory {
    #[serde(rename = "documentation")]
    Documentation,
    #[serde(rename = "security")]
    Security,
    #[serde(rename = "governance")]
    Governance,
    #[serde(rename = "structure")]
    Structure,
    #[serde(rename = "language_policy")]
    LanguagePolicy,
}

/// Check status
#[derive(Debug, Serialize, Clone, Copy, PartialEq, Eq)]
pub enum CheckStatus {
    #[serde(rename = "pass")]
    Pass,
    #[serde(rename = "fail")]
    Fail,
    #[serde(rename = "warn")]
    Warn,
    #[serde(rename = "skip")]
    Skip,
}

/// Load repository configuration from .rsr.toml
pub async fn load_repo_config(client: &GitHubClient, owner: &str, repo: &str) -> RepoConfig {
    match client.get_file_content(owner, repo, ".rsr.toml").await {
        Ok(content) => {
            toml::from_str(&content).unwrap_or_else(|e| {
                tracing::warn!("Failed to parse .rsr.toml: {}", e);
                RepoConfig::default()
            })
        }
        Err(_) => RepoConfig::default(),
    }
}

/// Check repository compliance with RSR
pub async fn check_compliance(config: &Config, owner: &str, repo: &str) -> Result<ComplianceReport> {
    let client = GitHubClient::new(config);

    // Load repo-specific configuration
    let repo_config = load_repo_config(&client, owner, repo).await;

    check_compliance_with_policy(config, owner, repo, &repo_config).await
}

/// Check repository compliance with a specific policy configuration
pub async fn check_compliance_with_policy(
    config: &Config,
    owner: &str,
    repo: &str,
    repo_config: &RepoConfig,
) -> Result<ComplianceReport> {
    let client = GitHubClient::new(config);
    let policy = repo_config.policy;

    let mut checks = Vec::new();
    let mut total_score = 0u8;
    let mut max_score = 0u8;
    let mut required_passed = true;

    // Check required files
    for check_def in REQUIRED_FILES {
        // Skip if explicitly configured to skip
        if repo_config.skip.contains(&check_def.name.to_string()) {
            checks.push(Check {
                name: check_def.name.to_string(),
                category: check_def.category,
                severity: Severity::Optional,
                status: CheckStatus::Skip,
                points: 0,
                max_points: 0,
                message: format!("{} skipped by config", check_def.description),
            });
            continue;
        }

        // Determine severity (allow override from repo config)
        let severity = repo_config
            .severity_overrides
            .get(check_def.name)
            .copied()
            .unwrap_or_else(|| check_def.severity_for(policy));

        // Skip optional checks in scoring
        if severity == Severity::Optional {
            let exists = client.file_exists(owner, repo, check_def.name).await;
            checks.push(Check {
                name: check_def.name.to_string(),
                category: check_def.category,
                severity,
                status: if exists { CheckStatus::Pass } else { CheckStatus::Skip },
                points: 0,
                max_points: 0,
                message: if exists {
                    format!("{} found (optional)", check_def.description)
                } else {
                    format!("{} not present (optional)", check_def.description)
                },
            });
            continue;
        }

        max_score += check_def.points;
        let exists = client.file_exists(owner, repo, check_def.name).await;

        if exists {
            total_score += check_def.points;
            checks.push(Check {
                name: check_def.name.to_string(),
                category: check_def.category,
                severity,
                status: CheckStatus::Pass,
                points: check_def.points,
                max_points: check_def.points,
                message: format!("{} found", check_def.description),
            });
        } else {
            let status = match severity {
                Severity::Required => {
                    required_passed = false;
                    CheckStatus::Fail
                }
                Severity::Recommended => CheckStatus::Warn,
                Severity::Optional => CheckStatus::Skip,
            };

            checks.push(Check {
                name: check_def.name.to_string(),
                category: check_def.category,
                severity,
                status,
                points: 0,
                max_points: check_def.points,
                message: format!("{} missing", check_def.description),
            });
        }
    }

    // Check for banned patterns
    for banned in BANNED_PATTERNS {
        if repo_config.skip.contains(&format!("no-{}", banned.name)) {
            continue;
        }

        let severity = repo_config
            .severity_overrides
            .get(&format!("no-{}", banned.name))
            .copied()
            .unwrap_or_else(|| banned.severity_for(policy));

        let exists = client.file_exists(owner, repo, banned.name).await;

        if exists {
            let status = match severity {
                Severity::Required => {
                    required_passed = false;
                    CheckStatus::Fail
                }
                Severity::Recommended => CheckStatus::Warn,
                Severity::Optional => CheckStatus::Warn,
            };

            checks.push(Check {
                name: format!("no-{}", banned.name),
                category: banned.category,
                severity,
                status,
                points: 0,
                max_points: 0,
                message: format!("{} detected - policy violation", banned.description),
            });
        }
    }

    // Check for workflow files
    let workflow_severity = match policy {
        PolicyPack::Minimal => Severity::Optional,
        PolicyPack::Standard => Severity::Recommended,
        PolicyPack::Strict | PolicyPack::Enterprise => Severity::Required,
        PolicyPack::Custom => repo_config
            .severity_overrides
            .get(".github/workflows")
            .copied()
            .unwrap_or(Severity::Recommended),
    };

    if workflow_severity != Severity::Optional {
        max_score += 5;
    }

    let has_workflows = client.file_exists(owner, repo, ".github/workflows").await;

    if has_workflows {
        if workflow_severity != Severity::Optional {
            total_score += 5;
        }
        checks.push(Check {
            name: ".github/workflows".to_string(),
            category: CheckCategory::Structure,
            severity: workflow_severity,
            status: CheckStatus::Pass,
            points: if workflow_severity != Severity::Optional { 5 } else { 0 },
            max_points: if workflow_severity != Severity::Optional { 5 } else { 0 },
            message: "GitHub Actions workflows found".to_string(),
        });
    } else {
        let status = match workflow_severity {
            Severity::Required => {
                required_passed = false;
                CheckStatus::Fail
            }
            Severity::Recommended => CheckStatus::Warn,
            Severity::Optional => CheckStatus::Skip,
        };

        checks.push(Check {
            name: ".github/workflows".to_string(),
            category: CheckCategory::Structure,
            severity: workflow_severity,
            status,
            points: 0,
            max_points: if workflow_severity != Severity::Optional { 5 } else { 0 },
            message: "No GitHub Actions workflows".to_string(),
        });
    }

    // Check for required workflow files (hypatia-scan, codeql, scorecard)
    if has_workflows {
        let required_workflows = [
            ("hypatia-scan.yml", "Hypatia neurosymbolic security scan"),
            ("codeql.yml", "CodeQL code analysis"),
            ("scorecard.yml", "OpenSSF Scorecard"),
        ];

        let wf_check_severity = match policy {
            PolicyPack::Minimal | PolicyPack::Standard => Severity::Optional,
            PolicyPack::Strict => Severity::Recommended,
            PolicyPack::Enterprise => Severity::Required,
            PolicyPack::Custom => repo_config
                .severity_overrides
                .get("required-workflows")
                .copied()
                .unwrap_or(Severity::Optional),
        };

        for (wf_name, wf_desc) in &required_workflows {
            if repo_config.skip.contains(&format!("workflow-{}", wf_name)) {
                continue;
            }

            let wf_path = format!(".github/workflows/{}", wf_name);
            let wf_exists = client.file_exists(owner, repo, &wf_path).await;

            if wf_check_severity != Severity::Optional {
                max_score += 2;
            }

            if wf_exists {
                if wf_check_severity != Severity::Optional {
                    total_score += 2;
                }
                checks.push(Check {
                    name: format!("workflow-{}", wf_name),
                    category: CheckCategory::Structure,
                    severity: wf_check_severity,
                    status: CheckStatus::Pass,
                    points: if wf_check_severity != Severity::Optional { 2 } else { 0 },
                    max_points: if wf_check_severity != Severity::Optional { 2 } else { 0 },
                    message: format!("{} workflow found", wf_desc),
                });
            } else {
                let status = match wf_check_severity {
                    Severity::Required => {
                        required_passed = false;
                        CheckStatus::Fail
                    }
                    Severity::Recommended => CheckStatus::Warn,
                    Severity::Optional => CheckStatus::Skip,
                };

                checks.push(Check {
                    name: format!("workflow-{}", wf_name),
                    category: CheckCategory::Structure,
                    severity: wf_check_severity,
                    status,
                    points: 0,
                    max_points: if wf_check_severity != Severity::Optional { 2 } else { 0 },
                    message: format!("{} workflow missing", wf_desc),
                });
            }
        }
    }

    // Check for author attribution (Cargo.toml author should not be "hyperpolymath")
    if let Ok(cargo_content) = client.get_file_content(owner, repo, "Cargo.toml").await {
        let author_severity = match policy {
            PolicyPack::Minimal => Severity::Optional,
            PolicyPack::Standard | PolicyPack::Custom => Severity::Recommended,
            PolicyPack::Strict | PolicyPack::Enterprise => Severity::Required,
        };

        if !repo_config.skip.contains(&"author-attribution".to_string()) {
            let bad_author = cargo_content.contains("authors = [\"hyperpolymath");

            if bad_author {
                let status = match author_severity {
                    Severity::Required => {
                        required_passed = false;
                        CheckStatus::Fail
                    }
                    Severity::Recommended => CheckStatus::Warn,
                    Severity::Optional => CheckStatus::Skip,
                };

                checks.push(Check {
                    name: "author-attribution".to_string(),
                    category: CheckCategory::Governance,
                    severity: author_severity,
                    status,
                    points: 0,
                    max_points: 0,
                    message: "Cargo.toml uses 'hyperpolymath' as author instead of real name".to_string(),
                });
            } else {
                checks.push(Check {
                    name: "author-attribution".to_string(),
                    category: CheckCategory::Governance,
                    severity: author_severity,
                    status: CheckStatus::Pass,
                    points: 0,
                    max_points: 0,
                    message: "Author attribution is correct".to_string(),
                });
            }
        }
    }

    // Check license type
    if let Ok(repo_info) = client.get_repository(owner, repo).await {
        let license_severity = match policy {
            PolicyPack::Minimal => Severity::Recommended,
            _ => Severity::Required,
        };

        max_score += 5;
        if let Some(license) = repo_info.license {
            let approved_licenses = [
                "pmpl-1.0", "pmpl-1.0-or-later",
                "apache-2.0", "mit", "mpl-2.0", "lgpl-3.0",
                "agpl-3.0",  // Legacy: kept for backwards compatibility
            ];
            if approved_licenses.contains(&license.key.as_str()) {
                total_score += 5;
                checks.push(Check {
                    name: "license-type".to_string(),
                    category: CheckCategory::Governance,
                    severity: license_severity,
                    status: CheckStatus::Pass,
                    points: 5,
                    max_points: 5,
                    message: format!("Approved license: {}", license.name),
                });
            } else {
                if license_severity == Severity::Required {
                    required_passed = false;
                }
                checks.push(Check {
                    name: "license-type".to_string(),
                    category: CheckCategory::Governance,
                    severity: license_severity,
                    status: CheckStatus::Warn,
                    points: 2,
                    max_points: 5,
                    message: format!("Non-standard license: {}", license.name),
                });
                total_score += 2;
            }
        } else {
            if license_severity == Severity::Required {
                required_passed = false;
            }
            checks.push(Check {
                name: "license-type".to_string(),
                category: CheckCategory::Governance,
                severity: license_severity,
                status: CheckStatus::Fail,
                points: 0,
                max_points: 5,
                message: "No license detected".to_string(),
            });
        }
    }

    let percentage = if max_score > 0 {
        (total_score as f32 / max_score as f32) * 100.0
    } else {
        100.0
    };

    let summary = if !required_passed {
        format!("RSR {} policy: Required checks failed", policy)
    } else if percentage >= 90.0 {
        format!("Excellent RSR compliance ({})", policy)
    } else if percentage >= 70.0 {
        format!("Good RSR compliance ({}) with minor issues", policy)
    } else if percentage >= 50.0 {
        format!("Partial RSR compliance ({}) - improvements needed", policy)
    } else {
        format!("Poor RSR compliance ({}) - significant work required", policy)
    };

    Ok(ComplianceReport {
        owner: owner.to_string(),
        repo: repo.to_string(),
        policy,
        score: total_score,
        max_score,
        percentage,
        required_passed,
        checks,
        summary,
    })
}

/// Get the policy pack configuration summary
pub fn policy_summary(policy: PolicyPack) -> &'static str {
    match policy {
        PolicyPack::Minimal => {
            "Minimal policy: README.adoc and LICENSE.txt required. Other checks optional."
        }
        PolicyPack::Standard => {
            "Standard policy: Core files required, security and structure recommended."
        }
        PolicyPack::Strict => {
            "Strict policy: Most files required, language policy enforced."
        }
        PolicyPack::Enterprise => {
            "Enterprise policy: All checks required, full compliance mandatory."
        }
        PolicyPack::Custom => {
            "Custom policy: Defined by repository .rsr.toml configuration."
        }
    }
}
