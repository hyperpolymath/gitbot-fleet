// SPDX-License-Identifier: MPL-2.0

//! RSR (Rhodium Standard Repository) compliance checking module
//!
//! Supports policy packs and opt-in severity levels for flexible compliance.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::canon::a2ml;
use crate::config::Config;
use crate::deed::{Deed, DocFormat};
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
    /// Canonical path, ordered by prevalence across the estate so the common
    /// case resolves on the first request.
    pub name: &'static str,
    /// Historically valid spellings accepted in place of the canonical path,
    /// so a rename is not reported as a compliance regression.
    pub alternates: &'static [&'static str],
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

/// Resolve a check against a repository: canonical path first, then each
/// accepted alternate in order.
///
/// Alternates are only consulted when the canonical path is absent, so the
/// overwhelmingly common case — a repository on the current convention —
/// costs exactly one request, unchanged from before.
pub async fn check_path_exists(
    client: &GitHubClient,
    owner: &str,
    repo: &str,
    def: &CheckDef,
) -> (bool, String) {
    if client.file_exists(owner, repo, def.name).await {
        return (true, def.name.to_string());
    }
    for alt in def.alternates {
        if client.file_exists(owner, repo, alt).await {
            return (true, (*alt).to_string());
        }
    }
    (false, def.name.to_string())
}

/// Points for the chora deed check, as the manifest it replaced carried.
const CHORA_DEED_POINTS: u8 = 3;

/// `CONTRIBUTING` or `CODE_OF_CONDUCT` when this check is one of the two
/// community documents whose spelling (`.adoc` or `.md`) is toggleable.
fn community_stem(name: &str) -> Option<&str> {
    name.strip_suffix(".adoc")
        .filter(|stem| matches!(*stem, "CONTRIBUTING" | "CODE_OF_CONDUCT" | "SECURITY"))
}

/// The spellings to look for, the first being the one a report names.
///
/// Every accepted spelling is always looked for, so a repository is never
/// reported missing a file it has in the other form. For the two community
/// documents the repository's own preference (from its bot directive, default
/// `.adoc`) goes first; it changes the wording of a finding, not whether there
/// is one.
pub fn candidates_for(def: &CheckDef, format: DocFormat) -> Vec<String> {
    let mut all: Vec<String> = std::iter::once(def.name)
        .chain(def.alternates.iter().copied())
        .map(str::to_string)
        .collect();
    if let Some(stem) = community_stem(def.name) {
        let preferred = format!("{stem}.{}", format.extension());
        if let Some(at) = all.iter().position(|name| *name == preferred) {
            let name = all.remove(at);
            all.insert(0, name);
        }
    }
    all
}

/// Whether any of the candidate paths exists, and which one.
pub async fn check_candidates_exist(
    client: &GitHubClient,
    owner: &str,
    repo: &str,
    candidates: &[String],
) -> (bool, String) {
    for candidate in candidates {
        if client.file_exists(owner, repo, candidate).await {
            return (true, candidate.clone());
        }
    }
    (false, candidates[0].clone())
}

/// The name of the priority-1 check for retired `.a2ml` files.
pub const A2ML_CHECK: &str = "no-*.a2ml";

/// Required files for RSR compliance with policy-based severity
pub const REQUIRED_FILES: &[CheckDef] = &[
    // NOTE ON THE 2026-09-18 REVISION
    //
    // Every path below was re-measured against 269 estate repositories (the
    // #119 denominator). The previous table predated the Markdown/TXT ->
    // AsciiDoc migration (#486, 2026-08) and the canonical-location changes
    // that followed, so it demanded filenames the estate had deliberately
    // moved away from: LICENSE.txt (0/269), CONTRIBUTING.md (25/269),
    // CODE_OF_CONDUCT.md (15/269), SECURITY.md (35/269) and lowercase
    // `justfile` (7/269). It also required Guile Scheme state files that
    // exist in a single repository.
    //
    // The canonical spelling is listed first and ordered by prevalence, so
    // the common case resolves on the first request; superseded spellings are
    // accepted as alternates rather than reported as regressions.
    CheckDef {
        name: "README.adoc",
        alternates: &["README.md"],
        description: "AsciiDoc README",
        category: CheckCategory::Documentation,
        points: 5,
        severity: (
            Severity::Required,
            Severity::Required,
            Severity::Required,
            Severity::Required,
        ),
    },
    CheckDef {
        // 269/269 repositories carry `LICENSE`; none carries `LICENSE.txt`.
        name: "LICENSE",
        alternates: &["LICENSE.txt", "LICENSE.md", "LICENSE.adoc"],
        description: "License file",
        category: CheckCategory::Governance,
        points: 5,
        severity: (
            Severity::Required,
            Severity::Required,
            Severity::Required,
            Severity::Required,
        ),
    },
    CheckDef {
        // SECURITY.adoc 250/269 versus SECURITY.md 35/269.
        name: "SECURITY.adoc",
        alternates: &["SECURITY.md"],
        description: "Security policy",
        category: CheckCategory::Security,
        points: 5,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    CheckDef {
        // CONTRIBUTING.adoc 255/269 versus CONTRIBUTING.md 25/269.
        name: "CONTRIBUTING.adoc",
        alternates: &["CONTRIBUTING.md"],
        description: "Contributing guidelines",
        category: CheckCategory::Documentation,
        points: 3,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    CheckDef {
        // CODE_OF_CONDUCT.adoc 240/269 versus CODE_OF_CONDUCT.md 15/269.
        name: "CODE_OF_CONDUCT.adoc",
        alternates: &["CODE_OF_CONDUCT.md"],
        description: "Code of conduct",
        category: CheckCategory::Governance,
        points: 3,
        severity: (
            Severity::Optional,
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
        ),
    },
    CheckDef {
        // 152/269 under .claude/, 39/269 at the root.
        name: ".claude/CLAUDE.md",
        alternates: &["CLAUDE.md", ".claude/CLAUDE.adoc"],
        description: "AI assistant instructions",
        category: CheckCategory::Structure,
        points: 2,
        severity: (
            Severity::Optional,
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
        ),
    },
    // The machine-readable repo facts (manifest, state, meta, ecosystem, ...)
    // are clauses of the chora deed, not files. They are checked by the chora
    // deed check in `check_compliance_with_policy`; no `.a2ml` or `.scm` file is
    // demanded here.
    // Additional RSR template files
    CheckDef {
        name: ".editorconfig",
        alternates: &[],
        description: "EditorConfig for consistent formatting",
        category: CheckCategory::Structure,
        points: 2,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    CheckDef {
        name: ".gitattributes",
        alternates: &[],
        description: "Git attributes for line endings and diffs",
        category: CheckCategory::Structure,
        points: 2,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    CheckDef {
        name: ".gitignore",
        alternates: &[],
        description: "Git ignore patterns",
        category: CheckCategory::Structure,
        points: 2,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    CheckDef {
        // `Justfile` 267/269; lowercase `justfile` 7/269. Both accepted: the
        // capital form is what just itself prefers, the lowercase form is
        // what some older checkouts carry.
        name: "Justfile",
        alternates: &["justfile"],
        description: "Just task runner (primary build system)",
        category: CheckCategory::Structure,
        points: 2,
        severity: (
            Severity::Optional,
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
        ),
    },
    // RSR-015 (.machine_readable/bot_directives/) is retired: its files were
    // .a2ml, and the bot directives are the chora deed's (bot-directives ...)
    // clause now. Requiring the directory demanded a retired form.
    CheckDef {
        // Entered the template 2026-08-26; 59/269 as of 2026-09-18. Either
        // canonical spelling is accepted (see check-root-shape.sh).
        name: ".machine_readable/root-allow.txt",
        alternates: &["machine-readable/root-allow.txt"],
        description: "Root-shape allowlist in canonical machine-readable location",
        category: CheckCategory::Structure,
        points: 2,
        severity: (
            Severity::Optional,
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
        ),
    },
    CheckDef {
        // Canonical location since #53 (2026-09-17). Measured 2026-09-18:
        // 0/269 repositories have any www/ tree, because the RSR update
        // mechanism that would create it has not yet run anywhere. The check
        // is therefore honest about the gap and drives the migration rather
        // than merely noting it; raise it once the sweep has landed.
        name: "www/.well-known/security.txt",
        alternates: &[],
        description: "Security contact metadata in the canonical www/ location",
        category: CheckCategory::Security,
        points: 3,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
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
        name: ".bot_directives",
        description: "Legacy bot directives directory at repo root (use the chora deed's (bot-directives ...) clause)",
        category: CheckCategory::Structure,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    BannedPattern {
        name: "package-lock.json",
        description: "npm lock file (use Deno)",
        category: CheckCategory::LanguagePolicy,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    BannedPattern {
        name: "yarn.lock",
        description: "Yarn lock file (use Deno)",
        category: CheckCategory::LanguagePolicy,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    BannedPattern {
        name: "pnpm-lock.yaml",
        description: "pnpm lock file (use Deno)",
        category: CheckCategory::LanguagePolicy,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    BannedPattern {
        name: "bun.lockb",
        description: "Bun lock file (use Deno)",
        category: CheckCategory::LanguagePolicy,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    BannedPattern {
        name: "go.mod",
        description: "Go module (use Rust)",
        category: CheckCategory::LanguagePolicy,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    BannedPattern {
        name: "go.sum",
        description: "Go checksum (use Rust)",
        category: CheckCategory::LanguagePolicy,
        severity: (
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
            Severity::Required,
        ),
    },
    BannedPattern {
        // Legacy location: canonical is www/.well-known/ since #53.
        // Advisory (Warn) at minimal and standard while the stage-5 migration
        // window is open; raise .2/.3 to Required once the sweep has landed
        // and the root location should no longer be tolerated at all.
        name: ".well-known/security.txt",
        description: "Legacy repository-root .well-known/ (canonical location is www/.well-known/)",
        category: CheckCategory::Structure,
        severity: (
            Severity::Optional,
            Severity::Optional,
            Severity::Recommended,
            Severity::Required,
        ),
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
        Ok(content) => toml::from_str(&content).unwrap_or_else(|e| {
            tracing::warn!("Failed to parse .rsr.toml: {}", e);
            RepoConfig::default()
        }),
        Err(_) => RepoConfig::default(),
    }
}

/// Check repository compliance with RSR
pub async fn check_compliance(
    config: &Config,
    owner: &str,
    repo: &str,
) -> Result<ComplianceReport> {
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

    // The chora deed holds the repo facts and the bot directives. A deed that
    // is present but does not read is an error, not "no directives".
    let chora_name = format!("{repo}_chora.deed");
    let chora_text = client
        .get_file_content_if_present(owner, repo, &chora_name)
        .await?;
    let deed = chora_text
        .as_deref()
        .map(Deed::parse)
        .transpose()
        .with_context(|| format!("{chora_name} does not read as an s-expression"))?;
    let doc_format = DocFormat::from_deed(deed.as_ref())?;

    // The chora deed is required in place of the retired `.a2ml` files.
    // Whether it conforms is `deed_lint.js`'s question (standards CI), not
    // this check's.
    {
        let severity = repo_config
            .severity_overrides
            .get(&chora_name)
            .copied()
            .unwrap_or(match policy {
                PolicyPack::Minimal => Severity::Optional,
                PolicyPack::Standard | PolicyPack::Custom => Severity::Recommended,
                PolicyPack::Strict | PolicyPack::Enterprise => Severity::Required,
            });
        let present = deed.is_some();
        let counted = severity != Severity::Optional && !repo_config.skip.contains(&chora_name);
        let points = if counted { CHORA_DEED_POINTS } else { 0 };
        if counted {
            max_score += points;
        }
        let status = match (present, severity) {
            (true, _) => CheckStatus::Pass,
            (false, Severity::Required) if counted => {
                required_passed = false;
                CheckStatus::Fail
            }
            (false, Severity::Recommended) if counted => CheckStatus::Warn,
            (false, _) => CheckStatus::Skip,
        };
        if present && counted {
            total_score += points;
        }
        checks.push(Check {
            name: chora_name.clone(),
            category: CheckCategory::Structure,
            severity,
            status,
            points: if present { points } else { 0 },
            max_points: points,
            message: if present {
                format!("Chora deed (repo facts and bot directives) found at {chora_name}")
            } else {
                format!(
                    "Chora deed (repo facts and bot directives) missing - expected at {chora_name}"
                )
            },
        });
    }

    // Check required files
    for check_def in REQUIRED_FILES {
        // The spellings to look for, the repository's preferred one first.
        let candidates = candidates_for(check_def, doc_format);
        let expected_name = candidates[0].clone();

        // Skip if explicitly configured to skip
        if repo_config.skip.contains(&check_def.name.to_string())
            || repo_config.skip.contains(&expected_name)
        {
            checks.push(Check {
                name: expected_name.clone(),
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
            .or_else(|| repo_config.severity_overrides.get(&expected_name))
            .copied()
            .unwrap_or_else(|| check_def.severity_for(policy));

        // Skip optional checks in scoring
        if severity == Severity::Optional {
            let (exists, found_at) =
                check_candidates_exist(&client, owner, repo, &candidates).await;
            checks.push(Check {
                name: expected_name.clone(),
                category: check_def.category,
                severity,
                status: if exists {
                    CheckStatus::Pass
                } else {
                    CheckStatus::Skip
                },
                points: 0,
                max_points: 0,
                message: if exists {
                    format!("{} found at {} (optional)", check_def.description, found_at)
                } else {
                    format!("{} not present (optional)", check_def.description)
                },
            });
            continue;
        }

        max_score += check_def.points;
        let (exists, found_at) = check_candidates_exist(&client, owner, repo, &candidates).await;
        let accepted = if candidates.len() < 2 {
            String::new()
        } else if community_stem(check_def.name).is_some() {
            // Either spelling satisfies it; neither is required as well.
            format!(" (or {})", candidates[1..].join(", "))
        } else {
            format!(" (or, historically: {})", candidates[1..].join(", "))
        };

        if exists {
            total_score += check_def.points;
            checks.push(Check {
                name: expected_name.clone(),
                category: check_def.category,
                severity,
                status: CheckStatus::Pass,
                points: check_def.points,
                max_points: check_def.points,
                message: format!("{} found at {}", check_def.description, found_at),
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
                name: expected_name.clone(),
                category: check_def.category,
                severity,
                status,
                points: 0,
                max_points: check_def.points,
                message: format!(
                    "{} missing - expected at {}{}",
                    check_def.description, expected_name, accepted
                ),
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

    // Priority 1: any `.a2ml` file anywhere in the tree. A2ML is retired (D99,
    // D269c, D312, D313), so this is Required in every policy pack and a repo
    // config can neither skip nor downgrade it. A tree that cannot be listed is
    // reported as skipped with the reason, never as a pass.
    match client.tree_paths(owner, repo).await {
        Ok(paths) => {
            let found: Vec<&String> = paths.iter().filter(|p| a2ml::is_a2ml(p)).collect();
            if !found.is_empty() {
                required_passed = false;
                let shown: Vec<&str> = found.iter().take(10).map(|p| p.as_str()).collect();
                let more = found.len().saturating_sub(shown.len());
                checks.push(Check {
                    name: A2ML_CHECK.to_string(),
                    category: CheckCategory::Structure,
                    severity: Severity::Required,
                    status: CheckStatus::Fail,
                    points: 0,
                    max_points: 0,
                    message: format!(
                        "P1: {} retired .a2ml file(s) present: {}{} - A2ML is retired; convert \
                         with kyX (D313), never by hand",
                        found.len(),
                        shown.join(", "),
                        if more > 0 {
                            format!(" (+{more} more)")
                        } else {
                            String::new()
                        }
                    ),
                });
            }
        }
        Err(error) => checks.push(Check {
            name: A2ML_CHECK.to_string(),
            category: CheckCategory::Structure,
            severity: Severity::Required,
            status: CheckStatus::Skip,
            points: 0,
            max_points: 0,
            message: format!("could not list the tree to look for .a2ml files: {error}"),
        }),
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
            points: if workflow_severity != Severity::Optional {
                5
            } else {
                0
            },
            max_points: if workflow_severity != Severity::Optional {
                5
            } else {
                0
            },
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
            max_points: if workflow_severity != Severity::Optional {
                5
            } else {
                0
            },
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
                    points: if wf_check_severity != Severity::Optional {
                        2
                    } else {
                        0
                    },
                    max_points: if wf_check_severity != Severity::Optional {
                        2
                    } else {
                        0
                    },
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
                    max_points: if wf_check_severity != Severity::Optional {
                        2
                    } else {
                        0
                    },
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
                    message: "Cargo.toml uses 'hyperpolymath' as author instead of real name"
                        .to_string(),
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
                "pmpl-1.0",
                "pmpl-1.0-or-later",
                "apache-2.0",
                "mit",
                "mpl-2.0",
                "lgpl-3.0",
                "agpl-3.0", // Legacy: kept for backwards compatibility
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
        format!(
            "Poor RSR compliance ({}) - significant work required",
            policy
        )
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
        PolicyPack::Strict => "Strict policy: Most files required, language policy enforced.",
        PolicyPack::Enterprise => {
            "Enterprise policy: All checks required, full compliance mandatory."
        }
        PolicyPack::Custom => "Custom policy: Defined by repository .rsr.toml configuration.",
    }
}
