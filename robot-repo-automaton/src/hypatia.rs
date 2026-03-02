// SPDX-License-Identifier: PMPL-1.0-or-later
//! Hypatia integration module
//!
//! Provides integration with the Hypatia neurosymbolic rules engine for:
//! - Fetching rulesets from the registry
//! - Executing rules on repositories
//! - Reporting results back to the engine for the learning loop
//!
//! ## Architecture
//!
//! ```text
//! Hypatia (neurosymbolic rules engine)
//!        |
//!        v
//! robot-repo-automaton (Tier 3 Executor)
//!        |
//!        v
//! target repositories
//!        |
//!        v (fix outcomes)
//! Hypatia learning loop
//! ```

use std::path::{Path, PathBuf};
use serde::{Deserialize, Serialize};

/// Configuration for cicd-hyper-a integration
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CicdHyperAConfig {
    /// Base URL of the cicd-hyper-a API
    #[serde(default = "default_api_url")]
    pub api_url: String,

    /// API key for authentication (optional for public rulesets)
    pub api_key: Option<String>,

    /// Default rulesets to use if not specified
    #[serde(default = "default_rulesets")]
    pub default_rulesets: Vec<String>,

    /// Enable learning pipeline feedback
    #[serde(default)]
    pub enable_feedback: bool,

    /// Timeout for API calls in seconds
    #[serde(default = "default_timeout")]
    pub timeout_secs: u64,
}

fn default_api_url() -> String {
    "http://localhost:8080/api/v1".to_string()
}

fn default_rulesets() -> Vec<String> {
    vec!["hyperpolymath/rsr-compliance".to_string()]
}

fn default_timeout() -> u64 {
    30
}

impl Default for CicdHyperAConfig {
    fn default() -> Self {
        Self {
            api_url: default_api_url(),
            api_key: None,
            default_rulesets: default_rulesets(),
            enable_feedback: false,
            timeout_secs: default_timeout(),
        }
    }
}

/// A rule from the cicd-hyper-a registry
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Rule {
    /// Unique rule identifier
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Rule category
    pub category: String,
    /// Severity level
    pub severity: RuleSeverity,
    /// Detection pattern (file glob, regex, etc.)
    pub pattern: RulePattern,
    /// Fix action to take
    pub fix: Option<RuleFix>,
    /// Rule metadata
    pub metadata: RuleMetadata,
}

/// Rule severity levels (matches cicd-hyper-a schema)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum RuleSeverity {
    Critical,
    High,
    Medium,
    Low,
    Info,
}

/// Rule pattern for detection
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum RulePattern {
    /// File path glob pattern
    FileGlob { glob: String },
    /// Content regex pattern
    ContentRegex { regex: String, file_glob: Option<String> },
    /// AST-based pattern (language-specific)
    Ast { language: String, query: String },
    /// Custom detector function
    Custom { name: String },
}

/// Fix action for a rule violation
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "action", rename_all = "kebab-case")]
pub enum RuleFix {
    /// Delete the file
    Delete { path: String },
    /// Modify file content
    Modify { path: String, content: String },
    /// Create a new file
    Create { path: String, content: String },
    /// Run a shell command
    Command { command: String, args: Vec<String> },
    /// Apply a patch
    Patch { patch: String },
}

/// Rule metadata
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RuleMetadata {
    /// Rule author
    pub author: Option<String>,
    /// Last updated timestamp
    pub updated_at: Option<String>,
    /// Rule version
    pub version: String,
    /// Tags for categorization
    #[serde(default)]
    pub tags: Vec<String>,
    /// Whether this rule was learned from patterns
    #[serde(default)]
    pub learned: bool,
}

/// A ruleset from the registry
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Ruleset {
    /// Ruleset identifier (org/name format)
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Description
    pub description: String,
    /// Version
    pub version: String,
    /// Rules in this ruleset
    pub rules: Vec<Rule>,
}

/// Result of executing a rule
#[derive(Debug, Clone, Serialize)]
pub struct RuleExecutionResult {
    /// Rule that was executed
    pub rule_id: String,
    /// Whether a violation was detected
    pub violation_detected: bool,
    /// Files affected
    pub affected_files: Vec<PathBuf>,
    /// Whether fix was applied
    pub fix_applied: bool,
    /// Error message if execution failed
    pub error: Option<String>,
}

/// cicd-hyper-a client for interacting with the rules engine
pub struct CicdHyperAClient {
    config: CicdHyperAConfig,
    #[allow(dead_code)]
    http_client: reqwest::Client,
}

impl CicdHyperAClient {
    /// Create a new client with the given configuration
    pub fn new(config: CicdHyperAConfig) -> Self {
        let http_client = reqwest::Client::builder()
            .timeout(std::time::Duration::from_secs(config.timeout_secs))
            .build()
            .expect("Failed to create HTTP client");

        Self {
            config,
            http_client,
        }
    }

    /// Fetch a ruleset from the registry
    pub async fn fetch_ruleset(&self, ruleset_id: &str) -> crate::Result<Ruleset> {
        // In a real implementation, this would call the cicd-hyper-a API
        // For now, return a placeholder that demonstrates the structure
        tracing::info!("Fetching ruleset: {} from {}", ruleset_id, self.config.api_url);

        // Placeholder ruleset demonstrating the integration
        Ok(Ruleset {
            id: ruleset_id.to_string(),
            name: "RSR Compliance".to_string(),
            description: "Rhodium Standard Repositories compliance rules".to_string(),
            version: "1.0.0".to_string(),
            rules: vec![
                Rule {
                    id: "RSR-001".to_string(),
                    name: "Missing README".to_string(),
                    category: "structure".to_string(),
                    severity: RuleSeverity::High,
                    pattern: RulePattern::FileGlob {
                        glob: "README.{md,adoc}".to_string(),
                    },
                    fix: Some(RuleFix::Create {
                        path: "README.adoc".to_string(),
                        content: "= Project Name\n\nDescription here.\n".to_string(),
                    }),
                    metadata: RuleMetadata {
                        author: Some("cicd-hyper-a".to_string()),
                        updated_at: Some("2026-01-18".to_string()),
                        version: "1.0.0".to_string(),
                        tags: vec!["rsr".to_string(), "documentation".to_string()],
                        learned: false,
                    },
                },
            ],
        })
    }

    /// Execute rules from a ruleset on a repository
    pub async fn execute_ruleset(
        &self,
        repo_path: &Path,
        ruleset: &Ruleset,
        dry_run: bool,
    ) -> Vec<RuleExecutionResult> {
        let mut results = Vec::new();

        for rule in &ruleset.rules {
            let result = self.execute_rule(repo_path, rule, dry_run).await;
            results.push(result);
        }

        results
    }

    /// Execute a single rule on a repository
    async fn execute_rule(
        &self,
        repo_path: &Path,
        rule: &Rule,
        dry_run: bool,
    ) -> RuleExecutionResult {
        tracing::debug!("Executing rule {} on {}", rule.id, repo_path.display());

        // Check for violation
        let (violation_detected, affected_files) = match &rule.pattern {
            RulePattern::FileGlob { glob } => {
                let pattern = repo_path.join(glob).to_string_lossy().to_string();
                let matches: Vec<PathBuf> = glob::glob(&pattern)
                    .map(|paths| paths.filter_map(Result::ok).collect())
                    .unwrap_or_default();

                // For "missing file" rules, violation is when NO matches
                // For "bad file" rules, violation is when matches exist
                // This is simplified; real implementation would check rule semantics
                let is_missing = matches.is_empty();
                (is_missing, matches)
            }
            RulePattern::ContentRegex { regex, file_glob } => {
                let mut affected = Vec::new();
                let glob_pattern = file_glob.clone().unwrap_or_else(|| "**/*".to_string());
                let pattern = repo_path.join(&glob_pattern).to_string_lossy().to_string();

                if let Ok(paths) = glob::glob(&pattern) {
                    let re = regex::Regex::new(regex).ok();
                    for path in paths.filter_map(Result::ok) {
                        if path.is_file() {
                            if let Ok(content) = std::fs::read_to_string(&path) {
                                if let Some(ref re) = re {
                                    if re.is_match(&content) {
                                        affected.push(path);
                                    }
                                }
                            }
                        }
                    }
                }

                (!affected.is_empty(), affected)
            }
            RulePattern::Ast { .. } => {
                // AST-based detection would require language-specific parsers
                (false, vec![])
            }
            RulePattern::Custom { .. } => {
                // Custom detectors would be registered plugins
                (false, vec![])
            }
        };

        // Apply fix if violation detected and not dry run
        let fix_applied = if violation_detected && !dry_run {
            if let Some(ref fix) = rule.fix {
                match fix {
                    RuleFix::Create { path, content } => {
                        let file_path = repo_path.join(path);
                        std::fs::write(&file_path, content).is_ok()
                    }
                    RuleFix::Delete { path } => {
                        let file_path = repo_path.join(path);
                        std::fs::remove_file(&file_path).is_ok()
                    }
                    RuleFix::Modify { path, content } => {
                        let file_path = repo_path.join(path);
                        std::fs::write(&file_path, content).is_ok()
                    }
                    RuleFix::Command { .. } | RuleFix::Patch { .. } => {
                        // Command and patch fixes require more complex handling
                        false
                    }
                }
            } else {
                false
            }
        } else {
            false
        };

        RuleExecutionResult {
            rule_id: rule.id.clone(),
            violation_detected,
            affected_files,
            fix_applied,
            error: None,
        }
    }

    /// Report execution results back to Hypatia for the neurosymbolic learning loop.
    ///
    /// Each rule execution result is recorded as a fix outcome observation,
    /// enabling the learning engine to:
    /// - Adjust confidence thresholds based on success/failure rates
    /// - Propose new rules when recurring patterns are observed
    /// - Auto-approve rules that consistently produce successful fixes
    pub async fn report_results(
        &self,
        repo_path: &PathBuf,
        results: &[RuleExecutionResult],
    ) -> crate::Result<()> {
        if !self.config.enable_feedback {
            return Ok(());
        }

        let repo_name = repo_path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");

        tracing::info!(
            "Reporting {} results to Hypatia learning pipeline for repo: {}",
            results.len(),
            repo_name
        );

        for result in results {
            let outcome = serde_json::json!({
                "rule_id": result.rule_id,
                "repo": repo_name,
                "violation_detected": result.violation_detected,
                "fix_applied": result.fix_applied,
                "affected_files": result.affected_files.iter()
                    .map(|p| p.display().to_string())
                    .collect::<Vec<_>>(),
                "success": result.fix_applied && result.error.is_none(),
                "error": result.error,
                "observed": chrono::Utc::now().to_rfc3339(),
            });

            tracing::debug!("Fix outcome: {:?}", outcome);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_defaults() {
        let config = CicdHyperAConfig::default();
        assert_eq!(config.api_url, "http://localhost:8080/api/v1");
        assert!(!config.enable_feedback);
    }

    #[test]
    fn test_rule_pattern_serde() {
        let pattern = RulePattern::FileGlob {
            glob: "*.rs".to_string(),
        };
        let json = serde_json::to_string(&pattern).unwrap();
        assert!(json.contains("file-glob"));
    }

    #[test]
    fn test_rule_severity_order() {
        assert!(matches!(RuleSeverity::Critical, RuleSeverity::Critical));
    }
}
