// SPDX-License-Identifier: PMPL-1.0-or-later
//! Policy Engine — reads `.machine_readable/bot_directives/cipherbot.a2ml`
//! for repo-specific crypto policy enforcement.
//!
//! Supports:
//! - Minimum hash algorithm requirements
//! - Minimum symmetric cipher requirements
//! - Post-quantum requirement enforcement
//! - Maximum key age enforcement
//! - Exception lists for legacy compatibility modules
//!
//! The SCM form was retired 2026-04-17; no fallback is supported.

use serde::{Deserialize, Serialize};
use std::path::Path;

/// Cipherbot policy configuration parsed from
/// `.machine_readable/bot_directives/cipherbot.a2ml`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CipherbotPolicy {
    /// Bot name (should be "cipherbot").
    pub name: String,
    /// Whether the bot is allowed to run.
    pub allow: bool,
    /// Directories to scan.
    pub scope: Vec<String>,
    /// Operating mode (advisor, consultant, regulator, policy).
    pub mode: String,
    /// Minimum acceptable hash algorithm.
    pub min_hash: Option<String>,
    /// Minimum acceptable symmetric cipher.
    pub min_symmetric: Option<String>,
    /// Whether post-quantum algorithms are required.
    pub require_pq: bool,
    /// Maximum key age in days.
    pub max_key_age_days: Option<u32>,
    /// Paths/modules that are exceptions to the policy.
    pub allowed_exceptions: Vec<String>,
}

impl Default for CipherbotPolicy {
    fn default() -> Self {
        Self {
            name: "cipherbot".to_string(),
            allow: true,
            scope: vec!["src".to_string(), "lib".to_string()],
            mode: "advisor".to_string(),
            min_hash: Some("shake3-512".to_string()),
            min_symmetric: Some("xchacha20-poly1305".to_string()),
            require_pq: false,
            max_key_age_days: Some(90),
            allowed_exceptions: Vec::new(),
        }
    }
}

/// Raw A2ML shape for cipherbot policy.
#[derive(Debug, Default, Deserialize)]
struct PolicyFile {
    #[serde(default)]
    bot: Option<String>,
    #[serde(default)]
    allow: Option<AllowField>,
    #[serde(default)]
    scope: Option<ScopeField>,
    #[serde(default)]
    mode: Option<String>,
    /// Nested [policy] block (lithoglyph-style) or flat top-level keys.
    #[serde(default)]
    policy: Option<PolicyBlock>,
    #[serde(default, rename = "min-hash")]
    min_hash_top: Option<String>,
    #[serde(default, rename = "min-symmetric")]
    min_symmetric_top: Option<String>,
    #[serde(default, rename = "require-pq")]
    require_pq_top: Option<bool>,
    #[serde(default, rename = "max-key-age-days")]
    max_key_age_days_top: Option<u32>,
    #[serde(default, rename = "allowed-exceptions")]
    allowed_exceptions_top: Option<Vec<String>>,
}

#[derive(Debug, Default, Deserialize)]
struct PolicyBlock {
    #[serde(default, rename = "min-hash")]
    min_hash: Option<String>,
    #[serde(default, rename = "min-symmetric")]
    min_symmetric: Option<String>,
    #[serde(default, rename = "require-pq")]
    require_pq: Option<bool>,
    #[serde(default, rename = "max-key-age-days")]
    max_key_age_days: Option<u32>,
    #[serde(default, rename = "allowed-exceptions")]
    allowed_exceptions: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum ScopeField {
    One(String),
    Many(Vec<String>),
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum AllowField {
    Bool(bool),
    Scopes(Vec<String>),
}

impl CipherbotPolicy {
    /// Load policy from `.machine_readable/bot_directives/cipherbot.a2ml` in
    /// the given repo root. Returns default policy if the file doesn't exist
    /// or can't be parsed.
    pub fn load(repo_root: &Path) -> Self {
        let policy_path = repo_root.join(".machine_readable/bot_directives/cipherbot.a2ml");
        if !policy_path.exists() {
            tracing::info!("No cipherbot.a2ml policy found, using defaults");
            return Self::default();
        }

        match std::fs::read_to_string(&policy_path) {
            Ok(content) => Self::parse_a2ml(&content).unwrap_or_else(|e| {
                tracing::warn!("Failed to parse cipherbot.a2ml: {}", e);
                Self::default()
            }),
            Err(e) => {
                tracing::warn!("Failed to read cipherbot.a2ml: {}", e);
                Self::default()
            }
        }
    }

    /// Parse cipherbot policy from A2ML/TOML content.
    fn parse_a2ml(content: &str) -> Result<Self, toml::de::Error> {
        let file: PolicyFile = toml::from_str(content)?;
        let mut policy = Self::default();

        if let Some(name) = file.bot {
            policy.name = name;
        }

        policy.allow = match file.allow {
            Some(AllowField::Bool(b)) => b,
            // List-of-scopes implies allow = true.
            Some(AllowField::Scopes(_)) => true,
            None => policy.allow,
        };

        if let Some(scope) = file.scope {
            policy.scope = match scope {
                ScopeField::One(s) => vec![s],
                ScopeField::Many(v) => v,
            };
        }

        if let Some(mode) = file.mode {
            policy.mode = mode;
        }

        // Nested [policy] block wins; top-level kebab keys are the fallback
        // for single-bot files where the author did not introduce a section.
        let pb = file.policy.unwrap_or_default();

        if let Some(h) = pb.min_hash.or(file.min_hash_top) {
            policy.min_hash = Some(h);
        }
        if let Some(s) = pb.min_symmetric.or(file.min_symmetric_top) {
            policy.min_symmetric = Some(s);
        }
        if let Some(b) = pb.require_pq.or(file.require_pq_top) {
            policy.require_pq = b;
        }
        if let Some(d) = pb.max_key_age_days.or(file.max_key_age_days_top) {
            policy.max_key_age_days = Some(d);
        }
        if let Some(ex) = pb.allowed_exceptions.or(file.allowed_exceptions_top) {
            policy.allowed_exceptions = ex;
        }

        Ok(policy)
    }

    /// Check if a file path is within the policy scope.
    pub fn is_in_scope(&self, path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        if self.scope.is_empty() {
            return true;
        }
        self.scope.iter().any(|s| path_str.contains(s))
    }

    /// Check if a file path is in the exception list.
    pub fn is_exception(&self, path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        self.allowed_exceptions
            .iter()
            .any(|e| path_str.contains(e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_policy() {
        let policy = CipherbotPolicy::default();
        assert!(policy.allow);
        assert_eq!(policy.mode, "advisor");
        assert_eq!(policy.min_hash, Some("shake3-512".to_string()));
        assert!(!policy.require_pq);
    }

    #[test]
    fn test_parse_a2ml_nested_policy_block() {
        let content = r#"
schema_version = "1.0"
bot = "cipherbot"
allow = true
scope = ["src", "lib"]
mode = "regulator"

[policy]
min-hash = "shake3-512"
min-symmetric = "xchacha20-poly1305"
require-pq = true
max-key-age-days = 90
allowed-exceptions = ["legacy-compat-module"]
"#;
        let policy = CipherbotPolicy::parse_a2ml(content).unwrap();
        assert_eq!(policy.mode, "regulator");
        assert!(policy.require_pq);
        assert_eq!(policy.max_key_age_days, Some(90));
        assert!(policy
            .allowed_exceptions
            .contains(&"legacy-compat-module".to_string()));
    }

    #[test]
    fn test_parse_a2ml_flat_toplevel() {
        // A directive where the author used only the top-level scalar keys
        // (no nested [policy] section) — migrated from a minimal SCM.
        let content = r#"
bot = "cipherbot"
allow = true
scope = "src"
mode = "advisor"
min-hash = "sha256"
require-pq = false
"#;
        let policy = CipherbotPolicy::parse_a2ml(content).unwrap();
        assert_eq!(policy.mode, "advisor");
        assert_eq!(policy.scope, vec!["src".to_string()]);
        assert_eq!(policy.min_hash, Some("sha256".to_string()));
        assert!(!policy.require_pq);
    }

    #[test]
    fn test_is_in_scope() {
        let policy = CipherbotPolicy::default();
        assert!(policy.is_in_scope(Path::new("src/main.rs")));
        assert!(policy.is_in_scope(Path::new("lib/crypto.rs")));
        assert!(!policy.is_in_scope(Path::new("tests/fixtures/test.rs")));
    }

    #[test]
    fn test_is_exception() {
        let mut policy = CipherbotPolicy::default();
        policy.allowed_exceptions = vec!["legacy-compat".to_string()];
        assert!(policy.is_exception(Path::new("src/legacy-compat/old.rs")));
        assert!(!policy.is_exception(Path::new("src/main.rs")));
    }
}
