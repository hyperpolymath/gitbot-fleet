// SPDX-License-Identifier: PMPL-1.0-or-later
//! Policy Engine — reads `.bot_directives/cipherbot.scm` for repo-specific
//! crypto policy enforcement.
//!
//! Supports:
//! - Minimum hash algorithm requirements
//! - Minimum symmetric cipher requirements
//! - Post-quantum requirement enforcement
//! - Maximum key age enforcement
//! - Exception lists for legacy compatibility modules

use serde::{Deserialize, Serialize};
use std::path::Path;

/// Cipherbot policy configuration parsed from `.bot_directives/cipherbot.scm`.
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

impl CipherbotPolicy {
    /// Load policy from `.bot_directives/cipherbot.scm` in the given repo root.
    ///
    /// Returns default policy if the file doesn't exist or can't be parsed.
    pub fn load(repo_root: &Path) -> Self {
        let policy_path = repo_root.join(".bot_directives/cipherbot.scm");
        if !policy_path.exists() {
            tracing::info!("No cipherbot.scm policy found, using defaults");
            return Self::default();
        }

        match std::fs::read_to_string(&policy_path) {
            Ok(content) => Self::parse_scm(&content),
            Err(e) => {
                tracing::warn!("Failed to read cipherbot.scm: {}", e);
                Self::default()
            }
        }
    }

    /// Parse a subset of S-expression policy format.
    ///
    /// This is a simplified parser that extracts key fields from the
    /// bot-directive S-expression format.
    fn parse_scm(content: &str) -> Self {
        let mut policy = Self::default();

        // Extract simple key-value pairs from the S-expression
        for line in content.lines() {
            let trimmed = line.trim();

            if let Some(rest) = trimmed.strip_prefix("(mode .") {
                if let Some(mode) = extract_string_value(rest) {
                    policy.mode = mode;
                }
            } else if let Some(rest) = trimmed.strip_prefix("(min-hash .") {
                if let Some(hash) = extract_string_value(rest) {
                    policy.min_hash = Some(hash);
                }
            } else if let Some(rest) = trimmed.strip_prefix("(min-symmetric .") {
                if let Some(sym) = extract_string_value(rest) {
                    policy.min_symmetric = Some(sym);
                }
            } else if let Some(rest) = trimmed.strip_prefix("(require-pq .") {
                policy.require_pq = rest.contains("#t");
            } else if let Some(rest) = trimmed.strip_prefix("(max-key-age-days .") {
                if let Some(days) = extract_number_value(rest) {
                    policy.max_key_age_days = Some(days);
                }
            } else if let Some(rest) = trimmed.strip_prefix("(allow .") {
                policy.allow = rest.contains("#t");
            } else if let Some(rest) = trimmed.strip_prefix("(scope .") {
                policy.scope = extract_list_values(rest);
            } else if let Some(rest) = trimmed.strip_prefix("(allowed-exceptions .") {
                policy.allowed_exceptions = extract_list_values(rest);
            }
        }

        policy
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

/// Extract a quoted string value from an S-expression fragment.
fn extract_string_value(s: &str) -> Option<String> {
    let start = s.find('"')?;
    let end = s[start + 1..].find('"')?;
    Some(s[start + 1..start + 1 + end].to_string())
}

/// Extract a numeric value from an S-expression fragment.
fn extract_number_value(s: &str) -> Option<u32> {
    let trimmed = s.trim().trim_end_matches(')');
    trimmed.trim().parse().ok()
}

/// Extract a list of string values from an S-expression fragment.
fn extract_list_values(s: &str) -> Vec<String> {
    let mut values = Vec::new();
    let mut rest = s;
    while let Some(start) = rest.find('"') {
        rest = &rest[start + 1..];
        if let Some(end) = rest.find('"') {
            values.push(rest[..end].to_string());
            rest = &rest[end + 1..];
        } else {
            break;
        }
    }
    values
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
    fn test_parse_scm() {
        let content = r#"
(bot-directive
  (name . "cipherbot")
  (allow . #t)
  (scope . ("src" "lib"))
  (mode . "regulator")
  (policy
    (min-hash . "shake3-512")
    (min-symmetric . "xchacha20-poly1305")
    (require-pq . #t)
    (max-key-age-days . 90)
    (allowed-exceptions . ("legacy-compat-module"))))
"#;
        let policy = CipherbotPolicy::parse_scm(content);
        assert_eq!(policy.mode, "regulator");
        assert!(policy.require_pq);
        assert_eq!(policy.max_key_age_days, Some(90));
        assert!(policy.allowed_exceptions.contains(&"legacy-compat-module".to_string()));
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

    #[test]
    fn test_extract_string_value() {
        assert_eq!(extract_string_value(r#" "hello")"#), Some("hello".to_string()));
        assert_eq!(extract_string_value("no quotes"), None);
    }

    #[test]
    fn test_extract_number_value() {
        assert_eq!(extract_number_value(" 90)"), Some(90));
        assert_eq!(extract_number_value(" abc)"), None);
    }

    #[test]
    fn test_extract_list_values() {
        let values = extract_list_values(r#"("src" "lib" "bin"))"#);
        assert_eq!(values, vec!["src", "lib", "bin"]);
    }
}
