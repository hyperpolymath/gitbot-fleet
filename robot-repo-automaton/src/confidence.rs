// SPDX-License-Identifier: PMPL-1.0-or-later
//! Confidence threshold system for fix application
//!
//! Controls which fixes are automatically applied vs. proposed for review.
//! Reads configuration from `.machine_readable/bot_directives/robot-repo-automaton.scm`
//! (with legacy `.bot_directives/robot-repo-automaton.scm` fallback).
//!
//! ## Confidence Levels
//!
//! - **High**: Auto-apply (e.g., SPDX headers, .editorconfig creation)
//! - **Medium**: Apply with review (e.g., known pattern modifications)
//! - **Low**: Propose only (e.g., complex multi-file refactoring)

use lexpr::Value;
use std::path::Path;
use tracing::{debug, info, warn};

use crate::catalog::{Fix, FixAction};
use crate::detector::DetectedIssue;

/// Confidence level for a fix operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConfidenceLevel {
    /// Low confidence: propose the fix but do not apply it
    Low,
    /// Medium confidence: apply with review flag
    Medium,
    /// High confidence: auto-apply without review
    High,
}

impl ConfidenceLevel {
    /// Parse from string (case-insensitive)
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "low" => Some(ConfidenceLevel::Low),
            "medium" | "med" => Some(ConfidenceLevel::Medium),
            "high" => Some(ConfidenceLevel::High),
            _ => None,
        }
    }

    /// Convert to string representation
    pub fn as_str(&self) -> &'static str {
        match self {
            ConfidenceLevel::Low => "low",
            ConfidenceLevel::Medium => "medium",
            ConfidenceLevel::High => "high",
        }
    }
}

impl std::fmt::Display for ConfidenceLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Decision on whether to apply a fix
#[derive(Debug, Clone)]
pub enum FixDecision {
    /// Apply the fix automatically
    AutoApply,
    /// Propose the fix for human review (includes diff preview)
    Propose { diff_preview: String },
    /// Skip the fix (below threshold)
    Skip { reason: String },
}

/// Proposed fix that was not applied due to low confidence
#[derive(Debug, Clone)]
pub struct ProposedFix {
    /// The issue this fix addresses
    pub issue_id: String,
    /// Description of the proposed change
    pub description: String,
    /// Confidence level of the fix
    pub confidence: ConfidenceLevel,
    /// The exact diff that would be applied
    pub diff_preview: String,
    /// Target file path
    pub target_file: String,
    /// Fix action type
    pub action: String,
}

/// Confidence threshold configuration
#[derive(Debug, Clone)]
pub struct ThresholdConfig {
    /// Minimum confidence level for auto-applying fixes
    pub auto_apply_threshold: ConfidenceLevel,
    /// Override thresholds per fix action type
    pub action_overrides: Vec<(String, ConfidenceLevel)>,
}

impl Default for ThresholdConfig {
    fn default() -> Self {
        Self {
            auto_apply_threshold: ConfidenceLevel::High,
            action_overrides: Vec::new(),
        }
    }
}

impl ThresholdConfig {
    /// Read threshold configuration from a bot directive file
    ///
    /// Looks for `.machine_readable/bot_directives/robot-repo-automaton.scm`
    /// first, then legacy `.bot_directives/robot-repo-automaton.scm`.
    pub fn from_repo(repo_path: &Path) -> Self {
        let directive_path = if repo_path
            .join(".machine_readable/bot_directives/robot-repo-automaton.scm")
            .exists()
        {
            repo_path.join(".machine_readable/bot_directives/robot-repo-automaton.scm")
        } else {
            repo_path.join(".bot_directives/robot-repo-automaton.scm")
        };

        if !directive_path.exists() {
            debug!("No bot directive found, using default thresholds");
            return Self::default();
        }

        match std::fs::read_to_string(&directive_path) {
            Ok(content) => Self::parse_directive(&content),
            Err(e) => {
                warn!("Failed to read bot directive: {}", e);
                Self::default()
            }
        }
    }

    /// Parse threshold configuration from S-expression content
    fn parse_directive(content: &str) -> Self {
        let mut config = Self::default();

        if let Ok(value) = lexpr::from_str(content) {
            Self::extract_threshold(&value, &mut config);
        } else {
            warn!("Failed to parse bot directive, using defaults");
        }

        config
    }

    /// Extract threshold from parsed S-expression
    fn extract_threshold(value: &Value, config: &mut ThresholdConfig) {
        // Look for (auto-apply-threshold . "high"|"medium"|"low")
        if let Some(cons) = value.as_cons() {
            for item in cons.iter() {
                let car = item.car();
                if let Some(inner_cons) = car.as_cons() {
                    let key = inner_cons.car();
                    let val = inner_cons.cdr();
                    if let Some(sym) = key.as_symbol() {
                        if sym == "auto-apply-threshold" {
                            let threshold_str = if let Some(s) = val.as_str() {
                                s.to_string()
                            } else if let Some(s) = val.as_symbol() {
                                s.to_string()
                            } else {
                                val.to_string().trim_matches('"').to_string()
                            };

                            if let Some(level) = ConfidenceLevel::parse(&threshold_str) {
                                info!("Bot directive: auto-apply-threshold = {}", level);
                                config.auto_apply_threshold = level;
                            }
                        }
                    }
                }
                // Recurse into nested lists
                Self::extract_threshold(car, config);
            }
        }
    }

    /// Determine the confidence level for a fix
    pub fn classify_fix(&self, issue: &DetectedIssue, fix: &Fix) -> ConfidenceLevel {
        // Check action-specific overrides first
        let action_name = match fix.action {
            FixAction::Delete => "delete",
            FixAction::Modify => "modify",
            FixAction::Create => "create",
            FixAction::Disable => "disable",
        };

        for (action, level) in &self.action_overrides {
            if action == action_name {
                return *level;
            }
        }

        let target = &fix.target;

        // GUARD: Protected files are NEVER auto-applied for destructive actions.
        // Deletions of protected files are always skipped (Low).
        // Modifications of protected files require human review (Low).
        // Creating over protected paths is blocked (Low).
        if is_protected_file(target) {
            match fix.action {
                FixAction::Create => {
                    // Creating a missing standard protected file is OK at medium
                    // (human still reviews), but never high
                    return ConfidenceLevel::Medium;
                }
                FixAction::Delete | FixAction::Modify | FixAction::Disable => {
                    warn!(
                        "Protected file '{}' targeted for {:?} — forcing Low confidence",
                        target, fix.action
                    );
                    return ConfidenceLevel::Low;
                }
            }
        }

        // Default classification based on action type and issue properties
        match fix.action {
            // Creating missing standard files is high confidence
            FixAction::Create => {
                if is_standard_file(target) {
                    ConfidenceLevel::High
                } else {
                    ConfidenceLevel::Medium
                }
            }
            // Deleting files depends on the confidence of the detection
            FixAction::Delete => {
                if issue.confidence >= 0.95 {
                    ConfidenceLevel::High
                } else if issue.confidence >= 0.7 {
                    ConfidenceLevel::Medium
                } else {
                    ConfidenceLevel::Low
                }
            }
            // Modifications are generally medium confidence
            FixAction::Modify => {
                if is_spdx_modification(fix) {
                    ConfidenceLevel::High
                } else if issue.confidence >= 0.9 {
                    ConfidenceLevel::Medium
                } else {
                    ConfidenceLevel::Low
                }
            }
            // Disabling workflows requires review
            FixAction::Disable => ConfidenceLevel::Medium,
        }
    }

    /// Decide whether to apply a fix based on confidence threshold
    pub fn decide(&self, issue: &DetectedIssue, fix: &Fix) -> FixDecision {
        let confidence = self.classify_fix(issue, fix);

        if confidence >= self.auto_apply_threshold {
            FixDecision::AutoApply
        } else if confidence >= ConfidenceLevel::Low {
            let diff_preview = generate_diff_preview(issue, fix);
            FixDecision::Propose { diff_preview }
        } else {
            FixDecision::Skip {
                reason: format!(
                    "Confidence {} below threshold {}",
                    confidence, self.auto_apply_threshold
                ),
            }
        }
    }

    /// Create a proposed fix finding for fixes that are below threshold
    pub fn create_proposal(
        &self,
        issue: &DetectedIssue,
        fix: &Fix,
        diff_preview: &str,
    ) -> ProposedFix {
        let confidence = self.classify_fix(issue, fix);
        let action = match fix.action {
            FixAction::Delete => "delete",
            FixAction::Modify => "modify",
            FixAction::Create => "create",
            FixAction::Disable => "disable",
        };

        ProposedFix {
            issue_id: issue.error_type_id.clone(),
            description: format!(
                "Proposed fix for {}: {} {}",
                issue.error_name, action, fix.target
            ),
            confidence,
            diff_preview: diff_preview.to_string(),
            target_file: fix.target.clone(),
            action: action.to_string(),
        }
    }
}

/// Check if a file is protected from bot deletion or modification.
///
/// Protected files include project state, documentation, and checkpoint files
/// that bots should NEVER delete or overwrite. These represent intentional
/// project work that cannot be regenerated from templates.
fn is_protected_file(target: &str) -> bool {
    let basename = target.rsplit('/').next().unwrap_or(target);

    // Exact protected filenames (case-insensitive check on basename)
    let protected_names = [
        "TODO.md", "TODO.adoc", "TODO.txt",
        "BLOCKERS.md", "BLOCKERS.adoc",
        "CHANGELOG.md", "CHANGELOG.adoc",
        "README.md", "README.adoc", "README.rst",
        "TOPOLOGY.md", "TOPOLOGY.adoc",
        "ARCHITECTURE.md", "ARCHITECTURE.adoc",
        "ROADMAP.md", "ROADMAP.adoc",
    ];
    if protected_names.iter().any(|&p| p.eq_ignore_ascii_case(basename)) {
        return true;
    }

    // Protected extensions — checkpoint and manifest files
    let protected_extensions = [".scm", ".a2ml"];
    if protected_extensions.iter().any(|ext| basename.ends_with(ext)) {
        return true;
    }

    // Protected directories — never touch contents
    let protected_dirs = [
        ".machine_readable/",
        "src/abi/",
        "docs/",
        "spec/",
        "verification/",
    ];
    if protected_dirs.iter().any(|dir| target.contains(dir)) {
        return true;
    }

    false
}

/// Check if a file is a standard repository file (high confidence to create)
fn is_standard_file(target: &str) -> bool {
    let standard_files = [
        "LICENSE",
        "LICENSE.txt",
        ".editorconfig",
        "SECURITY.md",
        ".gitignore",
        ".gitattributes",
    ];

    standard_files.contains(&target)
}

/// Check if a modification is an SPDX header insertion (high confidence)
fn is_spdx_modification(fix: &Fix) -> bool {
    fix.modification
        .as_deref()
        .map(|m| m.contains("SPDX") || m.contains("spdx"))
        .unwrap_or(false)
}

/// Generate a human-readable diff preview for a proposed fix
fn generate_diff_preview(issue: &DetectedIssue, fix: &Fix) -> String {
    let action = match fix.action {
        FixAction::Delete => "DELETE",
        FixAction::Modify => "MODIFY",
        FixAction::Create => "CREATE",
        FixAction::Disable => "DISABLE",
    };

    let mut preview = format!(
        "--- Proposed Fix ---\n\
         Issue: {} ({})\n\
         Action: {} {}\n",
        issue.error_name, issue.error_type_id, action, fix.target
    );

    if let Some(ref modification) = fix.modification {
        preview.push_str(&format!("Modification: {}\n", modification));
    }

    if let Some(ref reason) = fix.reason {
        preview.push_str(&format!("Reason: {}\n", reason));
    }

    preview
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::catalog::{Fix, FixAction, Severity};
    use crate::detector::DetectedIssue;

    fn make_issue(id: &str, confidence: f64) -> DetectedIssue {
        DetectedIssue {
            error_type_id: id.to_string(),
            error_name: "Test Issue".to_string(),
            severity: Severity::Medium,
            description: "Test".to_string(),
            affected_files: vec![],
            confidence,
            suggested_fix: "Test fix".to_string(),
            commit_message: "fix: test".to_string(),
        }
    }

    fn make_fix(action: FixAction, target: &str) -> Fix {
        Fix {
            action,
            target: target.to_string(),
            reason: None,
            modification: None,
            fallback: None,
        }
    }

    #[test]
    fn test_high_confidence_auto_applies() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 1.0);
        let fix = make_fix(FixAction::Create, "LICENSE");

        let decision = config.decide(&issue, &fix);
        assert!(matches!(decision, FixDecision::AutoApply));
    }

    #[test]
    fn test_low_confidence_creates_proposal() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 0.5);
        let fix = make_fix(FixAction::Modify, "complex-file.rs");

        let decision = config.decide(&issue, &fix);
        assert!(matches!(decision, FixDecision::Propose { .. }));
    }

    #[test]
    fn test_custom_threshold_from_directive() {
        let directive = r#"
        (bot-directive
          (name . "robot-repo-automaton")
          (auto-apply-threshold . "medium"))
        "#;

        let config = ThresholdConfig::parse_directive(directive);
        assert_eq!(config.auto_apply_threshold, ConfidenceLevel::Medium);
    }

    #[test]
    fn test_default_threshold_is_high() {
        let config = ThresholdConfig::default();
        assert_eq!(config.auto_apply_threshold, ConfidenceLevel::High);
    }

    #[test]
    fn test_classify_standard_file_creation() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 1.0);

        // Standard files are high confidence
        let fix = make_fix(FixAction::Create, "LICENSE");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::High);

        let fix = make_fix(FixAction::Create, ".editorconfig");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::High);

        // Non-standard files are medium confidence
        let fix = make_fix(FixAction::Create, "custom-file.txt");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::Medium);
    }

    #[test]
    fn test_classify_spdx_modification() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 1.0);
        let fix = Fix {
            action: FixAction::Modify,
            target: "src/main.rs".to_string(),
            reason: None,
            modification: Some("prepend:// SPDX-License-Identifier: PMPL-1.0-or-later".to_string()),
            fallback: None,
        };

        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::High);
    }

    #[test]
    fn test_classify_low_confidence_delete() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 0.5);
        let fix = make_fix(FixAction::Delete, "uncertain-file.txt");

        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::Low);
    }

    #[test]
    fn test_proposal_creation() {
        let config = ThresholdConfig::default();
        let issue = make_issue("ERR-001", 0.5);
        let fix = make_fix(FixAction::Modify, "src/lib.rs");

        let proposal = config.create_proposal(&issue, &fix, "diff preview here");
        assert_eq!(proposal.issue_id, "ERR-001");
        assert_eq!(proposal.confidence, ConfidenceLevel::Low);
        assert!(proposal.description.contains("Proposed fix"));
    }

    #[test]
    fn test_confidence_level_ordering() {
        assert!(ConfidenceLevel::High > ConfidenceLevel::Medium);
        assert!(ConfidenceLevel::Medium > ConfidenceLevel::Low);
    }

    #[test]
    fn test_medium_threshold_allows_medium_fixes() {
        let config = ThresholdConfig {
            auto_apply_threshold: ConfidenceLevel::Medium,
            action_overrides: Vec::new(),
        };
        let issue = make_issue("TEST", 0.9);
        let fix = make_fix(FixAction::Modify, "some-file.txt");

        // Medium confidence fix with medium threshold -> auto-apply
        let decision = config.decide(&issue, &fix);
        assert!(matches!(decision, FixDecision::AutoApply));
    }

    // === Protected file tests ===

    #[test]
    fn test_protected_file_delete_always_low() {
        let config = ThresholdConfig::default();
        // Even with perfect detection confidence, deleting TODO.md is Low
        let issue = make_issue("TEST", 1.0);
        let fix = make_fix(FixAction::Delete, "TODO.md");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::Low);
    }

    #[test]
    fn test_protected_scm_files() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 1.0);

        // .scm files are always protected
        let fix = make_fix(FixAction::Delete, ".machine_readable/STATE.scm");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::Low);

        let fix = make_fix(FixAction::Modify, "META.scm");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::Low);
    }

    #[test]
    fn test_protected_a2ml_files() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 1.0);

        let fix = make_fix(FixAction::Delete, "0-AI-MANIFEST.a2ml");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::Low);
    }

    #[test]
    fn test_protected_dirs_block_modifications() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 1.0);

        let fix = make_fix(FixAction::Delete, ".machine_readable/6a2/STATE.a2ml");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::Low);

        let fix = make_fix(FixAction::Modify, "docs/architecture.md");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::Low);

        let fix = make_fix(FixAction::Delete, "src/abi/Types.idr");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::Low);
    }

    #[test]
    fn test_non_protected_file_still_classifies_normally() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 1.0);

        // A random file should NOT be protected
        let fix = make_fix(FixAction::Delete, "old-script.sh");
        assert_eq!(config.classify_fix(&issue, &fix), ConfidenceLevel::High);
    }

    #[test]
    fn test_protected_readme_variants() {
        let config = ThresholdConfig::default();
        let issue = make_issue("TEST", 1.0);

        for name in &["README.md", "README.adoc", "README.rst"] {
            let fix = make_fix(FixAction::Delete, name);
            assert_eq!(
                config.classify_fix(&issue, &fix),
                ConfidenceLevel::Low,
                "Expected {} to be protected",
                name
            );
        }
    }

    #[test]
    fn test_contributing_removed_from_standard_files() {
        // CONTRIBUTING.md and CODE_OF_CONDUCT.md are NOT standard files
        // (no templates exist for them)
        assert!(!is_standard_file("CONTRIBUTING.md"));
        assert!(!is_standard_file("CODE_OF_CONDUCT.md"));
    }
}
