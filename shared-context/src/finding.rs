// SPDX-License-Identifier: PMPL-1.0
//! Finding representation for cross-bot communication

use crate::bot::BotId;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use uuid::Uuid;

/// Severity levels for findings
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    /// Critical issue - blocks release
    Error,
    /// Should be addressed
    Warning,
    /// Informational
    Info,
    /// Suggestion for improvement
    Suggestion,
}

impl Severity {
    /// Whether this severity blocks releases
    pub fn blocks_release(&self) -> bool {
        matches!(self, Severity::Error)
    }

    /// Get emoji icon
    pub fn icon(&self) -> &'static str {
        match self {
            Severity::Error => "❌",
            Severity::Warning => "⚠️",
            Severity::Info => "ℹ️",
            Severity::Suggestion => "💡",
        }
    }
}

/// A finding from a bot analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Finding {
    /// Unique identifier
    pub id: Uuid,
    /// Which bot produced this finding
    pub source: BotId,
    /// Rule/check identifier (e.g., "RSR-001", "WCAG-1.1.1")
    pub rule_id: String,
    /// Human-readable rule name
    pub rule_name: String,
    /// Severity level
    pub severity: Severity,
    /// Detailed message
    pub message: String,
    /// Category (e.g., "structure", "accessibility", "license")
    pub category: String,
    /// File where issue was found
    pub file: Option<PathBuf>,
    /// Line number (1-indexed)
    pub line: Option<usize>,
    /// Column number (1-indexed)
    pub column: Option<usize>,
    /// HTML/code element involved
    pub element: Option<String>,
    /// Suggested fix
    pub suggestion: Option<String>,
    /// Whether this can be auto-fixed
    pub fixable: bool,
    /// Fix was applied
    pub fixed: bool,
    /// When this finding was created
    pub created_at: DateTime<Utc>,
    /// Related findings (by ID)
    pub related: Vec<Uuid>,
    /// Custom metadata
    pub metadata: serde_json::Value,
}

impl Finding {
    /// Create a new finding
    pub fn new(source: BotId, rule_id: &str, severity: Severity, message: &str) -> Self {
        Self {
            id: Uuid::new_v4(),
            source,
            rule_id: rule_id.to_string(),
            rule_name: rule_id.to_string(),
            severity,
            message: message.to_string(),
            category: String::new(),
            file: None,
            line: None,
            column: None,
            element: None,
            suggestion: None,
            fixable: false,
            fixed: false,
            created_at: Utc::now(),
            related: Vec::new(),
            metadata: serde_json::Value::Null,
        }
    }

    /// Set rule name
    pub fn with_rule_name(mut self, name: &str) -> Self {
        self.rule_name = name.to_string();
        self
    }

    /// Set category
    pub fn with_category(mut self, category: &str) -> Self {
        self.category = category.to_string();
        self
    }

    /// Set file location
    pub fn with_file(mut self, file: PathBuf) -> Self {
        self.file = Some(file);
        self
    }

    /// Set line and column
    pub fn with_location(mut self, line: usize, column: usize) -> Self {
        self.line = Some(line);
        self.column = Some(column);
        self
    }

    /// Set just line
    pub fn with_line(mut self, line: usize) -> Self {
        self.line = Some(line);
        self
    }

    /// Set element
    pub fn with_element(mut self, element: &str) -> Self {
        self.element = Some(element.to_string());
        self
    }

    /// Set suggestion
    pub fn with_suggestion(mut self, suggestion: &str) -> Self {
        self.suggestion = Some(suggestion.to_string());
        self
    }

    /// Mark as fixable
    pub fn fixable(mut self) -> Self {
        self.fixable = true;
        self
    }

    /// Add related finding
    pub fn with_related(mut self, related_id: Uuid) -> Self {
        self.related.push(related_id);
        self
    }

    /// Add metadata
    pub fn with_metadata(mut self, metadata: serde_json::Value) -> Self {
        self.metadata = metadata;
        self
    }

    /// Mark as fixed
    pub fn mark_fixed(&mut self) {
        self.fixed = true;
    }

    /// Get location string for display
    pub fn location_string(&self) -> Option<String> {
        self.file.as_ref().map(|f| {
            match (self.line, self.column) {
                (Some(l), Some(c)) => format!("{}:{}:{}", f.display(), l, c),
                (Some(l), None) => format!("{}:{}", f.display(), l),
                _ => f.display().to_string(),
            }
        })
    }
}

/// A collection of findings with aggregation methods
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FindingSet {
    /// All findings
    pub findings: Vec<Finding>,
}

impl FindingSet {
    /// Create empty set
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a finding
    pub fn add(&mut self, finding: Finding) {
        self.findings.push(finding);
    }

    /// Extend with multiple findings
    pub fn extend(&mut self, findings: impl IntoIterator<Item = Finding>) {
        self.findings.extend(findings);
    }

    /// Get findings by source bot
    pub fn by_source(&self, source: BotId) -> Vec<&Finding> {
        self.findings.iter().filter(|f| f.source == source).collect()
    }

    /// Get findings by severity
    pub fn by_severity(&self, severity: Severity) -> Vec<&Finding> {
        self.findings
            .iter()
            .filter(|f| f.severity == severity)
            .collect()
    }

    /// Get findings by category
    pub fn by_category(&self, category: &str) -> Vec<&Finding> {
        self.findings
            .iter()
            .filter(|f| f.category == category)
            .collect()
    }

    /// Get all errors
    pub fn errors(&self) -> Vec<&Finding> {
        self.by_severity(Severity::Error)
    }

    /// Get all warnings
    pub fn warnings(&self) -> Vec<&Finding> {
        self.by_severity(Severity::Warning)
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        self.findings.iter().any(|f| f.severity == Severity::Error)
    }

    /// Check if release should be blocked
    pub fn blocks_release(&self) -> bool {
        self.findings.iter().any(|f| f.severity.blocks_release())
    }

    /// Get fixable findings
    pub fn fixable(&self) -> Vec<&Finding> {
        self.findings.iter().filter(|f| f.fixable && !f.fixed).collect()
    }

    /// Get unfixed findings
    pub fn unfixed(&self) -> Vec<&Finding> {
        self.findings.iter().filter(|f| !f.fixed).collect()
    }

    /// Total count
    pub fn len(&self) -> usize {
        self.findings.len()
    }

    /// Is empty
    pub fn is_empty(&self) -> bool {
        self.findings.is_empty()
    }

    /// Count by severity
    pub fn count_by_severity(&self) -> std::collections::HashMap<Severity, usize> {
        let mut counts = std::collections::HashMap::new();
        for finding in &self.findings {
            *counts.entry(finding.severity).or_insert(0) += 1;
        }
        counts
    }

    /// Count by source
    pub fn count_by_source(&self) -> std::collections::HashMap<BotId, usize> {
        let mut counts = std::collections::HashMap::new();
        for finding in &self.findings {
            *counts.entry(finding.source).or_insert(0) += 1;
        }
        counts
    }

    /// Find by ID
    pub fn find(&self, id: Uuid) -> Option<&Finding> {
        self.findings.iter().find(|f| f.id == id)
    }

    /// Find by ID (mutable)
    pub fn find_mut(&mut self, id: Uuid) -> Option<&mut Finding> {
        self.findings.iter_mut().find(|f| f.id == id)
    }
}

impl IntoIterator for FindingSet {
    type Item = Finding;
    type IntoIter = std::vec::IntoIter<Finding>;

    fn into_iter(self) -> Self::IntoIter {
        self.findings.into_iter()
    }
}

impl<'a> IntoIterator for &'a FindingSet {
    type Item = &'a Finding;
    type IntoIter = std::slice::Iter<'a, Finding>;

    fn into_iter(self) -> Self::IntoIter {
        self.findings.iter()
    }
}
