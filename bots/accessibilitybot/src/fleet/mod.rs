// SPDX-License-Identifier: PMPL-1.0-or-later
//! Fleet integration types for gitbot-fleet shared-context compatibility.
//!
//! These types mirror the gitbot-shared-context crate API so that
//! accessibilitybot can produce findings consumable by the fleet
//! coordinator. When gitbot-shared-context is published as a crate,
//! this module can be replaced with a direct dependency.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use uuid::Uuid;

/// Severity levels for findings (mirrors gitbot-shared-context::Severity)
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
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Error => write!(f, "ERROR"),
            Severity::Warning => write!(f, "WARNING"),
            Severity::Info => write!(f, "INFO"),
            Severity::Suggestion => write!(f, "SUGGESTION"),
        }
    }
}

/// WCAG conformance level
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum WcagLevel {
    /// Level A - minimum conformance
    A,
    /// Level AA - standard conformance
    AA,
    /// Level AAA - enhanced conformance
    AAA,
}

impl std::fmt::Display for WcagLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WcagLevel::A => write!(f, "A"),
            WcagLevel::AA => write!(f, "AA"),
            WcagLevel::AAA => write!(f, "AAA"),
        }
    }
}

/// Impact assessment: who is affected by an accessibility issue
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImpactAssessment {
    /// Affects blind users (screen reader users)
    pub blind: bool,
    /// Affects low-vision users
    pub low_vision: bool,
    /// Affects users with motor disabilities
    pub motor: bool,
    /// Affects users with cognitive disabilities
    pub cognitive: bool,
    /// Affects deaf/hard-of-hearing users
    pub deaf: bool,
}

impl ImpactAssessment {
    /// Create an impact affecting blind users
    pub fn blind() -> Self {
        Self { blind: true, low_vision: false, motor: false, cognitive: false, deaf: false }
    }

    /// Create an impact affecting low-vision users
    pub fn low_vision() -> Self {
        Self { blind: false, low_vision: true, motor: false, cognitive: false, deaf: false }
    }

    /// Create an impact affecting motor-disabled users
    pub fn motor() -> Self {
        Self { blind: false, low_vision: false, motor: true, cognitive: false, deaf: false }
    }

    /// Create an impact affecting cognitive-disabled users
    pub fn cognitive() -> Self {
        Self { blind: false, low_vision: false, motor: false, cognitive: true, deaf: false }
    }

    /// Create an impact affecting deaf/hard-of-hearing users
    pub fn deaf() -> Self {
        Self { blind: false, low_vision: false, motor: false, cognitive: false, deaf: true }
    }

    /// Create an impact affecting visual users (blind + low-vision)
    pub fn visual() -> Self {
        Self { blind: true, low_vision: true, motor: false, cognitive: false, deaf: false }
    }

    /// Describe affected groups
    pub fn affected_groups(&self) -> Vec<&'static str> {
        let mut groups = Vec::new();
        if self.blind { groups.push("blind"); }
        if self.low_vision { groups.push("low-vision"); }
        if self.motor { groups.push("motor"); }
        if self.cognitive { groups.push("cognitive"); }
        if self.deaf { groups.push("deaf/hard-of-hearing"); }
        groups
    }
}

/// A finding from an accessibility analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Finding {
    /// Unique identifier
    pub id: Uuid,
    /// Source bot identifier
    pub source: String,
    /// Rule/check identifier (e.g., "WCAG-1.1.1")
    pub rule_id: String,
    /// Human-readable rule name
    pub rule_name: String,
    /// Severity level
    pub severity: Severity,
    /// Detailed message
    pub message: String,
    /// Category (e.g., "accessibility/wcag-a")
    pub category: String,
    /// File where issue was found
    pub file: Option<PathBuf>,
    /// Line number (1-indexed)
    pub line: Option<usize>,
    /// Column number (1-indexed)
    pub column: Option<usize>,
    /// HTML/CSS element involved
    pub element: Option<String>,
    /// Suggested fix
    pub suggestion: Option<String>,
    /// Whether this can be auto-fixed
    pub fixable: bool,
    /// Fix was applied
    pub fixed: bool,
    /// When this finding was created
    pub created_at: DateTime<Utc>,
    /// WCAG criterion reference
    pub wcag_criterion: Option<String>,
    /// WCAG conformance level
    pub wcag_level: Option<WcagLevel>,
    /// Impact assessment
    pub impact: Option<ImpactAssessment>,
}

impl Finding {
    /// Create a new finding
    pub fn new(rule_id: &str, severity: Severity, message: &str) -> Self {
        Self {
            id: Uuid::new_v4(),
            source: "accessibilitybot".to_string(),
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
            wcag_criterion: None,
            wcag_level: None,
            impact: None,
        }
    }

    /// Set the WCAG criterion and level
    pub fn with_wcag(mut self, criterion: &str, level: WcagLevel) -> Self {
        self.wcag_criterion = Some(criterion.to_string());
        self.wcag_level = Some(level);
        self.category = match level {
            WcagLevel::A => "accessibility/wcag-a".to_string(),
            WcagLevel::AA => "accessibility/wcag-aa".to_string(),
            WcagLevel::AAA => "accessibility/wcag-aaa".to_string(),
        };
        self
    }

    /// Set the category
    pub fn with_category(mut self, category: &str) -> Self {
        self.category = category.to_string();
        self
    }

    /// Set the rule name
    pub fn with_rule_name(mut self, name: &str) -> Self {
        self.rule_name = name.to_string();
        self
    }

    /// Set file location
    pub fn with_file(mut self, file: PathBuf) -> Self {
        self.file = Some(file);
        self
    }

    /// Set line number
    pub fn with_line(mut self, line: usize) -> Self {
        self.line = Some(line);
        self
    }

    /// Set the HTML/CSS element
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
    pub fn as_fixable(mut self) -> Self {
        self.fixable = true;
        self
    }

    /// Set impact assessment
    pub fn with_impact(mut self, impact: ImpactAssessment) -> Self {
        self.impact = Some(impact);
        self
    }

    /// Mark as fixed
    pub fn mark_fixed(&mut self) {
        self.fixed = true;
    }

    /// Get location string for display
    pub fn location_string(&self) -> String {
        match (&self.file, self.line) {
            (Some(f), Some(l)) => format!("{}:{}", f.display(), l),
            (Some(f), None) => f.display().to_string(),
            _ => "<unknown>".to_string(),
        }
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

    /// Extend with findings from an iterator
    pub fn extend(&mut self, findings: impl IntoIterator<Item = Finding>) {
        self.findings.extend(findings);
    }

    /// Get findings by severity
    pub fn by_severity(&self, severity: Severity) -> Vec<&Finding> {
        self.findings.iter().filter(|f| f.severity == severity).collect()
    }

    /// Get findings by WCAG level
    pub fn by_wcag_level(&self, level: WcagLevel) -> Vec<&Finding> {
        self.findings.iter().filter(|f| f.wcag_level == Some(level)).collect()
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

    /// Total count
    pub fn len(&self) -> usize {
        self.findings.len()
    }

    /// Is empty
    pub fn is_empty(&self) -> bool {
        self.findings.is_empty()
    }

    /// Filter findings to a minimum WCAG level
    pub fn at_level(&self, min_level: WcagLevel) -> Vec<&Finding> {
        self.findings.iter().filter(|f| {
            match f.wcag_level {
                Some(level) => level <= min_level,
                None => true, // Include findings without a level
            }
        }).collect()
    }
}
