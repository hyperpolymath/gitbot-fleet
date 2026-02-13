// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Analyzers for presentation quality validation

pub mod visual;
pub mod accessibility;
pub mod seo;
pub mod machine;
pub mod git_seo_integration;

use crate::config::Config;
use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Severity levels for findings
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    /// Blocks release
    Error,
    /// Should be addressed
    Warning,
    /// Informational
    Info,
    /// Improvement suggestion
    Suggestion,
}

/// A single finding from analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Finding {
    /// Finding identifier
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Severity level
    pub severity: Severity,
    /// Detailed message
    pub message: String,
    /// File where issue was found
    pub file: Option<PathBuf>,
    /// Line number (1-indexed)
    pub line: Option<usize>,
    /// Column number (1-indexed)
    pub column: Option<usize>,
    /// Suggested fix
    pub suggestion: Option<String>,
    /// Whether this can be auto-fixed
    pub fixable: bool,
}

impl Finding {
    /// Create a new finding
    pub fn new(id: &str, name: &str, severity: Severity, message: &str) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            severity,
            message: message.to_string(),
            file: None,
            line: None,
            column: None,
            suggestion: None,
            fixable: false,
        }
    }

    /// Add file location
    pub fn with_file(mut self, file: PathBuf) -> Self {
        self.file = Some(file);
        self
    }

    /// Add line/column location
    pub fn with_location(mut self, line: usize, column: usize) -> Self {
        self.line = Some(line);
        self.column = Some(column);
        self
    }

    /// Add just line number
    pub fn with_line(mut self, line: usize) -> Self {
        self.line = Some(line);
        self
    }

    /// Add suggestion
    pub fn with_suggestion(mut self, suggestion: &str) -> Self {
        self.suggestion = Some(suggestion.to_string());
        self
    }

    /// Mark as auto-fixable
    pub fn fixable(mut self) -> Self {
        self.fixable = true;
        self
    }
}

/// Results from running an analyzer
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct AnalysisResult {
    /// List of findings
    pub findings: Vec<Finding>,
    /// Number of files analyzed
    pub files_checked: usize,
    /// Duration in milliseconds
    pub duration_ms: u64,
}

impl AnalysisResult {
    /// Create a new empty result
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a finding
    pub fn add(&mut self, finding: Finding) {
        self.findings.push(finding);
    }

    /// Get all errors
    pub fn errors(&self) -> Vec<&Finding> {
        self.findings
            .iter()
            .filter(|f| f.severity == Severity::Error)
            .collect()
    }

    /// Get all warnings
    pub fn warnings(&self) -> Vec<&Finding> {
        self.findings
            .iter()
            .filter(|f| f.severity == Severity::Warning)
            .collect()
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        self.findings.iter().any(|f| f.severity == Severity::Error)
    }
}

/// Trait for all analyzers
pub trait Analyzer {
    /// Get analyzer name
    fn name(&self) -> &str;

    /// Run the analyzer on a path
    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult>;

    /// Apply fixes (if supported)
    fn fix(&self, path: &Path, config: &Config, findings: &[Finding]) -> Result<Vec<String>>;
}

/// Combined analysis results from all analyzers
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct AuditResult {
    /// Visual polish analysis
    pub visual: AnalysisResult,
    /// Accessibility analysis
    pub accessibility: AnalysisResult,
    /// SEO analysis
    pub seo: AnalysisResult,
    /// Machine-readability analysis
    pub machine: AnalysisResult,
    /// Git-SEO integration (comprehensive forge-based SEO)
    pub git_seo: AnalysisResult,
}

impl AuditResult {
    /// Total number of findings
    pub fn total_findings(&self) -> usize {
        self.visual.findings.len()
            + self.accessibility.findings.len()
            + self.seo.findings.len()
            + self.machine.findings.len()
            + self.git_seo.findings.len()
    }

    /// Total errors
    pub fn total_errors(&self) -> usize {
        self.visual.errors().len()
            + self.accessibility.errors().len()
            + self.seo.errors().len()
            + self.machine.errors().len()
            + self.git_seo.errors().len()
    }

    /// Total warnings
    pub fn total_warnings(&self) -> usize {
        self.visual.warnings().len()
            + self.accessibility.warnings().len()
            + self.seo.warnings().len()
            + self.machine.warnings().len()
            + self.git_seo.warnings().len()
    }

    /// Check if release should be blocked
    pub fn should_block_release(&self) -> bool {
        self.visual.has_errors()
            || self.accessibility.has_errors()
            || self.seo.has_errors()
            || self.machine.has_errors()
            || self.git_seo.has_errors()
    }

    /// Get all findings across all analyzers
    pub fn all_findings(&self) -> Vec<&Finding> {
        let mut all = Vec::new();
        all.extend(self.visual.findings.iter());
        all.extend(self.accessibility.findings.iter());
        all.extend(self.seo.findings.iter());
        all.extend(self.machine.findings.iter());
        all.extend(self.git_seo.findings.iter());
        all
    }
}
