// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Analyzers for release readiness validation

pub mod license;
pub mod placeholder;
pub mod claims;
pub mod release;
pub mod scm_files;
pub mod testing;
pub mod tooling;
pub mod v1_readiness;

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

    /// Check if there are any fixable issues
    pub fn has_fixable(&self) -> bool {
        self.findings.iter().any(|f| f.fixable)
    }

    /// Get fixable findings
    pub fn fixable(&self) -> Vec<&Finding> {
        self.findings.iter().filter(|f| f.fixable).collect()
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
    /// License analysis
    pub license: AnalysisResult,
    /// Placeholder analysis
    pub placeholder: AnalysisResult,
    /// Claim verification
    pub claims: AnalysisResult,
    /// Release readiness
    pub release: AnalysisResult,
    /// SCM files analysis
    pub scm_files: AnalysisResult,
    /// Testing infrastructure analysis
    pub testing: AnalysisResult,
    /// Tooling and version management
    pub tooling: AnalysisResult,
    /// V1 release readiness
    pub v1_readiness: AnalysisResult,
}

impl AuditResult {
    /// Total number of findings
    pub fn total_findings(&self) -> usize {
        self.license.findings.len()
            + self.placeholder.findings.len()
            + self.claims.findings.len()
            + self.release.findings.len()
            + self.scm_files.findings.len()
            + self.testing.findings.len()
            + self.tooling.findings.len()
            + self.v1_readiness.findings.len()
    }

    /// Total errors
    pub fn total_errors(&self) -> usize {
        self.license.errors().len()
            + self.placeholder.errors().len()
            + self.claims.errors().len()
            + self.release.errors().len()
            + self.scm_files.errors().len()
            + self.testing.errors().len()
            + self.tooling.errors().len()
            + self.v1_readiness.errors().len()
    }

    /// Total warnings
    pub fn total_warnings(&self) -> usize {
        self.license.warnings().len()
            + self.placeholder.warnings().len()
            + self.claims.warnings().len()
            + self.release.warnings().len()
            + self.scm_files.warnings().len()
            + self.testing.warnings().len()
            + self.tooling.warnings().len()
            + self.v1_readiness.warnings().len()
    }

    /// Check if release should be blocked
    pub fn should_block_release(&self) -> bool {
        self.license.has_errors()
            || self.placeholder.has_errors()
            || self.claims.has_errors()
            || self.release.has_errors()
            || self.scm_files.has_errors()
            || self.testing.has_errors()
            || self.tooling.has_errors()
            || self.v1_readiness.has_errors()
    }

    /// Get all findings across all analyzers
    pub fn all_findings(&self) -> Vec<&Finding> {
        let mut all = Vec::new();
        all.extend(self.license.findings.iter());
        all.extend(self.placeholder.findings.iter());
        all.extend(self.claims.findings.iter());
        all.extend(self.release.findings.iter());
        all.extend(self.scm_files.findings.iter());
        all.extend(self.testing.findings.iter());
        all.extend(self.tooling.findings.iter());
        all.extend(self.v1_readiness.findings.iter());
        all
    }
}
