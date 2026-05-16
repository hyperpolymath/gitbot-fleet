// SPDX-License-Identifier: PMPL-1.0-or-later

//! Core seam data structures and types

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A seam represents an architectural boundary between components
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Seam {
    /// Unique identifier for the seam
    pub id: String,

    /// Human-readable name
    pub name: String,

    /// Description of what this seam represents
    pub description: String,

    /// Components on one side of the seam
    pub side_a: Vec<String>,

    /// Components on the other side of the seam
    pub side_b: Vec<String>,

    /// Seam type/category
    pub seam_type: SeamType,

    /// Invariants that must hold across this seam
    pub invariants: Vec<Invariant>,

    /// Stage when this seam was introduced
    pub introduced_at: Option<String>,

    /// Whether this seam is frozen (no changes allowed)
    pub frozen: bool,

    /// Ring level (0 = core, 1 = standard, 2 = augmented)
    pub ring: u8,

    /// Path to checklist file
    pub checklist_path: Option<String>,

    /// Paths to conformance examples
    pub conformance_paths: Vec<String>,

    /// Path to the boundary (directory or module path)
    #[serde(default)]
    pub boundary_path: String,

    /// Declared dependencies on other seams (by name)
    #[serde(default)]
    pub declared_dependencies: Vec<String>,
}

/// Types of seams
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum SeamType {
    /// Between modules within a repo
    Module,
    /// Between services/repos
    Service,
    /// Between layers (e.g., domain/infrastructure)
    Layer,
    /// Data flow boundary
    Data,
    /// API boundary
    Api,
    /// Build/deployment boundary
    Build,
    /// Cross-repo seam (managed by git-loom)
    CrossRepo,
}

/// An invariant that must hold across a seam
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Invariant {
    /// Unique identifier
    pub id: String,

    /// Description of the invariant
    pub description: String,

    /// How to verify this invariant
    pub verification: VerificationType,

    /// Severity if violated
    pub severity: Severity,
}

/// How an invariant is verified
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VerificationType {
    /// Manual review required
    Manual,
    /// Automated test exists
    Automated { test_path: String },
    /// Pattern matching in code
    Pattern { regex: String },
    /// Type system enforces it
    TypeSystem,
    /// Static analysis tool
    StaticAnalysis { tool: String },
}

/// Severity levels for violations
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum Severity {
    Info,
    Warning,
    Error,
    Critical,
}

/// The seam register containing all seams in a repository
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeamRegister {
    /// Schema version
    pub version: String,

    /// Repository this register belongs to
    pub repository: String,

    /// All registered seams
    pub seams: Vec<Seam>,

    /// Cross-repo seam references (for git-loom)
    #[serde(default)]
    pub cross_repo_seams: Vec<CrossRepoSeamRef>,

    /// Metadata about last update
    pub metadata: RegisterMetadata,
}

/// Reference to a seam in another repository
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrossRepoSeamRef {
    /// ID of the seam in the remote repo
    pub seam_id: String,

    /// Remote repository
    pub repository: String,

    /// Local components that touch this seam
    pub local_components: Vec<String>,
}

/// Metadata about the seam register
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegisterMetadata {
    /// When the register was last updated
    pub updated_at: String,

    /// Who/what updated it
    pub updated_by: String,

    /// Git commit hash at last update
    pub commit_hash: Option<String>,
}

/// A seam freeze stamp for stage delivery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeamFreezeStamp {
    /// Stage identifier (e.g., "f1", "f2")
    pub stage: String,

    /// Timestamp of the freeze
    pub frozen_at: String,

    /// Seams frozen at this stage
    pub frozen_seams: Vec<String>,

    /// SHA256 hash of the seam register at freeze time
    pub register_hash: String,

    /// Commit hash at freeze
    pub commit_hash: String,
}

/// Result of a seam check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckResult {
    /// Overall status
    pub status: CheckStatus,

    /// Individual findings
    pub findings: Vec<Finding>,

    /// Summary statistics
    pub summary: CheckSummary,
}

impl CheckResult {
    pub fn new() -> Self {
        Self {
            status: CheckStatus::Pass,
            findings: Vec::new(),
            summary: CheckSummary::default(),
        }
    }

    pub fn add_finding(&mut self, finding: Finding) {
        match finding.severity {
            Severity::Error | Severity::Critical => {
                self.status = CheckStatus::Fail;
                self.summary.errors += 1;
            }
            Severity::Warning => {
                if self.status == CheckStatus::Pass {
                    self.status = CheckStatus::Warn;
                }
                self.summary.warnings += 1;
            }
            Severity::Info => {
                self.summary.info += 1;
            }
        }
        self.findings.push(finding);
    }

    pub fn has_errors(&self) -> bool {
        self.summary.errors > 0
    }

    pub fn has_warnings(&self) -> bool {
        self.summary.warnings > 0
    }

    pub fn merge(&mut self, other: CheckResult) {
        for finding in other.findings {
            self.add_finding(finding);
        }
    }
}

impl Default for CheckResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Overall check status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum CheckStatus {
    Pass,
    Warn,
    Fail,
}

/// A finding from a check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Finding {
    /// Check that produced this finding
    pub check: String,

    /// Severity level
    pub severity: Severity,

    /// Message describing the finding
    pub message: String,

    /// Location (file path, seam ID, etc.)
    pub location: Option<String>,

    /// Suggested fix
    pub suggestion: Option<String>,

    /// Related seam ID if applicable
    pub seam_id: Option<String>,
}

/// Summary statistics for a check
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CheckSummary {
    pub total_seams: usize,
    pub checked_seams: usize,
    pub errors: usize,
    pub warnings: usize,
    pub info: usize,
}

/// Interface fingerprint for drift detection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterfaceFingerprint {
    /// Seam this fingerprint is for
    pub seam_id: String,

    /// Hash of the interface signature
    pub signature_hash: String,

    /// Files that make up this interface
    pub files: Vec<FileFingerprint>,

    /// Timestamp of fingerprint
    pub timestamp: String,
}

/// Fingerprint of a single file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileFingerprint {
    pub path: String,
    pub hash: String,
    pub exported_symbols: Vec<String>,
}

/// Drift baseline for comparison
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DriftBaseline {
    pub version: String,
    pub created_at: String,
    pub fingerprints: HashMap<String, InterfaceFingerprint>,
}
