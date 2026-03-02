// SPDX-License-Identifier: PMPL-1.0-or-later
//! robot-repo-automaton - Automated repository management executor
//!
//! This crate provides the core functionality for automating repository
//! management tasks based on rules from cicd-hyper-a.
//!
//! # Architecture
//!
//! ```text
//! cicd-hyper-a (rules) → robot-repo-automaton (executor) → repositories
//!                              ↓
//!                    gitbot-fleet (coordination)
//! ```
//!
//! # Components
//!
//! - **Catalog**: Parse error catalogs from ERROR-CATALOG.scm
//! - **Detector**: Scan repositories for issues
//! - **Fixer**: Apply fixes to repositories
//! - **GitHub**: GitHub API integration
//! - **Hooks**: Git hook management
//!
//! # Usage
//!
//! ```rust,no_run
//! use robot_repo_automaton::{Config, ErrorCatalog, Detector, Fixer};
//! use std::path::PathBuf;
//!
//! # async fn example() -> robot_repo_automaton::Result<()> {
//! // Load configuration
//! let config = Config::from_file(&PathBuf::from("config.toml"))?;
//!
//! // Load error catalog
//! let catalog = ErrorCatalog::from_file(&config.catalog_path)?;
//!
//! // Detect issues in a repository
//! let detector = Detector::new(PathBuf::from("/path/to/repo"))?;
//! let issues = detector.detect_all(&catalog.error_types);
//!
//! // Apply fixes
//! let fixer = Fixer::new(PathBuf::from("/path/to/repo"), false);
//! for issue in &issues {
//!     if let Some(error_type) = catalog.get(&issue.error_type_id) {
//!         let result = fixer.apply(&issue, &error_type.fix)?;
//!         println!("Fix result: {:?}", result);
//!     }
//! }
//! # Ok(())
//! # }
//! ```

pub mod catalog;
pub mod confidence;
pub mod hypatia;
pub mod config;
pub mod detector;
pub mod error;
pub mod fixer;
pub mod fleet;
pub mod github;
pub mod hooks;

pub use catalog::ErrorCatalog;
pub use confidence::{ConfidenceLevel, FixDecision, ProposedFix, ThresholdConfig};
pub use hypatia::{CicdHyperAClient, CicdHyperAConfig, Rule, Ruleset};
pub use config::Config;
pub use detector::{DetectedIssue, Detector};
pub use error::{Error, Result};
pub use fixer::{FixResult, Fixer};
pub use fleet::FleetCoordinator;
pub use github::GitHubClient;
pub use hooks::{HookManager, HookType, PreCommitChecker};

/// Prelude module for common imports
pub mod prelude {
    pub use crate::catalog::{ErrorCatalog, ErrorType, Severity};
    pub use crate::confidence::{ConfidenceLevel, FixDecision, ProposedFix, ThresholdConfig};
    pub use crate::config::Config;
    pub use crate::detector::{DetectedIssue, Detector};
    pub use crate::error::{Error, Result};
    pub use crate::fixer::{FixResult, Fixer};
    pub use crate::fleet::FleetCoordinator;
    pub use crate::github::GitHubClient;
    pub use crate::hooks::{HookManager, HookType};
}
