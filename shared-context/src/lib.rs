// SPDX-License-Identifier: PMPL-1.0
//! Gitbot Fleet Shared Context Layer
//!
//! This crate provides the coordination infrastructure for the gitbot-fleet,
//! enabling bots to share findings, state, and context during repository analysis.
//!
//! ## Architecture
//!
//! The shared context layer sits between two tiers of bots:
//!
//! **Tier 1 - Verifiers** (produce findings):
//! - `rhodibot` - RSR structural compliance
//! - `echidnabot` - Mathematical/formal verification
//! - `oikos` - Ecological/economic standards
//!
//! **Tier 2 - Finishers** (consume findings, produce results):
//! - `glambot` - Presentation quality
//! - `seambot` - Integration testing
//! - `finishing-bot` - Release readiness
//!
//! ## Usage
//!
//! ```rust,ignore
//! use gitbot_shared_context::{Context, BotId, Finding};
//!
//! // Create a context for a repository analysis session
//! let mut ctx = Context::new("my-repo", "/path/to/repo");
//!
//! // Rhodibot adds findings
//! ctx.add_finding(Finding::new(
//!     BotId::Rhodibot,
//!     "RSR-001",
//!     Severity::Error,
//!     "Missing README.adoc",
//! ));
//!
//! // Glambot can query findings from verifiers
//! let issues = ctx.findings_for_tier(Tier::Verifier);
//! ```

pub mod context;
pub mod finding;
pub mod bot;
pub mod state;
pub mod storage;

pub use context::Context;
pub use finding::{Finding, Severity};
pub use bot::{BotId, BotInfo, Tier};
pub use state::{SessionState, RepoState};
pub use storage::ContextStorage;

use thiserror::Error;

/// Errors from shared context operations
#[derive(Error, Debug)]
pub enum ContextError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    #[error("Context not found: {0}")]
    NotFound(String),

    #[error("Invalid state: {0}")]
    InvalidState(String),

    #[error("Bot not registered: {0}")]
    BotNotRegistered(String),
}

pub type Result<T> = std::result::Result<T, ContextError>;
