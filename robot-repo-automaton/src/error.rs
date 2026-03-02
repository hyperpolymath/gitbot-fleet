// SPDX-License-Identifier: PMPL-1.0-or-later
//! Error types for robot-repo-automaton

use thiserror::Error;

/// Result type alias using our Error type
pub type Result<T> = std::result::Result<T, Error>;

/// Main error type for robot-repo-automaton
#[derive(Error, Debug)]
pub enum Error {
    #[error("Configuration error: {0}")]
    Config(String),

    #[error("Catalog parse error: {0}")]
    CatalogParse(String),

    #[error("Git error: {0}")]
    Git(#[from] git2::Error),

    #[error("GitHub API error: {0}")]
    GitHub(String),

    #[error("HTTP error: {0}")]
    Http(#[from] reqwest::Error),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("TOML error: {0}")]
    Toml(#[from] toml::de::Error),

    #[error("S-expression parse error: {0}")]
    Sexpr(String),

    #[error("Detection error: {0}")]
    Detection(String),

    #[error("Fix error: {0}")]
    Fix(String),

    #[error("Hook error: {0}")]
    Hook(String),

    #[error("Repository not found: {0}")]
    RepoNotFound(String),

    #[error("Rule not found: {0}")]
    RuleNotFound(String),

    #[error("Validation error: {0}")]
    Validation(String),

    #[error("Internal error: {0}")]
    Internal(String),
}

impl From<lexpr::parse::Error> for Error {
    fn from(e: lexpr::parse::Error) -> Self {
        Error::Sexpr(e.to_string())
    }
}
