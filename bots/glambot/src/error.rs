// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Error types for glambot

use thiserror::Error;

pub type Result<T> = std::result::Result<T, GlambotError>;

#[derive(Error, Debug)]
pub enum GlambotError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Configuration error: {0}")]
    Config(String),

    #[error("Analysis error: {0}")]
    Analysis(String),

    #[error("Markdown parsing error: {0}")]
    Markdown(String),

    #[error("HTML parsing error: {0}")]
    Html(String),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("YAML error: {0}")]
    Yaml(#[from] serde_yaml::Error),

    #[error("URL error: {0}")]
    Url(#[from] url::ParseError),

    #[error("HTTP error: {0}")]
    Http(#[from] reqwest::Error),
}
