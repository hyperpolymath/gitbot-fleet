// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Language detection and support

use anyhow::{bail, Result};
use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Language {
    Rust,
    JavaScript,
    TypeScript,
    Python,
}

impl Language {
    /// Detect language from file extension
    pub fn detect(path: &Path) -> Result<Self> {
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("");

        match ext {
            "rs" => Ok(Language::Rust),
            "js" | "mjs" | "cjs" => Ok(Language::JavaScript),
            "ts" | "mts" | "cts" => Ok(Language::TypeScript),
            "py" | "pyw" => Ok(Language::Python),
            _ => bail!("Unsupported file extension: {}", ext),
        }
    }

    /// Get tree-sitter parser for this language
    pub fn parser(&self) -> tree_sitter::Language {
        match self {
            Language::Rust => tree_sitter_rust::LANGUAGE.into(),
            Language::JavaScript | Language::TypeScript => {
                tree_sitter_javascript::LANGUAGE.into()
            }
            Language::Python => tree_sitter_python::LANGUAGE.into(),
        }
    }

    /// Human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            Language::Rust => "Rust",
            Language::JavaScript => "JavaScript",
            Language::TypeScript => "TypeScript",
            Language::Python => "Python",
        }
    }
}
