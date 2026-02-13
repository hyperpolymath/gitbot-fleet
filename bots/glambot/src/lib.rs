// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! glambot: Presentation Quality Enforcer
//!
//! This crate provides comprehensive presentation quality validation for repositories,
//! ensuring they meet high standards for:
//!
//! - Visual polish (formatting, badges, branding)
//! - Accessibility (WCAG compliance, alt text, heading hierarchy)
//! - SEO (meta tags, descriptions, keywords)
//! - Machine-readability (structured data, valid JSON/YAML)
//!
//! Part of the Hyperpolymath Gitbot Fleet.

pub mod analyzers;
pub mod config;
pub mod error;
#[cfg(feature = "fleet")]
pub mod fleet;

pub use analyzers::{
    accessibility::AccessibilityAnalyzer, git_seo_integration::GitSeoAnalyzer,
    machine::MachineAnalyzer, seo::SeoAnalyzer, visual::VisualAnalyzer, AnalysisResult,
    Analyzer, AuditResult, Finding, Severity,
};
pub use config::Config;
pub use error::{GlambotError, Result};
