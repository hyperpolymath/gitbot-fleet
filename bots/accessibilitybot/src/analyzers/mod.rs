// SPDX-License-Identifier: PMPL-1.0-or-later
//! Accessibility analyzers implementing WCAG 2.3 checks.
//!
//! Each analyzer module focuses on a specific WCAG criterion or group
//! of related criteria. Analyzers scan HTML, CSS, and related files
//! to produce accessibility findings.

pub mod alt_text;
pub mod aria;
pub mod contrast;
pub mod css;
pub mod forms;
pub mod keyboard;
pub mod language;
pub mod media;
pub mod semantic;

use crate::fleet::{Finding, FindingSet, WcagLevel};
use std::path::Path;

/// Trait implemented by all analyzers
pub trait Analyzer: Send + Sync {
    /// Human-readable name of this analyzer
    fn name(&self) -> &str;

    /// Short description of what this analyzer checks
    fn description(&self) -> &str;

    /// Analyze a single file and return findings
    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding>;

    /// File extensions this analyzer applies to
    fn applicable_extensions(&self) -> &[&str];

    /// Check if this analyzer applies to the given WCAG level
    fn applies_to_level(&self, level: WcagLevel) -> bool;
}

/// Run all applicable analyzers on a file
pub fn analyze_file(
    path: &Path,
    content: &str,
    level: WcagLevel,
) -> FindingSet {
    let analyzers: Vec<Box<dyn Analyzer>> = vec![
        Box::new(alt_text::AltTextAnalyzer),
        Box::new(contrast::ContrastAnalyzer),
        Box::new(semantic::SemanticAnalyzer),
        Box::new(aria::AriaAnalyzer),
        Box::new(keyboard::KeyboardAnalyzer),
        Box::new(forms::FormAnalyzer),
        Box::new(media::MediaAnalyzer),
        Box::new(language::LanguageAnalyzer),
        Box::new(css::CssAnalyzer),
    ];

    let ext = path
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("");

    let mut findings = FindingSet::new();

    for analyzer in &analyzers {
        if !analyzer.applies_to_level(level) {
            continue;
        }

        let dominated_exts = analyzer.applicable_extensions();
        if dominated_exts.contains(&ext) || dominated_exts.contains(&"*") {
            let file_findings = analyzer.analyze_file(path, content);
            for f in file_findings {
                findings.add(f);
            }
        }
    }

    findings
}
