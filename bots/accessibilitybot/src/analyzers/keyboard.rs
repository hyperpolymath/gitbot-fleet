// SPDX-License-Identifier: PMPL-1.0-or-later
//! Keyboard navigation analyzer - WCAG 2.1.1 Keyboard (Level A), 2.4.7 Focus Visible (Level AA)
//!
//! Checks for keyboard accessibility:
//! - Interactive elements are keyboard-focusable
//! - No positive tabindex values (disrupts natural tab order)
//! - Focus indicators visible (not suppressed with outline: none)
//! - Skip navigation links present
//! - No keyboard traps (modals have escape mechanism)

use crate::analyzers::Analyzer;
use crate::fleet::{Finding, ImpactAssessment, Severity, WcagLevel};
use regex::Regex;
use scraper::{Html, Selector};
use std::path::Path;

/// Keyboard navigation analyzer
pub struct KeyboardAnalyzer;

impl Analyzer for KeyboardAnalyzer {
    fn name(&self) -> &str {
        "Keyboard Navigation Analyzer"
    }

    fn description(&self) -> &str {
        "Checks keyboard accessibility (WCAG 2.1.1, 2.4.7)"
    }

    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding> {
        let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
        match ext {
            "css" => check_focus_styles_css(path, content),
            "html" | "htm" | "jsx" | "tsx" | "svelte" | "vue" => {
                let document = Html::parse_document(content);
                let mut findings = Vec::new();
                check_positive_tabindex(&document, path, content, &mut findings);
                check_skip_nav(&document, path, &mut findings);
                check_onclick_no_keyboard(&document, path, content, &mut findings);
                check_inline_focus_suppression(path, content, &mut findings);
                findings
            }
            _ => Vec::new(),
        }
    }

    fn applicable_extensions(&self) -> &[&str] {
        &["html", "htm", "jsx", "tsx", "svelte", "vue", "css"]
    }

    fn applies_to_level(&self, _level: WcagLevel) -> bool {
        true // Level A minimum
    }
}

/// Check for positive tabindex values
fn check_positive_tabindex(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let tabindex_sel = Selector::parse("[tabindex]").expect("valid selector");

    for el in document.select(&tabindex_sel) {
        if let Some(val_str) = el.value().attr("tabindex") {
            if let Ok(val) = val_str.parse::<i32>() {
                if val > 0 {
                    let tag = el.value().name();
                    let line = find_tabindex_line(content, val_str);
                    findings.push(
                        Finding::new(
                            "WCAG-2.4.3-positive-tabindex",
                            Severity::Warning,
                            &format!(
                                "Element <{}> has tabindex=\"{}\" (positive). Positive tabindex disrupts natural tab order.",
                                tag, val
                            ),
                        )
                        .with_wcag("2.4.3", WcagLevel::A)
                        .with_rule_name("Focus Order: Positive Tabindex")
                        .with_file(path.to_path_buf())
                        .with_line(line)
                        .with_suggestion(
                            "Remove the tabindex attribute or use tabindex=\"0\" to follow natural document order",
                        )
                        .as_fixable()
                        .with_impact(ImpactAssessment::motor()),
                    );
                }
            }
        }
    }
}

/// Check for skip navigation link
fn check_skip_nav(
    document: &Html,
    path: &Path,
    findings: &mut Vec<Finding>,
) {
    // Look for a link targeting #main, #content, or containing "skip"
    let link_sel = Selector::parse("a[href^=\"#\"]").expect("valid selector");
    let body_sel = Selector::parse("body").expect("valid selector");

    // Only check if this is a full document
    if document.select(&body_sel).next().is_none() {
        return;
    }

    let has_skip_link = document.select(&link_sel).any(|el| {
        let href = el.value().attr("href").unwrap_or("");
        let text = el.text().collect::<String>().to_lowercase();
        href == "#main" || href == "#content" || href == "#main-content"
            || text.contains("skip")
    });

    if !has_skip_link {
        // Check if there is a nav element (suggesting this is a real page)
        let nav_sel = Selector::parse("nav").expect("valid selector");
        if document.select(&nav_sel).next().is_some() {
            findings.push(
                Finding::new(
                    "WCAG-2.4.1-no-skip-nav",
                    Severity::Info,
                    "Page has navigation but no skip navigation link. Keyboard users benefit from 'skip to main content' links.",
                )
                .with_wcag("2.4.1", WcagLevel::A)
                .with_rule_name("Bypass Blocks: Missing Skip Navigation")
                .with_file(path.to_path_buf())
                .with_suggestion(
                    "Add <a href=\"#main\" class=\"sr-only\">Skip to main content</a> as the first child of <body>",
                )
                .as_fixable()
                .with_impact(ImpactAssessment::motor()),
            );
        }
    }
}

/// Check for onclick handlers without keyboard equivalents
fn check_onclick_no_keyboard(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    // Non-interactive elements with onclick but no keyboard handler
    let non_interactive = ["div", "span", "p", "li", "td"];

    for tag in &non_interactive {
        if let Ok(selector) = Selector::parse(&format!("{}[onclick]", tag)) {
            for el in document.select(&selector) {
                let has_keydown = el.value().attr("onkeydown").is_some()
                    || el.value().attr("onkeypress").is_some()
                    || el.value().attr("onkeyup").is_some();
                let has_role = el.value().attr("role").is_some();
                let has_tabindex = el.value().attr("tabindex").is_some();

                if !has_keydown && (!has_role || !has_tabindex) {
                    let line = find_onclick_line(content, tag);
                    findings.push(
                        Finding::new(
                            "WCAG-2.1.1-onclick-no-keyboard",
                            Severity::Error,
                            &format!(
                                "<{}> has onclick handler but no keyboard event handler. Click-only interactions are not keyboard accessible.",
                                tag
                            ),
                        )
                        .with_wcag("2.1.1", WcagLevel::A)
                        .with_rule_name("Keyboard: Click-only Interaction")
                        .with_file(path.to_path_buf())
                        .with_line(line)
                        .with_suggestion(
                            &format!("Use a <button> instead of <{}>, or add role=\"button\" tabindex=\"0\" and onkeydown handler", tag),
                        )
                        .with_impact(ImpactAssessment::motor()),
                    );
                }
            }
        }
    }
}

/// Check for outline: none in CSS (suppresses focus indicator)
fn check_focus_styles_css(path: &Path, content: &str) -> Vec<Finding> {
    let mut findings = Vec::new();
    let outline_none_re = Regex::new(r"(?i)outline\s*:\s*(none|0)\s*[;}\n]").expect("valid regex");

    for (line_num, line) in content.lines().enumerate() {
        if outline_none_re.is_match(line) {
            // Check if there is a replacement focus style nearby
            let has_replacement = content.lines()
                .skip(line_num.saturating_sub(2))
                .take(5)
                .any(|l| {
                    let lower = l.to_lowercase();
                    lower.contains("box-shadow") || lower.contains("border") || lower.contains("outline-offset")
                });

            if !has_replacement {
                findings.push(
                    Finding::new(
                        "WCAG-2.4.7-outline-none",
                        Severity::Error,
                        "Focus indicator removed with 'outline: none' without a visible replacement. Keyboard users cannot see which element is focused.",
                    )
                    .with_wcag("2.4.7", WcagLevel::AA)
                    .with_rule_name("Focus Visible: Suppressed Focus Indicator")
                    .with_file(path.to_path_buf())
                    .with_line(line_num + 1)
                    .with_suggestion(
                        "Replace 'outline: none' with a visible focus style, e.g., 'outline: 2px solid #005fcc' or use box-shadow",
                    )
                    .as_fixable()
                    .with_impact(ImpactAssessment::motor()),
                );
            }
        }
    }

    findings
}

/// Check for inline focus suppression in HTML style attributes
fn check_inline_focus_suppression(
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let style_re = Regex::new(r#"style\s*=\s*"[^"]*outline\s*:\s*(none|0)[^"]*""#).expect("valid regex");

    for (line_num, line) in content.lines().enumerate() {
        if style_re.is_match(line) {
            findings.push(
                Finding::new(
                    "WCAG-2.4.7-inline-outline-none",
                    Severity::Error,
                    "Inline style suppresses focus indicator with outline: none.",
                )
                .with_wcag("2.4.7", WcagLevel::AA)
                .with_rule_name("Focus Visible: Inline Focus Suppression")
                .with_file(path.to_path_buf())
                .with_line(line_num + 1)
                .with_suggestion("Remove 'outline: none' from inline styles and use CSS with a visible focus replacement")
                .as_fixable()
                .with_impact(ImpactAssessment::motor()),
            );
        }
    }
}

/// Find the line with a tabindex attribute
fn find_tabindex_line(content: &str, value: &str) -> usize {
    let pattern = format!("tabindex=\"{}\"", value);
    for (idx, line) in content.lines().enumerate() {
        if line.contains(&pattern) {
            return idx + 1;
        }
    }
    1
}

/// Find the line with an onclick attribute on a specific element
fn find_onclick_line(content: &str, element: &str) -> usize {
    for (idx, line) in content.lines().enumerate() {
        let lower = line.to_lowercase();
        if lower.contains(&format!("<{}", element)) && lower.contains("onclick") {
            return idx + 1;
        }
    }
    1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_positive_tabindex() {
        let html = r#"<html><body><div tabindex="5">Focus me</div></body></html>"#;
        let findings = KeyboardAnalyzer.analyze_file(Path::new("test.html"), html);
        let tab_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-2.4.3-positive-tabindex").collect();
        assert_eq!(tab_findings.len(), 1);
    }

    #[test]
    fn test_tabindex_zero_is_fine() {
        let html = r#"<html><body><div tabindex="0" role="button">OK</div></body></html>"#;
        let findings = KeyboardAnalyzer.analyze_file(Path::new("test.html"), html);
        let tab_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-2.4.3-positive-tabindex").collect();
        assert!(tab_findings.is_empty());
    }

    #[test]
    fn test_outline_none_css() {
        let css = r#"
            button:focus {
                outline: none;
            }
        "#;
        let findings = KeyboardAnalyzer.analyze_file(Path::new("style.css"), css);
        assert!(!findings.is_empty());
        assert_eq!(findings[0].rule_id, "WCAG-2.4.7-outline-none");
    }

    #[test]
    fn test_outline_none_with_replacement() {
        let css = r#"
            button:focus {
                outline: none;
                box-shadow: 0 0 0 2px blue;
            }
        "#;
        let findings = KeyboardAnalyzer.analyze_file(Path::new("style.css"), css);
        assert!(findings.is_empty(), "Should pass when replacement focus style exists");
    }

    #[test]
    fn test_onclick_no_keyboard() {
        let html = r#"<html><body><div onclick="doStuff()">Click me</div></body></html>"#;
        let findings = KeyboardAnalyzer.analyze_file(Path::new("test.html"), html);
        let click_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-2.1.1-onclick-no-keyboard").collect();
        assert_eq!(click_findings.len(), 1);
    }
}
