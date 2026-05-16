// SPDX-License-Identifier: PMPL-1.0-or-later
//! ARIA validator - WCAG 4.1.2 Name, Role, Value (Level A)
//!
//! Validates correct use of ARIA attributes:
//! - ARIA roles match element semantics
//! - Required ARIA attributes are present
//! - aria-hidden not on focusable elements
//! - No redundant ARIA on native semantic elements

use crate::analyzers::Analyzer;
use crate::fleet::{Finding, ImpactAssessment, Severity, WcagLevel};
use scraper::{Html, Selector};
use std::path::Path;

/// ARIA validator analyzer
pub struct AriaAnalyzer;

/// Elements that have implicit ARIA roles (redundant to add explicitly)
const REDUNDANT_ROLES: &[(&str, &str)] = &[
    ("nav", "navigation"),
    ("main", "main"),
    ("header", "banner"),
    ("footer", "contentinfo"),
    ("aside", "complementary"),
    ("form", "form"),
    ("button", "button"),
    ("a", "link"),
    ("input", "textbox"),
    ("select", "listbox"),
    ("textarea", "textbox"),
    ("table", "table"),
    ("ul", "list"),
    ("ol", "list"),
    ("article", "article"),
    ("section", "region"),
];

/// Interactive/focusable element names
const FOCUSABLE_ELEMENTS: &[&str] = &[
    "a", "button", "input", "select", "textarea", "details", "summary",
];

impl Analyzer for AriaAnalyzer {
    fn name(&self) -> &str {
        "ARIA Validator"
    }

    fn description(&self) -> &str {
        "Validates ARIA attributes and roles (WCAG 4.1.2)"
    }

    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding> {
        let document = Html::parse_document(content);
        let mut findings = Vec::new();

        check_redundant_roles(&document, path, content, &mut findings);
        check_aria_hidden_focusable(&document, path, content, &mut findings);
        check_icon_buttons(&document, path, content, &mut findings);
        check_div_button_pattern(&document, path, content, &mut findings);

        findings
    }

    fn applicable_extensions(&self) -> &[&str] {
        &["html", "htm", "jsx", "tsx", "svelte", "vue"]
    }

    fn applies_to_level(&self, _level: WcagLevel) -> bool {
        true // Level A
    }
}

/// Check for redundant ARIA roles on elements that have implicit roles
fn check_redundant_roles(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    for (element_name, role_name) in REDUNDANT_ROLES {
        if let Ok(selector) = Selector::parse(&format!("{}[role=\"{}\"]", element_name, role_name)) {
            for _el in document.select(&selector) {
                let line = find_attr_line(content, element_name, "role", role_name);
                findings.push(
                    Finding::new(
                        "WCAG-4.1.2-redundant-role",
                        Severity::Warning,
                        &format!(
                            "Redundant role=\"{}\" on <{}> element. The element already has this implicit role.",
                            role_name, element_name
                        ),
                    )
                    .with_wcag("4.1.2", WcagLevel::A)
                    .with_category("accessibility/aria")
                    .with_rule_name("Name, Role, Value: Redundant ARIA Role")
                    .with_file(path.to_path_buf())
                    .with_line(line)
                    .with_element(&format!("<{} role=\"{}\">", element_name, role_name))
                    .with_suggestion(
                        &format!("Remove the role=\"{}\" attribute; <{}> already has this role", role_name, element_name),
                    )
                    .with_impact(ImpactAssessment::blind()),
                );
            }
        }
    }
}

/// Check for aria-hidden="true" on focusable elements
fn check_aria_hidden_focusable(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    for elem_name in FOCUSABLE_ELEMENTS {
        if let Ok(selector) = Selector::parse(&format!("{}[aria-hidden=\"true\"]", elem_name)) {
            for _el in document.select(&selector) {
                let line = find_attr_line(content, elem_name, "aria-hidden", "true");
                findings.push(
                    Finding::new(
                        "WCAG-4.1.2-aria-hidden-focusable",
                        Severity::Error,
                        &format!(
                            "aria-hidden=\"true\" is set on a focusable <{}> element. This hides it from screen readers while it remains keyboard-focusable.",
                            elem_name
                        ),
                    )
                    .with_wcag("4.1.2", WcagLevel::A)
                    .with_category("accessibility/aria")
                    .with_rule_name("Name, Role, Value: Hidden Focusable Element")
                    .with_file(path.to_path_buf())
                    .with_line(line)
                    .with_suggestion(
                        "Either remove aria-hidden=\"true\" or add tabindex=\"-1\" to remove from tab order",
                    )
                    .with_impact(ImpactAssessment::blind()),
                );
            }
        }
    }
}

/// Check for icon-only buttons without aria-label
fn check_icon_buttons(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let button_sel = Selector::parse("button").expect("valid selector");

    for button in document.select(&button_sel) {
        let text = button.text().collect::<String>();
        let text_trimmed = text.trim();
        let has_aria_label = button.value().attr("aria-label").is_some()
            || button.value().attr("aria-labelledby").is_some()
            || button.value().attr("title").is_some();

        // If button has no visible text and no ARIA label
        if text_trimmed.is_empty() && !has_aria_label {
            let line = find_element_line(content, "button");
            findings.push(
                Finding::new(
                    "WCAG-4.1.2-button-no-label",
                    Severity::Error,
                    "Button has no accessible name. It has no text content, aria-label, or aria-labelledby.",
                )
                .with_wcag("4.1.2", WcagLevel::A)
                .with_category("accessibility/aria")
                .with_rule_name("Name, Role, Value: Button Without Label")
                .with_file(path.to_path_buf())
                .with_line(line)
                .with_element("<button>")
                .with_suggestion(
                    "Add aria-label=\"description\" to the button, or add visible text content",
                )
                .as_fixable()
                .with_impact(ImpactAssessment::blind()),
            );
        }
    }
}

/// Check for div/span with role="button" when <button> would work
fn check_div_button_pattern(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let selector_str = "div[role=\"button\"], span[role=\"button\"]";
    if let Ok(selector) = Selector::parse(selector_str) {
        for el in document.select(&selector) {
            let tag = el.value().name();
            let line = find_attr_line(content, tag, "role", "button");
            findings.push(
                Finding::new(
                    "WCAG-4.1.2-div-as-button",
                    Severity::Warning,
                    &format!(
                        "<{}> with role=\"button\" should be a native <button> element for proper keyboard interaction and screen reader support.",
                        tag
                    ),
                )
                .with_wcag("4.1.2", WcagLevel::A)
                .with_category("accessibility/aria")
                .with_rule_name("Name, Role, Value: Div as Button")
                .with_file(path.to_path_buf())
                .with_line(line)
                .with_suggestion(
                    &format!("Replace <{} role=\"button\"> with a native <button> element", tag),
                )
                .with_impact(ImpactAssessment { blind: true, low_vision: false, motor: true, cognitive: false, deaf: false }),
            );
        }
    }
}

/// Find line number of an element with a specific attribute value
fn find_attr_line(content: &str, element: &str, attr: &str, value: &str) -> usize {
    let pattern = attr.to_string();
    let value_pattern = format!("\"{}\"", value);
    for (idx, line) in content.lines().enumerate() {
        let lower = line.to_lowercase();
        if lower.contains(&format!("<{}", element)) && lower.contains(&pattern) && lower.contains(&value_pattern) {
            return idx + 1;
        }
    }
    // Fallback: search for just the attribute
    for (idx, line) in content.lines().enumerate() {
        if line.contains(attr) && line.contains(value) {
            return idx + 1;
        }
    }
    1
}

/// Find the line of an element
fn find_element_line(content: &str, element: &str) -> usize {
    let tag = format!("<{}", element);
    for (idx, line) in content.lines().enumerate() {
        if line.to_lowercase().contains(&tag) {
            return idx + 1;
        }
    }
    1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_issues() {
        let html = r#"
            <html><body>
                <nav><ul><li><a href="/">Home</a></li></ul></nav>
                <main>
                    <button aria-label="Close">X</button>
                </main>
            </body></html>
        "#;
        let findings = AriaAnalyzer.analyze_file(Path::new("test.html"), html);
        assert!(findings.is_empty(), "Expected no findings: {:?}", findings);
    }

    #[test]
    fn test_redundant_role() {
        let html = r#"<html><body><nav role="navigation">Links</nav></body></html>"#;
        let findings = AriaAnalyzer.analyze_file(Path::new("test.html"), html);
        let redundant: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-4.1.2-redundant-role").collect();
        assert_eq!(redundant.len(), 1);
        assert_eq!(redundant[0].severity, Severity::Warning);
    }

    #[test]
    fn test_aria_hidden_focusable() {
        let html = r#"<html><body><button aria-hidden="true">Click</button></body></html>"#;
        let findings = AriaAnalyzer.analyze_file(Path::new("test.html"), html);
        let hidden: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-4.1.2-aria-hidden-focusable").collect();
        assert_eq!(hidden.len(), 1);
        assert_eq!(hidden[0].severity, Severity::Error);
    }

    #[test]
    fn test_icon_button_no_label() {
        let html = r#"<html><body><button><i class="icon-close"></i></button></body></html>"#;
        let findings = AriaAnalyzer.analyze_file(Path::new("test.html"), html);
        let no_label: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-4.1.2-button-no-label").collect();
        assert_eq!(no_label.len(), 1);
    }

    #[test]
    fn test_div_as_button() {
        let html = r#"<html><body><div role="button" onclick="doSomething()">Click Me</div></body></html>"#;
        let findings = AriaAnalyzer.analyze_file(Path::new("test.html"), html);
        let div_btn: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-4.1.2-div-as-button").collect();
        assert_eq!(div_btn.len(), 1);
    }

    #[test]
    fn test_button_with_aria_label() {
        let html = r#"<html><body><button aria-label="Close dialog"><i class="icon-x"></i></button></body></html>"#;
        let findings = AriaAnalyzer.analyze_file(Path::new("test.html"), html);
        let no_label: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-4.1.2-button-no-label").collect();
        assert!(no_label.is_empty());
    }
}
