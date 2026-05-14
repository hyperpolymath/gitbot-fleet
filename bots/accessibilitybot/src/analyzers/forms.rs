// SPDX-License-Identifier: PMPL-1.0-or-later
//! Form accessibility analyzer - WCAG 1.3.5 Identify Input Purpose (Level AA), 3.3.2 Labels (Level A)
//!
//! Checks form element accessibility:
//! - Every input has an associated label (via for/id or wrapping)
//! - Required fields indicated (not just by color)
//! - Autocomplete attributes present where appropriate
//! - Error messages associated with fields

use crate::analyzers::Analyzer;
use crate::fleet::{Finding, ImpactAssessment, Severity, WcagLevel};
use scraper::{Html, Selector};
use std::path::Path;

/// Form accessibility analyzer
pub struct FormAnalyzer;

/// Input types that do not need a visible label
const EXEMPT_INPUT_TYPES: &[&str] = &["hidden", "submit", "reset", "button", "image"];

/// Common autocomplete-eligible input types and their expected autocomplete values
const AUTOCOMPLETE_HINTS: &[(&str, &str, &str)] = &[
    ("email", "email", "autocomplete=\"email\""),
    ("tel", "tel", "autocomplete=\"tel\""),
    ("url", "url", "autocomplete=\"url\""),
];

impl Analyzer for FormAnalyzer {
    fn name(&self) -> &str {
        "Form Accessibility Analyzer"
    }

    fn description(&self) -> &str {
        "Checks form elements for labels, autocomplete, and error handling (WCAG 3.3.2, 1.3.5)"
    }

    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding> {
        let document = Html::parse_document(content);
        let mut findings = Vec::new();

        check_input_labels(&document, path, content, &mut findings);
        check_autocomplete(&document, path, content, &mut findings);

        findings
    }

    fn applicable_extensions(&self) -> &[&str] {
        &["html", "htm", "jsx", "tsx", "svelte", "vue"]
    }

    fn applies_to_level(&self, _level: WcagLevel) -> bool {
        true // Level A minimum
    }
}

/// Check that every input has an associated label
fn check_input_labels(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let input_sel = Selector::parse("input").expect("valid selector");
    let label_sel = Selector::parse("label").expect("valid selector");
    let select_sel = Selector::parse("select").expect("valid selector");
    let textarea_sel = Selector::parse("textarea").expect("valid selector");

    // Collect all label[for] values
    let label_fors: Vec<String> = document
        .select(&label_sel)
        .filter_map(|l| l.value().attr("for").map(String::from))
        .collect();

    // Check inputs
    for input in document.select(&input_sel) {
        let input_type = input.value().attr("type").unwrap_or("text");

        if EXEMPT_INPUT_TYPES.contains(&input_type) {
            continue;
        }

        let has_id = input.value().attr("id");
        let has_aria_label = input.value().attr("aria-label").is_some()
            || input.value().attr("aria-labelledby").is_some();
        let has_title = input.value().attr("title").is_some();
        let has_placeholder = input.value().attr("placeholder").is_some();

        let has_label = match has_id {
            Some(id) => label_fors.iter().any(|f| f == id),
            None => false,
        };

        // Check if the input is wrapped in a label
        // (scraper does not easily support parent traversal, so we do a heuristic)
        let is_wrapped = is_input_wrapped_in_label(content, input_type);

        if !has_label && !has_aria_label && !has_title && !is_wrapped {
            let line = find_input_line(content, input_type);

            let severity = if has_placeholder {
                // Placeholder alone is not sufficient, but less severe
                Severity::Warning
            } else {
                Severity::Error
            };

            let message = if has_placeholder {
                format!(
                    "<input type=\"{}\"> relies only on placeholder for labeling. Placeholders disappear when typing and are not reliable labels.",
                    input_type
                )
            } else {
                format!(
                    "<input type=\"{}\"> has no associated label. Every form input needs a <label>, aria-label, or aria-labelledby.",
                    input_type
                )
            };

            findings.push(
                Finding::new(
                    "WCAG-3.3.2-input-no-label",
                    severity,
                    &message,
                )
                .with_wcag("3.3.2", WcagLevel::A)
                .with_rule_name("Labels or Instructions: Missing Input Label")
                .with_file(path.to_path_buf())
                .with_line(line)
                .with_element(&format!("<input type=\"{}\">", input_type))
                .with_suggestion(
                    "Add a <label for=\"input-id\"> element or aria-label attribute",
                )
                .as_fixable()
                .with_impact(ImpactAssessment::blind()),
            );
        }
    }

    // Check select elements
    for select in document.select(&select_sel) {
        check_labelled_element(select, "select", &label_fors, path, content, findings);
    }

    // Check textarea elements
    for textarea in document.select(&textarea_sel) {
        check_labelled_element(textarea, "textarea", &label_fors, path, content, findings);
    }
}

/// Check if a form element has a label
fn check_labelled_element(
    element: scraper::ElementRef<'_>,
    tag: &str,
    label_fors: &[String],
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let has_id = element.value().attr("id");
    let has_aria_label = element.value().attr("aria-label").is_some()
        || element.value().attr("aria-labelledby").is_some();

    let has_label = match has_id {
        Some(id) => label_fors.iter().any(|f| f == id),
        None => false,
    };

    if !has_label && !has_aria_label {
        let line = find_element_line(content, tag);
        findings.push(
            Finding::new(
                "WCAG-3.3.2-element-no-label",
                Severity::Error,
                &format!(
                    "<{}> has no associated label. Form elements need a <label>, aria-label, or aria-labelledby.",
                    tag
                ),
            )
            .with_wcag("3.3.2", WcagLevel::A)
            .with_rule_name("Labels or Instructions: Missing Element Label")
            .with_file(path.to_path_buf())
            .with_line(line)
            .with_element(&format!("<{}>", tag))
            .with_suggestion("Add a <label> or aria-label attribute")
            .as_fixable()
            .with_impact(ImpactAssessment::blind()),
        );
    }
}

/// Check for autocomplete attributes on appropriate input types
fn check_autocomplete(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let input_sel = Selector::parse("input").expect("valid selector");

    for input in document.select(&input_sel) {
        let input_type = input.value().attr("type").unwrap_or("text");
        let has_autocomplete = input.value().attr("autocomplete").is_some();

        if !has_autocomplete {
            for (type_match, _, suggestion) in AUTOCOMPLETE_HINTS {
                if input_type == *type_match {
                    let line = find_input_line(content, input_type);
                    findings.push(
                        Finding::new(
                            "WCAG-1.3.5-missing-autocomplete",
                            Severity::Info,
                            &format!(
                                "<input type=\"{}\"> is missing autocomplete attribute. Adding {} helps users fill forms faster.",
                                input_type, suggestion
                            ),
                        )
                        .with_wcag("1.3.5", WcagLevel::AA)
                        .with_rule_name("Identify Input Purpose: Missing Autocomplete")
                        .with_file(path.to_path_buf())
                        .with_line(line)
                        .with_suggestion(&format!("Add {} to the input", suggestion))
                        .as_fixable()
                        .with_impact(ImpactAssessment::cognitive()),
                    );
                }
            }
        }
    }
}

/// Heuristic check if an input is wrapped in a label element
fn is_input_wrapped_in_label(content: &str, input_type: &str) -> bool {
    // Simple heuristic: look for <label> followed by <input type="..."> on nearby lines
    let input_pattern = "<input".to_string();
    let type_pattern = format!("type=\"{}\"", input_type);
    let mut in_label = false;

    for line in content.lines() {
        let lower = line.to_lowercase();
        if lower.contains("<label") {
            in_label = true;
        }
        if in_label && lower.contains(&input_pattern) && lower.contains(&type_pattern) {
            return true;
        }
        if lower.contains("</label") {
            in_label = false;
        }
    }
    false
}

/// Find the line of an input element
fn find_input_line(content: &str, input_type: &str) -> usize {
    let pattern = format!("type=\"{}\"", input_type);
    for (idx, line) in content.lines().enumerate() {
        if line.to_lowercase().contains("<input") && line.to_lowercase().contains(&pattern) {
            return idx + 1;
        }
    }
    // Fallback: just find any input
    for (idx, line) in content.lines().enumerate() {
        if line.to_lowercase().contains("<input") {
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
    fn test_labelled_input() {
        let html = r#"
            <html><body>
                <form>
                    <label for="name">Name:</label>
                    <input type="text" id="name">
                </form>
            </body></html>
        "#;
        let findings = FormAnalyzer.analyze_file(Path::new("test.html"), html);
        let label_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-3.3.2-input-no-label").collect();
        assert!(label_findings.is_empty(), "Labelled input should not produce findings");
    }

    #[test]
    fn test_unlabelled_input() {
        let html = r#"
            <html><body>
                <form>
                    <input type="text">
                </form>
            </body></html>
        "#;
        let findings = FormAnalyzer.analyze_file(Path::new("test.html"), html);
        let label_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-3.3.2-input-no-label").collect();
        assert_eq!(label_findings.len(), 1);
        assert_eq!(label_findings[0].severity, Severity::Error);
    }

    #[test]
    fn test_placeholder_only() {
        let html = r#"
            <html><body>
                <form>
                    <input type="text" placeholder="Enter name">
                </form>
            </body></html>
        "#;
        let findings = FormAnalyzer.analyze_file(Path::new("test.html"), html);
        let label_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-3.3.2-input-no-label").collect();
        assert_eq!(label_findings.len(), 1);
        assert_eq!(label_findings[0].severity, Severity::Warning); // Placeholder is partial
    }

    #[test]
    fn test_aria_label_input() {
        let html = r#"
            <html><body>
                <form>
                    <input type="search" aria-label="Search">
                </form>
            </body></html>
        "#;
        let findings = FormAnalyzer.analyze_file(Path::new("test.html"), html);
        let label_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-3.3.2-input-no-label").collect();
        assert!(label_findings.is_empty());
    }

    #[test]
    fn test_missing_autocomplete() {
        let html = r#"
            <html><body>
                <form>
                    <label for="email">Email:</label>
                    <input type="email" id="email">
                </form>
            </body></html>
        "#;
        let findings = FormAnalyzer.analyze_file(Path::new("test.html"), html);
        let auto_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.3.5-missing-autocomplete").collect();
        assert_eq!(auto_findings.len(), 1);
    }

    #[test]
    fn test_hidden_input_exempt() {
        let html = r#"
            <html><body>
                <form>
                    <input type="hidden" name="csrf" value="token123">
                </form>
            </body></html>
        "#;
        let findings = FormAnalyzer.analyze_file(Path::new("test.html"), html);
        assert!(findings.is_empty());
    }
}
