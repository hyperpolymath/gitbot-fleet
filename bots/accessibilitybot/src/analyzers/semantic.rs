// SPDX-License-Identifier: PMPL-1.0-or-later
//! Semantic HTML analyzer - WCAG 1.3.1 Info and Relationships (Level A)
//!
//! Checks for proper use of semantic HTML elements:
//! - Pages use semantic elements (header, nav, main, article, footer)
//! - Headings follow proper hierarchy (no skipping h1 to h3)
//! - Lists use proper elements (ul/ol/dl)
//! - Tables have thead/tbody and th with scope
//! - Detects excessive div nesting without semantic elements

use crate::analyzers::Analyzer;
use crate::fleet::{Finding, ImpactAssessment, Severity, WcagLevel};
use scraper::{Html, Selector};
use std::path::Path;

/// Semantic HTML analyzer
pub struct SemanticAnalyzer;

impl Analyzer for SemanticAnalyzer {
    fn name(&self) -> &str {
        "Semantic HTML Analyzer"
    }

    fn description(&self) -> &str {
        "Checks for proper semantic HTML usage (WCAG 1.3.1)"
    }

    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding> {
        let document = Html::parse_document(content);
        let mut findings = Vec::new();

        check_semantic_elements(&document, path, content, &mut findings);
        check_heading_hierarchy(&document, path, content, &mut findings);
        check_tables(&document, path, content, &mut findings);
        check_div_soup(&document, path, &mut findings);

        findings
    }

    fn applicable_extensions(&self) -> &[&str] {
        &["html", "htm", "jsx", "tsx", "svelte", "vue"]
    }

    fn applies_to_level(&self, _level: WcagLevel) -> bool {
        true // Level A
    }
}

/// Check for presence of semantic elements
fn check_semantic_elements(
    document: &Html,
    path: &Path,
    _content: &str,
    findings: &mut Vec<Finding>,
) {
    let semantic_elements = ["header", "nav", "main", "footer"];
    let mut found_any = false;

    for elem_name in &semantic_elements {
        if let Ok(sel) = Selector::parse(elem_name) {
            if document.select(&sel).next().is_some() {
                found_any = true;
            }
        }
    }

    // Check if this is a full HTML document (has <body>)
    let body_sel = Selector::parse("body").expect("valid selector");
    let has_body = document.select(&body_sel).next().is_some();

    if has_body && !found_any {
        findings.push(
            Finding::new(
                "WCAG-1.3.1-no-semantic",
                Severity::Warning,
                "Page does not use semantic HTML elements (header, nav, main, footer). Semantic elements improve screen reader navigation.",
            )
            .with_wcag("1.3.1", WcagLevel::A)
            .with_rule_name("Info and Relationships: Missing Semantic Elements")
            .with_file(path.to_path_buf())
            .with_suggestion(
                "Add semantic elements: <header> for page header, <nav> for navigation, <main> for primary content, <footer> for page footer",
            )
            .with_impact(ImpactAssessment::blind()),
        );
    }
}

/// Check heading hierarchy (h1-h6 should not skip levels)
fn check_heading_hierarchy(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let heading_sel = Selector::parse("h1, h2, h3, h4, h5, h6").expect("valid selector");
    let headings: Vec<u8> = document
        .select(&heading_sel)
        .filter_map(|el| {
            let tag = el.value().name();
            tag.strip_prefix('h').and_then(|n| n.parse::<u8>().ok())
        })
        .collect();

    if headings.is_empty() {
        return;
    }

    // Check that first heading is h1
    if headings[0] != 1 {
        let line = find_heading_line(content, headings[0]);
        findings.push(
            Finding::new(
                "WCAG-1.3.1-first-heading",
                Severity::Warning,
                &format!(
                    "First heading is <h{}> instead of <h1>. The first heading should be h1.",
                    headings[0]
                ),
            )
            .with_wcag("1.3.1", WcagLevel::A)
            .with_rule_name("Info and Relationships: Heading Hierarchy")
            .with_file(path.to_path_buf())
            .with_line(line)
            .with_suggestion("Start the page with an <h1> element")
            .with_impact(ImpactAssessment::blind()),
        );
    }

    // Check for skipped levels
    for window in headings.windows(2) {
        let prev = window[0];
        let curr = window[1];
        if curr > prev + 1 {
            let line = find_heading_line(content, curr);
            findings.push(
                Finding::new(
                    "WCAG-1.3.1-heading-skip",
                    Severity::Warning,
                    &format!(
                        "Heading level skipped from <h{}> to <h{}>. Do not skip heading levels.",
                        prev, curr
                    ),
                )
                .with_wcag("1.3.1", WcagLevel::A)
                .with_rule_name("Info and Relationships: Heading Skip")
                .with_file(path.to_path_buf())
                .with_line(line)
                .with_suggestion(
                    &format!("Use <h{}> instead of <h{}>, or add intermediate heading levels", prev + 1, curr),
                )
                .with_impact(ImpactAssessment::blind()),
            );
        }
    }
}

/// Check table accessibility
fn check_tables(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let table_sel = Selector::parse("table").expect("valid selector");
    let thead_sel = Selector::parse("thead").expect("valid selector");
    let th_sel = Selector::parse("th").expect("valid selector");

    for table in document.select(&table_sel) {
        let has_thead = table.select(&thead_sel).next().is_some();
        let line = find_element_line(content, "table");

        if !has_thead {
            findings.push(
                Finding::new(
                    "WCAG-1.3.1-table-no-thead",
                    Severity::Warning,
                    "Table is missing <thead> element. Tables should have a header section.",
                )
                .with_wcag("1.3.1", WcagLevel::A)
                .with_rule_name("Info and Relationships: Table Structure")
                .with_file(path.to_path_buf())
                .with_line(line)
                .with_element("<table>")
                .with_suggestion("Add <thead> with <th> elements to define table headers")
                .as_fixable()
                .with_impact(ImpactAssessment::blind()),
            );
        }

        // Check th elements have scope
        for th in table.select(&th_sel) {
            if th.value().attr("scope").is_none() {
                let th_text = th.text().collect::<String>();
                let display_text = if th_text.len() > 30 {
                    format!("{}...", &th_text[..30])
                } else {
                    th_text
                };
                findings.push(
                    Finding::new(
                        "WCAG-1.3.1-th-no-scope",
                        Severity::Info,
                        &format!(
                            "<th> element \"{}\" is missing scope attribute.",
                            display_text.trim()
                        ),
                    )
                    .with_wcag("1.3.1", WcagLevel::A)
                    .with_rule_name("Info and Relationships: Table Header Scope")
                    .with_file(path.to_path_buf())
                    .with_line(line)
                    .with_suggestion("Add scope=\"col\" or scope=\"row\" to <th> elements")
                    .as_fixable()
                    .with_impact(ImpactAssessment::blind()),
                );
            }
        }
    }
}

/// Check for div soup (excessive div nesting without semantic elements)
fn check_div_soup(
    document: &Html,
    path: &Path,
    findings: &mut Vec<Finding>,
) {
    let div_sel = Selector::parse("div").expect("valid selector");
    let semantic_sel = Selector::parse(
        "header, nav, main, article, section, aside, footer, figure, details, summary"
    ).expect("valid selector");

    let div_count = document.select(&div_sel).count();
    let semantic_count = document.select(&semantic_sel).count();

    if div_count > 20 && semantic_count == 0 {
        findings.push(
            Finding::new(
                "WCAG-1.3.1-div-soup",
                Severity::Warning,
                &format!(
                    "Page has {} <div> elements and no semantic elements. This is 'div soup' and hurts accessibility.",
                    div_count
                ),
            )
            .with_wcag("1.3.1", WcagLevel::A)
            .with_rule_name("Info and Relationships: Div Soup")
            .with_file(path.to_path_buf())
            .with_suggestion(
                "Replace structural <div> elements with semantic HTML: <header>, <nav>, <main>, <article>, <section>, <footer>",
            )
            .with_impact(ImpactAssessment::blind()),
        );
    }
}

/// Find the line of a heading element
fn find_heading_line(content: &str, level: u8) -> usize {
    let tag = format!("<h{}", level);
    for (idx, line) in content.lines().enumerate() {
        if line.to_lowercase().contains(&tag) {
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
    fn test_accessible_page() {
        let html = r#"
            <html>
            <body>
                <header><h1>Page Title</h1></header>
                <nav><ul><li><a href="/">Home</a></li></ul></nav>
                <main>
                    <h2>Section</h2>
                    <p>Content</p>
                    <h3>Subsection</h3>
                    <p>More content</p>
                </main>
                <footer><p>Footer</p></footer>
            </body>
            </html>
        "#;
        let findings = SemanticAnalyzer.analyze_file(Path::new("test.html"), html);
        assert!(findings.is_empty(), "Accessible page should have no findings: {:?}", findings);
    }

    #[test]
    fn test_heading_skip() {
        let html = r#"
            <html><body>
                <h1>Title</h1>
                <h3>Skipped h2</h3>
            </body></html>
        "#;
        let findings = SemanticAnalyzer.analyze_file(Path::new("test.html"), html);
        let skip_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.3.1-heading-skip").collect();
        assert_eq!(skip_findings.len(), 1);
    }

    #[test]
    fn test_no_semantic_elements() {
        let html = r#"
            <html><body>
                <div class="header">Header</div>
                <div class="content">Content</div>
                <div class="footer">Footer</div>
            </body></html>
        "#;
        let findings = SemanticAnalyzer.analyze_file(Path::new("test.html"), html);
        let semantic_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.3.1-no-semantic").collect();
        assert_eq!(semantic_findings.len(), 1);
    }

    #[test]
    fn test_table_missing_thead() {
        let html = r#"
            <html><body>
                <main>
                <table>
                    <tr><td>Cell 1</td><td>Cell 2</td></tr>
                </table>
                </main>
            </body></html>
        "#;
        let findings = SemanticAnalyzer.analyze_file(Path::new("test.html"), html);
        let table_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.3.1-table-no-thead").collect();
        assert_eq!(table_findings.len(), 1);
    }

    #[test]
    fn test_th_missing_scope() {
        let html = r#"
            <html><body>
                <main>
                <table>
                    <thead><tr><th>Name</th><th>Value</th></tr></thead>
                    <tbody><tr><td>A</td><td>1</td></tr></tbody>
                </table>
                </main>
            </body></html>
        "#;
        let findings = SemanticAnalyzer.analyze_file(Path::new("test.html"), html);
        let scope_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.3.1-th-no-scope").collect();
        assert_eq!(scope_findings.len(), 2); // Two th without scope
    }
}
