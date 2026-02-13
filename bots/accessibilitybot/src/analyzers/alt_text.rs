// SPDX-License-Identifier: PMPL-1.0-or-later
//! Image alt text analyzer - WCAG 1.1.1 Non-text Content (Level A)
//!
//! Checks that all `<img>` elements have appropriate alt attributes:
//! - Every `<img>` must have an `alt` attribute (not missing)
//! - Alt text must be descriptive (not generic like "image", "photo")
//! - Decorative images should use `alt=""` (empty, not missing)

use crate::analyzers::Analyzer;
use crate::fleet::{Finding, ImpactAssessment, Severity, WcagLevel};
use scraper::{Html, Selector};
use std::path::Path;

/// Generic alt text values that indicate lazy/unhelpful descriptions
const GENERIC_ALT_VALUES: &[&str] = &[
    "image",
    "photo",
    "picture",
    "icon",
    "graphic",
    "img",
    "banner",
    "logo",
    "untitled",
    "screenshot",
    "thumbnail",
    "placeholder",
];

/// Analyzer for image alt text compliance
pub struct AltTextAnalyzer;

impl Analyzer for AltTextAnalyzer {
    fn name(&self) -> &str {
        "Alt Text Analyzer"
    }

    fn description(&self) -> &str {
        "Checks <img> elements for proper alt text (WCAG 1.1.1)"
    }

    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding> {
        let document = Html::parse_document(content);
        let img_selector = Selector::parse("img").expect("valid selector");
        let mut findings = Vec::new();

        for (idx, element) in document.select(&img_selector).enumerate() {
            let line = estimate_line(content, idx);
            let element_html = format!("<img{}>", element.value().attrs()
                .map(|(k, v)| format!(" {}=\"{}\"", k, v))
                .collect::<String>());

            match element.value().attr("alt") {
                None => {
                    // Missing alt attribute entirely
                    findings.push(
                        Finding::new(
                            "WCAG-1.1.1-missing-alt",
                            Severity::Error,
                            "Image is missing alt attribute. Every <img> must have an alt attribute.",
                        )
                        .with_wcag("1.1.1", WcagLevel::A)
                        .with_rule_name("Non-text Content: Missing Alt")
                        .with_file(path.to_path_buf())
                        .with_line(line)
                        .with_element(&element_html)
                        .with_suggestion(
                            "Add alt=\"description\" for informative images or alt=\"\" for decorative images",
                        )
                        .as_fixable()
                        .with_impact(ImpactAssessment::blind()),
                    );
                }
                Some(alt) if !alt.is_empty() => {
                    // Check for generic/unhelpful alt text
                    let alt_lower = alt.to_lowercase();
                    let alt_trimmed = alt_lower.trim();

                    if GENERIC_ALT_VALUES.contains(&alt_trimmed) {
                        findings.push(
                            Finding::new(
                                "WCAG-1.1.1-generic-alt",
                                Severity::Warning,
                                &format!(
                                    "Image has generic alt text \"{}\". Alt text should be descriptive.",
                                    alt
                                ),
                            )
                            .with_wcag("1.1.1", WcagLevel::A)
                            .with_rule_name("Non-text Content: Generic Alt Text")
                            .with_file(path.to_path_buf())
                            .with_line(line)
                            .with_element(&element_html)
                            .with_suggestion(
                                "Replace with a meaningful description of the image content",
                            )
                            .with_impact(ImpactAssessment::blind()),
                        );
                    }

                    // Check if alt text is just the filename
                    if alt_trimmed.contains('.') && (
                        alt_trimmed.ends_with(".jpg")
                        || alt_trimmed.ends_with(".png")
                        || alt_trimmed.ends_with(".gif")
                        || alt_trimmed.ends_with(".svg")
                        || alt_trimmed.ends_with(".webp")
                    ) {
                        findings.push(
                            Finding::new(
                                "WCAG-1.1.1-filename-alt",
                                Severity::Warning,
                                &format!(
                                    "Image alt text appears to be a filename: \"{}\". Use a descriptive alternative.",
                                    alt
                                ),
                            )
                            .with_wcag("1.1.1", WcagLevel::A)
                            .with_rule_name("Non-text Content: Filename as Alt Text")
                            .with_file(path.to_path_buf())
                            .with_line(line)
                            .with_element(&element_html)
                            .with_suggestion(
                                "Replace the filename with a meaningful description of what the image shows",
                            )
                            .with_impact(ImpactAssessment::blind()),
                        );
                    }
                }
                Some(_) => {
                    // alt="" is valid for decorative images -- no finding
                }
            }
        }

        findings
    }

    fn applicable_extensions(&self) -> &[&str] {
        &["html", "htm", "jsx", "tsx", "svelte", "vue"]
    }

    fn applies_to_level(&self, _level: WcagLevel) -> bool {
        true // Level A applies to all
    }
}

/// Estimate the line number of an element by counting occurrences of `<img`
fn estimate_line(content: &str, img_index: usize) -> usize {
    let mut count = 0;
    for (line_num, line) in content.lines().enumerate() {
        let lower = line.to_lowercase();
        let mut search_from = 0;
        while let Some(pos) = lower[search_from..].find("<img") {
            if count == img_index {
                return line_num + 1;
            }
            count += 1;
            search_from += pos + 4;
        }
    }
    1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_accessible_images() {
        let html = r#"
            <html>
            <body>
                <img src="logo.png" alt="Company logo">
                <img src="decorative.png" alt="">
                <img src="chart.png" alt="Bar chart showing Q4 revenue growth of 15%">
            </body>
            </html>
        "#;
        let findings = AltTextAnalyzer.analyze_file(Path::new("test.html"), html);
        assert!(findings.is_empty(), "Expected no findings for accessible images, got: {:?}", findings);
    }

    #[test]
    fn test_missing_alt() {
        let html = r#"
            <html>
            <body>
                <img src="photo.jpg">
            </body>
            </html>
        "#;
        let findings = AltTextAnalyzer.analyze_file(Path::new("test.html"), html);
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].severity, Severity::Error);
        assert_eq!(findings[0].rule_id, "WCAG-1.1.1-missing-alt");
    }

    #[test]
    fn test_generic_alt_text() {
        let html = r#"
            <html>
            <body>
                <img src="photo.jpg" alt="image">
                <img src="icon.svg" alt="icon">
            </body>
            </html>
        "#;
        let findings = AltTextAnalyzer.analyze_file(Path::new("test.html"), html);
        assert_eq!(findings.len(), 2);
        for f in &findings {
            assert_eq!(f.severity, Severity::Warning);
            assert_eq!(f.rule_id, "WCAG-1.1.1-generic-alt");
        }
    }

    #[test]
    fn test_filename_alt_text() {
        let html = r#"
            <html>
            <body>
                <img src="hero.jpg" alt="hero-banner.jpg">
            </body>
            </html>
        "#;
        let findings = AltTextAnalyzer.analyze_file(Path::new("test.html"), html);
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].rule_id, "WCAG-1.1.1-filename-alt");
    }

    #[test]
    fn test_empty_alt_is_valid_decorative() {
        let html = r#"<html><body><img src="divider.png" alt=""></body></html>"#;
        let findings = AltTextAnalyzer.analyze_file(Path::new("test.html"), html);
        assert!(findings.is_empty());
    }

    #[test]
    fn test_multiple_issues() {
        let html = r#"
            <html>
            <body>
                <img src="a.png">
                <img src="b.png" alt="photo">
                <img src="c.png" alt="Good description of content">
                <img src="d.png">
            </body>
            </html>
        "#;
        let findings = AltTextAnalyzer.analyze_file(Path::new("test.html"), html);
        assert_eq!(findings.len(), 3); // 2 missing + 1 generic
    }
}
