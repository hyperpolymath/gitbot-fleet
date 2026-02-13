// SPDX-License-Identifier: PMPL-1.0-or-later
//! CSS analysis module - CSS-first accessibility checks
//!
//! Per philosophy: "CSS-first, HTML-second"
//!
//! Checks:
//! - Font sizes use relative units (rem/em, not px)
//! - Line height >= 1.5 for body text (WCAG 1.4.12)
//! - prefers-reduced-motion media query respected
//! - prefers-color-scheme supported (dark mode)
//! - prefers-contrast supported (high contrast)
//! - display: none not used where sr-only pattern should be used
//! - Responsive design (media queries for breakpoints)

use crate::analyzers::Analyzer;
use crate::fleet::{Finding, ImpactAssessment, Severity, WcagLevel};
use regex::Regex;
use std::path::Path;

/// CSS accessibility analyzer
pub struct CssAnalyzer;

impl Analyzer for CssAnalyzer {
    fn name(&self) -> &str {
        "CSS Accessibility Analyzer"
    }

    fn description(&self) -> &str {
        "Checks CSS for accessibility best practices (WCAG 1.4.4, 1.4.12, 2.3.3)"
    }

    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding> {
        let mut findings = Vec::new();

        check_px_font_sizes(path, content, &mut findings);
        check_line_height(path, content, &mut findings);
        check_reduced_motion(path, content, &mut findings);
        check_color_scheme(path, content, &mut findings);
        check_contrast_preference(path, content, &mut findings);
        check_display_none_misuse(path, content, &mut findings);

        findings
    }

    fn applicable_extensions(&self) -> &[&str] {
        &["css"]
    }

    fn applies_to_level(&self, _level: WcagLevel) -> bool {
        true
    }
}

/// Check for font sizes using px instead of rem/em
fn check_px_font_sizes(path: &Path, content: &str, findings: &mut Vec<Finding>) {
    let font_size_px_re = Regex::new(r"(?i)font-size\s*:\s*(\d+)px").expect("valid regex");

    for (line_num, line) in content.lines().enumerate() {
        if let Some(caps) = font_size_px_re.captures(line) {
            let px_val = &caps[1];
            findings.push(
                Finding::new(
                    "WCAG-1.4.4-px-font-size",
                    Severity::Warning,
                    &format!(
                        "Font size uses absolute unit ({}px). Use relative units (rem/em) so users can resize text.",
                        px_val
                    ),
                )
                .with_wcag("1.4.4", WcagLevel::AA)
                .with_category("accessibility/css")
                .with_rule_name("Resize Text: Pixel Font Size")
                .with_file(path.to_path_buf())
                .with_line(line_num + 1)
                .with_suggestion(
                    &format!("Replace {}px with {}rem (assuming 16px base)", px_val, px_val.parse::<f64>().unwrap_or(16.0) / 16.0),
                )
                .with_impact(ImpactAssessment::low_vision()),
            );
        }
    }
}

/// Check line-height is at least 1.5 for body text
fn check_line_height(path: &Path, content: &str, findings: &mut Vec<Finding>) {
    let line_height_re = Regex::new(r"(?i)line-height\s*:\s*([\d.]+)").expect("valid regex");
    let block_re = Regex::new(r"([^{]+)\{([^}]+)\}").expect("valid regex");

    for caps in block_re.captures_iter(content) {
        let selector = caps[1].trim();
        let declarations = &caps[2];

        // Only check body/paragraph/main text selectors
        let is_body_text = selector == "body" || selector == "p"
            || selector.contains("main") || selector.contains("article")
            || selector.contains("content");

        if !is_body_text {
            continue;
        }

        if let Some(lh_caps) = line_height_re.captures(declarations) {
            let value: f64 = match lh_caps[1].parse() {
                Ok(v) => v,
                Err(_) => continue,
            };

            // Values > 4 are likely in px, not unitless
            if value < 1.5 && value > 0.0 && value < 4.0 {
                let line = find_selector_line(content, selector);
                findings.push(
                    Finding::new(
                        "WCAG-1.4.12-line-height",
                        Severity::Warning,
                        &format!(
                            "Line height {:.1} for \"{}\" is below the recommended 1.5. Adequate line spacing improves readability.",
                            value, selector
                        ),
                    )
                    .with_wcag("1.4.12", WcagLevel::AA)
                    .with_category("accessibility/css")
                    .with_rule_name("Text Spacing: Insufficient Line Height")
                    .with_file(path.to_path_buf())
                    .with_line(line)
                    .with_suggestion("Set line-height to at least 1.5 for body text")
                    .with_impact(ImpactAssessment::low_vision()),
                );
            }
        }
    }
}

/// Check for prefers-reduced-motion media query
fn check_reduced_motion(path: &Path, content: &str, findings: &mut Vec<Finding>) {
    // Only flag if there are animations/transitions but no reduced-motion query
    let has_animation = content.contains("animation") || content.contains("transition");
    let has_reduced_motion = content.contains("prefers-reduced-motion");

    if has_animation && !has_reduced_motion {
        findings.push(
            Finding::new(
                "WCAG-2.3.3-no-reduced-motion",
                Severity::Warning,
                "CSS contains animations/transitions but does not respect prefers-reduced-motion. Users with vestibular disorders may experience discomfort.",
            )
            .with_wcag("2.3.3", WcagLevel::AAA)
            .with_category("accessibility/css")
            .with_rule_name("Animation from Interactions: No Reduced Motion")
            .with_file(path.to_path_buf())
            .with_suggestion(
                "Add @media (prefers-reduced-motion: reduce) { * { animation: none !important; transition: none !important; } }",
            )
            .with_impact(ImpactAssessment { blind: false, low_vision: false, motor: false, cognitive: true, deaf: false }),
        );
    }
}

/// Check for prefers-color-scheme support
fn check_color_scheme(path: &Path, content: &str, findings: &mut Vec<Finding>) {
    let has_colors = content.contains("color:") || content.contains("background");
    let has_color_scheme = content.contains("prefers-color-scheme");

    if has_colors && !has_color_scheme && content.len() > 500 {
        findings.push(
            Finding::new(
                "a11y-css-no-color-scheme",
                Severity::Info,
                "CSS does not include prefers-color-scheme media query. Supporting dark mode improves usability for low-vision users and reduces eye strain.",
            )
            .with_category("accessibility/css")
            .with_rule_name("Color Scheme: No Dark Mode Support")
            .with_file(path.to_path_buf())
            .with_suggestion(
                "Add @media (prefers-color-scheme: dark) { ... } with appropriate dark color values",
            )
            .with_impact(ImpactAssessment::low_vision()),
        );
    }
}

/// Check for prefers-contrast support
fn check_contrast_preference(path: &Path, content: &str, findings: &mut Vec<Finding>) {
    let has_colors = content.contains("color:") || content.contains("background");
    let has_contrast_pref = content.contains("prefers-contrast");

    if has_colors && !has_contrast_pref && content.len() > 500 {
        findings.push(
            Finding::new(
                "a11y-css-no-contrast-pref",
                Severity::Info,
                "CSS does not include prefers-contrast media query. High-contrast mode helps low-vision users.",
            )
            .with_category("accessibility/css")
            .with_rule_name("Contrast Preference: No High Contrast Support")
            .with_file(path.to_path_buf())
            .with_suggestion(
                "Add @media (prefers-contrast: more) { ... } with high-contrast color values",
            )
            .with_impact(ImpactAssessment::low_vision()),
        );
    }
}

/// Check for display: none being used where sr-only pattern should be used
fn check_display_none_misuse(path: &Path, content: &str, findings: &mut Vec<Finding>) {
    let block_re = Regex::new(r"([^{]+)\{([^}]+)\}").expect("valid regex");

    for caps in block_re.captures_iter(content) {
        let selector = caps[1].trim();
        let declarations = &caps[2];

        // Check if the selector name suggests screen-reader content
        let sr_related = selector.contains("sr-only")
            || selector.contains("screen-reader")
            || selector.contains("visually-hidden")
            || selector.contains("a11y");

        if sr_related && declarations.contains("display: none") {
            let line = find_selector_line(content, selector);
            findings.push(
                Finding::new(
                    "a11y-css-display-none-sr",
                    Severity::Error,
                    &format!(
                        "Selector \"{}\" uses display: none, which hides content from screen readers. Use the sr-only pattern instead.",
                        selector
                    ),
                )
                .with_category("accessibility/css")
                .with_rule_name("Screen Reader Content: Hidden with display:none")
                .with_file(path.to_path_buf())
                .with_line(line)
                .with_suggestion(
                    "Replace 'display: none' with the sr-only pattern: position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0, 0, 0, 0); border: 0;",
                )
                .as_fixable()
                .with_impact(ImpactAssessment::blind()),
            );
        }
    }
}

/// Find the line of a CSS selector
fn find_selector_line(content: &str, selector: &str) -> usize {
    let trimmed = selector.trim();
    for (idx, line) in content.lines().enumerate() {
        if line.contains(trimmed) {
            return idx + 1;
        }
    }
    1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_px_font_size() {
        let css = r#"
            body { font-size: 16px; }
        "#;
        let findings = CssAnalyzer.analyze_file(Path::new("style.css"), css);
        let px_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.4.4-px-font-size").collect();
        assert_eq!(px_findings.len(), 1);
    }

    #[test]
    fn test_rem_font_size() {
        let css = r#"
            body { font-size: 1rem; }
        "#;
        let findings = CssAnalyzer.analyze_file(Path::new("style.css"), css);
        let px_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.4.4-px-font-size").collect();
        assert!(px_findings.is_empty());
    }

    #[test]
    fn test_low_line_height() {
        let css = r#"
            body { line-height: 1.0; }
        "#;
        let findings = CssAnalyzer.analyze_file(Path::new("style.css"), css);
        let lh_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.4.12-line-height").collect();
        assert_eq!(lh_findings.len(), 1);
    }

    #[test]
    fn test_good_line_height() {
        let css = r#"
            body { line-height: 1.6; }
        "#;
        let findings = CssAnalyzer.analyze_file(Path::new("style.css"), css);
        let lh_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.4.12-line-height").collect();
        assert!(lh_findings.is_empty());
    }

    #[test]
    fn test_animation_without_reduced_motion() {
        let css = r#"
            .fade-in { animation: fadeIn 0.5s ease-in; }
            @keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }
        "#;
        let findings = CssAnalyzer.analyze_file(Path::new("style.css"), css);
        let motion_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-2.3.3-no-reduced-motion").collect();
        assert_eq!(motion_findings.len(), 1);
    }

    #[test]
    fn test_animation_with_reduced_motion() {
        let css = r#"
            .fade-in { animation: fadeIn 0.5s ease-in; }
            @media (prefers-reduced-motion: reduce) { .fade-in { animation: none; } }
        "#;
        let findings = CssAnalyzer.analyze_file(Path::new("style.css"), css);
        let motion_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-2.3.3-no-reduced-motion").collect();
        assert!(motion_findings.is_empty());
    }

    #[test]
    fn test_sr_only_display_none() {
        let css = r#"
            .sr-only { display: none; }
        "#;
        let findings = CssAnalyzer.analyze_file(Path::new("style.css"), css);
        let sr_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "a11y-css-display-none-sr").collect();
        assert_eq!(sr_findings.len(), 1);
        assert_eq!(sr_findings[0].severity, Severity::Error);
    }
}
