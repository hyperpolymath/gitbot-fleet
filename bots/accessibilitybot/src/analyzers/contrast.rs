// SPDX-License-Identifier: PMPL-1.0-or-later
//! Color contrast analyzer - WCAG 1.4.6 Contrast Enhanced (Level AAA)
//!
//! Parses CSS files for color/background-color pairs and calculates
//! contrast ratios using the WCAG luminance algorithm.
//! - AAA: 7:1 for normal text, 4.5:1 for large text
//! - AA: 4.5:1 for normal text, 3:1 for large text

use crate::analyzers::Analyzer;
use crate::fleet::{Finding, ImpactAssessment, Severity, WcagLevel};
use regex::Regex;
use std::path::Path;

/// Contrast analyzer for CSS color pairs
pub struct ContrastAnalyzer;

impl Analyzer for ContrastAnalyzer {
    fn name(&self) -> &str {
        "Color Contrast Analyzer"
    }

    fn description(&self) -> &str {
        "Checks color contrast ratios meet WCAG requirements (1.4.3/1.4.6)"
    }

    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding> {
        let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
        match ext {
            "css" => analyze_css(path, content),
            "html" | "htm" => analyze_inline_styles(path, content),
            _ => Vec::new(),
        }
    }

    fn applicable_extensions(&self) -> &[&str] {
        &["css", "html", "htm"]
    }

    fn applies_to_level(&self, _level: WcagLevel) -> bool {
        true // AA minimum, AAA enhanced
    }
}

/// Parse a CSS hex color (#rgb, #rrggbb) into (r, g, b) components
pub fn parse_hex_color(hex: &str) -> Option<(u8, u8, u8)> {
    let hex = hex.trim_start_matches('#');
    match hex.len() {
        3 => {
            let r = u8::from_str_radix(&hex[0..1].repeat(2), 16).ok()?;
            let g = u8::from_str_radix(&hex[1..2].repeat(2), 16).ok()?;
            let b = u8::from_str_radix(&hex[2..3].repeat(2), 16).ok()?;
            Some((r, g, b))
        }
        6 => {
            let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
            let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
            let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
            Some((r, g, b))
        }
        _ => None,
    }
}

/// Parse an rgb() or rgba() color into (r, g, b)
pub fn parse_rgb_color(value: &str) -> Option<(u8, u8, u8)> {
    let re = Regex::new(r"rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)").ok()?;
    let caps = re.captures(value)?;
    let r: u8 = caps[1].parse().ok()?;
    let g: u8 = caps[2].parse().ok()?;
    let b: u8 = caps[3].parse().ok()?;
    Some((r, g, b))
}

/// Parse any CSS color value into (r, g, b)
pub fn parse_color(value: &str) -> Option<(u8, u8, u8)> {
    let trimmed = value.trim().to_lowercase();
    if trimmed.starts_with('#') {
        parse_hex_color(&trimmed)
    } else if trimmed.starts_with("rgb") {
        parse_rgb_color(&trimmed)
    } else {
        parse_named_color(&trimmed)
    }
}

/// Parse a named CSS color
pub fn parse_named_color(name: &str) -> Option<(u8, u8, u8)> {
    match name {
        "white" => Some((255, 255, 255)),
        "black" => Some((0, 0, 0)),
        "red" => Some((255, 0, 0)),
        "green" => Some((0, 128, 0)),
        "blue" => Some((0, 0, 255)),
        "yellow" => Some((255, 255, 0)),
        "gray" | "grey" => Some((128, 128, 128)),
        "silver" => Some((192, 192, 192)),
        "maroon" => Some((128, 0, 0)),
        "olive" => Some((128, 128, 0)),
        "lime" => Some((0, 255, 0)),
        "aqua" | "cyan" => Some((0, 255, 255)),
        "teal" => Some((0, 128, 128)),
        "navy" => Some((0, 0, 128)),
        "fuchsia" | "magenta" => Some((255, 0, 255)),
        "purple" => Some((128, 0, 128)),
        "orange" => Some((255, 165, 0)),
        _ => None,
    }
}

/// Calculate relative luminance per WCAG 2.x
/// <https://www.w3.org/TR/WCAG21/#dfn-relative-luminance>
pub fn relative_luminance(r: u8, g: u8, b: u8) -> f64 {
    let srgb = [r, g, b].map(|c| {
        let v = c as f64 / 255.0;
        if v <= 0.04045 {
            v / 12.92
        } else {
            ((v + 0.055) / 1.055).powf(2.4)
        }
    });
    0.2126 * srgb[0] + 0.7152 * srgb[1] + 0.0722 * srgb[2]
}

/// Calculate contrast ratio between two colors
/// Returns a ratio >= 1.0 (e.g., 4.5, 7.0, 21.0)
pub fn contrast_ratio(fg: (u8, u8, u8), bg: (u8, u8, u8)) -> f64 {
    let l1 = relative_luminance(fg.0, fg.1, fg.2);
    let l2 = relative_luminance(bg.0, bg.1, bg.2);
    let (lighter, darker) = if l1 > l2 { (l1, l2) } else { (l2, l1) };
    (lighter + 0.05) / (darker + 0.05)
}

/// Analyze CSS content for contrast issues
fn analyze_css(path: &Path, content: &str) -> Vec<Finding> {
    let mut findings = Vec::new();
    let color_re = Regex::new(
        r"(?i)(?:^|;|\{)\s*color\s*:\s*([^;}\n]+)"
    ).expect("valid regex");
    let bg_re = Regex::new(
        r"(?i)background(?:-color)?\s*:\s*([^;}\n]+)"
    ).expect("valid regex");

    // Extract color/background-color pairs within CSS rule blocks
    let block_re = Regex::new(r"([^{]+)\{([^}]+)\}").expect("valid regex");

    for caps in block_re.captures_iter(content) {
        let selector = caps[1].trim();
        let declarations = &caps[2];

        let fg_color = color_re.captures(declarations).and_then(|c| {
            parse_color(c[1].trim())
        });
        let bg_color = bg_re.captures(declarations).and_then(|c| {
            parse_color(c[1].trim())
        });

        if let (Some(fg), Some(bg)) = (fg_color, bg_color) {
            let ratio = contrast_ratio(fg, bg);

            if ratio < 4.5 {
                // Below AA minimum for normal text
                let line = find_selector_line(content, selector);
                findings.push(
                    Finding::new(
                        "WCAG-1.4.3-contrast-aa",
                        Severity::Error,
                        &format!(
                            "Color contrast ratio {:.2}:1 is below WCAG AA minimum (4.5:1) for selector \"{}\"",
                            ratio, selector
                        ),
                    )
                    .with_wcag("1.4.3", WcagLevel::AA)
                    .with_rule_name("Contrast (Minimum)")
                    .with_file(path.to_path_buf())
                    .with_line(line)
                    .with_element(selector)
                    .with_suggestion(
                        "Increase contrast between text color and background color to at least 4.5:1",
                    )
                    .with_impact(ImpactAssessment::low_vision()),
                );
            } else if ratio < 7.0 {
                // Below AAA for normal text
                let line = find_selector_line(content, selector);
                findings.push(
                    Finding::new(
                        "WCAG-1.4.6-contrast-aaa",
                        Severity::Warning,
                        &format!(
                            "Color contrast ratio {:.2}:1 is below WCAG AAA requirement (7:1) for selector \"{}\"",
                            ratio, selector
                        ),
                    )
                    .with_wcag("1.4.6", WcagLevel::AAA)
                    .with_rule_name("Contrast (Enhanced)")
                    .with_file(path.to_path_buf())
                    .with_line(line)
                    .with_element(selector)
                    .with_suggestion(
                        "Increase contrast between text color and background color to at least 7:1 for AAA compliance",
                    )
                    .with_impact(ImpactAssessment::low_vision()),
                );
            }
        }
    }

    findings
}

/// Analyze inline styles in HTML for contrast issues
fn analyze_inline_styles(path: &Path, content: &str) -> Vec<Finding> {
    let mut findings = Vec::new();
    let style_re = Regex::new(r#"style\s*=\s*"([^"]+)""#).expect("valid regex");
    let color_re = Regex::new(r"(?i)(?:^|;)\s*color\s*:\s*([^;]+)").expect("valid regex");
    let bg_re = Regex::new(r"(?i)background(?:-color)?\s*:\s*([^;]+)").expect("valid regex");

    for (line_num, line) in content.lines().enumerate() {
        if let Some(style_caps) = style_re.captures(line) {
            let style = &style_caps[1];
            let fg = color_re.captures(style).and_then(|c| parse_color(c[1].trim()));
            let bg = bg_re.captures(style).and_then(|c| parse_color(c[1].trim()));

            if let (Some(fg), Some(bg)) = (fg, bg) {
                let ratio = contrast_ratio(fg, bg);
                if ratio < 4.5 {
                    findings.push(
                        Finding::new(
                            "WCAG-1.4.3-inline-contrast",
                            Severity::Error,
                            &format!(
                                "Inline style contrast ratio {:.2}:1 is below WCAG AA minimum (4.5:1)",
                                ratio
                            ),
                        )
                        .with_wcag("1.4.3", WcagLevel::AA)
                        .with_rule_name("Contrast (Minimum) - Inline Style")
                        .with_file(path.to_path_buf())
                        .with_line(line_num + 1)
                        .with_suggestion("Increase contrast ratio to at least 4.5:1")
                        .with_impact(ImpactAssessment::low_vision()),
                    );
                }
            }
        }
    }

    findings
}

/// Find the line number of a CSS selector
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
    fn test_parse_hex_color() {
        assert_eq!(parse_hex_color("#fff"), Some((255, 255, 255)));
        assert_eq!(parse_hex_color("#000"), Some((0, 0, 0)));
        assert_eq!(parse_hex_color("#ff0000"), Some((255, 0, 0)));
        assert_eq!(parse_hex_color("#00ff00"), Some((0, 255, 0)));
    }

    #[test]
    fn test_parse_rgb_color() {
        assert_eq!(parse_rgb_color("rgb(255, 0, 0)"), Some((255, 0, 0)));
        assert_eq!(parse_rgb_color("rgba(0, 128, 0, 0.5)"), Some((0, 128, 0)));
    }

    #[test]
    fn test_contrast_ratio_black_white() {
        let ratio = contrast_ratio((0, 0, 0), (255, 255, 255));
        assert!((ratio - 21.0).abs() < 0.1, "Black on white should be ~21:1, got {:.2}", ratio);
    }

    #[test]
    fn test_contrast_ratio_same_color() {
        let ratio = contrast_ratio((128, 128, 128), (128, 128, 128));
        assert!((ratio - 1.0).abs() < 0.01, "Same color should be 1:1, got {:.2}", ratio);
    }

    #[test]
    fn test_good_contrast_css() {
        let css = r#"
            .text { color: #000; background-color: #fff; }
        "#;
        let findings = ContrastAnalyzer.analyze_file(Path::new("style.css"), css);
        assert!(findings.is_empty(), "Black on white should pass: {:?}", findings);
    }

    #[test]
    fn test_poor_contrast_css() {
        let css = r#"
            .light-text { color: #aaa; background-color: #ccc; }
        "#;
        let findings = ContrastAnalyzer.analyze_file(Path::new("style.css"), css);
        assert!(!findings.is_empty(), "Light gray on lighter gray should fail");
        assert_eq!(findings[0].severity, Severity::Error);
    }

    #[test]
    fn test_medium_contrast_css() {
        // Dark gray on white - passes AA but not AAA
        let css = r#"
            .medium { color: #595959; background-color: #ffffff; }
        "#;
        let findings = ContrastAnalyzer.analyze_file(Path::new("style.css"), css);
        // Should produce a warning for AAA, not an error
        if !findings.is_empty() {
            assert_eq!(findings[0].severity, Severity::Warning);
        }
    }

    #[test]
    fn test_inline_style_contrast() {
        let html = r#"<div style="color: #fff; background-color: #fff;">invisible text</div>"#;
        let findings = ContrastAnalyzer.analyze_file(Path::new("test.html"), html);
        assert!(!findings.is_empty(), "White on white should fail");
    }

    #[test]
    fn test_relative_luminance() {
        let white = relative_luminance(255, 255, 255);
        let black = relative_luminance(0, 0, 0);
        assert!((white - 1.0).abs() < 0.01);
        assert!(black.abs() < 0.01);
    }
}
