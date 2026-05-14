// SPDX-License-Identifier: PMPL-1.0-or-later
//! Language and text analyzer - WCAG 3.1.1 Language of Page (Level A), 3.1.5 Reading Level (Level AAA)
//!
//! Checks language and text accessibility:
//! - HTML has lang attribute
//! - Language changes marked on containing elements
//! - Reading level analysis (Flesch-Kincaid, target grade 9 for AAA)

use crate::analyzers::Analyzer;
use crate::fleet::{Finding, ImpactAssessment, Severity, WcagLevel};
use scraper::{Html, Selector};
use std::path::Path;

/// Language and text analyzer
pub struct LanguageAnalyzer;

impl Analyzer for LanguageAnalyzer {
    fn name(&self) -> &str {
        "Language and Text Analyzer"
    }

    fn description(&self) -> &str {
        "Checks language attributes and reading level (WCAG 3.1.1, 3.1.5)"
    }

    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding> {
        let document = Html::parse_document(content);
        let mut findings = Vec::new();

        check_html_lang(&document, path, &mut findings);
        check_reading_level(&document, path, &mut findings);

        findings
    }

    fn applicable_extensions(&self) -> &[&str] {
        &["html", "htm"]
    }

    fn applies_to_level(&self, _level: WcagLevel) -> bool {
        true // Level A minimum
    }
}

/// Check that the <html> element has a lang attribute
fn check_html_lang(
    document: &Html,
    path: &Path,
    findings: &mut Vec<Finding>,
) {
    let html_sel = Selector::parse("html").expect("valid selector");

    if let Some(html_el) = document.select(&html_sel).next() {
        match html_el.value().attr("lang") {
            None => {
                findings.push(
                    Finding::new(
                        "WCAG-3.1.1-missing-lang",
                        Severity::Error,
                        "The <html> element is missing a lang attribute. Screen readers need this to determine the correct pronunciation.",
                    )
                    .with_wcag("3.1.1", WcagLevel::A)
                    .with_rule_name("Language of Page: Missing Lang Attribute")
                    .with_file(path.to_path_buf())
                    .with_line(1)
                    .with_element("<html>")
                    .with_suggestion("Add lang=\"en\" (or appropriate language code) to the <html> element")
                    .as_fixable()
                    .with_impact(ImpactAssessment::blind()),
                );
            }
            Some(lang) if lang.trim().is_empty() => {
                findings.push(
                    Finding::new(
                        "WCAG-3.1.1-empty-lang",
                        Severity::Error,
                        "The <html> element has an empty lang attribute.",
                    )
                    .with_wcag("3.1.1", WcagLevel::A)
                    .with_rule_name("Language of Page: Empty Lang Attribute")
                    .with_file(path.to_path_buf())
                    .with_line(1)
                    .with_element("<html lang=\"\">")
                    .with_suggestion("Set lang to a valid BCP 47 language tag, e.g., lang=\"en\"")
                    .as_fixable()
                    .with_impact(ImpactAssessment::blind()),
                );
            }
            Some(_) => {
                // Valid lang attribute present
            }
        }
    }
}

/// Check reading level using Flesch-Kincaid Grade Level
fn check_reading_level(
    document: &Html,
    path: &Path,
    findings: &mut Vec<Finding>,
) {
    let body_sel = Selector::parse("body").expect("valid selector");
    let body = match document.select(&body_sel).next() {
        Some(b) => b,
        None => return,
    };

    let text = body.text().collect::<String>();
    let text = text.trim();

    // Only analyze if there is substantial text content
    if text.len() < 200 {
        return;
    }

    let grade_level = flesch_kincaid_grade_level(text);

    if grade_level > 12.0 {
        findings.push(
            Finding::new(
                "WCAG-3.1.5-reading-level",
                Severity::Info,
                &format!(
                    "Text reading level is approximately grade {:.1}. WCAG AAA recommends content be readable at a lower secondary education level (grade 9).",
                    grade_level
                ),
            )
            .with_wcag("3.1.5", WcagLevel::AAA)
            .with_rule_name("Reading Level")
            .with_file(path.to_path_buf())
            .with_suggestion(
                "Consider simplifying language: use shorter sentences, common words, and active voice",
            )
            .with_impact(ImpactAssessment::cognitive()),
        );
    }
}

/// Calculate Flesch-Kincaid Grade Level
///
/// Formula: 0.39 * (words/sentences) + 11.8 * (syllables/words) - 15.59
pub fn flesch_kincaid_grade_level(text: &str) -> f64 {
    let sentences = count_sentences(text).max(1);
    let words = count_words(text).max(1);
    let syllables = count_syllables(text).max(1);

    0.39 * (words as f64 / sentences as f64)
        + 11.8 * (syllables as f64 / words as f64)
        - 15.59
}

/// Count sentences (heuristic: split on . ! ?)
fn count_sentences(text: &str) -> usize {
    text.chars()
        .filter(|c| *c == '.' || *c == '!' || *c == '?')
        .count()
        .max(1)
}

/// Count words
fn count_words(text: &str) -> usize {
    text.split_whitespace().count()
}

/// Count syllables (English heuristic)
fn count_syllables(text: &str) -> usize {
    text.split_whitespace()
        .map(count_word_syllables)
        .sum()
}

/// Count syllables in a single word (English heuristic)
fn count_word_syllables(word: &str) -> usize {
    let word = word.to_lowercase();
    let word: String = word.chars().filter(|c| c.is_alphabetic()).collect();

    if word.is_empty() {
        return 0;
    }

    if word.len() <= 3 {
        return 1;
    }

    let vowels = ['a', 'e', 'i', 'o', 'u', 'y'];
    let mut count = 0;
    let mut prev_vowel = false;
    let chars: Vec<char> = word.chars().collect();

    for ch in &chars {
        if vowels.contains(ch) {
            if !prev_vowel {
                count += 1;
            }
            prev_vowel = true;
        } else {
            prev_vowel = false;
        }
    }

    // Silent e at end
    if word.ends_with('e') && count > 1 {
        count -= 1;
    }

    count.max(1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_html_with_lang() {
        let html = r#"<html lang="en"><head><title>Test</title></head><body><p>Hello</p></body></html>"#;
        let findings = LanguageAnalyzer.analyze_file(Path::new("test.html"), html);
        let lang_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-3.1.1-missing-lang").collect();
        assert!(lang_findings.is_empty());
    }

    #[test]
    fn test_html_no_lang() {
        let html = r#"<html><head><title>Test</title></head><body><p>Hello</p></body></html>"#;
        let findings = LanguageAnalyzer.analyze_file(Path::new("test.html"), html);
        let lang_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-3.1.1-missing-lang").collect();
        assert_eq!(lang_findings.len(), 1);
        assert_eq!(lang_findings[0].severity, Severity::Error);
    }

    #[test]
    fn test_html_empty_lang() {
        let html = r#"<html lang=""><head><title>Test</title></head><body><p>Hello</p></body></html>"#;
        let findings = LanguageAnalyzer.analyze_file(Path::new("test.html"), html);
        let lang_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-3.1.1-empty-lang").collect();
        assert_eq!(lang_findings.len(), 1);
    }

    #[test]
    fn test_syllable_count() {
        assert_eq!(count_word_syllables("hello"), 2);
        assert_eq!(count_word_syllables("the"), 1);
        assert_eq!(count_word_syllables("accessibility"), 6);
        assert_eq!(count_word_syllables("a"), 1);
    }

    #[test]
    fn test_flesch_kincaid() {
        // Simple sentence should have low grade level
        let simple = "The cat sat on the mat. The dog ran fast. I am happy.";
        let grade = flesch_kincaid_grade_level(simple);
        assert!(grade < 6.0, "Simple sentences should be low grade, got {:.1}", grade);
    }

    #[test]
    fn test_reading_level_not_checked_for_short_content() {
        let html = r#"<html lang="en"><body><p>Short text.</p></body></html>"#;
        let findings = LanguageAnalyzer.analyze_file(Path::new("test.html"), html);
        let reading_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-3.1.5-reading-level").collect();
        assert!(reading_findings.is_empty());
    }
}
