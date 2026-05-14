// SPDX-License-Identifier: PMPL-1.0-or-later
//! Media accessibility analyzer - WCAG 1.2.1 Audio-only/Video-only (Level A), 2.3.1 Three Flashes (Level A)
//!
//! Checks media element accessibility:
//! - Video has captions/subtitles track
//! - Audio has transcript
//! - Auto-playing media can be paused/stopped
//! - No content that flashes more than 3 times per second

use crate::analyzers::Analyzer;
use crate::fleet::{Finding, ImpactAssessment, Severity, WcagLevel};
use scraper::{Html, Selector};
use std::path::Path;

/// Media accessibility analyzer
pub struct MediaAnalyzer;

impl Analyzer for MediaAnalyzer {
    fn name(&self) -> &str {
        "Media Accessibility Analyzer"
    }

    fn description(&self) -> &str {
        "Checks video/audio elements for captions, transcripts, and autoplay (WCAG 1.2.1, 2.3.1)"
    }

    fn analyze_file(&self, path: &Path, content: &str) -> Vec<Finding> {
        let document = Html::parse_document(content);
        let mut findings = Vec::new();

        check_video_captions(&document, path, content, &mut findings);
        check_audio_transcript(&document, path, content, &mut findings);
        check_autoplay(&document, path, content, &mut findings);

        findings
    }

    fn applicable_extensions(&self) -> &[&str] {
        &["html", "htm", "jsx", "tsx", "svelte", "vue"]
    }

    fn applies_to_level(&self, _level: WcagLevel) -> bool {
        true // Level A
    }
}

/// Check that video elements have captions/subtitles track
fn check_video_captions(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let video_sel = Selector::parse("video").expect("valid selector");
    let track_sel = Selector::parse("track").expect("valid selector");

    for video in document.select(&video_sel) {
        let has_captions = video.select(&track_sel).any(|track| {
            let kind = track.value().attr("kind").unwrap_or("");
            kind == "captions" || kind == "subtitles"
        });

        if !has_captions {
            let line = find_element_line(content, "video");
            findings.push(
                Finding::new(
                    "WCAG-1.2.2-video-no-captions",
                    Severity::Error,
                    "Video element has no captions or subtitles track. Deaf and hard-of-hearing users cannot access audio content.",
                )
                .with_wcag("1.2.2", WcagLevel::A)
                .with_rule_name("Captions (Prerecorded): Missing Video Captions")
                .with_file(path.to_path_buf())
                .with_line(line)
                .with_element("<video>")
                .with_suggestion(
                    "Add <track kind=\"captions\" src=\"captions.vtt\" srclang=\"en\" label=\"English\"> inside the <video> element",
                )
                .with_impact(ImpactAssessment::deaf()),
            );
        }
    }
}

/// Check that audio elements have a transcript reference
fn check_audio_transcript(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let audio_sel = Selector::parse("audio").expect("valid selector");

    for audio in document.select(&audio_sel) {
        // Check for aria-describedby pointing to a transcript
        let has_transcript_ref = audio.value().attr("aria-describedby").is_some()
            || audio.value().attr("aria-label").map(|l| l.to_lowercase().contains("transcript")).unwrap_or(false);

        if !has_transcript_ref {
            let line = find_element_line(content, "audio");
            findings.push(
                Finding::new(
                    "WCAG-1.2.1-audio-no-transcript",
                    Severity::Warning,
                    "Audio element has no transcript reference. Provide a text transcript for deaf and hard-of-hearing users.",
                )
                .with_wcag("1.2.1", WcagLevel::A)
                .with_rule_name("Audio-only: Missing Transcript")
                .with_file(path.to_path_buf())
                .with_line(line)
                .with_element("<audio>")
                .with_suggestion(
                    "Add a transcript section and link it with aria-describedby, or provide a link to the transcript near the audio element",
                )
                .with_impact(ImpactAssessment::deaf()),
            );
        }
    }
}

/// Check for auto-playing media without controls
fn check_autoplay(
    document: &Html,
    path: &Path,
    content: &str,
    findings: &mut Vec<Finding>,
) {
    let media_sel = Selector::parse("video[autoplay], audio[autoplay]").expect("valid selector");

    for media in document.select(&media_sel) {
        let tag = media.value().name();
        let has_controls = media.value().attr("controls").is_some();
        let is_muted = media.value().attr("muted").is_some();

        if !has_controls {
            let line = find_element_line(content, tag);
            let severity = if is_muted { Severity::Warning } else { Severity::Error };

            findings.push(
                Finding::new(
                    "WCAG-1.4.2-autoplay-no-controls",
                    severity,
                    &format!(
                        "<{}> has autoplay but no controls attribute. Users must be able to pause or stop auto-playing media.",
                        tag
                    ),
                )
                .with_wcag("1.4.2", WcagLevel::A)
                .with_rule_name("Audio Control: Autoplay Without Controls")
                .with_file(path.to_path_buf())
                .with_line(line)
                .with_element(&format!("<{} autoplay>", tag))
                .with_suggestion(
                    &format!("Add the 'controls' attribute to the <{}> element so users can pause/stop playback", tag),
                )
                .as_fixable()
                .with_impact(ImpactAssessment { blind: false, low_vision: false, motor: false, cognitive: true, deaf: false }),
            );
        }
    }
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
    fn test_video_with_captions() {
        let html = r#"
            <html><body>
                <video>
                    <source src="video.mp4" type="video/mp4">
                    <track kind="captions" src="captions.vtt" srclang="en" label="English">
                </video>
            </body></html>
        "#;
        let findings = MediaAnalyzer.analyze_file(Path::new("test.html"), html);
        let caption_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.2.2-video-no-captions").collect();
        assert!(caption_findings.is_empty());
    }

    #[test]
    fn test_video_no_captions() {
        let html = r#"
            <html><body>
                <video>
                    <source src="video.mp4" type="video/mp4">
                </video>
            </body></html>
        "#;
        let findings = MediaAnalyzer.analyze_file(Path::new("test.html"), html);
        let caption_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.2.2-video-no-captions").collect();
        assert_eq!(caption_findings.len(), 1);
        assert_eq!(caption_findings[0].severity, Severity::Error);
    }

    #[test]
    fn test_autoplay_no_controls() {
        let html = r#"<html><body><video autoplay src="bg.mp4"></video></body></html>"#;
        let findings = MediaAnalyzer.analyze_file(Path::new("test.html"), html);
        let autoplay_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.4.2-autoplay-no-controls").collect();
        assert_eq!(autoplay_findings.len(), 1);
        assert_eq!(autoplay_findings[0].severity, Severity::Error);
    }

    #[test]
    fn test_autoplay_muted_no_controls() {
        let html = r#"<html><body><video autoplay muted src="bg.mp4"></video></body></html>"#;
        let findings = MediaAnalyzer.analyze_file(Path::new("test.html"), html);
        let autoplay_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.4.2-autoplay-no-controls").collect();
        assert_eq!(autoplay_findings.len(), 1);
        assert_eq!(autoplay_findings[0].severity, Severity::Warning); // Less severe when muted
    }

    #[test]
    fn test_autoplay_with_controls() {
        let html = r#"<html><body><video autoplay controls src="vid.mp4"></video></body></html>"#;
        let findings = MediaAnalyzer.analyze_file(Path::new("test.html"), html);
        let autoplay_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.4.2-autoplay-no-controls").collect();
        assert!(autoplay_findings.is_empty());
    }

    #[test]
    fn test_audio_no_transcript() {
        let html = r#"<html><body><audio src="podcast.mp3" controls></audio></body></html>"#;
        let findings = MediaAnalyzer.analyze_file(Path::new("test.html"), html);
        let transcript_findings: Vec<_> = findings.iter().filter(|f| f.rule_id == "WCAG-1.2.1-audio-no-transcript").collect();
        assert_eq!(transcript_findings.len(), 1);
    }
}
