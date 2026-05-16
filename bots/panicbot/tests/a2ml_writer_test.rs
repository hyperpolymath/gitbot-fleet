// SPDX-License-Identifier: PMPL-1.0-or-later
//! Tests for the A2ML debt register writer.
//!
//! Validates:
//! - A2ML document structure (schema, version, envelope format)
//! - Summary statistics calculation
//! - Finding serialization to A2ML format
//! - File I/O (write to temp directory, overwrite behaviour)

use gitbot_shared_context::bot::BotId;
use gitbot_shared_context::finding::{Finding, Severity};
use gitbot_shared_context::triangle::TriangleTier;
use panicbot::a2ml_writer;
use std::path::PathBuf;

/// Helper: create a minimal Finding for testing.
fn test_finding(rule_id: &str, severity: Severity, fixable: bool, tier: TriangleTier) -> Finding {
    let mut f = Finding::new(BotId::Panicbot, rule_id, severity, &format!("Test: {}", rule_id))
        .with_category("static-analysis/test")
        .with_triangle_tier(tier)
        .with_confidence(0.75);
    if fixable {
        f = f.fixable();
    }
    f
}

// --- A2ML Document Structure ---

#[test]
fn test_a2ml_metadata_fields() {
    let doc = a2ml_writer::generate_a2ml(&[], &[], "hyperpolymath/test", "2.1.0");

    assert_eq!(doc.a2ml.schema, "panicbot.findings");
    assert_eq!(doc.a2ml.version, 1);
    assert_eq!(doc.a2ml.repo, "hyperpolymath/test");
    assert_eq!(doc.a2ml.panic_attack_version, "2.1.0");
    assert!(doc.a2ml.generator.starts_with("panicbot/"));
    // generated_at should be a valid timestamp
    assert!(doc.a2ml.generated_at.contains('T'));
}

#[test]
fn test_a2ml_empty_findings() {
    let doc = a2ml_writer::generate_a2ml(&[], &[], "test/repo", "1.0.0");

    assert_eq!(doc.payload.summary.total, 0);
    assert_eq!(doc.payload.summary.fixable, 0);
    assert_eq!(doc.payload.summary.unfixable, 0);
    assert!(doc.payload.unfixable_findings.is_empty());
    assert!(doc.payload.fixable_dispatched.is_empty());
}

#[test]
fn test_a2ml_with_mixed_findings() {
    let fixable_1 = test_finding("PA004", Severity::Error, true, TriangleTier::Eliminate);
    let fixable_2 = test_finding("PA006", Severity::Warning, true, TriangleTier::Eliminate);
    let unfixable_1 = test_finding("PA001", Severity::Warning, false, TriangleTier::Control);
    let unfixable_2 = test_finding("PA008", Severity::Info, false, TriangleTier::Control);

    let fixable = vec![&fixable_1, &fixable_2];
    let unfixable = vec![&unfixable_1, &unfixable_2];

    let doc = a2ml_writer::generate_a2ml(&fixable, &unfixable, "test/mixed", "2.0.0");

    assert_eq!(doc.payload.summary.total, 4);
    assert_eq!(doc.payload.summary.fixable, 2);
    assert_eq!(doc.payload.summary.unfixable, 2);
    assert_eq!(doc.payload.fixable_dispatched.len(), 2);
    assert_eq!(doc.payload.unfixable_findings.len(), 2);
}

// --- Summary Statistics ---

#[test]
fn test_summary_by_severity() {
    let f1 = test_finding("PA001", Severity::Error, false, TriangleTier::Control);
    let f2 = test_finding("PA002", Severity::Error, false, TriangleTier::Control);
    let f3 = test_finding("PA003", Severity::Warning, true, TriangleTier::Eliminate);

    let fixable = vec![&f3];
    let unfixable = vec![&f1, &f2];

    let doc = a2ml_writer::generate_a2ml(&fixable, &unfixable, "test/sev", "1.0.0");

    assert_eq!(*doc.payload.summary.by_severity.get("error").unwrap(), 2);
    assert_eq!(*doc.payload.summary.by_severity.get("warning").unwrap(), 1);
}

#[test]
fn test_summary_by_tier() {
    let f1 = test_finding("PA001", Severity::Warning, false, TriangleTier::Control);
    let f2 = test_finding("PA004", Severity::Error, true, TriangleTier::Eliminate);
    let f3 = test_finding("PA010", Severity::Info, false, TriangleTier::Substitute);

    let fixable = vec![&f2];
    let unfixable = vec![&f1, &f3];

    let doc = a2ml_writer::generate_a2ml(&fixable, &unfixable, "test/tier", "1.0.0");

    assert_eq!(*doc.payload.summary.by_tier.get("control").unwrap(), 1);
    assert_eq!(*doc.payload.summary.by_tier.get("eliminate").unwrap(), 1);
    assert_eq!(*doc.payload.summary.by_tier.get("substitute").unwrap(), 1);
}

// --- Finding Serialization ---

#[test]
fn test_finding_fields_in_a2ml() {
    let finding = Finding::new(BotId::Panicbot, "PA004", Severity::Error, "AWS key in source")
        .with_rule_name("Hardcoded secret or credential")
        .with_category("static-analysis/hardcoded-secret")
        .with_file(PathBuf::from("config.py"))
        .with_line(15)
        .with_triangle_tier(TriangleTier::Eliminate)
        .with_confidence(0.88)
        .fixable();

    let fixable = vec![&finding];
    let doc = a2ml_writer::generate_a2ml(&fixable, &[], "test/fields", "2.1.0");

    let a2ml_finding = &doc.payload.fixable_dispatched[0];
    assert_eq!(a2ml_finding.rule_id, "PA004");
    assert_eq!(a2ml_finding.rule_name, "Hardcoded secret or credential");
    assert_eq!(a2ml_finding.category, "static-analysis/hardcoded-secret");
    assert_eq!(a2ml_finding.severity, "error");
    assert_eq!(a2ml_finding.message, "AWS key in source");
    assert_eq!(a2ml_finding.file, Some("config.py".to_string()));
    assert_eq!(a2ml_finding.line, Some(15));
    assert_eq!(a2ml_finding.triangle_tier, Some("eliminate".to_string()));
    assert!((a2ml_finding.confidence.unwrap() - 0.88).abs() < f64::EPSILON);
}

#[test]
fn test_finding_optional_fields_absent() {
    let finding = Finding::new(BotId::Panicbot, "PA008", Severity::Info, "possible race");

    let unfixable = vec![&finding];
    let doc = a2ml_writer::generate_a2ml(&[], &unfixable, "test/opt", "1.0.0");

    let a2ml_finding = &doc.payload.unfixable_findings[0];
    assert!(a2ml_finding.file.is_none());
    assert!(a2ml_finding.line.is_none());
    assert!(a2ml_finding.triangle_tier.is_none());
    assert!(a2ml_finding.confidence.is_none());
}

// --- JSON Serialization ---

#[test]
fn test_a2ml_serializes_to_valid_json() {
    let f1 = test_finding("PA001", Severity::Warning, false, TriangleTier::Control);
    let unfixable = vec![&f1];
    let doc = a2ml_writer::generate_a2ml(&[], &unfixable, "test/json", "1.0.0");

    let json = serde_json::to_string_pretty(&doc).unwrap();

    // Verify it's valid JSON by parsing it back
    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(parsed["a2ml"]["schema"].is_string());
    assert!(parsed["payload"]["summary"]["total"].is_number());
    assert!(parsed["payload"]["unfixable_findings"].is_array());
}

#[test]
fn test_a2ml_json_envelope_format() {
    let doc = a2ml_writer::generate_a2ml(&[], &[], "test/envelope", "1.0.0");
    let json = serde_json::to_string_pretty(&doc).unwrap();

    // Must contain the A2ML envelope keys
    assert!(json.contains("\"a2ml\""));
    assert!(json.contains("\"payload\""));
    assert!(json.contains("\"schema\": \"panicbot.findings\""));
    assert!(json.contains("\"version\": 1"));
}

// --- File I/O ---

#[test]
fn test_write_a2ml_creates_directory_and_file() {
    let tmp = tempfile::tempdir().unwrap();
    let repo_path = tmp.path();

    let doc = a2ml_writer::generate_a2ml(&[], &[], "test/io", "1.0.0");
    let result = a2ml_writer::write_a2ml(repo_path, &doc);

    assert!(result.is_ok());
    let output_path = result.unwrap();
    assert!(output_path.exists());
    assert_eq!(
        output_path,
        repo_path.join(".panicbot/PANICBOT-FINDINGS.a2ml")
    );

    // Verify content is valid JSON
    let content = std::fs::read_to_string(&output_path).unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert_eq!(parsed["a2ml"]["schema"], "panicbot.findings");
}

#[test]
fn test_write_a2ml_overwrites_existing() {
    let tmp = tempfile::tempdir().unwrap();
    let repo_path = tmp.path();

    // Write first version
    let doc1 = a2ml_writer::generate_a2ml(&[], &[], "test/v1", "1.0.0");
    a2ml_writer::write_a2ml(repo_path, &doc1).unwrap();

    // Write second version (should overwrite)
    let f = test_finding("PA001", Severity::Warning, false, TriangleTier::Control);
    let unfixable = vec![&f];
    let doc2 = a2ml_writer::generate_a2ml(&[], &unfixable, "test/v2", "2.0.0");
    a2ml_writer::write_a2ml(repo_path, &doc2).unwrap();

    // Verify second version is what's on disk
    let output_path = repo_path.join(".panicbot/PANICBOT-FINDINGS.a2ml");
    let content = std::fs::read_to_string(&output_path).unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert_eq!(parsed["a2ml"]["repo"], "test/v2");
    assert_eq!(parsed["a2ml"]["panic_attack_version"], "2.0.0");
    assert_eq!(parsed["payload"]["summary"]["unfixable"], 1);
}
