// SPDX-License-Identifier: PMPL-1.0-or-later
//! Integration tests for panicbot — end-to-end fleet scan pipeline.
//!
//! These tests validate the complete flow from WeakPoint JSON input through
//! to FindingSet output and A2ML generation. They do NOT require the
//! `panic-attack` binary to be installed (they test the translation layer,
//! not the scanner subprocess).

use gitbot_shared_context::bot::BotId;
use gitbot_shared_context::finding::Severity;
use gitbot_shared_context::triangle::TriangleTier;
use panicbot::a2ml_writer;
use panicbot::config::PanicbotConfig;
use panicbot::fleet;
use panicbot::scanner::{AssailReport, WeakPoint};
use panicbot::translator;

/// Simulate a realistic AssailReport with mixed findings.
fn mock_assail_report() -> AssailReport {
    AssailReport {
        program_path: std::path::PathBuf::from("/tmp/test-repo"),
        language: "rust".to_string(),
        frameworks: vec!["WebServer".to_string()],
        weak_points: vec![
            WeakPoint {
                category: "UnsafeCode".to_string(),
                location: Some("src/ffi.rs:42".to_string()),
                severity: "High".to_string(),
                description: "3 unsafe blocks in FFI boundary".to_string(),
                recommended_attack: vec!["Memory".to_string()],
            suppressed: false,
            },
            WeakPoint {
                category: "HardcodedSecret".to_string(),
                location: Some("src/config.rs:15".to_string()),
                severity: "Critical".to_string(),
                description: "AWS_SECRET_ACCESS_KEY found in source code".to_string(),
                recommended_attack: vec![],
            suppressed: false,
            },
            WeakPoint {
                category: "PanicPath".to_string(),
                location: Some("src/handler.rs:87".to_string()),
                severity: "Medium".to_string(),
                description: "unwrap() on user-provided input".to_string(),
                recommended_attack: vec!["CPU".to_string()],
            suppressed: false,
            },
            WeakPoint {
                category: "RaceCondition".to_string(),
                location: Some("src/cache.rs:23".to_string()),
                severity: "High".to_string(),
                description: "Shared mutable state without synchronisation".to_string(),
                recommended_attack: vec!["Concurrency".to_string()],
            suppressed: false,
            },
            WeakPoint {
                category: "UncheckedError".to_string(),
                location: Some("src/db.rs:56".to_string()),
                severity: "Medium".to_string(),
                description: "Result ignored from database query".to_string(),
                recommended_attack: vec![],
            suppressed: false,
            },
        ],
        statistics: serde_json::json!({"total_lines": 5000}),
        file_statistics: vec![],
        recommended_attacks: vec!["Memory".to_string(), "Concurrency".to_string()],
        dependency_graph: serde_json::Value::Null,
        taint_matrix: serde_json::Value::Null,
    }
}

#[test]
fn test_full_translation_pipeline() {
    let report = mock_assail_report();
    let config = PanicbotConfig::default();

    let findings = translator::translate_all(&report.weak_points, &config);

    // Should translate all 5 findings
    assert_eq!(findings.len(), 5);

    // All should be sourced from Panicbot
    for f in &findings {
        assert_eq!(f.source, BotId::Panicbot);
    }

    // Check specific translations
    let unsafe_finding = findings.iter().find(|f| f.rule_id == "PA001").unwrap();
    assert_eq!(unsafe_finding.severity, Severity::Warning); // High → Warning
    assert_eq!(unsafe_finding.category, "static-analysis/unsafe-code");
    assert!(!unsafe_finding.fixable);

    let secret_finding = findings.iter().find(|f| f.rule_id == "PA004").unwrap();
    assert_eq!(secret_finding.severity, Severity::Error); // Critical → Error
    assert!(secret_finding.fixable);
    assert_eq!(secret_finding.triangle_tier, Some(TriangleTier::Eliminate));
}

#[test]
fn test_fixability_classification_pipeline() {
    let report = mock_assail_report();
    let config = PanicbotConfig::default();
    let findings = translator::translate_all(&report.weak_points, &config);

    let (fixable, unfixable) = translator::classify_fixability(&findings);

    // HardcodedSecret (PA004) and UncheckedError (PA006) should be fixable
    assert_eq!(fixable.len(), 2);
    // UnsafeCode (PA001), PanicPath (PA002 partial→not fixable flag),
    // RaceCondition (PA008) should be unfixable
    assert_eq!(unfixable.len(), 3);

    let fixable_ids: Vec<&str> = fixable.iter().map(|f| f.rule_id.as_str()).collect();
    assert!(fixable_ids.contains(&"PA004"));
    assert!(fixable_ids.contains(&"PA006"));
}

#[test]
fn test_a2ml_generation_from_translated_findings() {
    let report = mock_assail_report();
    let config = PanicbotConfig::default();
    let findings = translator::translate_all(&report.weak_points, &config);
    let (fixable, unfixable) = translator::classify_fixability(&findings);

    let doc = a2ml_writer::generate_a2ml(
        &fixable,
        &unfixable,
        "hyperpolymath/test-repo",
        "2.1.0",
    );

    assert_eq!(doc.a2ml.schema, "panicbot.findings");
    assert_eq!(doc.payload.summary.total, 5);
    assert_eq!(doc.payload.summary.fixable, 2);
    assert_eq!(doc.payload.summary.unfixable, 3);
    assert_eq!(doc.payload.fixable_dispatched.len(), 2);
    assert_eq!(doc.payload.unfixable_findings.len(), 3);
}

#[test]
fn test_a2ml_write_and_read_roundtrip() {
    let tmp = tempfile::tempdir().unwrap();
    let report = mock_assail_report();
    let config = PanicbotConfig::default();
    let findings = translator::translate_all(&report.weak_points, &config);
    let (fixable, unfixable) = translator::classify_fixability(&findings);

    let doc = a2ml_writer::generate_a2ml(
        &fixable,
        &unfixable,
        "hyperpolymath/roundtrip-test",
        "2.1.0",
    );

    // Write to temp dir
    let output = a2ml_writer::write_a2ml(tmp.path(), &doc).unwrap();
    assert!(output.exists());

    // Read back and verify structure
    let content = std::fs::read_to_string(&output).unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert_eq!(parsed["a2ml"]["schema"], "panicbot.findings");
    assert_eq!(parsed["a2ml"]["version"], 1);
    assert_eq!(parsed["a2ml"]["repo"], "hyperpolymath/roundtrip-test");
    assert_eq!(parsed["payload"]["summary"]["total"], 5);
    assert!(parsed["payload"]["unfixable_findings"].is_array());
    assert!(parsed["payload"]["fixable_dispatched"].is_array());
}

#[test]
fn test_bot_info_structure() {
    let info = fleet::bot_info();
    assert_eq!(info.id, BotId::Panicbot);
    assert_eq!(info.name, "Panicbot");
    assert!(!info.can_fix);
    assert_eq!(info.depends_on, vec![BotId::Rhodibot]);
    assert!(info.categories.len() >= 10);

    // Verify all categories start with "static-analysis/"
    for cat in &info.categories {
        assert!(
            cat.starts_with("static-analysis/"),
            "Category '{}' doesn't start with 'static-analysis/'",
            cat
        );
    }
}

#[test]
fn test_assail_report_json_roundtrip() {
    let report = mock_assail_report();
    let json = serde_json::to_string_pretty(&report).unwrap();
    let parsed: AssailReport = serde_json::from_str(&json).unwrap();

    assert_eq!(parsed.language, "rust");
    assert_eq!(parsed.weak_points.len(), 5);
    assert_eq!(parsed.weak_points[0].category, "UnsafeCode");
}

#[test]
fn test_empty_report_produces_clean_output() {
    let report = AssailReport {
        program_path: std::path::PathBuf::from("/tmp/empty"),
        language: "unknown".to_string(),
        frameworks: vec![],
        weak_points: vec![],
        statistics: serde_json::Value::Null,
        file_statistics: vec![],
        recommended_attacks: vec![],
        dependency_graph: serde_json::Value::Null,
        taint_matrix: serde_json::Value::Null,
    };
    let config = PanicbotConfig::default();

    let findings = translator::translate_all(&report.weak_points, &config);
    assert!(findings.is_empty());

    let (fixable, unfixable) = translator::classify_fixability(&findings);
    let doc = a2ml_writer::generate_a2ml(&fixable, &unfixable, "test/empty", "1.0.0");
    assert_eq!(doc.payload.summary.total, 0);
}
