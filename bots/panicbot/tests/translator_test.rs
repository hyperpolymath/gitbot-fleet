// SPDX-License-Identifier: PMPL-1.0-or-later
//! Tests for the translator module — WeakPoint → Finding conversion.
//!
//! Validates:
//! - All 20 known categories map correctly
//! - Unknown categories produce conservative defaults
//! - Severity mapping (Critical→Error, High→Warning, etc.)
//! - Location parsing (file:line, file-only, Windows paths)
//! - Confidence overrides from config
//! - Severity filtering
//! - Fixability classification

use gitbot_shared_context::bot::BotId;
use gitbot_shared_context::finding::Severity;
use gitbot_shared_context::triangle::TriangleTier;
use panicbot::config::{ConfidenceOverride, MinSeverity, PanicbotConfig};
use panicbot::scanner::WeakPoint;
use panicbot::translator::{self, Fixability};

/// Helper: create a WeakPoint with minimal fields.
fn weak_point(category: &str, severity: &str) -> WeakPoint {
    WeakPoint {
        category: category.to_string(),
        location: None,
        severity: severity.to_string(),
        description: format!("Test finding for {}", category),
        recommended_attack: vec![],
    suppressed: false,
    }
}

/// Helper: create a WeakPoint with location.
fn weak_point_at(category: &str, severity: &str, location: &str) -> WeakPoint {
    WeakPoint {
        category: category.to_string(),
        location: Some(location.to_string()),
        severity: severity.to_string(),
        description: format!("Test finding at {}", location),
        recommended_attack: vec![],
    suppressed: false,
    }
}

// --- Category Mapping Tests ---

#[test]
fn test_all_23_categories_have_mappings() {
    let categories = [
        "UnsafeCode",
        "PanicPath",
        "CommandInjection",
        "HardcodedSecret",
        "UnsafeDeserialization",
        "UncheckedError",
        "UnsafeFFI",
        "RaceCondition",
        "ResourceLeak",
        "PathTraversal",
        "AtomExhaustion",
        "ExcessivePermissions",
        "UnsafeTypeCoercion",
        "UncheckedAllocation",
        "UnboundedLoop",
        "BlockingIO",
        "DeadlockPotential",
        "DynamicCodeExecution",
        "InsecureProtocol",
        "InfiniteRecursion",
        "ProofDrift",
        "CryptoMisuse",
        "SupplyChain",
        "InputBoundary",
        "MutationGap",
    ];

    for cat in &categories {
        let mapping = translator::category_mapping(cat);
        assert!(
            mapping.is_some(),
            "Missing mapping for category: {}",
            cat
        );
    }
}

#[test]
fn test_rule_ids_sequential_pa001_to_pa020() {
    let categories = [
        "UnsafeCode",
        "PanicPath",
        "CommandInjection",
        "HardcodedSecret",
        "UnsafeDeserialization",
        "UncheckedError",
        "UnsafeFFI",
        "RaceCondition",
        "ResourceLeak",
        "PathTraversal",
        "AtomExhaustion",
        "ExcessivePermissions",
        "UnsafeTypeCoercion",
        "UncheckedAllocation",
        "UnboundedLoop",
        "BlockingIO",
        "DeadlockPotential",
        "DynamicCodeExecution",
        "InsecureProtocol",
        "InfiniteRecursion",
    ];

    for (i, cat) in categories.iter().enumerate() {
        let mapping = translator::category_mapping(cat).unwrap();
        let expected_id = format!("PA{:03}", i + 1);
        assert_eq!(
            mapping.rule_id, expected_id,
            "Category {} has rule_id {} but expected {}",
            cat, mapping.rule_id, expected_id
        );
    }
}

#[test]
fn test_eliminate_tier_categories() {
    let eliminate_categories = [
        "HardcodedSecret",
        "UnsafeDeserialization",
        "UncheckedError",
        "AtomExhaustion",
        "ExcessivePermissions",
    ];

    for cat in &eliminate_categories {
        let mapping = translator::category_mapping(cat).unwrap();
        assert_eq!(
            mapping.triangle_tier,
            TriangleTier::Eliminate,
            "{} should be Eliminate tier",
            cat
        );
    }
}

#[test]
fn test_substitute_tier_categories() {
    let substitute_categories = [
        "PanicPath",
        "ResourceLeak",
        "PathTraversal",
        "BlockingIO",
        "InsecureProtocol",
    ];

    for cat in &substitute_categories {
        let mapping = translator::category_mapping(cat).unwrap();
        assert_eq!(
            mapping.triangle_tier,
            TriangleTier::Substitute,
            "{} should be Substitute tier",
            cat
        );
    }
}

#[test]
fn test_control_tier_categories() {
    let control_categories = [
        "UnsafeCode",
        "CommandInjection",
        "UnsafeFFI",
        "RaceCondition",
        "UnsafeTypeCoercion",
        "UncheckedAllocation",
        "UnboundedLoop",
        "DeadlockPotential",
        "DynamicCodeExecution",
        "InfiniteRecursion",
    ];

    for cat in &control_categories {
        let mapping = translator::category_mapping(cat).unwrap();
        assert_eq!(
            mapping.triangle_tier,
            TriangleTier::Control,
            "{} should be Control tier",
            cat
        );
    }
}

#[test]
fn test_fixable_yes_categories() {
    let fixable_categories = [
        "HardcodedSecret",
        "UnsafeDeserialization",
        "UncheckedError",
        "AtomExhaustion",
        "ExcessivePermissions",
        "InsecureProtocol",
    ];

    for cat in &fixable_categories {
        let mapping = translator::category_mapping(cat).unwrap();
        assert_eq!(
            mapping.fixability,
            Fixability::Yes,
            "{} should be fully fixable",
            cat
        );
    }
}

// --- Translation Tests ---

#[test]
fn test_translate_sets_source_to_panicbot() {
    let wp = weak_point("UnsafeCode", "High");
    let config = PanicbotConfig::default();
    let finding = translator::translate_weak_point(&wp, &config);
    assert_eq!(finding.source, BotId::Panicbot);
}

#[test]
fn test_translate_severity_mapping() {
    let config = PanicbotConfig::default();

    let critical = translator::translate_weak_point(&weak_point("UnsafeCode", "Critical"), &config);
    assert_eq!(critical.severity, Severity::Error);

    let high = translator::translate_weak_point(&weak_point("UnsafeCode", "High"), &config);
    assert_eq!(high.severity, Severity::Warning);

    let medium = translator::translate_weak_point(&weak_point("UnsafeCode", "Medium"), &config);
    assert_eq!(medium.severity, Severity::Info);

    let low = translator::translate_weak_point(&weak_point("UnsafeCode", "Low"), &config);
    assert_eq!(low.severity, Severity::Suggestion);
}

#[test]
fn test_translate_location_parsing() {
    let config = PanicbotConfig::default();
    let wp = weak_point_at("UnsafeCode", "High", "src/ffi.rs:42");
    let finding = translator::translate_weak_point(&wp, &config);

    assert_eq!(
        finding.file,
        Some(std::path::PathBuf::from("src/ffi.rs"))
    );
    assert_eq!(finding.line, Some(42));
}

#[test]
fn test_translate_location_no_line() {
    let config = PanicbotConfig::default();
    let wp = weak_point_at("PanicPath", "Medium", "src/lib.rs");
    let finding = translator::translate_weak_point(&wp, &config);

    assert_eq!(
        finding.file,
        Some(std::path::PathBuf::from("src/lib.rs"))
    );
    assert_eq!(finding.line, None);
}

#[test]
fn test_translate_no_location() {
    let config = PanicbotConfig::default();
    let wp = weak_point("RaceCondition", "High");
    let finding = translator::translate_weak_point(&wp, &config);

    assert_eq!(finding.file, None);
    assert_eq!(finding.line, None);
}

#[test]
fn test_translate_unknown_category_conservative_defaults() {
    let config = PanicbotConfig::default();
    let wp = weak_point("TotallyNewCategory", "High");
    let finding = translator::translate_weak_point(&wp, &config);

    assert_eq!(finding.rule_id, "PA000");
    assert_eq!(finding.category, "static-analysis/unknown");
    assert_eq!(finding.triangle_tier, Some(TriangleTier::Control));
    assert!((finding.confidence.unwrap() - 0.50).abs() < f64::EPSILON);
    assert!(!finding.fixable);
}

#[test]
fn test_translate_confidence_override() {
    let config = PanicbotConfig {
        confidence_overrides: vec![ConfidenceOverride {
            category: "RaceCondition".to_string(),
            confidence: 0.40,
        }],
        ..Default::default()
    };
    let wp = weak_point("RaceCondition", "High");
    let finding = translator::translate_weak_point(&wp, &config);

    assert!((finding.confidence.unwrap() - 0.40).abs() < f64::EPSILON);
}

#[test]
fn test_translate_metadata_includes_panic_attack_category() {
    let config = PanicbotConfig::default();
    let wp = WeakPoint {
        category: "CommandInjection".to_string(),
        location: Some("app.py:10".to_string()),
        severity: "Critical".to_string(),
        description: "os.system with user input".to_string(),
        recommended_attack: vec!["Memory".to_string(), "Concurrency".to_string()],
    suppressed: false,
    };
    let finding = translator::translate_weak_point(&wp, &config);

    let metadata = &finding.metadata;
    assert_eq!(metadata["panic_attack_category"], "CommandInjection");
    assert_eq!(metadata["panic_attack_severity"], "Critical");
    assert_eq!(metadata["recommended_attacks"][0], "Memory");
}

// --- Severity Filter Tests ---

#[test]
fn test_translate_all_filters_by_min_severity() {
    let weak_points = vec![
        weak_point("UnsafeCode", "Low"),
        weak_point("CommandInjection", "Medium"),
        weak_point("HardcodedSecret", "High"),
        weak_point("RaceCondition", "Critical"),
    ];

    // Filter: only High and Critical
    let config = PanicbotConfig {
        min_severity: MinSeverity::High,
        ..Default::default()
    };
    let findings = translator::translate_all(&weak_points, &config);
    assert_eq!(findings.len(), 2);

    // Filter: only Critical
    let config = PanicbotConfig {
        min_severity: MinSeverity::Critical,
        ..Default::default()
    };
    let findings = translator::translate_all(&weak_points, &config);
    assert_eq!(findings.len(), 1);

    // Filter: everything
    let config = PanicbotConfig::default();
    let findings = translator::translate_all(&weak_points, &config);
    assert_eq!(findings.len(), 4);
}

// --- Fixability Classification Tests ---

#[test]
fn test_classify_fixability_separates_correctly() {
    let config = PanicbotConfig::default();

    // HardcodedSecret → fixable, UnsafeCode → not fixable
    let findings: Vec<_> = vec![
        translator::translate_weak_point(&weak_point("HardcodedSecret", "High"), &config),
        translator::translate_weak_point(&weak_point("UnsafeCode", "High"), &config),
        translator::translate_weak_point(&weak_point("AtomExhaustion", "Medium"), &config),
        translator::translate_weak_point(&weak_point("RaceCondition", "High"), &config),
    ];

    let (fixable, unfixable) = translator::classify_fixability(&findings);
    assert_eq!(fixable.len(), 2); // HardcodedSecret + AtomExhaustion
    assert_eq!(unfixable.len(), 2); // UnsafeCode + RaceCondition
}

#[test]
fn test_classify_empty_findings() {
    let (fixable, unfixable) = translator::classify_fixability(&[]);
    assert!(fixable.is_empty());
    assert!(unfixable.is_empty());
}

// --- Confidence Value Sanity ---

#[test]
fn test_all_confidence_values_in_valid_range() {
    let categories = [
        "UnsafeCode", "PanicPath", "CommandInjection", "HardcodedSecret",
        "UnsafeDeserialization", "UncheckedError", "UnsafeFFI", "RaceCondition",
        "ResourceLeak", "PathTraversal", "AtomExhaustion", "ExcessivePermissions",
        "UnsafeTypeCoercion", "UncheckedAllocation", "UnboundedLoop", "BlockingIO",
        "DeadlockPotential", "DynamicCodeExecution", "InsecureProtocol",
        "InfiniteRecursion", "ProofDrift", "CryptoMisuse", "SupplyChain",
        "InputBoundary", "MutationGap",
    ];

    for cat in &categories {
        let mapping = translator::category_mapping(cat).unwrap();
        assert!(
            mapping.default_confidence >= 0.0 && mapping.default_confidence <= 1.0,
            "Confidence for {} ({}) not in [0, 1]",
            cat,
            mapping.default_confidence
        );
        // Static analysis rarely exceeds 0.95 true-positive rate
        assert!(
            mapping.default_confidence <= 0.95,
            "Confidence for {} ({}) suspiciously high for static analysis",
            cat,
            mapping.default_confidence
        );
    }
}
