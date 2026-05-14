// SPDX-License-Identifier: PMPL-1.0-or-later
//! Integration tests for cipherbot — verifies end-to-end analysis pipeline.

use std::path::Path;

#[test]
fn test_scan_deprecated_fixture() {
    let findings = cipherbot::analyzers::run_all_analyzers(
        Path::new("tests/fixtures"),
    );

    // Should find issues in the deprecated fixture
    assert!(
        !findings.is_empty(),
        "Should find crypto issues in test fixtures"
    );

    // Verify we found MD5
    let md5_findings: Vec<_> = findings
        .iter()
        .filter(|f| f.rule_id.contains("MD5"))
        .collect();
    assert!(
        !md5_findings.is_empty(),
        "Should detect MD5 usage in deprecated_crypto.rs"
    );

    // Verify we found SHA-1
    let sha1_findings: Vec<_> = findings
        .iter()
        .filter(|f| f.rule_id.contains("SHA") && f.message.contains("broken"))
        .collect();
    assert!(
        !sha1_findings.is_empty(),
        "Should detect SHA-1 usage in deprecated_crypto.rs"
    );
}

#[test]
fn test_scan_modern_fixture() {
    let hashing = cipherbot::analyzers::hashing::HashingAnalyzer;
    let content = std::fs::read_to_string("tests/fixtures/modern_crypto.rs")
        .expect("Should read modern_crypto.rs");

    let usages = cipherbot::analyzers::Analyzer::analyze_content(
        &hashing,
        Path::new("tests/fixtures/modern_crypto.rs"),
        &content,
    );

    // Modern crypto should produce ACCEPT/PREFER, not REJECT
    for usage in &usages {
        assert_ne!(
            usage.status,
            cipherbot::analyzers::CryptoStatus::Reject,
            "Modern crypto file should not have REJECT findings, but found: {} ({})",
            usage.algorithm,
            usage.message,
        );
    }
}

#[test]
fn test_config_scanning() {
    let analyzer = cipherbot::analyzers::config::ConfigAnalyzer;
    let content = std::fs::read_to_string("tests/fixtures/weak_config.toml")
        .expect("Should read weak_config.toml");

    let usages = cipherbot::analyzers::Analyzer::analyze_content(
        &analyzer,
        Path::new("tests/fixtures/weak_config.toml"),
        &content,
    );

    assert!(
        !usages.is_empty(),
        "Should find issues in weak_config.toml"
    );

    // Should detect hardcoded password
    let secret_findings: Vec<_> = usages
        .iter()
        .filter(|u| u.algorithm == "hardcoded-secret")
        .collect();
    assert!(
        !secret_findings.is_empty(),
        "Should detect hardcoded password in config"
    );
}

#[test]
fn test_good_config_clean() {
    let analyzer = cipherbot::analyzers::config::ConfigAnalyzer;
    let content = std::fs::read_to_string("tests/fixtures/good_config.toml")
        .expect("Should read good_config.toml");

    let usages = cipherbot::analyzers::Analyzer::analyze_content(
        &analyzer,
        Path::new("tests/fixtures/good_config.toml"),
        &content,
    );

    assert!(
        usages.is_empty(),
        "Good config should have no findings, but found: {:?}",
        usages.iter().map(|u| &u.algorithm).collect::<Vec<_>>()
    );
}

#[test]
fn test_dns_zone_scanning() {
    let analyzer = cipherbot::analyzers::dns::DnsAnalyzer;
    let content = std::fs::read_to_string("tests/fixtures/sample.zone")
        .expect("Should read sample.zone");

    let usages = cipherbot::analyzers::Analyzer::analyze_content(
        &analyzer,
        Path::new("tests/fixtures/sample.zone"),
        &content,
    );

    assert!(
        !usages.is_empty(),
        "Should find DNS issues in sample.zone"
    );
}

#[test]
fn test_pq_readiness_assessment() {
    use cipherbot::analyzers::CryptoStatus;
    use cipherbot::pq_readiness;

    // Simulate all-broken scenario
    let broken_usages = vec![
        cipherbot::analyzers::CryptoUsage {
            algorithm: "MD5".to_string(),
            status: CryptoStatus::Reject,
            file: std::path::PathBuf::from("test.rs"),
            line: 1,
            matched_text: "md5::compute".to_string(),
            category: "crypto/deprecated".to_string(),
            message: "MD5 broken".to_string(),
            suggestion: None,
        },
        cipherbot::analyzers::CryptoUsage {
            algorithm: "SHA-1".to_string(),
            status: CryptoStatus::Reject,
            file: std::path::PathBuf::from("test.rs"),
            line: 2,
            matched_text: "sha1::hash".to_string(),
            category: "crypto/deprecated".to_string(),
            message: "SHA-1 broken".to_string(),
            suggestion: None,
        },
    ];

    let assessment = pq_readiness::assess(&broken_usages);
    assert!(
        assessment.score <= 20,
        "All-broken should score <= 20, got {}",
        assessment.score
    );
    assert_eq!(assessment.rating, pq_readiness::PqRating::Critical);

    // Simulate all-modern scenario
    let modern_usages = vec![
        cipherbot::analyzers::CryptoUsage {
            algorithm: "SHAKE3-512".to_string(),
            status: CryptoStatus::Prefer,
            file: std::path::PathBuf::from("test.rs"),
            line: 1,
            matched_text: "shake3_512::digest".to_string(),
            category: "crypto/deprecated".to_string(),
            message: "SHAKE3-512 ideal".to_string(),
            suggestion: None,
        },
        cipherbot::analyzers::CryptoUsage {
            algorithm: "Kyber-1024".to_string(),
            status: CryptoStatus::Prefer,
            file: std::path::PathBuf::from("test.rs"),
            line: 2,
            matched_text: "ml_kem_1024::encapsulate".to_string(),
            category: "crypto/pq-vulnerable".to_string(),
            message: "Kyber-1024 ideal".to_string(),
            suggestion: None,
        },
    ];

    let assessment = pq_readiness::assess(&modern_usages);
    assert!(
        assessment.score >= 60,
        "All-modern should score >= 60, got {}",
        assessment.score
    );
}

#[test]
fn test_sarif_report_generation() {
    let findings = cipherbot::analyzers::run_all_analyzers(
        Path::new("tests/fixtures"),
    );

    let sarif = cipherbot::report::generate_sarif(&findings);

    // Verify SARIF structure
    assert_eq!(sarif.version, "2.1.0");
    assert_eq!(sarif.runs.len(), 1);
    assert!(!sarif.runs[0].results.is_empty(), "SARIF should have results");

    // Verify it serializes to valid JSON
    let json = serde_json::to_string_pretty(&sarif);
    assert!(json.is_ok(), "SARIF should serialize to JSON");
}

#[test]
fn test_fleet_integration() {
    let info = cipherbot::fleet::bot_info();
    assert_eq!(info.name, "Cipherbot");
    assert_eq!(info.categories.len(), 6);

    // Verify categories match expected
    assert!(info.categories.contains(&"crypto/deprecated".to_string()));
    assert!(info.categories.contains(&"crypto/weak".to_string()));
    assert!(info.categories.contains(&"crypto/pq-vulnerable".to_string()));
    assert!(info.categories.contains(&"crypto/config".to_string()));
    assert!(info.categories.contains(&"crypto/dependency".to_string()));
    assert!(info.categories.contains(&"crypto/protocol".to_string()));
}
