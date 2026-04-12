// SPDX-License-Identifier: PMPL-1.0-or-later
//! Translator — converts panic-attack WeakPoints into gitbot-fleet Findings.
//!
//! Core responsibility: map each `WeakPoint` from the scanner output into a
//! `Finding` struct with appropriate:
//! - Fleet category (e.g., "static-analysis/unsafe-code")
//! - Rule ID (canonical PA001–PA025 pattern for dedup with Hypatia findings)
//! - Triangle tier (Eliminate / Substitute / Control)
//! - Confidence score (honest assessment of detection accuracy)
//! - Fixability (whether automated remediation is possible)
//!
//! ## Confidence Philosophy
//!
//! These are static analysis findings from regex/AST pattern matching, NOT
//! from formal verification. Confidence values reflect honest false-positive
//! rates for each category. Values above 0.90 mean "almost certainly a real
//! bug" — we only claim that for trivially detectable patterns like
//! `String.to_atom` (atom exhaustion) or `pickle.load` (unsafe deser).

use crate::config::PanicbotConfig;
use crate::scanner::WeakPoint;
use gitbot_shared_context::bot::BotId;
use gitbot_shared_context::finding::{Finding, Severity};
use gitbot_shared_context::triangle::TriangleTier;
use std::path::PathBuf;

/// Fixability classification for a finding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fixability {
    /// Can be automatically fixed by robot-repo-automaton.
    Yes,
    /// Partially fixable — some instances can be auto-fixed, others need review.
    Partial,
    /// Cannot be auto-fixed — requires human judgement.
    No,
}

/// Translation metadata for a single WeakPoint category.
///
/// Encapsulates the mapping rules for converting a panic-attack category
/// into fleet-compatible finding attributes.
#[derive(Debug, Clone)]
pub struct CategoryMapping {
    /// Fleet category string (e.g., "static-analysis/unsafe-code").
    pub fleet_category: &'static str,
    /// Canonical rule ID for deduplication (e.g., "PA001").
    pub rule_id: &'static str,
    /// Human-readable rule name.
    pub rule_name: &'static str,
    /// Safety triangle tier.
    pub triangle_tier: TriangleTier,
    /// Default confidence score (0.0 - 1.0).
    pub default_confidence: f64,
    /// Whether findings in this category can be auto-fixed.
    pub fixability: Fixability,
}

/// Look up the mapping for a panic-attack WeakPoint category.
///
/// Returns `None` for unrecognised categories — the caller should log a
/// warning and produce a generic finding.
pub fn category_mapping(category: &str) -> Option<CategoryMapping> {
    match category {
        "UnsafeCode" => Some(CategoryMapping {
            fleet_category: "static-analysis/unsafe-code",
            rule_id: "PA001",
            rule_name: "Unsafe code block",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.70,
            fixability: Fixability::No,
        }),
        "PanicPath" => Some(CategoryMapping {
            fleet_category: "static-analysis/panic-path",
            rule_id: "PA002",
            rule_name: "Potential panic path",
            triangle_tier: TriangleTier::Substitute,
            default_confidence: 0.70,
            fixability: Fixability::Partial,
        }),
        "CommandInjection" => Some(CategoryMapping {
            fleet_category: "static-analysis/command-injection",
            rule_id: "PA003",
            rule_name: "Command injection risk",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.80,
            fixability: Fixability::No,
        }),
        "HardcodedSecret" => Some(CategoryMapping {
            fleet_category: "static-analysis/hardcoded-secret",
            rule_id: "PA004",
            rule_name: "Hardcoded secret or credential",
            triangle_tier: TriangleTier::Eliminate,
            default_confidence: 0.88,
            fixability: Fixability::Yes,
        }),
        "UnsafeDeserialization" => Some(CategoryMapping {
            fleet_category: "static-analysis/unsafe-deser",
            rule_id: "PA005",
            rule_name: "Unsafe deserialization",
            triangle_tier: TriangleTier::Eliminate,
            default_confidence: 0.87,
            fixability: Fixability::Yes,
        }),
        "UncheckedError" => Some(CategoryMapping {
            fleet_category: "static-analysis/unchecked-error",
            rule_id: "PA006",
            rule_name: "Unchecked error / unwrap",
            triangle_tier: TriangleTier::Eliminate,
            default_confidence: 0.75,
            fixability: Fixability::Yes,
        }),
        "UnsafeFFI" => Some(CategoryMapping {
            fleet_category: "static-analysis/unsafe-ffi",
            rule_id: "PA007",
            rule_name: "Unsafe FFI boundary",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.75,
            fixability: Fixability::No,
        }),
        "RaceCondition" => Some(CategoryMapping {
            fleet_category: "static-analysis/race-condition",
            rule_id: "PA008",
            rule_name: "Potential race condition",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.60,
            fixability: Fixability::No,
        }),
        "ResourceLeak" => Some(CategoryMapping {
            fleet_category: "static-analysis/resource-leak",
            rule_id: "PA009",
            rule_name: "Resource leak",
            triangle_tier: TriangleTier::Substitute,
            default_confidence: 0.78,
            fixability: Fixability::Partial,
        }),
        "PathTraversal" => Some(CategoryMapping {
            fleet_category: "static-analysis/path-traversal",
            rule_id: "PA010",
            rule_name: "Path traversal risk",
            triangle_tier: TriangleTier::Substitute,
            default_confidence: 0.78,
            fixability: Fixability::Partial,
        }),
        "AtomExhaustion" => Some(CategoryMapping {
            fleet_category: "static-analysis/atom-exhaustion",
            rule_id: "PA011",
            rule_name: "Atom table exhaustion",
            triangle_tier: TriangleTier::Eliminate,
            default_confidence: 0.93,
            fixability: Fixability::Yes,
        }),
        "ExcessivePermissions" => Some(CategoryMapping {
            fleet_category: "static-analysis/excessive-perms",
            rule_id: "PA012",
            rule_name: "Excessive permissions",
            triangle_tier: TriangleTier::Eliminate,
            default_confidence: 0.88,
            fixability: Fixability::Yes,
        }),
        "UnsafeTypeCoercion" => Some(CategoryMapping {
            fleet_category: "static-analysis/unsafe-coercion",
            rule_id: "PA013",
            rule_name: "Unsafe type coercion",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.70,
            fixability: Fixability::No,
        }),
        // Original panic-attack categories not in the primary mapping table
        // but still valid findings:
        "UncheckedAllocation" => Some(CategoryMapping {
            fleet_category: "static-analysis/unchecked-alloc",
            rule_id: "PA014",
            rule_name: "Unchecked memory allocation",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.72,
            fixability: Fixability::No,
        }),
        "UnboundedLoop" => Some(CategoryMapping {
            fleet_category: "static-analysis/unbounded-loop",
            rule_id: "PA015",
            rule_name: "Unbounded loop",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.68,
            fixability: Fixability::No,
        }),
        "BlockingIO" => Some(CategoryMapping {
            fleet_category: "static-analysis/blocking-io",
            rule_id: "PA016",
            rule_name: "Blocking I/O in async context",
            triangle_tier: TriangleTier::Substitute,
            default_confidence: 0.72,
            fixability: Fixability::Partial,
        }),
        "DeadlockPotential" => Some(CategoryMapping {
            fleet_category: "static-analysis/deadlock",
            rule_id: "PA017",
            rule_name: "Deadlock potential",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.55,
            fixability: Fixability::No,
        }),
        "DynamicCodeExecution" => Some(CategoryMapping {
            fleet_category: "static-analysis/dynamic-exec",
            rule_id: "PA018",
            rule_name: "Dynamic code execution",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.82,
            fixability: Fixability::No,
        }),
        "InsecureProtocol" => Some(CategoryMapping {
            fleet_category: "static-analysis/insecure-proto",
            rule_id: "PA019",
            rule_name: "Insecure protocol usage",
            triangle_tier: TriangleTier::Substitute,
            default_confidence: 0.85,
            fixability: Fixability::Yes,
        }),
        "InfiniteRecursion" => Some(CategoryMapping {
            fleet_category: "static-analysis/infinite-recursion",
            rule_id: "PA020",
            rule_name: "Infinite recursion risk",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.65,
            fixability: Fixability::No,
        }),
        // PA021 — formal verification drift: banned proof escape hatches (sorry, Admitted,
        // believe_me, oops, trustMe, assert_total, %partial) or Julia mirror files that
        // substitute `@test x isa Y` assertions for formally-proven theorems.
        // Confidence is high because these keywords have essentially no false positives
        // in proof assistant files — they exist only to bypass the checker.
        "ProofDrift" => Some(CategoryMapping {
            fleet_category: "static-analysis/proof-drift",
            rule_id: "PA021",
            rule_name: "Formal verification drift",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.92,
            fixability: Fixability::No,
        }),
        // PA022 — cryptographic primitive misuse: MD5/SHA-1 in security contexts
        // (password, secret, token, auth, key, credential, hash, sign, verify, encrypt),
        // and == comparisons on secret-named variables (timing side-channel).
        // Confidence 0.75 — the context-window heuristic works well but has a modest
        // false-positive rate when security vocabulary appears nearby for unrelated reasons.
        "CryptoMisuse" => Some(CategoryMapping {
            fleet_category: "static-analysis/crypto-misuse",
            rule_id: "PA022",
            rule_name: "Cryptographic primitive misuse",
            triangle_tier: TriangleTier::Eliminate,
            default_confidence: 0.75,
            fixability: Fixability::Partial,
        }),
        // PA023 — supply chain integrity: unpinned or unverified dependencies and absent
        // lock files. Cargo.toml git deps without rev=, absent Cargo.lock for lib/bin crates,
        // Julia Manifest.toml without git-tree-sha1 entries, flake.nix inputs without narHash,
        // deno.json import map entries without version pin. These are fixable by adding pins.
        "SupplyChain" => Some(CategoryMapping {
            fleet_category: "static-analysis/supply-chain",
            rule_id: "PA023",
            rule_name: "Supply chain integrity gap",
            triangle_tier: TriangleTier::Eliminate,
            default_confidence: 0.85,
            fixability: Fixability::Yes,
        }),
        // PA024 — input boundary: unchecked deserialization of CBOR/MessagePack (serde_cbor,
        // ciborium, rmp_serde in Rust), JSON.parse without try-catch (JavaScript), JSON3.read
        // without error handling (Julia). Confidence 0.72 — per-file heuristics have false
        // positives when error handling is in the call site rather than the same file.
        "InputBoundary" => Some(CategoryMapping {
            fleet_category: "static-analysis/input-boundary",
            rule_id: "PA024",
            rule_name: "Unguarded input boundary",
            triangle_tier: TriangleTier::Control,
            default_confidence: 0.72,
            fixability: Fixability::Partial,
        }),
        // PA025 — mutation gap: test suites with no mutation-test tooling or no assertion
        // diversity. Rust projects without cargo-mutants config, Elixir without StreamData/
        // ExUnitProperties, Julia @testset with only type-check assertions. Confidence 0.80 —
        // absent tooling is factual; assertion diversity heuristic has modest FP rate.
        "MutationGap" => Some(CategoryMapping {
            fleet_category: "static-analysis/mutation-gap",
            rule_id: "PA025",
            rule_name: "Mutation coverage gap",
            triangle_tier: TriangleTier::Substitute,
            default_confidence: 0.80,
            fixability: Fixability::Partial,
        }),
        _ => None,
    }
}

/// Map a panic-attack severity string to a fleet Severity enum.
fn map_severity(panic_attack_severity: &str) -> Severity {
    match panic_attack_severity.to_lowercase().as_str() {
        "critical" => Severity::Error,
        "high" => Severity::Warning,
        "medium" => Severity::Info,
        "low" => Severity::Suggestion,
        _ => Severity::Info,
    }
}

/// Parse a panic-attack location string ("src/lib.rs:42") into (PathBuf, Option<usize>).
fn parse_location(location: &str) -> (PathBuf, Option<usize>) {
    // Format: "path/to/file.rs:line" or just "path/to/file.rs"
    if let Some(colon_pos) = location.rfind(':') {
        let (path_str, line_str) = location.split_at(colon_pos);
        let line_str = &line_str[1..]; // skip the ':'
        if let Ok(line) = line_str.parse::<usize>() {
            return (PathBuf::from(path_str), Some(line));
        }
    }
    (PathBuf::from(location), None)
}

/// Translate a single panic-attack WeakPoint into a fleet Finding.
///
/// Uses the category mapping table for fleet category, rule ID, triangle tier,
/// and confidence. If the category is unrecognised, produces a generic finding
/// with conservative defaults.
pub fn translate_weak_point(wp: &WeakPoint, config: &PanicbotConfig) -> Finding {
    let mapping = category_mapping(&wp.category);

    let (fleet_category, rule_id, rule_name, triangle_tier, default_confidence, fixability) =
        match &mapping {
            Some(m) => (
                m.fleet_category,
                m.rule_id,
                m.rule_name,
                m.triangle_tier,
                m.default_confidence,
                m.fixability,
            ),
            None => {
                tracing::warn!(
                    "Unknown WeakPoint category '{}', using conservative defaults",
                    wp.category
                );
                (
                    "static-analysis/unknown",
                    "PA000",
                    "Unknown static analysis finding",
                    TriangleTier::Control,
                    0.50,
                    Fixability::No,
                )
            }
        };

    // Apply confidence override from config if present
    let confidence = config
        .confidence_for(&wp.category)
        .unwrap_or(default_confidence);

    let severity = map_severity(&wp.severity);

    let mut finding = Finding::new(BotId::Panicbot, rule_id, severity, &wp.description)
        .with_rule_name(rule_name)
        .with_category(fleet_category)
        .with_triangle_tier(triangle_tier)
        .with_confidence(confidence);

    // Set fixable flag based on fixability classification
    if matches!(fixability, Fixability::Yes) {
        finding = finding.fixable();
    }

    // Parse and set location if present
    if let Some(ref loc) = wp.location {
        let (file, line) = parse_location(loc);
        finding = finding.with_file(file);
        if let Some(l) = line {
            finding = finding.with_line(l);
        }
    }

    // Add panic-attack category as metadata for traceability
    finding = finding.with_metadata(serde_json::json!({
        "panic_attack_category": wp.category,
        "panic_attack_severity": wp.severity,
        "recommended_attacks": wp.recommended_attack,
    }));

    finding
}

/// Translate all weak points from an AssailReport into a Vec of Findings.
///
/// Applies the configured minimum severity filter — findings below the
/// threshold are silently dropped.
pub fn translate_all(
    weak_points: &[WeakPoint],
    config: &PanicbotConfig,
) -> Vec<Finding> {
    let min_threshold = config.min_severity.threshold();

    weak_points
        .iter()
        .filter_map(|wp| {
            // Skip weak points suppressed by panic-attack's context-aware FP engine.
            // Suppressed = the logic engine found a defensive pattern (e.g. mutex guard,
            // RAII, schema validation) that makes this finding likely a false positive.
            if wp.suppressed {
                return None;
            }

            // Apply severity filter
            let severity_value = match wp.severity.to_lowercase().as_str() {
                "low" => 0u8,
                "medium" => 1,
                "high" => 2,
                "critical" => 3,
                _ => 0,
            };
            if severity_value < min_threshold {
                return None;
            }
            Some(translate_weak_point(wp, config))
        })
        .collect()
}

/// Classify findings into fixable and unfixable buckets.
///
/// Fixable findings are routed to the fleet pipeline for automated remediation.
/// Unfixable findings are written to the A2ML debt register.
pub fn classify_fixability(findings: &[Finding]) -> (Vec<&Finding>, Vec<&Finding>) {
    let mut fixable = Vec::new();
    let mut unfixable = Vec::new();

    for finding in findings {
        if finding.fixable {
            fixable.push(finding);
        } else {
            unfixable.push(finding);
        }
    }

    (fixable, unfixable)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_category_mapping_known() {
        let m = category_mapping("UnsafeCode").unwrap();
        assert_eq!(m.fleet_category, "static-analysis/unsafe-code");
        assert_eq!(m.rule_id, "PA001");
        assert_eq!(m.triangle_tier, TriangleTier::Control);
        assert!((m.default_confidence - 0.70).abs() < f64::EPSILON);
        assert_eq!(m.fixability, Fixability::No);
    }

    #[test]
    fn test_category_mapping_unknown() {
        assert!(category_mapping("SomethingNew").is_none());
    }

    #[test]
    fn test_map_severity() {
        assert_eq!(map_severity("Critical"), Severity::Error);
        assert_eq!(map_severity("High"), Severity::Warning);
        assert_eq!(map_severity("Medium"), Severity::Info);
        assert_eq!(map_severity("Low"), Severity::Suggestion);
        assert_eq!(map_severity("unknown"), Severity::Info);
    }

    #[test]
    fn test_parse_location_with_line() {
        let (path, line) = parse_location("src/lib.rs:42");
        assert_eq!(path, PathBuf::from("src/lib.rs"));
        assert_eq!(line, Some(42));
    }

    #[test]
    fn test_parse_location_without_line() {
        let (path, line) = parse_location("src/lib.rs");
        assert_eq!(path, PathBuf::from("src/lib.rs"));
        assert_eq!(line, None);
    }

    #[test]
    fn test_parse_location_windows_path() {
        // Ensure we handle paths with colons correctly (e.g., C:\foo)
        let (path, line) = parse_location("C:\\src\\lib.rs:10");
        assert_eq!(path, PathBuf::from("C:\\src\\lib.rs"));
        assert_eq!(line, Some(10));
    }

    #[test]
    fn test_translate_weak_point_known_category() {
        let wp = WeakPoint {
            category: "HardcodedSecret".to_string(),
            location: Some("config.py:15".to_string()),
            severity: "Critical".to_string(),
            description: "AWS_SECRET_KEY found in source".to_string(),
            recommended_attack: vec![],
            suppressed: false,
        };
        let config = PanicbotConfig::default();
        let finding = translate_weak_point(&wp, &config);

        assert_eq!(finding.source, BotId::Panicbot);
        assert_eq!(finding.rule_id, "PA004");
        assert_eq!(finding.category, "static-analysis/hardcoded-secret");
        assert_eq!(finding.severity, Severity::Error);
        assert!(finding.fixable);
        assert_eq!(finding.triangle_tier, Some(TriangleTier::Eliminate));
        assert!((finding.confidence.unwrap() - 0.88).abs() < f64::EPSILON);
        assert_eq!(finding.file, Some(PathBuf::from("config.py")));
        assert_eq!(finding.line, Some(15));
    }

    #[test]
    fn test_translate_weak_point_unknown_category() {
        let wp = WeakPoint {
            category: "BrandNewCategory".to_string(),
            location: None,
            severity: "Medium".to_string(),
            description: "Something new".to_string(),
            recommended_attack: vec![],
            suppressed: false,
        };
        let config = PanicbotConfig::default();
        let finding = translate_weak_point(&wp, &config);

        assert_eq!(finding.rule_id, "PA000");
        assert_eq!(finding.category, "static-analysis/unknown");
        assert_eq!(finding.triangle_tier, Some(TriangleTier::Control));
        assert!(!finding.fixable);
    }

    #[test]
    fn test_translate_with_confidence_override() {
        let wp = WeakPoint {
            category: "UnsafeCode".to_string(),
            location: Some("src/ffi.rs:10".to_string()),
            severity: "High".to_string(),
            description: "unsafe block".to_string(),
            recommended_attack: vec![],
            suppressed: false,
        };
        let config = PanicbotConfig {
            confidence_overrides: vec![crate::config::ConfidenceOverride {
                category: "UnsafeCode".to_string(),
                confidence: 0.55,
            }],
            ..Default::default()
        };
        let finding = translate_weak_point(&wp, &config);
        assert!((finding.confidence.unwrap() - 0.55).abs() < f64::EPSILON);
    }

    #[test]
    fn test_translate_all_with_severity_filter() {
        let weak_points = vec![
            WeakPoint {
                category: "UnsafeCode".to_string(),
                location: None,
                severity: "Low".to_string(),
                description: "low severity".to_string(),
                recommended_attack: vec![],
            suppressed: false,
            },
            WeakPoint {
                category: "CommandInjection".to_string(),
                location: None,
                severity: "Critical".to_string(),
                description: "critical severity".to_string(),
                recommended_attack: vec![],
            suppressed: false,
            },
        ];
        let config = PanicbotConfig {
            min_severity: crate::config::MinSeverity::High,
            ..Default::default()
        };
        let findings = translate_all(&weak_points, &config);

        // Only the Critical finding should pass the High filter
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].rule_id, "PA003");
    }

    #[test]
    fn test_classify_fixability() {
        let config = PanicbotConfig::default();
        let findings: Vec<Finding> = vec![
            translate_weak_point(
                &WeakPoint {
                    category: "HardcodedSecret".to_string(),
                    location: None,
                    severity: "High".to_string(),
                    description: "fixable".to_string(),
                    recommended_attack: vec![],
            suppressed: false,
                },
                &config,
            ),
            translate_weak_point(
                &WeakPoint {
                    category: "UnsafeCode".to_string(),
                    location: None,
                    severity: "High".to_string(),
                    description: "not fixable".to_string(),
                    recommended_attack: vec![],
            suppressed: false,
                },
                &config,
            ),
        ];

        let (fixable, unfixable) = classify_fixability(&findings);
        assert_eq!(fixable.len(), 1);
        assert_eq!(unfixable.len(), 1);
        assert_eq!(fixable[0].rule_id, "PA004"); // HardcodedSecret is fixable
        assert_eq!(unfixable[0].rule_id, "PA001"); // UnsafeCode is not
    }

    #[test]
    fn test_all_categories_have_unique_rule_ids() {
        let categories = [
            "UnsafeCode", "PanicPath", "CommandInjection", "HardcodedSecret",
            "UnsafeDeserialization", "UncheckedError", "UnsafeFFI", "RaceCondition",
            "ResourceLeak", "PathTraversal", "AtomExhaustion", "ExcessivePermissions",
            "UnsafeTypeCoercion", "UncheckedAllocation", "UnboundedLoop", "BlockingIO",
            "DeadlockPotential", "DynamicCodeExecution", "InsecureProtocol",
            "InfiniteRecursion", "ProofDrift", "CryptoMisuse", "SupplyChain",
            "InputBoundary", "MutationGap",
        ];

        let mut rule_ids = std::collections::HashSet::new();
        for cat in &categories {
            let mapping = category_mapping(cat).unwrap_or_else(|| panic!("Missing mapping for {}", cat));
            assert!(
                rule_ids.insert(mapping.rule_id),
                "Duplicate rule_id {} for category {}",
                mapping.rule_id,
                cat
            );
        }
        assert_eq!(rule_ids.len(), 25);
    }
}
