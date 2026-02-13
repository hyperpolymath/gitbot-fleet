// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Dependency analysis for sustainability.
//!
//! Parses Cargo.toml and package.json to flag heavy dependencies
//! and suggest minimal feature sets.

use anyhow::{Context, Result};
use std::path::Path;
use sustainabot_metrics::*;

/// Known heavy Rust dependencies and their lighter alternatives/fixes
const HEAVY_RUST_DEPS: &[(&str, &str)] = &[
    ("reqwest", "Consider using ureq for simple HTTP (no async runtime needed)"),
    ("tokio", "If only using basic features, specify minimal feature set instead of 'full'"),
    ("serde_yaml", "Consider using serde_yml or minimal YAML parser if only reading configs"),
    ("openssl", "Consider using rustls for TLS (pure Rust, smaller footprint)"),
    ("diesel", "For simple queries, consider sqlx with compile-time checked queries"),
    ("chrono", "Consider using time crate (smaller, no C dependency)"),
    ("num-bigint", "If only using basic math, std library may suffice"),
    ("regex", "For simple patterns, consider using glob or manual matching"),
    ("hyper", "For simple HTTP servers, consider using tiny_http"),
    ("image", "Specify only needed format features (e.g., features = [\"png\"])"),
];

/// Known heavy npm dependencies
const HEAVY_NPM_DEPS: &[(&str, &str)] = &[
    ("moment", "Replace with dayjs or date-fns (tree-shakeable, much smaller)"),
    ("lodash", "Use native JS methods or lodash-es for tree-shaking"),
    ("axios", "Use native fetch API (available in modern browsers and Deno)"),
    ("express", "Consider Hono or Fastify for better performance"),
    ("webpack", "Consider Vite or esbuild for faster builds"),
    ("jquery", "Use native DOM APIs"),
    ("underscore", "Use native JS methods"),
    ("request", "Deprecated — use native fetch or undici"),
    ("bluebird", "Use native Promise"),
    ("chalk", "Use picocolors (much smaller)"),
];

/// Dependency finding
#[derive(Debug, Clone)]
pub struct DepFinding {
    pub dep_name: String,
    pub manifest: String,
    pub suggestion: String,
    pub uses_all_features: bool,
}

/// Analyze dependencies in a project directory.
pub fn analyze_dependencies(repo_path: &Path) -> Vec<AnalysisResult> {
    let mut results = Vec::new();

    // Check Cargo.toml
    let cargo_toml = repo_path.join("Cargo.toml");
    if cargo_toml.exists() {
        if let Ok(findings) = analyze_cargo_toml(&cargo_toml) {
            for finding in findings {
                results.push(dep_finding_to_result(finding));
            }
        }
    }

    // Check package.json
    let package_json = repo_path.join("package.json");
    if package_json.exists() {
        if let Ok(findings) = analyze_package_json(&package_json) {
            for finding in findings {
                results.push(dep_finding_to_result(finding));
            }
        }
    }

    results
}

fn analyze_cargo_toml(path: &Path) -> Result<Vec<DepFinding>> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read {}", path.display()))?;

    let table: toml::Value = toml::from_str(&content)
        .with_context(|| format!("Failed to parse {}", path.display()))?;

    let mut findings = Vec::new();
    let manifest = path.display().to_string();

    // Check [dependencies]
    if let Some(deps) = table.get("dependencies").and_then(|d| d.as_table()) {
        for (name, value) in deps {
            check_rust_dep(name, value, &manifest, &mut findings);
        }
    }

    // Check [dev-dependencies]
    if let Some(deps) = table.get("dev-dependencies").and_then(|d| d.as_table()) {
        for (name, value) in deps {
            check_rust_dep(name, value, &manifest, &mut findings);
        }
    }

    Ok(findings)
}

fn check_rust_dep(name: &str, value: &toml::Value, manifest: &str, findings: &mut Vec<DepFinding>) {
    // Check if it's a known heavy dep
    if let Some((_, suggestion)) = HEAVY_RUST_DEPS.iter().find(|(dep, _)| *dep == name) {
        let uses_all = check_uses_all_features(value);
        findings.push(DepFinding {
            dep_name: name.to_string(),
            manifest: manifest.to_string(),
            suggestion: suggestion.to_string(),
            uses_all_features: uses_all,
        });
    }

    // Flag deps using features = ["full"] or default-features = true with many features
    if check_uses_all_features(value) {
        let already_flagged = findings.iter().any(|f| f.dep_name == name);
        if !already_flagged {
            findings.push(DepFinding {
                dep_name: name.to_string(),
                manifest: manifest.to_string(),
                suggestion: format!(
                    "Dependency '{}' uses all features. Consider specifying only needed features.",
                    name
                ),
                uses_all_features: true,
            });
        }
    }
}

fn check_uses_all_features(value: &toml::Value) -> bool {
    if let Some(table) = value.as_table() {
        if let Some(features) = table.get("features").and_then(|f| f.as_array()) {
            return features.iter().any(|f| f.as_str() == Some("full"));
        }
    }
    false
}

fn analyze_package_json(path: &Path) -> Result<Vec<DepFinding>> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read {}", path.display()))?;

    let json: serde_json::Value = serde_json::from_str(&content)
        .with_context(|| format!("Failed to parse {}", path.display()))?;

    let mut findings = Vec::new();
    let manifest = path.display().to_string();

    // Check dependencies
    if let Some(deps) = json.get("dependencies").and_then(|d| d.as_object()) {
        for name in deps.keys() {
            if let Some((_, suggestion)) = HEAVY_NPM_DEPS.iter().find(|(dep, _)| *dep == name.as_str()) {
                findings.push(DepFinding {
                    dep_name: name.clone(),
                    manifest: manifest.clone(),
                    suggestion: suggestion.to_string(),
                    uses_all_features: false,
                });
            }
        }
    }

    // Check devDependencies too
    if let Some(deps) = json.get("devDependencies").and_then(|d| d.as_object()) {
        for name in deps.keys() {
            if let Some((_, suggestion)) = HEAVY_NPM_DEPS.iter().find(|(dep, _)| *dep == name.as_str()) {
                findings.push(DepFinding {
                    dep_name: name.clone(),
                    manifest: manifest.clone(),
                    suggestion: suggestion.to_string(),
                    uses_all_features: false,
                });
            }
        }
    }

    Ok(findings)
}

fn dep_finding_to_result(finding: DepFinding) -> AnalysisResult {
    let severity_boost = if finding.uses_all_features { 2.0 } else { 1.0 };

    AnalysisResult {
        location: CodeLocation {
            file: finding.manifest,
            line: 0,
            column: 0,
            end_line: None,
            end_column: None,
            name: Some(format!("dep:{}", finding.dep_name)),
        },
        resources: ResourceProfile {
            energy: Energy::joules(10.0 * severity_boost),
            duration: Duration::milliseconds(50.0 * severity_boost),
            carbon: crate::carbon::estimate_carbon(Energy::joules(10.0 * severity_boost)),
            memory: Memory::kilobytes(500),
        },
        health: HealthIndex::compute(
            EcoScore::new(if finding.uses_all_features { 40.0 } else { 60.0 }),
            EconScore::new(50.0),
            50.0,
        ),
        recommendations: vec![finding.suggestion.clone()],
        rule_id: "sustainabot/heavy-dependency".to_string(),
        suggestion: Some(finding.suggestion),
        end_location: None,
        confidence: Confidence::Estimated,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_heavy_dep_detection() {
        let value: toml::Value = toml::from_str(r#"version = "1.0""#).unwrap();
        let mut findings = Vec::new();
        check_rust_dep("reqwest", &value, "Cargo.toml", &mut findings);
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].dep_name, "reqwest");
    }

    #[test]
    fn test_all_features_detection() {
        // Build the TOML value as a table directly
        let mut table = toml::map::Map::new();
        table.insert("version".to_string(), toml::Value::String("1.0".to_string()));
        table.insert(
            "features".to_string(),
            toml::Value::Array(vec![toml::Value::String("full".to_string())]),
        );
        let value = toml::Value::Table(table);
        assert!(check_uses_all_features(&value));
    }

    #[test]
    fn test_no_all_features() {
        let mut table = toml::map::Map::new();
        table.insert("version".to_string(), toml::Value::String("1.0".to_string()));
        table.insert(
            "features".to_string(),
            toml::Value::Array(vec![toml::Value::String("json".to_string())]),
        );
        let value = toml::Value::Table(table);
        assert!(!check_uses_all_features(&value));
    }
}
