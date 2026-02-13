// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell

//! Gitbot fleet integration for sustainabot
//!
//! Publishes ecological and economic analysis findings to the shared context
//! layer for consumption by other bots in the fleet.

use anyhow::Result;
use gitbot_shared_context::{BotId, Context, Finding, Severity};
use std::path::{Path, PathBuf};
use sustainabot_analysis::directives;
use sustainabot_metrics::AnalysisResult;

/// Ecological thresholds for reporting
pub struct EcologicalThresholds {
    /// Total energy threshold in kilojoules
    pub total_energy_threshold_kj: f64,
    /// Total carbon threshold in grams
    pub total_carbon_threshold_grams: f64,
    /// Per-function energy threshold in joules
    pub energy_per_function_joules: f64,
}

impl Default for EcologicalThresholds {
    fn default() -> Self {
        Self {
            total_energy_threshold_kj: 10.0,
            total_carbon_threshold_grams: 2.0,
            energy_per_function_joules: 100.0,
        }
    }
}

/// Publish sustainabot analysis findings to the fleet shared context.
///
/// This is the primary integration point: converts `AnalysisResult` items
/// from `sustainabot-metrics` into `Finding` objects for the fleet.
pub fn publish_findings(
    ctx: &mut Context,
    results: &[AnalysisResult],
    thresholds: &EcologicalThresholds,
) -> Result<()> {
    let mut total_energy_j = 0.0f64;
    let mut total_carbon_g = 0.0f64;
    let mut high_impact_functions = Vec::new();

    for result in results {
        total_energy_j += result.resources.energy.0;
        total_carbon_g += result.resources.carbon.0;

        // Flag high-impact functions
        if result.resources.energy.0 > thresholds.energy_per_function_joules {
            high_impact_functions.push((
                result.location.name.clone().unwrap_or_else(|| "<anon>".to_string()),
                result.resources.energy.0,
            ));
        }

        // Convert each result to a rich Finding
        let finding = convert_to_finding(result);
        ctx.add_finding(finding);
    }

    // Report overall resource usage
    let total_energy_kj = total_energy_j / 1000.0;
    if total_energy_kj > thresholds.total_energy_threshold_kj {
        ctx.add_finding(
            Finding::new(
                BotId::Sustainabot,
                "SUSTAIN-HIGH-ENERGY",
                Severity::Warning,
                &format!(
                    "High total energy consumption: {:.2} kJ (threshold: {:.2} kJ)",
                    total_energy_kj, thresholds.total_energy_threshold_kj
                ),
            )
            .with_category("sustainability"),
        );
    }

    if total_carbon_g > thresholds.total_carbon_threshold_grams {
        ctx.add_finding(
            Finding::new(
                BotId::Sustainabot,
                "SUSTAIN-HIGH-CARBON",
                Severity::Warning,
                &format!(
                    "High carbon footprint: {:.4}g CO2e (threshold: {:.2}g)",
                    total_carbon_g, thresholds.total_carbon_threshold_grams
                ),
            )
            .with_category("sustainability"),
        );
    }

    // Report high-impact functions
    if !high_impact_functions.is_empty() {
        let function_list = high_impact_functions
            .iter()
            .map(|(name, energy)| format!("{} ({:.2}J)", name, energy))
            .collect::<Vec<_>>()
            .join(", ");

        ctx.add_finding(
            Finding::new(
                BotId::Sustainabot,
                "SUSTAIN-HIGH-IMPACT-FUNCTIONS",
                Severity::Info,
                &format!(
                    "{} function(s) exceed per-function energy threshold: {}",
                    high_impact_functions.len(),
                    function_list
                ),
            )
            .with_category("sustainability"),
        );
    }

    // Efficiency rating
    let rating = calculate_efficiency_rating(results);
    ctx.add_finding(
        Finding::new(
            BotId::Sustainabot,
            "SUSTAIN-EFFICIENCY-RATING",
            Severity::Info,
            &format!("Ecological efficiency rating: {}", rating),
        )
        .with_category("sustainability")
        .with_metadata(serde_json::json!({
            "rating": rating,
            "total_energy_kj": total_energy_kj,
            "total_carbon_g": total_carbon_g,
            "functions_analyzed": results.len(),
        })),
    );

    Ok(())
}

/// Convert a sustainabot AnalysisResult to a gitbot-fleet Finding
/// using ALL builder fields for rich integration.
fn convert_to_finding(result: &AnalysisResult) -> Finding {
    let func_name = result
        .location
        .name
        .as_deref()
        .unwrap_or("<anonymous>");

    let severity = if result.health.eco_score.0 < 30.0 {
        Severity::Error
    } else if result.health.eco_score.0 < 60.0 {
        Severity::Warning
    } else if result.health.eco_score.0 < 80.0 {
        Severity::Info
    } else {
        Severity::Suggestion
    };

    let message = format!(
        "{}: eco={:.0}/100 energy={:.2}J carbon={:.4}gCO2e. {}",
        func_name,
        result.health.eco_score.0,
        result.resources.energy.0,
        result.resources.carbon.0,
        result.recommendations.join("; "),
    );

    let mut finding = Finding::new(
        BotId::Sustainabot,
        &result.rule_id,
        severity,
        &message,
    )
    .with_rule_name(&format!("Sustainability: {}", result.rule_id))
    .with_category("sustainability")
    .with_file(PathBuf::from(&result.location.file))
    .with_location(result.location.line, result.location.column)
    .with_metadata(serde_json::json!({
        "eco_score": result.health.eco_score.0,
        "econ_score": result.health.econ_score.0,
        "quality_score": result.health.quality_score,
        "overall_health": result.health.overall,
        "energy_joules": result.resources.energy.0,
        "carbon_gco2e": result.resources.carbon.0,
        "duration_ms": result.resources.duration.0,
        "memory_bytes": result.resources.memory.0,
        "confidence": format!("{:?}", result.confidence),
    }));

    if let Some(ref suggestion) = result.suggestion {
        finding = finding.with_suggestion(suggestion);
    }

    // Mark as fixable if there's a concrete suggestion
    if result.suggestion.is_some() {
        finding = finding.fixable();
    }

    finding
}

/// Run sustainabot as a fleet member with directive awareness.
///
/// Reads `.bot_directives/sustainabot.scm` to determine allowed scopes,
/// then runs analysis respecting the directive.
pub fn run_fleet_analysis(
    repo_path: &Path,
    context_path: Option<&Path>,
) -> Result<()> {
    // Check for sustainabot directive
    let directive = directives::check_directive(repo_path, "sustainabot");

    if let Some(ref d) = directive {
        if !d.allow {
            eprintln!(
                "Sustainabot denied by .bot_directives/sustainabot.scm: {}",
                d.notes.as_deref().unwrap_or("no reason given")
            );
            return Ok(());
        }
    }

    // Collect analysis results
    let results = collect_results(repo_path)?;

    // Build thresholds from directive if available
    let thresholds = if let Some(ref d) = directive {
        let mut t = EcologicalThresholds::default();
        for (key, val) in &d.thresholds {
            match key.as_str() {
                "energy" => t.energy_per_function_joules = *val,
                "carbon" => t.total_carbon_threshold_grams = *val,
                _ => {}
            }
        }
        t
    } else {
        EcologicalThresholds::default()
    };

    // If context file provided, publish to shared context
    if let Some(ctx_path) = context_path {
        let content = std::fs::read_to_string(ctx_path)?;
        let mut ctx: Context = serde_json::from_str(&content)?;

        ctx.start_bot(BotId::Sustainabot)?;
        publish_findings(&mut ctx, &results, &thresholds)?;
        ctx.complete_bot(BotId::Sustainabot, results.len(), 0, 0)?;

        let output = serde_json::to_string_pretty(&ctx)?;
        std::fs::write(ctx_path, output)?;
    } else {
        // Standalone mode: just print findings
        let thresholds = EcologicalThresholds::default();
        let rating = calculate_efficiency_rating(&results);

        eprintln!("Sustainabot fleet analysis: {} functions", results.len());
        eprintln!("Efficiency rating: {}", rating);

        let below: Vec<_> = results
            .iter()
            .filter(|r| r.health.eco_score.0 < 60.0)
            .collect();

        if !below.is_empty() {
            eprintln!("{} functions below eco threshold:", below.len());
            for r in &below {
                eprintln!(
                    "  {} (eco: {:.0}, energy: {:.2}J)",
                    r.location.name.as_deref().unwrap_or("<anon>"),
                    r.health.eco_score.0,
                    r.resources.energy.0,
                );
            }
        }

        let total_energy: f64 = results.iter().map(|r| r.resources.energy.0).sum();
        if total_energy / 1000.0 > thresholds.total_energy_threshold_kj {
            eprintln!(
                "WARNING: Total energy {:.2} kJ exceeds threshold {:.2} kJ",
                total_energy / 1000.0,
                thresholds.total_energy_threshold_kj
            );
        }
    }

    Ok(())
}

fn collect_results(repo_path: &Path) -> Result<Vec<AnalysisResult>> {
    let mut results = Vec::new();

    for entry in walkdir(repo_path) {
        match sustainabot_analysis::analyze_file(&entry) {
            Ok(file_results) => results.extend(file_results),
            Err(_) => continue,
        }
    }

    Ok(results)
}

/// Simple directory walker for supported files
fn walkdir(path: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    walk_recursive(path, &mut files);
    files
}

fn walk_recursive(path: &Path, files: &mut Vec<PathBuf>) {
    let entries = match std::fs::read_dir(path) {
        Ok(e) => e,
        Err(_) => return,
    };

    for entry in entries.flatten() {
        let p = entry.path();
        let name = p.file_name().and_then(|n| n.to_str()).unwrap_or("");

        // Skip common non-source directories
        if p.is_dir() {
            if !matches!(
                name,
                "target" | "node_modules" | ".git" | "dist" | "build" | ".cache" | "__pycache__"
            ) {
                walk_recursive(&p, files);
            }
            continue;
        }

        // Only include supported source files
        if let Some(ext) = p.extension().and_then(|e| e.to_str()) {
            if matches!(ext, "rs" | "js" | "py") {
                files.push(p);
            }
        }
    }
}

/// Calculate efficiency rating (A-F scale)
fn calculate_efficiency_rating(results: &[AnalysisResult]) -> String {
    if results.is_empty() {
        return "N/A".to_string();
    }

    let avg_energy: f64 =
        results.iter().map(|r| r.resources.energy.0).sum::<f64>() / results.len() as f64;

    if avg_energy < 10.0 {
        "A (Excellent)".to_string()
    } else if avg_energy < 50.0 {
        "B (Good)".to_string()
    } else if avg_energy < 100.0 {
        "C (Average)".to_string()
    } else if avg_energy < 200.0 {
        "D (Below Average)".to_string()
    } else if avg_energy < 500.0 {
        "E (Poor)".to_string()
    } else {
        "F (Very Poor)".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sustainabot_metrics::*;

    fn sample_result(energy_j: f64) -> AnalysisResult {
        AnalysisResult {
            location: CodeLocation {
                file: "test.rs".to_string(),
                line: 1,
                column: 1,
                end_line: Some(10),
                end_column: Some(2),
                name: Some("test_fn".to_string()),
            },
            resources: ResourceProfile {
                energy: Energy::joules(energy_j),
                duration: Duration::milliseconds(energy_j * 0.5),
                carbon: Carbon::grams_co2e(energy_j * 0.0001),
                memory: Memory::kilobytes(100),
            },
            health: HealthIndex::compute(
                EcoScore::new(80.0),
                EconScore::new(70.0),
                75.0,
            ),
            recommendations: vec!["Code looks efficient".to_string()],
            rule_id: "sustainabot/general".to_string(),
            suggestion: None,
            end_location: Some((10, 2)),
            confidence: Confidence::Estimated,
        }
    }

    #[test]
    fn test_efficiency_rating() {
        let results = vec![sample_result(5.0)];
        let rating = calculate_efficiency_rating(&results);
        assert!(rating.starts_with('A'));
    }

    #[test]
    fn test_convert_to_finding() {
        let result = sample_result(5.0);
        let finding = convert_to_finding(&result);

        assert_eq!(finding.source, BotId::Sustainabot);
        assert_eq!(finding.rule_id, "sustainabot/general");
        assert_eq!(finding.category, "sustainability");
        assert!(finding.file.is_some());
        assert!(finding.line.is_some());
    }

    #[test]
    fn test_convert_finding_with_suggestion() {
        let mut result = sample_result(5.0);
        result.suggestion = Some("Use hash map for O(1) lookup".to_string());
        result.rule_id = "sustainabot/nested-loops".to_string();

        let finding = convert_to_finding(&result);
        assert!(finding.fixable);
        assert!(finding.suggestion.is_some());
    }

    #[test]
    fn test_severity_mapping() {
        // Low eco score → Error
        let mut result = sample_result(5.0);
        result.health = HealthIndex::compute(EcoScore::new(20.0), EconScore::new(70.0), 75.0);
        let finding = convert_to_finding(&result);
        assert_eq!(finding.severity, Severity::Error);

        // Medium eco score → Warning
        result.health = HealthIndex::compute(EcoScore::new(50.0), EconScore::new(70.0), 75.0);
        let finding = convert_to_finding(&result);
        assert_eq!(finding.severity, Severity::Warning);

        // High eco score → Info
        result.health = HealthIndex::compute(EcoScore::new(75.0), EconScore::new(70.0), 75.0);
        let finding = convert_to_finding(&result);
        assert_eq!(finding.severity, Severity::Info);
    }
}
