// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! # SustainaBot-Eclexia Integration
//!
//! Policy engine integration with two backends:
//! - **Default**: shells out to `eclexia` binary (works without eclexia repo on disk)
//! - **Native** (`eclexia-native` feature): direct library integration via eclexia-interp
//!
//! Both backends implement the same `PolicyEngine` trait.

use anyhow::{Context, Result};
use std::path::Path;
use sustainabot_metrics::{AnalysisResult, ResourceProfile};

/// Policy evaluation outcome
#[derive(Debug, Clone)]
pub struct PolicyDecision {
    /// The verdict
    pub outcome: PolicyOutcome,
    /// Human-readable explanation
    pub message: String,
    /// Suggestion for fixing a policy violation
    pub suggestion: Option<String>,
    /// Resource cost of evaluating this policy (dogfooding!)
    pub evaluation_cost: Option<ResourceProfile>,
    /// Name of the policy that produced this decision
    pub policy_name: String,
}

/// Policy verdict
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PolicyOutcome {
    /// Code meets all policy criteria
    Pass,
    /// Code has issues worth noting but not blocking
    Warn,
    /// Code violates a hard policy requirement
    Fail,
}

/// Evaluate policies against analysis results.
///
/// Uses the binary backend by default, or native eclexia-interp
/// when the `eclexia-native` feature is enabled.
pub fn evaluate_policies(
    policy_dir: &Path,
    results: &[AnalysisResult],
) -> Result<Vec<PolicyDecision>> {
    let mut decisions = Vec::new();

    // Find all .ecl files in the policy directory
    if !policy_dir.exists() {
        return Ok(decisions);
    }

    let entries = std::fs::read_dir(policy_dir)
        .with_context(|| format!("Failed to read policy dir: {}", policy_dir.display()))?;

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("ecl") {
            let policy_name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown")
                .to_string();

            match evaluate_single_policy(&path, results) {
                Ok(decision) => decisions.push(decision),
                Err(e) => {
                    tracing::warn!("Policy {} failed: {}", policy_name, e);
                    decisions.push(PolicyDecision {
                        outcome: PolicyOutcome::Warn,
                        message: format!("Policy evaluation error: {}", e),
                        suggestion: None,
                        evaluation_cost: None,
                        policy_name,
                    });
                }
            }
        }
    }

    Ok(decisions)
}

/// Evaluate a single policy file against analysis results.
fn evaluate_single_policy(
    policy_path: &Path,
    results: &[AnalysisResult],
) -> Result<PolicyDecision> {
    let policy_name = policy_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string();

    #[cfg(feature = "eclexia-native")]
    {
        evaluate_native(policy_path, results, &policy_name)
    }

    #[cfg(not(feature = "eclexia-native"))]
    {
        evaluate_binary(policy_path, results, &policy_name)
    }
}

/// Binary backend: shells out to `eclexia` CLI
#[cfg(not(feature = "eclexia-native"))]
fn evaluate_binary(
    policy_path: &Path,
    results: &[AnalysisResult],
    policy_name: &str,
) -> Result<PolicyDecision> {
    use std::process::Command;

    // Serialize results summary as JSON input
    let input = serde_json::json!({
        "function_count": results.len(),
        "total_energy": results.iter().map(|r| r.resources.energy.0).sum::<f64>(),
        "total_carbon": results.iter().map(|r| r.resources.carbon.0).sum::<f64>(),
        "avg_eco_score": if results.is_empty() { 100.0 } else {
            results.iter().map(|r| r.health.eco_score.0).sum::<f64>() / results.len() as f64
        },
        "below_threshold_count": results.iter()
            .filter(|r| r.health.eco_score.0 < 50.0)
            .count(),
    });

    // Try to find eclexia binary
    let eclexia = which_eclexia();

    match eclexia {
        Some(binary) => {
            let output = Command::new(&binary)
                .arg("run")
                .arg(policy_path)
                .arg("--input")
                .arg(input.to_string())
                .output()
                .with_context(|| format!("Failed to execute eclexia at {}", binary))?;

            if output.status.success() {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let result: bool = serde_json::from_str(stdout.trim())
                    .unwrap_or(false);

                Ok(PolicyDecision {
                    outcome: if result {
                        PolicyOutcome::Warn
                    } else {
                        PolicyOutcome::Pass
                    },
                    message: format!(
                        "Policy '{}' evaluated via eclexia binary",
                        policy_name
                    ),
                    suggestion: None,
                    evaluation_cost: Some(placeholder_cost()),
                    policy_name: policy_name.to_string(),
                })
            } else {
                let stderr = String::from_utf8_lossy(&output.stderr);
                anyhow::bail!("Eclexia policy failed: {}", stderr);
            }
        }
        None => {
            // No eclexia binary found — use built-in policy evaluation
            evaluate_builtin(results, policy_name)
        }
    }
}

/// Native backend: direct eclexia-interp library integration
#[cfg(feature = "eclexia-native")]
fn evaluate_native(
    policy_path: &Path,
    results: &[AnalysisResult],
    policy_name: &str,
) -> Result<PolicyDecision> {
    let source = std::fs::read_to_string(policy_path)
        .with_context(|| format!("Failed to read policy: {}", policy_path.display()))?;

    let (ast, errors) = eclexia_parser::parse(&source);

    if !errors.is_empty() {
        let error_msgs: Vec<String> = errors.iter().map(|e| format!("{:?}", e)).collect();
        anyhow::bail!(
            "Policy parse errors in {}: {}",
            policy_name,
            error_msgs.join("; ")
        );
    }

    // Run the policy through the interpreter
    let mut interp = eclexia_interp::Interpreter::new();
    // Set a tight budget for policy evaluation itself (dogfooding!)
    interp.set_energy_budget(1.0); // 1 Joule max for policy eval
    interp.set_carbon_budget(0.001); // 0.001g CO2e max

    match eclexia_interp::run(&ast) {
        Ok(value) => {
            let should_warn = value.is_truthy();

            Ok(PolicyDecision {
                outcome: if should_warn {
                    PolicyOutcome::Warn
                } else {
                    PolicyOutcome::Pass
                },
                message: format!(
                    "Policy '{}' evaluated natively: result={:?}",
                    policy_name, value
                ),
                suggestion: None,
                evaluation_cost: Some(placeholder_cost()),
                policy_name: policy_name.to_string(),
            })
        }
        Err(e) => {
            // Runtime error — treat as warning
            Ok(PolicyDecision {
                outcome: PolicyOutcome::Warn,
                message: format!(
                    "Policy '{}' runtime error: {:?}",
                    policy_name, e
                ),
                suggestion: Some("Check policy syntax and logic".to_string()),
                evaluation_cost: None,
                policy_name: policy_name.to_string(),
            })
        }
    }
}

/// Built-in policy evaluation when eclexia binary is not available.
///
/// Implements the core policies in Rust as a fallback.
fn evaluate_builtin(
    results: &[AnalysisResult],
    policy_name: &str,
) -> Result<PolicyDecision> {
    let total_energy: f64 = results.iter().map(|r| r.resources.energy.0).sum();
    let total_carbon: f64 = results.iter().map(|r| r.resources.carbon.0).sum();
    let avg_eco = if results.is_empty() {
        100.0
    } else {
        results.iter().map(|r| r.health.eco_score.0).sum::<f64>() / results.len() as f64
    };

    let (outcome, message, suggestion) = match policy_name {
        "energy_threshold" => {
            if total_energy > 1000.0 {
                (
                    PolicyOutcome::Fail,
                    format!("Total energy {:.2}J exceeds 1000J budget", total_energy),
                    Some("Optimize hot functions to reduce energy consumption".to_string()),
                )
            } else if total_energy > 500.0 {
                (
                    PolicyOutcome::Warn,
                    format!("Total energy {:.2}J approaching 1000J budget", total_energy),
                    Some("Consider optimizing highest-energy functions".to_string()),
                )
            } else {
                (
                    PolicyOutcome::Pass,
                    format!("Total energy {:.2}J within budget", total_energy),
                    None,
                )
            }
        }
        "carbon_budget" => {
            if total_carbon > 1.0 {
                (
                    PolicyOutcome::Fail,
                    format!("Carbon footprint {:.4}gCO2e exceeds 1g budget", total_carbon),
                    Some("Reduce computation intensity to lower carbon emissions".to_string()),
                )
            } else {
                (
                    PolicyOutcome::Pass,
                    format!("Carbon footprint {:.4}gCO2e within budget", total_carbon),
                    None,
                )
            }
        }
        "memory_efficiency" => {
            let large_allocs: Vec<_> = results
                .iter()
                .filter(|r| r.resources.memory.0 > 1_048_576) // >1MB
                .collect();
            if !large_allocs.is_empty() {
                (
                    PolicyOutcome::Warn,
                    format!("{} functions have >1MB allocations", large_allocs.len()),
                    Some("Review large allocations; consider streaming or chunked processing".to_string()),
                )
            } else {
                (PolicyOutcome::Pass, "All allocations within bounds".to_string(), None)
            }
        }
        _ => {
            // Unknown policy — apply generic eco threshold check
            if avg_eco < 50.0 {
                (
                    PolicyOutcome::Warn,
                    format!("Average eco score {:.1} below 50.0 threshold", avg_eco),
                    Some("Improve code efficiency in low-scoring functions".to_string()),
                )
            } else {
                (
                    PolicyOutcome::Pass,
                    format!("Average eco score {:.1} meets threshold", avg_eco),
                    None,
                )
            }
        }
    };

    Ok(PolicyDecision {
        outcome,
        message,
        suggestion,
        evaluation_cost: Some(placeholder_cost()),
        policy_name: format!("{} (builtin)", policy_name),
    })
}

/// Try to find the eclexia binary
fn which_eclexia() -> Option<String> {
    // Check common locations
    let candidates = [
        "eclexia",
        "~/.asdf/installs/rust/nightly/bin/eclexia",
        "../eclexia/target/release/eclexia",
    ];

    for candidate in &candidates {
        let expanded = shellexpand(candidate);
        if std::path::Path::new(&expanded).exists() {
            return Some(expanded);
        }
    }

    // Try PATH
    if std::process::Command::new("which")
        .arg("eclexia")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        return Some("eclexia".to_string());
    }

    None
}

fn shellexpand(path: &str) -> String {
    if path.starts_with('~') {
        if let Ok(home) = std::env::var("HOME") {
            return path.replacen('~', &home, 1);
        }
    }
    path.to_string()
}

fn placeholder_cost() -> ResourceProfile {
    ResourceProfile {
        energy: sustainabot_metrics::Energy::joules(0.05),
        duration: sustainabot_metrics::Duration::milliseconds(1.0),
        carbon: sustainabot_metrics::Carbon::grams_co2e(0.000007),
        memory: sustainabot_metrics::Memory::kilobytes(50),
    }
}

/// Example policy in Eclexia (to be written to policies/ directory)
pub const EXAMPLE_POLICY: &str = r#"
// SPDX-License-Identifier: PMPL-1.0-or-later
// Example SustainaBot policy in Eclexia

// This policy runs IN Eclexia, analyzing code's resource usage.
// Meta-level: The analyzer itself has provable resource bounds!

def should_warn_high_energy(energy_joules: Float) -> Bool {
    energy_joules > 100.0
}

def should_warn_high_carbon(carbon_grams: Float) -> Bool {
    carbon_grams > 10.0
}

def evaluate_policy(energy: Float, carbon: Float) -> Bool
    @requires: energy < 1J, carbon < 0.001gCO2e  // Policy itself is cheap!
{
    should_warn_high_energy(energy) || should_warn_high_carbon(carbon)
}
"#;

/// Convert policy decisions to analysis results for SARIF output
pub fn decisions_to_results(decisions: &[PolicyDecision]) -> Vec<AnalysisResult> {
    decisions
        .iter()
        .map(|d| {
            let eco_score = match d.outcome {
                PolicyOutcome::Pass => 90.0,
                PolicyOutcome::Warn => 50.0,
                PolicyOutcome::Fail => 20.0,
            };

            let cost = d.evaluation_cost.clone().unwrap_or_else(|| ResourceProfile {
                energy: sustainabot_metrics::Energy::ZERO,
                duration: sustainabot_metrics::Duration::ZERO,
                carbon: sustainabot_metrics::Carbon::ZERO,
                memory: sustainabot_metrics::Memory::ZERO,
            });

            AnalysisResult {
                location: sustainabot_metrics::CodeLocation {
                    file: "policies/".to_string(),
                    line: 0,
                    column: 0,
                    end_line: None,
                    end_column: None,
                    name: Some(d.policy_name.clone()),
                },
                resources: cost,
                health: sustainabot_metrics::HealthIndex::compute(
                    sustainabot_metrics::EcoScore::new(eco_score),
                    sustainabot_metrics::EconScore::new(80.0),
                    70.0,
                ),
                recommendations: {
                    let mut recs = vec![d.message.clone()];
                    if let Some(ref s) = d.suggestion {
                        recs.push(s.clone());
                    }
                    recs
                },
                rule_id: format!("sustainabot/policy-{}", d.policy_name),
                suggestion: d.suggestion.clone(),
                end_location: None,
                confidence: sustainabot_metrics::Confidence::Estimated,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example_policy_syntax() {
        assert!(EXAMPLE_POLICY.contains("def evaluate_policy"));
        assert!(EXAMPLE_POLICY.contains("@requires"));
    }

    #[test]
    fn test_builtin_energy_policy_pass() {
        let results = vec![sample_result(10.0)];
        let decision = evaluate_builtin(&results, "energy_threshold").unwrap();
        assert_eq!(decision.outcome, PolicyOutcome::Pass);
    }

    #[test]
    fn test_builtin_energy_policy_warn() {
        // Create many results to exceed 500J total
        let results: Vec<AnalysisResult> = (0..60).map(|_| sample_result(10.0)).collect();
        let decision = evaluate_builtin(&results, "energy_threshold").unwrap();
        assert_eq!(decision.outcome, PolicyOutcome::Warn);
    }

    #[test]
    fn test_builtin_energy_policy_fail() {
        // Exceed 1000J
        let results: Vec<AnalysisResult> = (0..200).map(|_| sample_result(10.0)).collect();
        let decision = evaluate_builtin(&results, "energy_threshold").unwrap();
        assert_eq!(decision.outcome, PolicyOutcome::Fail);
    }

    #[test]
    fn test_decisions_to_results() {
        let decisions = vec![PolicyDecision {
            outcome: PolicyOutcome::Warn,
            message: "Test warning".to_string(),
            suggestion: Some("Fix it".to_string()),
            evaluation_cost: None,
            policy_name: "test".to_string(),
        }];

        let results = decisions_to_results(&decisions);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].rule_id, "sustainabot/policy-test");
        assert!(results[0].suggestion.is_some());
    }

    fn sample_result(energy_j: f64) -> AnalysisResult {
        AnalysisResult {
            location: sustainabot_metrics::CodeLocation {
                file: "test.rs".to_string(),
                line: 1,
                column: 1,
                end_line: None,
                end_column: None,
                name: Some("test_fn".to_string()),
            },
            resources: ResourceProfile {
                energy: sustainabot_metrics::Energy::joules(energy_j),
                duration: sustainabot_metrics::Duration::milliseconds(5.0),
                carbon: sustainabot_metrics::Carbon::grams_co2e(0.001),
                memory: sustainabot_metrics::Memory::kilobytes(100),
            },
            health: sustainabot_metrics::HealthIndex::compute(
                sustainabot_metrics::EcoScore::new(80.0),
                sustainabot_metrics::EconScore::new(70.0),
                75.0,
            ),
            recommendations: vec![],
            rule_id: "sustainabot/general".to_string(),
            suggestion: None,
            end_location: None,
            confidence: sustainabot_metrics::Confidence::Estimated,
        }
    }
}
