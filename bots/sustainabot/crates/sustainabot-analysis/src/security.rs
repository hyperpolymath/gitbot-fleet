// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Security-sustainability correlation engine.
//!
//! Maps panic-attack weak points to sustainability impact, producing
//! findings that correlate security risk with ecological cost.
//!
//! This module is only available when the `panic-attack` feature is enabled.

#[cfg(feature = "panic-attack")]
mod inner {
    use anyhow::Result;
    use std::path::Path;
    use sustainabot_metrics::*;

    /// Security-sustainability correlation result
    #[derive(Debug, Clone)]
    pub struct SecurityCorrelation {
        /// The original analysis results for the repo
        pub eco_results: Vec<AnalysisResult>,
        /// Security-derived sustainability findings
        pub security_findings: Vec<AnalysisResult>,
        /// Composite score (0-100) combining security and eco scores
        pub composite_score: f64,
    }

    /// Run panic-attack scan and correlate with sustainability analysis.
    pub fn correlate(
        repo_path: &Path,
        eco_results: &[AnalysisResult],
    ) -> Result<SecurityCorrelation> {
        // Run panic-attack scan
        let report = panic_attack::xray::analyze(repo_path)?;

        let mut security_findings = Vec::new();

        for wp in &report.weak_points {
            let (impact_desc, energy_multiplier) = match wp.category {
                panic_attack::types::WeakPointCategory::UnsafeCode => (
                    "Crash risk from unsafe code: all prior computation wasted on crash",
                    3.0,
                ),
                panic_attack::types::WeakPointCategory::PanicPath => (
                    "Panic/abort path: energy and carbon invested in computation becomes sunk cost",
                    2.5,
                ),
                panic_attack::types::WeakPointCategory::UnboundedLoop => (
                    "Unbounded loop: potential CPU waste and carbon spike",
                    4.0,
                ),
                panic_attack::types::WeakPointCategory::UncheckedAllocation => (
                    "Unchecked allocation: memory waste from potential OOM or oversized alloc",
                    2.0,
                ),
                panic_attack::types::WeakPointCategory::ResourceLeak => (
                    "Resource leak: ongoing waste of handles, goroutines, or memory",
                    3.5,
                ),
                panic_attack::types::WeakPointCategory::RaceCondition => (
                    "Race condition: unpredictable resource usage, potential for wasted retries",
                    2.0,
                ),
                panic_attack::types::WeakPointCategory::BlockingIO => (
                    "Blocking I/O: thread held idle, wasting CPU time and energy",
                    1.5,
                ),
                panic_attack::types::WeakPointCategory::DeadlockPotential => (
                    "Deadlock potential: complete CPU waste when threads stall",
                    4.0,
                ),
            };

            let severity_str = format!("{}", wp.severity);

            // Estimate energy impact based on severity and category
            let base_energy = match wp.severity {
                panic_attack::types::Severity::Critical => 100.0,
                panic_attack::types::Severity::High => 50.0,
                panic_attack::types::Severity::Medium => 20.0,
                panic_attack::types::Severity::Low => 5.0,
            };

            let energy = Energy::joules(base_energy * energy_multiplier);
            let carbon = crate::carbon::estimate_carbon(energy);

            let file = wp
                .location
                .clone()
                .unwrap_or_else(|| "<unknown>".to_string());

            // Check if any eco result overlaps this location (boost severity)
            let has_eco_overlap = eco_results.iter().any(|r| r.location.file == file);
            let boost = if has_eco_overlap { 1.5 } else { 1.0 };

            let eco_score = EcoScore::new(
                (100.0 - (energy.0 * boost).ln() * 10.0).max(0.0),
            );
            let econ_score = EconScore::new(50.0); // neutral economic impact

            let health = HealthIndex::compute(eco_score, econ_score, 50.0);

            let finding = AnalysisResult {
                location: CodeLocation {
                    file,
                    line: 0, // panic-attack doesn't provide line numbers in WeakPoint
                    column: 0,
                    end_line: None,
                    end_column: None,
                    name: Some(format!("{:?}", wp.category)),
                },
                resources: ResourceProfile {
                    energy,
                    duration: Duration::milliseconds(base_energy * 0.5),
                    carbon,
                    memory: Memory::kilobytes((base_energy * 10.0) as usize),
                },
                health,
                recommendations: vec![
                    format!("[{}] {}", severity_str, impact_desc),
                    wp.description.clone(),
                ],
                rule_id: "sustainabot/security-sustainability".to_string(),
                suggestion: Some(format!(
                    "Address {:?} weak point to prevent {} energy waste",
                    wp.category, impact_desc
                )),
                end_location: None,
                confidence: Confidence::Estimated,
            };

            security_findings.push(finding);
        }

        // Calculate composite score
        let all_eco: Vec<f64> = eco_results
            .iter()
            .chain(security_findings.iter())
            .map(|r| r.health.eco_score.0)
            .collect();

        let composite_score = if all_eco.is_empty() {
            100.0
        } else {
            all_eco.iter().sum::<f64>() / all_eco.len() as f64
        };

        Ok(SecurityCorrelation {
            eco_results: eco_results.to_vec(),
            security_findings,
            composite_score,
        })
    }
}

#[cfg(feature = "panic-attack")]
pub use inner::*;

/// Stub types available even without the feature, for API compatibility
#[cfg(not(feature = "panic-attack"))]
pub mod stub {
    /// Security correlation is unavailable without the `panic-attack` feature.
    pub fn is_available() -> bool {
        false
    }
}

#[cfg(not(feature = "panic-attack"))]
pub use stub::*;
