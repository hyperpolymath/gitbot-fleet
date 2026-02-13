// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! # SustainaBot Metrics
//!
//! Core data types for ecological and economic code analysis.
//! Inspired by Eclexia's resource-aware design principles.

use serde::{Deserialize, Serialize};
use std::ops::{Add, Mul};

/// Energy measurement in Joules
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Energy(pub f64);

impl Energy {
    pub const ZERO: Self = Energy(0.0);

    pub fn joules(j: f64) -> Self {
        Energy(j)
    }

    pub fn kilojoules(kj: f64) -> Self {
        Energy(kj * 1000.0)
    }
}

impl Add for Energy {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Energy(self.0 + rhs.0)
    }
}

impl Mul<f64> for Energy {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self::Output {
        Energy(self.0 * rhs)
    }
}

/// Time duration in milliseconds
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Duration(pub f64);

impl Duration {
    pub const ZERO: Self = Duration(0.0);

    pub fn milliseconds(ms: f64) -> Self {
        Duration(ms)
    }

    pub fn seconds(s: f64) -> Self {
        Duration(s * 1000.0)
    }
}

impl Add for Duration {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Duration(self.0 + rhs.0)
    }
}

/// Carbon emissions in grams of CO2 equivalent
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Carbon(pub f64);

impl Carbon {
    pub const ZERO: Self = Carbon(0.0);

    pub fn grams_co2e(g: f64) -> Self {
        Carbon(g)
    }

    pub fn kilograms_co2e(kg: f64) -> Self {
        Carbon(kg * 1000.0)
    }
}

impl Add for Carbon {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Carbon(self.0 + rhs.0)
    }
}

impl Mul<f64> for Carbon {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self::Output {
        Carbon(self.0 * rhs)
    }
}

/// Memory usage in bytes
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Memory(pub usize);

impl Memory {
    pub const ZERO: Self = Memory(0);

    pub fn bytes(b: usize) -> Self {
        Memory(b)
    }

    pub fn kilobytes(kb: usize) -> Self {
        Memory(kb * 1024)
    }

    pub fn megabytes(mb: usize) -> Self {
        Memory(mb * 1024 * 1024)
    }
}

impl Add for Memory {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Memory(self.0 + rhs.0)
    }
}

/// Complete resource profile for a code unit
///
/// This is inspired by Eclexia's `@provides` annotations but tracked
/// at runtime during analysis rather than compile-time.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceProfile {
    pub energy: Energy,
    pub duration: Duration,
    pub carbon: Carbon,
    pub memory: Memory,
}

impl ResourceProfile {
    pub fn zero() -> Self {
        ResourceProfile {
            energy: Energy::ZERO,
            duration: Duration::ZERO,
            carbon: Carbon::ZERO,
            memory: Memory::ZERO,
        }
    }

    /// Calculate cost using shadow prices (Eclexia-inspired)
    ///
    /// Cost = λ_energy * energy + λ_time * time + λ_carbon * carbon
    pub fn cost(&self, shadow_prices: &ShadowPrices) -> f64 {
        shadow_prices.energy * self.energy.0
            + shadow_prices.time * self.duration.0
            + shadow_prices.carbon * self.carbon.0
    }
}

impl Add for ResourceProfile {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        ResourceProfile {
            energy: self.energy + rhs.energy,
            duration: self.duration + rhs.duration,
            carbon: self.carbon + rhs.carbon,
            memory: self.memory + rhs.memory,
        }
    }
}

/// Shadow prices for resources (economic optimization)
///
/// These represent the marginal value of each resource, guiding
/// trade-off decisions. Inspired by Eclexia's shadow price system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShadowPrices {
    /// Value per Joule of energy
    pub energy: f64,
    /// Value per millisecond of time
    pub time: f64,
    /// Value per gram of CO2e
    pub carbon: f64,
}

impl Default for ShadowPrices {
    fn default() -> Self {
        // Default weights favoring carbon reduction
        ShadowPrices {
            energy: 1.0,
            time: 0.5,
            carbon: 2.0, // Carbon twice as important as energy
        }
    }
}

/// Ecological score (0-100)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct EcoScore(pub f64);

impl EcoScore {
    pub fn new(score: f64) -> Self {
        EcoScore(score.clamp(0.0, 100.0))
    }
}

/// Economic score (0-100) - measures Pareto efficiency
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct EconScore(pub f64);

impl EconScore {
    pub fn new(score: f64) -> Self {
        EconScore(score.clamp(0.0, 100.0))
    }
}

/// Confidence level for an estimate
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Confidence {
    /// Derived from actual measurements or profiling data
    Measured,
    /// Calibrated against known baselines
    Calibrated,
    /// Heuristic estimate with reasonable basis
    Estimated,
    /// No strong basis; placeholder value
    Unknown,
}

impl Default for Confidence {
    fn default() -> Self {
        Confidence::Unknown
    }
}

/// Overall health index combining eco and econ scores
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthIndex {
    pub eco_score: EcoScore,
    pub econ_score: EconScore,
    pub quality_score: f64,
    pub overall: f64,
}

impl HealthIndex {
    pub fn compute(eco: EcoScore, econ: EconScore, quality: f64) -> Self {
        // Formula from README: 0.4 × Eco + 0.3 × Econ + 0.3 × Quality
        let overall = 0.4 * eco.0 + 0.3 * econ.0 + 0.3 * quality;

        HealthIndex {
            eco_score: eco,
            econ_score: econ,
            quality_score: quality,
            overall,
        }
    }
}

/// Analysis result for a single code unit (function, file, module)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisResult {
    pub location: CodeLocation,
    pub resources: ResourceProfile,
    pub health: HealthIndex,
    pub recommendations: Vec<String>,
    /// Machine-readable rule identifier (e.g. "sustainabot/nested-loops")
    #[serde(default)]
    pub rule_id: String,
    /// Concrete suggestion for fixing the finding
    #[serde(default)]
    pub suggestion: Option<String>,
    /// End location for range-based annotations
    #[serde(default)]
    pub end_location: Option<(usize, usize)>,
    /// How confident is this estimate?
    #[serde(default)]
    pub confidence: Confidence,
}

/// Source code location
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
    /// End line (1-indexed)
    #[serde(default)]
    pub end_line: Option<usize>,
    /// End column (1-indexed)
    #[serde(default)]
    pub end_column: Option<usize>,
    pub name: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_energy_arithmetic() {
        let e1 = Energy::joules(10.0);
        let e2 = Energy::joules(5.0);
        assert_eq!(e1 + e2, Energy::joules(15.0));
        assert_eq!(e1 * 2.0, Energy::joules(20.0));
    }

    #[test]
    fn test_resource_cost() {
        let profile = ResourceProfile {
            energy: Energy::joules(10.0),
            duration: Duration::milliseconds(100.0),
            carbon: Carbon::grams_co2e(5.0),
            memory: Memory::bytes(1024),
        };

        let prices = ShadowPrices::default();
        let cost = profile.cost(&prices);

        // cost = 1.0*10 + 0.5*100 + 2.0*5 = 10 + 50 + 10 = 70
        assert_eq!(cost, 70.0);
    }

    #[test]
    fn test_health_index() {
        let health = HealthIndex::compute(
            EcoScore::new(80.0),
            EconScore::new(70.0),
            60.0,
        );

        // 0.4*80 + 0.3*70 + 0.3*60 = 32 + 21 + 18 = 71
        assert_eq!(health.overall, 71.0);
    }
}
