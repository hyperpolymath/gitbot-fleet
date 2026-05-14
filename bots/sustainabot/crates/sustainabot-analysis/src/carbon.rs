// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Carbon emission estimation

use sustainabot_metrics::{Carbon, Energy};

/// Estimate carbon emissions from energy consumption
///
/// Uses average grid carbon intensity. In future, this will integrate
/// with real-time APIs (ElectricityMaps, WattTime, etc.)
pub fn estimate_carbon(energy: Energy) -> Carbon {
    // Average grid intensity: ~475 gCO2e/kWh globally
    // 1 kWh = 3,600,000 J
    // So: gCO2e = J * (475 / 3,600,000)
    const CARBON_INTENSITY: f64 = 475.0 / 3_600_000.0;

    Carbon::grams_co2e(energy.0 * CARBON_INTENSITY)
}

/// Get real-time carbon intensity for a location (stub for now)
pub async fn get_carbon_intensity(_location: &str) -> f64 {
    // TODO: Integrate with ElectricityMaps or WattTime API
    // For now, return global average
    475.0 // gCO2e/kWh
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_carbon_estimation() {
        // 100 J should produce ~0.0132 gCO2e with 475 intensity
        let energy = Energy::joules(100.0);
        let carbon = estimate_carbon(energy);

        assert!(carbon.0 > 0.01 && carbon.0 < 0.02);
    }
}
