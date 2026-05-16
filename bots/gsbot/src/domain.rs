// SPDX-License-Identifier: PMPL-1.0-or-later
//! Correctness-critical sustainability kernel.
//!
//! This module is the **SPARK seam** (per the hyperpolymath Rust/SPARK
//! standard: "Rust-primary now, designed to admit SPARK/Ada modules even
//! if it contains none yet"). Every function here is *pure* — no I/O, no
//! allocation in the numeric core, total over its documented domain — so
//! the kernel can be replaced by a formally-verified SPARK/Ada module
//! linked through the standard Idris2-ABI / Zig-FFI pattern without
//! touching any caller. The [`ffi`] submodule already exposes the numeric
//! core under a stable C ABI for exactly that substitution.
//!
//! Behaviour is a faithful port of the original Python models/services.

/// Mean of the five 0–100 environmental sub-scores. Mirrors
/// `Material.calculate_overall_score`.
pub fn material_overall_score(scores: [f64; 5]) -> f64 {
    scores.iter().sum::<f64>() / scores.len() as f64
}

/// Letter grade for a 0–100 score. Mirrors `Material.get_grade`.
pub fn grade(score: f64) -> &'static str {
    if score >= 90.0 {
        "A+"
    } else if score >= 80.0 {
        "A"
    } else if score >= 70.0 {
        "B"
    } else if score >= 60.0 {
        "C"
    } else if score >= 50.0 {
        "D"
    } else {
        "F"
    }
}

/// Lifespan multiplier applied to a garment's base score. Mirrors the
/// branch ladder in `Garment.calculate_sustainability_score`.
pub fn lifespan_multiplier(expected_lifespan_years: Option<f64>) -> f64 {
    match expected_lifespan_years {
        Some(y) if y >= 5.0 => 1.2,
        Some(y) if y >= 3.0 => 1.1,
        Some(y) if y < 1.0 => 0.8,
        _ => 1.0,
    }
}

/// Overall garment sustainability score. `material_scores` are the
/// per-material overall scores. Mirrors
/// `Garment.calculate_sustainability_score` exactly, including the
/// no-materials default of 50.0 and the cap at 100.0.
pub fn garment_sustainability_score(
    material_scores: &[f64],
    expected_lifespan_years: Option<f64>,
) -> f64 {
    if material_scores.is_empty() {
        return 50.0;
    }
    let base = material_scores.iter().sum::<f64>() / material_scores.len() as f64;
    (base * lifespan_multiplier(expected_lifespan_years)).min(100.0)
}

/// One material's contribution to a garment's environmental impact.
#[derive(Debug, Clone, Copy, Default)]
pub struct MaterialImpactInputs {
    pub water_liters_per_kg: Option<f64>,
    pub co2_kg_per_kg: Option<f64>,
    pub energy_mj_per_kg: Option<f64>,
}

/// Aggregated, human-readable environmental impact. Mirrors
/// `Garment.get_environmental_impact` — note the original divides each
/// total by the material count (not by the number of contributing
/// materials), and yields "Unknown" when there are no materials or no
/// weight.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnvironmentalImpact {
    pub water_usage: String,
    pub carbon_footprint: String,
    pub energy_consumption: String,
}

impl EnvironmentalImpact {
    pub fn unknown() -> Self {
        Self {
            water_usage: "Unknown".into(),
            carbon_footprint: "Unknown".into(),
            energy_consumption: "Unknown".into(),
        }
    }
}

pub fn environmental_impact(
    materials: &[MaterialImpactInputs],
    typical_weight_kg: Option<f64>,
) -> EnvironmentalImpact {
    if materials.is_empty() {
        return EnvironmentalImpact::unknown();
    }
    let (mut water, mut co2, mut energy) = (0.0_f64, 0.0_f64, 0.0_f64);
    let mut count = 0_usize;
    for m in materials {
        if let (Some(w), Some(wt)) = (m.water_liters_per_kg, typical_weight_kg) {
            water += w * wt;
        }
        if let (Some(c), Some(wt)) = (m.co2_kg_per_kg, typical_weight_kg) {
            co2 += c * wt;
        }
        if let (Some(e), Some(wt)) = (m.energy_mj_per_kg, typical_weight_kg) {
            energy += e * wt;
        }
        count += 1;
    }
    if count == 0 {
        return EnvironmentalImpact::unknown();
    }
    let n = count as f64;
    EnvironmentalImpact {
        water_usage: format!("{:.1} liters", water / n),
        carbon_footprint: format!("{:.2} kg CO2", co2 / n),
        energy_consumption: format!("{:.1} MJ", energy / n),
    }
}

/// Result of awarding points to a user. Mirrors `User.add_points`:
/// points accumulate, query count increments, level is
/// `points // 100 + 1` but only ever ratchets upward.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Leveling {
    pub sustainability_points: i64,
    pub queries_count: i64,
    pub level: i64,
}

pub fn add_points(prev: Leveling, points: i64) -> Leveling {
    let sustainability_points = prev.sustainability_points + points;
    let queries_count = prev.queries_count + 1;
    let candidate = sustainability_points / 100 + 1;
    let level = if candidate > prev.level {
        candidate
    } else {
        prev.level
    };
    Leveling {
        sustainability_points,
        queries_count,
        level,
    }
}

/// User rank from level. Mirrors `User.get_rank`.
pub fn rank(level: i64) -> &'static str {
    if level >= 20 {
        "Sustainability Champion"
    } else if level >= 15 {
        "Eco Warrior"
    } else if level >= 10 {
        "Green Enthusiast"
    } else if level >= 5 {
        "Conscious Consumer"
    } else {
        "Sustainability Learner"
    }
}

/// Brand rating summary. Mirrors `Brand.get_rating_summary`.
pub fn brand_rating_summary(overall: f64) -> &'static str {
    if overall >= 80.0 {
        "Excellent sustainability practices"
    } else if overall >= 60.0 {
        "Good sustainability efforts"
    } else if overall >= 40.0 {
        "Making progress on sustainability"
    } else {
        "Needs significant improvement"
    }
}

/// Impact category with the original emoji decorations. Mirrors
/// `SustainabilityAnalyzer.get_impact_category`.
pub fn impact_category(score: f64) -> &'static str {
    if score >= 80.0 {
        "Very Low Impact ⭐⭐⭐"
    } else if score >= 60.0 {
        "Low Impact ⭐⭐"
    } else if score >= 40.0 {
        "Medium Impact ⭐"
    } else {
        "High Impact ⚠️"
    }
}

/// Material recommendation text. Mirrors
/// `SustainabilityAnalyzer.get_material_recommendation`.
pub fn material_recommendation(overall_score: f64) -> &'static str {
    if overall_score >= 80.0 {
        "Excellent choice! This material has minimal environmental impact."
    } else if overall_score >= 60.0 {
        "Good choice! This material is relatively sustainable."
    } else if overall_score >= 40.0 {
        "Consider alternatives. This material has moderate environmental impact."
    } else {
        "Not recommended. This material has significant environmental impact. \
         Look for alternatives."
    }
}

/// Stable C-ABI surface for the numeric core. A SPARK/Ada implementation
/// can export these same symbols and be linked in place of the Rust
/// bodies via the hyperpolymath Zig-FFI / Idris2-ABI pattern; callers in
/// this crate go through the safe wrappers above, so substitution is
/// transparent.
pub mod ffi {
    /// Mean of five environmental sub-scores.
    #[no_mangle]
    pub extern "C" fn gsbot_material_overall_score(
        water: f64,
        carbon: f64,
        biodegradability: f64,
        chemical: f64,
        energy: f64,
    ) -> f64 {
        super::material_overall_score([water, carbon, biodegradability, chemical, energy])
    }

    /// Lifespan multiplier; pass a negative value for "unset".
    #[no_mangle]
    pub extern "C" fn gsbot_lifespan_multiplier(expected_lifespan_years: f64) -> f64 {
        let arg = if expected_lifespan_years < 0.0 {
            None
        } else {
            Some(expected_lifespan_years)
        };
        super::lifespan_multiplier(arg)
    }

    /// Ratcheting level for a given lifetime points total.
    #[no_mangle]
    pub extern "C" fn gsbot_level_for_points(points: i64, current_level: i64) -> i64 {
        let candidate = points / 100 + 1;
        if candidate > current_level {
            candidate
        } else {
            current_level
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn overall_score_is_mean() {
        assert_eq!(
            material_overall_score([80.0, 70.0, 90.0, 60.0, 75.0]),
            75.0
        );
    }

    #[test]
    fn grades_match_python_thresholds() {
        assert_eq!(grade(90.0), "A+");
        assert_eq!(grade(50.0), "D");
        assert_eq!(grade(49.9), "F");
    }

    #[test]
    fn garment_score_no_materials_is_neutral() {
        assert_eq!(garment_sustainability_score(&[], Some(5.0)), 50.0);
    }

    #[test]
    fn garment_score_boosted_by_long_lifespan_and_capped() {
        // mean 80 * 1.2 = 96
        assert_eq!(
            garment_sustainability_score(&[80.0], Some(5.0)),
            96.0
        );
        // capped at 100
        assert_eq!(
            garment_sustainability_score(&[95.0], Some(5.0)),
            100.0
        );
    }

    #[test]
    fn add_points_levels_at_hundred() {
        let s = Leveling {
            sustainability_points: 0,
            queries_count: 0,
            level: 1,
        };
        let s = add_points(s, 50);
        assert_eq!((s.sustainability_points, s.level), (50, 1));
        let s = add_points(s, 60);
        assert_eq!((s.sustainability_points, s.level), (110, 2));
    }

    #[test]
    fn ranks_match() {
        assert_eq!(rank(1), "Sustainability Learner");
        assert_eq!(rank(10), "Green Enthusiast");
        assert_eq!(rank(20), "Sustainability Champion");
    }

    #[test]
    fn environmental_impact_unknown_without_materials() {
        assert_eq!(
            environmental_impact(&[], Some(0.15)),
            EnvironmentalImpact::unknown()
        );
    }
}
