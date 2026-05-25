// SPDX-License-Identifier: MPL-2.0
//! Sustainability analysis — faithful port of
//! `services/sustainability.py`. Pure scoring lives in [`crate::domain`];
//! this module holds the advisory text/lists and comparison shaping.

use crate::domain;
use crate::models::Material;

pub use crate::domain::impact_category as get_impact_category;

pub fn get_material_recommendation(material: &Material) -> &'static str {
    domain::material_recommendation(material.calculate_overall_score())
}

/// Care tips, in the original order: four base tips, then any
/// material-specific tips, then the two trailing tips. Mirrors
/// `generate_care_tips`.
pub fn generate_care_tips(material_names: &[String]) -> Vec<String> {
    let mut tips: Vec<String> = vec![
        "Wash in cold water to save energy and preserve fabric".into(),
        "Air dry when possible instead of using a tumble dryer".into(),
        "Turn garments inside out before washing to reduce fading".into(),
        "Use eco-friendly detergents to minimize chemical impact".into(),
    ];

    for name in material_names {
        let n = name.to_lowercase();
        if n.contains("cotton") {
            tips.push("Cotton: Iron while slightly damp for best results".into());
        } else if n.contains("wool") {
            tips.push("Wool: Hand wash or use wool cycle, lay flat to dry".into());
        } else if n.contains("synthetic") || n.contains("polyester") {
            tips.push(
                "Synthetic: Avoid high heat to prevent damage and microplastic shedding"
                    .into(),
            );
        }
    }

    tips.push("Don't overwash - air out garments between wears when possible".into());
    tips.push("Repair minor damage promptly to extend garment life".into());
    tips
}

/// One side of a pairwise material metric comparison.
#[derive(Debug, Clone)]
pub struct MetricComparison {
    pub a: f64,
    pub b: f64,
    pub better: String,
}

#[derive(Debug, Clone)]
pub struct MaterialComparison {
    pub material1: String,
    pub material2: String,
    pub overall1: f64,
    pub overall2: f64,
    pub water_usage: MetricComparison,
    pub carbon_footprint: MetricComparison,
    pub biodegradability: MetricComparison,
}

fn better(name1: &str, v1: f64, name2: &str, v2: f64) -> String {
    // Mirrors Python: `name1 if v1 > v2 else name2` (ties -> name2).
    if v1 > v2 { name1 } else { name2 }.to_string()
}

/// Mirrors `SustainabilityAnalyzer.compare_materials`.
pub fn compare_materials(m1: &Material, m2: &Material) -> MaterialComparison {
    MaterialComparison {
        material1: m1.name.clone(),
        material2: m2.name.clone(),
        overall1: m1.calculate_overall_score(),
        overall2: m2.calculate_overall_score(),
        water_usage: MetricComparison {
            a: m1.water_usage_score,
            b: m2.water_usage_score,
            better: better(&m1.name, m1.water_usage_score, &m2.name, m2.water_usage_score),
        },
        carbon_footprint: MetricComparison {
            a: m1.carbon_footprint_score,
            b: m2.carbon_footprint_score,
            better: better(
                &m1.name,
                m1.carbon_footprint_score,
                &m2.name,
                m2.carbon_footprint_score,
            ),
        },
        biodegradability: MetricComparison {
            a: m1.biodegradability_score,
            b: m2.biodegradability_score,
            better: better(
                &m1.name,
                m1.biodegradability_score,
                &m2.name,
                m2.biodegradability_score,
            ),
        },
    }
}

/// The twelve general sustainability tips, verbatim and in order.
/// Mirrors `get_sustainability_tips`.
pub fn get_sustainability_tips() -> Vec<&'static str> {
    vec![
        "Buy less, choose well, make it last - quality over quantity",
        "Support brands with transparent supply chains",
        "Look for certifications like GOTS, Fair Trade, or B Corp",
        "Choose natural or recycled materials when possible",
        "Repair and mend clothes instead of replacing them",
        "Buy second-hand or vintage when you can",
        "Organize clothing swaps with friends",
        "Donate or recycle clothes responsibly",
        "Consider renting formal wear instead of buying",
        "Learn basic sewing skills for repairs and alterations",
        "Store clothes properly to extend their life",
        "Avoid fast fashion and trends that quickly go out of style",
    ]
}
