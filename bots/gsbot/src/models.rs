// SPDX-License-Identifier: MPL-2.0
//! Data models — faithful port of the SQLAlchemy models. Behaviour-bearing
//! methods delegate to the pure [`crate::domain`] kernel.

use anyhow::{bail, Result};
use sqlx::FromRow;

use crate::domain;

/// Material category. Mirrors the Python `MaterialType` enum; the `.value`
/// strings are preserved for storage and display parity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MaterialType {
    Natural,
    Synthetic,
    SemiSynthetic,
    Recycled,
    Organic,
}

impl MaterialType {
    pub fn value(self) -> &'static str {
        match self {
            MaterialType::Natural => "natural",
            MaterialType::Synthetic => "synthetic",
            MaterialType::SemiSynthetic => "semi_synthetic",
            MaterialType::Recycled => "recycled",
            MaterialType::Organic => "organic",
        }
    }

    pub fn parse(s: &str) -> Result<Self> {
        Ok(match s {
            "natural" => MaterialType::Natural,
            "synthetic" => MaterialType::Synthetic,
            "semi_synthetic" => MaterialType::SemiSynthetic,
            "recycled" => MaterialType::Recycled,
            "organic" => MaterialType::Organic,
            other => bail!("unknown material_type: {other}"),
        })
    }
}

#[derive(Debug, Clone, FromRow)]
pub struct Material {
    pub id: i64,
    pub name: String,
    pub material_type: String,
    pub description: Option<String>,
    pub water_usage_score: f64,
    pub carbon_footprint_score: f64,
    pub biodegradability_score: f64,
    pub chemical_usage_score: f64,
    pub energy_consumption_score: f64,
    pub water_liters_per_kg: Option<f64>,
    pub co2_kg_per_kg: Option<f64>,
    pub energy_mj_per_kg: Option<f64>,
    pub is_biodegradable: Option<String>,
    pub recycling_potential: Option<String>,
    pub durability_rating: Option<f64>,
    pub typical_wash_temp: Option<String>,
    pub drying_method: Option<String>,
}

impl Material {
    pub fn calculate_overall_score(&self) -> f64 {
        domain::material_overall_score([
            self.water_usage_score,
            self.carbon_footprint_score,
            self.biodegradability_score,
            self.chemical_usage_score,
            self.energy_consumption_score,
        ])
    }

    pub fn get_grade(&self) -> &'static str {
        domain::grade(self.calculate_overall_score())
    }

    pub fn type_display(&self) -> String {
        // material_type.value.replace("_"," ").title()
        self.material_type
            .split('_')
            .map(|w| {
                let mut c = w.chars();
                match c.next() {
                    Some(f) => f.to_uppercase().chain(c).collect::<String>(),
                    None => String::new(),
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    }
}

#[derive(Debug, Clone, FromRow)]
pub struct Garment {
    pub id: i64,
    pub name: String,
    pub category: String,
    pub description: Option<String>,
    pub typical_weight_kg: Option<f64>,
    pub expected_lifespan_years: Option<f64>,
    pub typical_wears: Option<i64>,
    pub care_instructions: Option<String>,
    pub washing_frequency: Option<String>,
    pub sustainability_score: Option<f64>,
}

impl Garment {
    pub fn calculate_sustainability_score(&self, materials: &[Material]) -> f64 {
        let scores: Vec<f64> = materials.iter().map(Material::calculate_overall_score).collect();
        domain::garment_sustainability_score(&scores, self.expected_lifespan_years)
    }

    pub fn get_environmental_impact(&self, materials: &[Material]) -> domain::EnvironmentalImpact {
        let inputs: Vec<domain::MaterialImpactInputs> = materials
            .iter()
            .map(|m| domain::MaterialImpactInputs {
                water_liters_per_kg: m.water_liters_per_kg,
                co2_kg_per_kg: m.co2_kg_per_kg,
                energy_mj_per_kg: m.energy_mj_per_kg,
            })
            .collect();
        domain::environmental_impact(&inputs, self.typical_weight_kg)
    }
}

#[derive(Debug, Clone, FromRow)]
pub struct Brand {
    pub id: i64,
    pub name: String,
    pub description: Option<String>,
    pub website: Option<String>,
    pub overall_rating: f64,
    pub environmental_rating: f64,
    pub labor_rating: f64,
    pub animal_welfare_rating: f64,
    pub is_certified_bcorp: bool,
    pub is_fair_trade: bool,
    pub is_organic_certified: bool,
    pub uses_recycled_materials: bool,
    pub carbon_neutral: bool,
    pub country: Option<String>,
    pub price_range: Option<String>,
    pub transparency_score: Option<f64>,
    pub good_on_you_rating: Option<String>,
}

impl Brand {
    pub fn get_certification_badges(&self) -> Vec<&'static str> {
        let mut b = Vec::new();
        if self.is_certified_bcorp {
            b.push("B Corp Certified");
        }
        if self.is_fair_trade {
            b.push("Fair Trade");
        }
        if self.is_organic_certified {
            b.push("Organic Certified");
        }
        if self.uses_recycled_materials {
            b.push("Uses Recycled Materials");
        }
        if self.carbon_neutral {
            b.push("Carbon Neutral");
        }
        b
    }

    pub fn get_rating_summary(&self) -> &'static str {
        domain::brand_rating_summary(self.overall_rating)
    }
}

#[derive(Debug, Clone, FromRow)]
pub struct User {
    pub id: i64,
    pub discord_id: i64,
    pub username: Option<String>,
    pub preferred_materials: Option<String>,
    pub budget_range: Option<String>,
    pub sustainability_priority: Option<String>,
    pub queries_count: i64,
    pub sustainability_points: i64,
    pub level: i64,
    pub daily_tips_enabled: bool,
}

impl User {
    /// Apply the points/level mutation in place. Mirrors `User.add_points`.
    pub fn add_points(&mut self, points: i64) {
        let next = domain::add_points(
            domain::Leveling {
                sustainability_points: self.sustainability_points,
                queries_count: self.queries_count,
                level: self.level,
            },
            points,
        );
        self.sustainability_points = next.sustainability_points;
        self.queries_count = next.queries_count;
        self.level = next.level;
    }

    pub fn get_rank(&self) -> &'static str {
        domain::rank(self.level)
    }
}
