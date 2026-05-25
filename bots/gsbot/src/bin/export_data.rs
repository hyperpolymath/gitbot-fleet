// SPDX-License-Identifier: MPL-2.0
//! `gsbot-export-data` — port of `scripts/export_data.py`. Exports
//! materials/garments/brands to timestamped JSON plus a combined file.

use anyhow::Result;
use serde_json::json;

use gsbot::config::Config;
use gsbot::db;
use gsbot::models::{Brand, Garment, Material};
use gsbot::services::GarmentService;

#[tokio::main]
async fn main() -> Result<()> {
    let config = Config::from_env();
    let pool = db::connect(&config.sqlx_url()).await?;

    let materials: Vec<Material> = sqlx::query_as(
        "SELECT id, name, material_type, description, water_usage_score, \
         carbon_footprint_score, biodegradability_score, chemical_usage_score, \
         energy_consumption_score, water_liters_per_kg, co2_kg_per_kg, \
         energy_mj_per_kg, is_biodegradable, recycling_potential, \
         durability_rating, typical_wash_temp, drying_method FROM materials",
    )
    .fetch_all(&pool)
    .await?;

    let garments: Vec<Garment> = sqlx::query_as(
        "SELECT id, name, category, description, typical_weight_kg, \
         expected_lifespan_years, typical_wears, care_instructions, \
         washing_frequency, sustainability_score FROM garments",
    )
    .fetch_all(&pool)
    .await?;

    let brands: Vec<Brand> = sqlx::query_as(
        "SELECT id, name, description, website, overall_rating, \
         environmental_rating, labor_rating, animal_welfare_rating, \
         is_certified_bcorp, is_fair_trade, is_organic_certified, \
         uses_recycled_materials, carbon_neutral, country, price_range, \
         transparency_score, good_on_you_rating FROM brands",
    )
    .fetch_all(&pool)
    .await?;

    let materials_json: Vec<_> = materials
        .iter()
        .map(|m| {
            json!({
                "name": m.name,
                "type": m.material_type,
                "description": m.description,
                "scores": {
                    "water_usage": m.water_usage_score,
                    "carbon_footprint": m.carbon_footprint_score,
                    "biodegradability": m.biodegradability_score,
                    "chemical_usage": m.chemical_usage_score,
                    "energy_consumption": m.energy_consumption_score,
                },
                "metrics": {
                    "water_liters_per_kg": m.water_liters_per_kg,
                    "co2_kg_per_kg": m.co2_kg_per_kg,
                    "energy_mj_per_kg": m.energy_mj_per_kg,
                }
            })
        })
        .collect();

    let mut garments_json = Vec::new();
    for g in &garments {
        let mats = GarmentService::materials_of(&pool, g.id).await?;
        garments_json.push(json!({
            "name": g.name,
            "category": g.category,
            "description": g.description,
            "sustainability_score": g.sustainability_score,
            "materials": mats.iter().map(|m| m.name.clone()).collect::<Vec<_>>(),
        }));
    }

    let brands_json: Vec<_> = brands
        .iter()
        .map(|b| {
            json!({
                "name": b.name,
                "description": b.description,
                "website": b.website,
                "ratings": {
                    "overall": b.overall_rating,
                    "environmental": b.environmental_rating,
                    "labor": b.labor_rating,
                    "animal_welfare": b.animal_welfare_rating,
                },
                "certifications": b.get_certification_badges(),
            })
        })
        .collect();

    let out_dir = config.data_dir().join("exports");
    std::fs::create_dir_all(&out_dir)?;
    let ts = chrono::Local::now().format("%Y%m%d_%H%M%S").to_string();

    for (name, data) in [
        ("materials", &materials_json),
        ("garments", &garments_json),
        ("brands", &brands_json),
    ] {
        let path = out_dir.join(format!("{name}_{ts}.json"));
        std::fs::write(&path, serde_json::to_string_pretty(data)?)?;
    }
    let combined = json!({
        "materials": materials_json,
        "garments": garments_json,
        "brands": brands_json,
    });
    std::fs::write(
        out_dir.join(format!("all_data_{ts}.json")),
        serde_json::to_string_pretty(&combined)?,
    )?;

    println!("\n✅ Export completed successfully!");
    println!("   Materials: {}", materials_json.len());
    println!("   Garments: {}", garments_json.len());
    println!("   Brands: {}", brands_json.len());
    println!("\nFiles saved to: {}", out_dir.display());
    Ok(())
}
