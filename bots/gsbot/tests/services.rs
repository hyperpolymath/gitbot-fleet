// SPDX-License-Identifier: PMPL-1.0-or-later
//! Port of tests/unit/test_services.py and the spirit of
//! tests/integration/test_bot_commands.py — service queries and the
//! sustainability analyzer over an in-memory database.

use gsbot::db;
use gsbot::models::MaterialType;
use gsbot::services::{MaterialService, NewMaterial};
use gsbot::sustainability as analyzer;

async fn pool() -> sqlx::SqlitePool {
    let p = sqlx::SqlitePool::connect("sqlite::memory:").await.unwrap();
    db::init_db(&p).await.unwrap();
    p
}

fn new_material(name: &str, ty: MaterialType, desc: Option<&str>) -> NewMaterial {
    NewMaterial {
        name: name.into(),
        material_type: ty,
        description: desc.map(|s| s.to_string()),
        water_usage_score: 50.0,
        carbon_footprint_score: 50.0,
        biodegradability_score: 50.0,
        chemical_usage_score: 50.0,
        energy_consumption_score: 50.0,
        water_liters_per_kg: None,
        co2_kg_per_kg: None,
        energy_mj_per_kg: None,
        is_biodegradable: None,
        recycling_potential: None,
        durability_rating: None,
        typical_wash_temp: None,
        drying_method: None,
    }
}

#[tokio::test]
async fn create_and_get_by_name_case_insensitive() {
    let p = pool().await;
    MaterialService::create(&p, new_material("Organic Cotton", MaterialType::Organic, None))
        .await
        .unwrap();
    let found = MaterialService::get_by_name(&p, "organic").await.unwrap();
    assert!(found.is_some());
    assert!(found.unwrap().name.contains("Cotton"));
}

#[tokio::test]
async fn search_matches_name_and_description() {
    let p = pool().await;
    for (n, t, d) in [
        ("Cotton", MaterialType::Natural, "Natural fiber"),
        ("Polyester", MaterialType::Synthetic, "Synthetic fiber"),
        ("Organic Cotton", MaterialType::Organic, "Organic natural fiber"),
    ] {
        MaterialService::create(&p, new_material(n, t, Some(d)))
            .await
            .unwrap();
    }
    let results = MaterialService::search(&p, "cotton").await.unwrap();
    assert_eq!(results.len(), 2);
}

#[test]
fn impact_category_thresholds() {
    assert!(analyzer::get_impact_category(90.0).contains("Very Low"));
    assert!(analyzer::get_impact_category(70.0).contains("Low"));
    assert!(analyzer::get_impact_category(50.0).contains("Medium"));
    assert!(analyzer::get_impact_category(30.0).contains("High"));
}

#[test]
fn sustainability_tips_nonempty() {
    let tips = analyzer::get_sustainability_tips();
    assert!(!tips.is_empty());
}
