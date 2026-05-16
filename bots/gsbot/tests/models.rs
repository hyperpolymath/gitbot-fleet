// SPDX-License-Identifier: PMPL-1.0-or-later
//! Port of tests/unit/test_models.py — model behaviour over the domain
//! kernel, plus a round-trip through an in-memory database.

use gsbot::db;
use gsbot::models::{Brand, Material, MaterialType, User};
use gsbot::services::{MaterialService, NewMaterial};

fn material(scores: [f64; 5]) -> Material {
    Material {
        id: 0,
        name: "Test".into(),
        material_type: MaterialType::Natural.value().into(),
        description: None,
        water_usage_score: scores[0],
        carbon_footprint_score: scores[1],
        biodegradability_score: scores[2],
        chemical_usage_score: scores[3],
        energy_consumption_score: scores[4],
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

#[test]
fn calculate_overall_score_is_mean() {
    let m = material([80.0, 70.0, 90.0, 60.0, 75.0]);
    assert_eq!(m.calculate_overall_score(), 75.0);
}

#[test]
fn grades() {
    assert_eq!(material([90.0; 5]).get_grade(), "A+");
    assert_eq!(material([50.0; 5]).get_grade(), "D");
}

#[test]
fn brand_badges_and_summary() {
    let mut b = Brand {
        id: 0,
        name: "Test".into(),
        description: None,
        website: None,
        overall_rating: 85.0,
        environmental_rating: 0.0,
        labor_rating: 0.0,
        animal_welfare_rating: 0.0,
        is_certified_bcorp: true,
        is_fair_trade: true,
        is_organic_certified: false,
        uses_recycled_materials: false,
        carbon_neutral: false,
        country: None,
        price_range: None,
        transparency_score: None,
        good_on_you_rating: None,
    };
    let badges = b.get_certification_badges();
    assert!(badges.contains(&"B Corp Certified"));
    assert!(badges.contains(&"Fair Trade"));
    assert_eq!(badges.len(), 2);
    assert!(b.get_rating_summary().contains("Excellent"));
    b.overall_rating = 45.0;
    assert!(b.get_rating_summary().to_lowercase().contains("progress"));
}

#[test]
fn user_points_and_rank() {
    let mut u = User {
        id: 0,
        discord_id: 123,
        username: Some("t".into()),
        preferred_materials: None,
        budget_range: None,
        sustainability_priority: None,
        queries_count: 0,
        sustainability_points: 0,
        level: 1,
        daily_tips_enabled: true,
    };
    u.add_points(50);
    assert_eq!((u.sustainability_points, u.level), (50, 1));
    u.add_points(60);
    assert_eq!((u.sustainability_points, u.level), (110, 2));
    assert_eq!(u.get_rank(), "Sustainability Learner");
    u.level = 10;
    assert_eq!(u.get_rank(), "Green Enthusiast");
    u.level = 20;
    assert_eq!(u.get_rank(), "Sustainability Champion");
}

#[tokio::test]
async fn material_round_trips_through_db() {
    let pool = sqlx::SqlitePool::connect("sqlite::memory:").await.unwrap();
    db::init_db(&pool).await.unwrap();

    let created = MaterialService::create(
        &pool,
        NewMaterial {
            name: "Test Cotton".into(),
            material_type: MaterialType::Natural,
            description: None,
            water_usage_score: 70.0,
            carbon_footprint_score: 65.0,
            biodegradability_score: 90.0,
            chemical_usage_score: 75.0,
            energy_consumption_score: 70.0,
            water_liters_per_kg: None,
            co2_kg_per_kg: None,
            energy_mj_per_kg: None,
            is_biodegradable: None,
            recycling_potential: None,
            durability_rating: None,
            typical_wash_temp: None,
            drying_method: None,
        },
    )
    .await
    .unwrap();

    assert!(created.id > 0);
    assert_eq!(created.name, "Test Cotton");
}
