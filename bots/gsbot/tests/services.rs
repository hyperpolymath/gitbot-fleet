// SPDX-License-Identifier: MPL-2.0
//! Port of tests/unit/test_services.py and the spirit of
//! tests/integration/test_bot_commands.py — service queries and the
//! sustainability analyzer over an in-memory database.

use gsbot::{db, fixtures};
use gsbot::models::MaterialType;
use gsbot::services::{BrandService, GarmentService, MaterialService, NewMaterial, UserService};
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

#[tokio::test]
async fn literal_queries_keep_bound_values_and_joined_materials() {
    let p = pool().await;
    fixtures::load_all_fixtures(&p).await.unwrap();

    let garment = GarmentService::get_by_name(&p, "Linen Summer Shirt")
        .await.unwrap().unwrap();
    let materials = GarmentService::materials_of(&p, garment.id).await.unwrap();
    assert_eq!(materials.len(), 1);
    assert_eq!(materials[0].name, "Linen");
    assert!(BrandService::get_by_name(&p, "Patagonia").await.unwrap().is_some());
    assert!(!MaterialService::get_all(&p, 0, 2).await.unwrap().is_empty());
    let user = UserService::create(&p, 1234, "test user").await.unwrap();
    assert_eq!(UserService::get_by_discord_id(&p, 1234).await.unwrap().unwrap().id, user.id);
    assert!(!UserService::get_leaderboard(&p, 1).await.unwrap().is_empty());

    // A SQL fragment must be interpreted only as a bound search value.
    let injection = "' OR 1=1 --";
    assert!(MaterialService::search(&p, injection).await.unwrap().is_empty());
    assert!(GarmentService::search(&p, injection).await.unwrap().is_empty());
    assert!(BrandService::get_by_name(&p, injection).await.unwrap().is_none());
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

#[tokio::test]
async fn material_queries_preserve_nullable_columns_and_description_only_matches() {
    let p = pool().await;
    let nullable = MaterialService::create(&p,
        new_material("Plain", MaterialType::Natural, None)).await.unwrap();
    let described = MaterialService::create(&p,
        new_material("O'Brien", MaterialType::Organic, Some("Unique NEEDLE fiber"))).await.unwrap();
    assert!(nullable.description.is_none());
    assert!(nullable.water_liters_per_kg.is_none());
    assert!(nullable.co2_kg_per_kg.is_none());
    assert!(nullable.drying_method.is_none());
    let matches = MaterialService::search(&p, "needle").await.unwrap();
    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].id, described.id);
    let quoted = MaterialService::get_by_name(&p, "O'Brien").await.unwrap().unwrap();
    assert_eq!(quoted.id, described.id);
    assert_eq!(quoted.material_type, "organic");
    assert_eq!(quoted.description.as_deref(), Some("Unique NEEDLE fiber"));
    assert!(MaterialService::get_by_name(&p, "absent").await.unwrap().is_none());
    assert!(MaterialService::get_all(&p, 0, 0).await.unwrap().is_empty());
    assert!(MaterialService::get_all(&p, 2, 10).await.unwrap().is_empty());
    assert_eq!(MaterialService::get_all(&p, 0, 1).await.unwrap().len(), 1);
}

#[tokio::test]
async fn user_projection_preserves_updates_and_leaderboard_order_and_limit() {
    let p = pool().await;
    assert!(UserService::get_by_discord_id(&p, 99).await.unwrap().is_none());
    assert!(UserService::get_leaderboard(&p, 5).await.unwrap().is_empty());
    for (id, points) in [(10, 5), (20, 30), (30, 15)] {
        let mut user = UserService::create(&p, id, "O'Brien").await.unwrap();
        user.sustainability_points = points;
        user.preferred_materials = Some("linen".into());
        user.daily_tips_enabled = false;
        UserService::update(&p, &user).await.unwrap();
    }
    let leaders = UserService::get_leaderboard(&p, 2).await.unwrap();
    assert_eq!(leaders.iter().map(|u| u.discord_id).collect::<Vec<_>>(), vec![20, 30]);
    let user = UserService::get_by_discord_id(&p, 20).await.unwrap().unwrap();
    assert_eq!(user.username.as_deref(), Some("O'Brien"));
    assert_eq!(user.preferred_materials.as_deref(), Some("linen"));
    assert_eq!(user.sustainability_points, 30);
    assert!(!user.daily_tips_enabled);
    assert!(UserService::get_leaderboard(&p, 0).await.unwrap().is_empty());
}

#[tokio::test]
async fn garment_search_matches_category_and_description_with_nulls_present() {
    let p = pool().await;
    for (name, category, description) in [
        ("Plain shirt", "Tops", None),
        ("Warm coat", "Outerwear", Some("Winter NEEDLE insulation")),
    ] {
        sqlx::query("INSERT INTO garments (created_at, updated_at, name, category, description) VALUES ('t', 't', ?1, ?2, ?3)")
            .bind(name).bind(category).bind(description).execute(&p).await.unwrap();
    }
    let category = GarmentService::search(&p, "TOPS").await.unwrap();
    assert_eq!(category.len(), 1);
    assert_eq!(category[0].name, "Plain shirt");
    assert!(category[0].description.is_none());
    assert!(category[0].sustainability_score.is_none());
    let description = GarmentService::search(&p, "needle").await.unwrap();
    assert_eq!(description.len(), 1);
    assert_eq!(description[0].name, "Warm coat");
    assert!(GarmentService::get_by_name(&p, "missing").await.unwrap().is_none());
    assert!(GarmentService::materials_of(&p, category[0].id).await.unwrap().is_empty());
}

#[tokio::test]
async fn brand_projection_keeps_ratings_and_booleans_and_applies_top_limit() {
    let p = pool().await;
    for (name, rating, certified) in [("Low", 12.0, false), ("High", 98.0, true), ("Middle", 50.0, false)] {
        sqlx::query("INSERT INTO brands (created_at, updated_at, name, overall_rating, is_certified_bcorp) VALUES ('t', 't', ?1, ?2, ?3)")
            .bind(name).bind(rating).bind(certified).execute(&p).await.unwrap();
    }
    let top = BrandService::get_top_rated(&p, 2).await.unwrap();
    assert_eq!(top.iter().map(|b| b.name.as_str()).collect::<Vec<_>>(), vec!["High", "Middle"]);
    assert!(top[0].is_certified_bcorp);
    assert!(!top[1].is_certified_bcorp);
    let found = BrandService::get_by_name(&p, "HIGH").await.unwrap().unwrap();
    assert_eq!(found.id, top[0].id);
    assert_eq!(found.overall_rating, 98.0);
    assert!(found.website.is_none());
    assert!(BrandService::get_by_name(&p, "missing").await.unwrap().is_none());
    assert!(BrandService::get_top_rated(&p, 0).await.unwrap().is_empty());
}
