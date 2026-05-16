// SPDX-License-Identifier: PMPL-1.0-or-later
//! Database services — faithful port of `services/database.py`. Each
//! `*Service` is a stateless namespace of queries over the pool, mirroring
//! the original static-method classes (case-insensitive `ilike` →
//! `LOWER(col) LIKE LOWER(...)`).

use anyhow::Result;
use sqlx::SqlitePool;

use crate::db::now;
use crate::models::{Brand, Garment, Material, User};

const MATERIAL_COLS: &str = "id, name, material_type, description, water_usage_score, \
    carbon_footprint_score, biodegradability_score, chemical_usage_score, \
    energy_consumption_score, water_liters_per_kg, co2_kg_per_kg, energy_mj_per_kg, \
    is_biodegradable, recycling_potential, durability_rating, typical_wash_temp, drying_method";

const GARMENT_COLS: &str = "id, name, category, description, typical_weight_kg, \
    expected_lifespan_years, typical_wears, care_instructions, washing_frequency, \
    sustainability_score";

const BRAND_COLS: &str = "id, name, description, website, overall_rating, \
    environmental_rating, labor_rating, animal_welfare_rating, is_certified_bcorp, \
    is_fair_trade, is_organic_certified, uses_recycled_materials, carbon_neutral, \
    country, price_range, transparency_score, good_on_you_rating";

const USER_COLS: &str = "id, discord_id, username, preferred_materials, budget_range, \
    sustainability_priority, queries_count, sustainability_points, level, daily_tips_enabled";

pub struct MaterialService;

impl MaterialService {
    pub async fn get_by_name(db: &SqlitePool, name: &str) -> Result<Option<Material>> {
        let q = format!(
            "SELECT {MATERIAL_COLS} FROM materials \
             WHERE LOWER(name) LIKE '%' || LOWER(?1) || '%' LIMIT 1"
        );
        Ok(sqlx::query_as::<_, Material>(&q)
            .bind(name)
            .fetch_optional(db)
            .await?)
    }

    pub async fn get_all(db: &SqlitePool, skip: i64, limit: i64) -> Result<Vec<Material>> {
        let q = format!("SELECT {MATERIAL_COLS} FROM materials LIMIT ?1 OFFSET ?2");
        Ok(sqlx::query_as::<_, Material>(&q)
            .bind(limit)
            .bind(skip)
            .fetch_all(db)
            .await?)
    }

    pub async fn search(db: &SqlitePool, query: &str) -> Result<Vec<Material>> {
        let q = format!(
            "SELECT {MATERIAL_COLS} FROM materials \
             WHERE LOWER(name) LIKE '%' || LOWER(?1) || '%' \
                OR LOWER(COALESCE(description,'')) LIKE '%' || LOWER(?1) || '%'"
        );
        Ok(sqlx::query_as::<_, Material>(&q)
            .bind(query)
            .fetch_all(db)
            .await?)
    }

    /// Insert a material, returning the stored row. Mirrors
    /// `MaterialService.create`.
    #[allow(clippy::too_many_arguments)]
    pub async fn create(db: &SqlitePool, m: NewMaterial) -> Result<Material> {
        let ts = now();
        let id = sqlx::query(
            "INSERT INTO materials (created_at, updated_at, name, material_type, description, \
             water_usage_score, carbon_footprint_score, biodegradability_score, \
             chemical_usage_score, energy_consumption_score, water_liters_per_kg, \
             co2_kg_per_kg, energy_mj_per_kg, is_biodegradable, recycling_potential, \
             durability_rating, typical_wash_temp, drying_method) \
             VALUES (?1,?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13,?14,?15,?16,?17)",
        )
        .bind(&ts)
        .bind(&m.name)
        .bind(m.material_type.value())
        .bind(&m.description)
        .bind(m.water_usage_score)
        .bind(m.carbon_footprint_score)
        .bind(m.biodegradability_score)
        .bind(m.chemical_usage_score)
        .bind(m.energy_consumption_score)
        .bind(m.water_liters_per_kg)
        .bind(m.co2_kg_per_kg)
        .bind(m.energy_mj_per_kg)
        .bind(&m.is_biodegradable)
        .bind(&m.recycling_potential)
        .bind(m.durability_rating)
        .bind(&m.typical_wash_temp)
        .bind(&m.drying_method)
        .execute(db)
        .await?
        .last_insert_rowid();
        let q = format!("SELECT {MATERIAL_COLS} FROM materials WHERE id = ?1");
        Ok(sqlx::query_as::<_, Material>(&q).bind(id).fetch_one(db).await?)
    }

    pub async fn count(db: &SqlitePool) -> Result<i64> {
        Ok(sqlx::query_scalar("SELECT COUNT(*) FROM materials")
            .fetch_one(db)
            .await?)
    }
}

pub struct GarmentService;

impl GarmentService {
    pub async fn get_by_name(db: &SqlitePool, name: &str) -> Result<Option<Garment>> {
        let q = format!(
            "SELECT {GARMENT_COLS} FROM garments \
             WHERE LOWER(name) LIKE '%' || LOWER(?1) || '%' LIMIT 1"
        );
        Ok(sqlx::query_as::<_, Garment>(&q)
            .bind(name)
            .fetch_optional(db)
            .await?)
    }

    pub async fn search(db: &SqlitePool, query: &str) -> Result<Vec<Garment>> {
        let q = format!(
            "SELECT {GARMENT_COLS} FROM garments \
             WHERE LOWER(name) LIKE '%' || LOWER(?1) || '%' \
                OR LOWER(category) LIKE '%' || LOWER(?1) || '%' \
                OR LOWER(COALESCE(description,'')) LIKE '%' || LOWER(?1) || '%'"
        );
        Ok(sqlx::query_as::<_, Garment>(&q)
            .bind(query)
            .fetch_all(db)
            .await?)
    }

    /// Materials associated with a garment (the `garment_materials` M2M).
    pub async fn materials_of(db: &SqlitePool, garment_id: i64) -> Result<Vec<Material>> {
        let q = format!(
            "SELECT {} FROM materials m \
             JOIN garment_materials gm ON gm.material_id = m.id \
             WHERE gm.garment_id = ?1",
            MATERIAL_COLS
                .split(", ")
                .map(|c| format!("m.{c}"))
                .collect::<Vec<_>>()
                .join(", ")
        );
        Ok(sqlx::query_as::<_, Material>(&q)
            .bind(garment_id)
            .fetch_all(db)
            .await?)
    }

    /// Sustainable alternatives. Mirrors `GarmentService.get_alternatives`:
    /// same category, different id, stored score above the current
    /// computed score, top 5 descending.
    pub async fn get_alternatives(
        db: &SqlitePool,
        garment: &Garment,
        current_score: f64,
    ) -> Result<Vec<Garment>> {
        let q = format!(
            "SELECT {GARMENT_COLS} FROM garments \
             WHERE category = ?1 AND id <> ?2 AND sustainability_score > ?3 \
             ORDER BY sustainability_score DESC LIMIT 5"
        );
        Ok(sqlx::query_as::<_, Garment>(&q)
            .bind(&garment.category)
            .bind(garment.id)
            .bind(current_score)
            .fetch_all(db)
            .await?)
    }

    pub async fn count(db: &SqlitePool) -> Result<i64> {
        Ok(sqlx::query_scalar("SELECT COUNT(*) FROM garments")
            .fetch_one(db)
            .await?)
    }
}

pub struct BrandService;

impl BrandService {
    pub async fn get_by_name(db: &SqlitePool, name: &str) -> Result<Option<Brand>> {
        let q = format!(
            "SELECT {BRAND_COLS} FROM brands \
             WHERE LOWER(name) LIKE '%' || LOWER(?1) || '%' LIMIT 1"
        );
        Ok(sqlx::query_as::<_, Brand>(&q)
            .bind(name)
            .fetch_optional(db)
            .await?)
    }

    pub async fn get_top_rated(db: &SqlitePool, limit: i64) -> Result<Vec<Brand>> {
        let q = format!(
            "SELECT {BRAND_COLS} FROM brands ORDER BY overall_rating DESC LIMIT ?1"
        );
        Ok(sqlx::query_as::<_, Brand>(&q).bind(limit).fetch_all(db).await?)
    }

    pub async fn count(db: &SqlitePool) -> Result<i64> {
        Ok(sqlx::query_scalar("SELECT COUNT(*) FROM brands")
            .fetch_one(db)
            .await?)
    }
}

pub struct UserService;

impl UserService {
    pub async fn get_by_discord_id(db: &SqlitePool, discord_id: i64) -> Result<Option<User>> {
        let q = format!("SELECT {USER_COLS} FROM users WHERE discord_id = ?1");
        Ok(sqlx::query_as::<_, User>(&q)
            .bind(discord_id)
            .fetch_optional(db)
            .await?)
    }

    pub async fn create(db: &SqlitePool, discord_id: i64, username: &str) -> Result<User> {
        let ts = now();
        sqlx::query(
            "INSERT INTO users (created_at, updated_at, discord_id, username, \
             queries_count, sustainability_points, level, daily_tips_enabled) \
             VALUES (?1,?1,?2,?3,0,0,1,1)",
        )
        .bind(&ts)
        .bind(discord_id)
        .bind(username)
        .execute(db)
        .await?;
        Ok(Self::get_by_discord_id(db, discord_id)
            .await?
            .expect("user just inserted"))
    }

    pub async fn get_or_create(
        db: &SqlitePool,
        discord_id: i64,
        username: &str,
    ) -> Result<User> {
        match Self::get_by_discord_id(db, discord_id).await? {
            Some(u) => Ok(u),
            None => Self::create(db, discord_id, username).await,
        }
    }

    /// Persist the mutable user fields. Mirrors `UserService.update`
    /// (the Python version relies on the ORM session tracking the dirty
    /// instance; here we write the columns the bot can change).
    pub async fn update(db: &SqlitePool, user: &User) -> Result<()> {
        sqlx::query(
            "UPDATE users SET updated_at = ?1, username = ?2, preferred_materials = ?3, \
             budget_range = ?4, sustainability_priority = ?5, queries_count = ?6, \
             sustainability_points = ?7, level = ?8, daily_tips_enabled = ?9 \
             WHERE id = ?10",
        )
        .bind(now())
        .bind(&user.username)
        .bind(&user.preferred_materials)
        .bind(&user.budget_range)
        .bind(&user.sustainability_priority)
        .bind(user.queries_count)
        .bind(user.sustainability_points)
        .bind(user.level)
        .bind(user.daily_tips_enabled)
        .bind(user.id)
        .execute(db)
        .await?;
        Ok(())
    }

    pub async fn get_leaderboard(db: &SqlitePool, limit: i64) -> Result<Vec<User>> {
        let q = format!(
            "SELECT {USER_COLS} FROM users ORDER BY sustainability_points DESC LIMIT ?1"
        );
        Ok(sqlx::query_as::<_, User>(&q).bind(limit).fetch_all(db).await?)
    }

    pub async fn count(db: &SqlitePool) -> Result<i64> {
        Ok(sqlx::query_scalar("SELECT COUNT(*) FROM users")
            .fetch_one(db)
            .await?)
    }
}

/// Insert payload for [`MaterialService::create`].
#[derive(Debug, Clone)]
pub struct NewMaterial {
    pub name: String,
    pub material_type: crate::models::MaterialType,
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
