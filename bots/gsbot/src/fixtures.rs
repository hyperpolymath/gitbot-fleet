// SPDX-License-Identifier: MPL-2.0
//! Sample data fixtures — faithful port of `scripts/load_fixtures.py`.
//! Insert-if-absent by exact name; garment sustainability scores are
//! computed from associated materials exactly as the original did.

use anyhow::Result;
use sqlx::SqlitePool;

use crate::db::now;
use crate::domain;

struct M {
    name: &'static str,
    ty: &'static str,
    desc: &'static str,
    scores: [f64; 5], // water, carbon, biodeg, chemical, energy
    water_l: f64,
    co2: f64,
    energy_mj: f64,
    biodeg: &'static str,
    recycling: &'static str,
    durability: f64,
    wash_temp: &'static str,
    drying: &'static str,
}

const MATERIALS: &[M] = &[
    M { name: "Organic Cotton", ty: "organic", desc: "Cotton grown without synthetic pesticides or fertilizers, better for soil and water", scores: [65.0,75.0,95.0,90.0,70.0], water_l: 10000.0, co2: 1.8, energy_mj: 55.0, biodeg: "Yes", recycling: "Medium", durability: 7.0, wash_temp: "cold", drying: "air dry recommended" },
    M { name: "Conventional Cotton", ty: "natural", desc: "Traditional cotton with high water and pesticide use", scores: [30.0,40.0,90.0,25.0,45.0], water_l: 20000.0, co2: 5.3, energy_mj: 85.0, biodeg: "Yes", recycling: "Medium", durability: 6.5, wash_temp: "warm", drying: "tumble dry acceptable" },
    M { name: "Polyester", ty: "synthetic", desc: "Petroleum-based synthetic fiber, durable but not biodegradable", scores: [85.0,30.0,10.0,35.0,25.0], water_l: 70.0, co2: 7.0, energy_mj: 125.0, biodeg: "No", recycling: "Low", durability: 8.5, wash_temp: "cold", drying: "low heat only" },
    M { name: "Recycled Polyester", ty: "recycled", desc: "Polyester made from recycled plastic bottles and textile waste", scores: [90.0,70.0,10.0,60.0,75.0], water_l: 30.0, co2: 3.0, energy_mj: 60.0, biodeg: "No", recycling: "Medium", durability: 8.5, wash_temp: "cold", drying: "air dry preferred" },
    M { name: "Linen", ty: "natural", desc: "Made from flax plant, highly sustainable with minimal water needs", scores: [95.0,85.0,100.0,85.0,80.0], water_l: 2500.0, co2: 0.5, energy_mj: 30.0, biodeg: "Yes", recycling: "High", durability: 9.0, wash_temp: "cold", drying: "air dry" },
    M { name: "Hemp", ty: "natural", desc: "Fast-growing plant requiring minimal water and no pesticides", scores: [98.0,90.0,100.0,95.0,85.0], water_l: 2100.0, co2: 0.3, energy_mj: 25.0, biodeg: "Yes", recycling: "High", durability: 9.5, wash_temp: "cold", drying: "air dry" },
    M { name: "Wool", ty: "natural", desc: "Natural fiber from sheep, biodegradable but with animal welfare concerns", scores: [55.0,45.0,95.0,50.0,50.0], water_l: 125000.0, co2: 10.5, energy_mj: 65.0, biodeg: "Yes", recycling: "Medium", durability: 8.0, wash_temp: "cold", drying: "lay flat to dry" },
    M { name: "Tencel (Lyocell)", ty: "semi_synthetic", desc: "Made from wood pulp using closed-loop process, very sustainable", scores: [90.0,85.0,90.0,85.0,80.0], water_l: 1000.0, co2: 1.2, energy_mj: 45.0, biodeg: "Yes", recycling: "High", durability: 7.5, wash_temp: "cold", drying: "air dry" },
    M { name: "Bamboo Viscose", ty: "semi_synthetic", desc: "Made from bamboo but chemical-intensive processing reduces sustainability", scores: [70.0,65.0,75.0,40.0,60.0], water_l: 3500.0, co2: 2.5, energy_mj: 55.0, biodeg: "Partially", recycling: "Low", durability: 6.0, wash_temp: "cold", drying: "air dry" },
    M { name: "Nylon", ty: "synthetic", desc: "Petroleum-based synthetic with high environmental impact", scores: [80.0,25.0,5.0,30.0,20.0], water_l: 100.0, co2: 8.5, energy_mj: 150.0, biodeg: "No", recycling: "Low", durability: 9.0, wash_temp: "cold", drying: "low heat" },
];

struct G {
    name: &'static str,
    category: &'static str,
    desc: &'static str,
    weight: f64,
    lifespan: f64,
    wears: i64,
    care: &'static str,
    wash_freq: &'static str,
    material: &'static str, // single associated material by name (as in fixtures)
}

const GARMENTS: &[G] = &[
    G { name: "Organic Cotton T-Shirt", category: "shirt", desc: "Basic t-shirt made from 100% organic cotton", weight: 0.15, lifespan: 3.0, wears: 100, care: "Machine wash cold, tumble dry low or air dry", wash_freq: "After 1-2 wears", material: "Organic Cotton" },
    G { name: "Conventional Cotton T-Shirt", category: "shirt", desc: "Standard t-shirt made from conventional cotton", weight: 0.15, lifespan: 2.0, wears: 50, care: "Machine wash warm, tumble dry", wash_freq: "After each wear", material: "Conventional Cotton" },
    G { name: "Linen Summer Shirt", category: "shirt", desc: "Breathable linen shirt perfect for warm weather", weight: 0.2, lifespan: 5.0, wears: 150, care: "Hand wash or machine wash cold, air dry", wash_freq: "After 2-3 wears", material: "Linen" },
    G { name: "Hemp Jeans", category: "pants", desc: "Durable jeans made from hemp blend", weight: 0.6, lifespan: 7.0, wears: 300, care: "Machine wash cold inside out, air dry", wash_freq: "After 5-10 wears", material: "Hemp" },
    G { name: "Recycled Polyester Jacket", category: "outerwear", desc: "Lightweight jacket made from recycled plastic bottles", weight: 0.4, lifespan: 4.0, wears: 200, care: "Machine wash cold, air dry preferred", wash_freq: "After 5-10 wears or as needed", material: "Recycled Polyester" },
    G { name: "Polyester Athletic Wear", category: "activewear", desc: "Sports clothing made from conventional polyester", weight: 0.2, lifespan: 1.5, wears: 75, care: "Machine wash cold, low heat dry", wash_freq: "After each wear", material: "Polyester" },
    G { name: "Tencel Summer Dress", category: "dress", desc: "Flowing dress made from sustainable Tencel fabric", weight: 0.3, lifespan: 4.0, wears: 120, care: "Hand wash or delicate cycle cold, air dry", wash_freq: "After 1-2 wears", material: "Tencel (Lyocell)" },
];

struct B {
    name: &'static str,
    desc: &'static str,
    website: &'static str,
    ratings: [f64; 4], // overall, environmental, labor, animal_welfare
    flags: [bool; 5],   // bcorp, fair_trade, organic, recycled, carbon_neutral
    country: &'static str,
    price: &'static str,
    transparency: f64,
    goy: &'static str,
}

const BRANDS: &[B] = &[
    B { name: "Patagonia", desc: "Outdoor clothing company committed to environmental activism and sustainable practices", website: "https://www.patagonia.com", ratings: [88.0,90.0,85.0,90.0], flags: [true,true,true,true,false], country: "USA", price: "$$$", transparency: 95.0, goy: "Great" },
    B { name: "Eileen Fisher", desc: "Women's clothing brand focused on sustainability and circular fashion", website: "https://www.eileenfisher.com", ratings: [85.0,88.0,82.0,85.0], flags: [true,true,true,true,false], country: "USA", price: "$$$$", transparency: 90.0, goy: "Great" },
    B { name: "Reformation", desc: "Fashion brand focused on sustainable materials and transparent practices", website: "https://www.thereformation.com", ratings: [80.0,85.0,75.0,80.0], flags: [false,false,false,true,true], country: "USA", price: "$$$", transparency: 85.0, goy: "Good" },
    B { name: "People Tree", desc: "Pioneer in sustainable and Fair Trade fashion", website: "https://www.peopletree.co.uk", ratings: [87.0,85.0,90.0,85.0], flags: [false,true,true,false,false], country: "UK", price: "$$", transparency: 92.0, goy: "Great" },
    B { name: "Zara", desc: "Fast fashion brand with some sustainability initiatives", website: "https://www.zara.com", ratings: [35.0,30.0,35.0,40.0], flags: [false,false,false,true,false], country: "Spain", price: "$$", transparency: 40.0, goy: "Not good enough" },
    B { name: "H&M", desc: "Fast fashion retailer with Conscious Collection", website: "https://www.hm.com", ratings: [38.0,35.0,40.0,40.0], flags: [false,false,false,true,false], country: "Sweden", price: "$", transparency: 45.0, goy: "It's a start" },
];

#[derive(Debug, Clone, Copy, Default)]
pub struct FixtureCounts {
    pub materials: i64,
    pub garments: i64,
    pub brands: i64,
}

async fn material_id(db: &SqlitePool, name: &str) -> Result<Option<i64>> {
    Ok(
        sqlx::query_scalar::<_, i64>("SELECT id FROM materials WHERE name = ?1")
            .bind(name)
            .fetch_optional(db)
            .await?,
    )
}

pub async fn load_materials(db: &SqlitePool) -> Result<i64> {
    let mut count = 0;
    for m in MATERIALS {
        if material_id(db, m.name).await?.is_some() {
            continue;
        }
        let ts = now();
        sqlx::query(
            "INSERT INTO materials (created_at, updated_at, name, material_type, description, \
             water_usage_score, carbon_footprint_score, biodegradability_score, \
             chemical_usage_score, energy_consumption_score, water_liters_per_kg, \
             co2_kg_per_kg, energy_mj_per_kg, is_biodegradable, recycling_potential, \
             durability_rating, typical_wash_temp, drying_method) \
             VALUES (?1,?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13,?14,?15,?16,?17)",
        )
        .bind(&ts)
        .bind(m.name)
        .bind(m.ty)
        .bind(m.desc)
        .bind(m.scores[0])
        .bind(m.scores[1])
        .bind(m.scores[2])
        .bind(m.scores[3])
        .bind(m.scores[4])
        .bind(m.water_l)
        .bind(m.co2)
        .bind(m.energy_mj)
        .bind(m.biodeg)
        .bind(m.recycling)
        .bind(m.durability)
        .bind(m.wash_temp)
        .bind(m.drying)
        .execute(db)
        .await?;
        count += 1;
    }
    tracing::info!("Loaded {count} materials");
    Ok(count)
}

pub async fn load_garments(db: &SqlitePool) -> Result<i64> {
    let mut count = 0;
    for g in GARMENTS {
        let exists: Option<i64> =
            sqlx::query_scalar("SELECT id FROM garments WHERE name = ?1")
                .bind(g.name)
                .fetch_optional(db)
                .await?;
        if exists.is_some() {
            continue;
        }
        let mat_id = material_id(db, g.material).await?;
        let score = match mat_id {
            Some(_) => {
                // single associated material -> base = its overall score
                let scores: (f64, f64, f64, f64, f64) = sqlx::query_as(
                    "SELECT water_usage_score, carbon_footprint_score, \
                     biodegradability_score, chemical_usage_score, \
                     energy_consumption_score FROM materials WHERE name = ?1",
                )
                .bind(g.material)
                .fetch_one(db)
                .await?;
                let overall = domain::material_overall_score([
                    scores.0, scores.1, scores.2, scores.3, scores.4,
                ]);
                domain::garment_sustainability_score(&[overall], Some(g.lifespan))
            }
            None => domain::garment_sustainability_score(&[], Some(g.lifespan)),
        };
        let ts = now();
        let gid = sqlx::query(
            "INSERT INTO garments (created_at, updated_at, name, category, description, \
             typical_weight_kg, expected_lifespan_years, typical_wears, \
             care_instructions, washing_frequency, sustainability_score) \
             VALUES (?1,?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)",
        )
        .bind(&ts)
        .bind(g.name)
        .bind(g.category)
        .bind(g.desc)
        .bind(g.weight)
        .bind(g.lifespan)
        .bind(g.wears)
        .bind(g.care)
        .bind(g.wash_freq)
        .bind(score)
        .execute(db)
        .await?
        .last_insert_rowid();

        if let Some(mid) = mat_id {
            sqlx::query(
                "INSERT INTO garment_materials (garment_id, material_id, percentage) \
                 VALUES (?1, ?2, 100.0)",
            )
            .bind(gid)
            .bind(mid)
            .execute(db)
            .await?;
        }
        count += 1;
    }
    tracing::info!("Loaded {count} garments");
    Ok(count)
}

pub async fn load_brands(db: &SqlitePool) -> Result<i64> {
    let mut count = 0;
    for b in BRANDS {
        let exists: Option<i64> = sqlx::query_scalar("SELECT id FROM brands WHERE name = ?1")
            .bind(b.name)
            .fetch_optional(db)
            .await?;
        if exists.is_some() {
            continue;
        }
        let ts = now();
        sqlx::query(
            "INSERT INTO brands (created_at, updated_at, name, description, website, \
             overall_rating, environmental_rating, labor_rating, animal_welfare_rating, \
             is_certified_bcorp, is_fair_trade, is_organic_certified, \
             uses_recycled_materials, carbon_neutral, country, price_range, \
             transparency_score, good_on_you_rating) \
             VALUES (?1,?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13,?14,?15,?16,?17)",
        )
        .bind(&ts)
        .bind(b.name)
        .bind(b.desc)
        .bind(b.website)
        .bind(b.ratings[0])
        .bind(b.ratings[1])
        .bind(b.ratings[2])
        .bind(b.ratings[3])
        .bind(b.flags[0])
        .bind(b.flags[1])
        .bind(b.flags[2])
        .bind(b.flags[3])
        .bind(b.flags[4])
        .bind(b.country)
        .bind(b.price)
        .bind(b.transparency)
        .bind(b.goy)
        .execute(db)
        .await?;
        count += 1;
    }
    tracing::info!("Loaded {count} brands");
    Ok(count)
}

/// Load all fixtures. Mirrors `load_all_fixtures`.
pub async fn load_all_fixtures(db: &SqlitePool) -> Result<FixtureCounts> {
    tracing::info!("Loading all fixtures...");
    let counts = FixtureCounts {
        materials: load_materials(db).await?,
        garments: load_garments(db).await?,
        brands: load_brands(db).await?,
    };
    tracing::info!(
        "Fixtures loaded: materials={} garments={} brands={}",
        counts.materials,
        counts.garments,
        counts.brands
    );
    Ok(counts)
}
