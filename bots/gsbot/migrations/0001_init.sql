-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Initial schema for the Garment Sustainability Bot.
-- Mirrors the original SQLAlchemy models (materials, garments, brands,
-- users, garment_materials M2M). Timestamps are RFC3339 TEXT.

CREATE TABLE IF NOT EXISTS materials (
    id                       INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at               TEXT NOT NULL,
    updated_at               TEXT NOT NULL,
    name                     TEXT NOT NULL UNIQUE,
    material_type            TEXT NOT NULL,
    description              TEXT,
    water_usage_score        REAL NOT NULL DEFAULT 50.0,
    carbon_footprint_score   REAL NOT NULL DEFAULT 50.0,
    biodegradability_score   REAL NOT NULL DEFAULT 50.0,
    chemical_usage_score     REAL NOT NULL DEFAULT 50.0,
    energy_consumption_score REAL NOT NULL DEFAULT 50.0,
    water_liters_per_kg      REAL,
    co2_kg_per_kg            REAL,
    energy_mj_per_kg         REAL,
    is_biodegradable         TEXT,
    recycling_potential      TEXT,
    durability_rating        REAL,
    typical_wash_temp        TEXT,
    drying_method            TEXT
);
CREATE INDEX IF NOT EXISTS ix_materials_name ON materials (name);

CREATE TABLE IF NOT EXISTS garments (
    id                      INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at              TEXT NOT NULL,
    updated_at              TEXT NOT NULL,
    name                    TEXT NOT NULL UNIQUE,
    category                TEXT NOT NULL,
    description             TEXT,
    typical_weight_kg       REAL,
    expected_lifespan_years REAL,
    typical_wears           INTEGER,
    care_instructions       TEXT,
    washing_frequency       TEXT,
    sustainability_score    REAL
);
CREATE INDEX IF NOT EXISTS ix_garments_name ON garments (name);
CREATE INDEX IF NOT EXISTS ix_garments_category ON garments (category);

CREATE TABLE IF NOT EXISTS brands (
    id                    INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at            TEXT NOT NULL,
    updated_at            TEXT NOT NULL,
    name                  TEXT NOT NULL UNIQUE,
    description           TEXT,
    website               TEXT,
    overall_rating        REAL DEFAULT 50.0,
    environmental_rating  REAL DEFAULT 50.0,
    labor_rating          REAL DEFAULT 50.0,
    animal_welfare_rating REAL DEFAULT 50.0,
    is_certified_bcorp    INTEGER DEFAULT 0,
    is_fair_trade         INTEGER DEFAULT 0,
    is_organic_certified  INTEGER DEFAULT 0,
    uses_recycled_materials INTEGER DEFAULT 0,
    carbon_neutral        INTEGER DEFAULT 0,
    country               TEXT,
    price_range           TEXT,
    transparency_score    REAL,
    good_on_you_rating    TEXT
);
CREATE INDEX IF NOT EXISTS ix_brands_name ON brands (name);

CREATE TABLE IF NOT EXISTS users (
    id                      INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at              TEXT NOT NULL,
    updated_at              TEXT NOT NULL,
    discord_id              INTEGER NOT NULL UNIQUE,
    username                TEXT,
    preferred_materials     TEXT,
    budget_range            TEXT,
    sustainability_priority TEXT,
    queries_count           INTEGER NOT NULL DEFAULT 0,
    sustainability_points   INTEGER NOT NULL DEFAULT 0,
    level                   INTEGER NOT NULL DEFAULT 1,
    daily_tips_enabled      INTEGER NOT NULL DEFAULT 1
);
CREATE INDEX IF NOT EXISTS ix_users_discord_id ON users (discord_id);

CREATE TABLE IF NOT EXISTS garment_materials (
    garment_id  INTEGER NOT NULL REFERENCES garments (id),
    material_id INTEGER NOT NULL REFERENCES materials (id),
    percentage  REAL DEFAULT 100.0,
    PRIMARY KEY (garment_id, material_id)
);
