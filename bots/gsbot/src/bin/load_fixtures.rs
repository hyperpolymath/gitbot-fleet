// SPDX-License-Identifier: PMPL-1.0-or-later
//! `gsbot-load-fixtures` — port of `scripts/load_fixtures.py` __main__.

use gsbot::{config::Config, db, fixtures, logging};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let config = Config::from_env();
    let _g = logging::init(&config.log_level, config.log_file.as_deref());
    config.validate()?;

    let pool = db::connect(&config.sqlx_url()).await?;
    let counts = fixtures::load_all_fixtures(&pool).await?;

    println!("\n✅ Fixtures loaded successfully!");
    println!("   Materials: {}", counts.materials);
    println!("   Garments: {}", counts.garments);
    println!("   Brands: {}", counts.brands);
    Ok(())
}
