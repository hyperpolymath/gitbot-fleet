// SPDX-License-Identifier: MPL-2.0
//! Database connection + schema initialisation (SQLAlchemy engine /
//! Alembic analogue).

use anyhow::Result;
use sqlx::sqlite::{SqlitePool, SqlitePoolOptions};

/// Open (creating if necessary) the SQLite pool and apply migrations.
/// Equivalent to `models.base.engine` + `init_db()`.
pub async fn connect(sqlx_url: &str) -> Result<SqlitePool> {
    if let Some(path) = sqlx_url
        .strip_prefix("sqlite://")
        .and_then(|s| s.split('?').next())
    {
        if !path.is_empty() && path != ":memory:" {
            if let Some(parent) = std::path::Path::new(path).parent() {
                if !parent.as_os_str().is_empty() {
                    std::fs::create_dir_all(parent)?;
                }
            }
        }
    }

    let pool = SqlitePoolOptions::new()
        .max_connections(5)
        .connect(sqlx_url)
        .await?;

    init_db(&pool).await?;
    Ok(pool)
}

/// Create all tables. Mirrors `Base.metadata.create_all`.
pub async fn init_db(pool: &SqlitePool) -> Result<()> {
    sqlx::migrate!("./migrations").run(pool).await?;
    Ok(())
}

/// Current timestamp as RFC3339 (the `created_at`/`updated_at` encoding).
pub fn now() -> String {
    chrono::Utc::now().to_rfc3339()
}
