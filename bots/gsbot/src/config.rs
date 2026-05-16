// SPDX-License-Identifier: PMPL-1.0-or-later
//! Application configuration.
//!
//! Faithful port of the original `config/settings.py`: same environment
//! variables, same defaults, same validation semantics.

use std::path::PathBuf;

use anyhow::{bail, Result};

/// Application configuration, loaded from the process environment.
#[derive(Debug, Clone)]
pub struct Config {
    pub base_dir: PathBuf,
    pub discord_token: String,
    pub discord_prefix: String,
    pub discord_admin_ids: Vec<i64>,
    pub database_url: String,
    pub database_echo: bool,
    pub cache_ttl: u64,
    pub cache_maxsize: usize,
    pub log_level: String,
    pub log_file: Option<String>,
    pub api_timeout: u64,
    pub api_retry_count: u32,
    pub enable_caching: bool,
    pub enable_analytics: bool,
}

fn env_or(key: &str, default: &str) -> String {
    std::env::var(key).unwrap_or_else(|_| default.to_string())
}

fn env_bool(key: &str, default: bool) -> bool {
    match std::env::var(key) {
        Ok(v) => v.eq_ignore_ascii_case("true"),
        Err(_) => default,
    }
}

impl Config {
    /// Load configuration from the environment, applying the same defaults
    /// as the original Python `Config` class. `.env` is loaded first
    /// (python-dotenv parity).
    pub fn from_env() -> Self {
        let _ = dotenvy::dotenv();

        // BASE_DIR = settings.py is src/config/settings.py -> parent³ = crate root.
        let base_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

        let admin_ids = env_or("DISCORD_ADMIN_IDS", "")
            .split(',')
            .filter(|s| !s.is_empty())
            .filter_map(|s| s.trim().parse::<i64>().ok())
            .collect();

        let database_url = std::env::var("DATABASE_URL").unwrap_or_else(|_| {
            format!("sqlite:///{}/data/gsbot.db", base_dir.display())
        });

        Config {
            base_dir,
            discord_token: env_or("DISCORD_TOKEN", ""),
            discord_prefix: env_or("DISCORD_PREFIX", "!"),
            discord_admin_ids: admin_ids,
            database_url,
            database_echo: env_bool("DATABASE_ECHO", false),
            cache_ttl: env_or("CACHE_TTL", "3600").parse().unwrap_or(3600),
            cache_maxsize: env_or("CACHE_MAXSIZE", "1000").parse().unwrap_or(1000),
            log_level: env_or("LOG_LEVEL", "INFO"),
            log_file: std::env::var("LOG_FILE").ok().filter(|s| !s.is_empty()),
            api_timeout: env_or("API_TIMEOUT", "30").parse().unwrap_or(30),
            api_retry_count: env_or("API_RETRY_COUNT", "3").parse().unwrap_or(3),
            enable_caching: env_bool("ENABLE_CACHING", true),
            enable_analytics: env_bool("ENABLE_ANALYTICS", false),
        }
    }

    pub fn data_dir(&self) -> PathBuf {
        self.base_dir.join("data")
    }

    pub fn fixtures_dir(&self) -> PathBuf {
        self.data_dir().join("fixtures")
    }

    /// Validate required configuration values and ensure data directories
    /// exist. Mirrors `Config.validate()`.
    pub fn validate(&self) -> Result<()> {
        if self.discord_token.is_empty() {
            bail!("DISCORD_TOKEN environment variable is required");
        }
        std::fs::create_dir_all(self.data_dir())?;
        std::fs::create_dir_all(self.fixtures_dir())?;
        Ok(())
    }

    /// Filesystem path of the SQLite database. Mirrors
    /// `Config.get_database_path()` — only SQLite is supported.
    pub fn database_path(&self) -> Result<PathBuf> {
        if let Some(rest) = self.database_url.strip_prefix("sqlite:///") {
            Ok(PathBuf::from(rest))
        } else {
            bail!("Only SQLite databases are supported in this version")
        }
    }

    /// sqlx connection string. sqlx expects `sqlite://<path>` (or
    /// `sqlite::memory:`); the original used SQLAlchemy's `sqlite:///`.
    pub fn sqlx_url(&self) -> String {
        if self.database_url.contains(":memory:") {
            return "sqlite::memory:".to_string();
        }
        match self.database_url.strip_prefix("sqlite:///") {
            Some(path) => format!("sqlite://{path}?mode=rwc"),
            None => self.database_url.clone(),
        }
    }
}
