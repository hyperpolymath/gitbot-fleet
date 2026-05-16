// SPDX-License-Identifier: PMPL-1.0-or-later
//! Main entry point — equivalent of `bot/main.py`.

mod bot;

use std::process::exit;

use gsbot::{config::Config, db, logging};

#[tokio::main]
async fn main() {
    let config = Config::from_env();
    let _log_guard = logging::init(&config.log_level, config.log_file.as_deref());

    if let Err(e) = config.validate() {
        tracing::error!("Configuration error: {e}");
        exit(1);
    }
    tracing::info!("Configuration validated successfully");

    let pool = match db::connect(&config.sqlx_url()).await {
        Ok(p) => {
            tracing::info!("Database initialized successfully");
            p
        }
        Err(e) => {
            tracing::error!("Failed to initialize database: {e}");
            exit(1);
        }
    };

    tokio::select! {
        res = bot::run(config, pool) => {
            if let Err(e) = res {
                tracing::error!("Failed to start bot: {e}");
                exit(1);
            }
        }
        _ = tokio::signal::ctrl_c() => {
            tracing::info!("Bot stopped by user");
        }
    }
}
