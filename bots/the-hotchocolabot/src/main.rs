//! HotChocolaBot - Educational Robotics Platform
//!
//! An over-engineered hot chocolate dispenser designed to teach
//! reverse engineering and systems thinking through heutagogic learning.
//!
//! Part of UAL Creative Communities' postdisciplinary Mechatronics group (MechCC).

use anyhow::Result;
use tracing::{info, error};
use tracing_subscriber;

mod control;
mod hardware;
mod config;
mod safety;

use crate::control::DispenseController;
use crate::config::BotConfig;
use crate::safety::SafetyMonitor;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter("hotchocolabot=debug")
        .init();

    info!("HotChocolaBot v{} starting...", env!("CARGO_PKG_VERSION"));

    // Load configuration
    let config = BotConfig::load("config.toml")
        .unwrap_or_else(|_| {
            info!("No config found, using defaults");
            BotConfig::default()
        });

    info!("Configuration loaded: {:?}", config);

    // Initialize safety monitor (CNO - Certified Null Operations)
    let mut safety_monitor = SafetyMonitor::new(&config.safety)?;

    // Initialize hardware controller
    let controller = DispenseController::new(config.clone()).await?;

    // Pre-flight safety checks
    if !safety_monitor.run_preflight_checks(&controller).await? {
        error!("Pre-flight safety checks failed. Aborting.");
        return Err(anyhow::anyhow!("Safety checks failed"));
    }

    info!("Safety checks passed. System ready.");

    // Main control loop
    controller.run(&mut safety_monitor).await?;

    info!("HotChocolaBot shutting down gracefully.");
    Ok(())
}
