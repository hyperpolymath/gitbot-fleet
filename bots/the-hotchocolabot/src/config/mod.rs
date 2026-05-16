//! Configuration management for HotChocolaBot
//!
//! Handles loading and validation of system configuration from TOML files.

use serde::{Deserialize, Serialize};
use std::fs;
use anyhow::{Result, Context};

/// Main bot configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BotConfig {
    /// Hardware configuration
    pub hardware: HardwareConfig,

    /// Safety system configuration
    pub safety: SafetyConfig,

    /// Recipe configuration
    pub recipes: RecipeConfig,

    /// Educational mode settings
    pub education: EducationConfig,
}

/// Hardware pin assignments and settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HardwareConfig {
    /// GPIO pin for cocoa pump
    pub cocoa_pump_pin: u8,

    /// GPIO pin for milk pump
    pub milk_pump_pin: u8,

    /// GPIO pin for sugar pump
    pub sugar_pump_pin: u8,

    /// I2C address for temperature sensor
    pub temp_sensor_addr: u8,

    /// I2C address for LCD display
    pub lcd_addr: u8,

    /// GPIO pin for emergency stop button
    pub emergency_stop_pin: u8,

    /// GPIO pin for LED status indicator
    pub status_led_pin: u8,
}

/// Safety system configuration (CNO - Certified Null Operations)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyConfig {
    /// Maximum temperature in Celsius before shutdown
    pub max_temperature: f32,

    /// Minimum temperature in Celsius for operation
    pub min_temperature: f32,

    /// Maximum pump runtime in seconds
    pub max_pump_runtime: u64,

    /// Timeout for operations in seconds
    pub operation_timeout: u64,

    /// Enable verbose safety logging
    pub verbose_logging: bool,

    /// Emergency stop button enabled
    pub emergency_stop_enabled: bool,
}

/// Recipe definitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecipeConfig {
    /// Standard hot chocolate recipe
    pub standard: Recipe,

    /// Light recipe (less cocoa)
    pub light: Recipe,

    /// Rich recipe (more cocoa)
    pub rich: Recipe,
}

/// Single recipe definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Recipe {
    /// Cocoa dispense time in milliseconds
    pub cocoa_ms: u64,

    /// Milk dispense time in milliseconds
    pub milk_ms: u64,

    /// Sugar dispense time in milliseconds
    pub sugar_ms: u64,

    /// Target temperature in Celsius
    pub target_temp: f32,
}

/// Educational mode configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EducationConfig {
    /// Enable student challenge mode
    pub challenge_mode: bool,

    /// Show detailed system state
    pub show_internals: bool,

    /// Enable intentional failures for learning
    pub enable_teaching_failures: bool,

    /// Delay between operations for observation (ms)
    pub observation_delay_ms: u64,
}

impl BotConfig {
    /// Load configuration from TOML file
    pub fn load(path: &str) -> Result<Self> {
        let contents = fs::read_to_string(path)
            .context("Failed to read config file")?;

        let config: BotConfig = toml::from_str(&contents)
            .context("Failed to parse config file")?;

        config.validate()?;
        Ok(config)
    }

    /// Validate configuration values
    fn validate(&self) -> Result<()> {
        if self.safety.max_temperature <= self.safety.min_temperature {
            anyhow::bail!("max_temperature must be greater than min_temperature");
        }

        if self.safety.max_pump_runtime == 0 {
            anyhow::bail!("max_pump_runtime must be greater than 0");
        }

        Ok(())
    }

    /// Save configuration to TOML file
    pub fn save(&self, path: &str) -> Result<()> {
        let contents = toml::to_string_pretty(self)
            .context("Failed to serialize config")?;

        fs::write(path, contents)
            .context("Failed to write config file")?;

        Ok(())
    }
}

impl Default for BotConfig {
    fn default() -> Self {
        Self {
            hardware: HardwareConfig {
                cocoa_pump_pin: 17,
                milk_pump_pin: 27,
                sugar_pump_pin: 22,
                temp_sensor_addr: 0x48,
                lcd_addr: 0x27,
                emergency_stop_pin: 23,
                status_led_pin: 24,
            },
            safety: SafetyConfig {
                max_temperature: 90.0,
                min_temperature: 5.0,
                max_pump_runtime: 30,
                operation_timeout: 120,
                verbose_logging: true,
                emergency_stop_enabled: true,
            },
            recipes: RecipeConfig {
                standard: Recipe {
                    cocoa_ms: 2000,
                    milk_ms: 5000,
                    sugar_ms: 1000,
                    target_temp: 65.0,
                },
                light: Recipe {
                    cocoa_ms: 1000,
                    milk_ms: 6000,
                    sugar_ms: 800,
                    target_temp: 65.0,
                },
                rich: Recipe {
                    cocoa_ms: 3000,
                    milk_ms: 4000,
                    sugar_ms: 1200,
                    target_temp: 70.0,
                },
            },
            education: EducationConfig {
                challenge_mode: false,
                show_internals: true,
                enable_teaching_failures: false,
                observation_delay_ms: 500,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config_valid() {
        let config = BotConfig::default();
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_invalid_temperature_range() {
        let mut config = BotConfig::default();
        config.safety.max_temperature = 10.0;
        config.safety.min_temperature = 20.0;
        assert!(config.validate().is_err());
    }
}
