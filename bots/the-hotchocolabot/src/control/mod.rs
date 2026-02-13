//! Main dispense control logic for HotChocolaBot

use crate::config::{BotConfig, Recipe};
use crate::hardware::{Pump, TemperatureSensor, Display};
use crate::hardware::mock::{MockPump, MockTemperatureSensor, MockDisplay};
use crate::safety::SafetyMonitor;
use anyhow::{Result, Context};
use tracing::{info, warn, error};

/// Main controller for the hot chocolate dispensing system
pub struct DispenseController {
    config: BotConfig,
    cocoa_pump: Box<dyn Pump>,
    milk_pump: Box<dyn Pump>,
    sugar_pump: Box<dyn Pump>,
    temp_sensor: Box<dyn TemperatureSensor>,
    display: Box<dyn Display>,
}

impl DispenseController {
    /// Create new dispense controller with real hardware
    #[cfg(target_os = "linux")]
    pub async fn new(config: BotConfig) -> Result<Self> {
        use crate::hardware::pump::GpioPump;
        use crate::hardware::sensor::I2cTemperatureSensor;
        use crate::hardware::display::I2cLcdDisplay;

        info!("Initializing hardware...");

        let cocoa_pump = Box::new(GpioPump::new(config.hardware.cocoa_pump_pin, "Cocoa")?);
        let milk_pump = Box::new(GpioPump::new(config.hardware.milk_pump_pin, "Milk")?);
        let sugar_pump = Box::new(GpioPump::new(config.hardware.sugar_pump_pin, "Sugar")?);

        let temp_sensor = Box::new(I2cTemperatureSensor::new(config.hardware.temp_sensor_addr)?);
        let display = Box::new(I2cLcdDisplay::new(config.hardware.lcd_addr, 2, 16)?);

        Ok(Self {
            config,
            cocoa_pump,
            milk_pump,
            sugar_pump,
            temp_sensor,
            display,
        })
    }

    /// Create new dispense controller with mock hardware (for testing/development)
    #[cfg(not(target_os = "linux"))]
    pub async fn new(config: BotConfig) -> Result<Self> {
        info!("Initializing MOCK hardware for testing...");

        let cocoa_pump = Box::new(MockPump::new("Cocoa"));
        let milk_pump = Box::new(MockPump::new("Milk"));
        let sugar_pump = Box::new(MockPump::new("Sugar"));

        let temp_sensor = Box::new(MockTemperatureSensor::new(20.0));
        let display = Box::new(MockDisplay::new());

        Ok(Self {
            config,
            cocoa_pump,
            milk_pump,
            sugar_pump,
            temp_sensor,
            display,
        })
    }

    /// Main control loop
    pub async fn run(&self, safety_monitor: &mut SafetyMonitor) -> Result<()> {
        info!("HotChocolaBot ready. Waiting for commands...");

        self.display.show_message("HotChocolaBot\nReady!").await?;

        // In educational mode, show system status
        if self.config.education.show_internals {
            self.show_system_status().await?;
        }

        // For demonstration, dispense one standard hot chocolate
        info!("Dispensing standard hot chocolate...");
        self.dispense_recipe(&self.config.recipes.standard, safety_monitor).await?;

        self.display.show_message("Complete!\nEnjoy!").await?;

        Ok(())
    }

    /// Dispense hot chocolate according to recipe
    pub async fn dispense_recipe(
        &self,
        recipe: &Recipe,
        safety_monitor: &mut SafetyMonitor,
    ) -> Result<()> {
        info!("Starting dispense sequence");

        // Check temperature before starting
        let temp = self.temp_sensor.read_temperature().await
            .context("Failed to read temperature")?;

        safety_monitor.validate_temperature(temp)?;

        // Display recipe info
        self.display.show_message(&format!("Temp: {:.1}C\nPreparing...", temp)).await?;

        // Add observation delay in educational mode
        if self.config.education.observation_delay_ms > 0 {
            tokio::time::sleep(tokio::time::Duration::from_millis(
                self.config.education.observation_delay_ms
            )).await;
        }

        // Phase 1: Milk (base)
        self.display.show_message("Adding milk...").await?;
        self.dispense_ingredient("milk", self.milk_pump.as_ref(), recipe.milk_ms, safety_monitor).await?;

        // Phase 2: Cocoa
        self.display.show_message("Adding cocoa...").await?;
        self.dispense_ingredient("cocoa", self.cocoa_pump.as_ref(), recipe.cocoa_ms, safety_monitor).await?;

        // Phase 3: Sugar
        self.display.show_message("Adding sugar...").await?;
        self.dispense_ingredient("sugar", self.sugar_pump.as_ref(), recipe.sugar_ms, safety_monitor).await?;

        info!("Dispense complete!");
        safety_monitor.record_success();

        Ok(())
    }

    /// Dispense a single ingredient with safety monitoring
    async fn dispense_ingredient(
        &self,
        name: &str,
        pump: &dyn Pump,
        duration_ms: u64,
        safety_monitor: &mut SafetyMonitor,
    ) -> Result<()> {
        // Safety check: ensure not exceeding max runtime
        if duration_ms > self.config.safety.max_pump_runtime * 1000 {
            let msg = format!("Pump runtime {} exceeds maximum {}",
                            duration_ms,
                            self.config.safety.max_pump_runtime * 1000);
            error!("{}", msg);
            safety_monitor.trigger_emergency_stop(&msg);
            anyhow::bail!(msg);
        }

        // Check for emergency stop
        if safety_monitor.is_emergency_stop() {
            anyhow::bail!("Emergency stop active - cannot dispense");
        }

        info!("Dispensing {} for {}ms", name, duration_ms);

        // Cast to mutable to call dispense
        // Note: This is a limitation of the current design - in production,
        // we'd use interior mutability or refactor the trait
        let pump_mut = unsafe {
            let ptr = pump as *const dyn Pump as *mut dyn Pump;
            &mut *ptr
        };

        pump_mut.dispense(duration_ms).await?;

        // Add observation delay in educational mode
        if self.config.education.observation_delay_ms > 0 {
            tokio::time::sleep(tokio::time::Duration::from_millis(
                self.config.education.observation_delay_ms
            )).await;
        }

        Ok(())
    }

    /// Show system status on display
    async fn show_system_status(&self) -> Result<()> {
        let temp = self.temp_sensor.read_temperature().await?;

        let status = format!(
            "Temp: {:.1}C\nPumps: Ready",
            temp
        );

        self.display.show_message(&status).await?;

        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        Ok(())
    }

    /// Get pump runtime statistics (for educational display)
    pub fn get_pump_stats(&self) -> PumpStats {
        PumpStats {
            cocoa_runtime_ms: self.cocoa_pump.total_runtime_ms(),
            milk_runtime_ms: self.milk_pump.total_runtime_ms(),
            sugar_runtime_ms: self.sugar_pump.total_runtime_ms(),
        }
    }
}

/// Statistics about pump usage
#[derive(Debug)]
pub struct PumpStats {
    pub cocoa_runtime_ms: u64,
    pub milk_runtime_ms: u64,
    pub sugar_runtime_ms: u64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::SafetyConfig;

    #[tokio::test]
    async fn test_controller_creation() {
        let config = BotConfig::default();
        let controller = DispenseController::new(config).await;
        assert!(controller.is_ok());
    }

    #[tokio::test]
    async fn test_dispense_recipe() {
        let config = BotConfig::default();
        let controller = DispenseController::new(config.clone()).await.unwrap();
        let mut safety = SafetyMonitor::new(&config.safety).unwrap();

        let result = controller.dispense_recipe(&config.recipes.standard, &mut safety).await;
        assert!(result.is_ok());
    }
}
