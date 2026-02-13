//! Pump hardware implementation using GPIO control

use crate::hardware::Pump;
use anyhow::{Result, Context};
use async_trait::async_trait;
use std::time::{Duration, Instant};
use tokio::time::sleep;
use tracing::{info, warn};

#[cfg(target_os = "linux")]
use rppal::gpio::{Gpio, OutputPin};

/// GPIO-controlled peristaltic pump
pub struct GpioPump {
    #[cfg(target_os = "linux")]
    pin: OutputPin,
    #[cfg(not(target_os = "linux"))]
    pin_number: u8,

    name: String,
    is_running: bool,
    total_runtime_ms: u64,
    last_start: Option<Instant>,
}

impl GpioPump {
    /// Create new GPIO pump controller
    pub fn new(pin_number: u8, name: &str) -> Result<Self> {
        #[cfg(target_os = "linux")]
        let pin = Gpio::new()
            .context("Failed to initialize GPIO")?
            .get(pin_number)
            .context(format!("Failed to get GPIO pin {}", pin_number))?
            .into_output();

        info!("Initialized {} pump on GPIO pin {}", name, pin_number);

        Ok(Self {
            #[cfg(target_os = "linux")]
            pin,
            #[cfg(not(target_os = "linux"))]
            pin_number,

            name: name.to_string(),
            is_running: false,
            total_runtime_ms: 0,
            last_start: None,
        })
    }

    /// Internal helper to activate GPIO pin
    fn activate(&mut self) -> Result<()> {
        #[cfg(target_os = "linux")]
        self.pin.set_high();

        #[cfg(not(target_os = "linux"))]
        info!("[MOCK] Setting pin {} HIGH for {} pump", self.pin_number, self.name);

        Ok(())
    }

    /// Internal helper to deactivate GPIO pin
    fn deactivate(&mut self) -> Result<()> {
        #[cfg(target_os = "linux")]
        self.pin.set_low();

        #[cfg(not(target_os = "linux"))]
        info!("[MOCK] Setting pin {} LOW for {} pump", self.pin_number, self.name);

        Ok(())
    }
}

#[async_trait]
impl Pump for GpioPump {
    async fn dispense(&mut self, duration_ms: u64) -> Result<()> {
        if self.is_running {
            warn!("{} pump already running", self.name);
            return Ok(());
        }

        info!("{} pump dispensing for {}ms", self.name, duration_ms);

        self.is_running = true;
        self.last_start = Some(Instant::now());
        self.activate()?;

        // Run pump for specified duration
        sleep(Duration::from_millis(duration_ms)).await;

        self.stop().await?;
        Ok(())
    }

    async fn stop(&mut self) -> Result<()> {
        if !self.is_running {
            return Ok(());
        }

        self.deactivate()?;

        // Update runtime counter
        if let Some(start) = self.last_start {
            let elapsed = start.elapsed().as_millis() as u64;
            self.total_runtime_ms += elapsed;
            info!("{} pump stopped after {}ms (total: {}ms)",
                  self.name, elapsed, self.total_runtime_ms);
        }

        self.is_running = false;
        self.last_start = None;
        Ok(())
    }

    fn is_running(&self) -> bool {
        self.is_running
    }

    fn total_runtime_ms(&self) -> u64 {
        self.total_runtime_ms
    }

    fn reset_counter(&mut self) {
        info!("{} pump runtime counter reset (was {}ms)",
              self.name, self.total_runtime_ms);
        self.total_runtime_ms = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_pump_dispense() {
        let mut pump = GpioPump::new(17, "test").unwrap();
        assert!(!pump.is_running());

        pump.dispense(100).await.unwrap();

        assert!(!pump.is_running());
        assert!(pump.total_runtime_ms() >= 100);
    }

    #[tokio::test]
    async fn test_pump_counter_reset() {
        let mut pump = GpioPump::new(17, "test").unwrap();

        pump.dispense(50).await.unwrap();
        assert!(pump.total_runtime_ms() > 0);

        pump.reset_counter();
        assert_eq!(pump.total_runtime_ms(), 0);
    }
}
