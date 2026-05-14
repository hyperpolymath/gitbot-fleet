//! Hardware abstraction layer for HotChocolaBot
//!
//! Provides trait-based interfaces for hardware components to enable
//! testing without physical hardware and future platform portability.

pub mod pump;
pub mod sensor;
pub mod display;
pub mod mock;

use anyhow::Result;
use async_trait::async_trait;

/// Trait for controllable pump devices
#[async_trait]
pub trait Pump: Send + Sync {
    /// Activate pump for specified duration in milliseconds
    async fn dispense(&mut self, duration_ms: u64) -> Result<()>;

    /// Stop pump immediately
    async fn stop(&mut self) -> Result<()>;

    /// Check if pump is currently running
    fn is_running(&self) -> bool;

    /// Get total runtime in milliseconds
    fn total_runtime_ms(&self) -> u64;

    /// Reset runtime counter
    fn reset_counter(&mut self);
}

/// Trait for temperature sensors
#[async_trait]
pub trait TemperatureSensor: Send + Sync {
    /// Read current temperature in Celsius
    async fn read_temperature(&mut self) -> Result<f32>;

    /// Check if sensor is functioning
    async fn is_healthy(&self) -> bool;
}

/// Trait for LCD display
#[async_trait]
pub trait Display: Send + Sync {
    /// Write text to display
    async fn write(&mut self, text: &str) -> Result<()>;

    /// Clear display
    async fn clear(&mut self) -> Result<()>;

    /// Set cursor position (row, column)
    async fn set_cursor(&mut self, row: u8, col: u8) -> Result<()>;

    /// Display message with automatic formatting
    async fn show_message(&mut self, message: &str) -> Result<()> {
        self.clear().await?;
        self.write(message).await
    }
}

/// Trait for emergency stop button
#[async_trait]
pub trait EmergencyStop: Send + Sync {
    /// Check if emergency stop is pressed
    async fn is_pressed(&self) -> bool;

    /// Register callback for emergency stop events
    fn on_press<F>(&mut self, callback: F)
    where
        F: Fn() + Send + 'static;
}

/// Hardware abstraction for status LED
#[async_trait]
pub trait StatusLed: Send + Sync {
    /// Turn LED on
    async fn on(&mut self) -> Result<()>;

    /// Turn LED off
    async fn off(&mut self) -> Result<()>;

    /// Blink LED with specified pattern (on_ms, off_ms, count)
    async fn blink(&mut self, on_ms: u64, off_ms: u64, count: u32) -> Result<()>;
}
