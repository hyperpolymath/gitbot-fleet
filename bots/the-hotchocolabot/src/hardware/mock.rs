//! Mock hardware implementations for testing without physical devices

use crate::hardware::{Pump, TemperatureSensor, Display, EmergencyStop, StatusLed};
use anyhow::Result;
use async_trait::async_trait;
use std::sync::{Arc, Mutex};
use std::time::Instant;
use tracing::info;

/// Mock pump for testing
#[derive(Clone)]
pub struct MockPump {
    name: String,
    state: Arc<Mutex<MockPumpState>>,
}

struct MockPumpState {
    is_running: bool,
    total_runtime_ms: u64,
    last_start: Option<Instant>,
}

impl MockPump {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            state: Arc::new(Mutex::new(MockPumpState {
                is_running: false,
                total_runtime_ms: 0,
                last_start: None,
            })),
        }
    }
}

#[async_trait]
impl Pump for MockPump {
    async fn dispense(&mut self, duration_ms: u64) -> Result<()> {
        let mut state = self.state.lock().unwrap();

        info!("[MOCK] {} pump dispensing for {}ms", self.name, duration_ms);

        state.is_running = true;
        state.last_start = Some(Instant::now());

        drop(state); // Release lock during sleep

        tokio::time::sleep(tokio::time::Duration::from_millis(duration_ms)).await;

        let mut state = self.state.lock().unwrap();
        state.is_running = false;

        if let Some(start) = state.last_start {
            let elapsed = start.elapsed().as_millis() as u64;
            state.total_runtime_ms += elapsed;
        }

        Ok(())
    }

    async fn stop(&mut self) -> Result<()> {
        let mut state = self.state.lock().unwrap();
        info!("[MOCK] {} pump stopped", self.name);
        state.is_running = false;
        Ok(())
    }

    fn is_running(&self) -> bool {
        self.state.lock().unwrap().is_running
    }

    fn total_runtime_ms(&self) -> u64 {
        self.state.lock().unwrap().total_runtime_ms
    }

    fn reset_counter(&mut self) {
        let mut state = self.state.lock().unwrap();
        info!("[MOCK] {} pump counter reset", self.name);
        state.total_runtime_ms = 0;
    }
}

/// Mock temperature sensor
pub struct MockTemperatureSensor {
    temperature: Arc<Mutex<f32>>,
}

impl MockTemperatureSensor {
    pub fn new(initial_temp: f32) -> Self {
        Self {
            temperature: Arc::new(Mutex::new(initial_temp)),
        }
    }

    /// Set the mock temperature (for testing)
    pub fn set_temperature(&mut self, temp: f32) {
        *self.temperature.lock().unwrap() = temp;
    }
}

#[async_trait]
impl TemperatureSensor for MockTemperatureSensor {
    async fn read_temperature(&mut self) -> Result<f32> {
        let temp = *self.temperature.lock().unwrap();
        info!("[MOCK] Temperature reading: {:.1}°C", temp);
        Ok(temp)
    }

    async fn is_healthy(&self) -> bool {
        true
    }
}

/// Mock LCD display
pub struct MockDisplay {
    buffer: Arc<Mutex<String>>,
}

impl MockDisplay {
    pub fn new() -> Self {
        Self {
            buffer: Arc::new(Mutex::new(String::new())),
        }
    }

    /// Get current display buffer (for testing)
    pub fn get_buffer(&self) -> String {
        self.buffer.lock().unwrap().clone()
    }
}

#[async_trait]
impl Display for MockDisplay {
    async fn write(&mut self, text: &str) -> Result<()> {
        let mut buffer = self.buffer.lock().unwrap();
        buffer.push_str(text);
        info!("[MOCK] Display write: {}", text);
        Ok(())
    }

    async fn clear(&mut self) -> Result<()> {
        let mut buffer = self.buffer.lock().unwrap();
        buffer.clear();
        info!("[MOCK] Display cleared");
        Ok(())
    }

    async fn set_cursor(&mut self, row: u8, col: u8) -> Result<()> {
        info!("[MOCK] Display cursor set to ({}, {})", row, col);
        Ok(())
    }
}

/// Mock emergency stop button
pub struct MockEmergencyStop {
    pressed: Arc<Mutex<bool>>,
}

impl MockEmergencyStop {
    pub fn new() -> Self {
        Self {
            pressed: Arc::new(Mutex::new(false)),
        }
    }

    /// Simulate button press (for testing)
    pub fn press(&mut self) {
        *self.pressed.lock().unwrap() = true;
        info!("[MOCK] Emergency stop pressed!");
    }

    /// Release button (for testing)
    pub fn release(&mut self) {
        *self.pressed.lock().unwrap() = false;
        info!("[MOCK] Emergency stop released");
    }
}

#[async_trait]
impl EmergencyStop for MockEmergencyStop {
    async fn is_pressed(&self) -> bool {
        *self.pressed.lock().unwrap()
    }

    fn on_press<F>(&mut self, _callback: F)
    where
        F: Fn() + Send + 'static,
    {
        info!("[MOCK] Emergency stop callback registered");
    }
}

/// Mock status LED
pub struct MockStatusLed {
    state: Arc<Mutex<bool>>,
}

impl MockStatusLed {
    pub fn new() -> Self {
        Self {
            state: Arc::new(Mutex::new(false)),
        }
    }

    /// Check if LED is on (for testing)
    pub fn is_on(&self) -> bool {
        *self.state.lock().unwrap()
    }
}

#[async_trait]
impl StatusLed for MockStatusLed {
    async fn on(&mut self) -> Result<()> {
        *self.state.lock().unwrap() = true;
        info!("[MOCK] Status LED ON");
        Ok(())
    }

    async fn off(&mut self) -> Result<()> {
        *self.state.lock().unwrap() = false;
        info!("[MOCK] Status LED OFF");
        Ok(())
    }

    async fn blink(&mut self, on_ms: u64, off_ms: u64, count: u32) -> Result<()> {
        info!("[MOCK] Status LED blinking {}ms on, {}ms off, {} times", on_ms, off_ms, count);

        for _ in 0..count {
            self.on().await?;
            tokio::time::sleep(tokio::time::Duration::from_millis(on_ms)).await;
            self.off().await?;
            tokio::time::sleep(tokio::time::Duration::from_millis(off_ms)).await;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_mock_pump() {
        let mut pump = MockPump::new("test");
        assert!(!pump.is_running());

        pump.dispense(100).await.unwrap();

        assert!(!pump.is_running());
        assert!(pump.total_runtime_ms() >= 100);
    }

    #[tokio::test]
    async fn test_mock_temperature_sensor() {
        let mut sensor = MockTemperatureSensor::new(25.0);
        let temp = sensor.read_temperature().await.unwrap();
        assert_eq!(temp, 25.0);

        sensor.set_temperature(30.0);
        let temp = sensor.read_temperature().await.unwrap();
        assert_eq!(temp, 30.0);
    }

    #[tokio::test]
    async fn test_mock_display() {
        let mut display = MockDisplay::new();
        display.write("Hello").await.unwrap();
        assert_eq!(display.get_buffer(), "Hello");

        display.clear().await.unwrap();
        assert_eq!(display.get_buffer(), "");
    }

    #[tokio::test]
    async fn test_mock_emergency_stop() {
        let mut estop = MockEmergencyStop::new();
        assert!(!estop.is_pressed().await);

        estop.press();
        assert!(estop.is_pressed().await);

        estop.release();
        assert!(!estop.is_pressed().await);
    }
}
