//! Temperature sensor implementation using I2C

use crate::hardware::TemperatureSensor;
use anyhow::{Result, Context};
use async_trait::async_trait;
use tracing::{info, warn};

#[cfg(target_os = "linux")]
use rppal::i2c::I2c;

/// I2C temperature sensor (e.g., TMP102, DS18B20)
pub struct I2cTemperatureSensor {
    #[cfg(target_os = "linux")]
    i2c: I2c,

    #[cfg(not(target_os = "linux"))]
    address: u8,

    last_reading: Option<f32>,
    consecutive_failures: u32,
}

impl I2cTemperatureSensor {
    /// Create new I2C temperature sensor
    pub fn new(address: u8) -> Result<Self> {
        #[cfg(target_os = "linux")]
        let mut i2c = I2c::new().context("Failed to initialize I2C")?;

        #[cfg(target_os = "linux")]
        i2c.set_slave_address(address as u16)
            .context(format!("Failed to set I2C address 0x{:02X}", address))?;

        info!("Initialized temperature sensor at I2C address 0x{:02X}", address);

        Ok(Self {
            #[cfg(target_os = "linux")]
            i2c,
            #[cfg(not(target_os = "linux"))]
            address,

            last_reading: None,
            consecutive_failures: 0,
        })
    }

    /// Read raw temperature from sensor
    #[cfg(target_os = "linux")]
    async fn read_raw(&mut self) -> Result<f32> {
        // TMP102 register layout: 2-byte temperature reading
        let mut buf = [0u8; 2];
        self.i2c.read(&mut buf)
            .context("Failed to read from temperature sensor")?;

        // Convert to temperature (12-bit resolution, 0.0625°C per LSB)
        let raw = ((buf[0] as i16) << 4) | ((buf[1] as i16) >> 4);
        let temp = (raw as f32) * 0.0625;

        Ok(temp)
    }

    #[cfg(not(target_os = "linux"))]
    async fn read_raw(&mut self) -> Result<f32> {
        // Mock reading: simulate temperature around 20-25°C
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let temp = rng.gen_range(20.0..25.0);
        Ok(temp)
    }
}

#[async_trait]
impl TemperatureSensor for I2cTemperatureSensor {
    async fn read_temperature(&mut self) -> Result<f32> {
        match self.read_raw().await {
            Ok(temp) => {
                self.last_reading = Some(temp);
                self.consecutive_failures = 0;
                Ok(temp)
            }
            Err(e) => {
                self.consecutive_failures += 1;
                warn!("Temperature sensor read failed (attempt {}): {:?}",
                      self.consecutive_failures, e);

                if self.consecutive_failures > 5 {
                    Err(anyhow::anyhow!("Temperature sensor failed {} times",
                                       self.consecutive_failures))
                } else if let Some(last) = self.last_reading {
                    warn!("Using last known temperature: {}°C", last);
                    Ok(last)
                } else {
                    Err(e)
                }
            }
        }
    }

    async fn is_healthy(&self) -> bool {
        self.consecutive_failures < 3
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_sensor_reading() {
        let mut sensor = I2cTemperatureSensor::new(0x48).unwrap();
        let temp = sensor.read_temperature().await.unwrap();

        assert!(temp > -40.0 && temp < 125.0, "Temperature out of sensor range");
        assert!(sensor.is_healthy().await);
    }
}
