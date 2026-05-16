//! LCD display implementation using I2C

use crate::hardware::Display;
use anyhow::{Result, Context};
use async_trait::async_trait;
use tracing::info;

#[cfg(target_os = "linux")]
use rppal::i2c::I2c;
#[cfg(target_os = "linux")]
use std::sync::Mutex;

/// I2C LCD display (e.g., 16x2 or 20x4 with PCF8574 backpack)
pub struct I2cLcdDisplay {
    // `rppal::I2c` is `Send` but not `Sync`; wrapping it in a `Mutex` makes the
    // display `Sync` so it satisfies the `Display: Send + Sync` trait bound
    // (which the `async_trait` `&self` methods require) without changing the
    // single-threaded access behaviour, as all access is via `&mut self`.
    #[cfg(target_os = "linux")]
    i2c: Mutex<I2c>,

    #[cfg(not(target_os = "linux"))]
    address: u8,

    // Read only by `set_cursor`, which is part of the intentional `Display`
    // API surface but unused by the current demo binary.
    #[allow(dead_code)]
    rows: u8,
    cols: u8,
    cursor_row: u8,
    cursor_col: u8,
}

impl I2cLcdDisplay {
    /// Create new I2C LCD display
    pub fn new(address: u8, rows: u8, cols: u8) -> Result<Self> {
        #[cfg(target_os = "linux")]
        let mut i2c = I2c::new().context("Failed to initialize I2C")?;

        #[cfg(target_os = "linux")]
        i2c.set_slave_address(address as u16)
            .context(format!("Failed to set I2C address 0x{:02X}", address))?;

        info!("Initialized {}x{} LCD at I2C address 0x{:02X}", rows, cols, address);

        let mut display = Self {
            #[cfg(target_os = "linux")]
            i2c: Mutex::new(i2c),
            #[cfg(not(target_os = "linux"))]
            address,

            rows,
            cols,
            cursor_row: 0,
            cursor_col: 0,
        };

        // Initialize display
        display.initialize()?;

        Ok(display)
    }

    /// Initialize LCD display
    fn initialize(&mut self) -> Result<()> {
        #[cfg(target_os = "linux")]
        {
            // LCD initialization sequence for HD44780
            // This is simplified - real implementation would need proper timing
            self.send_command(0x33)?; // Initialize
            self.send_command(0x32)?; // Set to 4-bit mode
            self.send_command(0x28)?; // 2 line, 5x8 matrix
            self.send_command(0x0C)?; // Display on, cursor off
            self.send_command(0x06)?; // Increment cursor
            self.send_command(0x01)?; // Clear display
        }

        #[cfg(not(target_os = "linux"))]
        info!("[MOCK] LCD display initialized");

        Ok(())
    }

    /// Send command to LCD
    #[cfg(target_os = "linux")]
    fn send_command(&mut self, cmd: u8) -> Result<()> {
        // Simplified command sending - real implementation needs proper timing
        let high_nibble = (cmd & 0xF0) | 0x0C; // En=1, Rs=0, Rw=0
        let low_nibble = ((cmd << 4) & 0xF0) | 0x0C;

        let mut i2c = self.i2c.lock().unwrap_or_else(|e| e.into_inner());
        i2c.write(&[high_nibble, high_nibble & !0x04])?;
        i2c.write(&[low_nibble, low_nibble & !0x04])?;

        Ok(())
    }

    /// Send data to LCD
    #[cfg(target_os = "linux")]
    fn send_data(&mut self, data: u8) -> Result<()> {
        let high_nibble = (data & 0xF0) | 0x0D; // En=1, Rs=1, Rw=0
        let low_nibble = ((data << 4) & 0xF0) | 0x0D;

        let mut i2c = self.i2c.lock().unwrap_or_else(|e| e.into_inner());
        i2c.write(&[high_nibble, high_nibble & !0x04])?;
        i2c.write(&[low_nibble, low_nibble & !0x04])?;

        Ok(())
    }
}

#[async_trait]
impl Display for I2cLcdDisplay {
    async fn write(&mut self, text: &str) -> Result<()> {
        #[cfg(target_os = "linux")]
        for byte in text.bytes() {
            if self.cursor_col >= self.cols {
                break;
            }
            self.send_data(byte)?;
            self.cursor_col += 1;
        }

        #[cfg(not(target_os = "linux"))]
        info!("[MOCK] LCD write: {}", text);

        Ok(())
    }

    async fn clear(&mut self) -> Result<()> {
        #[cfg(target_os = "linux")]
        self.send_command(0x01)?;

        #[cfg(not(target_os = "linux"))]
        info!("[MOCK] LCD cleared");

        self.cursor_row = 0;
        self.cursor_col = 0;
        Ok(())
    }

    async fn set_cursor(&mut self, row: u8, col: u8) -> Result<()> {
        if row >= self.rows || col >= self.cols {
            anyhow::bail!("Cursor position out of bounds: ({}, {})", row, col);
        }

        // Row offsets for HD44780
        let row_offsets = [0x00, 0x40, 0x14, 0x54];
        let offset = row_offsets[row as usize] + col;

        #[cfg(target_os = "linux")]
        self.send_command(0x80 | offset)?;

        #[cfg(not(target_os = "linux"))]
        info!("[MOCK] LCD cursor set to ({}, {})", row, col);

        self.cursor_row = row;
        self.cursor_col = col;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Real I2C is only available on a Raspberry Pi; off-Pi hosts fail
    // `I2c::new()` legitimately, so skip rather than fail spuriously.
    fn make_display() -> Option<I2cLcdDisplay> {
        match I2cLcdDisplay::new(0x27, 2, 16) {
            Ok(display) => Some(display),
            Err(e) => {
                eprintln!("skipping I2C display test (no Pi hardware): {e:#}");
                None
            }
        }
    }

    #[tokio::test]
    async fn test_display_creation() {
        let Some(display) = make_display() else { return };
        assert_eq!(display.rows, 2);
        assert_eq!(display.cols, 16);
    }

    #[tokio::test]
    async fn test_display_write() {
        let Some(mut display) = make_display() else { return };
        assert!(display.write("Hello").await.is_ok());
        assert!(display.clear().await.is_ok());
    }
}
