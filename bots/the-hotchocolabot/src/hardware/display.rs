//! LCD display implementation using I2C

use crate::hardware::Display;
use anyhow::{Result, Context};
use async_trait::async_trait;
use tracing::info;

#[cfg(target_os = "linux")]
use rppal::i2c::I2c;

/// I2C LCD display (e.g., 16x2 or 20x4 with PCF8574 backpack)
pub struct I2cLcdDisplay {
    #[cfg(target_os = "linux")]
    i2c: I2c,

    #[cfg(not(target_os = "linux"))]
    address: u8,

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
            i2c,
            #[cfg(not(target_os = "linux"))]
            address,

            rows,
            cols,
            cursor_row: 0,
            cursor_col: 0,
        };

        // Initialize display
        display.initialize().await?;

        Ok(display)
    }

    /// Initialize LCD display
    async fn initialize(&mut self) -> Result<()> {
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

        self.i2c.write(&[high_nibble, high_nibble & !0x04])?;
        self.i2c.write(&[low_nibble, low_nibble & !0x04])?;

        Ok(())
    }

    /// Send data to LCD
    #[cfg(target_os = "linux")]
    fn send_data(&mut self, data: u8) -> Result<()> {
        let high_nibble = (data & 0xF0) | 0x0D; // En=1, Rs=1, Rw=0
        let low_nibble = ((data << 4) & 0xF0) | 0x0D;

        self.i2c.write(&[high_nibble, high_nibble & !0x04])?;
        self.i2c.write(&[low_nibble, low_nibble & !0x04])?;

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

    #[tokio::test]
    async fn test_display_creation() {
        let display = I2cLcdDisplay::new(0x27, 2, 16).unwrap();
        assert_eq!(display.rows, 2);
        assert_eq!(display.cols, 16);
    }

    #[tokio::test]
    async fn test_display_write() {
        let mut display = I2cLcdDisplay::new(0x27, 2, 16).unwrap();
        assert!(display.write("Hello").await.is_ok());
        assert!(display.clear().await.is_ok());
    }
}
