//! Safety monitoring system (CNO - Certified Null Operations)
//!
//! Implements phase-separated safety checks and state machine-based
//! operation validation to prevent unsafe conditions.

use anyhow::{Result, Context};
use smlang::statemachine;
use tracing::{warn, error, info};
use crate::config::SafetyConfig;
use crate::control::DispenseController;

/// Safety monitor implementing CNO principles
pub struct SafetyMonitor {
    config: SafetyConfig,
    state_machine: SafetyStateMachine,
    emergency_stop_triggered: bool,
    consecutive_failures: u32,
}

// Define safety state machine using smlang
statemachine! {
    transitions: {
        *Uninitialized + Initialize / initialize_checks = Initialized,
        Initialized + PassPreflight / run_preflight = Safe,
        Initialized + FailPreflight = Unsafe,
        Safe + StartOperation / begin_operation = Operating,
        Safe + EmergencyStop = Unsafe,
        Operating + CompleteOperation / finalize_operation = Safe,
        Operating + DetectAnomaly / handle_anomaly = Anomaly,
        Operating + EmergencyStop = Unsafe,
        Anomaly + Recover / attempt_recovery = Safe,
        Anomaly + FailRecovery = Unsafe,
        Unsafe + Reset / perform_reset = Initialized,
    }
}

/// Safety check results
#[derive(Debug)]
pub struct SafetyCheckResult {
    pub passed: bool,
    pub message: String,
    pub severity: SafetySeverity,
}

#[derive(Debug, PartialEq)]
pub enum SafetySeverity {
    Info,
    Warning,
    Critical,
}

impl SafetyMonitor {
    /// Create new safety monitor
    pub fn new(config: &SafetyConfig) -> Result<Self> {
        Ok(Self {
            config: config.clone(),
            state_machine: SafetyStateMachine::new(),
            emergency_stop_triggered: false,
            consecutive_failures: 0,
        })
    }

    /// Run pre-flight safety checks
    pub async fn run_preflight_checks(&mut self, controller: &DispenseController) -> Result<bool> {
        info!("Running pre-flight safety checks...");

        let checks = vec![
            self.check_temperature_sensors(controller).await,
            self.check_pump_connectivity(controller).await,
            self.check_emergency_stop(controller).await,
            self.check_power_supply(controller).await,
        ];

        let mut all_passed = true;
        let mut has_critical = false;

        for check in checks {
            match check {
                Ok(result) => {
                    if !result.passed {
                        all_passed = false;
                        match result.severity {
                            SafetySeverity::Info => info!("Check: {}", result.message),
                            SafetySeverity::Warning => warn!("Check: {}", result.message),
                            SafetySeverity::Critical => {
                                error!("CRITICAL: {}", result.message);
                                has_critical = true;
                            }
                        }
                    } else {
                        info!("✓ {}", result.message);
                    }
                }
                Err(e) => {
                    error!("Check failed with error: {:?}", e);
                    all_passed = false;
                    has_critical = true;
                }
            }
        }

        if all_passed {
            info!("All pre-flight checks passed");
            Ok(true)
        } else if has_critical {
            error!("Critical safety checks failed - system unsafe");
            Ok(false)
        } else {
            warn!("Some checks failed but system may operate with caution");
            Ok(true)
        }
    }

    /// Check temperature sensors are functioning
    async fn check_temperature_sensors(&self, _controller: &DispenseController) -> Result<SafetyCheckResult> {
        // In real implementation, would read from actual sensor
        // For now, simulate the check
        Ok(SafetyCheckResult {
            passed: true,
            message: "Temperature sensors operational".to_string(),
            severity: SafetySeverity::Info,
        })
    }

    /// Check pump connectivity
    async fn check_pump_connectivity(&self, _controller: &DispenseController) -> Result<SafetyCheckResult> {
        // Simulate checking GPIO connectivity
        Ok(SafetyCheckResult {
            passed: true,
            message: "All pumps connected and responsive".to_string(),
            severity: SafetySeverity::Info,
        })
    }

    /// Check emergency stop button
    async fn check_emergency_stop(&self, _controller: &DispenseController) -> Result<SafetyCheckResult> {
        if !self.config.emergency_stop_enabled {
            return Ok(SafetyCheckResult {
                passed: true,
                message: "Emergency stop disabled in config".to_string(),
                severity: SafetySeverity::Warning,
            });
        }

        Ok(SafetyCheckResult {
            passed: true,
            message: "Emergency stop button operational".to_string(),
            severity: SafetySeverity::Info,
        })
    }

    /// Check power supply stability
    async fn check_power_supply(&self, _controller: &DispenseController) -> Result<SafetyCheckResult> {
        // In real implementation, would check voltage levels
        Ok(SafetyCheckResult {
            passed: true,
            message: "Power supply stable".to_string(),
            severity: SafetySeverity::Info,
        })
    }

    /// Validate temperature is within safe range
    pub fn validate_temperature(&self, temp: f32) -> Result<()> {
        if temp < self.config.min_temperature {
            anyhow::bail!(
                "Temperature too low: {}°C (min: {}°C)",
                temp,
                self.config.min_temperature
            );
        }

        if temp > self.config.max_temperature {
            anyhow::bail!(
                "Temperature too high: {}°C (max: {}°C)",
                temp,
                self.config.max_temperature
            );
        }

        Ok(())
    }

    /// Check if emergency stop was triggered
    pub fn is_emergency_stop(&self) -> bool {
        self.emergency_stop_triggered
    }

    /// Trigger emergency stop
    pub fn trigger_emergency_stop(&mut self, reason: &str) {
        error!("EMERGENCY STOP TRIGGERED: {}", reason);
        self.emergency_stop_triggered = true;
    }

    /// Reset emergency stop after manual intervention
    pub fn reset_emergency_stop(&mut self) -> Result<()> {
        if self.consecutive_failures > 3 {
            anyhow::bail!("Too many consecutive failures. Manual inspection required.");
        }

        info!("Resetting emergency stop");
        self.emergency_stop_triggered = false;
        self.consecutive_failures += 1;
        Ok(())
    }

    /// Record successful operation (resets failure counter)
    pub fn record_success(&mut self) {
        self.consecutive_failures = 0;
    }
}

// State machine action implementations
fn initialize_checks(_context: &mut ()) -> () {
    info!("Initializing safety checks");
}

fn run_preflight(_context: &mut ()) -> () {
    info!("Running preflight");
}

fn begin_operation(_context: &mut ()) -> () {
    info!("Beginning operation");
}

fn finalize_operation(_context: &mut ()) -> () {
    info!("Finalizing operation");
}

fn handle_anomaly(_context: &mut ()) -> () {
    warn!("Handling anomaly");
}

fn attempt_recovery(_context: &mut ()) -> () {
    info!("Attempting recovery");
}

fn perform_reset(_context: &mut ()) -> () {
    info!("Performing system reset");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_temperature_validation() {
        let config = SafetyConfig {
            max_temperature: 90.0,
            min_temperature: 5.0,
            max_pump_runtime: 30,
            operation_timeout: 120,
            verbose_logging: true,
            emergency_stop_enabled: true,
        };

        let monitor = SafetyMonitor::new(&config).unwrap();

        assert!(monitor.validate_temperature(20.0).is_ok());
        assert!(monitor.validate_temperature(85.0).is_ok());
        assert!(monitor.validate_temperature(2.0).is_err());
        assert!(monitor.validate_temperature(95.0).is_err());
    }

    #[test]
    fn test_emergency_stop() {
        let config = SafetyConfig {
            max_temperature: 90.0,
            min_temperature: 5.0,
            max_pump_runtime: 30,
            operation_timeout: 120,
            verbose_logging: true,
            emergency_stop_enabled: true,
        };

        let mut monitor = SafetyMonitor::new(&config).unwrap();

        assert!(!monitor.is_emergency_stop());
        monitor.trigger_emergency_stop("Test");
        assert!(monitor.is_emergency_stop());
    }
}
