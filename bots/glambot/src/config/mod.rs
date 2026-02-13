// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Configuration for glambot

use crate::error::{GlambotError, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub visual: VisualConfig,
    pub accessibility: AccessibilityConfig,
    pub seo: SeoConfig,
    pub machine: MachineConfig,
    pub dry_run: bool,
    pub auto_fix: bool,
    pub exclude: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisualConfig {
    pub max_line_length: usize,
    pub enforce_badges: bool,
    pub require_logo: bool,
    pub check_formatting: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccessibilityConfig {
    pub wcag_level: WcagLevel,
    pub require_alt_text: bool,
    pub check_heading_hierarchy: bool,
    pub check_link_text: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum WcagLevel {
    A,
    AA,
    AAA,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeoConfig {
    pub require_meta_tags: bool,
    pub require_opengraph: bool,
    pub check_titles: bool,
    pub check_descriptions: bool,
    /// Enable git-seo integration for comprehensive forge-based SEO analysis
    pub enable_git_seo: Option<bool>,
    /// Enable automatic application of git-seo fixes (requires GITHUB_TOKEN)
    pub enable_auto_apply: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MachineConfig {
    pub validate_json: bool,
    pub validate_yaml: bool,
    pub require_schema_org: bool,
    pub check_structured_data: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            visual: VisualConfig::default(),
            accessibility: AccessibilityConfig::default(),
            seo: SeoConfig::default(),
            machine: MachineConfig::default(),
            dry_run: false,
            auto_fix: false,
            exclude: vec![
                "target".to_string(),
                "node_modules".to_string(),
                ".git".to_string(),
                "dist".to_string(),
                "build".to_string(),
            ],
        }
    }
}

impl Default for VisualConfig {
    fn default() -> Self {
        Self {
            max_line_length: 80,
            enforce_badges: true,
            require_logo: false,
            check_formatting: true,
        }
    }
}

impl Default for AccessibilityConfig {
    fn default() -> Self {
        Self {
            wcag_level: WcagLevel::AA,
            require_alt_text: true,
            check_heading_hierarchy: true,
            check_link_text: true,
        }
    }
}

impl Default for SeoConfig {
    fn default() -> Self {
        Self {
            require_meta_tags: false,
            require_opengraph: false,
            check_titles: true,
            check_descriptions: true,
            enable_git_seo: Some(true),  // Enable by default
            enable_auto_apply: Some(false),  // Safety: manual by default
        }
    }
}

impl Default for MachineConfig {
    fn default() -> Self {
        Self {
            validate_json: true,
            validate_yaml: true,
            require_schema_org: false,
            check_structured_data: true,
        }
    }
}

pub fn default_config_path() -> PathBuf {
    dirs::config_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join("glambot")
        .join("config.yml")
}

pub fn load_config(path: &Path) -> Result<Config> {
    if !path.exists() {
        return Ok(Config::default());
    }

    let content = std::fs::read_to_string(path)?;

    if path.extension().and_then(|s| s.to_str()) == Some("toml") {
        toml::from_str(&content)
            .map_err(|e| GlambotError::Config(format!("TOML parse error: {}", e)))
    } else {
        serde_yaml::from_str(&content)
            .map_err(|e| GlambotError::Config(format!("YAML parse error: {}", e)))
    }
}

pub fn write_default_config(path: &Path) -> Result<()> {
    let config = Config::default();

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let content = if path.extension().and_then(|s| s.to_str()) == Some("toml") {
        toml::to_string_pretty(&config)
            .map_err(|e| GlambotError::Config(format!("TOML serialize error: {}", e)))?
    } else {
        serde_yaml::to_string(&config)?
    };

    std::fs::write(path, content)?;
    Ok(())
}
