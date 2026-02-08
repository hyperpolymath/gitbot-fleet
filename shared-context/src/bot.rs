// SPDX-License-Identifier: PMPL-1.0
//! Bot identification and metadata

use serde::{Deserialize, Serialize};
use std::fmt;

/// Unique identifier for each bot in the fleet
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum BotId {
    /// RSR structural compliance validator
    Rhodibot,
    /// Mathematical/formal verification
    Echidnabot,
    /// Ecological/economic standards
    Sustainabot,
    /// Presentation quality (accessibility, SEO)
    Glambot,
    /// Integration testing
    Seambot,
    /// Release readiness validation
    Finishbot,
    /// Workflow cleanup and security executor
    RobotRepoAutomaton,
    /// Neurosymbolic CI/CD intelligence platform
    Hypatia,
    /// WCAG accessibility compliance validator
    Accessibilitybot,
    /// Custom/external bot
    Custom(u32),
}

impl fmt::Display for BotId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BotId::Rhodibot => write!(f, "rhodibot"),
            BotId::Echidnabot => write!(f, "echidnabot"),
            BotId::Sustainabot => write!(f, "sustainabot"),
            BotId::Glambot => write!(f, "glambot"),
            BotId::Seambot => write!(f, "seambot"),
            BotId::Finishbot => write!(f, "finishbot"),
            BotId::RobotRepoAutomaton => write!(f, "robot-repo-automaton"),
            BotId::Hypatia => write!(f, "hypatia"),
            BotId::Accessibilitybot => write!(f, "accessibilitybot"),
            BotId::Custom(id) => write!(f, "custom-{}", id),
        }
    }
}

impl BotId {
    /// Get the tier this bot belongs to
    pub fn tier(&self) -> Tier {
        match self {
            BotId::Rhodibot | BotId::Echidnabot | BotId::Sustainabot => Tier::Verifier,
            BotId::Glambot | BotId::Seambot | BotId::Finishbot | BotId::Accessibilitybot => Tier::Finisher,
            BotId::RobotRepoAutomaton => Tier::Executor,
            BotId::Hypatia => Tier::Engine,
            BotId::Custom(_) => Tier::Custom,
        }
    }

    /// Get all standard bot IDs
    pub fn all() -> Vec<BotId> {
        vec![
            BotId::Rhodibot,
            BotId::Echidnabot,
            BotId::Sustainabot,
            BotId::Glambot,
            BotId::Seambot,
            BotId::Finishbot,
            BotId::RobotRepoAutomaton,
            BotId::Hypatia,
            BotId::Accessibilitybot,
        ]
    }

    /// Parse from string
    pub fn from_str(s: &str) -> Option<BotId> {
        match s.to_lowercase().as_str() {
            "rhodibot" => Some(BotId::Rhodibot),
            "echidnabot" => Some(BotId::Echidnabot),
            "sustainabot" => Some(BotId::Sustainabot),
            "glambot" => Some(BotId::Glambot),
            "seambot" => Some(BotId::Seambot),
            "finishbot" | "finishingbot" | "finishing-bot" => Some(BotId::Finishbot),
            "robot-repo-automaton" | "robotrepoautomaton" => Some(BotId::RobotRepoAutomaton),
            "hypatia" | "cicd-hyper-a" | "cicdhypera" => Some(BotId::Hypatia),
            "accessibilitybot" | "accessibility-bot" => Some(BotId::Accessibilitybot),
            _ => None,
        }
    }
}

/// Bot execution tier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Tier {
    /// First tier - produces findings (rhodibot, echidnabot, sustainabot)
    Verifier,
    /// Second tier - consumes findings, produces results (glambot, seambot, finishbot)
    Finisher,
    /// Third tier - executes actions based on findings (robot-repo-automaton)
    Executor,
    /// Central intelligence layer - coordinates all bots (hypatia)
    Engine,
    /// Custom/external bot
    Custom,
}

impl Tier {
    /// Get execution order (lower = earlier)
    pub fn execution_order(&self) -> u8 {
        match self {
            Tier::Engine => 0,    // Engine coordinates, runs first
            Tier::Verifier => 1,
            Tier::Finisher => 2,
            Tier::Executor => 3,  // Executor runs after verification
            Tier::Custom => 4,
        }
    }

    /// Get all bots in this tier
    pub fn bots(&self) -> Vec<BotId> {
        BotId::all()
            .into_iter()
            .filter(|b| b.tier() == *self)
            .collect()
    }
}

/// Bot metadata and capabilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BotInfo {
    /// Unique identifier
    pub id: BotId,
    /// Human-readable name
    pub name: String,
    /// Description of purpose
    pub description: String,
    /// Version string
    pub version: String,
    /// Supported check categories
    pub categories: Vec<String>,
    /// Whether this bot can auto-fix issues
    pub can_fix: bool,
    /// Dependencies on other bots (must run first)
    pub depends_on: Vec<BotId>,
}

impl BotInfo {
    /// Create info for a standard bot
    pub fn standard(id: BotId) -> Self {
        match id {
            BotId::Rhodibot => Self {
                id,
                name: "Rhodibot".to_string(),
                description: "RSR (Rhodium Standard Repositories) compliance validation".to_string(),
                version: "0.1.0".to_string(),
                categories: vec![
                    "structure".to_string(),
                    "files".to_string(),
                    "layout".to_string(),
                ],
                can_fix: true,
                depends_on: vec![],
            },
            BotId::Echidnabot => Self {
                id,
                name: "Echidnabot".to_string(),
                description: "Formal mathematical/statistical verification and fuzzing".to_string(),
                version: "0.1.0".to_string(),
                categories: vec![
                    "verification".to_string(),
                    "fuzzing".to_string(),
                    "proofs".to_string(),
                ],
                can_fix: false,
                depends_on: vec![],
            },
            BotId::Sustainabot => Self {
                id,
                name: "Sustainabot".to_string(),
                description: "Ecological and economic code standards".to_string(),
                version: "0.1.0".to_string(),
                categories: vec![
                    "sustainability".to_string(),
                    "efficiency".to_string(),
                    "debt".to_string(),
                ],
                can_fix: false,
                depends_on: vec![],
            },
            BotId::Glambot => Self {
                id,
                name: "Glambot".to_string(),
                description: "Presentation quality - accessibility, SEO, machine-readability".to_string(),
                version: "0.1.0".to_string(),
                categories: vec![
                    "accessibility".to_string(),
                    "seo".to_string(),
                    "html".to_string(),
                    "docs".to_string(),
                ],
                can_fix: true,
                depends_on: vec![BotId::Rhodibot],
            },
            BotId::Seambot => Self {
                id,
                name: "Seambot".to_string(),
                description: "Architectural seam analysis - drift detection, hidden channels, forge integration".to_string(),
                version: "0.1.0".to_string(),
                categories: vec![
                    "seam-analysis".to_string(),
                    "drift-detection".to_string(),
                    "hidden-channels".to_string(),
                    "forge-integration".to_string(),
                    "integration".to_string(),
                    "api".to_string(),
                    "contracts".to_string(),
                ],
                can_fix: false,
                depends_on: vec![BotId::Rhodibot, BotId::Echidnabot],
            },
            BotId::Finishbot => Self {
                id,
                name: "Finishing Bot".to_string(),
                description: "Release readiness - placeholders, licenses, claims".to_string(),
                version: "0.1.0".to_string(),
                categories: vec![
                    "license".to_string(),
                    "placeholder".to_string(),
                    "claims".to_string(),
                    "release".to_string(),
                ],
                can_fix: true,
                depends_on: vec![BotId::Rhodibot, BotId::Glambot],
            },
            BotId::RobotRepoAutomaton => Self {
                id,
                name: "Robot Repo Automaton".to_string(),
                description: "Workflow cleanup and security executor for repository automation".to_string(),
                version: "0.1.0".to_string(),
                categories: vec![
                    "security".to_string(),
                    "workflow".to_string(),
                    "structure".to_string(),
                ],
                can_fix: true,
                depends_on: vec![], // Executes based on rules from hypatia
            },
            BotId::Hypatia => Self {
                id,
                name: "Hypatia".to_string(),
                description: "Neurosymbolic CI/CD intelligence platform - central rules engine".to_string(),
                version: "0.1.0".to_string(),
                categories: vec![
                    "rules".to_string(),
                    "learning".to_string(),
                    "coordination".to_string(),
                ],
                can_fix: false, // Engine provides rules, doesn't directly fix
                depends_on: vec![], // Engine is the root, no dependencies
            },
            BotId::Accessibilitybot => Self {
                id,
                name: "Accessibilitybot".to_string(),
                description: "WCAG 2.3 AAA accessibility compliance validator".to_string(),
                version: "0.1.0".to_string(),
                categories: vec![
                    "accessibility/wcag-a".to_string(),
                    "accessibility/wcag-aa".to_string(),
                    "accessibility/wcag-aaa".to_string(),
                    "accessibility/aria".to_string(),
                    "accessibility/css".to_string(),
                ],
                can_fix: true,
                depends_on: vec![BotId::Rhodibot, BotId::Glambot],
            },
            BotId::Custom(_) => Self {
                id,
                name: "Custom Bot".to_string(),
                description: "Custom/external bot".to_string(),
                version: "0.0.0".to_string(),
                categories: vec![],
                can_fix: false,
                depends_on: vec![],
            },
        }
    }
}

/// Bot execution status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum BotStatus {
    /// Not yet started
    Pending,
    /// Currently running
    Running,
    /// Completed successfully
    Completed,
    /// Failed with errors
    Failed,
    /// Skipped (e.g., not applicable)
    Skipped,
}

/// Record of a bot's execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BotExecution {
    /// Which bot
    pub bot: BotId,
    /// Current status
    pub status: BotStatus,
    /// When started (if started)
    pub started_at: Option<chrono::DateTime<chrono::Utc>>,
    /// When completed (if completed)
    pub completed_at: Option<chrono::DateTime<chrono::Utc>>,
    /// Duration in milliseconds
    pub duration_ms: Option<u64>,
    /// Number of findings produced
    pub findings_count: usize,
    /// Number of errors
    pub errors_count: usize,
    /// Number of files analyzed
    pub files_analyzed: usize,
    /// Error message if failed
    pub error_message: Option<String>,
}

impl BotExecution {
    /// Create a new pending execution record
    pub fn new(bot: BotId) -> Self {
        Self {
            bot,
            status: BotStatus::Pending,
            started_at: None,
            completed_at: None,
            duration_ms: None,
            findings_count: 0,
            errors_count: 0,
            files_analyzed: 0,
            error_message: None,
        }
    }

    /// Mark as started
    pub fn start(&mut self) {
        self.status = BotStatus::Running;
        self.started_at = Some(chrono::Utc::now());
    }

    /// Mark as completed
    pub fn complete(&mut self, findings: usize, errors: usize, files: usize) {
        let now = chrono::Utc::now();
        self.status = if errors > 0 {
            BotStatus::Completed // Has errors but completed
        } else {
            BotStatus::Completed
        };
        self.completed_at = Some(now);
        self.findings_count = findings;
        self.errors_count = errors;
        self.files_analyzed = files;

        if let Some(started) = self.started_at {
            self.duration_ms = Some((now - started).num_milliseconds() as u64);
        }
    }

    /// Mark as failed
    pub fn fail(&mut self, error: &str) {
        self.status = BotStatus::Failed;
        self.completed_at = Some(chrono::Utc::now());
        self.error_message = Some(error.to_string());

        if let Some(started) = self.started_at {
            self.duration_ms = Some((chrono::Utc::now() - started).num_milliseconds() as u64);
        }
    }

    /// Mark as skipped
    pub fn skip(&mut self, reason: &str) {
        self.status = BotStatus::Skipped;
        self.error_message = Some(reason.to_string());
    }
}
