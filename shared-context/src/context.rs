// SPDX-License-Identifier: PMPL-1.0
//! Shared context for coordinating bot executions

use crate::bot::{BotExecution, BotId, BotStatus, Tier};
use crate::finding::{Finding, FindingSet, Severity};
use crate::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use uuid::Uuid;

/// Shared context for a repository analysis session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Context {
    /// Unique session ID
    pub session_id: Uuid,
    /// Repository name
    pub repo_name: String,
    /// Path to repository
    pub repo_path: PathBuf,
    /// When the session started
    pub started_at: DateTime<Utc>,
    /// When the session completed (if completed)
    pub completed_at: Option<DateTime<Utc>>,
    /// Bot execution records
    pub executions: HashMap<BotId, BotExecution>,
    /// All findings from all bots
    pub findings: FindingSet,
    /// Shared key-value store for bot communication
    pub data: HashMap<String, serde_json::Value>,
    /// Session configuration overrides
    pub config: ContextConfig,
}

impl Context {
    /// Create a new context for a repository
    pub fn new(repo_name: &str, repo_path: impl Into<PathBuf>) -> Self {
        Self {
            session_id: Uuid::new_v4(),
            repo_name: repo_name.to_string(),
            repo_path: repo_path.into(),
            started_at: Utc::now(),
            completed_at: None,
            executions: HashMap::new(),
            findings: FindingSet::new(),
            data: HashMap::new(),
            config: ContextConfig::default(),
        }
    }

    /// Register a bot for execution
    pub fn register_bot(&mut self, bot: BotId) {
        self.executions.insert(bot, BotExecution::new(bot));
    }

    /// Register all standard bots
    pub fn register_all_bots(&mut self) {
        for bot in BotId::all() {
            self.register_bot(bot);
        }
    }

    /// Start a bot's execution
    pub fn start_bot(&mut self, bot: BotId) -> Result<()> {
        let execution = self
            .executions
            .get_mut(&bot)
            .ok_or_else(|| crate::ContextError::BotNotRegistered(bot.to_string()))?;
        execution.start();
        Ok(())
    }

    /// Complete a bot's execution
    pub fn complete_bot(
        &mut self,
        bot: BotId,
        findings_count: usize,
        errors_count: usize,
        files_analyzed: usize,
    ) -> Result<()> {
        let execution = self
            .executions
            .get_mut(&bot)
            .ok_or_else(|| crate::ContextError::BotNotRegistered(bot.to_string()))?;
        execution.complete(findings_count, errors_count, files_analyzed);
        Ok(())
    }

    /// Mark a bot as failed
    pub fn fail_bot(&mut self, bot: BotId, error: &str) -> Result<()> {
        let execution = self
            .executions
            .get_mut(&bot)
            .ok_or_else(|| crate::ContextError::BotNotRegistered(bot.to_string()))?;
        execution.fail(error);
        Ok(())
    }

    /// Skip a bot
    pub fn skip_bot(&mut self, bot: BotId, reason: &str) -> Result<()> {
        let execution = self
            .executions
            .get_mut(&bot)
            .ok_or_else(|| crate::ContextError::BotNotRegistered(bot.to_string()))?;
        execution.skip(reason);
        Ok(())
    }

    /// Add a finding
    pub fn add_finding(&mut self, finding: Finding) {
        self.findings.add(finding);
    }

    /// Add multiple findings
    pub fn add_findings(&mut self, findings: impl IntoIterator<Item = Finding>) {
        self.findings.extend(findings);
    }

    /// Get findings from a specific bot
    pub fn findings_from(&self, bot: BotId) -> Vec<&Finding> {
        self.findings.by_source(bot)
    }

    /// Get findings from all bots in a tier
    pub fn findings_from_tier(&self, tier: Tier) -> Vec<&Finding> {
        self.findings
            .findings
            .iter()
            .filter(|f| f.source.tier() == tier)
            .collect()
    }

    /// Get findings by category
    pub fn findings_by_category(&self, category: &str) -> Vec<&Finding> {
        self.findings.by_category(category)
    }

    /// Check if any errors exist
    pub fn has_errors(&self) -> bool {
        self.findings.has_errors()
    }

    /// Check if release should be blocked
    pub fn blocks_release(&self) -> bool {
        self.findings.blocks_release()
    }

    /// Store data for sharing between bots
    pub fn set_data(&mut self, key: &str, value: serde_json::Value) {
        self.data.insert(key.to_string(), value);
    }

    /// Get shared data
    pub fn get_data(&self, key: &str) -> Option<&serde_json::Value> {
        self.data.get(key)
    }

    /// Get shared data as specific type
    pub fn get_data_as<T: for<'de> Deserialize<'de>>(&self, key: &str) -> Option<T> {
        self.data
            .get(key)
            .and_then(|v| serde_json::from_value(v.clone()).ok())
    }

    /// Check if a bot has completed
    pub fn bot_completed(&self, bot: BotId) -> bool {
        self.executions
            .get(&bot)
            .map(|e| matches!(e.status, BotStatus::Completed | BotStatus::Skipped))
            .unwrap_or(false)
    }

    /// Check if all tier-1 (verifier) bots have completed
    pub fn verifiers_complete(&self) -> bool {
        Tier::Verifier
            .bots()
            .iter()
            .all(|bot| self.bot_completed(*bot) || !self.executions.contains_key(bot))
    }

    /// Get bots ready to run (dependencies satisfied)
    pub fn ready_bots(&self) -> Vec<BotId> {
        use crate::bot::BotInfo;

        let mut ready = Vec::new();

        for (bot, execution) in &self.executions {
            if execution.status != BotStatus::Pending {
                continue;
            }

            let info = BotInfo::standard(*bot);
            let deps_satisfied = info
                .depends_on
                .iter()
                .all(|dep| self.bot_completed(*dep));

            if deps_satisfied {
                ready.push(*bot);
            }
        }

        ready
    }

    /// Mark session as complete
    pub fn complete_session(&mut self) {
        self.completed_at = Some(Utc::now());
    }

    /// Get session duration in milliseconds
    pub fn duration_ms(&self) -> Option<u64> {
        self.completed_at
            .map(|end| (end - self.started_at).num_milliseconds() as u64)
    }

    /// Generate summary statistics
    pub fn summary(&self) -> ContextSummary {
        let mut bots_run = 0;
        let mut bots_failed = 0;
        let mut total_files = 0;

        for execution in self.executions.values() {
            match execution.status {
                BotStatus::Completed => {
                    bots_run += 1;
                    total_files += execution.files_analyzed;
                }
                BotStatus::Failed => {
                    bots_failed += 1;
                }
                _ => {}
            }
        }

        ContextSummary {
            session_id: self.session_id,
            repo_name: self.repo_name.clone(),
            duration_ms: self.duration_ms(),
            bots_run,
            bots_failed,
            total_findings: self.findings.len(),
            total_errors: self.findings.errors().len(),
            total_warnings: self.findings.warnings().len(),
            total_files_analyzed: total_files,
            blocks_release: self.blocks_release(),
        }
    }
}

/// Context configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextConfig {
    /// Enable dry-run mode (no changes)
    pub dry_run: bool,
    /// Enable auto-fix mode
    pub auto_fix: bool,
    /// Strict mode (fail on warnings)
    pub strict: bool,
    /// Bots to skip
    pub skip_bots: Vec<BotId>,
    /// Categories to skip
    pub skip_categories: Vec<String>,
    /// Custom bot configuration
    pub bot_config: HashMap<BotId, serde_json::Value>,
}

impl Default for ContextConfig {
    fn default() -> Self {
        Self {
            dry_run: false,
            auto_fix: false,
            strict: false,
            skip_bots: Vec::new(),
            skip_categories: Vec::new(),
            bot_config: HashMap::new(),
        }
    }
}

/// Summary of context execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextSummary {
    pub session_id: Uuid,
    pub repo_name: String,
    pub duration_ms: Option<u64>,
    pub bots_run: usize,
    pub bots_failed: usize,
    pub total_findings: usize,
    pub total_errors: usize,
    pub total_warnings: usize,
    pub total_files_analyzed: usize,
    pub blocks_release: bool,
}

impl ContextSummary {
    /// Print summary to stdout
    pub fn print(&self) {
        println!();
        println!("╔════════════════════════════════════════════════════════════════╗");
        println!("║                    GITBOT FLEET SUMMARY                        ║");
        println!("╠════════════════════════════════════════════════════════════════╣");
        println!("║  Repository: {:50} ║", self.repo_name);
        println!("║  Session:    {:50} ║", self.session_id);
        if let Some(ms) = self.duration_ms {
            println!("║  Duration:   {:50} ║", format!("{}ms", ms));
        }
        println!("╠════════════════════════════════════════════════════════════════╣");
        println!(
            "║  Bots:       {:4} run, {:4} failed                             ║",
            self.bots_run, self.bots_failed
        );
        println!(
            "║  Files:      {:4} analyzed                                      ║",
            self.total_files_analyzed
        );
        println!(
            "║  Findings:   {:4} total ({:3} errors, {:3} warnings)           ║",
            self.total_findings, self.total_errors, self.total_warnings
        );
        println!("╠════════════════════════════════════════════════════════════════╣");

        if self.blocks_release {
            println!("║  STATUS: ❌ RELEASE BLOCKED                                    ║");
        } else if self.total_warnings > 0 {
            println!("║  STATUS: ⚠️  READY WITH WARNINGS                                ║");
        } else {
            println!("║  STATUS: ✅ RELEASE READY                                      ║");
        }

        println!("╚════════════════════════════════════════════════════════════════╝");
        println!();
    }
}
