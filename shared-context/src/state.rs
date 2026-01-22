// SPDX-License-Identifier: PMPL-1.0
//! State management for session and repository tracking

use crate::bot::BotId;
use crate::context::{Context, ContextSummary};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use uuid::Uuid;

/// Session state - tracks a single analysis run
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionState {
    /// Session ID
    pub id: Uuid,
    /// Repository being analyzed
    pub repo: String,
    /// Repository path
    pub path: PathBuf,
    /// When session started
    pub started_at: DateTime<Utc>,
    /// When session ended
    pub ended_at: Option<DateTime<Utc>>,
    /// Current phase
    pub phase: SessionPhase,
    /// Bots that have run
    pub completed_bots: Vec<BotId>,
    /// Bots that failed
    pub failed_bots: Vec<BotId>,
    /// Summary (when complete)
    pub summary: Option<ContextSummary>,
}

impl SessionState {
    /// Create from context
    pub fn from_context(ctx: &Context) -> Self {
        Self {
            id: ctx.session_id,
            repo: ctx.repo_name.clone(),
            path: ctx.repo_path.clone(),
            started_at: ctx.started_at,
            ended_at: ctx.completed_at,
            phase: if ctx.completed_at.is_some() {
                SessionPhase::Complete
            } else if ctx.verifiers_complete() {
                SessionPhase::Finishing
            } else {
                SessionPhase::Verifying
            },
            completed_bots: ctx
                .executions
                .iter()
                .filter(|(_, e)| {
                    matches!(
                        e.status,
                        crate::bot::BotStatus::Completed | crate::bot::BotStatus::Skipped
                    )
                })
                .map(|(id, _)| *id)
                .collect(),
            failed_bots: ctx
                .executions
                .iter()
                .filter(|(_, e)| e.status == crate::bot::BotStatus::Failed)
                .map(|(id, _)| *id)
                .collect(),
            summary: if ctx.completed_at.is_some() {
                Some(ctx.summary())
            } else {
                None
            },
        }
    }
}

/// Session execution phase
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SessionPhase {
    /// Session created but not started
    Pending,
    /// Running tier-1 verifier bots
    Verifying,
    /// Running tier-2 finisher bots
    Finishing,
    /// Applying fixes
    Fixing,
    /// Session complete
    Complete,
    /// Session failed
    Failed,
}

/// Repository state - tracks long-term repo health
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepoState {
    /// Repository name
    pub name: String,
    /// Repository path
    pub path: PathBuf,
    /// Last analysis session ID
    pub last_session: Option<Uuid>,
    /// Last analysis time
    pub last_analyzed: Option<DateTime<Utc>>,
    /// Historical summaries (most recent first)
    pub history: Vec<HistoryEntry>,
    /// Persistent findings (not yet fixed)
    pub persistent_findings: Vec<PersistentFinding>,
    /// Bot-specific state
    pub bot_state: HashMap<BotId, serde_json::Value>,
}

impl RepoState {
    /// Create new repo state
    pub fn new(name: &str, path: PathBuf) -> Self {
        Self {
            name: name.to_string(),
            path,
            last_session: None,
            last_analyzed: None,
            history: Vec::new(),
            persistent_findings: Vec::new(),
            bot_state: HashMap::new(),
        }
    }

    /// Record a completed session
    pub fn record_session(&mut self, summary: ContextSummary) {
        self.last_session = Some(summary.session_id);
        self.last_analyzed = Some(Utc::now());

        // Add to history (keep last 100)
        self.history.insert(
            0,
            HistoryEntry {
                session_id: summary.session_id,
                timestamp: Utc::now(),
                errors: summary.total_errors,
                warnings: summary.total_warnings,
                total_findings: summary.total_findings,
                blocked_release: summary.blocks_release,
            },
        );

        if self.history.len() > 100 {
            self.history.truncate(100);
        }
    }

    /// Add a persistent finding (one that spans sessions)
    pub fn add_persistent_finding(&mut self, finding: PersistentFinding) {
        self.persistent_findings.push(finding);
    }

    /// Mark a persistent finding as resolved
    pub fn resolve_finding(&mut self, finding_id: Uuid) {
        if let Some(f) = self
            .persistent_findings
            .iter_mut()
            .find(|f| f.id == finding_id)
        {
            f.resolved_at = Some(Utc::now());
        }
    }

    /// Get unresolved persistent findings
    pub fn unresolved_findings(&self) -> Vec<&PersistentFinding> {
        self.persistent_findings
            .iter()
            .filter(|f| f.resolved_at.is_none())
            .collect()
    }

    /// Get trend (improving/declining/stable)
    pub fn trend(&self) -> Trend {
        if self.history.len() < 2 {
            return Trend::Stable;
        }

        let recent = &self.history[0];
        let previous = &self.history[1];

        let recent_score = recent.errors * 10 + recent.warnings;
        let previous_score = previous.errors * 10 + previous.warnings;

        if recent_score < previous_score {
            Trend::Improving
        } else if recent_score > previous_score {
            Trend::Declining
        } else {
            Trend::Stable
        }
    }
}

/// History entry for tracking trends
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HistoryEntry {
    pub session_id: Uuid,
    pub timestamp: DateTime<Utc>,
    pub errors: usize,
    pub warnings: usize,
    pub total_findings: usize,
    pub blocked_release: bool,
}

/// A finding that persists across sessions until resolved
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PersistentFinding {
    /// Finding ID
    pub id: Uuid,
    /// Original bot source
    pub source: BotId,
    /// Rule ID
    pub rule_id: String,
    /// Message
    pub message: String,
    /// File (if applicable)
    pub file: Option<PathBuf>,
    /// When first detected
    pub first_seen: DateTime<Utc>,
    /// Last session where seen
    pub last_seen: Uuid,
    /// How many sessions it's been present
    pub sessions_present: usize,
    /// When resolved (if resolved)
    pub resolved_at: Option<DateTime<Utc>>,
}

/// Health trend
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Trend {
    /// Getting better
    Improving,
    /// Getting worse
    Declining,
    /// No change
    Stable,
}

impl Trend {
    /// Get emoji icon
    pub fn icon(&self) -> &'static str {
        match self {
            Trend::Improving => "📈",
            Trend::Declining => "📉",
            Trend::Stable => "➡️",
        }
    }
}
