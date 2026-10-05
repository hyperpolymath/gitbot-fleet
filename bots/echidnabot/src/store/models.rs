// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Database models

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::adapters::Platform;
use crate::dispatcher::ProverKind;
use crate::modes::BotMode;
use crate::scheduler::{JobId, JobPriority, JobStatus};

/// Repository record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Repository {
    pub id: Uuid,
    pub platform: Platform,
    pub owner: String,
    pub name: String,
    pub webhook_secret: Option<String>,
    pub enabled_provers: Vec<ProverKind>,
    pub check_on_push: bool,
    pub check_on_pr: bool,
    pub auto_comment: bool,
    pub enabled: bool,
    pub last_checked_commit: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    /// Per-repo bot operating mode. Reads as the second tier of the cascade
    /// (target-repo `.machine_readable/bot_directives/echidnabot.a2ml`
    /// directive overrides this when present). See `modes::resolve_mode`.
    #[serde(default)]
    pub mode: BotMode,
    /// Regulator-mode coverage threshold (percent, 0..=100).
    /// A commit's check run is marked Failure (= branch protection blocks
    /// merge) only when proven_count * 100 / total_count < this threshold.
    /// 100 means "every proof must pass" (the simplest Regulator); lower
    /// values tolerate flake during incremental coverage growth. Ignored
    /// for non-Regulator modes.
    #[serde(default = "default_regulator_threshold")]
    pub regulator_coverage_threshold: u8,
}

fn default_regulator_threshold() -> u8 {
    100
}

impl Repository {
    pub fn new(platform: Platform, owner: String, name: String) -> Self {
        let now = Utc::now();
        Self {
            id: Uuid::new_v4(),
            platform,
            owner,
            name,
            webhook_secret: None,
            enabled_provers: vec![ProverKind::new("metamath")], // Default to easiest prover
            check_on_push: true,
            check_on_pr: true,
            auto_comment: true,
            enabled: true,
            last_checked_commit: None,
            created_at: now,
            updated_at: now,
            mode: BotMode::default(), // Verifier
            regulator_coverage_threshold: default_regulator_threshold(),
        }
    }

    pub fn full_name(&self) -> String {
        format!("{}/{}", self.owner, self.name)
    }
}

/// Proof job database record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofJobRecord {
    pub id: Uuid,
    pub repo_id: Uuid,
    pub commit_sha: String,
    pub prover: ProverKind,
    pub file_paths: Vec<String>,
    pub status: JobStatus,
    pub priority: JobPriority,
    pub queued_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub error_message: Option<String>,
    /// PR number that triggered the job (None for direct push events).
    /// Used by the result-reporter to comment on the originating PR.
    #[serde(default)]
    pub pr_number: Option<u64>,
    /// Webhook delivery ID for traceability.
    #[serde(default)]
    pub delivery_id: Option<String>,
}

impl From<crate::scheduler::ProofJob> for ProofJobRecord {
    fn from(job: crate::scheduler::ProofJob) -> Self {
        Self {
            id: job.id.0,
            repo_id: job.repo_id,
            commit_sha: job.commit_sha,
            prover: job.prover,
            file_paths: job.file_paths,
            status: job.status,
            priority: job.priority,
            queued_at: job.queued_at,
            started_at: job.started_at,
            completed_at: job.completed_at,
            error_message: job
                .result
                .as_ref()
                .filter(|r| !r.success)
                .map(|r| r.message.clone()),
            pr_number: job.pr_number,
            delivery_id: job.delivery_id,
        }
    }
}

/// Proof result database record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofResultRecord {
    pub id: Uuid,
    pub job_id: Uuid,
    pub success: bool,
    pub message: String,
    pub prover_output: String,
    pub duration_ms: i64,
    pub verified_files: Vec<String>,
    pub failed_files: Vec<String>,
    pub created_at: DateTime<Utc>,
}

impl ProofResultRecord {
    pub fn new(job_id: JobId, result: &crate::scheduler::JobResult) -> Self {
        Self {
            id: Uuid::new_v4(),
            job_id: job_id.0,
            success: result.success,
            message: result.message.clone(),
            prover_output: result.prover_output.clone(),
            duration_ms: result.duration_ms as i64,
            verified_files: result.verified_files.clone(),
            failed_files: result.failed_files.clone(),
            created_at: Utc::now(),
        }
    }
}

/// Check run record (for tracking GitHub/GitLab status updates)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckRunRecord {
    pub id: Uuid,
    pub job_id: Uuid,
    pub platform: Platform,
    pub external_id: String, // Platform-specific ID
    pub status: String,
    pub conclusion: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Tactic outcome record — feeds the double-loop reranker (Package 7b).
/// `job_id` is optional so ad-hoc calls (MCP tool invocations, CLI) can record
/// outcomes even when no webhook-driven proof job exists.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TacticOutcomeRecord {
    pub id: Uuid,
    pub job_id: Option<Uuid>,
    pub prover: ProverKind,
    pub goal_fingerprint: String,
    pub tactic: String,
    pub succeeded: bool,
    pub duration_ms: i64,
    pub created_at: DateTime<Utc>,
}

impl TacticOutcomeRecord {
    pub fn new(
        job_id: Option<Uuid>,
        prover: ProverKind,
        goal_fingerprint: String,
        tactic: String,
        succeeded: bool,
        duration_ms: i64,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            job_id,
            prover,
            goal_fingerprint,
            tactic,
            succeeded,
            duration_ms,
            created_at: Utc::now(),
        }
    }
}

/// Stable fingerprint of a goal-state string for reranker similarity lookups.
/// Normalises whitespace + case, then SHA-256 hex. Not a cryptographic identity;
/// lexically-identical goals collide by design so the reranker can aggregate.
pub fn goal_fingerprint(goal_state: &str) -> String {
    use sha2::{Digest, Sha256};
    let normalised = goal_state
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .to_lowercase();
    let digest = Sha256::digest(normalised.as_bytes());
    let mut out = String::with_capacity(64);
    for byte in digest.iter() {
        use std::fmt::Write;
        let _ = write!(out, "{:02x}", byte);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fingerprint_is_whitespace_and_case_insensitive() {
        let a = goal_fingerprint("forall x : Nat, x + 0 = x");
        let b = goal_fingerprint("FORALL   x : Nat,\n  x  +  0 = x");
        assert_eq!(a, b);
    }

    #[test]
    fn fingerprint_distinguishes_distinct_goals() {
        let a = goal_fingerprint("forall x, x = x");
        let b = goal_fingerprint("forall x, x = 0");
        assert_ne!(a, b);
    }

    #[test]
    fn fingerprint_is_sha256_hex_length() {
        assert_eq!(goal_fingerprint("any").len(), 64);
    }
}
