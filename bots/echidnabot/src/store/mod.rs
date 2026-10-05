// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Persistent state store

pub mod models;
mod sqlite;

pub use sqlite::SqliteStore;

use async_trait::async_trait;
use uuid::Uuid;

use crate::adapters::Platform;
use crate::dispatcher::ProverKind;
use crate::error::Result;
use crate::scheduler::JobId;
use models::{ProofJobRecord, ProofResultRecord, Repository, TacticOutcomeRecord};

/// Per-commit coverage view — total proof attempts vs successful ones.
/// Empty results means no jobs run yet for that commit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CommitCoverage {
    pub total: u64,
    pub proven: u64,
}

impl CommitCoverage {
    /// Coverage as a 0–100 percentage. Returns 100 for the empty case
    /// (no jobs run) so threshold checks default to "passing" before any
    /// proof has been attempted; the check run won't post until at least
    /// one job has finalized anyway, so this is a no-op corner.
    pub fn percent(&self) -> u8 {
        self.proven
            .saturating_mul(100)
            .checked_div(self.total)
            .unwrap_or(100)
            .min(100) as u8
    }
}

/// Abstract store trait for different database backends
#[async_trait]
pub trait Store: Send + Sync {
    // Repository operations
    async fn create_repository(&self, repo: &Repository) -> Result<()>;
    async fn get_repository(&self, id: Uuid) -> Result<Option<Repository>>;
    async fn get_repository_by_name(
        &self,
        platform: Platform,
        owner: &str,
        name: &str,
    ) -> Result<Option<Repository>>;
    async fn list_repositories(&self, platform: Option<Platform>) -> Result<Vec<Repository>>;
    async fn update_repository(&self, repo: &Repository) -> Result<()>;
    async fn delete_repository(&self, id: Uuid) -> Result<()>;

    // Job operations
    async fn create_job(&self, job: &ProofJobRecord) -> Result<()>;
    async fn get_job(&self, id: JobId) -> Result<Option<ProofJobRecord>>;
    async fn update_job(&self, job: &ProofJobRecord) -> Result<()>;
    async fn list_jobs_for_repo(&self, repo_id: Uuid, limit: usize) -> Result<Vec<ProofJobRecord>>;
    async fn list_pending_jobs(&self, limit: usize) -> Result<Vec<ProofJobRecord>>;

    // Result operations
    async fn save_result(&self, result: &ProofResultRecord) -> Result<()>;
    async fn get_result_for_job(&self, job_id: JobId) -> Result<Option<ProofResultRecord>>;

    /// Coverage for the (repo_id, commit_sha) tuple — counts of total
    /// and successful proof_jobs at that commit. Used by Regulator mode
    /// to decide whether the threshold is met before blocking a merge.
    async fn commit_coverage(&self, repo_id: Uuid, commit_sha: &str) -> Result<CommitCoverage>;

    // Tactic-outcome operations (double-loop feedback, Package 7b)
    async fn record_tactic_outcome(&self, outcome: &TacticOutcomeRecord) -> Result<()>;
    async fn list_tactic_outcomes_by_fingerprint(
        &self,
        prover: ProverKind,
        goal_fingerprint: &str,
        limit: usize,
    ) -> Result<Vec<TacticOutcomeRecord>>;
    async fn list_tactic_outcomes_by_tactic(
        &self,
        prover: ProverKind,
        tactic: &str,
        limit: usize,
    ) -> Result<Vec<TacticOutcomeRecord>>;

    // Utility
    async fn health_check(&self) -> Result<bool>;
}
