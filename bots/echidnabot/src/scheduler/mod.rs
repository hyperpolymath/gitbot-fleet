// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Job scheduler for proof verification tasks

pub mod job_queue;
pub mod limiter; // Concurrent job limits to prevent overwhelming prover backends
pub mod retry; // Exponential backoff for transient failures

pub use job_queue::JobScheduler;
pub use limiter::{JobLimiter, LimiterConfig};
pub use retry::{
    retry, retry_with_backoff, CircuitBreaker, CircuitState, RetryConfig, RetryPolicy,
};

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::dispatcher::ProverKind;
use crate::trust::{axiom_tracker::AxiomReport, confidence::ConfidenceReport};

/// Unique job identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct JobId(pub Uuid);

impl JobId {
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl Default for JobId {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for JobId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Proof verification job
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofJob {
    pub id: JobId,
    pub repo_id: Uuid,
    pub commit_sha: String,
    pub prover: ProverKind,
    pub file_paths: Vec<String>,
    pub status: JobStatus,
    pub priority: JobPriority,
    pub queued_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub result: Option<JobResult>,
    /// PR number that triggered this job (None for direct push events).
    /// Plumbed through so the result-reporter (Phase 3) can comment on
    /// the originating PR rather than the commit page.
    #[serde(default)]
    pub pr_number: Option<u64>,
    /// Webhook delivery ID for traceability — `X-GitHub-Delivery` header
    /// for GitHub, equivalent for GitLab/Bitbucket. Lets us correlate a
    /// proof outcome back to the exact webhook that triggered it.
    #[serde(default)]
    pub delivery_id: Option<String>,
}

impl ProofJob {
    pub fn new(
        repo_id: Uuid,
        commit_sha: String,
        prover: ProverKind,
        file_paths: Vec<String>,
    ) -> Self {
        Self {
            id: JobId::new(),
            repo_id,
            commit_sha,
            prover,
            file_paths,
            status: JobStatus::Queued,
            priority: JobPriority::Normal,
            queued_at: Utc::now(),
            started_at: None,
            completed_at: None,
            result: None,
            pr_number: None,
            delivery_id: None,
        }
    }

    /// Create a high-priority job (e.g., for PR checks)
    pub fn with_priority(mut self, priority: JobPriority) -> Self {
        self.priority = priority;
        self
    }

    /// Attach PR + delivery context (for jobs originating from webhooks).
    pub fn with_context(mut self, pr_number: Option<u64>, delivery_id: Option<String>) -> Self {
        self.pr_number = pr_number;
        self.delivery_id = delivery_id;
        self
    }

    /// Mark as started
    pub fn start(&mut self) {
        self.status = JobStatus::Running;
        self.started_at = Some(Utc::now());
    }

    /// Mark as completed
    pub fn complete(&mut self, result: JobResult) {
        self.status = match result.success {
            true => JobStatus::Completed,
            false => JobStatus::Failed,
        };
        self.completed_at = Some(Utc::now());
        self.result = Some(result);
    }

    /// Mark as cancelled
    pub fn cancel(&mut self) {
        self.status = JobStatus::Cancelled;
        self.completed_at = Some(Utc::now());
    }

    /// Get duration in milliseconds (if completed)
    pub fn duration_ms(&self) -> Option<u64> {
        match (self.started_at, self.completed_at) {
            (Some(start), Some(end)) => Some((end - start).num_milliseconds().max(0) as u64),
            _ => None,
        }
    }
}

/// Job execution status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum JobStatus {
    Queued,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// Job priority for queue ordering
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum JobPriority {
    Low = 0,
    Normal = 1,
    High = 2,     // PR checks
    Critical = 3, // Manual triggers
}

/// Result of a completed job, including trust-bridge data propagated from ECHIDNA.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobResult {
    pub success: bool,
    pub message: String,
    pub prover_output: String,
    pub duration_ms: u64,
    pub verified_files: Vec<String>,
    pub failed_files: Vec<String>,
    /// Confidence level assessed over the aggregated prover output.
    #[serde(default)]
    pub confidence: Option<ConfidenceReport>,
    /// Axiom usage flags found in the aggregated prover output.
    #[serde(default)]
    pub axioms: Option<AxiomReport>,
}
