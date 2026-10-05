// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! GraphQL schema and resolvers

use async_graphql::{Context, EmptySubscription, Object, Schema, SimpleObject, ID};
use chrono::{DateTime, Utc};
use std::sync::Arc;
use uuid::Uuid;

use crate::dispatcher::echidna_client::ProverStatus as CoreProverStatus;
use crate::dispatcher::{
    EchidnaClient, ProverKind as CoreProverKind, TacticSuggestion as CoreSuggestion,
};
use crate::scheduler::{JobPriority, JobScheduler};
use crate::store::models::{
    goal_fingerprint, ProofJobRecord, Repository as StoreRepository, TacticOutcomeRecord,
};
use crate::store::Store;

/// GraphQL schema type
pub type EchidnabotSchema = Schema<QueryRoot, MutationRoot, EmptySubscription>;

/// Create the GraphQL schema
pub fn create_schema(state: GraphQLState) -> EchidnabotSchema {
    Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(state)
        .finish()
}

/// Shared GraphQL state
#[derive(Clone)]
pub struct GraphQLState {
    pub store: Arc<dyn Store>,
    pub scheduler: Arc<JobScheduler>,
    pub echidna: Arc<EchidnaClient>,
}

// =============================================================================
// Types
// =============================================================================

/// Platform enum
#[derive(async_graphql::Enum, Copy, Clone, Eq, PartialEq, Debug)]
pub enum Platform {
    GitHub,
    GitLab,
    Bitbucket,
    Codeberg,
}

/// Prover kind enum
#[derive(async_graphql::Enum, Copy, Clone, Eq, PartialEq, Debug)]
pub enum ProverKind {
    Agda,
    Coq,
    Lean,
    Isabelle,
    Z3,
    Cvc5,
    Metamath,
    HolLight,
    Mizar,
    Pvs,
    Acl2,
    Hol4,
}

/// Job status enum
#[derive(async_graphql::Enum, Copy, Clone, Eq, PartialEq, Debug)]
pub enum JobStatus {
    Queued,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// Proof verification status
#[derive(async_graphql::Enum, Copy, Clone, Eq, PartialEq, Debug)]
pub enum ProofStatus {
    Verified,
    Failed,
    Timeout,
    Error,
    Unknown,
}

/// Prover availability status
#[derive(async_graphql::Enum, Copy, Clone, Eq, PartialEq, Debug)]
pub enum ProverStatus {
    Available,
    Degraded,
    Unavailable,
    Unknown,
}

/// Repository information
#[derive(SimpleObject, Clone)]
pub struct Repository {
    pub id: ID,
    pub platform: Platform,
    pub owner: String,
    pub name: String,
    pub enabled_provers: Vec<ProverKind>,
    pub last_checked_commit: Option<String>,
}

/// Proof job information
#[derive(SimpleObject, Clone)]
pub struct ProofJob {
    pub id: ID,
    pub repo_id: ID,
    pub commit_sha: String,
    pub prover: ProverKind,
    pub status: JobStatus,
    pub queued_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
}

/// Proof verification result
#[derive(SimpleObject, Clone)]
pub struct ProofResult {
    pub status: ProofStatus,
    pub message: String,
    pub prover_output: String,
    pub duration_ms: i32,
}

/// Prover information
#[derive(SimpleObject, Clone)]
pub struct ProverInfo {
    pub kind: ProverKind,
    pub name: String,
    pub tier: i32,
    pub file_extensions: Vec<String>,
    pub status: ProverStatus,
}

/// Tactic suggestion from ML
#[derive(SimpleObject, Clone)]
pub struct TacticSuggestion {
    pub tactic: String,
    pub confidence: f64,
    pub explanation: Option<String>,
}

/// A recorded tactic outcome (double-loop feedback store)
#[derive(SimpleObject, Clone)]
pub struct TacticOutcome {
    pub id: ID,
    pub prover: ProverKind,
    pub goal_fingerprint: String,
    pub tactic: String,
    pub succeeded: bool,
    pub duration_ms: i64,
    pub recorded_at: DateTime<Utc>,
}

impl From<TacticOutcomeRecord> for TacticOutcome {
    fn from(r: TacticOutcomeRecord) -> Self {
        Self {
            id: ID::from(r.id.to_string()),
            prover: map_prover_kind(r.prover),
            goal_fingerprint: r.goal_fingerprint,
            tactic: r.tactic,
            succeeded: r.succeeded,
            duration_ms: r.duration_ms,
            recorded_at: r.created_at,
        }
    }
}

/// Input for recording a tactic outcome from an external agent
#[derive(async_graphql::InputObject)]
pub struct RecordTacticOutcomeInput {
    /// Which prover was used
    pub prover: ProverKind,
    /// The proof goal / context (used to compute a fingerprint)
    pub goal_state: String,
    /// The tactic that was attempted
    pub tactic: String,
    /// Whether the tactic succeeded
    pub succeeded: bool,
    /// How long the attempt took (milliseconds)
    pub duration_ms: i64,
    /// Job ID this outcome belongs to (optional)
    pub job_id: Option<ID>,
}

// =============================================================================
// Query Root
// =============================================================================

pub struct QueryRoot;

#[Object]
impl QueryRoot {
    /// Get a repository by platform, owner, and name
    async fn repository(
        &self,
        ctx: &Context<'_>,
        platform: Platform,
        owner: String,
        name: String,
    ) -> Option<Repository> {
        let state = ctx.data::<GraphQLState>().ok()?;
        let repo = state
            .store
            .get_repository_by_name(map_platform(platform), &owner, &name)
            .await
            .ok()??;
        Some(repo.into())
    }

    /// List all registered repositories
    async fn repositories(&self, ctx: &Context<'_>, platform: Option<Platform>) -> Vec<Repository> {
        let state = match ctx.data::<GraphQLState>() {
            Ok(state) => state,
            Err(_) => return vec![],
        };
        let repos = state
            .store
            .list_repositories(platform.map(map_platform))
            .await
            .unwrap_or_default();
        repos.into_iter().map(Repository::from).collect()
    }

    /// Get a proof job by ID
    async fn job(&self, ctx: &Context<'_>, id: ID) -> Option<ProofJob> {
        let state = ctx.data::<GraphQLState>().ok()?;
        let job_id = Uuid::parse_str(id.as_str()).ok()?;
        let job = state
            .store
            .get_job(crate::scheduler::JobId(job_id))
            .await
            .ok()??;
        Some(job.into())
    }

    /// List jobs for a repository
    async fn jobs_for_repo(
        &self,
        ctx: &Context<'_>,
        repo_id: ID,
        limit: Option<i32>,
    ) -> Vec<ProofJob> {
        let state = match ctx.data::<GraphQLState>() {
            Ok(state) => state,
            Err(_) => return vec![],
        };
        let repo_uuid = match Uuid::parse_str(repo_id.as_str()) {
            Ok(id) => id,
            Err(_) => return vec![],
        };
        let limit = limit.unwrap_or(50).max(1) as usize;
        let jobs = state
            .store
            .list_jobs_for_repo(repo_uuid, limit)
            .await
            .unwrap_or_default();
        jobs.into_iter().map(ProofJob::from).collect()
    }

    /// List available provers
    async fn available_provers(&self, ctx: &Context<'_>) -> Vec<ProverInfo> {
        let state = match ctx.data::<GraphQLState>() {
            Ok(state) => state,
            Err(_) => return vec![],
        };
        let mut provers = Vec::new();
        for kind in CoreProverKind::all() {
            let status = match state.echidna.prover_status(&kind).await {
                Ok(status) => map_prover_status(status),
                Err(_) => ProverStatus::Unknown,
            };
            provers.push(ProverInfo {
                kind: map_prover_kind(kind.clone()),
                name: kind.display_name().to_string(),
                tier: kind.tier() as i32,
                file_extensions: kind
                    .file_extensions()
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                status,
            });
        }
        provers
    }

    /// Check prover status
    async fn prover_status(&self, ctx: &Context<'_>, prover: ProverKind) -> ProverStatus {
        let state = match ctx.data::<GraphQLState>() {
            Ok(state) => state,
            Err(_) => return ProverStatus::Unknown,
        };
        let kind = map_prover_kind_to_core(prover);
        match state.echidna.prover_status(&kind).await {
            Ok(status) => map_prover_status(status),
            Err(_) => ProverStatus::Unknown,
        }
    }

    /// List recorded tactic outcomes for a (prover, goal_fingerprint) pair.
    ///
    /// Used by LLM agents to inspect historical success rates before suggesting
    /// a tactic, and by operators to audit the feedback store.
    async fn tactic_outcomes(
        &self,
        ctx: &Context<'_>,
        prover: ProverKind,
        goal_fingerprint: String,
        limit: Option<i32>,
    ) -> Vec<TacticOutcome> {
        let state = match ctx.data::<GraphQLState>() {
            Ok(s) => s,
            Err(_) => return vec![],
        };
        let limit = limit.unwrap_or(50).max(1) as usize;
        state
            .store
            .list_tactic_outcomes_by_fingerprint(
                map_prover_kind_to_core(prover),
                &goal_fingerprint,
                limit,
            )
            .await
            .unwrap_or_default()
            .into_iter()
            .map(TacticOutcome::from)
            .collect()
    }

    /// List recorded tactic outcomes for a specific (prover, tactic) pair
    /// across all goal fingerprints. Useful for global win-rate queries.
    async fn tactic_outcomes_by_tactic(
        &self,
        ctx: &Context<'_>,
        prover: ProverKind,
        tactic: String,
        limit: Option<i32>,
    ) -> Vec<TacticOutcome> {
        let state = match ctx.data::<GraphQLState>() {
            Ok(s) => s,
            Err(_) => return vec![],
        };
        let limit = limit.unwrap_or(200).max(1) as usize;
        state
            .store
            .list_tactic_outcomes_by_tactic(map_prover_kind_to_core(prover), &tactic, limit)
            .await
            .unwrap_or_default()
            .into_iter()
            .map(TacticOutcome::from)
            .collect()
    }
}

// =============================================================================
// Mutation Root
// =============================================================================

pub struct MutationRoot;

/// Input for registering a repository
#[derive(async_graphql::InputObject)]
pub struct RegisterRepoInput {
    pub platform: Platform,
    pub owner: String,
    pub name: String,
    pub webhook_secret: Option<String>,
    pub enabled_provers: Option<Vec<ProverKind>>,
}

/// Input for repository settings
#[derive(async_graphql::InputObject)]
pub struct RepoSettingsInput {
    pub webhook_secret: Option<String>,
    pub enabled_provers: Option<Vec<ProverKind>>,
    pub check_on_push: Option<bool>,
    pub check_on_pr: Option<bool>,
    pub auto_comment: Option<bool>,
}

#[Object]
impl MutationRoot {
    /// Register a repository for monitoring
    async fn register_repository(
        &self,
        ctx: &Context<'_>,
        input: RegisterRepoInput,
    ) -> async_graphql::Result<Repository> {
        let state = ctx.data::<GraphQLState>()?;

        let mut repo = StoreRepository::new(map_platform(input.platform), input.owner, input.name);
        repo.webhook_secret = input.webhook_secret;
        if let Some(provers) = input.enabled_provers {
            repo.enabled_provers = provers.into_iter().map(map_prover_kind_to_core).collect();
        }

        state
            .store
            .create_repository(&repo)
            .await
            .map_err(|e| async_graphql::Error::new(e.to_string()))?;
        Ok(repo.into())
    }

    /// Manually trigger a proof check
    async fn trigger_check(
        &self,
        ctx: &Context<'_>,
        repo_id: ID,
        commit_sha: Option<String>,
        provers: Option<Vec<ProverKind>>,
    ) -> async_graphql::Result<ProofJob> {
        let state = ctx.data::<GraphQLState>()?;
        let repo_uuid = Uuid::parse_str(repo_id.as_str())
            .map_err(|_| async_graphql::Error::new("Invalid repository ID"))?;
        let repo = state
            .store
            .get_repository(repo_uuid)
            .await
            .map_err(|e| async_graphql::Error::new(e.to_string()))?
            .ok_or_else(|| async_graphql::Error::new("Repository not found"))?;
        let commit = commit_sha.unwrap_or_else(|| "HEAD".to_string());

        let provers = provers.unwrap_or_else(|| {
            repo.enabled_provers
                .iter()
                .cloned()
                .map(map_prover_kind)
                .collect()
        });

        let mut first_job = None;
        for prover in provers {
            let job = crate::scheduler::ProofJob::new(
                repo.id,
                commit.clone(),
                map_prover_kind_to_core(prover),
                Vec::new(),
            )
            .with_priority(JobPriority::Critical);
            let record = ProofJobRecord::from(job.clone());
            state
                .store
                .create_job(&record)
                .await
                .map_err(|e| async_graphql::Error::new(e.to_string()))?;
            let _ = state
                .scheduler
                .enqueue(job.clone())
                .await
                .map_err(|e| async_graphql::Error::new(e.to_string()))?;
            if first_job.is_none() {
                first_job = Some(job);
            }
        }

        let job = first_job.ok_or_else(|| async_graphql::Error::new("No jobs enqueued"))?;
        Ok(ProofJobRecord::from(job).into())
    }

    /// Request ML-powered tactic suggestions
    async fn request_suggestions(
        &self,
        ctx: &Context<'_>,
        prover: ProverKind,
        context: String,
        goal_state: String,
    ) -> async_graphql::Result<Vec<TacticSuggestion>> {
        let state = ctx.data::<GraphQLState>()?;
        let suggestions = state
            .echidna
            .suggest_tactics(&map_prover_kind_to_core(prover), &context, &goal_state)
            .await
            .map_err(|e| async_graphql::Error::new(e.to_string()))?;
        Ok(suggestions.into_iter().map(map_suggestion).collect())
    }

    /// Update repository settings
    async fn update_repo_settings(
        &self,
        ctx: &Context<'_>,
        repo_id: ID,
        settings: RepoSettingsInput,
    ) -> async_graphql::Result<Repository> {
        let state = ctx.data::<GraphQLState>()?;
        let repo_uuid = Uuid::parse_str(repo_id.as_str())
            .map_err(|_| async_graphql::Error::new("Invalid repository ID"))?;
        let mut repo = state
            .store
            .get_repository(repo_uuid)
            .await
            .map_err(|e| async_graphql::Error::new(e.to_string()))?
            .ok_or_else(|| async_graphql::Error::new("Repository not found"))?;

        if let Some(secret) = settings.webhook_secret {
            repo.webhook_secret = Some(secret);
        }
        if let Some(provers) = settings.enabled_provers {
            repo.enabled_provers = provers.into_iter().map(map_prover_kind_to_core).collect();
        }
        if let Some(check) = settings.check_on_push {
            repo.check_on_push = check;
        }
        if let Some(check) = settings.check_on_pr {
            repo.check_on_pr = check;
        }
        if let Some(auto_comment) = settings.auto_comment {
            repo.auto_comment = auto_comment;
        }
        repo.updated_at = Utc::now();

        state
            .store
            .update_repository(&repo)
            .await
            .map_err(|e| async_graphql::Error::new(e.to_string()))?;
        Ok(repo.into())
    }

    /// Enable or disable repository monitoring
    async fn set_repo_enabled(
        &self,
        ctx: &Context<'_>,
        repo_id: ID,
        enabled: bool,
    ) -> async_graphql::Result<Repository> {
        let state = ctx.data::<GraphQLState>()?;
        let repo_uuid = Uuid::parse_str(repo_id.as_str())
            .map_err(|_| async_graphql::Error::new("Invalid repository ID"))?;
        let mut repo = state
            .store
            .get_repository(repo_uuid)
            .await
            .map_err(|e| async_graphql::Error::new(e.to_string()))?
            .ok_or_else(|| async_graphql::Error::new("Repository not found"))?;
        repo.enabled = enabled;
        repo.updated_at = Utc::now();
        state
            .store
            .update_repository(&repo)
            .await
            .map_err(|e| async_graphql::Error::new(e.to_string()))?;
        Ok(repo.into())
    }

    /// Record the outcome of a tactic attempt (double-loop feedback).
    ///
    /// Called by LLM agents (via MCP or direct GraphQL) when they observe a
    /// tactic being applied in a proof session. The outcome feeds the local
    /// Reranker store so future suggestions for the same goal fingerprint are
    /// ranked by historical success rate.
    async fn record_tactic_outcome(
        &self,
        ctx: &Context<'_>,
        input: RecordTacticOutcomeInput,
    ) -> async_graphql::Result<TacticOutcome> {
        let state = ctx.data::<GraphQLState>()?;
        let prover = map_prover_kind_to_core(input.prover);
        let fingerprint = goal_fingerprint(&input.goal_state);

        let job_uuid = input
            .job_id
            .as_deref()
            .and_then(|id| Uuid::parse_str(id).ok());

        let record = TacticOutcomeRecord::new(
            job_uuid,
            prover,
            fingerprint,
            input.tactic,
            input.succeeded,
            input.duration_ms,
        );
        state
            .store
            .record_tactic_outcome(&record)
            .await
            .map_err(|e| async_graphql::Error::new(e.to_string()))?;
        Ok(TacticOutcome::from(record))
    }
}

impl From<StoreRepository> for Repository {
    fn from(repo: StoreRepository) -> Self {
        Self {
            id: ID::from(repo.id.to_string()),
            platform: map_platform_to_graphql(repo.platform),
            owner: repo.owner,
            name: repo.name,
            enabled_provers: repo
                .enabled_provers
                .into_iter()
                .map(map_prover_kind)
                .collect(),
            last_checked_commit: repo.last_checked_commit,
        }
    }
}

impl From<ProofJobRecord> for ProofJob {
    fn from(job: ProofJobRecord) -> Self {
        Self {
            id: ID::from(job.id.to_string()),
            repo_id: ID::from(job.repo_id.to_string()),
            commit_sha: job.commit_sha,
            prover: map_prover_kind(job.prover),
            status: map_job_status(job.status),
            queued_at: job.queued_at,
            started_at: job.started_at,
            completed_at: job.completed_at,
        }
    }
}

fn map_platform(platform: Platform) -> crate::adapters::Platform {
    match platform {
        Platform::GitHub => crate::adapters::Platform::GitHub,
        Platform::GitLab => crate::adapters::Platform::GitLab,
        Platform::Bitbucket => crate::adapters::Platform::Bitbucket,
        Platform::Codeberg => crate::adapters::Platform::Codeberg,
    }
}

fn map_platform_to_graphql(platform: crate::adapters::Platform) -> Platform {
    match platform {
        crate::adapters::Platform::GitHub => Platform::GitHub,
        crate::adapters::Platform::GitLab => Platform::GitLab,
        crate::adapters::Platform::Bitbucket => Platform::Bitbucket,
        crate::adapters::Platform::Codeberg => Platform::Codeberg,
    }
}

fn map_prover_kind(kind: CoreProverKind) -> ProverKind {
    match kind.as_str() {
        "agda" => ProverKind::Agda,
        "coq" => ProverKind::Coq,
        "lean" => ProverKind::Lean,
        "isabelle" => ProverKind::Isabelle,
        "z3" => ProverKind::Z3,
        "cvc5" => ProverKind::Cvc5,
        "metamath" => ProverKind::Metamath,
        "hol-light" => ProverKind::HolLight,
        "mizar" => ProverKind::Mizar,
        "pvs" => ProverKind::Pvs,
        "acl2" => ProverKind::Acl2,
        "hol4" => ProverKind::Hol4,
        _ => ProverKind::Coq, // fallback for non-classic slugs
    }
}

fn map_prover_kind_to_core(kind: ProverKind) -> CoreProverKind {
    CoreProverKind::new(match kind {
        ProverKind::Agda => "agda",
        ProverKind::Coq => "coq",
        ProverKind::Lean => "lean",
        ProverKind::Isabelle => "isabelle",
        ProverKind::Z3 => "z3",
        ProverKind::Cvc5 => "cvc5",
        ProverKind::Metamath => "metamath",
        ProverKind::HolLight => "hol-light",
        ProverKind::Mizar => "mizar",
        ProverKind::Pvs => "pvs",
        ProverKind::Acl2 => "acl2",
        ProverKind::Hol4 => "hol4",
    })
}

fn map_job_status(status: crate::scheduler::JobStatus) -> JobStatus {
    match status {
        crate::scheduler::JobStatus::Queued => JobStatus::Queued,
        crate::scheduler::JobStatus::Running => JobStatus::Running,
        crate::scheduler::JobStatus::Completed => JobStatus::Completed,
        crate::scheduler::JobStatus::Failed => JobStatus::Failed,
        crate::scheduler::JobStatus::Cancelled => JobStatus::Cancelled,
    }
}

fn map_prover_status(status: CoreProverStatus) -> ProverStatus {
    match status {
        CoreProverStatus::Available => ProverStatus::Available,
        CoreProverStatus::Degraded => ProverStatus::Degraded,
        CoreProverStatus::Unavailable => ProverStatus::Unavailable,
        CoreProverStatus::Unknown => ProverStatus::Unknown,
    }
}

fn map_suggestion(suggestion: CoreSuggestion) -> TacticSuggestion {
    TacticSuggestion {
        tactic: suggestion.tactic,
        confidence: suggestion.confidence,
        explanation: suggestion.explanation,
    }
}
