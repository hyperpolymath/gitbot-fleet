// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! SQLite store implementation

use async_trait::async_trait;
use sqlx::{sqlite::SqlitePoolOptions, Pool, Sqlite};
use uuid::Uuid;

use super::{models::*, Store};
use crate::adapters::Platform;
use crate::dispatcher::ProverKind;
use crate::error::{Error, Result};
use crate::scheduler::JobId;

/// SQLite-backed store
pub struct SqliteStore {
    pool: Pool<Sqlite>,
}

impl SqliteStore {
    /// Create a new SQLite store
    pub async fn new(database_url: &str) -> Result<Self> {
        let pool = SqlitePoolOptions::new()
            .max_connections(5)
            .connect(database_url)
            .await?;

        let store = Self { pool };
        store.run_migrations().await?;

        Ok(store)
    }

    /// Gracefully close the underlying connection pool.
    ///
    /// Called during shutdown to drain outstanding queries and release
    /// SQLite file handles cleanly. After `close()`, all `Store`
    /// operations on this instance return an error. Safe to call once;
    /// subsequent calls are no-ops because `Pool::close()` is idempotent.
    ///
    /// See `crate::shutdown::ShutdownCoordinator` for the orchestrated
    /// call site (DB close runs after the scheduler drains, so no
    /// in-flight job tries to write after the pool is closed).
    pub async fn close(&self) {
        self.pool.close().await;
    }

    /// Borrow the underlying pool. Exposed for shutdown coordination
    /// (e.g. wiring `pool.close()` into the shutdown sequence without
    /// taking ownership of the `SqliteStore`).
    pub fn pool(&self) -> &Pool<Sqlite> {
        &self.pool
    }

    /// Run database migrations
    async fn run_migrations(&self) -> Result<()> {
        sqlx::query(
            r#"
            CREATE TABLE IF NOT EXISTS repositories (
                id TEXT PRIMARY KEY,
                platform TEXT NOT NULL,
                owner TEXT NOT NULL,
                name TEXT NOT NULL,
                webhook_secret TEXT,
                enabled_provers TEXT NOT NULL,
                check_on_push INTEGER NOT NULL DEFAULT 1,
                check_on_pr INTEGER NOT NULL DEFAULT 1,
                auto_comment INTEGER NOT NULL DEFAULT 1,
                enabled INTEGER NOT NULL DEFAULT 1,
                last_checked_commit TEXT,
                created_at TEXT NOT NULL,
                updated_at TEXT NOT NULL,
                mode TEXT NOT NULL DEFAULT 'verifier',
                regulator_coverage_threshold INTEGER NOT NULL DEFAULT 100,
                UNIQUE(platform, owner, name)
            )
            "#,
        )
        .execute(&self.pool)
        .await?;

        sqlx::query(
            r#"
            CREATE TABLE IF NOT EXISTS proof_jobs (
                id TEXT PRIMARY KEY,
                repo_id TEXT NOT NULL REFERENCES repositories(id),
                commit_sha TEXT NOT NULL,
                prover TEXT NOT NULL,
                file_paths TEXT NOT NULL,
                status TEXT NOT NULL,
                priority INTEGER NOT NULL DEFAULT 1,
                queued_at TEXT NOT NULL,
                started_at TEXT,
                completed_at TEXT,
                error_message TEXT,
                pr_number INTEGER,
                delivery_id TEXT
            )
            "#,
        )
        .execute(&self.pool)
        .await?;

        // Idempotent migrations for older databases. SQLite returns
        // "duplicate column" when the column already exists; we treat that
        // as success.
        for ddl in [
            "ALTER TABLE proof_jobs ADD COLUMN pr_number INTEGER",
            "ALTER TABLE proof_jobs ADD COLUMN delivery_id TEXT",
            "ALTER TABLE repositories ADD COLUMN mode TEXT NOT NULL DEFAULT 'verifier'",
            "ALTER TABLE repositories ADD COLUMN regulator_coverage_threshold INTEGER NOT NULL DEFAULT 100",
        ] {
            match sqlx::query(ddl).execute(&self.pool).await {
                Ok(_) => {}
                Err(sqlx::Error::Database(e))
                    if e.message().contains("duplicate column") =>
                {
                    // Column already exists — fresh DB created above already
                    // had it, or an earlier migration added it. Either is fine.
                }
                Err(e) => return Err(e.into()),
            }
        }

        sqlx::query(
            r#"
            CREATE TABLE IF NOT EXISTS proof_results (
                id TEXT PRIMARY KEY,
                job_id TEXT NOT NULL REFERENCES proof_jobs(id),
                success INTEGER NOT NULL,
                message TEXT NOT NULL,
                prover_output TEXT NOT NULL,
                duration_ms INTEGER NOT NULL,
                verified_files TEXT NOT NULL,
                failed_files TEXT NOT NULL,
                created_at TEXT NOT NULL
            )
            "#,
        )
        .execute(&self.pool)
        .await?;

        sqlx::query(
            r#"
            CREATE INDEX IF NOT EXISTS idx_jobs_repo_id ON proof_jobs(repo_id);
            "#,
        )
        .execute(&self.pool)
        .await?;

        sqlx::query(
            r#"
            CREATE INDEX IF NOT EXISTS idx_jobs_status ON proof_jobs(status);
            "#,
        )
        .execute(&self.pool)
        .await?;

        // Tactic-outcome table — feedback-loop substrate (Package 7b).
        // `job_id` is nullable so outcomes recorded via MCP / CLI (no webhook
        // job) can still be ingested.
        sqlx::query(
            r#"
            CREATE TABLE IF NOT EXISTS tactic_outcomes (
                id TEXT PRIMARY KEY,
                job_id TEXT REFERENCES proof_jobs(id),
                prover TEXT NOT NULL,
                goal_fingerprint TEXT NOT NULL,
                tactic TEXT NOT NULL,
                succeeded INTEGER NOT NULL,
                duration_ms INTEGER NOT NULL,
                created_at TEXT NOT NULL
            )
            "#,
        )
        .execute(&self.pool)
        .await?;

        sqlx::query(
            r#"
            CREATE INDEX IF NOT EXISTS idx_tactic_outcomes_prover_fp
                ON tactic_outcomes(prover, goal_fingerprint);
            "#,
        )
        .execute(&self.pool)
        .await?;

        sqlx::query(
            r#"
            CREATE INDEX IF NOT EXISTS idx_tactic_outcomes_prover_tactic
                ON tactic_outcomes(prover, tactic);
            "#,
        )
        .execute(&self.pool)
        .await?;

        Ok(())
    }
}

#[async_trait]
impl Store for SqliteStore {
    async fn create_repository(&self, repo: &Repository) -> Result<()> {
        let enabled_provers = serde_json::to_string(&repo.enabled_provers)?;

        sqlx::query(
            r#"
            INSERT INTO repositories (
                id, platform, owner, name, webhook_secret, enabled_provers,
                check_on_push, check_on_pr, auto_comment, enabled,
                last_checked_commit, created_at, updated_at, mode,
                regulator_coverage_threshold
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            "#,
        )
        .bind(repo.id.to_string())
        .bind(format!("{:?}", repo.platform))
        .bind(&repo.owner)
        .bind(&repo.name)
        .bind(&repo.webhook_secret)
        .bind(&enabled_provers)
        .bind(repo.check_on_push)
        .bind(repo.check_on_pr)
        .bind(repo.auto_comment)
        .bind(repo.enabled)
        .bind(&repo.last_checked_commit)
        .bind(repo.created_at.to_rfc3339())
        .bind(repo.updated_at.to_rfc3339())
        .bind(
            serde_json::to_value(repo.mode)?
                .as_str()
                .unwrap_or("verifier"),
        )
        .bind(repo.regulator_coverage_threshold as i64)
        .execute(&self.pool)
        .await?;

        Ok(())
    }

    async fn get_repository(&self, id: Uuid) -> Result<Option<Repository>> {
        let row: Option<RepoRow> = sqlx::query_as("SELECT * FROM repositories WHERE id = ?")
            .bind(id.to_string())
            .fetch_optional(&self.pool)
            .await?;

        row.map(|r| r.try_into()).transpose()
    }

    async fn get_repository_by_name(
        &self,
        platform: Platform,
        owner: &str,
        name: &str,
    ) -> Result<Option<Repository>> {
        let row: Option<RepoRow> = sqlx::query_as(
            "SELECT * FROM repositories WHERE platform = ? AND owner = ? AND name = ?",
        )
        .bind(format!("{:?}", platform))
        .bind(owner)
        .bind(name)
        .fetch_optional(&self.pool)
        .await?;

        row.map(|r| r.try_into()).transpose()
    }

    async fn list_repositories(&self, platform: Option<Platform>) -> Result<Vec<Repository>> {
        let rows: Vec<RepoRow> = match platform {
            Some(p) => {
                sqlx::query_as(
                    "SELECT * FROM repositories WHERE platform = ? ORDER BY created_at DESC",
                )
                .bind(format!("{:?}", p))
                .fetch_all(&self.pool)
                .await?
            }
            None => {
                sqlx::query_as("SELECT * FROM repositories ORDER BY created_at DESC")
                    .fetch_all(&self.pool)
                    .await?
            }
        };

        rows.into_iter().map(|r| r.try_into()).collect()
    }

    async fn update_repository(&self, repo: &Repository) -> Result<()> {
        let enabled_provers = serde_json::to_string(&repo.enabled_provers)?;

        sqlx::query(
            r#"
            UPDATE repositories SET
                webhook_secret = ?,
                enabled_provers = ?,
                check_on_push = ?,
                check_on_pr = ?,
                auto_comment = ?,
                enabled = ?,
                last_checked_commit = ?,
                updated_at = ?
            WHERE id = ?
            "#,
        )
        .bind(&repo.webhook_secret)
        .bind(&enabled_provers)
        .bind(repo.check_on_push)
        .bind(repo.check_on_pr)
        .bind(repo.auto_comment)
        .bind(repo.enabled)
        .bind(&repo.last_checked_commit)
        .bind(repo.updated_at.to_rfc3339())
        .bind(repo.id.to_string())
        .execute(&self.pool)
        .await?;

        Ok(())
    }

    async fn delete_repository(&self, id: Uuid) -> Result<()> {
        sqlx::query("DELETE FROM repositories WHERE id = ?")
            .bind(id.to_string())
            .execute(&self.pool)
            .await?;
        Ok(())
    }

    async fn create_job(&self, job: &ProofJobRecord) -> Result<()> {
        let file_paths = serde_json::to_string(&job.file_paths)?;

        sqlx::query(
            r#"
            INSERT INTO proof_jobs (
                id, repo_id, commit_sha, prover, file_paths,
                status, priority, queued_at, started_at, completed_at, error_message,
                pr_number, delivery_id
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            "#,
        )
        .bind(job.id.to_string())
        .bind(job.repo_id.to_string())
        .bind(&job.commit_sha)
        .bind(format!("{:?}", job.prover))
        .bind(&file_paths)
        .bind(format!("{:?}", job.status))
        .bind(job.priority as i32)
        .bind(job.queued_at.to_rfc3339())
        .bind(job.started_at.map(|t| t.to_rfc3339()))
        .bind(job.completed_at.map(|t| t.to_rfc3339()))
        .bind(&job.error_message)
        .bind(job.pr_number.map(|n| n as i64))
        .bind(&job.delivery_id)
        .execute(&self.pool)
        .await?;

        Ok(())
    }

    async fn get_job(&self, id: JobId) -> Result<Option<ProofJobRecord>> {
        let row: Option<JobRow> = sqlx::query_as("SELECT * FROM proof_jobs WHERE id = ?")
            .bind(id.0.to_string())
            .fetch_optional(&self.pool)
            .await?;

        row.map(|r| r.try_into()).transpose()
    }

    async fn update_job(&self, job: &ProofJobRecord) -> Result<()> {
        sqlx::query(
            r#"
            UPDATE proof_jobs SET
                status = ?,
                started_at = ?,
                completed_at = ?,
                error_message = ?
            WHERE id = ?
            "#,
        )
        .bind(format!("{:?}", job.status))
        .bind(job.started_at.map(|t| t.to_rfc3339()))
        .bind(job.completed_at.map(|t| t.to_rfc3339()))
        .bind(&job.error_message)
        .bind(job.id.to_string())
        .execute(&self.pool)
        .await?;

        Ok(())
    }

    async fn list_jobs_for_repo(&self, repo_id: Uuid, limit: usize) -> Result<Vec<ProofJobRecord>> {
        let rows: Vec<JobRow> = sqlx::query_as(
            "SELECT * FROM proof_jobs WHERE repo_id = ? ORDER BY queued_at DESC LIMIT ?",
        )
        .bind(repo_id.to_string())
        .bind(limit as i64)
        .fetch_all(&self.pool)
        .await?;

        rows.into_iter().map(|r| r.try_into()).collect()
    }

    async fn list_pending_jobs(&self, limit: usize) -> Result<Vec<ProofJobRecord>> {
        let rows: Vec<JobRow> = sqlx::query_as(
            "SELECT * FROM proof_jobs WHERE status = 'Queued' ORDER BY priority DESC, queued_at ASC LIMIT ?",
        )
        .bind(limit as i64)
        .fetch_all(&self.pool)
        .await?;

        rows.into_iter().map(|r| r.try_into()).collect()
    }

    async fn save_result(&self, result: &ProofResultRecord) -> Result<()> {
        let verified_files = serde_json::to_string(&result.verified_files)?;
        let failed_files = serde_json::to_string(&result.failed_files)?;

        sqlx::query(
            r#"
            INSERT INTO proof_results (
                id, job_id, success, message, prover_output,
                duration_ms, verified_files, failed_files, created_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            "#,
        )
        .bind(result.id.to_string())
        .bind(result.job_id.to_string())
        .bind(result.success)
        .bind(&result.message)
        .bind(&result.prover_output)
        .bind(result.duration_ms)
        .bind(&verified_files)
        .bind(&failed_files)
        .bind(result.created_at.to_rfc3339())
        .execute(&self.pool)
        .await?;

        Ok(())
    }

    async fn get_result_for_job(&self, job_id: JobId) -> Result<Option<ProofResultRecord>> {
        let row: Option<ResultRow> = sqlx::query_as("SELECT * FROM proof_results WHERE job_id = ?")
            .bind(job_id.0.to_string())
            .fetch_optional(&self.pool)
            .await?;

        row.map(|r| r.try_into()).transpose()
    }

    async fn commit_coverage(
        &self,
        repo_id: Uuid,
        commit_sha: &str,
    ) -> Result<super::CommitCoverage> {
        // LEFT JOIN proof_results onto proof_jobs so jobs without results
        // (still running) are counted as not-proven. Coverage is a
        // running tally — Regulator runs at each finalize_job, so the
        // last job to finalize for a commit gets the final say.
        let row: (i64, i64) = sqlx::query_as(
            r#"
            SELECT
                COUNT(*) as total,
                COALESCE(SUM(CASE WHEN pr.success = 1 THEN 1 ELSE 0 END), 0) as proven
            FROM proof_jobs pj
            LEFT JOIN proof_results pr ON pr.job_id = pj.id
            WHERE pj.repo_id = ? AND pj.commit_sha = ?
            "#,
        )
        .bind(repo_id.to_string())
        .bind(commit_sha)
        .fetch_one(&self.pool)
        .await?;

        Ok(super::CommitCoverage {
            total: row.0.max(0) as u64,
            proven: row.1.max(0) as u64,
        })
    }

    async fn record_tactic_outcome(&self, outcome: &TacticOutcomeRecord) -> Result<()> {
        sqlx::query(
            r#"
            INSERT INTO tactic_outcomes (
                id, job_id, prover, goal_fingerprint, tactic,
                succeeded, duration_ms, created_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            "#,
        )
        .bind(outcome.id.to_string())
        .bind(outcome.job_id.map(|id| id.to_string()))
        .bind(format!("{:?}", outcome.prover))
        .bind(&outcome.goal_fingerprint)
        .bind(&outcome.tactic)
        .bind(outcome.succeeded)
        .bind(outcome.duration_ms)
        .bind(outcome.created_at.to_rfc3339())
        .execute(&self.pool)
        .await?;

        Ok(())
    }

    async fn list_tactic_outcomes_by_fingerprint(
        &self,
        prover: ProverKind,
        goal_fingerprint: &str,
        limit: usize,
    ) -> Result<Vec<TacticOutcomeRecord>> {
        let rows: Vec<OutcomeRow> = sqlx::query_as(
            "SELECT * FROM tactic_outcomes \
             WHERE prover = ? AND goal_fingerprint = ? \
             ORDER BY created_at DESC LIMIT ?",
        )
        .bind(format!("{:?}", prover))
        .bind(goal_fingerprint)
        .bind(limit as i64)
        .fetch_all(&self.pool)
        .await?;

        rows.into_iter().map(|r| r.try_into()).collect()
    }

    async fn list_tactic_outcomes_by_tactic(
        &self,
        prover: ProverKind,
        tactic: &str,
        limit: usize,
    ) -> Result<Vec<TacticOutcomeRecord>> {
        let rows: Vec<OutcomeRow> = sqlx::query_as(
            "SELECT * FROM tactic_outcomes \
             WHERE prover = ? AND tactic = ? \
             ORDER BY created_at DESC LIMIT ?",
        )
        .bind(format!("{:?}", prover))
        .bind(tactic)
        .bind(limit as i64)
        .fetch_all(&self.pool)
        .await?;

        rows.into_iter().map(|r| r.try_into()).collect()
    }

    async fn health_check(&self) -> Result<bool> {
        let result: (i32,) = sqlx::query_as("SELECT 1").fetch_one(&self.pool).await?;
        Ok(result.0 == 1)
    }
}

// =============================================================================
// Row types for sqlx
// =============================================================================

#[derive(sqlx::FromRow)]
struct RepoRow {
    id: String,
    platform: String,
    owner: String,
    name: String,
    webhook_secret: Option<String>,
    enabled_provers: String,
    check_on_push: bool,
    check_on_pr: bool,
    auto_comment: bool,
    enabled: bool,
    last_checked_commit: Option<String>,
    created_at: String,
    updated_at: String,
    #[sqlx(default)]
    mode: Option<String>,
    #[sqlx(default)]
    regulator_coverage_threshold: Option<i64>,
}

impl TryFrom<RepoRow> for Repository {
    type Error = Error;

    fn try_from(row: RepoRow) -> Result<Self> {
        let platform = match row.platform.as_str() {
            "GitHub" => Platform::GitHub,
            "GitLab" => Platform::GitLab,
            "Bitbucket" => Platform::Bitbucket,
            "Codeberg" => Platform::Codeberg,
            _ => {
                return Err(Error::Internal(format!(
                    "Unknown platform: {}",
                    row.platform
                )))
            }
        };

        let enabled_provers: Vec<ProverKind> = serde_json::from_str(&row.enabled_provers)?;

        // Parse mode via serde, falling back to Verifier if the column was
        // null (older DB pre-migration) or contained an unrecognised value.
        let mode = row
            .mode
            .as_deref()
            .and_then(|s| {
                serde_json::from_value::<crate::modes::BotMode>(serde_json::Value::String(
                    s.to_string(),
                ))
                .ok()
            })
            .unwrap_or_default();

        Ok(Repository {
            id: Uuid::parse_str(&row.id).map_err(|e| Error::Internal(e.to_string()))?,
            platform,
            owner: row.owner,
            name: row.name,
            webhook_secret: row.webhook_secret,
            enabled_provers,
            check_on_push: row.check_on_push,
            check_on_pr: row.check_on_pr,
            auto_comment: row.auto_comment,
            enabled: row.enabled,
            last_checked_commit: row.last_checked_commit,
            created_at: chrono::DateTime::parse_from_rfc3339(&row.created_at)
                .map_err(|e| Error::Internal(e.to_string()))?
                .with_timezone(&chrono::Utc),
            updated_at: chrono::DateTime::parse_from_rfc3339(&row.updated_at)
                .map_err(|e| Error::Internal(e.to_string()))?
                .with_timezone(&chrono::Utc),
            mode,
            regulator_coverage_threshold: row
                .regulator_coverage_threshold
                .map(|v| v.clamp(0, 100) as u8)
                .unwrap_or(100),
        })
    }
}

#[derive(sqlx::FromRow)]
struct JobRow {
    id: String,
    repo_id: String,
    commit_sha: String,
    prover: String,
    file_paths: String,
    status: String,
    priority: i32,
    queued_at: String,
    started_at: Option<String>,
    completed_at: Option<String>,
    error_message: Option<String>,
    #[sqlx(default)]
    pr_number: Option<i64>,
    #[sqlx(default)]
    delivery_id: Option<String>,
}

impl TryFrom<JobRow> for ProofJobRecord {
    type Error = Error;

    fn try_from(row: JobRow) -> Result<Self> {
        use crate::scheduler::{JobPriority, JobStatus};

        let prover = parse_prover(&row.prover)?;
        let status = match row.status.as_str() {
            "Queued" => JobStatus::Queued,
            "Running" => JobStatus::Running,
            "Completed" => JobStatus::Completed,
            "Failed" => JobStatus::Failed,
            "Cancelled" => JobStatus::Cancelled,
            _ => return Err(Error::Internal(format!("Unknown status: {}", row.status))),
        };
        let priority = match row.priority {
            0 => JobPriority::Low,
            1 => JobPriority::Normal,
            2 => JobPriority::High,
            3 => JobPriority::Critical,
            _ => JobPriority::Normal,
        };
        let file_paths: Vec<String> = serde_json::from_str(&row.file_paths)?;

        Ok(ProofJobRecord {
            id: Uuid::parse_str(&row.id).map_err(|e| Error::Internal(e.to_string()))?,
            repo_id: Uuid::parse_str(&row.repo_id).map_err(|e| Error::Internal(e.to_string()))?,
            commit_sha: row.commit_sha,
            prover,
            file_paths,
            status,
            priority,
            queued_at: chrono::DateTime::parse_from_rfc3339(&row.queued_at)
                .map_err(|e| Error::Internal(e.to_string()))?
                .with_timezone(&chrono::Utc),
            started_at: row
                .started_at
                .map(|s| {
                    chrono::DateTime::parse_from_rfc3339(&s).map(|t| t.with_timezone(&chrono::Utc))
                })
                .transpose()
                .map_err(|e| Error::Internal(e.to_string()))?,
            completed_at: row
                .completed_at
                .map(|s| {
                    chrono::DateTime::parse_from_rfc3339(&s).map(|t| t.with_timezone(&chrono::Utc))
                })
                .transpose()
                .map_err(|e| Error::Internal(e.to_string()))?,
            error_message: row.error_message,
            pr_number: row.pr_number.map(|n| n as u64),
            delivery_id: row.delivery_id,
        })
    }
}

#[derive(sqlx::FromRow)]
struct ResultRow {
    id: String,
    job_id: String,
    success: bool,
    message: String,
    prover_output: String,
    duration_ms: i64,
    verified_files: String,
    failed_files: String,
    created_at: String,
}

impl TryFrom<ResultRow> for ProofResultRecord {
    type Error = Error;

    fn try_from(row: ResultRow) -> Result<Self> {
        let verified_files: Vec<String> = serde_json::from_str(&row.verified_files)?;
        let failed_files: Vec<String> = serde_json::from_str(&row.failed_files)?;

        Ok(ProofResultRecord {
            id: Uuid::parse_str(&row.id).map_err(|e| Error::Internal(e.to_string()))?,
            job_id: Uuid::parse_str(&row.job_id).map_err(|e| Error::Internal(e.to_string()))?,
            success: row.success,
            message: row.message,
            prover_output: row.prover_output,
            duration_ms: row.duration_ms,
            verified_files,
            failed_files,
            created_at: chrono::DateTime::parse_from_rfc3339(&row.created_at)
                .map_err(|e| Error::Internal(e.to_string()))?
                .with_timezone(&chrono::Utc),
        })
    }
}

#[derive(sqlx::FromRow)]
struct OutcomeRow {
    id: String,
    job_id: Option<String>,
    prover: String,
    goal_fingerprint: String,
    tactic: String,
    succeeded: bool,
    duration_ms: i64,
    created_at: String,
}

impl TryFrom<OutcomeRow> for TacticOutcomeRecord {
    type Error = Error;

    fn try_from(row: OutcomeRow) -> Result<Self> {
        let prover = parse_prover(&row.prover)?;
        let job_id = row
            .job_id
            .map(|s| Uuid::parse_str(&s).map_err(|e| Error::Internal(e.to_string())))
            .transpose()?;

        Ok(TacticOutcomeRecord {
            id: Uuid::parse_str(&row.id).map_err(|e| Error::Internal(e.to_string()))?,
            job_id,
            prover,
            goal_fingerprint: row.goal_fingerprint,
            tactic: row.tactic,
            succeeded: row.succeeded,
            duration_ms: row.duration_ms,
            created_at: chrono::DateTime::parse_from_rfc3339(&row.created_at)
                .map_err(|e| Error::Internal(e.to_string()))?
                .with_timezone(&chrono::Utc),
        })
    }
}

fn parse_prover(s: &str) -> Result<ProverKind> {
    match s {
        "Agda" => Ok(ProverKind::new("agda")),
        "Coq" => Ok(ProverKind::new("coq")),
        "Lean" => Ok(ProverKind::new("lean")),
        "Isabelle" => Ok(ProverKind::new("isabelle")),
        "Z3" => Ok(ProverKind::new("z3")),
        "Cvc5" => Ok(ProverKind::new("cvc5")),
        "Metamath" => Ok(ProverKind::new("metamath")),
        "HolLight" => Ok(ProverKind::new("hol-light")),
        "Mizar" => Ok(ProverKind::new("mizar")),
        "Pvs" => Ok(ProverKind::new("pvs")),
        "Acl2" => Ok(ProverKind::new("acl2")),
        "Hol4" => Ok(ProverKind::new("hol4")),
        _ => Ok(ProverKind::new(s)), // Support all 113 provers dynamically
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::store::models::{goal_fingerprint, TacticOutcomeRecord};

    async fn fresh_store() -> (SqliteStore, std::path::PathBuf) {
        let path =
            std::env::temp_dir().join(format!("echidnabot-store-test-{}.db", Uuid::new_v4()));
        let url = format!("sqlite://{}?mode=rwc", path.display());
        let store = SqliteStore::new(&url).await.expect("open store");
        (store, path)
    }

    #[tokio::test]
    async fn tactic_outcome_insert_then_lookup_by_fingerprint() {
        let (store, path) = fresh_store().await;
        let fp = goal_fingerprint("forall x : Nat, x = x");

        let first = TacticOutcomeRecord::new(
            None,
            ProverKind::new("coq"),
            fp.clone(),
            "reflexivity".into(),
            true,
            12,
        );
        let second = TacticOutcomeRecord::new(
            None,
            ProverKind::new("coq"),
            fp.clone(),
            "auto".into(),
            false,
            30,
        );
        store.record_tactic_outcome(&first).await.unwrap();
        store.record_tactic_outcome(&second).await.unwrap();

        let found = store
            .list_tactic_outcomes_by_fingerprint(ProverKind::new("coq"), &fp, 10)
            .await
            .unwrap();
        assert_eq!(found.len(), 2);
        // DESC by created_at — second row (auto) is newer
        assert_eq!(found[0].tactic, "auto");
        assert!(!found[0].succeeded);
        assert_eq!(found[1].tactic, "reflexivity");
        assert!(found[1].succeeded);

        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn tactic_outcome_filters_by_prover() {
        let (store, path) = fresh_store().await;
        let fp = goal_fingerprint("P /\\ Q -> P");

        store
            .record_tactic_outcome(&TacticOutcomeRecord::new(
                None,
                ProverKind::new("coq"),
                fp.clone(),
                "split".into(),
                true,
                5,
            ))
            .await
            .unwrap();
        store
            .record_tactic_outcome(&TacticOutcomeRecord::new(
                None,
                ProverKind::new("lean"),
                fp.clone(),
                "exact".into(),
                true,
                5,
            ))
            .await
            .unwrap();

        let coq_hits = store
            .list_tactic_outcomes_by_fingerprint(ProverKind::new("coq"), &fp, 10)
            .await
            .unwrap();
        let lean_hits = store
            .list_tactic_outcomes_by_fingerprint(ProverKind::new("lean"), &fp, 10)
            .await
            .unwrap();
        assert_eq!(coq_hits.len(), 1);
        assert_eq!(coq_hits[0].tactic, "split");
        assert_eq!(lean_hits.len(), 1);
        assert_eq!(lean_hits[0].tactic, "exact");

        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn tactic_outcome_lookup_by_tactic() {
        let (store, path) = fresh_store().await;
        let fp1 = goal_fingerprint("goal 1");
        let fp2 = goal_fingerprint("goal 2");

        store
            .record_tactic_outcome(&TacticOutcomeRecord::new(
                None,
                ProverKind::new("coq"),
                fp1,
                "intros".into(),
                true,
                3,
            ))
            .await
            .unwrap();
        store
            .record_tactic_outcome(&TacticOutcomeRecord::new(
                None,
                ProverKind::new("coq"),
                fp2,
                "intros".into(),
                false,
                99,
            ))
            .await
            .unwrap();

        let hits = store
            .list_tactic_outcomes_by_tactic(ProverKind::new("coq"), "intros", 10)
            .await
            .unwrap();
        assert_eq!(hits.len(), 2);
        // Both rows share the tactic name but have different fingerprints
        let successes = hits.iter().filter(|h| h.succeeded).count();
        assert_eq!(successes, 1);

        let _ = std::fs::remove_file(&path);
    }
}
