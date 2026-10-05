-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- Initial schema for echidnabot on PostgreSQL.
-- Mirrors the SQLite DDL emitted at runtime by `SqliteStore::run_migrations`
-- (see src/store/sqlite.rs). When a PostgresStore backend is added, sqlx
-- migrate will apply this file at startup; until then it documents the
-- canonical relational shape for the Compose-deployed Postgres service
-- (issue #60) and is safe to apply via `psql -f`.
--
-- Idempotent: uses IF NOT EXISTS everywhere so re-applying is a no-op.

CREATE TABLE IF NOT EXISTS repositories (
    id                              TEXT PRIMARY KEY,
    platform                        TEXT NOT NULL,
    owner                           TEXT NOT NULL,
    name                            TEXT NOT NULL,
    webhook_secret                  TEXT,
    enabled_provers                 TEXT NOT NULL,
    check_on_push                   INTEGER NOT NULL DEFAULT 1,
    check_on_pr                     INTEGER NOT NULL DEFAULT 1,
    auto_comment                    INTEGER NOT NULL DEFAULT 1,
    enabled                         INTEGER NOT NULL DEFAULT 1,
    last_checked_commit             TEXT,
    created_at                      TEXT NOT NULL,
    updated_at                      TEXT NOT NULL,
    mode                            TEXT NOT NULL DEFAULT 'verifier',
    regulator_coverage_threshold    INTEGER NOT NULL DEFAULT 100,
    UNIQUE (platform, owner, name)
);

CREATE TABLE IF NOT EXISTS proof_jobs (
    id              TEXT PRIMARY KEY,
    repo_id         TEXT NOT NULL REFERENCES repositories(id),
    commit_sha      TEXT NOT NULL,
    prover          TEXT NOT NULL,
    file_paths      TEXT NOT NULL,
    status          TEXT NOT NULL,
    priority        INTEGER NOT NULL DEFAULT 1,
    queued_at       TEXT NOT NULL,
    started_at      TEXT,
    completed_at    TEXT,
    error_message   TEXT,
    pr_number       INTEGER,
    delivery_id     TEXT
);

CREATE INDEX IF NOT EXISTS idx_jobs_repo_id ON proof_jobs (repo_id);
CREATE INDEX IF NOT EXISTS idx_jobs_status  ON proof_jobs (status);

CREATE TABLE IF NOT EXISTS proof_results (
    id              TEXT PRIMARY KEY,
    job_id          TEXT NOT NULL REFERENCES proof_jobs(id),
    success         INTEGER NOT NULL,
    message         TEXT NOT NULL,
    prover_output   TEXT NOT NULL,
    duration_ms     INTEGER NOT NULL,
    verified_files  TEXT NOT NULL,
    failed_files    TEXT NOT NULL,
    created_at      TEXT NOT NULL
);

-- tactic_outcomes — double-loop feedback substrate (Package 7b).
-- job_id is nullable so MCP/CLI-recorded outcomes (no webhook job) ingest.
CREATE TABLE IF NOT EXISTS tactic_outcomes (
    id                  TEXT PRIMARY KEY,
    job_id              TEXT REFERENCES proof_jobs(id),
    prover              TEXT NOT NULL,
    goal_fingerprint    TEXT NOT NULL,
    tactic              TEXT NOT NULL,
    succeeded           INTEGER NOT NULL,
    duration_ms         INTEGER NOT NULL,
    created_at          TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_tactic_outcomes_prover_fp
    ON tactic_outcomes (prover, goal_fingerprint);

CREATE INDEX IF NOT EXISTS idx_tactic_outcomes_prover_tactic
    ON tactic_outcomes (prover, tactic);
