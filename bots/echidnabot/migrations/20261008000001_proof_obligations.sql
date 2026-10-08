-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- proof_obligations — obligations received via the `submitProofObligation`
-- GraphQL mutation (hypatia FleetDispatcher / LearningScheduler).
-- Mirrors the SQLite DDL emitted at runtime by `SqliteStore::run_migrations`.
-- `id` is a UUIDv8 content id; `repo_id` is nullable because the sender
-- names a repo slug that need not be registered with echidnabot.
CREATE TABLE IF NOT EXISTS proof_obligations (
    id                  TEXT PRIMARY KEY,
    repo_slug           TEXT NOT NULL,
    repo_id             TEXT REFERENCES repositories(id),
    claim               TEXT NOT NULL,
    context             TEXT NOT NULL,
    prover              TEXT,
    inline_requested    INTEGER NOT NULL,
    status              TEXT NOT NULL,
    created_at          TEXT NOT NULL
);
