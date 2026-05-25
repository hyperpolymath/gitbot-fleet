<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell -->

# Gitbot Fleet — Architecture

This is the **single source of truth** for the gitbot-fleet architecture.
The repo-root README links here; the `TOPOLOGY.md` dashboard at the repo
root tracks live completion state.

## Pipeline

```
hypatia (scanner)
   │
   ▼  findings.jsonl
fleet-coordinator.sh        ← orchestrator, root of the repo
   │
   ▼  dispatch manifest
dispatch-runner.sh          ← scripts/dispatch-runner.sh
   │
   ├──────────────┬──────────────┬─────────────────────┐
   ▼              ▼              ▼                     ▼
rhodibot     echidnabot     sustainabot          ... 8 more bots
(git ops)    (verify)       (eco/econ)
   │              │              │
   └──────────────┴──────────────┴────► shared-context layer
                                        (shared-context/, Rust crate)
                                                │
                                                ▼
                                       robot-repo-automaton
                                       (scan → fix → commit → PR)
```

## Components

| Component | Path | Language | Purpose |
|---|---|---|---|
| `fleet-coordinator.sh` | repo root | bash | Orchestrates bot dispatching; reads Hypatia findings, builds manifests, dispatches per-bot |
| `dispatch-runner.sh` | `scripts/` | bash | Reads JSONL manifests, executes per-finding fixes via `fix-*.sh` |
| `process-review-findings.sh` | `scripts/` | bash | Opens GitHub issues for review-tier findings (Substitute / Control) |
| `fix-*.sh` (~50 files) | `scripts/` | bash | One-shot fixers for eliminate-tier patterns (e.g. SPDX header, license file) |
| `shared-context/` | `shared-context/` | Rust | Crate for inter-bot communication (RPC + state-sharing) |
| `robot-repo-automaton/` | `robot-repo-automaton/` | Rust | CLI: scan, fix, PR creation |
| `bots/*` | `bots/` | mostly Rust, AffineScript in sustainabot | Per-bot specialised logic |

## Safety triangle

```
Eliminate (auto_execute >= 0.95)  →  Direct fix, no review
Substitute (review >= 0.85)       →  Proven-module replacement, needs review
Control (report < 0.85)           →  Human review required
```

The thresholds gate every automated action: a fix script is only run when
the upstream Hypatia finding's confidence meets the eliminate threshold;
substitute-tier findings open PRs with a `needs-review` label; control-tier
findings file issues.

## Critical invariants

1. **Machine-readable files** live in `.machine_readable/` (the canonical
   A2ML files — `STATE`, `META`, `ECOSYSTEM`, `AGENTIC`, `NEUROSYM`,
   `PLAYBOOK`, `ANCHOR` — sit directly there). See the
   `A2ML-REPO-TEMPLATE` in `hyperpolymath/standards`.
2. **Shell scripts** validate untrusted input before use.
3. **Secrets** come from env vars with `${VAR:-}` defaults — never
   hardcoded.
4. **Fix scripts must be idempotent** (safe to run multiple times).
5. **Confidence thresholds** gate every automated action.

## Repository position

Gitbot Fleet is a satellite of `git-dispatcher` (central Git-automation
coordination) and the execution fleet for OPSM (Operational Process State
Management) batch operations.
