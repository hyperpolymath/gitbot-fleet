# SPDX-License-Identifier: CC-BY-SA-4.0

# CLAUDE.md — Gitbot Fleet

## Project Overview

Gitbot Fleet is the bot orchestration hub for automated repository
quality enforcement across ~500 hyperpolymath repositories. It
coordinates 11 bots through a shared-context system with safety-triangle
-aware routing.

The authoritative architecture document is [`docs/ARCHITECTURE.md`](../docs/ARCHITECTURE.md).
The repo-root `README.adoc` is the entry point for humans.

## Architecture (summary — see `docs/ARCHITECTURE.md` for the full pipeline)

```
hypatia (scanner)  →  findings JSONL  →  fleet-coordinator.sh  →  dispatch-runner.sh
                                               │
                                    ┌──────────┼──────────┐
                                    ▼          ▼          ▼
                               rhodibot   echidnabot  sustainabot   ... + 8 more
                               (git ops)  (quality)   (eco/econ)
                                    │
                                    ▼
                        robot-repo-automaton
                        (scan → fix → commit → PR)
```

## Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| `fleet-coordinator.sh` | repo root | Orchestrates bot dispatching |
| `dispatch-runner.sh` | `scripts/` | Reads JSONL manifests, executes fixes |
| `process-review-findings.sh` | `scripts/` | Opens GitHub issues for review-tier findings |
| `fix-*.sh` | `scripts/` | ~50 fix scripts for eliminate-tier patterns |
| `shared-context/` | `shared-context/` | Rust crate for inter-bot communication |
| `robot-repo-automaton/` | `robot-repo-automaton/` | Rust CLI: scan, fix, PR creation |
| `fleet-report-back.sh`, `fleet-report-back-build.sh` | `scripts/` | Build the enrolled adapters, run them on a PR head, and write the report-back envelopes the Hypatia verdict grid reads (`docs/REPORT-BACK.adoc`) |

## Build & Run

```bash
# Shell scripts (no build needed)
bash fleet-coordinator.sh --help
just maintenance-hard-pass /absolute/path/to/repo
just scan-supervised

# Robot-repo-automaton (Rust)
cd robot-repo-automaton
OPENSSL_NO_VENDOR=1 cargo build --release
./target/release/robot-repo-automaton scan /path/to/repo
./target/release/robot-repo-automaton fix /path/to/repo --create-pr
```

## Safety Triangle

```
Eliminate (auto_execute >= 0.95)  →  Direct fix, no review
Substitute (review >= 0.85)       →  Proven module replacement, needs review
Control    (report < 0.85)        →  Human review required
```

## Code Style

- SPDX headers: `MPL-2.0`
- Author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
- Shell scripts: bash with `set -euo pipefail`
- Rust: standard formatting, `anyhow::Result` for error handling
- JSON construction in shell: always use `jq` (never string interpolation)

## Critical Invariants

1. **A2ML is retired; repo facts are deeds.** No `.a2ml` file should exist,
   and no bot, script or doc here may create, extend or recommend one. The
   facts the seven canonical A2ML files held (`STATE`, `META`, `ECOSYSTEM`,
   `AGENTIC`, `NEUROSYM`, `PLAYBOOK`, `ANCHOR`), and the manifest, the RSR
   profile and the bot directives, are clauses of `<repo>_chora.deed` at the
   repo root (one per repo), or of a descriptile deed such as
   `.machine_readable/descriptiles/META.deed`, `STATE.deed` or `AGENTIC.deed`
   as standards' A7 migration moves them out. Conformance is
   `1-formats/deed/tools/deed_lint.js` in `metadatastician/standards`; bots
   here only *read* deeds and never hand-convert A2ML (D313).
   - **The only exception** is a deed-first **read fallback**: a shared reader
     may still read a legacy `.a2ml` file after a deed source, and only while
     A7's migration plan (M5–M7) requires it until its callers migrate.
     Rhodibot's `canon` command does this (`src/canon/overlay.rs`), reporting a
     legacy file as "migrate", never as "missing", and never naming an `.a2ml`
     as the form to create.
   - Earlier direct-under-`.machine_readable/`, `6scm/`, and `6a2/` layouts
     are retired and must not be restored.
2. All shell scripts validate untrusted input before use.
3. No hardcoded secrets — use env vars with `${VAR:-}` defaults.
4. Fix scripts must be idempotent (safe to run multiple times).
5. Confidence thresholds gate all automated actions.
6. **`bots/<name>/` directories are THIN ADAPTERS, not homes for standalone
   products.** Never vendor an entire external project (its own Cargo
   workspace, analyzers, containers, deployment, docs) into a bot slot, and
   never let a bot crate add a `path` dependency that escapes the repo. If a
   capability deserves its own project, build it in its own repository and
   depend on it externally. See [`bots/README.adoc`](../bots/README.adoc).
   - **Disambiguation:** `sustainabot` (this fleet's eco/econ slot, kept as
     `BotId::Sustainabot`) is **not** `OikosBot` and **not** `oikos`. A misfiled
     full copy of OikosBot once lived in `bots/sustainabot/`; it was extracted to
     `hyperpolymath/oikosbot` and the slot reset to a placeholder. `oikos` is a
     separate DSL (`hyperpolymath/oikos-economics-accounting-dsl`).
7. **Mark intentional mass-deletions.** The Repo Integrity Guard
   (`.github/workflows/repo-integrity-guard.yml`) fails any change that deletes
   more than `MAX_DELETIONS` (50) tracked files vs. base — it exists because
   `main` was once silently gutted from 1777 files to 2. For a *deliberate*
   large removal, include the literal marker `[mass-delete-ok]` in a commit
   message in the range, **or** in the PR title/body. Never weaken the guard or
   trim its critical-file list just to get a change through.

## Repo health

- Open PRs / issues, dated session reports, and historical status
  snapshots: see [`docs/archive/`](../docs/archive/).
- Component readiness (CRG grades + evidence): [`READINESS.md`](../READINESS.md).
- Roadmap (all 11 bots): [`ROADMAP.adoc`](../ROADMAP.adoc).
