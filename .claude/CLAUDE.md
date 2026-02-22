# SPDX-License-Identifier: PMPL-1.0-or-later

# CLAUDE.md - Gitbot Fleet

## Project Overview

Gitbot Fleet is the bot orchestration hub for automated repository quality enforcement across 500+ hyperpolymath repositories. It coordinates multiple bots through a shared-context system with safety-triangle-aware routing.

## Architecture

```
hypatia (scanner)  →  findings JSONL  →  fleet-coordinator.sh  →  dispatch-runner.sh
                                               ↓
                                    ┌──────────┼──────────┐
                                    ▼          ▼          ▼
                               rhodibot   echidnabot  sustainabot
                               (git ops)  (quality)   (deps)
                                    ↓
                        robot-repo-automaton
                        (scan → fix → commit → PR)
```

## Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| fleet-coordinator.sh | root | Orchestrates bot dispatching |
| dispatch-runner.sh | scripts/ | Reads JSONL manifests, executes fixes |
| process-review-findings.sh | scripts/ | Creates GitHub issues for review-tier |
| fix-*.sh | scripts/ | 7 fix scripts for eliminate-tier patterns |
| shared-context/ | shared-context/ | Rust crate for inter-bot communication |
| robot-repo-automaton/ | robot-repo-automaton/ | Rust CLI: scan, fix, PR creation |

## Build & Run

```bash
# Shell scripts (no build needed)
bash fleet-coordinator.sh --help

# Robot-repo-automaton (Rust)
cd robot-repo-automaton
OPENSSL_NO_VENDOR=1 cargo build --release
./target/release/robot-repo-automaton scan /path/to/repo
./target/release/robot-repo-automaton fix /path/to/repo --create-pr
```

## Safety Triangle

```
Eliminate (auto_execute >= 0.95)  →  Direct fix, no review
Substitute (review >= 0.85)      →  proven module replacement, needs review
Control (report < 0.85)          →  Human review required
```

## Code Style

- SPDX headers: `PMPL-1.0-or-later`
- Author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
- Shell scripts: bash with `set -euo pipefail`
- Rust: standard formatting, `anyhow::Result` for error handling
- JSON construction in shell: always use `jq` (never string interpolation)

## Critical Invariants

1. SCM files ONLY in `.machine_readable/6scm/` (never root)
2. All shell scripts must validate untrusted input before use
3. No hardcoded secrets — use env vars with `${VAR:-}` defaults
4. Fix scripts must be idempotent (safe to run multiple times)
5. Confidence thresholds gate all automated actions
