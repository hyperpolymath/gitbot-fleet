<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell -->

# Architecture

The full source of truth lives in
[`docs/ARCHITECTURE.md`](https://github.com/hyperpolymath/gitbot-fleet/blob/main/docs/ARCHITECTURE.md)
in the main repo. This wiki page is a wiki-friendly mirror.

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
| `fleet-coordinator.sh` | repo root | bash | Orchestrates bot dispatching |
| `dispatch-runner.sh` | `scripts/` | bash | Reads JSONL manifests, executes per-finding fixes |
| `process-review-findings.sh` | `scripts/` | bash | Opens GitHub issues for review-tier findings |
| `fix-*.sh` (~50) | `scripts/` | bash | One-shot fixers for eliminate-tier patterns |
| `shared-context/` | `shared-context/` | Rust | Inter-bot communication crate |
| `robot-repo-automaton/` | `robot-repo-automaton/` | Rust | scan/fix/PR CLI |
| `bots/*` | `bots/` | mostly Rust, AffineScript in sustainabot | Per-bot specialised logic |

## Critical invariants

1. The canonical A2ML files live directly under `.machine_readable/`
   per `hyperpolymath/standards`'s `A2ML-REPO-TEMPLATE`.
2. Shell scripts validate untrusted input before use.
3. Secrets come from env vars with `${VAR:-}` defaults — never hardcoded.
4. Fix scripts must be idempotent.
5. Confidence thresholds gate every automated action.
