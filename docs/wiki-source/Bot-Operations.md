<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell -->

# Bot Operations

The long-form operational guide lives in
[`docs/BOT-OPERATIONS.md`](https://github.com/hyperpolymath/gitbot-fleet/blob/main/docs/BOT-OPERATIONS.md)
in the main repo. The headings below are a quick orientation.

## Lifecycle

1. **Hypatia** scans the supervised inventory and emits `findings.jsonl`.
2. **fleet-coordinator** builds a per-bot dispatch manifest from the
   findings, gated on the safety triangle.
3. **dispatch-runner** reads the manifest and either runs a `fix-*.sh`
   in-place (eliminate tier) or queues the finding for review.
4. **process-review-findings** opens GitHub issues for `report` tier
   and PRs for `review` tier.
5. **robot-repo-automaton** is the Rust execution path for fleet-scale
   batch operations (scan → fix → commit → PR per repo).

## Per-bot ownership

See [`bots/*/README.adoc`](https://github.com/hyperpolymath/gitbot-fleet/tree/main/bots)
for each bot's specific contract. Each bot ships its own:

- `Cargo.toml` + `src/` (Rust executable)
- `README.adoc` (purpose, IO contract, current grade)
- `ROADMAP.adoc` (forward plan)
- `tests/` (per-bot integration tests)

## Common operational tasks

| Task | Command / pointer |
|---|---|
| Force a one-shot scan of a single repo | `bash fleet-coordinator.sh --repo /path` |
| Re-run a failed dispatch | `bash scripts/dispatch-runner.sh --manifest <path>` |
| Triage review-tier findings | `bash scripts/process-review-findings.sh` |
| Enroll a new repo into supervision | `just enroll-repos` |
| Refresh CRG badges | `just crg-badge` |
| Investigate a finding manually | inspect `findings.jsonl` then run the bot CLI directly |

## See also

- [`docs/PERFORMANCE.md`](https://github.com/hyperpolymath/gitbot-fleet/blob/main/docs/PERFORMANCE.md) — benchmark + profiling
- [`docs/BRANCH-PROTECTION-SETUP.md`](https://github.com/hyperpolymath/gitbot-fleet/blob/main/docs/BRANCH-PROTECTION-SETUP.md) — wiring branch-protection rules
- [`READINESS.md`](https://github.com/hyperpolymath/gitbot-fleet/blob/main/READINESS.md) — current CRG grades
