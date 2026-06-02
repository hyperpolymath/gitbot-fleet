<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell -->

# Gitbot Fleet — Wiki

Bot orchestration hub for automated repository quality enforcement across
the Hyperpolymath ecosystem (~500 repos).

> **The repo is the source of truth.** This wiki is a publication target —
> see [`docs/wiki-source/`](https://github.com/hyperpolymath/gitbot-fleet/tree/main/docs/wiki-source) in the
> main repo for the canonical Markdown. Edits made here directly will be
> overwritten on the next sync.

## What is Gitbot Fleet?

A coordinator that takes Hypatia scanner findings, dispatches them to the
right specialist bot via a safety-triangle decision (Eliminate /
Substitute / Control), and either applies a fix automatically or files a
PR / issue for human review.

## Bot Roster (11 bots)

| Bot | Purpose |
|---|---|
| **rhodibot** | Git ops, Rhodium Standard Repository (RSR) enforcement |
| **echidnabot** | Formal verification, multi-prover dispatch, security fuzzing |
| **sustainabot** | Eco/econ + technical-debt scoring; carbon-intensity gates |
| **glambot** | Presentation: visual polish, WCAG accessibility, SEO |
| **seambot** | Integration seam health, cross-component contracts |
| **finishingbot** | Release readiness: placeholder removal, license + claim verification |
| **accessibilitybot** | Accessibility-rule enforcement (WCAG, ARIA, contrast) |
| **cipherbot** | Crypto + secret hygiene; policy-driven cipher checks |
| **gsbot** | General-services: backup/load/export fixtures and operational scripts |
| **panicbot** | Panic + crash-capture surfaces |
| **the-hotchocolabot** | Comfort / human-facing channels; non-blocking advisory output |

Component-readiness grades (CRG) for each bot live in
[`READINESS.md`](https://github.com/hyperpolymath/gitbot-fleet/blob/main/READINESS.md).

## Quick links

- [Architecture](Architecture) — pipeline + components
- [Safety Triangle](Safety-Triangle) — confidence thresholds and dispatch policy
- [Build & Run](Build-and-Run)
- [Bot Operations](Bot-Operations) — long-form ops guide
- [Contributing](https://github.com/hyperpolymath/gitbot-fleet/blob/main/CONTRIBUTING.md)
- [Roadmap](https://github.com/hyperpolymath/gitbot-fleet/blob/main/ROADMAP.adoc)
- [Component Readiness](https://github.com/hyperpolymath/gitbot-fleet/blob/main/READINESS.md)

## Position in the ecosystem

- Satellite of `git-dispatcher` (central Git-automation coordination).
- Execution fleet for OPSM (Operational Process State Management) batch
  operations.
- Consumes findings from `hyperpolymath/hypatia`.
- Governance gate inherited from `hyperpolymath/standards`.

## License

PMPL-1.0-or-later — see [LICENSE](https://github.com/hyperpolymath/gitbot-fleet/blob/main/LICENSE).
