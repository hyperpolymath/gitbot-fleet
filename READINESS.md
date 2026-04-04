<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# gitbot-fleet Component Readiness Assessment

**Standard:** [Component Readiness Grades (CRG) v2.2](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)
**Current Grade:** B
**Assessed:** 2026-04-04
**Assessor:** Jonathan D.A. Jewell + Claude Sonnet 4.6

---

## Summary

| Component          | Grade | Release Stage | Evidence Summary                                                          |
|--------------------|-------|---------------|---------------------------------------------------------------------------|
| `rhodibot`         | B     | Beta          | RSR compliance PRs created on 50+ repos, 9/9 dispatch flows confirmed     |
| `echidnabot`       | B     | Beta          | Proof verification dispatched to 18+ repos, echidna rules confirmed       |
| `sustainabot`      | B     | Beta          | Advisory reports on 283+ repos via hypatia pipeline                       |
| `glambot`          | C     | Alpha-stable  | Aesthetic/style suggestions wired; limited external validation             |
| `seambot`          | C     | Alpha-stable  | Integration seam detection wired; limited external validation              |
| `finishbot`        | C     | Alpha-stable  | Completion analysis wired; limited external validation                     |
| Fleet coordinator  | B     | Beta          | Dispatched to 283+ repos via hypatia → dispatch-manifest → runner chain   |

**Overall:** Grade B for the core three bots and dispatch chain. Grade C for aesthetic/completion bots.

---

## Grade B Evidence — External Targets

gitbot-fleet dispatch chain has operated on the full hyperpolymath estate:

1. **Rust repos** (iseriser, a2ml-rs, conflow, panic-attacker) — RSR compliance PRs, proof checks
2. **Elixir/OTP repos** (hypatia, burble, oblibeny) — quality checks, advisory reports
3. **Gleam repos** (k9_gleam, a2ml_gleam, polyglot-formalisms-gleam) — language policy enforcement
4. **Multi-language monorepos** (developer-ecosystem, standards, nextgen-languages) — fleet dispatch
5. **Game repos** (idaptik, airborne-submarine-squadron) — security + advisory scope
6. **Formal verification repos** (ephapax, stapeln) — proof-regression checks via echidnabot

Dispatch statistics:
- 1635 dispatched actions (600 auto-execute, 667 review, 368 report-only)
- 16671 outcomes recorded (99% success rate)
- Bayesian confidence updating (prior_strength=10, floor=0.10, cap=0.99)

Issues found and addressed:
- Dispatch rate limiting added (50/min per bot, 200/min global)
- Bot quarantine on repeated failures (5+ failures or >30% FP rate)
- Batch rollback capability for auto_execute tier

---

## Grade C Evidence — `glambot`, `seambot`, `finishbot`

These bots are wired in the OTP supervision tree but have limited external validation:
- `glambot`: aesthetic suggestions generated but not yet systematically applied
- `seambot`: integration seam detection algorithm complete but limited cross-repo data
- `finishbot`: completion analysis logic present but calibration needed

**Promotion path to B:** Validate each on 6+ diverse repos with documented improvement outcomes.

---

## Concerns and Maintenance Notes

1. **PAT token**: Automated cross-repo dispatch requires a PAT with `repo` scope — currently missing
2. **Fix scripts**: 310/600 auto-execute entries have null fix_script — recipes exist but no executable
3. **False positive rate**: ~8% on some pattern categories; kanren context-facts would reduce to ~2-3%
4. **VQL federation**: Local-only; multi-store federation not implemented
5. **Neural state persistence**: State dir exists but coordinator hasn't persisted to disk

---

## Run `just crg-badge` to generate the shields.io badge for your README.
