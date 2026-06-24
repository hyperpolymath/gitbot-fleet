<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell -->

# Safety Triangle

Every automated action gates on a confidence threshold supplied by the
upstream Hypatia scanner. The tier decides whether the fix is applied
silently, opens a PR, or files an issue for human review.

```
Eliminate (auto_execute >= 0.95)  →  Direct fix, no review
Substitute (review >= 0.85)       →  Proven-module replacement, needs review
Control    (report < 0.85)        →  Human review required
```

## How it maps to action

| Tier | Confidence | What happens |
|---|---|---|
| **Eliminate** | `>= 0.95` | `fix-*.sh` runs in-place, commits with a deterministic message, no PR. |
| **Substitute** | `>= 0.85` | Module-level replacement (e.g. swap `unsafe.deref` for `proven::checked`) — PR opened with `needs-review` label. |
| **Control** | `< 0.85` | No code change. Issue filed via `process-review-findings.sh`. |

## Why these thresholds?

The Hypatia rule corpus is calibrated such that:

- `>= 0.95` rules carry estate-wide observational evidence that the
  finding is a true positive (e.g. missing SPDX header, tracked
  `package-lock.json`).
- `>= 0.85` rules have substitution-equivalent fixes whose
  *semantics* are settled but whose application context wants a
  human glance.
- `< 0.85` is by definition out of scope for automation — the
  remediation requires judgment.

## See also

- [Fix scripts in `scripts/fix-*.sh`](https://github.com/hyperpolymath/gitbot-fleet/tree/main/scripts) — the eliminate-tier executors.
- [`docs/ARCHITECTURE.md`](https://github.com/hyperpolymath/gitbot-fleet/blob/main/docs/ARCHITECTURE.md) — the full pipeline.
