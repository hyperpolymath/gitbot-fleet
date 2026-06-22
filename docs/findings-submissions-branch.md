<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) -->

# The `findings-submissions` branch

`findings-submissions` is an **automated data-sink branch**, not a feature
branch. The *Hypatia Finding Submitter* pushes scan-finding JSON to it on a
schedule (commits titled `findings: <repo> @ <date>`), accumulating the raw
findings stream that feeds the fleet's review queue.

## Do not reconcile it

- **Do not merge it into `main`.** It carries tens of thousands of lines of
  findings JSON; merging would pollute `main` with transient data.
- **Do not delete it.** It is an active channel — the submitter pushes to it
  continuously and downstream tooling reads from it.
- **Do not rebase/squash/branch-hygiene it.** Its divergence from `main` is
  intentional and expected.

Treat it like a queue, not a contribution. It is deliberately long-lived and
divergent from `main`.
