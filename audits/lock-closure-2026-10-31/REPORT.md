# Lock-sync closure audit and bounded secret-scanner migration

**Date:** 2026-10-31
**Corpus:** `hyperpolymath/gitbot-fleet` at `83e35bcfd04d43960d635fec5397da8dfa344268` (read-only for measurement; no workflow deployed, no ruleset edited)
**Scope:** the repository's CI lock-sync automation, its regeneration blind spots, and the *prepared* — not applied — consumer migration of `Secret Scanner` from `hyperpolymath/standards` to `hyperpolymath/cicd-suite@4404c3c0474e17753fe7689f3485c02890aac113`.
**Status:** findings below include one live, user-visible failure. The migration is **blocked** on a live canary that has not run (`preflight.sh` exit 2).

This report separates two things that a single green tick conflates:

* **Findings** — measured defects in the artefact or in the live estate. They are actionable now.
* **Not run** — checks this audit intended and could not execute, with the exact command and the missing prerequisite. Not-run is never counted as a pass.

---

## 1. What was measured, and how

| # | Measurement | Evidence |
|---|---|---|
| M1 | The repository's Lock Sync Gate is green at `83e35bc` | push run [40933665365](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/40933665365) — `success`; `check-lock-sync.sh` executes on the runner (so gawk/yq are present there) |
| M2 | All five job-level reusable callers fail with **zero jobs** | runs [40933665219](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/40933665219) (secret-scanner), [40933665285](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/40933665285) (governance), [40933665246](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/40933665246) (hypatia-scan), [40933665259](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/40933665259) (scorecard), [40933665305](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/40933665305) (mirror): `conclusion=failure`, `jobs=[]`, `name == path` |
| M2b | **Reproduced live, after this audit was written** | PR #631 head `364b2672`: `Secret Scanner` run [41199708273](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/41199708273), `failure`, **0 jobs**; `gh pr checks 631` lists no `scan / …` context at all, while `Lock Sync Gate` and `E2E Tests` ran and passed |
| M3 | The same caller pin produced jobs two days earlier | run [40175794668](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/40175794668), 2026-10-25, `success`, `referenced_workflows=[standards…secret-scanner-reusable.yml@75bd5994…]` |
| M4 | The caller YAML has not changed since 2026-10-20 | commit history of `.github/workflows/secret-scanner.yml` ends at `6e38719569` (2026-10-20); `actions.lock` last changed 2026-10-27 (`e44af814`) but that change only added `fleet-report-back*` entries |
| M5 | The replacement callee runs and creates jobs | cicd-suite push run [41195030267](https://github.com/hyperpolymath/cicd-suite/actions/runs/41195030267) at `4404c3c0`, `success`, `jobs=1` |
| M6 | Required contexts, read from live rulesets | `Secret-Scan-Floor` (24309193): `scan / gitleaks`. `gitbot-fleet-proof-stack-branch` (22415578): `scan / gitleaks`, `scan / rust-secrets`, `scan / shell-secrets` (+ four non-scanner contexts) |
| M7 | Current lock's remote closure is **complete** for `standards@75bd5994…` | union of external `uses:` across the five called reusable files at that SHA = 11 refs; the lock record names exactly those 11 |
| M8 | Current lock's composite closure is **complete** for `actions/upload-pages-artifact@v5.0.0` → `fc324d35…` | its `action.yml` at that commit uses exactly `actions/upload-artifact@bbbca2dd…`; the record names it |
| M9 | Two `check-lock-sync.sh` implementations exist | this repo's script (transitive-closure clauses, job-level refs tolerated when absent) vs. the estate template in `hyperpolymath/standards` (exact uses-set equality) |
| M10 | Regeneration chain upstream is documented as incomplete for our two classes | `scripts/regen-dependabot-locks.sh` in standards: “`--verify-local` cannot see refs used through local composite actions, or job-level reusable-workflow refs”; `complete-job-refs.sh`: “`gh actions-lock` v0.2.0 … still is [blind to job-level `uses:`]” |
| M11 | `dtolnay/rust-toolchain@v1` resolves to two different commits inside one lock | records: `@v1 → 02cb101e…`, `@e2a55d2f… → ref v1`; live `refs/tags/v1` today = `683b588c…` |
| M12 | `@main` step-level pins have drifted from their snapshots | live `deed-ecosystem@main = edb3640c…` vs recorded `ed83d692…`; live `k9-ecosystem@main = 20f6be5b…` vs recorded `c1a34884…` |
| M13 | cicd-suite's own lock at `4404c3c0` is incomplete | 4 `workflows:` keys for 14 workflow files; no `actions/checkout@3d3c42e5…` record although the reusable pins that SHA |

---

## 2. Findings

### F1 — FATAL, live: the secret-scan floor is not executing

The required contexts `scan / gitleaks`, `scan / rust-secrets`, `scan / shell-secrets` are **not being produced**. Every `Secret Scanner` run since 2026-10-27 has zero jobs (M2), including the push to `main` at `83e35bc`; the same pin produced jobs on 2026-10-25 (M3). This is not a YAML regression in this repository (M4).

Reproduced on this very change: the PR that reports the defect (#631) shows no
`scan / gitleaks` entry in `gh pr checks`, because run 41199708273 created zero
jobs, while its `Lock Sync Gate` and `E2E Tests` checks passed normally.

A zero-job run produces **no check run**. A required context that never reports does not read as failing — it reads as absent, which in `gh pr checks` is indistinguishable from “not yet started”. The ruleset is therefore satisfied by silence.

> Consequence for this migration: it cannot “restore” scanning by itself. The replacement must be proven to create jobs *before* it is trusted, which is exactly what the live canary is for.

### F2 — Tool-only regeneration drops job-level reusable edges

`gh actions-lock` v0.2.0 has no code path that writes a job-level `uses:` (upstream #129; re-measured upstream 2026-10-14, M10). A bare regeneration therefore removes the five `standards@75bd5994…` entries under `workflows:`. Nothing dangles, so `check-lock-sync.sh` clauses 1–4 stay green: clause 1 deliberately tolerates an absent job-level ref, clause 2 only flags orphans, clause 3 only flags refs that *are* named, clause 4 only checks that the path key exists.

The new audit reports the resulting state as **NOT RUN (`REUSABLE-UNLOCKED`)** — unproven closure — not green. `scripts/tests/lock-closure-test.sh` pins this: a tool-only regeneration fixture exits 2; a hand-added *leaf* record (the tempting way to go green) exits 1 with `CLOSURE-DROPPED`.

### F3 — Composite and remote-reusable closure can be shortened with no dangling edge

A record's nested `uses:` list is the closed edge set for an external composite action (and, for a job-level call, the union of external `uses:` of the called reusable workflows). Shortening that list to a leaf creates no unresolvable ref, so no existing clause sees it. This is the class that a receipt closes: `scripts/lib/lock-closure-receipts.tsv` records the reviewed union at the record's **resolved commit**, and the audit compares.

Mutations covered by tests: dropped composite edge (`CLOSURE-DROPPED`), dropped reusable edge, leaf-ified record, unreviewed extra edge (`CLOSURE-EXTRA`).

### F4 — The current lock's closure is complete *today*

Measured against the pinned sources (M7, M8): 11/11 remote-reusable refs and 1/1 composite ref. This is the baseline the receipts encode, not an assumption. It is also why the audit exits 0 on the repository rather than adding noise.

### F5 — REVIEW: one reviewed tag, two immutable commits (`dtolnay/rust-toolchain@v1`)

Two records claim `ref: v1` with different commits (`02cb101e…`, `e2a55d2f…`), and the live tag is a third commit (`683b588c…`). The toolchain tag moves; the lock freezes snapshots. Keeping two *contradictory* snapshots is a representation defect even if both were individually valid when written. Baselined for review rather than rewritten here: re-resolving either record is a behaviour change for a different workflow.

### F6 — REVIEW: mutable branch pins at step level (`deed-ecosystem@main`, `k9-ecosystem@main`)

The YAML pins a branch; the lock freezes a commit that has since diverged (M12). The freeze is good — it is the only immutability these calls have — but “pin to `main`, trust the lock” is a policy that should be explicit. Fixing it means changing the dogfood-gate caller or publishing a tag, not editing the lock.

### F7 — cicd-suite's published lock does not prove its own callability

At `4404c3c0` the callee's `actions.lock` keys 4 of 14 workflow files and has no SHA-keyed record for the checkout it actually pins (M13). Its own same-repository runs succeed (M5), so this is **not** proof of a startup failure; it is also not proof that a *cross-repository* caller resolves. The canary is the discriminator. Coordination ask filed against cicd-suite #9 and in the migration README.

### F8 — The current caller carries needless authority

`security-events: write`, `pull-requests: write`, `actions: read` at job level while the callee's jobs declare `contents: read`. Granting more than the callee needs is not a startup failure, but it is unnecessary authority on a workflow whose only job is to read the tree. The migration reduces this to `contents: read`.

### F9 — The gate's own toolchain is unpinned

`check-lock-sync.sh` requires gawk with 3-argument `match()` *or* mikefarah yq / Ruby-Psych. The workflow installs none of them. CI is green (M1), so the current runner image happens to satisfy it; a runner-image change turns the gate red — or, if the probes were weaker, silently vacuous. Upstream's regeneration workflow explicitly installs `gawk`; ours should too (or the script should grow a portable parser).

### F10 — Two authorities for one invariant

Two `check-lock-sync.sh` variants disagree on whether an absent job-level ref is a failure (M9). This audit sides with this repository's measured position (absent is harmless, present-but-unresolvable is fatal) and adds the closure proof on top. Reconciling the variants is a standards-side change; recorded as a coordination ask, not patched here.

### F11 — No regression test covered closure before this change

`check-lock-sync.sh` had no fixture tests in this repository. The two classes in F2/F3 were therefore only discoverable in production. `scripts/tests/lock-closure-test.sh` adds 41 assertions, including the tool-only-regeneration and leaf-record shapes.

---

## 3. Not run

| # | Check | Why not run | How to run it |
|---|---|---|---|
| N1 | A real `gh actions-lock` regeneration diff | the extension's release asset download is blocked by this sandbox's egress allowlist (`release-assets.githubusercontent.com`), so the tool could not be installed | `gh extension install github/gh-actions-lock --pin v0.2.0 && gh actions-lock` on a runner; expect the five `standards@75bd5994…` `workflows:` entries and the composite nested list to be the first casualties, then compare with `scripts/audit-lock-closure.sh` |
| N2 | Executing `check-lock-sync.sh` in this sandbox | no gawk (mawk only), no mikefarah yq, no Ruby-Psych — the script fails closed with `FATAL` rather than lying | `sudo apt-get install -y gawk yq`; CI already runs it green (M1) |
| N3 | The live canary | no live run exists for `unified-dataset-vocab` against `4404c3c0…`; `preflight.sh` exits 2 | see `docs/migrations/secret-scanner-cicd-suite/README.adoc` §3 |
| N4 | Root cause of the zero-job failures | the run API carries no reason; the annotations endpoint 404s for zero-job runs; only the run page states one | open run 40933665219 in a browser as a repo admin, or ask standards for the callee-side change on 2026-10-26/27 |
| N5 | Remote resolution of every tag/branch | opt-in (`--online`) because it needs network; executed here (results: M11, M12) | `./scripts/audit-lock-closure.sh --online` |
| N6 | Fleet-wide consumer census / propagation | out of scope by instruction: deployment must not expand before the canary | not scheduled |
| N7 | The cicd-suite consumer-canary workflow (read-only corpus audit of this repo at `83e35bc`) | not executed in this session | `gh workflow run secret-scanner-consumer-canaries.yml -R hyperpolymath/cicd-suite` (owner action) |
| N8 | Whether GitHub validates a cross-repository reusable against the callee's lock for *this* callee | evidence is contradictory (cicd-suite's own runs succeed with an incomplete lock; standards callers failed while the standards lock looked complete) | the canary, plus a same-SHA control in a second repo |

---

## 4. Deliverables in this change

| Path | What it proves / does |
|---|---|
| `scripts/audit-lock-closure.sh` | Audits representation and closure against reviewed receipts; reports FINDINGS vs NOT RUN; exit 0/1/2/3. Portable (mawk), fails closed on unparseable `uses:`. |
| `scripts/lib/lock-closure-receipts.tsv` | SHA-bound reviewed closure union for `standards@75bd5994…` (11 rows) and `upload-pages-artifact@fc324d35…` (1 row), with provenance. |
| `scripts/lib/lock-audit-baseline.tsv` | Three acknowledged REVIEW findings (F5, F6) plus the online drift re-checks; FATAL findings cannot be baselined. |
| `scripts/tests/lock-closure-test.sh` | 41 assertions: dropped closure (reusable/composite/leaf), malformed refs (subpath, `refs/`, self-SHA action, missing/mismatched commit, case duplicate, ambiguous tag), unparseable values, local composites, the real repository. |
| `docs/migrations/secret-scanner-cicd-suite/` | Prepared three-file migration, receipts, vendored callee fixture, required-context snapshot, canary schema, `preflight.sh`, `rollback.sh`. |
| `.github/workflows/e2e.yml` | Runs the new audit and tests; asserts the migration remains gated (exit 0 or 2, never 1/3). |

The migration itself is **not applied**: `.github/workflows/secret-scanner.yml` is byte-identical to `main`.

---

## 5. Concrete PR and rollback

**PR shape** (from `arena/67f4e25d-gitbot-fleet`):

1. `ci(lock): audit closure against SHA-bound receipts` — audit script, receipts, baseline, tests, E2E wiring, gate guidance.
2. `docs(migration): prepare bounded secret-scanner move to cicd-suite@4404c3c0` — migration package and this report.
3. *(separate, later)* `ci(secrets): migrate the scan caller` — only after `canary/receipt.json` exists; the three patches are the diff.

Required checks for PR 1/2 are unchanged: the new steps run inside `E2E Tests`; no `uses:` is added, so `actions.lock` does not change.

**Rollback of PR 1/2:** revert the commit. The audit is additive; reverting removes tests and receipts and restores the previous gate guidance. No runtime behaviour changes.

**Rollback of the migration (PR 3)** — verified on a full copy of this tree:
apply all three patches → `audit-lock-closure.sh` exit 0 → `rollback.sh --apply`
exit 0 → `secret-scanner.yml`, `actions.lock` and the receipts file are
byte-identical to `main`. The script refuses to run when any of the three
patches has drifted, and it re-audits after reverting.

```bash
git revert <migration-commit>
# or, before committing:
./docs/migrations/secret-scanner-cicd-suite/rollback.sh          # dry run
./docs/migrations/secret-scanner-cicd-suite/rollback.sh --apply  # reverse + re-audit
```

No ruleset edit is required in either direction: the caller job stays `scan`, and the three contexts keep their names. Stop conditions are listed in the migration README §5.

---

## 6. Coordination

**standards** (lock tooling, governance, regeneration): reconcile the two `check-lock-sync.sh` authorities; publish the repair order as the only regeneration path; make `--verify-local` state what it cannot see; decide the `dtolnay/rust-toolchain@v1` snapshot. Issue context: standards#968 (lock drift), standards#1108 §6b (no secret inheritance), standards#1122 (local composites).

**cicd-suite** (replacement callee): close or regenerate the lock at the next published pin (4/14 workflow keys, no SHA-keyed checkout record); land cicd-suite#9; keep compatibility context names stable. Issue context: cicd-suite#9, #30 (floating `@main` composites), #63 (receipts / zero-job evidence).

**Hypatia** (startup and subject verification): count zero-job runs as failed evidence; require SHA-bound detector receipts for compatibility contexts; record the canary receipt immutably and cite the run id.

---

## 7. Reproduction

```bash
# Findings on this repository (offline; expect exit 0 with 3 accepted REVIEWs)
./scripts/audit-lock-closure.sh

# The same, resolving live tags/branches (expect the M11/M12 drift, all baselined)
./scripts/audit-lock-closure.sh --online

# The regression contracts (41 assertions; no gawk/yq needed)
bash scripts/tests/lock-closure-test.sh

# The migration gate (expect exit 2: canary not run)
./docs/migrations/secret-scanner-cicd-suite/preflight.sh --online
```
