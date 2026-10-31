# Pull requests for this change

Three pull requests, in order. The first two are safe to merge now. The third is
*authored but blocked*: it must not merge until `canary/receipt.json` records a
live run.

Branch: `arena/67f4e25d-gitbot-fleet`

---

## PR 1 — `ci(lock): prove closure against SHA-bound receipts, and fail closed on dropped edges`

**Base:** `main` · **Head:** `arena/67f4e25d-gitbot-fleet`

### Why

The existing gate (`scripts/check-lock-sync.sh`, clauses 1–4) proves that every
ref named in `actions.lock` is locked and resolvable. It cannot see three shapes
that a regeneration produces:

1. **A dropped job-level reusable edge.** `gh actions-lock` v0.2.0 does not write
   job-level `uses:` entries, so a bare regeneration removes the five
   `hyperpolymath/standards@75bd5994…` entries under `workflows:` — and keeps
   passing, because clause 1 treats an absent job-level ref as harmless.
2. **A shortened nested `uses:` list.** A composite or reusable record whose
   nested list is reduced to a leaf leaves no dangling ref, so clauses 2 and 3
   are silent.
3. **A hand-added leaf record** that makes the gate green while representing
   nothing that the callee actually uses.

All three are closure losses that read as green. This PR makes them observable
and separates them from the question "has this been checked at all?".

### What

* `scripts/audit-lock-closure.sh` — new audit (mawk-portable, fails closed on
  unparseable `uses:`). Emits `FATAL` (exit 1), `REVIEW` (baselinable),
  `NOT-RUN` (exit 2 — never a pass) and `ACCEPTED`. `--online` re-resolves tags
  and branches against the network; `--emit-receipt` produces reviewed closure
  rows.
* `scripts/lib/lock-closure-receipts.tsv` — the reviewed closure union at the
  pinned commits: 11 refs for `hyperpolymath/standards@75bd5994…`, 1 ref for
  `actions/upload-pages-artifact@fc324d35…`, with provenance.
* `scripts/lib/lock-audit-baseline.tsv` — three acknowledged REVIEW findings
  (one tag with two snapshots; two mutable `@main` step pins) plus their online
  drift re-checks. A FATAL finding can never be baselined (asserted by test).
* `scripts/tests/lock-closure-test.sh` — 41 assertions: dropped reusable edge,
  dropped composite edge, leaf-ified record, unreviewed extra edge, subpath ref,
  `refs/`-prefixed ref, missing/mismatched commit, case-duplicate ref,
  self-SHA action ref, ambiguous tag, blocked-scalar `uses:`, local composite
  closure, hand-written record without a receipt, and this repository's own
  lock.
* `.github/workflows/e2e.yml` — runs the contracts and the audit, and asserts
  that the prepared migration never acquires a finding.
* `scripts/check-lock-sync.sh` — message only: it now points at the repair
  order and at the audit, and its "job-level refs not locked (harmless)" note
  no longer claims a closure it cannot prove.
* `audits/lock-closure-2026-10-31/{REPORT.md,evidence.json}` — measurement.

### Findings in this PR (versus not run)

* **FATAL, live:** every `Secret Scanner` run since 2026-10-27 has **zero jobs**
  (run 40933665219, push to `main` at `83e35bc`), so `scan / gitleaks`,
  `scan / rust-secrets` and `scan / shell-secrets` are *not produced*. The same
  pin produced three jobs on 2026-10-25 (run 40175794668) and the caller YAML
  has not changed since 2026-10-20. A required context that never reports is
  absent, not failed.
* Same signature on the other four reusable callers (governance, hypatia-scan,
  scorecard, mirror): `failure`, `jobs: []`.
* `Lock Sync Gate` is green at the same commit: the gate cannot see this class.
* Current closure itself is complete today (11/11 and 1/1) — the receipts encode
  that, they do not assume it.
* **Not run:** a real `gh actions-lock` regeneration diff (the extension's
  release asset is unreachable from this sandbox), `check-lock-sync.sh` locally
  (no gawk/yq/Ruby; CI is green), the live canary, and the root cause of the
  zero-job runs (the run API carries no reason). Each has a command in
  `REPORT.md` §3.

### Test plan

```bash
bash scripts/tests/lock-closure-test.sh     # 41 passed, 0 failed
./scripts/audit-lock-closure.sh             # exit 0, fatal=0 review=0 accepted=3 not-run=0
./scripts/audit-lock-closure.sh --online    # exit 0 (drift all baselined)
PATH=<pyyaml> bash tests/e2e.sh             # 41 passed, 0 failed
```

Required checks are unchanged; no `uses:` is added, so `actions.lock` does not
change in this PR.

### Rollback

`git revert` — the change is additive. Reverting removes the audit, receipts,
baseline, tests and the report, and restores the previous gate message. No
runtime behaviour depends on it.

---

## PR 2 — `docs(migration): prepare bounded secret-scanner move to cicd-suite@4404c3c0 (not applied)`

**Base:** `main` (or PR 1) · **Head:** `arena/67f4e25d-gitbot-fleet`

### Why

The current callee is not executing (PR 1, FATAL finding). The published
replacement, `hyperpolymath/cicd-suite@4404c3c0474e17753fe7689f3485c02890aac113`,
runs real Gitleaks/Rust/shell detectors, verifies its own suite SHA from the
direct caller, and publishes SHA-bound receipts that its compatibility contexts
consume (its push run at that SHA: 41195030267, `success`, 1 job).

### What

Everything under `docs/migrations/secret-scanner-cicd-suite/`, and nothing
outside it except this PR text:

* three patches (`secret-scanner.yml`, `actions.lock`, closure receipt);
* `preflight.sh` — the gate. It fails on findings and refuses to treat missing
  evidence as a pass;
* `rollback.sh` — dry-run reversal with re-audit;
* the vendored callee workflow at the pinned SHA, with `SHA256SUMS`;
* `required-contexts.txt` — the three scanner contexts read from the live
  rulesets on 2026-10-31;
* `canary/receipt.schema.json` + a `result: not_run` example that preflight
  rejects.

### Deliberate boundaries

* No context renamed, added or disabled. Caller job stays `scan`; contexts stay
  `scan / gitleaks`, `scan / rust-secrets`, `scan / shell-secrets`.
* No `secrets:` line (`secrets: inherit` is rejected by preflight).
* `fast-pr: false`; triggers unchanged (`pull_request`, `push` to `main`).
* The reviewed `standards@75bd5994…` record is preserved for the four other
  callers, with its immutable commit.
* Deployment does not expand: the migration is not applied, and the canary gate
  is the only thing that opens it.

### Current gate state

`./preflight.sh --online` → **exit 2**, `findings=0`, NOT-RUN = `CANARY`.
`--check-apply` confirms all three patches apply to this checkout.

### Test plan

```bash
./docs/migrations/secret-scanner-cicd-suite/preflight.sh --online            # exit 2, findings 0
./docs/migrations/secret-scanner-cicd-suite/preflight.sh --online --check-apply
./docs/migrations/secret-scanner-cicd-suite/rollback.sh                      # dry run on an APPLIED tree
# verified: apply -> audit exit 0 -> rollback --apply exit 0 -> files byte-identical
```

### Rollback

Close without merging; nothing was applied. If a later PR applies it, use
`git revert`, or `rollback.sh --apply` before committing.

---

## PR 3 — `ci(secrets): migrate the scan caller to cicd-suite@4404c3c0` — BLOCKED

Draft until the first live `unified-dataset-vocab` canary succeeds and
`canary/receipt.json` is committed with `result: pass`, `suite_sha: 4404c3c0…`,
`fast_pr: false`, `secrets_passed: []`, and all three contexts `success` with
`receipt_sha == subject_sha`.

Then:

1. `preflight.sh --online --check-apply` must exit 0.
2. Apply the three patches (README §4).
3. `./scripts/audit-lock-closure.sh` must exit 0 on the applied tree.
4. Open the PR; require that `Secret Scanner` produces three contexts on the PR
   itself — zero jobs is a failure, not a pending check.
5. Merge only with `Lock Sync Gate` green and no ruleset change in the diff.

Rollback: `git revert <sha>`, or
`docs/migrations/secret-scanner-cicd-suite/rollback.sh --apply` if uncommitted.
Stop conditions: README §5 (zero-job runs, unresolvable ref, success without a
detector receipt, a required secret, or a false negative the current scanner
caught).
