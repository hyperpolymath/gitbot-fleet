# Open-issue ultraplan — gitbot-fleet

**Recon date:** 2026-09-27 (UTC) · **Repository:** `hyperpolymath/gitbot-fleet` · **Base inspected:** `9c4e7d3` (`main`)  
**Purpose:** turn the 15 currently open repository issues into a sequenced, evidence-led resolution program. This is a plan, not a claim that remote incidents have been revalidated or fixed.

## Executive view

The open list is not 15 independent code tasks. It contains (a) two urgent evidence/CI incidents, (b) security and unsafe-automation guardrails, (c) shared-fate workflow work, (d) product/architecture changes with broad blast radius, (e) owner-only credential or data-recovery decisions, and (f) deferred maintenance. Resolve in that order, and do not let a broad feature obscure an active integrity or CI incident.

**First 48 hours:** establish current GitHub check state for #568 using the full head SHA and the `github-actions` check-suite app; verify the `.claude` gitlink condition on default branch and install a regression guard for #502; preserve evidence and seek owner direction on the corrupt external clone in #491; request token owner action for #433. These cannot be resolved by guessing from this checkout.

**Next:** protect dispatch from unsafe LicensePolicy findings (#253), make Hypatia ingestion fail closed and canary-bounded (#264), then address concrete Rust/dependency CI gaps (#432, #278) and complete independent bot/client improvements (#245, #255). Plan the expensive fleet-wide mode/rhodibot redesign (#310) and privileged settings actuator (#362) as separately gated programs. Keep #503 explicitly deferred until its stated dependency/backlog condition is clear.

## Recon findings and confidence

- `gh issue list --state all` returned **15 OPEN** and 8 CLOSED issues. Open IDs: #568, #503, #502, #491, #433, #432, #362, #324, #310, #278, #264, #255, #253, #245, #214.
- The worktree was clean on the required Arena branch at inspection. The checkout is shallow/grafted at `9c4e7d3`; it does not contain enough local ancestry to validate remote incidents or reconstruct historic commits.
- `bots/*`, `robot-repo-automaton`, `shared-context`, scripts, and Actions workflows form a multi-component fleet. Root CI already builds/tests/clippies four Rust modules (`robot-repo-automaton`, `shared-context`, `dashboard`, `rhodibot`); Rustfmt is explicitly informational. GSBot has a real `cargo deny` advisory gate in the current `.github/workflows/rust.yml`, so #432's 2026-07 premise is stale at this checkout and needs live re-triage before code changes.
- `.gitignore` already contains `.claude/` (twice); `git ls-files -s` showed no mode-160000 entries in the current checkout. This does not prove main has no historical/current orphan gitlinks; #502's durable missing piece is the explicit tree-level guard and authoritative main check.
- The check names in #568 map to extant workflow configuration: GSBot job is in `rust.yml`; governance workflow exists; validate-hypatia checks must be investigated in the workflow/scripts and remote run logs. Do not interpret absent check-runs as green: startup failures may create no job/check.
- Relevant implementation homes: `.github/workflows/{rust,governance,repo-integrity-guard,hypatia-dispatch-intake}.yml`; `scripts/` and `scripts/tests/`; `shared-context/dispatch`; Rust bot crates under `bots/`; `robot-repo-automaton/src/`; and cross-repo reusable workflow/credential actions in the linked standards/farm repositories.
- Issue descriptions include snapshots from June–August 2026. Re-derive all counts, alert states, remote heads, and credential status immediately before acting. In particular #278 and #432 must not be implemented from stale evidence.

## Priority and sequencing

| Wave | Issues | Order / reason |
|---|---|---|
| 0 — establish truth & contain | #568, #502, #491, #433 | Evidence-first: CI startup/check-suite truth, git tree integrity, preserve possibly unique commits, owner-only credential refresh. No speculative mutations. |
| 1 — safety boundaries | #253, #264 | Ensure unsafe findings cannot auto-execute; make structured route metadata, unknown-category fallback, dedupe, limits, canary and kill switch enforceable before any fan-out. |
| 2 — bounded CI/security | #432, #278, #324 | Re-triage current reality; implement only remaining actionable controls. Signed propagation depends on external farm billing/signing. |
| 3 — focused product/code | #245, #255, #214 | Typed protocol handling, audited panic cleanup, standing recipe only if sustainabot source/engine exists and supports required predicates. |
| 4 — architectural/fleet programs | #310, #362 | Separate RFC/design, local proof, staged rollout and explicit owner authorization for write scopes. No estate-wide actuation as one PR. |
| Deferred by intent | #503 | Respect its explicit no-churn deferral until dependency/CI backlog is cleared; then scope migration/proof/template drift. |

This ordering is a dependency order, not an assertion that lower waves are unimportant. Any active security exposure discovered during revalidation jumps to containment.

## Per-issue resolution plans

### #568 — triage three pre-existing red checks (P0 evidence / #566 follow-up)
**Decision needed:** fixed, retired, or documented exemption for each of GSBot build/security, governance workflow security linter, and Validate Hypatia Baseline. No silent muting.

1. Resolve the full 40-character PR head SHA; query check suites and separate the `github-actions` app from third-party apps. Inspect the actual run/workflow files and logs, not just rollups. Compare the same checks on current default-branch SHA. Specifically detect `startup_failure` and missing GitHub Actions check suites.
2. Re-run authoritative workflow startup on the current head. The issue notes that `gh actions-lock --verify-local` has both false-stale and false-green cases; treat it as a hint only. Confirm literal `uses:` values satisfy GitHub's policy.
3. For each red, record root cause, branch-vs-main comparison, owner, and disposition. Fix in repository code/workflow when attributable; retirement must remove workflow and required-check configuration together; exemption must record reason, scope, expiry/review date, and approver.
4. Verify fixed items green on default branch, then update/close #568 with run links and evidence. Do not weaken required checks or use `continue-on-error` to make a red disappear.

**Done means:** all three have recorded dispositions; fixed checks are green on default branch; retired checks are absent from both workflow and required set; exemptions are time-bounded and reviewable.

### #502 — orphan `.claude/` gitlinks breaking submodule traversals (P0 integrity)
1. Confirm `git ls-tree -r HEAD` and `git ls-files -s` on live default branch for mode `160000`, inspect `.gitmodules`, then run the exact affected CI commands. Preserve any existing gitlink history until its source is understood.
2. `.gitignore` already excludes `.claude/`; remove duplicate ignore entry when convenient, but do not mistake that for a protection against explicitly staged gitlinks.
3. Add a deterministic repository-integrity test that rejects any mode-160000 entry beneath `.claude/`, and rejects any gitlink lacking a matching `.gitmodules` path/URL (including nested paths). Make it run on PR and default-branch workflows before submodule-dependent scans. Add fixtures for valid registered submodule, orphan `.claude` gitlink, and generic unregistered gitlink.
4. Ensure test logic handles quoted paths/spaces and obtains modes from the Git index/tree rather than filesystem directory heuristics. Document safe staging guidance.

**Done means:** no offending gitlinks in default branch; regression test catches both issue classes; formerly failing scans run successfully.

### #491 — reported corrupt external clone and possibly unique commits (P0 data preservation; owner decision)
This describes `/home/hyperpolymath/...`, not this checkout. **Never delete, reclone, reset, repack, or force-push that clone as part of this plan.**
1. Ask owner to verify the healthy recovery clone and preserve its full copy. Determine, via GitHub commit/object APIs and branch refs, whether `632b54a` is wanted and whether `e69a536`/`ce02567` content was squash-merged. Record SHA + patch IDs + remote branch locations.
2. If owner confirms `632b54a` is wanted, publish it from the healthy recovery clone through a new reviewable branch/PR (avoid direct main push); verify patch and CI. If it is already represented, document equivalence.
3. Only after preservation and owner sign-off, diagnose the damaged clone read-only: record `git fsck` findings, object IDs, pack/index hashes, refs and reflogs. Recover into a separate fresh destination from verified surviving objects/remotes; never overwrite the damaged source. Retain an archive until recovered work is validated.
4. Close only with recovery disposition and proof that no unique commit/work was lost.

**Done means:** potentially unique work is either safely published or explicitly rejected by owner; damaged source preserved; recovery process documented.

### #433 — `FARM_DISPATCH_TOKEN` bad credentials (owner action; no code workaround)
1. Re-derive current failure cohort and exact API error from recent `Instant Sync` runs; differentiate expired/revoked/under-scoped from workflow permission or target-repo configuration.
2. Request authorized owner to rotate/replace the secret with a least-privilege token able to dispatch only intended target repos, store it in the expected secret location, and avoid exposing the value in logs/chat.
3. Test a small supervised canary, then sample the cohort; confirm success and absence of token text in logs. If rejected, stop and return the error to owner; do not substitute broader credentials or weaken workflow.

**Done means:** owner-managed credential repaired, canary and sampled cohort green, no secret leakage. This issue is not closed by changing code alone.

### #253 — LicensePolicy must never reach automatic executor (P1 safety)
1. Find the actual route/dispatcher implementation and schema in current checkout (`shared-context/dispatch`, fleet scripts/registry, intake workflow); do not transplant the Elixir example in issue verbatim—the repo is primarily Rust/shell and may have moved since filing.
2. Enforce the invariant at the last dispatch boundary: `do_not_automate=true` and LicensePolicy always go to advisory/manual route independent of confidence. Unknown policy metadata also fails closed. Preserve original finding payload/provenance.
3. Tests: LicensePolicy at 0.99 and 0.5 remains advisory; `do_not_automate` on any category remains advisory; non-license 0.99 keeps existing routing; malformed/missing flags cannot opt into execution; dedupe/replay doesn't upgrade route.
4. Add a contract test to workflow/integration path, and ensure executor independently refuses prohibited categories (defense in depth). No automated license edits.

**Done means:** unit + dispatch contract tests pass, and production entrypoint demonstrates refusal before any mutation/PR action.

### #264 — consume Hypatia safety metadata and control blast radius (P1 design + staged implementation)
1. Trace event ingress (`hypatia-dispatch-intake.yml`), schema, persistence, dispatcher, registry, executor, existing exclusion/kill-switch and audit logs. Inventory which fields truly arrive today.
2. Define versioned typed contract: stable finding ID, category/class, route, safety/action mode, confidence, recipe/provenance, target repo, expiry/scan revision. Reject malformed records; unknown category/action defaults report-only.
3. Define explicit state machine: report-only default; approval gates before PR/actuation; canary cohort; per-rule/per-repo concurrency; idempotency key/dedupe; retries and terminal states; audit record; quarantine/kill switch. Confidence decreases on verified false positives/failures but cannot automatically authorize a more privileged action.
4. Implement ingestion/schema + route-only dry-run first. Simulate representative known/unknown/malformed findings with fixture events. Run canary on non-critical repos, require owner approval for widening, and set stop conditions (unexpected mutation, duplicate action, false positive, error budget).
5. Add separate authorized rollout controls and operational runbook; do not let Hypatia metadata itself grant write permission.

**Done means:** integration tests show unknown/malformed findings cannot mutate; dry-run and canary evidence; limit, dedupe, and kill-switch exercised; explicit sign-off before production fan-out.

### #432 — Rust advisory gate (P2, re-triage stale premise)
Current `rust.yml` already has `cargo deny ... check advisories` in GSBot job. First determine whether this satisfies the intent for the reusable workflow and all relevant crate jobs; `cargo audit` specifically is not the only accepted mechanism in the issue. Check `deny.toml`, lockfile coverage and actual live run. Avoid duplicate scanners.
1. Reopen/retain only if the advisory check doesn't cover required crates/workspaces or is not blocking on vulnerable/unsound dependencies.
2. If a gap exists, select one canonical advisory tool/config; run locally on every relevant lockfile, patch/triage current findings, and ensure CI runs it with locked inputs and no unapproved blanket ignores.
3. Update issue with current evidence and close as already implemented if scope is fully met; otherwise implement the narrow gap and verify green CI.

**Done means:** policy coverage proven per relevant Rust dependency graph, current advisories triaged, gate is blocking, no redundant check without benefit.

### #278 — Dependabot alerts (P2, live triage required)
1. Query current GitHub security alert inventory; old count (1 high/1 moderate/4 low, June) is not assumed current. Export package, ecosystem, vulnerable range, fixed version, manifest/lockfile, dependency scope and reachability.
2. Triage high and moderate first, updating manifests and locks with minimal compatible versions. Run crate-specific tests, workspace build, clippy and advisory scan. Batch low alerts only when upgrades are low-risk and don't bundle unrelated major changes.
3. For no-fix or unreachable alerts, record evidence, compensating controls, reviewer and next review date; never suppress by default.
4. Verify alert closure in GitHub security tab after merged default-branch update.

**Done means:** every currently open alert has a patched, accepted, or time-bounded documented disposition; high/moderate fixed or approved exception.

### #324 — 14 strict-ruleset reusable pin consumers (P2; external dependency)
1. Revalidate each repository's current pin, ruleset, branch protection, signed-commit requirement, default branch and current standards SHA. Exclude `hyperpolymath/007` as issue requires.
2. Check `.git-private-farm#98` billing/status and whether signed propagation is operational. Do not use unsigned Contents API or bypass rulesets.
3. If farm path works, run one or two canaries, verify signature, resulting pin and required checks, then staged cohort propagation with per-repo error capture. If not, use signed human-authored per-repo PRs; preserve PR-only and code-scanning constraints.
4. Record completion matrix with commit/signature evidence and any repo-specific blocker; don't report all 14 complete from one successful sample.

**Done means:** all 14 current consumers verified at target pin with verified signed propagation (or explicitly blocked with owner), no excluded repo altered.

### #245 — Echidnabot typed VerifyOutcome (P2 independent feature)
1. Verify echidna API schema currently exposes `outcome`, `mode`, `smtStatus` for GraphQL and REST; capture schema/version compatibility and casing.
2. Define Rust enums with serde compatibility for known outcomes plus safe unknown handling. Add optional fields to GraphQL and REST response structs; request GraphQL fields only when server supports them or handle schema incompatibility predictably.
3. Map typed `TIMEOUT` to `ProofStatus::Timeout`, other outcomes deliberately, and keep current message-based parser only as fallback when outcome omitted. Preserve raw unknown outcome for diagnostics; don't conflate `INVALID_INPUT` with no proof.
4. Add transport fixture tests (GraphQL and REST), legacy server omission test, unknown enum test, and mapping table tests. Do not edit `src/abi/` per issue constraint.

**Done means:** cargo tests prove both transports and old-server fallback; downstream consumers distinguish timeout; documented compatibility.

### #255 — `expect` in Rust hot paths (P2 reliability/hygiene)
Do not apply issue's blanket suggestion to use `unwrap_unchecked`; that adds unsafe code and is not a routine remedy.
1. Regenerate current Hypatia finding set; map 29 reported findings to present source lines and establish baseline by crate/file. De-duplicate findings that moved or disappeared.
2. For each hot-path expect, establish invariant. Prefer fallible propagation (`?`), typed construction, or explicit recoverable error. Use a panic only where contract explicitly makes it programmer error and document why. Avoid unsafe unchecked access absent a separately reviewed proof.
3. Fix in small crate- or analyzer-coherent patches. Tests cover malformed input, empty/missing values and relevant boundary cases. Run `cargo test --locked`, `cargo clippy --locked --all-targets -- -D warnings`, formatting check for affected crates, and Hypatia rescan.
4. Update the count by finding ID and explain any retained deliberate panic.

**Done means:** current confirmed hot-path findings are fixed or individually justified; no new unsafe code; tests and rescan substantiate reduction.

### #214 — standing SafeDOM `.res` → `.affine` recipe (P3; verify product exists)
1. Inspect sustainabot implementation, recipe schema, action allowlist and current source-of-truth path. Issue's sample is aspirational until implementation supports `file_exists`, `file_missing`, `skip_if` semantics and safe substitute.
2. Confirm canonical fixture content/hash in burble and check whether the template fixes in linked repos still stand. Do not copy a stale artifact from issue text.
3. Implement idempotent recipe with exact-path allowlist, skip condition for substantive `.res` files, canonical source provenance, dry-run output, no destructive overwrite, and recipe unit tests for positive/negative/skip cases. Test paths with spaces and symlink/non-regular files.
4. Canary against synthetic fixture or isolated test repo, then document operational activation. Keep this recurring recipe open as recurring only if the tracker policy requires it.

**Done means:** recipe runs only on intended case, preserves unrelated `.res` files, and produces deterministic source-verified output.

### #310 — rhodibot deterministic engine + fleet `mode` (P3 architecture / high-risk rollout)
Split into design, engine, workflow, directive migration and estate propagation. The issue reports interim audit mitigations on three repos; verify they're still in place.
1. Inventory current `rhodibot.yml` variants, Rust engine behavior, licence guards, config parsing, app auth TODOs, stale `workflow_run` contexts and available canary repos. Preserve report-only/audit mitigation during all work.
2. Specify a shared directive schema for `mode` (`active/audit/passive/ignore`), enforcement and repo-kind, including defaults. Safety default must never mutate on absent/invalid mode; define whether that means audit or passive through owner decision. `audit` records full would-act forensic data but performs zero writes; `ignore` skips scanning.
3. Implement deterministic Rust engine mode semantics with a side-effect boundary and tests asserting zero network writes/PRs in audit/passive/ignore. Build reusable workflow and thin delegators only after engine parity; retain guarded licence behavior.
4. Add GitHub App JWT with least-privilege permissions as an isolated change, secret-free tests, token refresh/expiry tests, and no fallback that silently broadens token scope.
5. Migrate directives across bots in small PRs, with schema validation, backwards-compatible reader, owner approval, and per-repo mode inventory. Remove dangling contexts only after branch-protection required-check migration is coordinated.
6. Canary audit first; owner explicitly authorizes active canary; staged rollout and immediate kill switch.

**Done means:** all modes contract-tested, no guardless inline workflow, safe default decided, app auth reviewed, stale contexts handled, migration inventory and canary evidence complete. No estate-wide active rollout without approval.

### #362 — Actions-policy settings-write actuator / self-heal (P3 privileged automation)
1. Reconfirm current affected repo cohort, `allowed_actions` and SHA pin settings; check existing `scripts/fix-actions-policy.sh`, registry, actuator code, exclusion registry and whether issue was partially completed since filing. This issue may be stale: it references a 91-repo incident and sibling work that may have landed.
2. Keep report-only as default. Define allowed target owner/repo list, excluded repos, policy source-of-truth, approved values, dry-run output, explicit per-run authorization, audit trail, rate limit and rollback/stop conditions. GET before/after verify both `allowed_actions` and `sha_pinning_required`; never disable SHA pinning.
3. Separate read/report from write capability. Add a dedicated capability check at every settings PUT path (not just PR write guard). Require explicit safe-repo allowlist and approval before first write. No blanket write token or automatic fan-out from a finding.
4. Unit/integration tests with mocked GitHub API for reset-to-default, empty allowlist, API failure, partial update, verification mismatch and retry/idempotency. Canary one repo, then very small batches; log outcome without secrets.
5. Only after actuator safety is proven, owner approves fleet remediation and monitor startup failures/check suites per repo.

**Done means:** current scope rederived; actuator cannot write outside explicit allowlist; dry-run, kill switch and GET verification tested; canary owner-approved; SHA pinning remains on throughout.

### #503 — Creusot + retire legacy `6a2` skeleton paths (explicitly deferred)
Do not mix with CI clearance. Once dependency/CI PR backlog is explicitly clear:
1. Inventory all canonical and embedded `6a2` paths, generated skeleton output and `rsr-template-repo` source/drift policy; verify issue's stated destination path against current naming convention before changing it.
2. Write proof obligations and select Creusot-supported boundary; avoid claiming whole-crate verification where only pure functions are proven. Build small proof tranche with CI toolchain pin and documented unproven boundary.
3. Migrate canonical skeleton and embedded template together; add negative test rejecting `6a2` and positive exact `descriptiles` output; run generated-tree diff/drift tests.
4. Audit docs/fixtures and test generation from clean temp repository. Plan downstream propagation as a later, separately approved campaign.

**Done means:** proof command is reproducible in CI, generated template matches canon, tests reject legacy output and downstream rollout is not bundled.

## Cross-cutting guardrails

- **Evidence ledger:** each work item records checkout/head SHA, remote run URL, data timestamp, environment, and owner decisions. Issue text is a report, not ground truth.
- **No false-green interpretation:** enumerate check suites, identify GitHub Actions app, inspect startup failures, use full SHA, compare to current default branch.
- **Least privilege:** separate read-only ingestion, PR creation, and repository-settings writes; credentials never enter logs or artifacts.
- **Fail closed:** unknown category, malformed metadata, missing recipe, invalid mode or missing approval is report-only/no mutation.
- **Prove safety with tests:** negative tests must assert no write/PR/dispatch, not merely expected route labels.
- **Incremental rollout:** local tests → fixture/integration → isolated canary → small cohort → fleet; each step has stop conditions and rollback.
- **Avoid stale duplicate work:** before implementing, query issue state and recent commits/PRs, and update/close issue if already satisfied.

## Suggested operating checklist

1. Post a short evidence update on #568/#502 and request owner decisions for #491/#433; do not close owner-action issues without their evidence.
2. Open one implementation branch/PR per coherent safety boundary, not one omnibus “issues” PR.
3. Start #253 and #502 regression tests early; these are small, high-leverage guardrails.
4. Design #264 before wiring mutation-capable behavior; its fail-closed contract should be a prerequisite for any Hypatia-driven fan-out.
5. Re-triage #432/#278 and mark stale premise explicitly instead of doing duplicate work.
6. Keep #310 and #362 as staged programs with separate approvals for credentials, settings writes, and active fleet rollout.
7. Review all acceptance criteria against current issue bodies before closure; link tests, CI runs, canary outcomes and owner approvals.
