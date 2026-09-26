<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Fleet wiring audit — 2026-09-26

## Verdict and scope

The inspected fleet is **not a reliably connected, end-to-end enforcement system**. There is real scanner, fixer, proof-client and coordination code, but multiple entry points have different execution semantics and storage contracts. Some report success without executing anything. More permissions alone will not repair this.

Audit basis: checkout `ce89763a7f9b874288a909732bf21b3143c6afea`, read-only GitHub Actions API inspection, and isolated shell reproductions. No estate repositories were changed, no workflows dispatched, no policies weakened. External Hypatia, proof-burrower, ECHIDNA, cicd-squabbler, GitHub App installations and running hosts were **not** comprehensively audited. Conclusions about them remain open.

## Confirmed breaks

### 1. Critical: generated scripts are recorded as completed fixes

`fleet-coordinator.sh`, `process_findings()` / `execute_auto_fixes()` (roughly lines 289–422):

- `execute_auto_fixes` creates shell files; it never executes them against the target.
- Its return count is the number of generated `.sh` files, including a skeleton created before unsupported finding types are rejected.
- The caller logs “auto-fixed”, writes `outcome: success` to `learning/fix-outcomes.jsonl`, and marks the submission processed.
- Confidence thresholds are loaded in `process_findings`, but this script-generation path selects on `auto_fixable`, not a verified confidence/approval decision.
- The later “dispatch runner” block counts pending entries and prints a message. It does not invoke the runner.

**Reproduced:** copied the coordinator into a temporary directory, supplied one medium-severity `unpinned_action` finding with `auto_fixable: true`, and supplied neither a target checkout nor a fix executable. Result: “auto-fixed 1 issues” and a stored success outcome with `count: 1`. This demonstrates unreliable learning evidence; it does not establish how much production learning data consumed those records.

### 2. Critical: Rust `fleet run` is a simulation

`shared-context/fleet-cli/src/main.rs`, `run_bots()`:

```rust
// Simulate bot execution (would call actual bot here)
ctx.complete_bot(bot, 0, 0, 0)?;
```

It updates an in-memory context, not actual bot processes/services. `show_status` constructs a fresh context rather than loading a persisted execution. `compose.yml` runs this CLI as its coordinator: therefore that compose coordinator is not an actual bot executor. It also mounts the target read-only, and passes `FLEET_REPO_PATH` while the inspected CLI selects the target through `--repo` (default `.`).

### 3. High: supervised scan can be green with no repositories scanned

`fleet-coordinator.sh`, `scan_supervised_repos()`:

```bash
mapfile -t repo_paths < <("$list_script" "${args[@]}")
```

The process-substitution resolver's failure is not propagated. Empty output causes “No supervised repositories resolved” and return 0. Scan failures are counted but do not determine an unsuccessful final status.

The scheduled workflow clones only fleet and Hypatia. It does not provision the default `$HOME/developer/hyper-repos` checkout tree. `shared-context/enrollment/repos.json` is absent here. The resolver requires the root directory to exist before its current-repository fallback.

**Reproduced:** an isolated coordinator with the real resolver and a nonexistent `REPOS_BASE` prints the resolver error, then exits 0 with no repositories resolved. This is a concrete explanation for false-green CI, not proof of the exact contents of unavailable historical logs.

### 4. High: incoming dispatch events stop at an inbox

`.github/workflows/hypatia-dispatch-intake.yml` records event JSON and an index on `findings-submissions`. Its “Trigger fleet processing for incidents” step only echoes values.

`process_findings` consumes `shared-context/findings/<slug>/latest.json` symlinks with `.submission_metadata.repo` and `.findings`. It does not consume that event inbox. The learning workflow checks out the default branch, not the inbox branch. No event-to-findings bridge is wired in these inspected workflows.

Other schema drift: `run_hypatia_scan` accepts bare-array JSON, while `process_findings` expects an object with `.findings`. A successful scan is not sufficient evidence that its output can be consumed.

### 5. High: named membership and readiness are not deployment evidence

`deploy_bots()` writes hardcoded `ready` statuses; it does not build, launch or probe bots.

`bots/sustainabot/README.adoc` explicitly says the adapter is unbuilt and the slot empty. Yet `READINESS.adoc` describes it as Grade B and `deploy_bots()` declares it ready. Historical grades/counters are not current operational attestations.

The shell coordinator, `run-fleet.sh`, and Rust CLI are separate entry points. The first two even default to different repository roots (`~/developer/hyper-repos` versus `~/Documents/hyperpolymath-repos`). There is no single authoritative lifecycle across them.

### 6. Echidnabot has real integration code, but that is not an operational proof chain

`bots/echidnabot/src/dispatcher/echidna_client.rs` implements GraphQL/REST calls. `src/scheduler/job_queue.rs` references the fleet bridge, and `src/fleet/mod.rs` can persist sessions through `ContextStorage`.

However:

- `FleetCoordinator::connect()` creates a new context rather than joining an existing shared session.
- Publishing while disconnected is a no-op returning success.
- Persistence failure is logged but does not fail disconnect.
- Persisted sessions are under `~/.gitbot-fleet`, not the shell coordinator's findings inbox.
- Example configuration points both the bot listener and ECHIDNA service at localhost port 8080; that requires explicit deployment configuration, not two services using their defaults together.
- No `burrow` reference was found in the checked-in echidnabot tree. This does not rule out integration in external projects.

Do not label echidnabot wholly fake; do not label the full proof chain connected without a real request/result/check-run trace either.

### 7. Policy and verification differ by execution path

`scripts/dispatch-runner.sh` is a real executable fixer path with traversal checks, third-party exclusions, a licence/SPDX refusal gate, and optional calls to `mix hypatia.record_outcome`. These are valuable safeguards, not gates to remove.

But it records script exit 0 as success before proving that the targeted finding disappeared. Outcome reporting can be skipped when Hypatia or `mix` is unavailable. The script path does not itself establish a published PR/merged fix. The shell coordinator's separate generator does not share all these gates. `run-fleet.sh` is another independent fixer path.

No centralized repository-policy reconciliation across all these entry points was established. There is no evidence from this audit that cicd-squabbler is the primary cause or that an app is secretly overriding everything. Existing failures suffice to explain substantial inactivity without that hypothesis.

### 8. Tests do not certify the advertised pipeline

`tests/e2e.sh` describes and primarily checks repository structure, shell syntax and required configuration. A passing “E2E Tests” run is not proof of dispatch → bot execution → changed target → verification → published check/PR.

The learning workflow explicitly disables `deploy-approved-rules`; the coordinator's generic generated Logtalk detector contains a TODO and an unconditional `fail`. Generated proposals are not necessarily functioning new detectors.

## Live GitHub observations and limits

- [Supervised scan 35557250407](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/35557250407), 2026-09-21: success; all listed steps succeeded, total run reported as 13 seconds. Its head was `e78aa60f15558c9a2e8ceda14580b0e616565e4b`, not the audited checkout. Actual scan coverage could not be recovered.
- [Dispatch intake 28281412277](https://github.com/hyperpolymath/gitbot-fleet/actions/runs/28281412277), 2026-06-27: failure; check annotation says exit 128. Two adjacent intake runs also failed. Expired logs prevent attributing this specifically to authentication, branch concurrency, or another git error.
- Recent main runs included successful Hypatia Security Scan/E2E/Dogfood Gate and failed Rust/Governance/Lock Sync/Scorecard runs. Overall greenness and overall failure are both too coarse to diagnose capability.
- Recent log downloads failed with EOF; older intake logs returned HTTP 410.
- Repository secret names and Actions permission settings returned HTTP 403 “Resource not accessible by integration”. This limits the audit credential; it does **not** demonstrate missing or broken bot credentials. App installation coverage and target branch protections remain unverified.

## Repair order

1. **Restore truthful status before increasing autonomy.** Eliminate synthetic successes; distinguish planned, queued, running, changed, verified, PR-created, merged, blocked, skipped and failed. Treat historical coordinator-generated success records as unverified, preserving originals for provenance.
2. **Pick one real executor.** Prefer consolidating around the existing dispatch runner after hardening it, rather than adding another orchestrator. Make other entry points adapters or explicitly diagnostic-only. Persist one event/job ID and target SHA across each handoff.
3. **Make missing prerequisites fail visibly.** Propagate inventory/scanner failures; require an explicit inventory, provision target checkouts, validate scanner dependencies, and publish scanned/skipped/failed counts. Allow zero coverage only as an explicit mode.
4. **Implement the inbox bridge.** Normalize incoming events into a versioned finding/job schema; durable acknowledgement only after ingestion; retries, idempotency and dead-letter reporting. Account for concurrent inbox pushes.
5. **Centralize authorization and policy.** Explicit bot capabilities, per-repo opt-outs, allowed operations, approval requirements, and documented precedence. Record why an action was blocked. Retain licence exclusions and confidence gates. Verify least-privilege App installation coverage without exposing secrets.
6. **Prove one canary end-to-end.** An enrolled disposable repo with one known harmless defect must produce a real fix, targeted rescan, and human-reviewable PR/check. Assert missing credentials, unavailable services, zero-diff fixes, duplicate events, and repo opt-outs cannot appear as success.
7. **Then qualify each member.** For echidnabot, demonstrate actual ECHIDNA endpoint health, proof submission, result provenance and a published check; separately specify/test the proof-burrower contract. Keep sustainabot unavailable until an adapter exists. Audit Hypatia and cicd-squabbler producers and external service deployments next.

This is repairable integration and observability debt. The intended separation of scanner, policy, executor and verifier is reasonable; the implementation currently mistakes declarations and bookkeeping for evidence of completed work.
