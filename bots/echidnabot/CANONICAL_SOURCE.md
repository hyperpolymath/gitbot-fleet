<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- Copyright (c) 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> -->

# Canonical Source Map — echidnabot (fleet copy)

> **You are reading the FLEET tree perspective.** The sibling perspective
> lives at [`hyperpolymath/echidnabot/CANONICAL_SOURCE.md`](https://github.com/hyperpolymath/echidnabot/blob/main/CANONICAL_SOURCE.md)
> and has identical structure with the two roles swapped.

Echidnabot exists in **two trees** by design. Each tree has a different
**purpose**; neither is a stale copy of the other. This document resolves the
seam (issue [`hyperpolymath/echidnabot#51`](https://github.com/hyperpolymath/echidnabot/issues/51)).

---

## 1. Purpose — this tree (`hyperpolymath/gitbot-fleet/bots/echidnabot/`)

The **fleet copy** is the **deployed production bot**:

- Co-deployed with sibling bots (`accessibilitybot`, `finishingbot`,
  `glambot`, `panicbot`, `rhodibot`, `seambot`, etc.) under
  `gitbot-fleet/bots/`.
- Shares `gitbot-fleet/shared-context/` for cross-bot coordination
  (Dependabot watches each bot independently — see
  `dependabot/cargo/bots/echidnabot/*` branches).
- **Exact-pinned deps** (e.g. `tokio = "1.52.3"`, `axum = "0.8.9"`,
  `serde = "1.0.228"`) — production lockfile alignment, no version drift
  between deploy cycles.
- Slimmer development surface: no `.claude/`, no per-bot `.github/`
  (fleet-level governance lives in `gitbot-fleet/.github/`), no
  `EXPLAINME.adoc`, single fuzz target.
- Carries **production-only modules** that may or may not flow back to the
  SDK: `src/trust/migration_scanner.rs`, `tests/webhook_e2e_test.rs`,
  `examples/SafeDOMExample.affine` (estate `.affine` migration in flight).

## 2. Sibling — `hyperpolymath/echidnabot` (standalone)

The **standalone repository** is the **SDK / library / reference
implementation**:

- Tagged releases, semver versioning.
- Buildable as a library crate (`echidnabot` on crates.io eventually) and as
  a reference binary.
- **Relaxed dependency pins** (e.g. `tokio = "1"`, `axum = "0.8"`,
  `serde = "1"`) — version *ranges*, not exact versions. Downstream consumers
  pick their own pinned lockfile.
- Carries the **full development surface**: `.claude/`, `.github/` (issue
  templates, workflows), `EXPLAINME.adoc`, `RSR_OUTLINE.adoc`, `proofs/`,
  `ffi/` (Idris2 ABI bindings), `contractiles/`, `.clusterfuzzlite/`,
  governance scripts (`scripts/governance/`), full fuzz target set
  (`fuzz_config.rs` + `fuzz_hmac.rs` + `fuzz_webhook_json.rs`), full test
  matrix (`integration_tests.rs` + `lifecycle.rs` + `property_tests.rs` +
  `seam_test.rs` + `smoke.rs` + `regressions/`).
- Library-shaped src tree: `src/abi/`, `src/feedback/`, `src/llm.rs`,
  `src/api/rate_limit.rs`, `src/modes/directives.rs` — features that exist
  there are the **forward edge** of the codebase.

---

## 3. File classes — who is canonical for what

| File glob | Canonical | Direction | Rationale |
|---|---|---|---|
| `src/**/*.rs` (library + bot code) | **standalone** | standalone → fleet | Library/SDK is the forward edge. Fleet consumes a snapshot. |
| `src/abi/**` | **standalone** | standalone → fleet (when fleet wants ABI surface) | ABI namespace is owner-managed in standalone; fleet does not need it for deploy. See memory note `feedback_echidna_src_abi_namespace_intentional`. |
| `src/trust/migration_scanner.rs` | **fleet** | fleet → standalone (when promoted) | Production-driven feature; promote to standalone when the API stabilises. |
| `src/feedback/**`, `src/llm.rs` | **standalone** | standalone → fleet (on demand) | Forward-edge research surface (Package 7b double-loop, BoJ-mediated LLM); fleet adopts when production-ready. |
| `src/api/rate_limit.rs` | **standalone** | standalone → fleet | Hardening landed in standalone first; fleet should adopt for production. **See drift note below.** |
| `src/modes/directives.rs` | **standalone** | standalone → fleet | Mode-selection directives are SDK surface. |
| `Cargo.toml` (package metadata) | **shared** | bidirectional — diverge by design | Standalone keeps relaxed ranges; fleet keeps exact pins. Authors string + crate metadata sync periodically. |
| `Cargo.lock` | **each tree owns its own** | n/a | Lockfiles are deployment artefacts; standalone's reflects relaxed-range resolution, fleet's reflects pinned-version resolution. |
| `tests/integration_tests.rs`, `tests/lifecycle.rs`, `tests/property_tests.rs`, `tests/seam_test.rs`, `tests/smoke.rs`, `tests/regressions/**` | **standalone** | standalone → fleet (on demand) | Full test matrix lives in standalone; fleet runs a subset in production CI. |
| `tests/webhook_e2e_test.rs` | **fleet** | fleet → standalone (recommended) | End-to-end webhook test was added during fleet deployment hardening; should be promoted back to standalone. |
| `fuzz/fuzz_targets/fuzz_hmac.rs`, `fuzz_webhook_json.rs` | **standalone** | standalone → fleet (on demand) | Fuzz target expansion lives in standalone (also `.clusterfuzzlite/`). |
| `examples/*.affine` (e.g. `SafeDOMExample.affine`) | **fleet** | fleet → standalone (on `.affine` migration) | Estate `.affine` migration touched the fleet copy first; will reach standalone when the SafeDOM stdlib lands (`affinescript#56`). |
| `examples/*.json`, `examples/*.ts`, `examples/*.rescript` | **standalone** | standalone → fleet | Reference examples for SDK users. |
| `echidnabot.example.toml`, `echidnabot.toml` | **standalone** | standalone → fleet | Configuration *schema* is SDK surface; fleet should mirror schema and only override defaults. |
| `Containerfile`, `guix.scm` | **standalone** | standalone → fleet | Reproducible-build manifests are SDK surface. Fleet may override base image for deploy. |
| `packaging/**` (debian/, rpm/, arch/, aur/, chocolatey/, macports/, scoop/) | **standalone** | standalone → fleet (when versions bump) | Distribution packaging is release-process artefact. |
| `hooks/**` (git hooks: SPDX, SHA-pins, CodeQL, permissions, tsjs-blocker) | **standalone** | standalone → fleet | Governance hooks; standalone is the source of truth. |
| `README.adoc`, `README.md`, `CHANGELOG.md` (vs `CHANGELOG.adoc`), `ROADMAP.adoc`, `CITATION.cff`, `codemeta.json`, `PALIMPSEST.adoc` | **standalone** | standalone → fleet | Doc canon. The `.md` vs `.adoc` CHANGELOG split is a long-standing inconsistency; standalone uses both. |
| `EXPLAINME.adoc`, `MAINTAINERS.adoc`, `RSR_OUTLINE.adoc`, `RSR_COMPLIANCE.adoc`, `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`, `SECURITY.md` | **standalone** | standalone → fleet (where applicable) | Project-level docs; some (e.g. `RSR_OUTLINE.adoc`) are standalone-only because the SDK is the RSR-compliant artefact. |
| `.claude/`, `.github/`, `.gitattributes`, `.gitignore`, `.editorconfig`, `.guix-channel`, `.well-known/`, `.machine_readable/`, `0-AI-MANIFEST.a2ml` | **standalone-only** | n/a | Per-repo metadata. Fleet-level equivalents live at `gitbot-fleet/.github/` and `gitbot-fleet/.claude/`. |
| `proofs/`, `ffi/`, `contractiles/`, `scripts/governance/` | **standalone-only** | n/a | Research / formal-methods / governance surface that does not belong in a deployed bot. |
| `scripts/batch_driver.sh` | **fleet-only** | n/a | Fleet-orchestration helper; out of scope for SDK. |
| `wiki/Home.md`, `docs/content/api.md`, `docs/templates/default.html` | **standalone** | standalone → fleet (rarely) | Documentation canon. |
| `docs/tech-debt-2026-05-26.md` | **standalone-only** | n/a | Tech-debt log; SDK-internal planning artefact. |
| `BRANDING.md`, `TESTING-REPORT.adoc`, `TESTING-REPORT.scm`, `SESSION_SUMMARY_2026-01-29.md`, `SONNET-TASKS.md`, `RELEASE_CHECKLIST.md`, `Mustfile`, `Containerfile` | **standalone** | standalone → fleet | Shared release/branding/testing surface. |

---

## 4. Sync policy

**Manual cherry-pick with quarterly diff sweep.** No automation.

- **Default flow:** changes land in the canonical tree (per the table above)
  via PR. The owner cherry-picks to the sibling when ready, in a separate PR
  with a `Refs hyperpolymath/<other-repo>#<PR>` line.
- **Quarterly diff sweep:** the owner runs `diff -rq` between the two trees,
  classifies new deltas against the table above, and either (a) cherry-picks
  to align, (b) updates the table here to record intentional divergence, or
  (c) files a follow-up issue if the delta needs design work.
- **CI gating:** none today. Adding a "no undocumented divergence" check
  would require standards-repo work and is explicitly **out of scope**
  (issue #51 picked the documentation-first option).

### What this policy explicitly does NOT do

- No auto-mirror / no sync bot / no submodule / no git subtree.
- No "regenerate fleet from standalone on every release" script.
- No standards-repo workflow.

If automation becomes necessary later, a one-off `gitbot-fleet/scripts/sync-bot.sh`
(per-bot, not estate-wide) would be the natural place — but the owner has
deliberately deferred this until the divergence pattern stabilises.

---

## 5. When to PR which — decision tree for contributors

```
What kind of change?
│
├── New library API / new src module / SDK surface
│   └─→ PR to STANDALONE (hyperpolymath/echidnabot). Owner cherry-picks
│       to fleet when ready.
│
├── Bug fix in shared src/**/*.rs
│   └─→ PR to STANDALONE. Fix flows fleet-ward at next sweep.
│       (If the bug is production-only and you have a reproduction,
│        a fleet-side hotfix PR is acceptable; cross-reference standalone.)
│
├── Production hardening (rate limit, retry, observability)
│   ├── If it's a new SDK feature → STANDALONE first, fleet adopts.
│   └── If it's deploy-specific (k8s tuning, fleet routing) → FLEET only
│       (this repo).
│
├── Dependency bump
│   ├── Patch bump (security) → BOTH trees, simultaneously.
│   ├── Minor/major in standalone → STANDALONE only (relaxed ranges
│   │   absorb it). Fleet updates exact-pin when ready.
│   └── Dependabot-driven exact-pin bump → FLEET only (this is what
│       Dependabot does; standalone's relaxed pins don't need it).
│
├── `.affine` migration / AffineScript example
│   └─→ FLEET (where the migration is in flight). Promote to STANDALONE
│       once `affinescript#56` lands the SafeDOM stdlib bindings.
│
├── Documentation / README / CHANGELOG / branding
│   └─→ STANDALONE. Fleet mirrors at next sweep.
│
├── Governance: SPDX hooks, SHA-pin validators, security policy
│   └─→ STANDALONE. Fleet adopts the hook updates at next sweep.
│       (Fleet-level governance lives separately at gitbot-fleet/.github/.)
│
├── Deployment config (Containerfile base image, k8s, compose)
│   └─→ FLEET. The standalone Containerfile is a reference; the fleet
│       Containerfile is the deployed one.
│
└── New issue templates, .claude/ config, workflow files
    └─→ STANDALONE for repo-specific. Fleet uses gitbot-fleet/.github/
        and gitbot-fleet/.claude/ for fleet-wide.
```

### Quick reference

| You are doing... | PR target |
|---|---|
| Adding a Rust module under `src/` | **standalone** |
| Fixing a bug in `src/` shared by both | **standalone** |
| Adding a `.affine` example | **fleet** (this repo) |
| Updating production deploy config | **fleet** (this repo) |
| Promoting a fleet hotfix back to SDK | **standalone** (then close fleet hotfix) |
| Security patch bump on a transitive dep | **both** |
| Tagging a release | **standalone** |
| Rolling out a release to production | **fleet** (this repo) |

---

## See also

- Issue [`hyperpolymath/echidnabot#51`](https://github.com/hyperpolymath/echidnabot/issues/51) — diagnosis of the 109-file divergence.
- Memory note `feedback_echidna_license_docs_mpl_intentional` — docs stay MPL-2.0 despite AGPL `LICENSE`; do not reconcile.
- Memory note `feedback_echidna_src_abi_namespace_intentional` — `src/abi/` dual-tree layout is owner-managed.
