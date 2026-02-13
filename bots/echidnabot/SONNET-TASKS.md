# Echidnabot — Sonnet Task Plan

## Context

Echidnabot is a Tier 1 (Verifier) bot in the gitbot-fleet ecosystem. It acts as the bridge between the ECHIDNA neurosymbolic theorem proving platform and the gitbot-fleet orchestration layer. It receives webhook events, dispatches verification requests to ECHIDNA, and reports findings back to the fleet.

**Current state**: ~65-70% actual completion (claims 75%). Core infrastructure complete (Axum server, webhooks, database, GraphQL, ECHIDNA HTTP client). Critical gaps: container isolation is EMPTY, bot modes not wired into handlers, retry logic not integrated, ZERO automated tests.

---

## Task 1: Implement Container Isolation (CRITICAL SECURITY)

**File**: `src/executor/container.rs`

This file is EMPTY. Proofs currently run without any isolation — a malicious proof could execute arbitrary code on the host.

### 1.1 Implement PodmanExecutor
```rust
pub struct PodmanExecutor {
    image: String,
    timeout: Duration,
    memory_limit: String,
    network: bool, // should be false for proof checking
}
```

### 1.2 Core isolation features
- Run proof-checking in Podman containers (rootless)
- No network access (`--network=none`)
- Memory limit (`--memory=512m` default, configurable)
- CPU limit (`--cpus=2` default, configurable)
- Timeout with SIGKILL (`--timeout`)
- Read-only filesystem except `/tmp` for proof artifacts
- Drop ALL capabilities (`--cap-drop=ALL`)
- No new privileges (`--security-opt=no-new-privileges`)

### 1.3 Input/output handling
- Mount proof files as read-only volume
- Capture stdout/stderr for proof results
- Parse exit code: 0 = verified, non-zero = failed/timeout
- Clean up containers after completion

### 1.4 Fallback for systems without Podman
- Check if Podman is available at startup
- If not: log warning, use `bubblewrap` (bwrap) as lighter alternative
- If neither: refuse to run proofs (fail-safe, not fail-open)

### Verification
- Unit test: PodmanExecutor creates correct command line args
- Integration test: run a trivial proof in container, verify result
- Test: malicious proof attempt (e.g., `rm -rf /`) is contained
- Test: timeout kills container after configured duration

---

## Task 2: Wire Bot Modes into Webhook Handlers

**Files**: `src/webhook/` handlers, `src/bot/modes.rs` or equivalent

Bot modes are defined (Verifier, Advisor, Consultant, Regulator) but NOT connected to the webhook handlers.

### 2.1 Mode selection logic
- Read bot mode from `.bot_directives/echidnabot.scm` in the target repo
- Default to `Verifier` mode if no directive found
- Mode determines:
  - **Verifier**: Full proof checking, block PR on failure
  - **Advisor**: Check proofs, comment results, don't block
  - **Consultant**: Only analyze when explicitly requested (@echidnabot check)
  - **Regulator**: Enforce minimum proof coverage thresholds

### 2.2 Wire into PR webhook handler
- On PR open/update: determine mode → dispatch appropriate action
- Verifier/Advisor: automatically trigger proof checking
- Consultant: only respond to explicit mentions
- Regulator: check proof coverage metrics

### 2.3 Wire into push webhook handler
- On push to main: determine mode → dispatch appropriate action
- All modes: update proof status dashboard

### Verification
- Test: webhook with Verifier mode triggers proof checking
- Test: webhook with Consultant mode does NOT auto-trigger
- Test: missing directive defaults to Verifier

---

## Task 3: Integrate Retry Logic

**Files**: `src/scheduler/` or `src/executor/`

Retry logic is defined somewhere in the codebase but NOT integrated into the actual execution pipeline.

### 3.1 Find and wire retry logic
- Locate the retry/backoff implementation
- Wire it into the proof execution pipeline:
  - Container startup failure → retry with backoff
  - ECHIDNA API timeout → retry up to 3 times
  - Transient network errors → retry with exponential backoff
  - Proof timeout → do NOT retry (intentional, resource-saving)

### 3.2 Circuit breaker
- If ECHIDNA API fails 5 consecutive times → circuit breaker opens
- Log error, notify fleet coordinator
- Auto-reset after 5 minutes

### Verification
- Test: transient failure retries and succeeds on second attempt
- Test: permanent failure stops after max retries
- Test: circuit breaker opens after consecutive failures

---

## Task 4: Add Automated Tests (CRITICAL)

The repo has ZERO tests despite importing test libraries.

### 4.1 Unit tests for ECHIDNA client
- Test: HTTP client constructs correct API requests
- Test: response parsing handles success case
- Test: response parsing handles error case
- Test: timeout handling

### 4.2 Unit tests for webhook verification
- Test: valid HMAC-SHA256 signature passes
- Test: invalid signature is rejected
- Test: missing signature header is rejected

### 4.3 Unit tests for GraphQL API
- Test: query resolves proof status
- Test: mutation triggers proof check
- Test: authentication required for mutations

### 4.4 Unit tests for database models
- Test: proof result CRUD operations
- Test: concurrent access handling

### 4.5 Integration test
- Test: full webhook → dispatch → (mock) ECHIDNA → finding → fleet context flow
- Use mock ECHIDNA server (axum test server)

### Verification
- `cargo test` — minimum 20 tests, all passing
- No test requires actual ECHIDNA instance (use mocks)

---

## Task 5: Fix Metadata

### 5.1 Cargo.toml
- License: must be `PMPL-1.0-or-later` (not AGPL)
- Author: must be `"Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"`

### 5.2 SPDX headers
- Every `.rs` file needs:
  ```rust
  // SPDX-License-Identifier: PMPL-1.0-or-later
  // SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
  ```

### 5.3 STATE.scm update
- Update completion to actual percentage
- Fix tech-stack if inaccurate
- Add session history entry

### Verification
- `grep -r "AGPL" .` returns nothing
- All `.rs` files have SPDX headers

---

## Task 6: ECHIDNA Trust Bridge

Connect echidnabot to echidna's trust verification mechanisms.

### 6.1 Proof confidence reporting
- When ECHIDNA returns a proof result, include the confidence level in the Finding:
  - Level 5: Cross-checked by 2+ independent small-kernel systems
  - Level 4: Checked by small-kernel system (Lean4, Coq, Isabelle) with certificate
  - Level 3: Single prover with proof certificate (Alethe, DRAT/LRAT)
  - Level 2: Single prover result without certificate
  - Level 1: Large-TCB system or unchecked result
- Map confidence to Finding severity and metadata

### 6.2 Solver integrity verification
- Before dispatching to ECHIDNA, verify that the solver binaries haven't been tampered with
- Check SHA256 manifest (see echidna SONNET-TASKS.md Task 2)
- Report integrity status in Finding metadata

### 6.3 Axiom usage tracking
- Parse ECHIDNA proof results for axiom usage
- Flag proofs using `sorry`, `Admitted`, `postulate`, `choice`, `--type-in-type`
- Report axiom reliance as separate Finding with Warning severity

### Verification
- Test: confidence level correctly mapped for each prover type
- Test: axiom usage detected and reported
- Test: solver integrity check included in results
