# Seambot — Sonnet Task Plan

## Context

Seambot is a Tier 2 (Finisher) bot in the gitbot-fleet ecosystem — an Architectural Seam Hygiene Auditor. It tracks, enforces, and detects drift in architectural boundaries ("seams"). It detects hidden channels (undeclared coupling), validates seam registers, tracks conformance examples, and validates stage freezes.

**Current state**: ~90% actual completion. 4337 LOC Rust. 21 tests passing. License and author are CORRECT. Core features all work. GitHub integration complete. STATE.scm is outdated (claims 0% / "initial" phase).

**Key gaps**: GitLab/Bitbucket forge clients are scaffolded but unused. STATE.scm not updated. Minor compiler warnings (unused imports, dead code from forge abstraction). One TODO for symbol extraction in fingerprints.

---

## Task 1: Update STATE.scm (CRITICAL)

**File**: `.machine_readable/STATE.scm` (or `.machine_readable/6scm/STATE.scm`)

The STATE.scm currently says 0% completion / "initial" phase, which is wildly inaccurate.

### 1.1 Update metadata
- version: reflect actual version from Cargo.toml
- updated: current date
- phase: "production-ready" or "v1.0-polishing"

### 1.2 Update completion
- overall-completion: 90 (not 0)
- List all working components with accurate percentages:
  - Hidden channels detection: 100% (5 channel types, multilingual)
  - Seam register validation: 100%
  - Drift detection: 100% (SHA256 fingerprinting)
  - Conformance validation: 100%
  - Freeze stamp validation: 100%
  - GitHub integration: 95% (Checks API, JWT auth, webhooks)
  - GitLab integration: 20% (scaffolded, not wired)
  - Bitbucket integration: 20% (scaffolded, not wired)
  - Fleet integration: 100%
  - SARIF output: 100%
  - CLI: 100% (12 subcommands)

### 1.3 Update tech-stack
- Primary: "Rust (2021 edition)"
- Key dependencies: gitbot-shared-context, reqwest, serde, sha2, clap

### 1.4 Add session history
- Document the state of the project accurately

### Verification
- STATE.scm accurately reflects reality

---

## Task 2: Wire Multi-Forge Abstraction

**Files**: `src/main.rs`, `src/forge/mod.rs`, `src/forge/gitlab.rs`, `src/forge/bitbucket.rs`

### 2.1 Wire ForgeClient trait into CLI
- The `ForgeClient` trait exists in `src/forge/mod.rs` but is unused
- `main.rs` directly imports GitHub client
- Refactor `main.rs` to use `ForgeClient` trait object
- Select forge implementation based on CLI flag or environment variable:
  - `--forge github` (default)
  - `--forge gitlab`
  - `--forge bitbucket`
  - Auto-detect from git remote URL

### 2.2 Complete GitLab forge client
- `src/forge/gitlab.rs` is scaffolded
- Implement GitLab Merge Request Notes API for posting comments
- Implement GitLab Pipeline status reporting
- Use GitLab personal access token or CI job token for auth

### 2.3 Complete Bitbucket forge client
- `src/forge/bitbucket.rs` is scaffolded
- Implement Bitbucket PR comments API
- Implement Bitbucket Pipeline status reporting
- Use Bitbucket App passwords for auth

### Verification
- `cargo check` compiles with no unused code warnings for forge modules
- Test: create GitLab check result (mock API)
- Test: create Bitbucket check result (mock API)

---

## Task 3: Fix Compiler Warnings

### 3.1 Unused imports
- Remove or use `warn` import in `checks.rs` line 9
- Remove or use `Conclusion` import in `forge/github.rs` line 6
- Remove or use `std::io::Write` import in `report.rs`

### 3.2 Dead code
- Either wire the ForgeClient trait (Task 2) or prefix unused items with `_`
- If Task 2 is done, the dead code warnings should resolve naturally

### 3.3 Unused variable
- `_suspicious_patterns` in `checks.rs` line 305 — either use it or remove it

### 3.4 TODO marker
- `checks.rs` line 446: `// TODO: extract symbols` — implement symbol extraction for fingerprints
- Extract function/method/type signatures from seam interface files
- Hash the extracted symbols as part of the fingerprint

### Verification
- `cargo check` with zero warnings
- `cargo clippy` with zero warnings

---

## Task 4: Expand Test Coverage

### 4.1 Hidden channels tests
Currently no dedicated tests for the hidden channels module (816 LOC, most complex module).

Add tests for each of the 5 channel types:
- **Undeclared imports**: Test with Rust/JS/Python files importing across seam boundaries
- **Global state**: Test with `static mut`, `lazy_static`, `Arc<Mutex<>>` patterns
- **Filesystem coupling**: Test with multiple seams referencing same file paths
- **Database coupling**: Test with SQL table references, ORM patterns, shared DB env vars
- **Network coupling**: Test with HTTP client patterns, gRPC definitions, WebSocket patterns

### 4.2 Drift detection tests
- Test: unchanged seam → no drift detected
- Test: modified seam interface → drift detected with correct diff
- Test: frozen seam with changes → freeze violation reported

### 4.3 Report format tests
- Test: SARIF output is valid JSON and follows SARIF 2.1.0 schema
- Test: Markdown output contains all findings
- Test: JSON output round-trips correctly

### Verification
- `cargo test` — minimum 35 tests (adding 14+ to existing 21)
- All tests pass

---

## Task 5: Security Hardening

### 5.1 Webhook signature verification hardening
- Verify HMAC-SHA256 webhook signatures use constant-time comparison
- If using `==` for signature comparison, switch to `subtle::ConstantTimeEq` or `ring::constant_time::verify_slices_are_equal`
- This prevents timing attacks on webhook signatures

### 5.2 JWT token handling
- Verify JWT tokens are not logged (check all log statements)
- Verify JWT tokens have appropriate expiry (10 minutes max for GitHub App)
- Verify private keys are not embedded in binary or logged

### 5.3 Input validation
- Seam register files: validate JSON schema before processing
- File paths in seam definitions: prevent path traversal (`../../../etc/passwd`)
- Git operations: sanitize branch names and file paths

### Verification
- Security review checklist passed
- No secrets in logs (test by grepping log output)
- Path traversal attempt → rejected with error

---

## Task 6: ECOSYSTEM.scm and META.scm Updates

### 6.1 ECOSYSTEM.scm
- Fill in position-in-ecosystem (Tier 2 Finisher)
- Document relationships:
  - Sibling: glambot, finishingbot (other Tier 2 bots)
  - Consumer: gitbot-fleet (orchestration), echidnabot (verification)
  - Provider: robot-repo-automaton (consumes seambot findings)

### 6.2 META.scm
- Document architecture decisions
- Document seam-first design philosophy
- Document hidden channel detection categories
