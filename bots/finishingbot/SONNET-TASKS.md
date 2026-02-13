# Finishingbot — Sonnet Task Plan

## Context

Finishingbot is a Tier 2 (Finisher) bot in the gitbot-fleet ecosystem. It analyzes repos for "unfinished" work: missing licenses, placeholder text, incomplete releases, missing SCM files, inadequate testing, missing tooling, and v1 readiness. It's at ~92% completion with 8 working analyzers (~4391 LOC Rust).

**Current state**: Core analysis works. Needs metadata fixes, test coverage, and integration polish.

---

## Task 1: Fix Cargo.toml Metadata (CRITICAL)

**File**: `Cargo.toml`

### 1.1 Fix license
- Line 8: Change `license = "AGPL-3.0-or-later"` → `license = "PMPL-1.0-or-later"`

### 1.2 Fix author
- Line 6: Change `authors = ["Hyperpolymath <dev@hyperpolymath.org>"]` → `authors = ["Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"]`

### 1.3 Fix SPDX headers
- Audit ALL `.rs` files in `src/` for SPDX headers
- Every file must have:
  ```rust
  // SPDX-License-Identifier: PMPL-1.0-or-later
  // SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
  ```
- If any file has `AGPL-3.0-or-later`, change to `PMPL-1.0-or-later`

### Verification
- `cargo check` compiles
- `grep -r "AGPL" src/` returns nothing

---

## Task 2: Add Unit Tests for All 8 Analyzers

**Directory**: `tests/` or inline `#[cfg(test)]` modules

Each analyzer needs at minimum:
1. Test with a "clean" repo mock (all checks pass)
2. Test with a "dirty" repo mock (specific issues detected)
3. Test that findings have correct severity levels

### 2.1 License analyzer tests (`src/analyzers/license.rs`)
- Test: repo with valid LICENSE file → no findings
- Test: repo with no LICENSE file → finding with severity Error
- Test: repo with AGPL license → finding suggesting PMPL migration

### 2.2 Placeholder analyzer tests (`src/analyzers/placeholder.rs`)
- Test: files with "TODO", "FIXME", "XXX", "PLACEHOLDER" → findings
- Test: clean files → no findings
- Test: placeholder in different file types (.rs, .md, .toml)

### 2.3 Claims analyzer tests (`src/analyzers/claims.rs`)
- Test: STATE.scm claiming 100% but missing features → finding
- Test: README claiming features that don't exist → finding
- Test: consistent claims → no findings

### 2.4 Release analyzer tests (`src/analyzers/release.rs`)
- Test: missing CHANGELOG → finding
- Test: version mismatch between Cargo.toml and STATE.scm → finding
- Test: proper release setup → no findings

### 2.5 SCM files analyzer tests (`src/analyzers/scm_files.rs`)
- Test: missing STATE.scm → finding
- Test: STATE.scm in root (wrong location) → finding suggesting `.machine_readable/`
- Test: all SCM files present in `.machine_readable/` → no findings

### 2.6 Testing analyzer tests (`src/analyzers/testing.rs`)
- Test: repo with 0 test files → finding with severity Error
- Test: repo with tests but low coverage indicators → finding with severity Warning
- Test: well-tested repo → no findings or Note-level only

### 2.7 Tooling analyzer tests (`src/analyzers/tooling.rs`)
- Test: missing .editorconfig → finding
- Test: missing CI workflows → finding
- Test: complete tooling → no findings

### 2.8 V1 readiness analyzer tests (`src/analyzers/v1_readiness.rs`)
- Test: repo meeting all v1 criteria → pass
- Test: repo missing critical items → finding with severity Error

### Verification
- `cargo test` — all new tests pass
- Aim for ≥80% line coverage across analyzers

---

## Task 3: Update STATE.scm

**File**: `.machine_readable/STATE.scm` (or `.machine_readable/6scm/STATE.scm`)

- Update test coverage from "0%" to actual percentage after Task 2
- Ensure tech-stack says "Rust" (not anything else)
- Update `updated` date
- Verify completion percentages match reality
- Add session history entry for this work

---

## Task 4: Fleet Integration Verification

**File**: `src/fleet.rs` or equivalent

- Verify `Finding` builder usage matches current `gitbot-shared-context` API
- Ensure `BotId::Finishingbot` is used (not a string literal)
- Test that findings serialize correctly to shared context JSON
- Add integration test: run analysis on a test repo, verify findings are valid `Finding` structs

### Verification
- `cargo check` with gitbot-shared-context dependency
- Integration test passes

---

## Task 5: Self-Analysis (Dogfooding)

Run finishingbot's own analyzers against itself:
- Does it have a valid LICENSE? (should after Task 1)
- Any placeholders left?
- Do its own claims match reality?
- Is it release-ready?

Fix any issues found by its own analysis.

### Verification
- Finishingbot can analyze itself with no Error-level findings
