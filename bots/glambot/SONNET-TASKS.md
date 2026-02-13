# Glambot — Sonnet Task Plan

## Context

Glambot is a Tier 2 (Finisher) bot in the gitbot-fleet ecosystem. It analyzes repos for "glamour" — visual presentation, accessibility, SEO, machine-readability, and git SEO integration. It's at ~20% completion with 5 analyzers (~1493 LOC Rust). All `fix()` methods are stubs.

**Critical issues**: Compilation bug in git_seo_integration.rs, STATE.scm claims wrong tech stack, no tests.

---

## Task 1: Fix Compilation Bug (CRITICAL)

**File**: `src/analyzers/git_seo_integration.rs`

### 1.1 Fix `f.code` → `f.id` reference
- Line ~102: The code references `f.code.starts_with("GS-")` but the `Finding` struct from `gitbot-shared-context` uses `f.id`, not `f.code`
- Change all `f.code` references to `f.id` in this file
- Search for any other files that reference `f.code` and fix them too

### Verification
- `cargo check` compiles with zero errors
- `grep -rn "\.code" src/` — verify no remaining references to a nonexistent `code` field

---

## Task 2: Fix Cargo.toml Metadata

**File**: `Cargo.toml`

### 2.1 Fix license
- If `license` field says `AGPL-3.0-or-later`, change to `PMPL-1.0-or-later`

### 2.2 Fix author
- Ensure authors = `["Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"]`
- NOT "Hyperpolymath" or "dev@hyperpolymath.org"

### 2.3 Fix SPDX headers
- Every `.rs` file must have:
  ```rust
  // SPDX-License-Identifier: PMPL-1.0-or-later
  // SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
  ```

### Verification
- `grep -r "AGPL" .` returns nothing
- `cargo check` still compiles

---

## Task 3: Update STATE.scm Tech Stack

**File**: `.machine_readable/STATE.scm` (or `.machine_readable/6scm/STATE.scm`)

### 3.1 Fix tech-stack
- STATE.scm currently claims the tech stack is "ReScript/Deno"
- The actual implementation is 100% Rust
- Update tech-stack to accurately reflect: `"Rust (gitbot-fleet workspace member)"`

### 3.2 Update completion
- Update completion percentage to match reality after these tasks are done
- Update the `updated` date

### 3.3 Add session history
- Add a new session entry documenting bug fixes and improvements

---

## Task 4: Implement fix() Methods

Currently all 5 analyzers have `fix()` methods that return stubs ("not yet implemented"). Implement at least the mechanical fixes:

### 4.1 Visual analyzer fixes (`src/analyzers/visual.rs`)
- Fix: Add missing README.adoc template if none exists
- Fix: Add badges section to README if missing
- Fix: Add screenshot placeholder if repo has UI components

### 4.2 Accessibility analyzer fixes (`src/analyzers/accessibility.rs`)
- Fix: Add alt text placeholders to images in markdown files
- Fix: Add language attribute to HTML files if missing
- Fix: Generate WCAG compliance checklist

### 4.3 SEO analyzer fixes (`src/analyzers/seo.rs`)
- Fix: Add repository description to GitHub API metadata
- Fix: Add topics/tags suggestion based on repo content
- Fix: Add Open Graph metadata template

### 4.4 Machine readability analyzer fixes (`src/analyzers/machine.rs`)
- Fix: Create missing `.machine_readable/` directory structure
- Fix: Generate template SCM files (STATE.scm, META.scm, ECOSYSTEM.scm)
- Fix: Add structured data (schema.org JSON-LD) template

### 4.5 Git SEO integration fixes (`src/analyzers/git_seo_integration.rs`)
- Fix: Optimize `.gitattributes` for search indexing
- Fix: Add `.github/` metadata files (FUNDING.yml, etc.)

### Verification
- Each fix() method does something concrete (not just returning a stub string)
- `cargo test` passes
- At least one fix per analyzer can be demonstrated on a test repo

---

## Task 5: Add Unit Tests

### 5.1 Per-analyzer tests
For each of the 5 analyzers, add:
- Test with a well-presented repo → no/minimal findings
- Test with a bare repo → multiple findings with correct severity
- Test that fix() produces valid output (not stub text)

### 5.2 Integration test
- Create a `tests/integration.rs` that:
  - Sets up a temporary directory with known deficiencies
  - Runs all 5 analyzers
  - Verifies correct number and types of findings
  - Verifies findings serialize to valid `Finding` structs

### Verification
- `cargo test` — all tests pass
- Minimum 15 tests (3 per analyzer)

---

## Task 6: Fleet Integration

### 6.1 Verify Finding builder usage
- Ensure `BotId::Glambot` is used
- Ensure all Finding fields are populated: id, severity, message, file, location, suggestion
- Ensure `.with_category("presentation")` or similar is set

### 6.2 Shared context publishing
- Verify findings can be written to shared-context JSON
- Test round-trip: create findings → serialize → deserialize → verify fields

### Verification
- `cargo check` with all dependencies
- Integration with gitbot-shared-context compiles and tests pass
