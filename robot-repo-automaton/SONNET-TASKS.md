# Robot-Repo-Automaton — Sonnet Task Plan

## Context

Robot-repo-automaton is the Tier 3 (Executor) bot in the gitbot-fleet ecosystem. It's the only bot that actually MODIFIES code — it takes findings from Tier 1 (Verifiers) and Tier 2 (Finishers) and applies automated fixes. It parses ERROR-CATALOG.scm for known fix patterns, detects issues, and applies corrections.

**Current state**: ~40-50% actual completion (claims 70%). S-expression parser works, language detection works, delete fixes work. 3 compilation errors in fleet.rs. Modify and create fix types are stubs. Hypatia integration not implemented.

---

## Task 1: Fix Compilation Errors in fleet.rs (CRITICAL)

**File**: `src/fleet.rs`

### 1.1 Fix gitbot-shared-context API mismatch
There are 3 compilation errors where the code calls `ctx.findings(BotId::RobotRepoAutomaton)` or similar — the API has changed in gitbot-shared-context.

Steps:
1. Read `src/fleet.rs` to identify the exact error lines
2. Read the gitbot-shared-context crate API (check `/var/mnt/eclipse/repos/gitbot-fleet/crates/gitbot-shared-context/src/lib.rs` or the public API)
3. Fix the API calls to match the current gitbot-shared-context interface
4. Common fixes:
   - `ctx.findings(bot_id)` may need to be `ctx.get_findings(bot_id)` or similar
   - Finding builder pattern may have changed
   - BotId enum import path may have changed

### 1.2 Verify dependency versions
- Check that `Cargo.toml` points to the correct version/path of `gitbot-shared-context`
- Ensure the workspace dependency resolution works

### Verification
- `cargo check` compiles with ZERO errors
- `cargo test` — existing 10 tests still pass

---

## Task 2: Implement Modify Fix Application

**File**: `src/fixer.rs` or `src/fixes/modify.rs` (find the stub)

Currently only "delete" fix type works. "Modify" is a stub.

### 2.1 Implement line-level modifications
- Parse the fix specification from ERROR-CATALOG.scm
- Apply sed-like transformations to specific lines
- Support:
  - Replace line content
  - Insert before/after a line
  - Replace regex pattern within a line

### 2.2 Safety checks
- Before modifying: snapshot the file (in-memory copy)
- After modifying: verify the file still parses (for known languages)
- If modification breaks parsing: rollback and report failure
- Never modify binary files

### 2.3 Git integration
- Stage modified files with `git add`
- Create fix commit with descriptive message
- Commit message format: `fix(<analyzer>): <description> [robot-repo-automaton]`

### Verification
- Test: modify a specific line in a test file → line changed correctly
- Test: modify with regex pattern → pattern replaced
- Test: invalid modification → rollback, no change to file
- Test: binary file → skipped with warning

---

## Task 3: Implement Create Fix Application

**File**: `src/fixer.rs` or `src/fixes/create.rs` (find the stub)

### 3.1 Implement file creation
- Create new files as specified by fix patterns
- Support template expansion (variables like `{{REPO}}`, `{{OWNER}}`, `{{LICENSE}}`)
- Common creation targets:
  - Missing LICENSE files
  - Missing .editorconfig
  - Missing SECURITY.md
  - Missing .machine_readable/ SCM files

### 3.2 Directory creation
- Create parent directories as needed (`mkdir -p` equivalent)
- Respect `.gitignore` — don't create files that would be ignored

### 3.3 Template source
- Templates embedded in the binary (include_str!)
- Or loaded from a templates directory
- RSR template repo as reference for standard file content

### Verification
- Test: create a missing LICENSE file → file exists with correct content
- Test: create file in new directory → directory created automatically
- Test: create file that would be gitignored → warning, not created

---

## Task 4: Confidence Threshold System

**File**: `src/confidence.rs` or `src/fixer.rs`

Robot-repo-automaton should NOT blindly apply every fix. It needs confidence thresholds.

### 4.1 Confidence levels for fix types
- **High confidence** (auto-apply): license header addition, SPDX header insertion, .editorconfig creation
- **Medium confidence** (apply with review): line modifications matching known patterns
- **Low confidence** (propose only): complex multi-file changes, refactoring

### 4.2 Threshold configuration
- Read thresholds from `.bot_directives/robot-repo-automaton.scm`
- Default: only apply high-confidence fixes automatically
- Allow repos to configure: `(auto-apply-threshold . "medium")` or `(auto-apply-threshold . "high")`

### 4.3 Proposal mode for low-confidence fixes
- Instead of applying: create a Finding describing the proposed fix
- Include the exact diff that WOULD be applied
- Let human review before application

### Verification
- Test: high-confidence fix auto-applies
- Test: low-confidence fix creates proposal Finding, does NOT modify files
- Test: custom threshold from directive is respected

---

## Task 5: Fix Metadata

### 5.1 Cargo.toml
- License: `PMPL-1.0-or-later`
- Author: `"Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"`

### 5.2 SPDX headers on all `.rs` files

### 5.3 STATE.scm
- Update completion to actual percentage
- Update session history

### Verification
- `grep -r "AGPL" .` returns nothing

---

## Task 6: Add Tests

### 6.1 Fix application tests
- Test: delete fix removes specified file
- Test: modify fix changes specified line
- Test: create fix creates specified file
- Test: rollback on failure

### 6.2 ERROR-CATALOG.scm parsing tests
- Test: parse a catalog entry → correct fix type, pattern, template
- Test: malformed entry → graceful error
- Test: empty catalog → no fixes available

### 6.3 Integration test
- Set up a test repo with known issues
- Run robot-repo-automaton against it
- Verify: correct fixes applied, correct commits created
- Verify: low-confidence fixes proposed, not applied

### Verification
- `cargo test` — minimum 20 tests (adding 10+ to existing 10)
- All tests pass
