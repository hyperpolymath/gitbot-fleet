# Rhodibot — Sonnet Task Plan

## Context

Rhodibot is a Tier 1 (Verifier) bot in the gitbot-fleet ecosystem — the RSR (Rhodium Standard Repository) compliance enforcer. It runs as a GitHub App, validates repos against RSR requirements, creates check runs on PRs/pushes, and auto-creates RSR checklist issues for new repos.

**Current state**: ~85% functional. 1399 LOC Rust. Compiles with zero errors (7 dead code warnings). Core compliance checking works with 5 policy packs (Minimal/Standard/Strict/Enterprise/Custom). Scores repos on documentation, security, governance, structure, and language policy.

**Critical gaps**: Cargo.toml has AGPL license + wrong author. SCM files have AGPL headers. ZERO tests (despite test framework deps). No gitbot-shared-context fleet integration. STATE.scm is template stub. GitHub App JWT auth not implemented (uses token-only). Approved license list doesn't include PMPL.

---

## Task 1: Fix Metadata (CRITICAL)

### 1.1 Cargo.toml
- Line ~: Change `license = "AGPL-3.0-or-later"` → `license = "PMPL-1.0-or-later"`
- Line ~: Change `authors = ["hyperpolymath <hyperpolymath@users.noreply.github.com>"]` → `authors = ["Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"]`

### 1.2 SCM file license headers
All files in `.machine_readable/` (STATE.scm, META.scm, ECOSYSTEM.scm, AGENTIC.scm, PLAYBOOK.scm, NEUROSYM.scm):
- Change `AGPL-3.0-or-later` → `PMPL-1.0-or-later` in SPDX headers

### 1.3 Approved license list
**File**: `src/rsr.rs`
- Add `"pmpl-1.0"` and `"pmpl-1.0-or-later"` to the approved licenses list
- The current list is: agpl-3.0, apache-2.0, mit, mpl-2.0, lgpl-3.0
- PMPL is the primary license — it MUST be in the approved list

### Verification
- `grep -r "AGPL" .` returns nothing (except possibly in the license comparison list for backwards compatibility)
- `cargo check` compiles

---

## Task 2: Add Tests (CRITICAL)

Test framework is ready (tokio-test + wiremock in dev-deps) but ZERO tests exist.

### 2.1 RSR compliance engine tests (`src/rsr.rs`)
- Test: repo with all required files → high score, `required_passed = true`
- Test: repo missing README → lower score, specific check fails
- Test: repo missing LICENSE → `required_passed = false`
- Test: repo with banned files (go.mod, package-lock.json) → language policy findings
- Test: each policy pack (Minimal, Standard, Strict, Enterprise) → different severity levels
- Test: custom policy from `.rsr.toml` → overrides applied correctly
- Test: score calculation: percentage matches expected value

### 2.2 Webhook handler tests (`src/webhook.rs`)
- Test: valid HMAC-SHA256 signature → accepted
- Test: invalid signature → rejected with 401
- Test: missing signature header → rejected
- Test: push event to default branch → check run created
- Test: push event to non-default branch → ignored
- Test: PR opened → check run created
- Test: repository created → issue created with RSR checklist
- Test: ping event → 200 OK, no action

### 2.3 GitHub client tests (`src/github.rs`)
Using wiremock to mock GitHub API:
- Test: `file_exists()` → HEAD request, returns true/false
- Test: `get_file_content()` → returns file contents
- Test: `create_check_run()` → correct API call with markdown output
- Test: `create_issue()` → issue created with correct labels

### 2.4 Report formatting tests (`src/webhook.rs`)
- Test: report markdown output contains all categories
- Test: emoji indicators match pass/fail status
- Test: severity badges rendered correctly

### Verification
- `cargo test` — minimum 25 tests, all passing
- All tests use mocks (no real GitHub API calls)

---

## Task 3: Fleet Integration

### 3.1 Add gitbot-shared-context dependency
**File**: `Cargo.toml`
```toml
gitbot-shared-context = { path = "../gitbot-fleet/shared-context" }
```

### 3.2 Create fleet module
**New file**: `src/fleet.rs`

- Convert RSR compliance results → `Finding` structs
- Use `BotId::Rhodibot`
- Finding categories:
  - `"rsr/documentation"` — missing docs
  - `"rsr/security"` — missing SECURITY.md
  - `"rsr/governance"` — missing CODE_OF_CONDUCT/CONTRIBUTING
  - `"rsr/structure"` — missing SCM files
  - `"rsr/language-policy"` — banned language/tool detected
  - `"rsr/license"` — license issues
- Publish to shared context for other bots to consume
- Robot-repo-automaton can then auto-fix RSR issues

### 3.3 Wire into main.rs
- After compliance check: publish findings to fleet context
- Optional `--fleet-context <path>` flag for fleet mode
- Add `fleet` subcommand

### Verification
- `cargo check` compiles with fleet dependency
- Test: findings serialize to valid `Finding` structs
- Test: fleet context file written correctly

---

## Task 4: Resolve Dead Code Warnings

### 4.1 app_id and private_key in Config
Either:
- **Implement GitHub App JWT auth** (preferred): Use `app_id` and `private_key` to generate JWT tokens for GitHub App authentication instead of relying on `GITHUB_TOKEN`
- **Or**: Remove the fields and add TODO comment for future implementation

### 4.2 Unused Repository fields
- Some deserialized fields from GitHub API are unused
- Either use them in reporting or mark with `#[allow(dead_code)]` with a comment explaining they're from the API schema

### 4.3 get_contents() method
- Either use it (e.g., for listing `.github/workflows/`) or remove it

### Verification
- `cargo check` with zero warnings
- `cargo clippy` with zero warnings

---

## Task 5: Update STATE.scm

**File**: `.machine_readable/STATE.scm`

Currently a template stub (claims 0% / "initial" phase).

### 5.1 Populate with actual state
- version: from Cargo.toml
- phase: "production" or "v1.0-polishing"
- overall-completion: 85
- Components with percentages:
  - RSR compliance engine: 95% (9 file checks + language policy + scoring)
  - GitHub integration: 80% (webhooks + check runs, missing JWT auth)
  - Fleet integration: 0% → will be improved by Task 3
  - Multi-forge: 0% (GitHub only)
  - Testing: 0% → will be improved by Task 2

### 5.2 Update ECOSYSTEM.scm
- Position: Tier 1 Verifier
- Relationships: echidnabot (sibling verifier), sustainabot (sibling verifier), finishingbot/glambot/seambot (downstream consumers), robot-repo-automaton (executor of RSR fixes)

### Verification
- STATE.scm accurately reflects reality

---

## Task 6: Extend RSR Checks

### 6.1 Additional file checks
Add checks for files that RSR template requires but aren't currently checked:
- `.editorconfig` presence
- `.gitattributes` presence
- `.gitignore` presence
- `justfile` presence (primary build system)
- `.bot_directives/` directory presence

### 6.2 Workflow validation
Currently just checks if `.github/workflows/` exists. Expand to:
- Check for specific required workflows (hypatia-scan.yml, codeql.yml, scorecard.yml)
- Verify workflows use SHA-pinned actions (not `@v4` tags)
- Check for SPDX headers in workflow files

### 6.3 SPDX header validation
- Check that source files have SPDX-License-Identifier headers
- Check that SPDX identifier matches Cargo.toml/package.json license

### 6.4 Author attribution validation
- Check Cargo.toml author is not "hyperpolymath" (common mistake)
- Suggest correct author format

### Verification
- Tests cover new checks
- `cargo test` passes
- New checks produce correct findings on test repos

---

## Task 7: Security Hardening

### 7.1 Webhook signature
- Verify HMAC comparison uses constant-time comparison
- If using `==`, switch to `subtle::ConstantTimeEq` or `ring::constant_time`

### 7.2 Token handling
- Verify GITHUB_TOKEN is not logged
- Verify API responses with tokens are not logged verbatim

### 7.3 Input sanitization
- Repository names from webhooks: validate format
- File paths: prevent path traversal
- Markdown output: sanitize user-provided content in check run output

### Verification
- Security review checklist passed
- No secrets in log output
