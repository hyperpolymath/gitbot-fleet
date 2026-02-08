;; SPDX-License-Identifier: PMPL-1.0-or-later

# Gitbot Fleet: Bot Operations Guide

**Author**: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
**Date**: 2026-02-05
**Version**: 0.2.0

---

## 1. Fleet Architecture

The gitbot-fleet uses a **four-tier execution model** where bots execute in dependency order. Each tier must complete before the next tier begins.

```
Tier 0: ENGINE
  └── hypatia (central rules engine, coordinates everything)
        │
        ▼
Tier 1: VERIFIERS (run in parallel, produce findings)
  ├── rhodibot     (RSR structural compliance)
  ├── echidnabot   (formal verification & fuzzing)
  └── sustainabot  (ecological/economic analysis)
        │
        ▼ verifiers_complete() gate
        │
Tier 2: FINISHERS (consume findings, produce results)
  ├── glambot      (presentation quality)  [depends: rhodibot]
  ├── seambot      (architecture health)   [depends: rhodibot, echidnabot]
  └── finishbot    (release readiness)     [depends: rhodibot, glambot]
        │
        ▼
Tier 3: EXECUTOR
  └── robot-repo-automaton (applies approved fixes)
```

**Source**: `shared-context/src/bot.rs` defines `BotId`, `Tier`, and `BotInfo` with dependency chains.

---

## 2. Execution Pipeline

### How Bots Coordinate via Shared Context

The shared-context Rust library (`shared-context/`) provides the coordination layer:

```
1. Context::new(repo_name)          -- Create session
2. context.register_all_bots()      -- Register all fleet members
3. context.start_bot(BotId)         -- Mark bot as running
4. context.add_finding(finding)     -- Bot publishes findings
5. context.complete_bot(BotId)      -- Bot finishes
6. context.verifiers_complete()     -- Gate: all Tier 1 done?
7. context.findings_for_tier(tier)  -- Tier 2 reads Tier 1 findings
8. context.summary()                -- Generate ContextSummary
```

**Storage**: Sessions persist as JSON in `~/.gitbot-fleet/sessions/`.

### Fleet Coordinator Script

`fleet-coordinator.sh` (652 lines) orchestrates the full pipeline:

| Command | Purpose |
|---------|---------|
| `run-scan <repo>` | Execute hypatia scanner on a repository |
| `process-findings` | Process `findings/<repo>/latest.json`, dispatch auto-fixes, trigger learning |
| `generate-rules` | Analyze pattern frequency, propose Logtalk rules at threshold (5+ observations) |
| `deploy-bots` | Generate deployment status JSON |
| `status` | Show deployed bots with tier and status |

### Learning Loop

```
Findings → observed-patterns.jsonl → threshold (5+) → propose_new_rule()
  → Logtalk rule in learning/rule-proposals/ → create_rule_approval_pr()
  → Human reviews PR → Approved rule deployed to hypatia
```

---

## 3. Per-Bot Reference Cards

### 3.1 rhodibot - RSR Compliance Validator

| Field | Value |
|-------|-------|
| **Purpose** | Validate RSR (Repository Structure Requirements) compliance |
| **Repo** | `hyperpolymath/rhodibot` |
| **Path** | `/mnt/eclipse/repos/rhodibot/` |
| **Tech** | Rust 1.83+, Tokio, Axum, gix |
| **Tier** | 1 (Verifier) |
| **Completion** | **50%** |
| **Dependencies** | None (runs first) |

**What Works**:
- Required files checker (README.adoc, LICENSE, SECURITY.md, CODE_OF_CONDUCT.md, CONTRIBUTING.md)
- SCM file validator (STATE.scm, META.scm, ECOSYSTEM.scm structure)
- Directory layout checker (RSR structure)
- Language policy enforcer (detects banned: TypeScript, Python, Go, npm, Bun)
- CCCP banned pattern detection (no npm lock files, yarn.lock, go.mod)
- Webhook receiver endpoint
- Manual compliance check API: `GET /api/check/{owner}/{repo}`

**What's Missing**:
- GitHub App webhook handlers (40% done)
- Auto-issue creation for compliance checklists
- CII badge automation
- Fleet shared-context integration

**CLI**: `rhodibot check <path>` / `rhodibot serve` (webhook server)

---

### 3.2 echidnabot - Formal Verification Bot

| Field | Value |
|-------|-------|
| **Purpose** | Formal mathematical/statistical verification via ECHIDNA |
| **Repo** | `hyperpolymath/echidnabot` |
| **Path** | `/mnt/eclipse/repos/echidnabot/` |
| **Tech** | Rust |
| **Tier** | 1 (Verifier) |
| **Completion** | **75%** |
| **Dependencies** | None |

**What Works**:
- GitHub App integration
- 12 prover backend integration via ECHIDNA core
- Finding submission to fleet
- Basic verification workflow

**What's Missing**:
- Container isolation for prover execution (security)
- Retry logic with exponential backoff
- Concurrent job limits
- Integration tests
- Full ABI/FFI (Idris2 + Zig)

**Relationship to ECHIDNA**: echidnabot wraps the ECHIDNA theorem prover (`/mnt/eclipse/repos/echidna/`) for GitHub integration. ECHIDNA provides the 12 prover backends; echidnabot provides the bot lifecycle, webhook handling, and fleet coordination.

---

### 3.3 sustainabot - Ecological & Economic Analyzer

| Field | Value |
|-------|-------|
| **Purpose** | Analyze code for ecological carbon intensity and economic efficiency |
| **Repo** | `hyperpolymath/sustainabot` |
| **Path** | `/mnt/eclipse/repos/sustainabot/` |
| **Tech** | Rust (4-crate workspace), tree-sitter |
| **Tier** | 1 (Verifier) |
| **Completion** | **25%** |
| **Dependencies** | None |

**What Works (Phase 1)**:
- AST-based code analysis with tree-sitter (Rust + JavaScript)
- Function detection and cyclomatic complexity estimation
- Resource metric calculation (Energy, Carbon, Duration, Memory as newtypes)
- Health index computation: `0.4*EcoScore + 0.3*EconScore + 0.3*QualityScore`
- CLI: `sustainabot analyze <file>`, `sustainabot self-analyze` (dogfooding)
- CLI: `sustainabot check <dir>` (recursive directory analysis)
- JSON output support

**What's Missing**:
- Phase 2: Bot integration (GitHub/GitLab webhooks, PR comments) = 5% skeleton
- Phase 3: Policy engine (Eclexia/Datalog rules, DeepProbLog) = 20% stub
- Haskell AST analyzer (design only)
- OCaml documentation analyzer (design only)
- ArangoDB + Virtuoso databases (design only)
- Carbon API integration (ElectricityMaps/WattTime) - placeholder only

**Architecture**: Polyglot by design (Haskell/OCaml/Rust/ReScript) but only Rust is implemented. See `ARCHITECTURE.md` (31KB) for full design.

**CLI**: `sustainabot analyze <file>` / `sustainabot check <dir>` / `sustainabot self-analyze`

---

### 3.4 glambot - Presentation Quality Enforcer

| Field | Value |
|-------|-------|
| **Purpose** | Visual polish, WCAG accessibility, SEO, machine-readability |
| **Repo** | `hyperpolymath/glambot` |
| **Path** | `/mnt/eclipse/repos/glambot/` |
| **Tech** | Rust 1.83+, pulldown-cmark, scraper, HTML5ever |
| **Tier** | 2 (Finisher) |
| **Completion** | **60%** |
| **Dependencies** | rhodibot |

**What Works (4 Analyzers)**:
1. **Visual Polish** - README formatting, badges, logos
2. **Accessibility (WCAG 2.1 AA)** - Alt-text, heading hierarchy, link text
3. **SEO** - Meta tags, OpenGraph, repository description
4. **Machine-Readability** - JSON/YAML validation, structured data, robots.txt

**What's Missing**:
- Auto-fix capability (returns placeholder)
- Fuzzing infrastructure
- Stress tests
- Integration tests

**CLI**: `glambot check <path>` with `--format text|json|markdown|sarif`

---

### 3.5 seambot - Architectural Seam Auditor

| Field | Value |
|-------|-------|
| **Purpose** | Track, enforce, and detect drift in architectural boundaries |
| **Repo** | `hyperpolymath/seambot` |
| **Path** | `/mnt/eclipse/repos/seambot/` |
| **Tech** | Rust 1.83+, walkdir, regex, tree-sitter |
| **Tier** | 2 (Finisher) |
| **Completion** | **55%** |
| **Dependencies** | rhodibot, echidnabot |

**What Works (3,067 lines)**:
- Seam register parsing and validation
- Conformance checking (file existence)
- Drift detection (baseline comparison with SHA256)
- Hidden channels: undeclared imports, global state, filesystem coupling, database coupling, network coupling
- GitHub App integration (JWT, check runs, PR comments, webhook verification)
- Multi-format output (text, JSON, Markdown, SARIF)
- 8 CLI commands + 3 GitHub subcommands

**CLI Commands**:
1. `seambot check` - Run all checks
2. `seambot register` - Verify seam register
3. `seambot drift` - Detect interface changes
4. `seambot conformance` - Validate examples
5. `seambot report` - Generate summary
6. `seambot init` - Create seam infrastructure
7. `seambot freeze-check` - Validate freeze stamps
8. `seambot hidden-channels` - Detect coupling
9. `seambot github check-run` / `pr-comment` / `verify-webhook`

**Seam Types**: module, service, layer, data, api, build, cross_repo

---

### 3.6 finishbot - Release Readiness Validator

| Field | Value |
|-------|-------|
| **Purpose** | Gate releases by validating completeness and quality |
| **Repo** | `hyperpolymath/finishingbot` |
| **Path** | `/mnt/eclipse/repos/finishingbot/` |
| **Tech** | Rust 1.83+, git2, pulldown-cmark |
| **Tier** | 2 (Finisher - LAST in pipeline) |
| **Completion** | **65%** |
| **Dependencies** | rhodibot, glambot |

**What Works (8 Analyzers)**:
1. **License** - SPDX headers, allowed licenses (PMPL, MIT, Apache)
2. **Placeholder** - TODO/FIXME/XXX/WIP detection
3. **Claims** - README accuracy, test coverage claims
4. **Release** - SHA256/SHA512 hashes, GPG signatures, semver
5. **SCM Files** - Validates all 6 required .scm files
6. **Testing** - Benchmarks, fuzzing, stress test presence
7. **Tooling** - .tool-versions, .editorconfig, CI workflows
8. **V1 Readiness** - Bans TypeScript/Python/Go, validates core docs

**What's Missing**:
- Fleet integration (60%) - needs gitbot-shared-context path resolution
- Auto-fix beyond license/placeholder
- Integration tests

**CLI**: `finishbot check <path>` with `--format text|json|markdown|sarif`

---

### 3.7 hypatia - Central Rules Engine

| Field | Value |
|-------|-------|
| **Purpose** | Neurosymbolic CI/CD intelligence platform |
| **Repo** | `hyperpolymath/hypatia` |
| **Path** | `/mnt/eclipse/repos/hypatia/` |
| **Tech** | Rust + Haskell + Logtalk/Prolog + Ada/SPARK |
| **Tier** | 0 (Engine) |
| **Completion** | **70%** |
| **Dependencies** | None (coordinates everything) |

**Components**:
- **CLI** (Rust) - 7 commands: scan, deposit, withdraw, search, batch, fleet, hooks
- **Registry** (Haskell) - Type-safe ruleset DSL, property-based testing
- **Engine** (Logtalk) - Declarative rule execution, distillation
- **Adapters** (Rust) - GitHub, GitLab, Bitbucket, Codeberg (SourceHut/Radicle pending)
- **Data** (Rust) - ArangoDB graph DB, Dragonfly cache
- **Fixer** (Rust) - Error catalog parser, issue detection, fix application
- **TUI** (Ada/SPARK) - Terminal UI with formal verification
- **Hooks** - pre-commit (language policy), pre-push (signing), post-receive (enforcement)
- **Deploy** - Kubernetes manifests, Helm charts, Terraform, Docker Compose, Prometheus/Grafana

**What's Missing**:
- Liquid Haskell integration
- SourceHut + Radicle adapters
- Public registry launch
- Enterprise features

---

### 3.8 robot-repo-automaton - Action Executor

| Field | Value |
|-------|-------|
| **Purpose** | Execute fixes identified by fleet bots |
| **Repo** | `hyperpolymath/robot-repo-automaton` |
| **Path** | `/mnt/eclipse/repos/robot-repo-automaton/` |
| **Tech** | Rust 1.83+, Tokio, Axum, lexpr |
| **Tier** | 3 (Executor) |
| **Completion** | **~5%** |
| **Dependencies** | All fleet bots (consumes their findings) |

**What Exists**:
- Catalog parser for ERROR-CATALOG.scm
- Detector for compliance issues
- Fixer module (dry-run and commit modes)
- Hook manager (install/remove git hooks)
- Hypatia integration stub

**What's Missing**:
- Most implementation is nascent/template
- STATE.scm is template only
- No tests
- Fleet integration incomplete

---

## 4. Deployment Configuration

### deploy-bot-fleet.k9.ncl (Nickel)

Bot execution priority (1 = highest):
1. rhodibot (structural compliance)
2. echidnabot (formal verification)
3. sustainabot (eco/economic)
4. glambot (presentation)
5. seambot (architecture)
6. finishbot (release gate)

Key settings:
- `mode`: Test (default) or Production
- `auto_fix`: false (report only by default)
- `create_issues`: true
- `pr_comments`: true
- `schedule`: `0 0 * * 0` (weekly)
- `state_storage`: File (default), Redis, or Memory

### Per-Bot Workflow Checks

| Bot | Checks |
|-----|--------|
| rhodibot | required_files, directory_structure, license_presence, checkpoint_files, workflow_compliance |
| echidnabot | formal_spec_presence, proof_validation, fuzzing_coverage, theorem_prover_integration |
| sustainabot | carbon_intensity, resource_efficiency, technical_debt, dependency_health |
| glambot | wcag_accessibility, seo_optimization, machine_readability, visual_consistency |
| seambot | api_contracts, cross_component_tests, end_to_end_flows, integration_coverage |
| finishbot | no_placeholders, license_validation, claim_verification, execution_tests, changelog_present |

---

## 5. System Relationships

```
git-dispatcher (Central Coordination Hub)
  │
  ├── gitbot-fleet (Fleet Orchestrator - this repo)
  │     ├── 6 specialized bots (see above)
  │     └── shared-context (Rust coordination library)
  │
  ├── hypatia (Rules Engine, Engine tier)
  │     ├── Logtalk rules (symbolic reasoning)
  │     ├── Haskell registry (type-safe verification)
  │     └── ArangoDB + Dragonfly (graph DB + cache)
  │
  ├── robot-repo-automaton (Action Executor, Executor tier)
  │     └── Applies fixes with confidence thresholds
  │
  └── git-hud (Visualization Dashboard)
        └── Fleet status and finding display

OPSM (Operations Management)
  └── gitbot-fleet is the execution fleet for OPSM batch operations

.git-private-farm (Supervised Repos Config)
  └── Pre-configured repos for hypatia supervision
```

### Bot Modes

| Mode | Behavior |
|------|----------|
| **Consultant** | Reports findings without taking action |
| **Advisor** | Suggests fixes with explanations |
| **Regulator** | Enforces rules and blocks non-compliant changes |

---

## 6. Shared Context API Reference

### Core Types (`shared-context/src/`)

```rust
// Bot identification
enum BotId { Rhodibot, Echidnabot, Sustainabot, Glambot, Seambot, Finishbot, RobotRepoAutomaton, Hypatia, Custom(String) }
enum Tier { Engine=0, Verifier=1, Finisher=2, Executor=3, Custom=4 }

// Findings
struct Finding { id: Uuid, source: BotId, rule_id: String, severity: Severity, message: String, ... }
enum Severity { Error, Warning, Info, Suggestion }

// Context (main coordination point)
struct Context {
    session_id: Uuid,
    repo_name: String,
    executions: HashMap<BotId, BotExecution>,
    findings: FindingSet,
    data: HashMap<String, Value>,  // inter-bot shared data
    config: ContextConfig,
}
```

### Usage Pattern

```rust
use gitbot_shared_context::{Context, Finding, Severity, BotId};

// Create session
let mut ctx = Context::new("my-repo");
ctx.register_all_bots();

// Bot execution
ctx.start_bot(BotId::Rhodibot);
ctx.add_finding(Finding::new(
    BotId::Rhodibot,
    "missing-readme",
    Severity::Error,
    "README.adoc not found"
).with_file("README.adoc").fixable());
ctx.complete_bot(BotId::Rhodibot);

// Check gate
if ctx.verifiers_complete() {
    // Tier 2 can start
    let findings = ctx.findings_for_tier(Tier::Verifier);
}
```

### Storage

```rust
use gitbot_shared_context::ContextStorage;

let storage = ContextStorage::default(); // ~/.gitbot-fleet/
storage.save_context(&ctx)?;
let loaded = storage.load_context(session_id)?;
```

---

## 7. Build & Run Commands

```bash
# Build any bot
cd ~/Documents/hyperpolymath-repos/<bot-name>
cargo build --release

# Run fleet coordinator
cd ~/Documents/hyperpolymath-repos/gitbot-fleet
./fleet-coordinator.sh status
./fleet-coordinator.sh run-scan <repo-name>
./fleet-coordinator.sh process-findings

# Run individual bots
seambot check --path /path/to/repo
sustainabot analyze /path/to/file.rs
sustainabot check /path/to/dir --eco-threshold 50
finishbot check /path/to/repo --format sarif
rhodibot check /path/to/repo
glambot check /path/to/repo
```

---

*Last updated: 2026-02-05 by Opus recovery session*
