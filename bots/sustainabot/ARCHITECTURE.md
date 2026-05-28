// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2024-2025 Jonathan D.A. Jewell

# SustainaBot Architecture

## Overview

SustainaBot is a Rust workspace that statically analyzes code for ecological and economic sustainability. It uses tree-sitter for multi-language AST parsing and produces SARIF 2.1.0 output for IDE/CI integration.

**Unique value**: the only tool correlating security findings with sustainability impact.

## Crate Dependency Graph

```
sustainabot-cli
  |-- sustainabot-analysis   (AST parsing, patterns, calibration, deps, security)
  |-- sustainabot-metrics    (core types: AnalysisResult, ResourceProfile, HealthIndex)
  |-- sustainabot-sarif      (SARIF 2.1.0 output)
  |-- sustainabot-eclexia    (policy engine)
  |-- sustainabot-fleet      (gitbot-fleet integration)
```

All crates depend on `sustainabot-metrics` for shared types.

## Crate Details

### sustainabot-metrics

Core data types shared across all crates.

**Key types:**
- `AnalysisResult` — per-function analysis output (location, resources, health, rule_id, suggestion, confidence)
- `ResourceProfile` — energy (J), carbon (gCO2e), duration (ms), memory (bytes)
- `HealthIndex` — eco_score, econ_score, quality_score, overall (all 0-100)
- `CodeLocation` — file, line, column, end_line, end_column, function name
- `Confidence` — Measured | Calibrated | Estimated | Unknown
- Newtype wrappers: `EcoScore(f64)`, `EconScore(f64)`, `Joules(f64)`, `CarbonGrams(f64)`, etc.

### sustainabot-analysis

The analysis engine. Modules:

- **analyzer.rs** — `Analyzer` struct with `analyze_file()` and `analyze_source()`. Uses tree-sitter to parse Rust, JavaScript, Python. Walks the AST to find function nodes, computes complexity metrics, estimates resources, detects patterns.

- **patterns.rs** — 7 pattern detectors returning `PatternMatch` structs (name, description, suggestion, impact_multiplier):
  1. `nested-loops` — O(n^k) from nested iteration
  2. `busy-wait` — loops without sleep/await/yield
  3. `string-concat-in-loop` — string allocation per iteration
  4. `clone-in-loop` — unnecessary deep copies
  5. `unbuffered-io` — I/O without buffering
  6. `large-allocation` — single allocations >1MB
  7. `redundant-allocation` — `.to_string()` where borrow suffices

- **calibration.rs** — Replaces naive `complexity * 0.1J` with pattern-based resource profiles. `ResourceRange` (min/typical/max) for 8 operation kinds. Confidence levels attached.

- **dependencies.rs** — Parses `Cargo.toml` and `package.json`. Flags heavy deps (tokio, serde, react, webpack, etc.) and deps using all features.

- **directives.rs** — Parses `.bot_directives/*.scm` S-expression files using `lexpr`. Returns `BotDirective` with allow/deny/scopes/thresholds.

- **security.rs** — Behind `#[cfg(feature = "panic-attack")]`. Calls `panic_attack::xray::analyze()`, maps `WeakPointCategory` to energy waste multipliers, produces security-sustainability composite scores. Boosts severity 1.5x when eco and security findings overlap.

- **language.rs** — `Language` enum (Rust, JavaScript, Python) with tree-sitter parser creation.

### sustainabot-sarif

SARIF 2.1.0 output. Converts `Vec<AnalysisResult>` into a valid SARIF log with:
- Full `physicalLocation` (file, startLine, startColumn, endLine, endColumn)
- 11 builtin rule definitions (eco-threshold, carbon-intensity, nested-loops, etc.)
- `fixes` array with concrete suggestions
- Custom `properties` carrying eco/econ scores, resource profiles, confidence
- Severity mapping: eco_score < 30 → Error, < 60 → Warning, ≥ 60 → Note

### sustainabot-eclexia

Policy engine with two backends (feature-flagged):

- **Default (binary)**: Shells out to `eclexia` binary, JSON I/O. Falls back to built-in Rust implementation of standard policies.
- **Native (eclexia-native feature)**: Direct `eclexia-interp` integration for type-safe evaluation.

Built-in policies: energy threshold, carbon budget, memory efficiency.

Returns `Vec<PolicyDecision>` (Pass/Warn/Fail) which get converted to `AnalysisResult` for SARIF output.

### sustainabot-fleet

gitbot-fleet integration. Converts `AnalysisResult` → `gitbot_shared_context::Finding` using the full builder API. Directive-aware: reads `.bot_directives/sustainabot.scm` for scope control and custom thresholds.

### sustainabot-cli

CLI binary with 5 subcommands:
- `analyze <file>` — single file analysis
- `check <dir>` — recursive directory check with eco threshold gating
- `report <dir>` — same as check but defaults to SARIF output
- `fleet <dir>` — run as gitbot-fleet member
- `self-analyze` — dogfooding mode

Flags: `--format`, `--output`, `--eco-threshold`, `--security`, `--policy-dir`, `--verbose`

## Analysis Pipeline

```
1. Input: file path or directory
2. Language detection (.rs → Rust, .js → JavaScript, .py → Python)
3. tree-sitter parsing → AST
4. Function node extraction (fn_item, function_declaration, function_definition)
5. Per-function analysis:
   a. Complexity: cyclomatic (branches), nesting depth
   b. Resource estimation: energy, carbon, duration, memory
   c. Pattern detection: 7 anti-patterns with fix suggestions
   d. Health scoring: eco, econ, quality → overall
6. Optional: security correlation (panic-attack)
7. Optional: policy evaluation (Eclexia)
8. Optional: dependency analysis
9. Output: SARIF / JSON / text
```

## Feature Flags

| Flag | Crate | Effect |
|------|-------|--------|
| `security` | sustainabot-cli | Enables `--security` flag, pulls in panic-attack |
| `panic-attack` | sustainabot-analysis | Enables security.rs correlation engine |
| `eclexia-native` | sustainabot-eclexia | Uses eclexia-interp library instead of binary |

## Ecosystem Integration

```
sustainabot ──────── SARIF ──────── GitHub Security Tab / IDE
     |
     |── panic-attack ────────────── Security-sustainability correlation
     |
     |── eclexia ─────────────────── Resource-aware policy evaluation
     |
     |── gitbot-fleet ────────────── Multi-repo orchestration
     |       |
     |       +── shared-context ──── Cross-bot findings
     |
     |── hypatia (green_web.ex) ──── Green Web hosting/CDN/registry checks
     |       |                        Routes via fleet_dispatcher.ex
     |       +── reportEcoScore ──── GraphQL mutation → eco_score findings
     |
     +── .bot_directives/ ────────── Per-repo permission control
```

### Hypatia Green Web Integration

SustainaBot receives infrastructure-level sustainability findings from Hypatia's
`green_web.ex` rule module via the fleet dispatch pipeline:

1. **Hypatia** scans repos for hosting provider, CDN, container registry choices
2. **fleet_dispatcher.ex** classifies Green Web findings as `:control` actions
3. **`dispatch_to_sustainabot/1`** sends a `reportEcoScore` GraphQL mutation
4. **SustainaBot** incorporates the infrastructure eco score and generates fix suggestions

This separates concerns: Hypatia owns *detection* (scanning 500+ repos), SustainaBot
owns *action* (PRs, badges, SARIF annotations).

## Scoring

### Health Index
```
HealthIndex = 0.4 * EcoScore + 0.3 * EconScore + 0.3 * QualityScore
```

### Eco Score
Based on resource efficiency: energy per function, carbon intensity, pattern penalties.

### Econ Score
Based on economic efficiency: complexity economics, allocative efficiency, maintainability.

### Quality Score
Based on code quality: cyclomatic complexity, nesting depth, pattern count.

## Testing

27 tests across all crates:
- `sustainabot-analysis`: analyzer, patterns, calibration, dependencies, directives
- `sustainabot-sarif`: SARIF structure, JSON validity, suggestions, severity
- `sustainabot-eclexia`: policy evaluation, decisions_to_results
- `sustainabot-fleet`: finding conversion, severity mapping, suggestions

## Budget-Resume Sweep Integration (AM010 / BP008)

**TODO** (future Rust integration — Rust implementation deferred, out of scope for this cycle).

When sustainabot's periodic estate scan detects more than 50 open PRs that are stuck
exclusively on phantom required status-check contexts (BP008 findings), it should trigger
the `.git-private-farm/dispatch-templates/budget-resume-sweep.yml` workflow via
`workflow_dispatch` to admin-merge the eligible PRs in bulk.

### Calling pattern

sustainabot triggers the sweep by dispatching to the farm workflow:

```sh
gh workflow run budget-resume-sweep.yml \
  --repo hyperpolymath/.git-private-farm \
  --field target_owner=<OWNER> \
  --field target_repo=<REPO> \
  --field max_merges=20
```

### Eligibility criteria (AM010)

A PR is considered eligible for the sweep when every required status-check context
configured in branch protection (see GitHub API
`repos/{owner}/{repo}/branches/main/protection/required_status_checks`) either:

1. **passes** (conclusion `SUCCESS`, `NEUTRAL`, or `SKIPPED` in the PR's rollup), or
2. **is a phantom** — it is listed as required but has emitted zero check-runs across
   the last five commits on main (Hypatia rule BP008), AND it has no rollup entry on
   this specific PR (ALARP type-1 mitigation: a path-filtered workflow that happens to
   fire on this PR is not treated as phantom even if it was silent on recent main commits).

The `hypatia pr-eligibility --owner X --repo Y --pr N` escript command implements this
check and emits JSON:

```json
{"eligible": true, "reason": "AM010", "phantom_contexts": ["spark-theatre-gate / SPARK Theatre Gate"], "required_contexts": [...]}
```

### Threshold

The dispatch fires when `eligible_pr_count >= 50` (configurable via Hypatia DBA002 rule).
This was the empirically validated threshold from the 2026-05-27 estate sweep where the
GitHub Actions billing cliff-hit first occurred.

### Why Rust integration is deferred

The sweep logic is pure GitHub API orchestration (list PRs → query Hypatia → admin-merge).
This is already fully covered by the shell script inside `budget-resume-sweep.yml` which
clones and builds the Hypatia escript on-demand. Embedding the same logic in Rust would
duplicate the Hypatia eligibility engine and create a maintenance surface. The correct
long-term architecture is for sustainabot to invoke the workflow dispatch (one HTTP call)
rather than re-implement the BP008/AM010 checks natively.

When the Hypatia escript gains a stable gRPC or HTTP API (planned for a future release),
sustainabot-fleet can be updated to call that API directly instead of shelling out.

## References

- [SARIF 2.1.0 Specification](https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html)
- [Software Carbon Intensity (SCI) Specification](https://sci.greensoftware.foundation/)
- [Green Software Foundation](https://greensoftware.foundation/)
- [Hypatia BP008/AM010 rules](https://github.com/hyperpolymath/hypatia) — phantom required context detection and admin-merge eligibility
- [`.git-private-farm/dispatch-templates/budget-resume-sweep.yml`](https://github.com/hyperpolymath/.git-private-farm/blob/main/dispatch-templates/budget-resume-sweep.yml) — the workflow triggered when eligible PR count exceeds 50
