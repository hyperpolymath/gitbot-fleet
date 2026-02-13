// SPDX-License-Identifier: PMPL-1.0-or-later
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
     +── .bot_directives/ ────────── Per-repo permission control
```

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

## References

- [SARIF 2.1.0 Specification](https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html)
- [Software Carbon Intensity (SCI) Specification](https://sci.greensoftware.foundation/)
- [Green Software Foundation](https://greensoftware.foundation/)
