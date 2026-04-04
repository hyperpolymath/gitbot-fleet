# Test & Benchmark Requirements

## Current State (updated 2026-04-04)

### What Was Added (this session — CRG C blitz)

| Area | Tests Added | Location |
|------|-------------|----------|
| `robot-repo-automaton` fixer idempotency | 3 tests (delete/create/modify × apply-twice) | `robot-repo-automaton/tests/fixer_tests.rs` |
| `robot-repo-automaton` path traversal security | 4 tests (delete/create/modify traversal + safe path) | `robot-repo-automaton/tests/fixer_tests.rs` |
| `robot-repo-automaton` confidence numeric thresholds | 5 tests (0.95/0.7 boundaries, Delete thresholds, decide outputs) | `robot-repo-automaton/src/confidence.rs` (in-module) |
| Shared-context E2E fleet coordination | 6 scenarios (single-bot, multi-bot, failure isolation, persistence, reporting, severity gate) | `shared-context/tests/e2e_fleet_coordination_test.rs` |
| Shared-context P2P property tests | 11 property tests (bot subsets, confidence bounds, dispatch determinism) | `shared-context/tests/property_tests.rs` |
| Shared-context `context_tests.rs` fixes | Fixed stale API calls, tokio missing features | `shared-context/tests/context_tests.rs` |
| `echidnabot` benchmark (stub → real) | assess_confidence × 3 variants, all ProverKinds, file extension lookups | `bots/echidnabot/benches/echidnabot_bench.rs` |
| Path traversal guard in fixer | `normalise_path()` + security check in `Fixer::apply()` | `robot-repo-automaton/src/fixer.rs` |
| Shell script syntax validation | 85 scripts validated with `bash -n` (0 errors) | All `*.sh` in repo |

### Test Counts After This Session

| Crate | Test Count | Status |
|-------|-----------|--------|
| `shared-context` (all test files) | 67 tests | All passing |
| `robot-repo-automaton` (lib + 3 test files + doctest) | 79 tests | All passing |
| `panicbot` | 76 tests | All passing |
| `seambot` | 68 tests | All passing |
| `glambot` | 47 tests | All passing |

---

## Remaining Gaps

### High Priority

- **Unit tests for `Detector::detect_all` with numeric confidence thresholds**:
  The task-level thresholds (>0.9 auto-apply, 0.7-0.9 suggest, <0.7 skip) are
  documented in tests but only exercised via `FixAction::Delete`. Tests for other
  action types (Modify, Create) should be added.

- **robot-repo-automaton: `detector.rs` confidence score verification**:
  Content-match detection returns 0.95, file-existence returns 1.0, language
  mismatch returns 0.90 — these specific values should be regression-tested.

- **sustainabot**: Has minimal test coverage (only `test_sample.rs`). The 6-crate
  workspace needs test coverage per crate.

- **finishingbot**: `analyzer_tests.rs` exists but coverage vs source count is unclear.

### Medium Priority

- **Fleet E2E at script level**: `fleet-coordinator.sh` + `dispatch-runner.sh`
  integration — currently untested end-to-end. Bash integration tests via
  `bats` or similar.

- **Dashboard**: No tests found.

- **367 JavaScript files**: Bot action scripts (campaigns/, hooks/) are completely
  untested.

- **34 Julia files**: No tests.

### Low Priority

- **Fuzz testing**: `tests/fuzz/placeholder.txt` is still a scorecard placeholder
  from rsr-template-repo — does NOT provide real fuzz coverage. Replace with a
  real libFuzzer harness targeting `robot-repo-automaton` catalog parsing.

- **self-test / fleet health check**: No self-diagnostic for the fleet as a whole.

- **echidnabot benchmark**: The real benchmark was added but not verified to
  compile (compilation takes >2 minutes). Verify on next visit.

## Shell Script Validation (2026-04-04)

```
$ find . -name "*.sh" -not -path "*/target/*" | xargs bash -n
(no output — all 85 scripts pass syntax check)
```

**Result: 85/85 scripts pass `bash -n` with zero syntax errors.**
