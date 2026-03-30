# Test & Benchmark Requirements

## Current State
- Unit tests: ~22 Rust test files + shared-context tests — count unknown (no Cargo.toml at root to run)
- Integration tests: partial (echidnabot/tests/integration_tests.rs, rhodibot/tests/integration_tests.rs)
- E2E tests: NONE
- Benchmarks: 5 files exist
- panic-attack scan: NEVER RUN

## What's Missing
### Point-to-Point (P2P)
205 Rust + 367 JavaScript + 42 ReScript + 34 Julia + 13 Haskell + 9 Idris2 + 85 Shell source files:

#### Bots (major components):
- **echidnabot/** — has integration tests, but coverage vs source count unclear
- **rhodibot/** — has integration tests
- **sustainabot/** — has test_sample.rs + .env.test, minimal coverage
- **finishbot/** — has analyzers/testing.rs (test utilities, not tests OF finishbot)
- **glambot/** — no tests found
- **seambot/** — no tests found
- **panicbot/** — no tests found

#### Shared Context (Rust):
- shared-context/tests/context_tests.rs — exists
- shared-context/tests/fleet_coordination_test.rs — exists
- Other shared modules may lack coverage

#### Robot-repo-automaton:
- No test files found

#### Dashboard:
- No test files found

#### Campaigns:
- No test files found

#### Hooks:
- No test files found

#### Tasks/Scripts:
- 85 shell scripts — no tests
- 367 JavaScript files — no tests (these are likely bot action scripts)
- 34 Julia files — no tests

### End-to-End (E2E)
- Bot fleet coordination: dispatch task -> bot processes -> report results -> aggregate
- Individual bot lifecycle: start -> receive event -> analyze -> act -> report
- echidnabot: receive webhook -> analyze repo -> generate findings -> submit
- rhodibot: detect issue -> apply fix -> create PR
- sustainabot: check dependencies -> evaluate health -> report
- finishbot: analyze completion -> identify gaps -> notify
- glambot: check style -> suggest fixes
- seambot: check integration points -> verify
- panicbot: run security scan -> report findings
- Robot-repo-automaton: detect issue -> calculate confidence -> auto-fix
- Fleet coordination: multiple bots on same repo -> deconflict

### Aspect Tests
- [ ] Security (webhook signature verification, bot credential handling, repo access scoping, auto-fix safety)
- [ ] Performance (fleet throughput, bot startup latency, concurrent repo processing)
- [ ] Concurrency (bot deconfliction, shared-context locking, parallel webhook processing)
- [ ] Error handling (bot crash recovery, API rate limiting, malformed webhooks)
- [ ] Accessibility (dashboard UI if applicable)

### Build & Execution
- [ ] cargo build per bot — not verified
- [ ] Individual bot startup — not verified
- [ ] Fleet orchestration — not verified
- [ ] Dashboard build — not verified
- [ ] Self-diagnostic — none

### Benchmarks Needed
- Per-bot analysis throughput (repos/hour)
- Fleet coordination overhead
- Webhook processing latency
- Auto-fix confidence score accuracy
- Memory usage per active bot

### Self-Tests
- [ ] panic-attack assail on own repo
- [ ] Fleet health check
- [ ] Per-bot self-test
- [ ] Shared-context integrity verification

## Priority
- **HIGH** — Bot fleet (205 Rust + 367 JS + 42 ReScript + 34 Julia files) with tests only for echidnabot, rhodibot, and shared-context. 5 out of 7+ bots have ZERO tests. The 367 JavaScript files (bot action scripts) are completely untested. The robot-repo-automaton that auto-fixes repos based on confidence scores has no tests — this is a system that makes automated changes to other repos and needs extremely high correctness guarantees.

## FAKE-FUZZ ALERT

- `tests/fuzz/placeholder.txt` is a scorecard placeholder inherited from rsr-template-repo — it does NOT provide real fuzz testing
- Replace with an actual fuzz harness (see rsr-template-repo/tests/fuzz/README.adoc) or remove the file
- Priority: P2 — creates false impression of fuzz coverage
