#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Exercise dispatch orchestration with an inert fix executable and isolated data.
set -euo pipefail
fleet_test_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
fixture="$(mktemp -d)"
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/repos/sample" "$fixture/checkout/scripts" "$fixture/primary/dispatch"
cp "$fleet_test_root/scripts/repo-path-overrides.json" "$fixture/checkout/scripts/"

run_dispatch() {
    env -u GITHUB_TOKEN -u FLEET_DISPATCH_TOKEN \
        FLEET_ROOT="$fixture/checkout" REPOS_BASE="$fixture/repos" \
        HYPATIA_DATA="$fixture/primary" VERISIMDB_DATA="$1" \
        HYPATIA_OUTCOME_REPORT=off KIN_DIR="$fixture/kin" RRA_BIN=/bin/true \
        bash "$fleet_test_root/scripts/dispatch-runner.sh" --limit 1
}

manifest="$fixture/primary/dispatch/pending.jsonl"
printf '%s\n' '{"repo":"sample","tier":"eliminate","strategy":"auto_execute","pattern_id":"fixture","recipe_id":"none","auto_fixable":true}' > "$manifest"
# Default primary and central stores are the same; exactly one JSONL record.
run_dispatch "$fixture/primary/."
month="$(date -u +%Y-%m)"
test "$(wc -l < "$fixture/primary/outcomes/$month.jsonl")" -eq 1
jq -e '.outcome == "success"' "$fixture/primary/outcomes/$month.jsonl" >/dev/null
# Separate stores both receive one copy of this new outcome.
run_dispatch "$fixture/central"
test "$(wc -l < "$fixture/primary/outcomes/$month.jsonl")" -eq 2
test "$(wc -l < "$fixture/central/outcomes/$month.jsonl")" -eq 1

printf '%s\n' '{"repo":"sample","tier":"control","strategy":"review","pattern_id":"review-fixture"}' > "$manifest"
run_dispatch "$fixture/central"
test -s "$fixture/checkout/shared-context/findings/pending/sample--review-fixture.json"
printf '%s\n' '{"repo":"sample","tier":"control","strategy":"report_only","pattern_id":"advisory-fixture"}' > "$manifest"
run_dispatch "$fixture/central"
test -s "$fixture/checkout/shared-context/advisories/$month.jsonl"
test ! -d "$fixture/repos/gitbot-fleet"

# Legacy aliases remain fallbacks; direct checkouts still take precedence.
mkdir -p "$fixture/repos/developer-ecosystem/julia-ecosystem/packages/Axiom.jl"
printf '%s\n' '{"repo":"Axiom.jl","tier":"control","strategy":"review","pattern_id":"alias-fixture"}' > "$manifest"
run_dispatch "$fixture/central"
test -s "$fixture/checkout/shared-context/findings/pending/Axiom.jl--alias-fixture.json"
echo 'Dispatch path and outcome contracts passed'
