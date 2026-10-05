#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Exercise dispatch orchestration with an inert fix executable and isolated data.
set -euo pipefail
fleet_test_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
fixture="$(mktemp -d)"
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/repos/sample" "$fixture/checkout/scripts" "$fixture/primary/dispatch"
cp "$fleet_test_root/scripts/repo-path-overrides.json" "$fixture/checkout/scripts/"

# While dispatch is quarantined (docs/AUTOMATION-QUARANTINE.adoc), the
# production runner must refuse every call. The path/outcome contracts
# below run against a test-only copy with the interlock removed, inside the
# throwaway fixture; that copy is unreachable from any production entry point.
real_runner="$fleet_test_root/scripts/dispatch-runner.sh"
seam_runner="$fixture/checkout/scripts/dispatch-runner.sh"
interlock_msg='BLOCKED: repository directive enforcement is not qualified'
if ! grep -qF "$interlock_msg" "$real_runner"; then
    echo "FAIL: dispatch-runner.sh no longer carries the quarantine interlock;" >&2
    echo "      update this test to run the real runner and drop the seam." >&2
    exit 1
fi
# Strip exactly the interlock: the BLOCKED printf and the exit 78 after it.
awk -v msg="$interlock_msg" '
    skip_exit && /^exit 78$/ { skip_exit = 0; next }
    index($0, msg) && /^printf / { skip_exit = 1; next }
    { print }
' "$real_runner" > "$seam_runner"
if grep -qF "$interlock_msg" "$seam_runner" || cmp -s "$real_runner" "$seam_runner"; then
    echo "FAIL: could not build the test-only seam runner" >&2
    exit 1
fi

# Run one dispatch against the fixture with the given runner and central store.
invoke_runner() {
    local runner="$1" central="$2"
    env -u GITHUB_TOKEN -u FLEET_DISPATCH_TOKEN \
        FLEET_ROOT="$fixture/checkout" REPOS_BASE="$fixture/repos" \
        HYPATIA_DATA="$fixture/primary" VERISIMDB_DATA="$central" \
        HYPATIA_OUTCOME_REPORT=off KIN_DIR="$fixture/kin" RRA_BIN=/bin/true \
        bash "$runner" --limit 1
}

# Run one dispatch through the test-only seam runner (interlock removed).
run_dispatch() {
    invoke_runner "$seam_runner" "$1"
}

manifest="$fixture/primary/dispatch/pending.jsonl"
printf '%s\n' '{"repo":"sample","tier":"eliminate","strategy":"auto_execute","pattern_id":"fixture","recipe_id":"none","auto_fixable":true}' > "$manifest"

# The production runner refuses with exit 78 and writes no outcome state.
set +e
quarantine_err="$(invoke_runner "$real_runner" "$fixture/primary/." 2>&1 >/dev/null)"
quarantine_rc=$?
set -e
test "$quarantine_rc" -eq 78
grep -qF "$interlock_msg" <<<"$quarantine_err"
test ! -e "$fixture/primary/outcomes"
# Planted control: the seam copy must NOT be refused, or the assertion above
# could never distinguish a quarantined runner from an unquarantined one.
set +e
invoke_runner "$seam_runner" "$fixture/scratch-control" >/dev/null 2>&1
control_rc=$?
set -e
test "$control_rc" -ne 78
rm -rf "$fixture/primary/outcomes" "$fixture/scratch-control" "$fixture/kin"
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
