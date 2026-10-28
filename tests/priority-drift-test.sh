#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# priority-drift-test.sh — verify canonical priority order is consistently used
# across the fleet: member-priority.json, fleet-report-back.sh, fix-fleet-enrolment.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FLEET_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

PRI_FILE="$FLEET_DIR/.machine_readable/member-priority.json"
REPORT_BACK="$FLEET_DIR/scripts/fleet-report-back.sh"
FIX_ENROL="$FLEET_DIR/scripts/fix-fleet-enrolment.sh"

failures=0

check_pri_file() {
    echo "=== Checking .machine_readable/member-priority.json ==="
    [ -f "$PRI_FILE" ] || { echo "FAIL: $PRI_FILE not found"; return 1; }

    # Validate schema
    jq -e '.schema == 1' "$PRI_FILE" >/dev/null 2>&1 || { echo "FAIL: schema != 1"; return 1; }
    jq -e '.priority | type == "array"' "$PRI_FILE" >/dev/null 2>&1 || { echo "FAIL: priority not an array"; return 1; }

    # Check all expected members present
    local expected=("rhodibot" "echidnabot" "sustainabot" "panicbot" "glambot" "seambot" "finishingbot" "accessibilitybot" "cipherbot" "robot-repo-automaton" "hypatia")
    local missing=()
    for m in "${expected[@]}"; do
        jq -e --arg m "$m" '.priority | index($m)' "$PRI_FILE" >/dev/null 2>&1 || missing+=("$m")
    done
    if [ ${#missing[@]} -gt 0 ]; then
        echo "FAIL: missing members in priority: ${missing[*]}"
        return 1
    fi

    # Check no duplicates
    local dupes
    dupes=$(jq -r '.priority[]' "$PRI_FILE" | sort | uniq -d)
    [ -z "$dupes" ] || { echo "FAIL: duplicate members in priority: $dupes"; return 1; }

    echo "OK: priority file valid with $(jq '.priority | length' "$PRI_FILE") members"
    return 0
}

check_report_back_sort() {
    echo "=== Checking fleet-report-back.sh uses priority order ==="
    [ -f "$REPORT_BACK" ] || { echo "FAIL: $REPORT_BACK not found"; return 1; }

    # Check for sort_by_priority function
    grep -q 'sort_by_priority()' "$REPORT_BACK" || { echo "FAIL: sort_by_priority function not found"; return 1; }

    # Check that members are sorted by priority before execution
    grep -q 'sort_by_priority' "$REPORT_BACK" || { echo "FAIL: sort_by_priority not called"; return 1; }

    # Check priority_order function exists
    grep -q 'priority_order()' "$REPORT_BACK" || { echo "FAIL: priority_order function not found"; return 1; }

    echo "OK: fleet-report-back.sh uses priority sorting"
    return 0
}

check_fix_enrol_order() {
    echo "=== Checking fix-fleet-enrolment.sh writes manifest in priority order ==="
    [ -f "$FIX_ENROL" ] || { echo "FAIL: $FIX_ENROL not found"; return 1; }

    # Check that manifest creation uses priority order from member-priority.json
    grep -q 'member-priority.json' "$FIX_ENROL" || { echo "FAIL: fix-fleet-enrolment.sh does not reference member-priority.json"; return 1; }

    # Check for priority_order variable usage
    grep -q 'priority_order' "$FIX_ENROL" || { echo "FAIL: priority_order not used in fix-fleet-enrolment.sh"; return 1; }

    echo "OK: fix-fleet-enrolment.sh writes manifest in priority order"
    return 0
}

check_manifest_matches_priority() {
    echo "=== Checking generated manifest key order matches priority ==="
    local test_repo
    test_repo=$(mktemp -d)
    trap 'rm -rf "$test_repo"' RETURN

    # Create minimal repo structure
    mkdir -p "$test_repo/.gitbot-fleet"
    touch "$test_repo/Cargo.toml"
    mkdir -p "$test_repo/spec/seams"
    touch "$test_repo/spec/seams/seam-register.json"
    mkdir -p "$test_repo/proofs"
    touch "$test_repo/index.html"

    # Run fix-fleet-enrolment.sh on test repo (exit 3 is OK - manifest created but not fully wired)
    local fix_rc=0
    "$FIX_ENROL" "$test_repo" >/dev/null 2>&1 || fix_rc=$?
    if [ "$fix_rc" -ne 0 ] && [ "$fix_rc" -ne 3 ]; then
        echo "FAIL: fix-fleet-enrolment.sh failed on test repo (exit $fix_rc)"
        return 1
    fi

    local manifest="$test_repo/.gitbot-fleet/enrolment.json"
    [ -f "$manifest" ] || { echo "FAIL: manifest not created"; return 1; }

    # Extract reporter keys in serialization order from raw JSON
    # jq's keys[] sorts alphabetically, so we use Python to preserve order
    local manifest_keys
    manifest_keys=$(python3 -c "
import json, sys
with open('$manifest') as f:
    data = json.load(f)
# Python 3.7+ preserves dict insertion order
for k in data.get('reporters', {}):
    print(k)
")

    # Extract priority order (only enrolled members)
    local priority_keys
    priority_keys=$(jq -r '.priority[]' "$PRI_FILE")

    # Compare enrolled members in manifest order vs priority order
    local enrolled_in_manifest=()
    local enrolled_in_priority=()

    while IFS= read -r key; do
        [ -n "$key" ] || continue
        if jq -e --arg k "$key" '.reporters[$k].enrolled == true' "$manifest" >/dev/null 2>&1; then
            enrolled_in_manifest+=("$key")
        fi
    done <<< "$manifest_keys"

    while IFS= read -r key; do
        if jq -e --arg k "$key" '.reporters[$k].enrolled == true' "$manifest" >/dev/null 2>&1; then
            enrolled_in_priority+=("$key")
        fi
    done <<< "$priority_keys"

    # Check if orders match
    local match=true
    if [ ${#enrolled_in_manifest[@]} -ne ${#enrolled_in_priority[@]} ]; then
        match=false
    else
        for i in "${!enrolled_in_manifest[@]}"; do
            if [ "${enrolled_in_manifest[i]}" != "${enrolled_in_priority[i]}" ]; then
                match=false
                break
            fi
        done
    fi

    if [ "$match" = false ]; then
        echo "FAIL: manifest key order does not match priority order"
        echo "  Manifest order: ${enrolled_in_manifest[*]}"
        echo "  Priority order: ${enrolled_in_priority[*]}"
        return 1
    fi

    echo "OK: manifest key order matches priority order (${#enrolled_in_manifest[@]} enrolled members)"
    return 0
}

check_envelope_provenance() {
    echo "=== Checking envelope provenance fields (F3) ==="
    # Check write_envelope includes adapter_version, tool_version
    grep -q 'adapter_version' "$REPORT_BACK" || { echo "FAIL: adapter_version not in write_envelope"; return 1; }
    grep -q 'tool_version' "$REPORT_BACK" || { echo "FAIL: tool_version not in write_envelope"; return 1; }

    # Check enrich_findings function
    grep -q 'enrich_findings()' "$REPORT_BACK" || { echo "FAIL: enrich_findings function not found"; return 1; }

    # Check semantic_fingerprint function
    grep -q 'semantic_fingerprint()' "$REPORT_BACK" || { echo "FAIL: semantic_fingerprint function not found"; return 1; }

    # Check evidence_class_of function
    grep -q 'evidence_class_of()' "$REPORT_BACK" || { echo "FAIL: evidence_class_of function not found"; return 1; }

    echo "OK: envelope provenance fields present"
    return 0
}

check_enrolment_lint() {
    echo "=== Checking enrolment lint (F2) ==="
    grep -q 'LINT:' "$FIX_ENROL" || { echo "FAIL: LINT warnings not found in fix-fleet-enrolment.sh"; return 1; }

    # Check for the three specific lint cases
    grep -q 'panicbot enrolled but Hypatia lane already runs panic-attack' "$FIX_ENROL" || { echo "FAIL: panicbot double-counting lint not found"; return 1; }
    grep -q 'echidnabot enrolled but no ECHIDNA engine' "$FIX_ENROL" || { echo "FAIL: echidnabot no-engine lint not found"; return 1; }
    grep -q 'sustainabot enrolled but no sustainabot adapter' "$FIX_ENROL" || { echo "FAIL: sustainabot no-adapter lint not found"; return 1; }
    grep -q 'cipherbot enrolled but no cryptographic code' "$FIX_ENROL" || { echo "FAIL: cipherbot no-crypto lint not found"; return 1; }

    echo "OK: enrolment lint checks present"
    return 0
}

# Run all checks
echo "Running priority drift tests..."
echo

check_pri_file || failures=$((failures + 1))
check_report_back_sort || failures=$((failures + 1))
check_fix_enrol_order || failures=$((failures + 1))
check_manifest_matches_priority || failures=$((failures + 1))
check_envelope_provenance || failures=$((failures + 1))
check_enrolment_lint || failures=$((failures + 1))

echo
if [ "$failures" -eq 0 ]; then
    echo "All priority drift tests PASSED"
    exit 0
else
    echo "$failures test(s) FAILED"
    exit 1
fi