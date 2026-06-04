#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Smoke test for hypatia_record_outcome() in scripts/dispatch-runner.sh.
#
# Covers the no-op short-circuit paths that don't require an actual
# hypatia checkout or `mix` on PATH:
#   1. HYPATIA_OUTCOME_REPORT=off  → silent no-op, exit 0.
#   2. HYPATIA_HOME not a directory → warn to stderr, exit 0.
#   3. Empty / "none" / "null" recipe_id → silent no-op, exit 0.
#
# End-to-end paths that *do* talk to hypatia (exit 0/2/1) require a
# running mix env and are covered by the integration suite on the
# hypatia side. This file just guarantees the dispatch loop never
# crashes on the optional-call ladder.

set -euo pipefail

SCRIPT_DIR=$(dirname "$(realpath "$0")")
DISPATCH_RUNNER="${SCRIPT_DIR}/../scripts/dispatch-runner.sh"

if [[ ! -f "$DISPATCH_RUNNER" ]]; then
    echo "FAIL: dispatch-runner.sh not found at $DISPATCH_RUNNER"
    exit 1
fi

# We source the script in a guarded mode so we get function definitions
# without triggering the main loop. The script exits early when its
# manifest doesn't exist, so we point it at a non-existent path and
# capture the early exit.
TMP=$(mktemp -d /tmp/c15-smoke-XXXXXX)
trap 'rm -rf "$TMP"' EXIT

# Extract just the function definitions we need (between # --- Hypatia
# closed-loop contract --- and the next "# --- " section). Cheap and
# avoids running the main loop.
awk '
  /^# --- Hypatia closed-loop contract ---/ { capture=1 }
  capture { print }
  /^record_outcome\(\)/ && capture { record_seen=1 }
  record_seen && /^}/ { print "# end-extract"; exit }
' "$DISPATCH_RUNNER" > "$TMP/extracted.sh"

# Provide a stub REPOS_BASE so HYPATIA_HOME default resolves to
# something the test can control.
export REPOS_BASE="$TMP/repos"

# Source the extracted block into this shell.
# shellcheck disable=SC1091
source "$TMP/extracted.sh"

pass=0
fail=0

assert_silent_ok() {
    local label="$1"
    shift
    local out
    if out=$("$@" 2>&1); then
        if [[ -z "$out" ]]; then
            pass=$((pass + 1))
            echo "PASS: $label"
        else
            fail=$((fail + 1))
            echo "FAIL: $label — expected silent, got: $out"
        fi
    else
        fail=$((fail + 1))
        echo "FAIL: $label — expected exit 0, got non-zero"
    fi
}

assert_warn_ok() {
    local label="$1"
    local expect="$2"
    shift 2
    local out
    if out=$("$@" 2>&1); then
        if [[ "$out" == *"$expect"* ]]; then
            pass=$((pass + 1))
            echo "PASS: $label"
        else
            fail=$((fail + 1))
            echo "FAIL: $label — expected warning containing '$expect', got: $out"
        fi
    else
        fail=$((fail + 1))
        echo "FAIL: $label — expected exit 0, got non-zero"
    fi
}

# Case 1: opt-out.
HYPATIA_OUTCOME_REPORT=off \
    assert_silent_ok "opt-out silently no-ops" \
    hypatia_record_outcome "rid-1" "owner/repo" "src/file.py" "success"

# Case 2: HYPATIA_HOME not a directory.
HYPATIA_OUTCOME_REPORT=on \
HYPATIA_HOME="$TMP/does-not-exist" \
    assert_warn_ok "missing HYPATIA_HOME warns and no-ops" \
    "not a directory" \
    hypatia_record_outcome "rid-2" "owner/repo" "src/file.py" "success"

# Cases 3a-c need HYPATIA_HOME to exist so the recipe-id guard fires
# rather than the missing-dir guard.
mkdir -p "$TMP/repos/hypatia"

# Case 3a: empty recipe_id.
HYPATIA_OUTCOME_REPORT=on \
HYPATIA_HOME="$TMP/repos/hypatia" \
    assert_silent_ok "empty recipe_id silently no-ops" \
    hypatia_record_outcome "" "owner/repo" "src/file.py" "success"

# Case 3b: "none" recipe_id.
HYPATIA_OUTCOME_REPORT=on \
HYPATIA_HOME="$TMP/repos/hypatia" \
    assert_silent_ok "'none' recipe_id silently no-ops" \
    hypatia_record_outcome "none" "owner/repo" "src/file.py" "success"

# Case 3c: "null" recipe_id.
HYPATIA_OUTCOME_REPORT=on \
HYPATIA_HOME="$TMP/repos/hypatia" \
    assert_silent_ok "'null' recipe_id silently no-ops" \
    hypatia_record_outcome "null" "owner/repo" "src/file.py" "success"

echo ""
echo "Results: $pass passed, $fail failed"
[[ "$fail" -eq 0 ]]
