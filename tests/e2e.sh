#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# gitbot-fleet — End-to-End Tests
#
# Validates the gitbot fleet coordinator scripts, bot directory structure,
# shell script syntax, and required configuration files/directories.
#
# Usage:
#   bash tests/e2e.sh
#   just e2e

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

PASS=0
FAIL=0
SKIP=0

# ─── Colour helpers ──────────────────────────────────────────────────
green() { printf '\033[32m%s\033[0m\n' "$*"; }
red()   { printf '\033[31m%s\033[0m\n' "$*"; }
yellow(){ printf '\033[33m%s\033[0m\n' "$*"; }
bold()  { printf '\033[1m%s\033[0m\n' "$*"; }

# ─── Assertion helpers ───────────────────────────────────────────────

# check <label> <expected-substring> <actual>
check() {
    local name="$1" expected="$2" actual="$3"
    if echo "$actual" | grep -q "$expected"; then
        green "  PASS: $name"
        PASS=$((PASS + 1))
    else
        red "  FAIL: $name (expected '$expected', got '${actual:0:120}')"
        FAIL=$((FAIL + 1))
    fi
}

# check_exists <label> <path>
check_exists() {
    local name="$1" path="$2"
    if [ -e "$path" ]; then
        green "  PASS: $name"
        PASS=$((PASS + 1))
    else
        red "  FAIL: $name (path not found: $path)"
        FAIL=$((FAIL + 1))
    fi
}

# check_dir <label> <path>
check_dir() {
    local name="$1" path="$2"
    if [ -d "$path" ]; then
        green "  PASS: $name"
        PASS=$((PASS + 1))
    else
        red "  FAIL: $name (directory not found: $path)"
        FAIL=$((FAIL + 1))
    fi
}

# check_bash_syntax <label> <script>
check_bash_syntax() {
    local name="$1" script="$2"
    if bash -n "$script" 2>/dev/null; then
        green "  PASS: $name"
        PASS=$((PASS + 1))
    else
        red "  FAIL: $name (bash syntax error in $script)"
        bash -n "$script" 2>&1 | head -5 >&2
        FAIL=$((FAIL + 1))
    fi
}

# skip_test <label> <reason>
skip_test() {
    yellow "  SKIP: $1 ($2)"
    SKIP=$((SKIP + 1))
}

echo "═══════════════════════════════════════════════════════════════"
echo "  gitbot-fleet — End-to-End Tests"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# ─── Section 1: Repository structure ────────────────────────────────
bold "Section 1: Repository structure"

check_dir  "bots/ directory exists"              "$PROJECT_DIR/bots"
check_dir  "campaigns/ directory exists"          "$PROJECT_DIR/campaigns"
check_dir  "deploy/ directory exists"             "$PROJECT_DIR/deploy"
check_dir  "hooks/ directory exists"              "$PROJECT_DIR/hooks"
check_exists "AI manifest present"               "$PROJECT_DIR/0-AI-MANIFEST.a2ml"
check_exists "fleet-coordinator.sh present"      "$PROJECT_DIR/fleet-coordinator.sh"
check_exists "learning-monitor.sh present"       "$PROJECT_DIR/learning-monitor.sh"
check_exists "Justfile present"                  "$PROJECT_DIR/Justfile"
check_exists "docker-compose.yml present"        "$PROJECT_DIR/docker-compose.yml"

echo ""

# ─── Section 2: Bot directory structure ─────────────────────────────
bold "Section 2: Bot directory structure (expected fleet members)"

EXPECTED_BOTS="rhodibot echidnabot sustainabot panicbot glambot seambot finishingbot accessibilitybot cipherbot"
for bot in $EXPECTED_BOTS; do
    check_dir "bots/$bot directory exists" "$PROJECT_DIR/bots/$bot"
done

echo ""

# ─── Section 3: Shell script syntax validation ───────────────────────
bold "Section 3: Shell script syntax checks"

for script in \
    "$PROJECT_DIR/fleet-coordinator.sh" \
    "$PROJECT_DIR/learning-monitor.sh" \
    "$PROJECT_DIR/run-fleet.sh" \
    "$PROJECT_DIR/setup.sh"; do
    if [ -f "$script" ]; then
        check_bash_syntax "$(basename "$script") syntax valid" "$script"
    else
        skip_test "$(basename "$script") syntax check" "file not present"
    fi
done

# Also check hooks directory for bash scripts
if [ -d "$PROJECT_DIR/hooks" ]; then
    for script in "$PROJECT_DIR/hooks/"*.sh; do
        [ -f "$script" ] || continue
        check_bash_syntax "hooks/$(basename "$script") syntax valid" "$script"
    done
fi

echo ""

# ─── Section 4: fleet-coordinator.sh usage output ───────────────────
bold "Section 4: fleet-coordinator.sh help/usage output"

if [ -f "$PROJECT_DIR/fleet-coordinator.sh" ]; then
    OUTPUT=$(bash "$PROJECT_DIR/fleet-coordinator.sh" --help 2>&1 || bash "$PROJECT_DIR/fleet-coordinator.sh" help 2>&1 || true)
    if [ -z "$OUTPUT" ]; then
        # Try calling with no args — many scripts print usage on no args
        OUTPUT=$(bash "$PROJECT_DIR/fleet-coordinator.sh" 2>&1 || true)
    fi
    check "fleet-coordinator has usage/help text" "USAGE\|Usage\|usage\|COMMAND\|command\|fleet" "$OUTPUT"
    check "fleet-coordinator mentions bots/run-scan" "run-scan\|scan\|bot\|fleet\|deploy" "$OUTPUT"
else
    skip_test "fleet-coordinator.sh help" "script not found"
fi

echo ""

# ─── Section 5: GitHub Actions workflows ────────────────────────────
bold "Section 5: GitHub Actions workflow validity (YAML syntax)"

WORKFLOWS_DIR="$PROJECT_DIR/.github/workflows"
if [ -d "$WORKFLOWS_DIR" ]; then
    WORKFLOW_COUNT=0
    WORKFLOW_FAIL=0
    for wf in "$WORKFLOWS_DIR/"*.yml; do
        [ -f "$wf" ] || continue
        WORKFLOW_COUNT=$((WORKFLOW_COUNT + 1))
        # Validate YAML by checking with python3 (available in CI)
        if command -v python3 >/dev/null 2>&1; then
            if python3 -c "import yaml; yaml.safe_load(open('$wf'))" 2>/dev/null; then
                :
            else
                red "  FAIL: workflow YAML parse failed: $(basename "$wf")"
                FAIL=$((FAIL + 1))
                WORKFLOW_FAIL=$((WORKFLOW_FAIL + 1))
            fi
        fi
        # Check SPDX header
        if grep -q "SPDX-License-Identifier" "$wf" 2>/dev/null; then
            :
        else
            yellow "  WARN: missing SPDX header in $(basename "$wf")"
        fi
    done
    if [ "$WORKFLOW_COUNT" -gt 0 ] && [ "$WORKFLOW_FAIL" -eq 0 ]; then
        green "  PASS: all $WORKFLOW_COUNT workflow YAML files parse successfully"
        PASS=$((PASS + 1))
    elif [ "$WORKFLOW_COUNT" -eq 0 ]; then
        skip_test "workflow YAML validation" "no .yml files in .github/workflows"
    fi
    check "minimum workflow count (>=5)" "[5-9]\|[0-9][0-9]" "$WORKFLOW_COUNT"
else
    skip_test "workflow YAML validation" ".github/workflows not found"
fi

echo ""

# ─── Section 6: bot Cargo.toml manifests ────────────────────────────
bold "Section 6: Bot Cargo manifests"

for bot in rhodibot echidnabot panicbot cipherbot; do
    CARGO="$PROJECT_DIR/bots/$bot/Cargo.toml"
    if [ -f "$CARGO" ]; then
        OUTPUT=$(cat "$CARGO")
        check "$bot Cargo.toml has package name" "name.*$bot\|name.*\"$bot\"" "$OUTPUT"
        check "$bot Cargo.toml has PMPL license" "PMPL\|pmpl" "$OUTPUT"
    else
        skip_test "$bot Cargo.toml" "file not found"
    fi
done

echo ""

# ═══════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════
echo "═══════════════════════════════════════════════════════════════"
printf "  Results: "
green "PASS=$PASS" | tr -d '\n'
echo -n "  "
if [ "$FAIL" -gt 0 ]; then red "FAIL=$FAIL" | tr -d '\n'; else echo -n "FAIL=0"; fi
echo -n "  "
if [ "$SKIP" -gt 0 ]; then yellow "SKIP=$SKIP"; else echo "SKIP=0"; fi
echo "═══════════════════════════════════════════════════════════════"

exit "$FAIL"
