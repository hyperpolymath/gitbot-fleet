#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# post-scan.sh — Post-scan hook for gitbot-fleet
#
# Called automatically by run-fleet.sh after a scan completes.
# If critical findings are detected, triggers immediate fix dispatch.
#
# Arguments:
#   $1  Number of critical findings
#   $2  Number of high findings
#   $3  Number of repos scanned
#
# Logs triggers to shared-context/learning/triggers.jsonl

set -euo pipefail

FLEET_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LEARNING_DIR="$FLEET_DIR/shared-context/learning"
TRIGGERS_LOG="$LEARNING_DIR/triggers.jsonl"

mkdir -p "$LEARNING_DIR"

CRITICAL="${1:-0}"
HIGH="${2:-0}"
REPOS_SCANNED="${3:-0}"
TIMESTAMP="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

# Log the scan completion trigger
jq -n \
    --arg ts "$TIMESTAMP" \
    --argjson critical "$CRITICAL" \
    --argjson high "$HIGH" \
    --argjson repos "$REPOS_SCANNED" \
    --arg event "scan_complete" \
    '{timestamp: $ts, event: $event, critical: $critical, high: $high, repos_scanned: $repos}' \
    >> "$TRIGGERS_LOG"

# If critical findings exist, immediately trigger fix for critical severity
if [[ "$CRITICAL" -gt 0 ]]; then
    echo "[post-scan] $CRITICAL critical findings detected -- triggering critical fix dispatch"

    jq -n \
        --arg ts "$TIMESTAMP" \
        --argjson critical "$CRITICAL" \
        --arg event "critical_autofix_trigger" \
        --arg action "run-fleet.sh fix --severity critical" \
        '{timestamp: $ts, event: $event, critical_count: $critical, action: $action}' \
        >> "$TRIGGERS_LOG"

    # Run the fix command for critical severity only (dry run -- safety first)
    # To auto-apply, change to: --apply --severity critical
    "$FLEET_DIR/run-fleet.sh" fix --severity critical
else
    echo "[post-scan] No critical findings. $HIGH high-severity findings across $REPOS_SCANNED repos."
fi
