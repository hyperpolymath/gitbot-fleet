#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Run PCC verification as a gitbot-fleet bot check.
# Called by fleet-coordinator.sh for repos that have a tools/pcc/ directory.
#
# Usage: run-panel-check.sh <repo-path> [bot-name] [output-dir]
#
# Arguments:
#   repo-path   - Path to the repository containing tools/pcc/.
#   bot-name    - Bot identity for logging (default: seambot).
#   output-dir  - Directory for findings output (default: findings).
#
# Exit codes:
#   0 - All panels are releasable (or no PCC binary found).
#   1 - One or more panels are not releasable, or PCC produced no output.
set -euo pipefail

REPO_PATH="${1:?Usage: run-panel-check.sh <repo-path>}"
BOT_NAME="${2:-seambot}"
OUTPUT_DIR="${3:-findings}"

echo "[${BOT_NAME}] Running panel verification on ${REPO_PATH}..."

# Find PCC binary — prefer release build, fall back to debug.
PCC_BIN=""
if [[ -x "${REPO_PATH}/tools/pcc/target/release/panll" ]]; then
    PCC_BIN="${REPO_PATH}/tools/pcc/target/release/panll"
elif [[ -x "${REPO_PATH}/tools/pcc/target/debug/panll" ]]; then
    PCC_BIN="${REPO_PATH}/tools/pcc/target/debug/panll"
else
    echo "[${BOT_NAME}] No PCC binary found, skipping panel check"
    exit 0
fi

# Run PCC verify with JSON output.
RESULT=$("${PCC_BIN}" panel verify --json --repo-root "${REPO_PATH}" 2>/dev/null || true)

if [[ -z "${RESULT}" ]]; then
    echo "[${BOT_NAME}] PCC returned empty output"
    exit 1
fi

# Write findings to timestamped file with a "latest" symlink.
REPO_NAME=$(basename "${REPO_PATH}")
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
FINDINGS_DIR="${OUTPUT_DIR}/${REPO_NAME}"
mkdir -p "${FINDINGS_DIR}"

echo "${RESULT}" | jq '.' > "${FINDINGS_DIR}/panel-check-${TIMESTAMP}.json"
ln -sf "panel-check-${TIMESTAMP}.json" "${FINDINGS_DIR}/latest-panel-check.json"

# Summarise: count total panels and how many are releasable.
TOTAL=$(echo "${RESULT}" | jq 'if type == "array" then length else 1 end')
RELEASABLE=$(echo "${RESULT}" | jq 'if type == "array" then [.[] | select(.state == "releasable")] | length else (if .state == "releasable" then 1 else 0 end) end')

echo "[${BOT_NAME}] Panel check complete: ${RELEASABLE}/${TOTAL} releasable"

if [[ "${RELEASABLE}" -lt "${TOTAL}" ]]; then
    exit 1
fi
