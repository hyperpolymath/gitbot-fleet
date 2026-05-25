#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# fix-tracked-package-lock.sh — Remove tracked package-lock.json (standards#67)
#
# Part of the gitbot-fleet remediation scripts for estate-wide enforcement.
# This script is triggered by the robot-repo-automaton when a Hypatia
# finding of type `cicd_rules/tracked_npm_lockfile` is detected.
#
# What it does:
#   1. Finds all tracked package-lock.json files in the repo.
#   2. Runs `git rm --cached` on each (removing from index, not disk).
#   3. Ensures package-lock.json and **/package-lock.json are in .gitignore.
#
# What it does NOT do:
#   * Does not commit or push — the fleet coordinator handles that.
#   * Does not delete node_modules/ (separate concern).
#   * Does not modify anything outside .gitignore and git index.
#
# Usage (gitbot fleet-coordinator integration):
#   bash fix-tracked-package-lock.sh <REPO_PATH> [FINDING_FILE]
#
# Standalone usage:
#   cd <repo> && bash /path/to/fix-tracked-package-lock.sh .

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=lib/third-party-excludes.sh
source "${SCRIPT_DIR}/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:-.}"
FINDING_FILE="${2:-}"

if [ ! -d "${REPO_PATH}/.git" ]; then
    echo "ERROR: ${REPO_PATH} is not a git repository." >&2
    exit 1
fi

echo "fix-tracked-package-lock: scanning ${REPO_PATH} ..."

# ---------------------------------------------------------------------------
# Step 1: un-track package-lock.json files
# ---------------------------------------------------------------------------

TRACKED=$(git -C "${REPO_PATH}" ls-files \
    'package-lock.json' \
    '**/package-lock.json' \
    2>/dev/null || true)

FIXED_COUNT=0
if [ -n "$TRACKED" ]; then
    while IFS= read -r f; do
        [ -z "$f" ] && continue
        git -C "${REPO_PATH}" rm --cached "$f"
        echo "  [rm-cached] $f"
        FIXED_COUNT=$((FIXED_COUNT + 1))
    done <<< "$TRACKED"
else
    echo "  No tracked package-lock.json files found."
fi

# ---------------------------------------------------------------------------
# Step 2: ensure .gitignore covers package-lock.json
# ---------------------------------------------------------------------------

GITIGNORE="${REPO_PATH}/.gitignore"

# Ensure .gitignore exists
[ -f "$GITIGNORE" ] || touch "$GITIGNORE"

added_gi=false
for entry in "package-lock.json" "**/package-lock.json"; do
    if ! grep -qxF "$entry" "$GITIGNORE" 2>/dev/null; then
        if ! $added_gi; then
            printf '\n# npm-avoidant (standards#67): estate JS-runtime policy is Deno>Bun>pnpm>npm.\n# npm lockfiles must never be committed estate-wide.\n' >> "$GITIGNORE"
            added_gi=true
        fi
        printf '%s\n' "$entry" >> "$GITIGNORE"
        echo "  [gitignore] Added: $entry"
        FIXED_COUNT=$((FIXED_COUNT + 1))
    fi
done

if [ $FIXED_COUNT -eq 0 ]; then
    echo "  Nothing to fix — already clean."
    exit 0
fi

echo "fix-tracked-package-lock: $FIXED_COUNT change(s) applied."
echo "  Refs hyperpolymath/standards#67"
echo "  Next step: review staged changes and commit on a branch."
