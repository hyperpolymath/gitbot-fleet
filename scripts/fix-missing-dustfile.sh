#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-missing-dustfile.sh — Add Dustfile.a2ml to repositories that lack one
#
# Fixes MissingDustfile by creating .machine_readable/contractiles/dust/Dustfile.a2ml
# with cleanup and hygiene contract declarations.
#
# Usage: fix-missing-dustfile.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

TARGET_DIR="$REPO_PATH/.machine_readable/contractiles/dust"
TARGET_FILE="$TARGET_DIR/Dustfile.a2ml"

echo "=== Missing Dustfile Fix ==="
echo "  Repo: $REPO_PATH"

# Idempotency: skip if already exists
if [[ -f "$TARGET_FILE" ]]; then
    echo "  SKIP: Dustfile.a2ml already exists"
    exit 0
fi

# Create parent directories
mkdir -p "$TARGET_DIR"

# Write template content
cat > "$TARGET_FILE" <<'DUSTFILE'
# SPDX-License-Identifier: PMPL-1.0-or-later
# Dustfile — Cleanup and Hygiene Contract

[dustfile]
version = "1.0.0"
format = "a2ml"

[cleanup]
stale-branch-policy = "delete-after-merge"
artifact-retention = "90-days"
cache-policy = "clear-on-release"

[hygiene]
linting = "required"
formatting = "required"
dead-code-removal = "encouraged"
todo-tracking = "tracked-in-issues"

[reversibility]
backup-before-destructive = true
rollback-mechanism = "git-revert"
data-retention-policy = "preserve-30-days"
DUSTFILE

echo "  CREATED .machine_readable/contractiles/dust/Dustfile.a2ml"
echo ""
echo "Fixed: 1 file created"
exit 0
