#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-workflow-permissions.sh — Add permissions: read-all to GitHub Actions workflows
#
# Fixes Scorecard SC-018 (Token-Permissions) by adding top-level
# permissions: read-all to workflow files that lack a permissions declaration.
#
# Usage: fix-workflow-permissions.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

WORKFLOWS_DIR="$REPO_PATH/.github/workflows"

echo "=== Workflow Permissions Fix ==="
echo "  Repo: $REPO_PATH"

if [[ ! -d "$WORKFLOWS_DIR" ]]; then
    echo "  No .github/workflows/ directory found"
    exit 0
fi

FIXED_COUNT=0
SKIPPED_COUNT=0

while IFS= read -r -d '' wf; do
    rel_path="${wf#$REPO_PATH/}"

    # Check if workflow already has permissions
    if grep -q '^permissions:' "$wf" 2>/dev/null; then
        ((SKIPPED_COUNT++)) || true
        continue
    fi

    # Insert permissions: read-all after the 'on:' block
    # Find where the 'on:' block ends (next top-level key) and insert before it
    TEMP_FILE=$(mktemp)

    awk '
    BEGIN { added = 0; in_on_block = 0 }
    /^on:/ { in_on_block = 1; print; next }
    in_on_block && /^[a-zA-Z]/ && !/^  / && !/^on:/ {
        print ""
        print "permissions: read-all"
        print ""
        in_on_block = 0
        added = 1
    }
    { print }
    END {
        if (!added && in_on_block) {
            print ""
            print "permissions: read-all"
        }
    }
    ' "$wf" > "$TEMP_FILE"

    mv "$TEMP_FILE" "$wf"
    echo "  FIXED $rel_path"
    ((FIXED_COUNT++)) || true

done < <(find "$WORKFLOWS_DIR" -maxdepth 1 \( -name "*.yml" -o -name "*.yaml" \) -print0 2>/dev/null)

echo ""
echo "Fixed: $FIXED_COUNT workflow(s), Skipped: $SKIPPED_COUNT (already had permissions)"
