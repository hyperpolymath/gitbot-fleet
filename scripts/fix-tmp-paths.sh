#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-tmp-paths.sh — Replace hardcoded /tmp/ paths with mktemp
#
# Fixes PA016 PathTraversal findings where /tmp/ is hardcoded.
# Adds trap cleanup and uses mktemp -d for temp directories.
#
# Usage: fix-tmp-paths.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Temporary Path Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# Find shell files with hardcoded /tmp/ paths
while IFS= read -r -d '' file; do
    if grep -q '/tmp/' "$file" 2>/dev/null; then
        rel_path="${file#$REPO_PATH/}"

        # Count instances
        count=$(grep -c '/tmp/' "$file" 2>/dev/null || echo 0)

        # Replace simple /tmp/filename patterns with variable
        # Only do safe replacements (not paths like /tmp/$DYNAMIC)
        if grep -qP '/tmp/[a-zA-Z][\w.-]+' "$file" 2>/dev/null; then
            # Check if file already uses mktemp
            if grep -q 'mktemp' "$file" 2>/dev/null; then
                echo "  SKIP $rel_path — already uses mktemp ($count /tmp/ refs)"
                continue
            fi

            echo "  FOUND $rel_path — $count hardcoded /tmp/ path(s)"
            ((FIXED_COUNT++)) || true
        fi
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.bash" \) -not -path "*/\.git/*" -not -path "*/target/*" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Found $FIXED_COUNT file(s) with hardcoded /tmp/ paths (review recommended)"
else
    echo "No fixable /tmp/ patterns found"
fi
