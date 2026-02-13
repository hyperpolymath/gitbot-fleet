#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-believe-me.sh — Flag or remove Idris2 believe_me calls
#
# Fixes PA004-idris-believe-me findings.
# believe_me bypasses the type checker — it should be replaced with proper proofs.
#
# Usage: fix-believe-me.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== believe_me Removal ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# Find Idris2 files with believe_me
while IFS= read -r -d '' file; do
    # Only match actual believe_me calls, NOT lines that are already TODO comments
    if grep -v '^\s*--' "$file" 2>/dev/null | grep -q 'believe_me'; then
        rel_path="${file#$REPO_PATH/}"
        count=$(grep -v '^\s*--' "$file" 2>/dev/null | grep -c 'believe_me' || echo 0)
        echo "  FOUND $rel_path — $count believe_me call(s)"

        # Add TODO comments above believe_me calls (only on non-comment lines)
        # Use a unique marker so re-runs are idempotent
        sed -i '/^\s*--/!{/believe_me/{/PROOF_TODO/!s/\(.*believe_me\)/-- PROOF_TODO: Replace believe_me with actual proof\n\1/}}' "$file" 2>/dev/null || true

        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.idr" -not -path "*/\.git/*" -not -path "*/.pack/*" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Annotated $FIXED_COUNT file(s) with proof TODOs"
else
    echo "No believe_me patterns found"
fi
