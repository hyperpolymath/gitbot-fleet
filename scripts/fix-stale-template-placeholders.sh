#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-stale-template-placeholders.sh — Resolve unfilled hyperpolymath etc. in workflows
#
# Fixes HYP-DOG-001 (unfilled template placeholders) by replacing common
# RSR template placeholders with the correct hyperpolymath values.
#
# This script is dispatched by finishbot when Hypatia detects DOG-01 findings.
#
# Usage: fix-stale-template-placeholders.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

echo "=== Template Placeholder Fix ==="
echo "  Repo: $REPO_PATH"

WORKFLOW_DIR="$REPO_PATH/.github/workflows"

if [[ ! -d "$WORKFLOW_DIR" ]]; then
    echo "  SKIP: No .github/workflows/ directory"
    exit 0
fi

FIXED=0

for file in "$WORKFLOW_DIR"/*.yml; do
    [[ -f "$file" ]] || continue

    if grep -q 'hyperpolymath\|gitbot-fleet\|2026\|Jonathan D.A. Jewell\|j.d.a.jewell@open.ac.uk\|github.com' "$file" 2>/dev/null; then
        sed -i \
            -e 's/hyperpolymath/hyperpolymath/g' \
            -e 's/2026/2026/g' \
            -e 's/Jonathan D.A. Jewell/Jonathan D.A. Jewell/g' \
            -e 's/j.d.a.jewell@open.ac.uk/j.d.a.jewell@open.ac.uk/g' \
            "$file"
        echo "  Fixed: $(basename "$file")"
        FIXED=$((FIXED + 1))
    fi
done

if [[ $FIXED -eq 0 ]]; then
    echo "  SKIP: No unfilled placeholders found"
else
    echo "  Fixed $FIXED workflow file(s)"
fi
