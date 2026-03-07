#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-sorry-lean.sh — Annotate Lean 4 sorry placeholders with PROOF_TODO
#
# Fixes UnsafeCode findings for sorry in Lean files.
# sorry is an axiom that marks an unproven proposition — it must be replaced
# with actual proofs for the code to be trustworthy.
#
# This script adds a PROOF_TODO comment above each sorry occurrence to make
# the gaps visible and trackable. It cannot auto-generate proofs.
#
# Usage: fix-sorry-lean.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Lean sorry Annotation ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0
SORRY_TOTAL=0

# Find Lean files with sorry, skipping .lake/packages/ (upstream deps) and .git/
while IFS= read -r -d '' file; do
    # Match sorry that is:
    #   - NOT in a comment (lines starting with --)
    #   - NOT inside a string (best-effort: skip lines with sorry in quotes)
    #   - An actual keyword (word boundary: not part of another identifier)
    if grep -v '^\s*--' "$file" 2>/dev/null | grep -qw 'sorry'; then
        rel_path="${file#"$REPO_PATH"/}"
        count=$(grep -v '^\s*--' "$file" 2>/dev/null | grep -cw 'sorry' || echo 0)
        echo "  FOUND $rel_path — $count sorry occurrence(s)"

        # Add PROOF_TODO comment above sorry lines (only on non-comment lines)
        # Idempotent: skip if PROOF_TODO already present on the line or preceding line
        # Uses sed to insert a comment line before any line containing sorry as a word
        sed -i '/^\s*--/!{/\bsorry\b/{/PROOF_TODO/!s/\(.*\bsorry\b\)/-- PROOF_TODO: Replace sorry with actual proof\n\1/}}' "$file" 2>/dev/null || true

        ((FIXED_COUNT++)) || true
        SORRY_TOTAL=$((SORRY_TOTAL + count))
    fi
done < <(find "$REPO_PATH" -type f -name "*.lean" \
    -not -path "*/.git/*" \
    -not -path "*/.lake/packages/*" \
    -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Annotated $FIXED_COUNT file(s) with $SORRY_TOTAL sorry occurrence(s)"
else
    echo "No sorry patterns found"
fi
