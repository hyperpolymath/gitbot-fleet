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

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Temporary Path Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# Find shell files with hardcoded /tmp/ paths
while IFS= read -r -d '' file; do
    # Skip binary files
    if file "$file" | grep -q "binary"; then
        continue
    fi

    # Only match static /tmp/name patterns (not /tmp/$VAR or /tmp/${VAR})
    if grep -qP '/tmp/[a-zA-Z][\w.-]+' "$file" 2>/dev/null; then
        rel_path="${file#$REPO_PATH/}"

        # Skip files that already use mktemp properly
        if grep -q 'mktemp' "$file" 2>/dev/null; then
            echo "  SKIP $rel_path — already uses mktemp"
            continue
        fi

        count=$(grep -cP '/tmp/[a-zA-Z][\w.-]+' "$file" 2>/dev/null || echo 0)

        # Check if file already has our TMPDIR block (idempotent)
        if grep -q 'HYPATIA_TMPDIR' "$file" 2>/dev/null; then
            echo "  SKIP $rel_path — already patched"
            continue
        fi

        # Strategy: Add a mktemp block after set -e (or shebang) and replace /tmp/X with $HYPATIA_TMPDIR/X
        # Step 1: Extract all static /tmp/filename patterns
        tmp_names=$(grep -oP '/tmp/\K[a-zA-Z][\w.-]+' "$file" 2>/dev/null | sort -u)

        if [[ -z "$tmp_names" ]]; then
            continue
        fi

        # Step 2: Insert mktemp block after the first 'set -' line or after shebang
        MKTEMP_BLOCK='# Auto-generated temp dir (replaces hardcoded /tmp/)
HYPATIA_TMPDIR="$(mktemp -d)"
trap '\''rm -rf "$HYPATIA_TMPDIR"'\'' EXIT'

        if grep -q '^set -' "$file"; then
            # Insert after the 'set -' line
            sed -i "/^set -/a\\
\\
${MKTEMP_BLOCK}" "$file" 2>/dev/null || true
        else
            # Insert after shebang
            sed -i "1a\\
\\
${MKTEMP_BLOCK}" "$file" 2>/dev/null || true
        fi

        # Step 3: Replace /tmp/name with $HYPATIA_TMPDIR/name for each static name
        while IFS= read -r name; do
            sed -i "s|/tmp/${name}|\"\$HYPATIA_TMPDIR/${name}\"|g" "$file" 2>/dev/null || true
        done <<< "$tmp_names"

        echo "  FIXED $rel_path — replaced $count hardcoded /tmp/ path(s) with mktemp"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.bash" \) -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed $FIXED_COUNT file(s) — hardcoded /tmp/ replaced with mktemp"
else
    echo "No fixable /tmp/ patterns found"
fi
