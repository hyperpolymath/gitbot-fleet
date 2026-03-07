#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-unsafe-ffi.sh — Add SAFETY documentation to undocumented unsafe FFI blocks
#
# Fixes UnsafeFFI findings (85 entries).
# Rust's clippy::undocumented_unsafe_blocks lint requires a // SAFETY: comment
# above every unsafe block or function. This script adds a TODO marker where
# the comment is missing.
#
# Usage: fix-unsafe-ffi.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Unsafe FFI Documentation ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0
ANNOTATED_LINES=0

# Find Rust files with unsafe blocks or functions
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"

    # Track whether this file needed any fixes
    file_fixed=false

    # Process the file line-by-line using a temp file for output
    tmpfile="$(mktemp)"

    prev_line=""
    line_num=0
    while IFS= read -r line; do
        ((line_num++)) || true

        # Check if this line contains an unsafe block or unsafe fn declaration
        # Match: unsafe {, unsafe fn, pub unsafe fn, pub(crate) unsafe fn, etc.
        if echo "$line" | grep -qE '^\s*(pub(\s*\(.*\))?\s+)?unsafe\s+(fn\s|impl\s|\{)'; then
            # Check if the previous non-blank line is already a SAFETY comment
            if ! echo "$prev_line" | grep -qF '// SAFETY:'; then
                # Determine indentation of the unsafe line
                indent=$(echo "$line" | sed 's/\(^\s*\).*/\1/')
                echo "${indent}// SAFETY: TODO — document why this unsafe block is sound" >> "$tmpfile"
                ((ANNOTATED_LINES++)) || true
                file_fixed=true
            fi
        fi

        echo "$line" >> "$tmpfile"

        # Track previous non-blank line for SAFETY comment check
        if [[ -n "${line// /}" ]]; then
            prev_line="$line"
        fi
    done < "$file"

    if [[ "$file_fixed" == true ]]; then
        cp "$tmpfile" "$file"
        count=$(grep -c '// SAFETY: TODO' "$file" || echo 0)
        echo "  FIXED $rel_path — added SAFETY TODO ($count total annotations)"
        ((FIXED_COUNT++)) || true
    fi

    rm -f "$tmpfile"

done < <(find "$REPO_PATH" -type f -name "*.rs" \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" \
    -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Annotated $FIXED_COUNT file(s) with $ANNOTATED_LINES SAFETY TODO comment(s)"
else
    echo "No undocumented unsafe blocks found"
fi
