#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-atom-exhaustion.sh — Replace String.to_atom with String.to_existing_atom
#
# String.to_atom/1 creates new atoms at runtime, which can exhaust the BEAM
# atom table (limited, non-garbage-collected). String.to_existing_atom/1 only
# converts strings to atoms that already exist in the table, preventing
# exhaustion attacks from untrusted input.
#
# This is a safe, mechanical replacement — the function signatures are
# identical; only the safety semantics differ.
#
# Usage: fix-atom-exhaustion.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

DESCRIPTION=$(jq -r '.description // ""' "$FINDING_JSON")
PATTERN_ID=$(jq -r '.pattern_id // ""' "$FINDING_JSON")

echo "=== Atom Exhaustion Fix ==="
echo "  Repo:    $REPO_PATH"
echo "  Pattern: $PATTERN_ID"
echo ""

# Use shared third-party exclusions
FIND_EXCLUDES=(-not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}")

# Find all Elixir source files
EX_FILES=()
while IFS= read -r -d '' f; do
    EX_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.ex" -o -name "*.exs" \) "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

if [[ ${#EX_FILES[@]} -eq 0 ]]; then
    echo "  No Elixir files found in $REPO_PATH"
    exit 0
fi

FIXED_COUNT=0

for file in "${EX_FILES[@]}"; do
    # Skip binary files
    if file "$file" | grep -q "binary"; then
        continue
    fi

    # Check if file contains String.to_atom (outside of comments)
    if ! grep -qP '^\s*[^#]*\bString\.to_atom\(' "$file" 2>/dev/null; then
        continue
    fi

    # Replace String.to_atom( with String.to_existing_atom(
    # Only on lines that are not comments (lines where # appears before the match)
    tmpfile=$(mktemp)
    awk '
    /^\s*#/ { print; next }
    /\bString\.to_atom\(/ {
        gsub(/\bString\.to_atom\(/, "String.to_existing_atom(")
        print
        next
    }
    { print }
    ' "$file" > "$tmpfile"

    if ! diff -q "$file" "$tmpfile" >/dev/null 2>&1; then
        cp "$tmpfile" "$file"
        rel_path="${file#"$REPO_PATH"/}"
        echo "  Fixed String.to_atom → String.to_existing_atom in $rel_path"
        ((FIXED_COUNT++)) || true
    fi
    rm -f "$tmpfile"
done

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed $FIXED_COUNT file(s)"
else
    echo "No String.to_atom patterns found (already safe or only in comments)"
fi
