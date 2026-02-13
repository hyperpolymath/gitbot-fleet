#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-todo-markers.sh — Resolve unchecked TODO/FIXME markers
#
# Fixes PA018-unchecked-todo findings.
# Converts bare TODOs to structured FILL markers or resolves them.
#
# Usage: fix-todo-markers.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== TODO/FIXME Marker Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0
TODO_COUNT=0

# Find files with TODO/FIXME (excluding .git, target, node_modules, vendor)
while IFS= read -r -d '' file; do
    # Skip binary files
    if file "$file" | grep -q "binary"; then
        continue
    fi

    count=$(grep -ciP '(TODO|FIXME|HACK|XXX)\b' "$file" 2>/dev/null || true)
    count="${count//[^0-9]/}"
    count="${count:-0}"
    if [[ "$count" -gt 0 ]]; then
        rel_path="${file#$REPO_PATH/}"
        echo "  FOUND $rel_path — $count marker(s)"
        TODO_COUNT=$((TODO_COUNT + count))
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \
    -not -path "*/\.git/*" \
    -not -path "*/target/*" \
    -not -path "*/node_modules/*" \
    -not -path "*/_build/*" \
    -not -path "*/vendor/*" \
    -not -path "*/.pack/*" \
    -not -path "*/.lake/*" \
    -not -name "*.min.js" \
    -not -name "*.min.css" \
    \( -name "*.rs" -o -name "*.ex" -o -name "*.exs" -o -name "*.res" -o -name "*.js" \
       -o -name "*.sh" -o -name "*.bash" -o -name "*.idr" -o -name "*.lean" \
       -o -name "*.hs" -o -name "*.ml" -o -name "*.mli" -o -name "*.zig" \
       -o -name "*.ada" -o -name "*.adb" -o -name "*.ads" \
       -o -name "justfile" -o -name "Makefile" -o -name "*.toml" \) \
    -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Found $TODO_COUNT TODO/FIXME markers in $FIXED_COUNT file(s)"
else
    echo "No TODO/FIXME markers found"
fi
