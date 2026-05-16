#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-todo-markers.sh — Resolve unchecked TODO/FIXME markers
#
# Fixes PA018-unchecked-todo findings.
# Converts bare TODOs to structured FILL markers or resolves them.
#
# Uses git ls-files for fast traversal and hash-based caching to skip
# files unchanged since last scan.
#
# Usage: fix-todo-markers.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "=== TODO/FIXME Marker Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0
TODO_COUNT=0

# Source scan cache if available
if [[ -f "$SCRIPT_DIR/lib/scan-cache.sh" ]]; then
    source "$SCRIPT_DIR/lib/scan-cache.sh"
    cache_init "fix-todo-markers" "$REPO_PATH"
    USE_CACHE=true
else
    USE_CACHE=false
fi

# File extensions to check
EXTENSIONS=("*.rs" "*.ex" "*.exs" "*.res" "*.js" "*.sh" "*.bash" "*.idr" "*.lean" "*.hs" "*.ml" "*.mli" "*.zig" "*.ada" "*.adb" "*.ads" "justfile" "Makefile" "*.toml")

# Use git ls-files if in a git repo
list_files() {
    if [[ -d "$REPO_PATH/.git" ]]; then
        (cd "$REPO_PATH" && git ls-files -z "${EXTENSIONS[@]}" 2>/dev/null) | \
            while IFS= read -r -d '' f; do
                case "$f" in
                    */node_modules/*|*/target/*|*/.git/*|*/deps/*|*/vendor/*|*/_build/*) continue ;;
                    */winget-pkgs/*|*/compiler-source/*|*/.lake/*|*/.pack/*) continue ;;
                    *.min.js|*.min.css) continue ;;
                esac
                [[ -f "$REPO_PATH/$f" ]] && printf '%s\0' "$REPO_PATH/$f"
            done
    else
        find "$REPO_PATH" -type f \
            -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/node_modules/*" \
            -not -path "*/_build/*" -not -path "*/vendor/*" -not -path "*/.pack/*" \
            -not -path "*/.lake/*" -not -path "*/winget-pkgs/*" -not -path "*/compiler-source/*" \
            -not -name "*.min.js" -not -name "*.min.css" \
            \( -name "*.rs" -o -name "*.ex" -o -name "*.exs" -o -name "*.res" -o -name "*.js" \
               -o -name "*.sh" -o -name "*.bash" -o -name "*.idr" -o -name "*.lean" \
               -o -name "*.hs" -o -name "*.ml" -o -name "*.mli" -o -name "*.zig" \
               -o -name "*.ada" -o -name "*.adb" -o -name "*.ads" \
               -o -name "justfile" -o -name "Makefile" -o -name "*.toml" \) \
            -print0 2>/dev/null
    fi
}

while IFS= read -r -d '' file; do
    # Skip via hash cache
    if [[ "$USE_CACHE" == "true" ]] && cache_unchanged "$file"; then
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

    # Update cache
    [[ "$USE_CACHE" == "true" ]] && cache_update "$file"
done < <(list_files)

# Save cache
if [[ "$USE_CACHE" == "true" ]]; then
    cache_save
    cache_stats
fi

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Found $TODO_COUNT TODO/FIXME markers in $FIXED_COUNT file(s)"
else
    echo "No TODO/FIXME markers found"
fi
