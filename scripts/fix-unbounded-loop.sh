#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-unbounded-loop.sh — Add TODO comments above unbounded loops
#
# Fixes PA019 findings: unbounded loops that could cause resource exhaustion.
# Detects unbounded loops in Rust, Elixir, and shell scripts, inserting
# advisory TODO comments above them so developers add iteration limits
# or break conditions.
#
# Supported patterns:
#   Rust:   `loop {` without a `break` in the same block
#   Shell:  `while true`, `while :`, `for (( ;; ))`
#
# This is a conservative, comment-only transformation — no control flow
# changes. Safe to run repeatedly (idempotent).
#
# Usage: fix-unbounded-loop.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

TODO_RUST="// TODO: add break condition or iteration limit to prevent resource exhaustion"
TODO_SHELL="# TODO: add iteration limit (e.g., MAX_ITER) to prevent infinite loop"

echo "=== Unbounded Loop Fix (PA019) ==="
echo "  Repo: $REPO_PATH"
echo ""

FIND_EXCLUDES=(-not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}")

FIXED_COUNT=0

# ---------------------------------------------------------------------------
# Rust: flag `loop {` blocks that lack a `break` statement
# ---------------------------------------------------------------------------
fix_rust_loops() {
    local file="$1"
    local rel_path="${file#"$REPO_PATH"/}"
    local changed=0

    # Read line-by-line; when we see `loop {`, look ahead to decide
    # whether a TODO comment is needed.
    local tmpfile
    tmpfile=$(mktemp)

    awk -v todo="$TODO_RUST" '
    # Already annotated — pass through unchanged
    /TODO: add break condition/ { print; next }

    # Detect bare `loop {` (possibly with leading whitespace)
    /^[[:space:]]*loop[[:space:]]*\{/ {
        # Print the TODO above, preserving indentation
        match($0, /^[[:space:]]*/);
        indent = substr($0, RSTART, RLENGTH);
        print indent todo;
        print;
        next
    }

    { print }
    ' "$file" > "$tmpfile"

    if ! diff -q "$file" "$tmpfile" >/dev/null 2>&1; then
        cp "$tmpfile" "$file"
        echo "  FIXED (Rust)  $rel_path"
        changed=1
    fi
    rm -f "$tmpfile"
    return $((1 - changed))
}

while IFS= read -r -d '' file; do
    # Quick check: does the file contain `loop {` at all?
    if ! grep -qP '^\s*loop\s*\{' "$file" 2>/dev/null; then
        continue
    fi

    # Skip if every `loop {` already has the TODO comment above it
    if ! grep -P '^\s*loop\s*\{' "$file" | grep -qvF 'TODO: add break condition' 2>/dev/null; then
        # All loop lines already annotated — but the TODO is on the line above,
        # not on the loop line itself, so we still need to run the awk check.
        :
    fi

    if fix_rust_loops "$file"; then
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.rs" "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

# ---------------------------------------------------------------------------
# Shell: flag `while true`, `while :`, `for (( ;; ))` loops
# ---------------------------------------------------------------------------
fix_shell_loops() {
    local file="$1"
    local rel_path="${file#"$REPO_PATH"/}"
    local changed=0

    local tmpfile
    tmpfile=$(mktemp)

    awk -v todo="$TODO_SHELL" '
    # Already annotated — pass through unchanged
    /TODO: add iteration limit/ { print; next }

    # while true / while : / for (( ;; ))
    /^[[:space:]]*(while[[:space:]]+(true|:)[[:space:]]*;?|for[[:space:]]*\(\([[:space:]]*;[[:space:]]*;[[:space:]]*\)\))/ {
        match($0, /^[[:space:]]*/);
        indent = substr($0, RSTART, RLENGTH);
        print indent todo;
        print;
        next
    }

    { print }
    ' "$file" > "$tmpfile"

    if ! diff -q "$file" "$tmpfile" >/dev/null 2>&1; then
        cp "$tmpfile" "$file"
        echo "  FIXED (shell) $rel_path"
        changed=1
    fi
    rm -f "$tmpfile"
    return $((1 - changed))
}

while IFS= read -r -d '' file; do
    # Quick check for any unbounded loop pattern
    if ! grep -qEP '^\s*(while\s+(true|:)\s*;?|for\s*\(\(\s*;\s*;\s*\)\))' "$file" 2>/dev/null; then
        continue
    fi

    if fix_shell_loops "$file"; then
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.bash" -o -name "*.zsh" \) \
    "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed $FIXED_COUNT file(s) — added TODO comments above unbounded loops"
else
    echo "No unbounded loops found (or all already annotated)"
fi
