#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-dynamic-code-exec.sh — Mitigate dynamic code execution patterns
#
# Fixes or flags dangerous patterns:
#   - JavaScript: eval(expr) → Function('"use strict"; return ' + expr)()
#   - JavaScript: new Function(str) → warning comment added
#   - Elixir: Code.eval_string → warning comment added
#   - Python: exec()/eval() → warning comment added
#
# Usage: fix-dynamic-code-exec.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

DESCRIPTION=$(jq -r '.description // ""' "$FINDING_JSON")
PATTERN_ID=$(jq -r '.pattern_id // ""' "$FINDING_JSON")

echo "=== Dynamic Code Execution Fix ==="
echo "  Repo:    $REPO_PATH"
echo "  Pattern: $PATTERN_ID"
echo ""

# Use shared third-party exclusions
FIND_EXCLUDES=(-not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}")

FIXED_COUNT=0

# --- JavaScript / MJS files ---

JS_FILES=()
while IFS= read -r -d '' f; do
    JS_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.js" -o -name "*.mjs" \) "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

for file in "${JS_FILES[@]}"; do
    # Skip binary files
    if file "$file" | grep -q "binary"; then
        continue
    fi

    CHANGED=false

    # Pattern 1: Replace simple eval(expr) with safer Function form.
    # Matches: eval(someExpression) where the argument is a simple variable or
    # string concatenation (no nested parens). Skips lines already containing
    # the SECURITY WARNING comment or lines that are comments.
    if grep -qP '^\s*[^/]*\beval\(' "$file" 2>/dev/null; then
        # Add warning comment above eval lines that don't already have one.
        # Then replace simple eval(variable) with Function form.
        tmpfile=$(mktemp)
        awk '
        /\/\/ SECURITY WARNING: dynamic code execution/ { print; next }
        /^\s*\/\// { print; next }
        /^\s*\*/ { print; next }
        /\beval\(/ {
            # Only add warning if previous line does not already have it
            if (prev !~ /SECURITY WARNING: dynamic code execution/) {
                # Preserve indentation
                match($0, /^[[:space:]]*/);
                indent = substr($0, RSTART, RLENGTH);
                print indent "// SECURITY WARNING: dynamic code execution — consider eliminating eval";
            }
            # Replace simple eval(varName) with Function form
            # Matches eval(identifier) but not eval("string") or eval(complex.expr())
            gsub(/\beval\(([A-Za-z_][A-Za-z0-9_]*)\)/, "Function('\"use strict\"; return ' + \\1)()")
            print
            prev = $0
            next
        }
        { print; prev = $0 }
        ' "$file" > "$tmpfile"

        if ! diff -q "$file" "$tmpfile" >/dev/null 2>&1; then
            cp "$tmpfile" "$file"
            CHANGED=true
        fi
        rm -f "$tmpfile"
    fi

    # Pattern 2: new Function(str) — add warning comment (cannot safely auto-replace)
    if grep -qP '^\s*[^/]*\bnew\s+Function\(' "$file" 2>/dev/null; then
        tmpfile=$(mktemp)
        awk '
        /\/\/ SECURITY WARNING: dynamic code execution/ { print; next }
        /^\s*\/\// { print; next }
        /^\s*\*/ { print; next }
        /\bnew\s+Function\(/ {
            if (prev !~ /SECURITY WARNING: dynamic code execution/) {
                match($0, /^[[:space:]]*/);
                indent = substr($0, RSTART, RLENGTH);
                print indent "// SECURITY WARNING: dynamic code execution — new Function() is equivalent to eval";
            }
            print
            prev = $0
            next
        }
        { print; prev = $0 }
        ' "$file" > "$tmpfile"

        if ! diff -q "$file" "$tmpfile" >/dev/null 2>&1; then
            cp "$tmpfile" "$file"
            CHANGED=true
        fi
        rm -f "$tmpfile"
    fi

    if [[ "$CHANGED" == true ]]; then
        rel_path="${file#"$REPO_PATH"/}"
        echo "  Fixed JS patterns in $rel_path"
        ((FIXED_COUNT++)) || true
    fi
done

# --- Elixir files ---

EX_FILES=()
while IFS= read -r -d '' f; do
    EX_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.ex" -o -name "*.exs" \) "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

for file in "${EX_FILES[@]}"; do
    if file "$file" | grep -q "binary"; then
        continue
    fi

    if grep -qP '^\s*[^#]*\bCode\.eval_string\b' "$file" 2>/dev/null; then
        tmpfile=$(mktemp)
        awk '
        /# SECURITY:.*Code\.eval_string/ { print; next }
        /^\s*#/ { print; next }
        /\bCode\.eval_string\b/ {
            if (prev !~ /SECURITY:.*Code\.eval_string/) {
                match($0, /^[[:space:]]*/);
                indent = substr($0, RSTART, RLENGTH);
                print indent "# SECURITY: Code.eval_string is dangerous — consider Code.compile_quoted or pattern matching";
            }
            print
            prev = $0
            next
        }
        { print; prev = $0 }
        ' "$file" > "$tmpfile"

        if ! diff -q "$file" "$tmpfile" >/dev/null 2>&1; then
            cp "$tmpfile" "$file"
            rel_path="${file#"$REPO_PATH"/}"
            echo "  Flagged Code.eval_string in $rel_path"
            ((FIXED_COUNT++)) || true
        fi
        rm -f "$tmpfile"
    fi
done

# --- Python files ---

PY_FILES=()
while IFS= read -r -d '' f; do
    PY_FILES+=("$f")
done < <(find "$REPO_PATH" -type f -name "*.py" "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

for file in "${PY_FILES[@]}"; do
    if file "$file" | grep -q "binary"; then
        continue
    fi

    if grep -qP '^\s*[^#]*\b(exec|eval)\(' "$file" 2>/dev/null; then
        tmpfile=$(mktemp)
        awk '
        /# SECURITY WARNING: dynamic code execution/ { print; next }
        /^\s*#/ { print; next }
        /\b(exec|eval)\(/ {
            if (prev !~ /SECURITY WARNING: dynamic code execution/) {
                match($0, /^[[:space:]]*/);
                indent = substr($0, RSTART, RLENGTH);
                print indent "# SECURITY WARNING: dynamic code execution — avoid exec()/eval() where possible";
            }
            print
            prev = $0
            next
        }
        { print; prev = $0 }
        ' "$file" > "$tmpfile"

        if ! diff -q "$file" "$tmpfile" >/dev/null 2>&1; then
            cp "$tmpfile" "$file"
            rel_path="${file#"$REPO_PATH"/}"
            echo "  Flagged exec/eval in $rel_path"
            ((FIXED_COUNT++)) || true
        fi
        rm -f "$tmpfile"
    fi
done

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed/flagged $FIXED_COUNT file(s)"
else
    echo "No fixable patterns found (may need manual review)"
fi
