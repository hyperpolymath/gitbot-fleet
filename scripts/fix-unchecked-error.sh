#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-unchecked-error.sh — Annotate unchecked error returns
#
# Fixes PA019 UncheckedError findings.
# Adds annotations where function return values are silently discarded.
#
# Usage: fix-unchecked-error.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Unchecked Error Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# --- Rust: Find let _ = expr that discards Results ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changed=false

    # Pattern: let _ = something_that_returns_result
    # This silently discards errors
    if grep -qP 'let\s+_\s*=\s*\S+' "$file" 2>/dev/null; then
        while IFS= read -r line_num; do
            prev=$(sed -n "$((line_num - 1))p" "$file" 2>/dev/null || echo "")
            if ! echo "$prev" | grep -q 'SAFETY\|intentional\|TODO'; then
                sed -i "${line_num}i\\    // TODO: handle error instead of discarding with let _ =" "$file" 2>/dev/null || true
                changed=true
            fi
        done < <(grep -nP 'let\s+_\s*=\s*\S+' "$file" 2>/dev/null | cut -d: -f1 | sort -rn)
    fi

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.rs" \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

# --- Go: Find unchecked errors (err not used after assignment) ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changed=false

    # Pattern: result, _ := someFunc() — discarding error
    if grep -qP ',\s*_\s*:?=\s*\S+\(' "$file" 2>/dev/null; then
        while IFS= read -r line_num; do
            prev=$(sed -n "$((line_num - 1))p" "$file" 2>/dev/null || echo "")
            if ! echo "$prev" | grep -q 'TODO\|intentional\|nolint'; then
                sed -i "${line_num}i\\	// TODO: handle error instead of discarding" "$file" 2>/dev/null || true
                changed=true
            fi
        done < <(grep -nP ',\s*_\s*:?=\s*\S+\(' "$file" 2>/dev/null | cut -d: -f1 | sort -rn)
    fi

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.go" \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

# --- Elixir: Find bare function calls ignoring {:error, _} returns ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changed=false

    # Pattern: File.write(...) without case/with wrapping (ignores {:error, reason})
    for func in "File.write" "File.rm" "File.mkdir" "File.rename" "File.cp"; do
        if grep -qP "^\\s+${func//./\\.}\(" "$file" 2>/dev/null; then
            while IFS= read -r line_num; do
                prev=$(sed -n "$((line_num - 1))p" "$file" 2>/dev/null || echo "")
                if ! echo "$prev" | grep -q 'TODO\|SECURITY\|case\|with'; then
                    sed -i "${line_num}i\\    # TODO: handle {:error, reason} return from ${func}" "$file" 2>/dev/null || true
                    changed=true
                fi
            done < <(grep -nP "^\\s+${func//./\\.}\(" "$file" 2>/dev/null | cut -d: -f1 | sort -rn)
        fi
    done

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.ex" -o -name "*.exs" \) \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Annotated unchecked errors in $FIXED_COUNT file(s)"
else
    echo "No unchecked error patterns found"
fi
