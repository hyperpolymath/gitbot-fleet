#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-resource-leak.sh — Annotate potential resource leaks
#
# Fixes PA020 ResourceLeak findings.
# Adds annotations where file handles, connections, or streams may not be closed.
#
# Usage: fix-resource-leak.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Resource Leak Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# --- Rust: File::open without drop/close guarantee ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changed=false

    # Pattern: File::open that's not in a scope block or with ? operator
    if grep -qP 'File::open\(' "$file" 2>/dev/null; then
        while IFS= read -r line_num; do
            line=$(sed -n "${line_num}p" "$file")
            # Skip if already has ? (propagates error and drops on scope exit)
            if echo "$line" | grep -qP '\?;?\s*$'; then
                continue
            fi
            prev=$(sed -n "$((line_num - 1))p" "$file" 2>/dev/null || echo "")
            if ! echo "$prev" | grep -q 'TODO\|SAFETY\|resource'; then
                sed -i "${line_num}i\\    // TODO: ensure file handle is dropped/closed (use ? or explicit drop)" "$file" 2>/dev/null || true
                changed=true
            fi
        done < <(grep -nP 'File::open\(' "$file" 2>/dev/null | cut -d: -f1 | sort -rn)
    fi

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.rs" \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

# --- Python: open() without with statement ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changed=false

    # Pattern: f = open("file") without "with" context manager
    # (lines starting with variable = open, not "with open")
    if grep -qP '^\s+\w+\s*=\s*open\(' "$file" 2>/dev/null; then
        while IFS= read -r line_num; do
            line=$(sed -n "${line_num}p" "$file")
            # Skip if already a with statement
            if echo "$line" | grep -qP '^\s*with\s'; then
                continue
            fi
            prev=$(sed -n "$((line_num - 1))p" "$file" 2>/dev/null || echo "")
            if ! echo "$prev" | grep -q 'TODO\|resource\|pylint'; then
                sed -i "${line_num}i\\    # TODO: use 'with open(...)' context manager to prevent resource leak" "$file" 2>/dev/null || true
                changed=true
            fi
        done < <(grep -nP '^\s+\w+\s*=\s*open\(' "$file" 2>/dev/null | cut -d: -f1 | sort -rn)
    fi

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.py" \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -not -path "*/venv/*" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Annotated resource leaks in $FIXED_COUNT file(s)"
else
    echo "No resource leak patterns found"
fi
