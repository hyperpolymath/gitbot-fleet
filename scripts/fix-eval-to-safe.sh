#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-eval-to-safe.sh — Annotate dangerous eval/exec and pipe-to-shell patterns
#
# Fixes DynamicCodeExecution findings in shell scripts:
#   - eval "$var" — annotate with security warning
#   - curl ... | bash/sh — annotate with download-then-verify guidance
#
# These patterns cannot be safely auto-replaced, so this script adds
# prominent warning comments to make the risks visible and trackable.
#
# Usage: fix-eval-to-safe.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Dynamic Code Execution Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# Directories to skip
SKIP_DIRS=( ".git" "node_modules" "target" "_build" "vendor" )

FIND_EXCLUDES=()
for dir in "${SKIP_DIRS[@]}"; do
    FIND_EXCLUDES+=( -not -path "*/${dir}/*" )
done

# --- Shell files (.sh, .bash) ---
while IFS= read -r -d '' file; do
    changed=false

    # --- Fix 1: eval statements ---
    # Annotate eval calls that are not already marked
    if grep -v '^\s*#' "$file" 2>/dev/null | grep -qw 'eval'; then
        if ! grep -q 'SECURITY.*eval' "$file" 2>/dev/null; then
            sed -i '/^\s*#/!{/\beval\b/{/SECURITY/!s/\(.*\beval\b\)/# SECURITY: eval is unsafe with untrusted input — refactor to avoid dynamic code execution\n\1/}}' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    # --- Fix 2: curl|bash / curl|sh / wget|bash / wget|sh pipe-to-shell ---
    # Match patterns like: curl ... | bash, curl ... | sh, wget ... | bash
    # Also handles variants with sudo, env, etc.
    if grep -v '^\s*#' "$file" 2>/dev/null | grep -qE '(curl|wget)\s+.*\|\s*(sudo\s+)?(ba)?sh'; then
        if ! grep -q 'SECURITY.*pipe-to-shell' "$file" 2>/dev/null; then
            sed -i '/^\s*#/!{/\(curl\|wget\).*|.*sh/{/SECURITY/!s/\(.*\(curl\|wget\).*|.*sh\)/# SECURITY: pipe-to-shell is unsafe — download to file, verify checksum, then execute\n# Example: _SCRIPT=$(mktemp); curl -fsSL "URL" -o "$_SCRIPT"; sha256sum -c <<<"HASH  $_SCRIPT"; bash "$_SCRIPT"; rm -f "$_SCRIPT"\n\1/}}' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    if [[ "$changed" == true ]]; then
        rel_path="${file#"$REPO_PATH"/}"
        echo "  FIXED $rel_path (shell eval/pipe-to-shell)"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.bash" \) "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

# --- Also scan files without extension that have a shell shebang ---
while IFS= read -r -d '' file; do
    # Check if file has a bash/sh shebang
    head_line=$(head -1 "$file" 2>/dev/null || true)
    if [[ "$head_line" != *"#!/"*"sh"* ]] && [[ "$head_line" != *"#!/usr/bin/env bash"* ]] && [[ "$head_line" != *"#!/usr/bin/env sh"* ]]; then
        continue
    fi

    changed=false

    # eval annotation
    if grep -v '^\s*#' "$file" 2>/dev/null | grep -qw 'eval'; then
        if ! grep -q 'SECURITY.*eval' "$file" 2>/dev/null; then
            sed -i '/^\s*#/!{/\beval\b/{/SECURITY/!s/\(.*\beval\b\)/# SECURITY: eval is unsafe with untrusted input — refactor to avoid dynamic code execution\n\1/}}' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    # pipe-to-shell annotation
    if grep -v '^\s*#' "$file" 2>/dev/null | grep -qE '(curl|wget)\s+.*\|\s*(sudo\s+)?(ba)?sh'; then
        if ! grep -q 'SECURITY.*pipe-to-shell' "$file" 2>/dev/null; then
            sed -i '/^\s*#/!{/\(curl\|wget\).*|.*sh/{/SECURITY/!s/\(.*\(curl\|wget\).*|.*sh\)/# SECURITY: pipe-to-shell is unsafe — download to file, verify checksum, then execute\n# Example: _SCRIPT=$(mktemp); curl -fsSL "URL" -o "$_SCRIPT"; sha256sum -c <<<"HASH  $_SCRIPT"; bash "$_SCRIPT"; rm -f "$_SCRIPT"\n\1/}}' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    if [[ "$changed" == true ]]; then
        rel_path="${file#"$REPO_PATH"/}"
        echo "  FIXED $rel_path (shell eval/pipe-to-shell)"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -not -name "*.*" "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Annotated dynamic code execution patterns in $FIXED_COUNT file(s)"
else
    echo "No eval/pipe-to-shell patterns found"
fi
