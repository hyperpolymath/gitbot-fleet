#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-command-injection.sh — Fix unsafe command execution patterns
#
# Targets CommandInjection findings across shell, Elixir, and Racket files.
#
# Shell fixes:
#   - eval "$var" → replaced with warning comment (not auto-fixable safely)
#   - backtick `cmd` → $(cmd) substitution
#   - Unquoted variables in command arguments → quoted
#
# Elixir fixes:
#   - System.cmd with string interpolation → annotated for review
#
# Racket fixes:
#   - system/process calls → annotated for review
#
# Usage: fix-command-injection.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

DESCRIPTION=$(jq -r '.description // ""' "$FINDING_JSON")
PATTERN_ID=$(jq -r '.pattern_id // ""' "$FINDING_JSON")

echo "=== Command Injection Fix ==="
echo "  Repo:    $REPO_PATH"
echo "  Pattern: $PATTERN_ID"
echo ""

# Use shared third-party exclusions
FIND_EXCLUDES=(-not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}")

FIXED_COUNT=0

# ---------------------------------------------------------------------------
# Phase 1: Shell scripts (.sh, .bash)
# ---------------------------------------------------------------------------
SHELL_FILES=()
while IFS= read -r -d '' f; do
    SHELL_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.bash" \) \
    "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

for file in "${SHELL_FILES[@]}"; do
    # Skip binary files
    if file "$file" | grep -q "binary"; then
        continue
    fi

    CHANGES=0
    rel_path="${file#"$REPO_PATH"/}"

    # --- Fix 1: Replace backtick substitution with $() ---
    # Match lines containing `...` (backtick command substitution)
    # Convert `cmd args` → $(cmd args)
    # Careful: skip lines that are comments or already use $()
    if grep -qP '(?<!\\)`[^`]+`' "$file" 2>/dev/null; then
        # Replace backtick substitution with $() form
        # Skip comment lines, convert `...` to $(...)
        cp "$file" "$file.bak"
        sed -i -E '/^\s*#/!s/`([^`]+)`/$(\1)/g' "$file"
        if ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
            ((CHANGES++)) || true
            echo "  [shell] Replaced backtick substitution in $rel_path"
        fi
        rm -f "$file.bak"
    fi

    # --- Fix 2: Annotate eval usage with warning ---
    # Insert a warning comment above eval lines that use variable expansion
    if grep -qP '^\s*eval\s+["\$]' "$file" 2>/dev/null; then
        # Check if warning comment already exists (idempotent)
        if ! grep -q "SECURITY: eval with variable expansion is a command injection risk" "$file" 2>/dev/null; then
            sed -i.bak '/^\s*eval\s\+["\$]/i\# SECURITY: eval with variable expansion is a command injection risk — refactor to avoid eval' "$file"
            if [[ -f "$file.bak" ]] && ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
                ((CHANGES++)) || true
                echo "  [shell] Annotated eval usage in $rel_path"
            fi
            rm -f "$file.bak"
        fi
    fi

    # --- Fix 3: Quote unquoted variables in command positions ---
    # Fix patterns like: cmd $VAR → cmd "$VAR"
    # Only target common command prefixes to avoid false positives
    for cmd in install cp mv rm mkdir rmdir chmod chown cat grep sed awk curl wget; do
        if grep -qP "\\b${cmd}\\s+\\\$[A-Za-z_][A-Za-z0-9_]*(?![\"'])" "$file" 2>/dev/null; then
            sed -i.bak "s/\\(\\b${cmd}\\s\\+\\)\\$\\([A-Za-z_][A-Za-z0-9_]*\\)/\\1\"\\$\\2\"/g" "$file"
            if [[ -f "$file.bak" ]] && ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
                ((CHANGES++)) || true
            fi
            rm -f "$file.bak"
        fi
    done

    # --- Fix 4: Quote unquoted $() in command arguments ---
    # Fix: cmd $(subcmd) → cmd "$(subcmd)"
    # Only where $() is not already inside quotes
    if grep -qP '(?<!")\$\([^)]+\)(?!")' "$file" 2>/dev/null; then
        # Quote bare $() in command arguments
        cp "$file" "$file.bak"
        sed -i -E '/^\s*#/!s/ \$\(([^)]+)\)(?!")/ "$(\1)"/g' "$file"
        if ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
            ((CHANGES++)) || true
            echo "  [shell] Quoted bare command substitutions in $rel_path"
        fi
        rm -f "$file.bak"
    fi

    if [[ "$CHANGES" -gt 0 ]]; then
        ((FIXED_COUNT++)) || true
    fi
done

# ---------------------------------------------------------------------------
# Phase 2: Elixir files (.ex, .exs)
# ---------------------------------------------------------------------------
ELIXIR_FILES=()
while IFS= read -r -d '' f; do
    ELIXIR_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.ex" -o -name "*.exs" \) \
    "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

for file in "${ELIXIR_FILES[@]}"; do
    rel_path="${file#"$REPO_PATH"/}"

    # Find System.cmd with string interpolation in the command argument
    # Pattern: System.cmd("...#{...}...", ...)
    if grep -qP 'System\.cmd\(\s*"[^"]*#\{' "$file" 2>/dev/null; then
        # Check if annotation already present (idempotent)
        if ! grep -q "SECURITY: validate input before System.cmd" "$file" 2>/dev/null; then
            sed -i.bak '/System\.cmd(\s*"[^"]*#\{/i\    # SECURITY: validate input before System.cmd — string interpolation in command is a command injection risk' "$file"
            if [[ -f "$file.bak" ]] && ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
                ((FIXED_COUNT++)) || true
                echo "  [elixir] Annotated System.cmd interpolation in $rel_path"
            fi
            rm -f "$file.bak"
        fi
    fi

    # Also catch :os.cmd with interpolation
    if grep -qP ':os\.cmd\(' "$file" 2>/dev/null; then
        if ! grep -q "SECURITY: :os.cmd executes shell commands" "$file" 2>/dev/null; then
            sed -i.bak '/:os\.cmd(/i\    # SECURITY: :os.cmd executes shell commands — validate all inputs to prevent command injection' "$file"
            if [[ -f "$file.bak" ]] && ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
                ((FIXED_COUNT++)) || true
                echo "  [elixir] Annotated :os.cmd usage in $rel_path"
            fi
            rm -f "$file.bak"
        fi
    fi
done

# ---------------------------------------------------------------------------
# Phase 3: Racket files (.rkt) — annotation only
# ---------------------------------------------------------------------------
RACKET_FILES=()
while IFS= read -r -d '' f; do
    RACKET_FILES+=("$f")
done < <(find "$REPO_PATH" -type f -name "*.rkt" \
    "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

for file in "${RACKET_FILES[@]}"; do
    rel_path="${file#"$REPO_PATH"/}"

    # Annotate (system ...) and (process ...) calls
    if grep -qP '\(\s*(system|process)\s' "$file" 2>/dev/null; then
        if ! grep -q "SECURITY: system/process call needs manual review for command injection" "$file" 2>/dev/null; then
            sed -i.bak '/(\s*\(system\|process\)\s/i\; SECURITY: system/process call needs manual review for command injection' "$file"
            if [[ -f "$file.bak" ]] && ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
                ((FIXED_COUNT++)) || true
                echo "  [racket] Annotated system/process call in $rel_path"
            fi
            rm -f "$file.bak"
        fi
    fi
done

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed/annotated $FIXED_COUNT file(s)"
else
    echo "No fixable patterns found (may need manual review)"
fi
