#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-unsafe-deserialize.sh — Fix unsafe deserialization patterns
#
# Fixes UnsafeDeserialization findings across multiple languages:
#   - Python: yaml.load() → yaml.safe_load(), annotate pickle.loads()
#   - Ruby: Annotate Marshal.load()
#   - JavaScript: Annotate JSON.parse of user input
#   - Elixir: Annotate :erlang.binary_to_term()
#
# Usage: fix-unsafe-deserialize.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Unsafe Deserialization Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# Use shared third-party exclusions
FIND_EXCLUDES=(-not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}")

# --- Python files (.py) ---
while IFS= read -r -d '' file; do
    changed=false

    # Fix yaml.load(x) → yaml.safe_load(x)  (no Loader= argument)
    # Match yaml.load(...) where there is no Loader= inside the parens
    if grep -q 'yaml\.load(' "$file" 2>/dev/null; then
        # Replace yaml.load(x) with yaml.safe_load(x) when no Loader= is present
        # This handles the simple case: yaml.load(something) without Loader kwarg
        if grep -P 'yaml\.load\([^)]*\)' "$file" 2>/dev/null | grep -qv 'Loader='; then
            sed -i '/Loader=/!s/yaml\.load(/yaml.safe_load(/g' "$file" 2>/dev/null || true
            changed=true
        fi
        # Replace yaml.load(x, Loader=yaml.FullLoader) → yaml.safe_load(x)
        sed -i 's/yaml\.load(\([^,]*\),\s*Loader=yaml\.FullLoader\s*)/yaml.safe_load(\1)/g' "$file" 2>/dev/null || true
        # Replace yaml.load(x, Loader=yaml.UnsafeLoader) → yaml.safe_load(x)
        sed -i 's/yaml\.load(\([^,]*\),\s*Loader=yaml\.UnsafeLoader\s*)/yaml.safe_load(\1)/g' "$file" 2>/dev/null || true
        changed=true
    fi

    # Annotate pickle.loads() — add warning comment if not already present
    if grep -v '^\s*#' "$file" 2>/dev/null | grep -q 'pickle\.loads('; then
        sed -i '/^\s*#/!{/pickle\.loads(/{/SECURITY_WARNING/!s/\(.*pickle\.loads(\)/# SECURITY_WARNING: pickle.loads is unsafe — attacker-controlled data can execute arbitrary code\n\1/}}' "$file" 2>/dev/null || true
        changed=true
    fi

    if [[ "$changed" == true ]]; then
        rel_path="${file#"$REPO_PATH"/}"
        echo "  FIXED $rel_path (Python deserialization)"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.py" "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

# --- Ruby files (.rb) ---
while IFS= read -r -d '' file; do
    # Annotate Marshal.load() — add warning comment if not already present
    if grep -v '^\s*#' "$file" 2>/dev/null | grep -q 'Marshal\.load('; then
        sed -i '/^\s*#/!{/Marshal\.load(/{/SECURITY_WARNING/!s/\(.*Marshal\.load(\)/# SECURITY_WARNING: Marshal.load is unsafe — attacker-controlled data can execute arbitrary code\n\1/}}' "$file" 2>/dev/null || true
        rel_path="${file#"$REPO_PATH"/}"
        echo "  FIXED $rel_path (Ruby Marshal.load)"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.rb" "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

# --- JavaScript files (.js) ---
while IFS= read -r -d '' file; do
    # Annotate JSON.parse without try/catch — best-effort heuristic
    # We flag JSON.parse that is NOT inside a try block on the same or preceding line
    if grep -v '^\s*//' "$file" 2>/dev/null | grep -q 'JSON\.parse('; then
        # Only annotate if no SECURITY_WARNING already present
        if ! grep -q 'SECURITY_WARNING.*JSON\.parse' "$file" 2>/dev/null; then
            sed -i '/^\s*\/\//!{/JSON\.parse(/{/SECURITY_WARNING/!s/\(.*JSON\.parse(\)/\/\/ SECURITY_WARNING: JSON.parse can throw on malformed input — wrap in try\/catch\n\1/}}' "$file" 2>/dev/null || true
            rel_path="${file#"$REPO_PATH"/}"
            echo "  FIXED $rel_path (JavaScript JSON.parse)"
            ((FIXED_COUNT++)) || true
        fi
    fi
done < <(find "$REPO_PATH" -type f -name "*.js" "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

# --- Elixir files (.ex, .exs) ---
while IFS= read -r -d '' file; do
    # Annotate :erlang.binary_to_term() — should use [:safe] option
    if grep -v '^\s*#' "$file" 2>/dev/null | grep -q ':erlang\.binary_to_term('; then
        # Check it doesn't already have the :safe flag or a warning
        if ! grep -q 'SECURITY_WARNING.*binary_to_term' "$file" 2>/dev/null; then
            sed -i '/^\s*#/!{/:erlang\.binary_to_term(/{/SECURITY_WARNING/!s/\(.*:erlang\.binary_to_term(\)/# SECURITY_WARNING: binary_to_term without [:safe] can create atoms and execute code — use :erlang.binary_to_term(data, [:safe])\n\1/}}' "$file" 2>/dev/null || true
            rel_path="${file#"$REPO_PATH"/}"
            echo "  FIXED $rel_path (Elixir binary_to_term)"
            ((FIXED_COUNT++)) || true
        fi
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.ex" -o -name "*.exs" \) "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed deserialization patterns in $FIXED_COUNT file(s)"
else
    echo "No unsafe deserialization patterns found"
fi
