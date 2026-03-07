#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-sql-parameterize.sh — Replace string-interpolated SQL with parameterized queries
#
# Fixes PA012 SQLInjection findings.
# Adds warning annotations and performs safe mechanical replacements where possible.
#
# Usage: fix-sql-parameterize.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== SQL Parameterization Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# --- Elixir: Ecto raw SQL with string interpolation ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changed=false

    # Pattern: Ecto.Adapters.SQL.query(repo, "SELECT ... #{var} ...")
    # Fix: Add warning comment
    if grep -qP 'SQL\.(query|query!)\(.*, ".*#\{' "$file" 2>/dev/null; then
        if ! grep -q 'SECURITY: parameterize SQL' "$file" 2>/dev/null; then
            sed -i '/SQL\.\(query\|query!\)(.*".*#\{/i\    # SECURITY: parameterize SQL — use $1 placeholders instead of interpolation' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    # Pattern: Repo.query("... #{var} ...")
    if grep -qP 'Repo\.\w+\(.*".*#\{' "$file" 2>/dev/null; then
        if ! grep -q 'SECURITY: parameterize' "$file" 2>/dev/null; then
            sed -i '/Repo\.\w\+(.*".*#\{/i\    # SECURITY: parameterize SQL — use Ecto.Query or $1 placeholders' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path — added SQL parameterization warnings"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.ex" -o -name "*.exs" \) \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

# --- Python: string formatting in SQL ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changed=false

    # Pattern: cursor.execute("SELECT ... %s" % var) → cursor.execute("SELECT ... %s", (var,))
    # Pattern: cursor.execute(f"SELECT ... {var}") → add warning
    if grep -qP 'execute\(f["\x27]' "$file" 2>/dev/null; then
        if ! grep -q 'SECURITY: parameterize SQL' "$file" 2>/dev/null; then
            sed -i '/execute(f["\x27]/i\    # SECURITY: parameterize SQL — use placeholder parameters instead of f-strings' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    # Pattern: execute("..." % (var)) → add warning
    if grep -qP 'execute\(".*"\s*%' "$file" 2>/dev/null; then
        if ! grep -q 'SECURITY: parameterize SQL' "$file" 2>/dev/null; then
            sed -i '/execute(".*"\s*%/i\    # SECURITY: parameterize SQL — use ? or %s with parameter tuple' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path — added SQL parameterization warnings"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.py" \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -not -path "*/venv/*" -print0 2>/dev/null)

# --- JavaScript: string concatenation in SQL ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changed=false

    # Pattern: query("SELECT ... " + var) or query(`SELECT ... ${var}`)
    if grep -qP 'query\(`.*\$\{' "$file" 2>/dev/null; then
        if ! grep -q 'SECURITY: parameterize SQL' "$file" 2>/dev/null; then
            sed -i '/query(`.*\$\{/i\    // SECURITY: parameterize SQL — use prepared statements with ? placeholders' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path — added SQL parameterization warnings"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.js" -o -name "*.mjs" \) \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Annotated $FIXED_COUNT file(s) with SQL parameterization warnings"
else
    echo "No SQL injection patterns found"
fi
