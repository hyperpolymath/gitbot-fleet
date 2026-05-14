#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-missing-assail-recipe.sh — Add assail recipe to Justfile if missing
#
# Fixes MissingAssailRecipe by appending a panic-attacker pre-commit scan
# recipe to the existing Justfile. Skips if no Justfile exists.
#
# Usage: fix-missing-assail-recipe.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

JUSTFILE="$REPO_PATH/justfile"

echo "=== Missing Assail Recipe Fix ==="
echo "  Repo: $REPO_PATH"

# Check for Justfile (case-insensitive variants)
if [[ ! -f "$JUSTFILE" ]]; then
    JUSTFILE="$REPO_PATH/Justfile"
    if [[ ! -f "$JUSTFILE" ]]; then
        echo "  SKIP: No Justfile found in repository"
        exit 0
    fi
fi

# Idempotency: skip if assail recipe already exists
if grep -q '^assail:' "$JUSTFILE" 2>/dev/null; then
    echo "  SKIP: assail recipe already exists in Justfile"
    exit 0
fi

# Append assail recipe (ensure trailing newline before appending)
{
    echo ""
    echo "# Run panic-attacker pre-commit scan"
    echo "assail:"
    echo '    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"'
} >> "$JUSTFILE"

echo "  ADDED assail recipe to Justfile"
echo ""
echo "Fixed: 1 recipe appended"
exit 0
