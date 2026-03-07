#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-unsafe-type-coercion.sh — Annotate unsafe type coercion patterns
#
# Fixes UnsafeTypeCoercion findings (20 entries).
# Handles multiple languages:
#   - Haskell (.hs):  unsafeCoerce
#   - OCaml (.ml):    Obj.magic
#   - Coq (.v):       Admitted
#   - Idris2 (.idr):  Skipped (handled by fix-believe-me.sh)
#   - Lean (.lean):   Skipped (handled by fix-sorry-lean.sh)
#
# Usage: fix-unsafe-type-coercion.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Unsafe Type Coercion Annotation ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# --- Haskell: unsafeCoerce ---
while IFS= read -r -d '' file; do
    # Only match actual code lines, not existing safety comments
    if grep -v '^\s*--' "$file" 2>/dev/null | grep -q 'unsafeCoerce'; then
        rel_path="${file#$REPO_PATH/}"
        count=$(grep -v '^\s*--' "$file" 2>/dev/null | grep -c 'unsafeCoerce' || echo 0)
        echo "  FOUND [Haskell] $rel_path — $count unsafeCoerce call(s)"

        # Idempotent: only add comment if SAFETY marker not already present on that line group
        sed -i '/^\s*--/!{/unsafeCoerce/{/SAFETY.*unsafeCoerce/!s/\(.*unsafeCoerce\)/-- SAFETY: unsafeCoerce bypasses type checker — replace with safe cast\n\1/}}' "$file" 2>/dev/null || true

        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.hs" \
    -not -path "*/\.git/*" \
    -not -path "*/.stack-work/*" \
    -not -path "*/dist-newstyle/*" \
    -print0 2>/dev/null)

# --- OCaml: Obj.magic ---
while IFS= read -r -d '' file; do
    # Only match actual code lines, not existing safety comments
    if grep -v '^\s*(\*' "$file" 2>/dev/null | grep -q 'Obj\.magic'; then
        rel_path="${file#$REPO_PATH/}"
        count=$(grep -c 'Obj\.magic' "$file" || echo 0)
        echo "  FOUND [OCaml] $rel_path — $count Obj.magic call(s)"

        # Idempotent: only add comment if SAFETY marker not already present
        sed -i '/Obj\.magic/{/SAFETY.*Obj\.magic/!s/\(.*Obj\.magic\)/(* SAFETY: Obj.magic bypasses type checker — use proper conversion *)\n\1/}' "$file" 2>/dev/null || true

        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.ml" \
    -not -path "*/\.git/*" \
    -not -path "*/_build/*" \
    -not -path "*/_opam/*" \
    -print0 2>/dev/null)

# --- Coq: Admitted ---
while IFS= read -r -d '' file; do
    # Match Admitted in non-comment lines
    if grep -v '^\s*(\*' "$file" 2>/dev/null | grep -q 'Admitted'; then
        rel_path="${file#$REPO_PATH/}"
        count=$(grep -c 'Admitted' "$file" || echo 0)
        echo "  FOUND [Coq] $rel_path — $count Admitted usage(s)"

        # Idempotent: only add comment if PROOF_TODO marker not already present
        sed -i '/Admitted/{/PROOF_TODO/!s/\(.*Admitted\)/(* PROOF_TODO: Replace Admitted with actual proof *)\n\1/}' "$file" 2>/dev/null || true

        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f -name "*.v" \
    -not -path "*/\.git/*" \
    -not -path "*/_build/*" \
    -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Annotated $FIXED_COUNT file(s) with safety/proof TODO comments"
else
    echo "No unsafe type coercion patterns found"
fi
