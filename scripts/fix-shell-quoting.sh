#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-shell-quoting.sh — Quote unquoted shell variable expansions
#
# Fixes patterns like: $VAR → "${VAR}" in shell scripts
# Targets PA009-shell-unquoted-var findings.
#
# Usage: fix-shell-quoting.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

# Extract details from manifest entry
DESCRIPTION=$(jq -r '.description // ""' "$FINDING_JSON")
PATTERN_ID=$(jq -r '.pattern_id // ""' "$FINDING_JSON")

echo "=== Shell Quoting Fix ==="
echo "  Repo:    $REPO_PATH"
echo "  Pattern: $PATTERN_ID"
echo ""

# Find shell files in the repo
SHELL_FILES=()
while IFS= read -r -d '' f; do
    SHELL_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.bash" \) -not -path "*/\.git/*" -not -path "*/node_modules/*" -not -path "*/target/*" -print0 2>/dev/null)

# Also check .yml/.yaml files for shell: sections
while IFS= read -r -d '' f; do
    SHELL_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.yml" -o -name "*.yaml" \) -path "*/.github/workflows/*" -print0 2>/dev/null)

if [[ ${#SHELL_FILES[@]} -eq 0 ]]; then
    echo "  No shell files found in $REPO_PATH"
    exit 0
fi

FIXED_COUNT=0

for file in "${SHELL_FILES[@]}"; do
    # Skip binary files
    if file "$file" | grep -q "binary"; then
        continue
    fi

    # Fix common unquoted patterns in shell scripts (not YAML)
    if [[ "$file" == *.sh || "$file" == *.bash ]]; then
        CHANGES=0

        # Pattern 1: $VAR in command arguments (not in already-quoted strings)
        # This is conservative — only fixes obvious cases
        if grep -qP '(?<!")\$[A-Z_][A-Z0-9_]*(?!")' "$file" 2>/dev/null; then
            # Use sed to quote unquoted $VAR in common patterns
            # Only quote variables that appear outside of existing quotes
            # This is a simplified heuristic — complex cases need manual review

            # Fix: cd $DIR → cd "$DIR"
            if sed -i.bak 's/\bcd \$\([A-Z_][A-Z0-9_]*\)/cd "\$\1"/g' "$file" && ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
                ((CHANGES++)) || true
            fi

            # Fix: cp/mv/rm $FILE → cp/mv/rm "$FILE"
            for cmd in cp mv rm mkdir rmdir chmod chown; do
                if sed -i.bak "s/\\b${cmd} \\$\\([A-Z_][A-Z0-9_]*\\)/${cmd} \"\\$\\1\"/g" "$file" && ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
                    ((CHANGES++)) || true
                fi
            done

            # Fix: echo $VAR → echo "$VAR"
            if sed -i.bak 's/\becho \$\([A-Z_][A-Z0-9_]*\)/echo "\$\1"/g' "$file" && ! diff -q "$file" "$file.bak" >/dev/null 2>&1; then
                ((CHANGES++)) || true
            fi

            rm -f "$file.bak"
        fi

        if [[ "$CHANGES" -gt 0 ]]; then
            rel_path="${file#$REPO_PATH/}"
            echo "  Fixed $CHANGES patterns in $rel_path"
            ((FIXED_COUNT++)) || true
        fi
    fi
done

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed $FIXED_COUNT file(s)"
else
    echo "No fixable patterns found (may need manual review)"
fi
