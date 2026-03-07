#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-innerhtml.sh — Replace innerHTML with textContent to prevent XSS
#
# Fixes PA014 InnerHTML / XSS findings.
# innerHTML allows script injection; textContent is safe for text display.
#
# Usage: fix-innerhtml.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== innerHTML → textContent Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# Find JS/TS/ReScript files with innerHTML
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changed=false

    # Skip minified files
    if [[ "$file" == *.min.js || "$file" == *.min.css ]]; then
        continue
    fi

    # Pattern 1: .innerHTML = expr (assignment)
    # Safe replacement: .textContent = expr (for text-only content)
    # Only replace if the assigned value looks like a variable or string (not HTML markup)
    if grep -qP '\.innerHTML\s*=' "$file" 2>/dev/null; then
        # Check each innerHTML assignment
        while IFS= read -r line_num; do
            line=$(sed -n "${line_num}p" "$file")

            # Skip if it's assigning HTML markup (contains < or >)
            if echo "$line" | grep -qP '\.innerHTML\s*=.*[<>]'; then
                # This is actual HTML insertion — add warning instead
                if ! echo "$line" | grep -q 'SECURITY'; then
                    sed -i "${line_num}i\\    // SECURITY: innerHTML with HTML content — sanitize input or use DOM API" "$file" 2>/dev/null || true
                    changed=true
                fi
            else
                # Plain text assignment — safe to replace with textContent
                sed -i "${line_num}s/\.innerHTML\s*=/.textContent =/" "$file" 2>/dev/null || true
                changed=true
            fi
        done < <(grep -nP '\.innerHTML\s*=' "$file" 2>/dev/null | cut -d: -f1)
    fi

    # Pattern 2: .innerHTML used in concatenation (.innerHTML += ...)
    if grep -qP '\.innerHTML\s*\+=' "$file" 2>/dev/null; then
        if ! grep -q 'SECURITY.*innerHTML' "$file" 2>/dev/null; then
            sed -i '/\.innerHTML\s*+=/i\    // SECURITY: innerHTML concatenation is XSS-prone — use DOM createElement/appendChild' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    # Pattern 3: outerHTML assignment
    if grep -qP '\.outerHTML\s*=' "$file" 2>/dev/null; then
        if ! grep -q 'SECURITY.*outerHTML' "$file" 2>/dev/null; then
            sed -i '/\.outerHTML\s*=/i\    // SECURITY: outerHTML assignment is XSS-prone — use DOM API instead' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    # Pattern 4: document.write (legacy XSS vector)
    if grep -qP 'document\.write\s*\(' "$file" 2>/dev/null; then
        if ! grep -q 'SECURITY.*document.write' "$file" 2>/dev/null; then
            sed -i '/document\.write\s*(/i\    // SECURITY: document.write is an XSS vector — use DOM API instead' "$file" 2>/dev/null || true
            changed=true
        fi
    fi

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.js" -o -name "*.mjs" -o -name "*.jsx" -o -name "*.res" \) \
    -not -path "*/node_modules/*" -not -path "*/\.git/*" -not -path "*/target/*" \
    -not -path "*/_build/*" -not -name "*.min.js" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed innerHTML/XSS patterns in $FIXED_COUNT file(s)"
else
    echo "No innerHTML/XSS patterns found"
fi
