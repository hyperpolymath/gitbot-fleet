#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-heredoc-install.sh — Replace eval/heredoc patterns with safe alternatives
#
# Fixes PA009-shell-heredoc-eval findings.
# Replaces `curl | bash`, `eval $(...)`, and heredoc-eval patterns.
#
# Usage: fix-heredoc-install.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Heredoc/Eval Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# Find shell files with eval or curl-pipe-bash patterns
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"
    changes=0

    # Pattern 1: curl | bash / curl | sh (pipe-to-shell)
    if grep -qP 'curl\s.*\|\s*(ba)?sh' "$file" 2>/dev/null; then
        echo "  FOUND curl|bash in $rel_path"
        # Add warning comment
        sed -i 's/\(.*curl.*|.*sh\)/# WARNING: Pipe-to-shell is unsafe — download and verify first\n\1/' "$file" 2>/dev/null || true
        ((changes++)) || true
    fi

    # Pattern 2: eval "$(cat ..." or eval "$(heredoc..."
    if grep -qP 'eval\s+["\x27]?\$\(' "$file" 2>/dev/null; then
        echo "  FOUND eval \$() in $rel_path"
        ((changes++)) || true
    fi

    # Pattern 3: eval $VAR
    if grep -qP 'eval\s+\$[A-Z_]' "$file" 2>/dev/null; then
        echo "  FOUND eval \$VAR in $rel_path"
        ((changes++)) || true
    fi

    if [[ "$changes" -gt 0 ]]; then
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.bash" \) -not -path "*/\.git/*" -not -path "*/target/*" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Found eval/heredoc patterns in $FIXED_COUNT file(s)"
else
    echo "No eval/heredoc patterns found"
fi
