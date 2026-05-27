#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-gitattributes-eol.sh — Add EOL normalization to .gitattributes
# AND renormalize CRLF blobs so git's autocrlf round-trip stops
# producing phantom diffs that block rebase. ERR-GIT-001.
#
# Root cause (claude-integrations#43, 2026-05-27): repo has CRLF
# blobs in the git object store but no .gitattributes directing
# normalization. On every checkout/rebase, the working tree gets a
# CRLF→LF round-trip via core.autocrlf, producing uncommittable
# diffs. The fix is twofold:
#   1. Write `.gitattributes` declaring `* text=auto eol=lf`
#   2. Run `git add --renormalize .` to refresh the object store
#
# Usage: fix-gitattributes-eol.sh <repo-path>

set -euo pipefail

REPO_PATH="${1:-.}"
cd "$REPO_PATH"

if [ ! -d .git ]; then
    echo "ERROR: not a git repo: $REPO_PATH" >&2
    exit 1
fi

# Build the canonical .gitattributes content
CANONICAL_GA=$(cat <<'EOF'
# SPDX-License-Identifier: MPL-2.0
# Normalise line endings to LF for text files; preserve CRLF on .bat
# (Windows shell-script files require CRLF to execute). Binary
# extensions are declared explicitly to prevent any normalisation.

* text=auto eol=lf

# Windows-shell scripts
*.bat text eol=crlf
*.cmd text eol=crlf

# Binaries (do not normalise)
*.png binary
*.jpg binary
*.jpeg binary
*.gif binary
*.ico binary
*.pdf binary
*.zip binary
*.gz binary
*.tar binary
*.wasm binary
*.so binary
*.dylib binary
*.dll binary
EOF
)

# Apply EOL normalisation if .gitattributes doesn't already declare it
if [ -f .gitattributes ] && grep -qE "(eol=lf|text=auto)" .gitattributes; then
    echo "OK: .gitattributes already normalises EOL"
else
    if [ -f .gitattributes ]; then
        # Prepend our directives if file exists but is silent on EOL
        tmp=$(mktemp)
        {
            echo "$CANONICAL_GA"
            echo ""
            echo "# --- Pre-existing directives (preserved) ---"
            cat .gitattributes
        } > "$tmp"
        mv "$tmp" .gitattributes
    else
        echo "$CANONICAL_GA" > .gitattributes
    fi
    echo "FIXED: .gitattributes now normalises EOL to LF"
fi

# Now renormalise blobs. This is the critical step the
# claude-integrations#43 phantom-diff was missing.
echo "Renormalising blobs (git add --renormalize .)..."
git add --renormalize .
if git diff --cached --quiet; then
    echo "OK: no blobs needed renormalisation (working tree already clean)"
else
    git diff --cached --name-only | sed 's|^|  RENORMALISED  |'
fi

echo ""
echo "SUMMARY: $REPO_PATH .gitattributes + blobs reconciled. Commit the staged changes."
