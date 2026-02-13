#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-http-to-https.sh — Replace HTTP URLs with HTTPS
#
# Fixes PA014 InsecureProtocol findings where http:// is used.
# Upgrades to https:// where safe to do so.
#
# Usage: fix-http-to-https.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== HTTP to HTTPS Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0
SKIPPED_COUNT=0

# Domains known to support HTTPS
SAFE_DOMAINS="github.com|gitlab.com|bitbucket.org|crates.io|npmjs.com|pypi.org|rubygems.org|hex.pm|opam.ocaml.org|hackage.haskell.org|pkg.go.dev|docs.rs|wikipedia.org|stackoverflow.com|mozilla.org|w3.org|schema.org|creativecommons.org|opensource.org|spdx.org"

# Find all text files (not binary, not .git)
while IFS= read -r -d '' file; do
    # Skip binary files
    if file "$file" | grep -q "binary"; then
        continue
    fi

    if grep -qP 'http://(?!localhost|127\.|0\.0\.0\.|192\.168\.|10\.|172\.(1[6-9]|2[0-9]|3[01])\.)' "$file" 2>/dev/null; then
        rel_path="${file#$REPO_PATH/}"

        # Only fix URLs for known HTTPS-capable domains
        if grep -qP "http://($SAFE_DOMAINS)" "$file" 2>/dev/null; then
            sed -i "s|http://\($SAFE_DOMAINS\)|https://\1|g" "$file" 2>/dev/null || true
            count=$(grep -cP "http://($SAFE_DOMAINS)" "$file" 2>/dev/null || echo 0)
            echo "  FIXED $rel_path"
            ((FIXED_COUNT++)) || true
        else
            echo "  SKIP $rel_path — unknown domain (manual review needed)"
            ((SKIPPED_COUNT++)) || true
        fi
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.yml" -o -name "*.yaml" -o -name "*.md" -o -name "*.adoc" -o -name "*.txt" -o -name "*.json" -o -name "*.toml" -o -name "*.cfg" -o -name "*.ini" -o -name "*.rs" -o -name "*.ex" -o -name "*.exs" -o -name "*.res" -o -name "*.js" \) -not -path "*/\.git/*" -not -path "*/target/*" -not -path "*/node_modules/*" -print0 2>/dev/null)

echo ""
echo "Fixed: $FIXED_COUNT file(s), Skipped: $SKIPPED_COUNT file(s)"
