#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-http-to-https.sh — Replace HTTP URLs with HTTPS
#
# Fixes PA014 InsecureProtocol findings where http:// is used.
# Upgrades to https:// where safe to do so.
#
# Uses git ls-files for fast traversal and hash-based caching to skip
# files unchanged since last scan.
#
# Usage: fix-http-to-https.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "=== HTTP to HTTPS Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0
SKIPPED_COUNT=0

# Source scan cache if available
if [[ -f "$SCRIPT_DIR/lib/scan-cache.sh" ]]; then
    source "$SCRIPT_DIR/lib/scan-cache.sh"
    cache_init "fix-http-to-https" "$REPO_PATH"
    USE_CACHE=true
else
    USE_CACHE=false
fi

# Domains known to support HTTPS
SAFE_DOMAINS="github.com|gitlab.com|bitbucket.org|crates.io|npmjs.com|pypi.org|rubygems.org|hex.pm|opam.ocaml.org|hackage.haskell.org|pkg.go.dev|docs.rs|wikipedia.org|stackoverflow.com|mozilla.org|w3.org|schema.org|creativecommons.org|opensource.org|spdx.org"

# File extensions to check
EXTENSIONS=("*.sh" "*.yml" "*.yaml" "*.md" "*.adoc" "*.txt" "*.json" "*.toml" "*.cfg" "*.ini" "*.rs" "*.ex" "*.exs" "*.res" "*.js")

# Use git ls-files if in a git repo, otherwise find
list_files() {
    if [[ -d "$REPO_PATH/.git" ]]; then
        (cd "$REPO_PATH" && git ls-files -z "${EXTENSIONS[@]}" 2>/dev/null) | \
            while IFS= read -r -d '' f; do
                # Skip vendored/generated directories
                case "$f" in
                    */node_modules/*|*/target/*|*/.git/*|*/deps/*|*/vendor/*|*/_build/*) continue ;;
                    */winget-pkgs/*|*/compiler-source/*|*/.lake/*|*/__pycache__/*) continue ;;
                esac
                [[ -f "$REPO_PATH/$f" ]] && printf '%s\0' "$REPO_PATH/$f"
            done
    else
        find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.yml" -o -name "*.yaml" -o -name "*.md" -o -name "*.adoc" -o -name "*.txt" -o -name "*.json" -o -name "*.toml" -o -name "*.cfg" -o -name "*.ini" -o -name "*.rs" -o -name "*.ex" -o -name "*.exs" -o -name "*.res" -o -name "*.js" \) \
            -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/node_modules/*" \
            -not -path "*/winget-pkgs/*" -not -path "*/compiler-source/*" \
            -print0 2>/dev/null
    fi
}

while IFS= read -r -d '' file; do
    # Skip via hash cache
    if [[ "$USE_CACHE" == "true" ]] && cache_unchanged "$file"; then
        continue
    fi

    # Skip binary files (quick check via file extension — avoid slow `file` command)
    case "$file" in
        *.png|*.jpg|*.gif|*.ico|*.woff|*.woff2|*.ttf|*.eot|*.svg|*.pdf|*.zip|*.tar|*.gz) continue ;;
    esac

    if grep -qP 'http://(?!localhost|127\.|0\.0\.0\.|192\.168\.|10\.|172\.(1[6-9]|2[0-9]|3[01])\.)' "$file" 2>/dev/null; then
        rel_path="${file#$REPO_PATH/}"

        # Only fix URLs for known HTTPS-capable domains
        if grep -qP "http://($SAFE_DOMAINS)" "$file" 2>/dev/null; then
            sed -i -E "s#http://($SAFE_DOMAINS)#https://\1#g" "$file" 2>/dev/null || true
            echo "  FIXED $rel_path"
            ((FIXED_COUNT++)) || true
        else
            echo "  SKIP $rel_path — unknown domain (manual review needed)"
            ((SKIPPED_COUNT++)) || true
        fi
    fi

    # Update cache regardless of whether we fixed anything
    [[ "$USE_CACHE" == "true" ]] && cache_update "$file"
done < <(list_files)

# Save cache
if [[ "$USE_CACHE" == "true" ]]; then
    cache_save
    cache_stats
fi

echo ""
echo "Fixed: $FIXED_COUNT file(s), Skipped: $SKIPPED_COUNT file(s)"
