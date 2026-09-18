#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# rsr-census.sh — snapshot every estate repository's file list.
#
# detect-rsr-drift.sh measures the RSR rule table against the estate, but it
# needs a cheap representation of the estate to do so: one file per repository,
# one path per line. That is this script's only job. It is deliberately
# network-only and slow, so run it on a schedule and reuse the output.
#
# Output layout (what --trees-dir expects):
#   <CENSUS_DIR>/<repo>          # newline-separated paths, no leading "./"
#
# Usage:
#   rsr-census.sh [--out DIR] [--repos-file FILE] [--refresh]
#
# Env: GH_TOKEN or GITHUB_TOKEN (required — 1 request per repository).

set -euo pipefail

TOKEN="${GH_TOKEN:-${GITHUB_TOKEN:-}}"
OUT="${RSR_CENSUS_DIR:-.rsr-census}"
REPOS_FILE=""
ORG="${ORG:-hyperpolymath}"
REFRESH=0

while [ $# -gt 0 ]; do
    case "$1" in
        --out)         OUT="$2"; shift 2 ;;
        --repos-file)  REPOS_FILE="$2"; shift 2 ;;
        --org)         ORG="$2"; shift 2 ;;
        --refresh)     REFRESH=1; shift ;;
        -h|--help)     sed -n '2,18p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) echo "unknown option: $1" >&2; exit 2 ;;
    esac
done

[ -n "$TOKEN" ] || { echo "need GH_TOKEN or GITHUB_TOKEN" >&2; exit 2; }
mkdir -p "$OUT"

fetch_tree() {
    # One recursive tree call per repo. Returns newline-separated blob paths.
    local repo="$1"
    curl -sS \
        -H "Authorization: Bearer $TOKEN" \
        -H "Accept: application/vnd.github+json" \
        "https://api.github.com/repos/$ORG/$repo/git/trees/HEAD?recursive=1" \
    | jq -r '
        if .tree then
            .tree[] | select(.type == "blob") | .path
        else
            "__ERROR__ \(.message // "unknown")"
        end'
}

if [ -n "$REPOS_FILE" ]; then
    mapfile -t REPOS < <(sed '/^$/d' "$REPOS_FILE")
else
    echo "fetching repository list for $ORG ..." >&2
    mapfile -t REPOS < <(curl -sS \
        -H "Authorization: Bearer $TOKEN" \
        "https://api.github.com/orgs/$ORG/repos?per_page=100&page=1" \
        | jq -r '.[].name' | sort)
fi

echo "census target: ${#REPOS[@]} repositories -> $OUT" >&2

n=0; skipped=0; fresh=0; failed=0
for repo in "${REPOS[@]}"; do
    [ -z "$repo" ] && continue
    dest="$OUT/$repo"

    if [ -f "$dest" ] && [ "$REFRESH" -eq 0 ]; then
        fresh=$((fresh + 1)); continue
    fi

    body=$(fetch_tree "$repo")
    if printf '%s' "$body" | head -n1 | grep -q '^__ERROR__'; then
        printf '  WARN %s: %s\n' "$repo" "$(printf '%s' "$body" | head -n1)" >&2
        failed=$((failed + 1))
        continue
    fi
    if [ -z "$body" ]; then
        printf '  WARN %s: empty tree (rate limited or empty repo)\n' "$repo" >&2
        failed=$((failed + 1))
        continue
    fi

    # Write atomically so a killed run cannot leave a half census that the
    # detector would read as a repository missing most of its files.
    printf '%s\n' "$body" | sort -u > "$dest.tmp"
    mv "$dest.tmp" "$dest"
    n=$((n + 1))
    [ $((n % 25)) -eq 0 ] && echo "  ...$n fetched" >&2
done

echo "" >&2
echo "wrote $n, reused $fresh, failed $failed" >&2
echo "census dir: $OUT" >&2
