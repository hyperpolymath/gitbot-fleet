#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-lock-self-sha-refs.sh — give every actions.lock dependency record whose
# `ref:` is its own commit SHA the tag that commit was resolved from.
# Hypatia rule PI006 (self-SHA lock record).
#
# MEASURED 2026-10-15/16 across the estate (cicd-squabbler run 38846539059,
# hypatia#929): GitHub now validates a reusable workflow against the CALLEE
# repository's lock at the called SHA, and rejects at Set up job
#     Unable to resolve action `actions/checkout@3d3c42e5…`: invalid lockfile pin
# whenever the record reads
#     'actions/checkout@3d3c42e5…':
#         ref: '3d3c42e5…'          <- the SHA repeated as the ref
# `gh actions-lock` writes the tag (`ref: 'v7.0.1'`); hand-added records
# wrote the SHA, and every caller of a reusable whose repository carries such
# a record died. standards main carried them from 2026-08-19 to 2026-10-19.
#
# What it does: for each record `'owner/repo@<sha>':` followed by
# `ref: '<sha>'`, where owner/repo is NOT an estate reusable-workflow source
# (hyperpolymath/standards, hyperpolymath/cicd-suite — a reusable record
# legitimately carries its SHA as ref; standards' accepted lock does), look the
# commit up in scripts/lib/action-ref-tags.tsv, else (unless --offline) in
# `git ls-remote --tags https://github.com/owner/repo`, preferring the most
# specific tag; rewrite the ref line. A commit with no tag is reported and
# left alone — a wrong tag is worse than a known-bad one.
#
# Idempotent. Exit 0 whether or not anything changed; exit 1 if a self-SHA
# record remains unresolved (so a caller can tell "fixed" from "still dead");
# exit 2, lock untouched, if the lock cannot be parsed.
#
# Usage:
#   fix-lock-self-sha-refs.sh <repo-path> [--offline]
#   TAG_MAP=/path/map.tsv fix-lock-self-sha-refs.sh <repo-path>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TAG_MAP="${TAG_MAP:-$SCRIPT_DIR/lib/action-ref-tags.tsv}"
REPO_PATH="${1:?Usage: fix-lock-self-sha-refs.sh <repo-path> [--offline]}"
OFFLINE=0; [ "${2:-}" = "--offline" ] && OFFLINE=1
LOCK="$REPO_PATH/.github/workflows/actions.lock"

[ -f "$LOCK" ] || { echo "OK (no actions.lock): $REPO_PATH"; exit 0; }

# Prefer the most specific tag: more dotted numeric components, then longer.
best_tag() { awk '{ n = gsub(/\./, ".", $0); print n "\t" length($0) "\t" $0 }' | sort -t $'\t' -k1,1nr -k2,2nr | head -1 | cut -f3; }

lookup_tag() { # <owner/repo> <sha>
    local repo="$1" sha="$2" tag
    tag=$(awk -F'\t' -v r="$repo" -v s="$sha" '$1 == r && $2 == s { print $3; exit }' "$TAG_MAP" 2>/dev/null || true)
    if [ -z "$tag" ] && [ "$OFFLINE" -eq 0 ]; then
        tag=$(git ls-remote --tags "https://github.com/$repo" 2>/dev/null \
              | awk -v s="$sha" '$1 == s { sub(/^refs\/tags\//, "", $2); sub(/\^\{\}$/, "", $2); print $2 }' \
              | sort -u | best_tag || true)
    fi
    printf '%s' "$tag"
}

fixed=0; unresolved=0
tmp=$(mktemp); cp "$LOCK" "$tmp"
# Parse first, loop second: a parser that dies inside a process substitution
# would look like "no records" and the script would report OK. POSIX awk only
# (Debian/Ubuntu's default awk is mawk: no match() capture array, no interval
# expressions), so the SHA is checked for content and length separately.
records=$(mktemp)
if ! awk '
    /^    '"'"'[^'"'"'@]+@[0-9a-f]+'"'"':$/ {
        s = $0; sub(/^    '"'"'/, "", s); sub(/'"'"':$/, "", s)
        i = index(s, "@"); repo = substr(s, 1, i - 1); sha = substr(s, i + 1)
        if (length(sha) == 40) { key_line = NR; key_repo = repo; key_sha = sha; next }
    }
    key_line && NR == key_line + 1 && $0 == "        ref: '"'"'" key_sha "'"'"'" { print key_line "\t" key_repo "\t" key_sha }
    { key_line = 0 }
' "$LOCK" > "$records"; then
    echo "fix-lock-self-sha-refs: could not parse $LOCK (awk failed); lock left unchanged" >&2
    rm -f "$tmp" "$records"; exit 2
fi
while IFS=$'\t' read -r lineno repo sha; do
    case "$repo" in hyperpolymath/standards|hyperpolymath/cicd-suite) continue ;; esac
    tag=$(lookup_tag "$repo" "$sha")
    if [ -z "$tag" ]; then
        echo "UNRESOLVED  $repo@${sha:0:7}: no tag found for this commit (record left as is; GitHub rejects it)" >&2
        unresolved=$((unresolved + 1)); continue
    fi
    # The ref line is the one directly after the record key.
    sed -i "$((lineno + 1))s|ref: '$sha'|ref: '$tag'|" "$tmp"
    echo "FIXED  $repo@${sha:0:7}  ref: $sha -> $tag"
    fixed=$((fixed + 1))
done < "$records"
rm -f "$records"

if ! cmp -s "$LOCK" "$tmp"; then mv "$tmp" "$LOCK"; else rm -f "$tmp"; fi
if [ "$fixed" -eq 0 ] && [ "$unresolved" -eq 0 ]; then echo "OK (no self-SHA action records): $LOCK"; fi
[ "$fixed" -gt 0 ] && echo "Rewrote $fixed record(s) in $LOCK"
[ "$unresolved" -eq 0 ]
