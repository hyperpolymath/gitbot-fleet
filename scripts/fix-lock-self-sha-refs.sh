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
# `ref: '<sha>'` (either quote style), where the workflows use owner/repo at
# that revision as an ACTION — a reusable-workflow call at that revision
# legitimately keeps its SHA as ref, standards' accepted lock does; a record no
# workflow uses directly falls back to hyperpolymath/standards and
# hyperpolymath/cicd-suite by name — look the commit up in
# scripts/lib/action-ref-tags.tsv, else (unless --offline) in
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

# A record's class is read from how the workflows USE that repository at that
# revision: `uses: owner/repo/.github/workflows/<file>.yml@<ref>` is a reusable
# call (the record keeps its SHA ref — standards' accepted lock does), while
# `uses: owner/repo[/path]@<ref>` is an action (the SHA ref is the defect).
# One repository can be both at different revisions (cicd-suite ships gates
# AND composite actions), so the class is per repository@revision. A record no
# workflow uses directly (a nested dependency, a tree without workflows) falls
# back to the two estate reusable sources by name.
usage_of() { # <owner/repo> <sha>  -> reusable | action | unknown
    local key="^[[:space:]]*(-[[:space:]]+)?[\"']?uses[\"']?[[:space:]]*:[[:space:]]*[\"']?$1"
    local end="@$2([\"'[:space:]]|$)"
    if grep -rqE "${key}/\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml${end}" --include='*.yml' --include='*.yaml' "$REPO_PATH/.github/workflows" 2>/dev/null; then
        echo reusable
    elif grep -rqE "${key}(/[A-Za-z0-9_./-]+)?${end}" --include='*.yml' --include='*.yaml' "$REPO_PATH/.github/workflows" 2>/dev/null; then
        echo action
    else
        echo unknown
    fi
}
keeps_sha_ref() { # <owner/repo> <sha>
    case "$(usage_of "$1" "$2")" in
        reusable) return 0 ;;
        action) return 1 ;;
        *) case "$1" in hyperpolymath/standards|hyperpolymath/cicd-suite) return 0 ;; esac; return 1 ;;
    esac
}

fixed=0; unresolved=0
tmp=$(mktemp); cp "$LOCK" "$tmp"
# Parse first, loop second: a parser that dies inside a process substitution
# would look like "no records" and the script would report OK. POSIX awk only
# (Debian/Ubuntu's default awk is mawk: no match() capture array, no interval
# expressions), so the SHA is checked for content and length separately.
# A record key and its ref may be single- or double-quoted (gh actions-lock
# writes single quotes; a hand-added record — exactly where this defect
# comes from — may not), and the quote style is kept on the rewrite.
records=$(mktemp)
if ! awk '
    /^    ["'"'"'][^"'"'"'@]+@[0-9a-f]+["'"'"']:$/ {
        s = $0; sub(/^    ["'"'"']/, "", s); sub(/["'"'"']:$/, "", s)
        i = index(s, "@"); repo = substr(s, 1, i - 1); sha = substr(s, i + 1)
        if (length(sha) == 40) { key_line = NR; key_repo = repo; key_sha = sha; next }
    }
    key_line && NR == key_line + 1 && /^        ref: ["'"'"']/ {
        r = $0; sub(/^        ref: /, "", r); gsub(/["'"'"']/, "", r)
        if (r == key_sha) print key_line "\t" key_repo "\t" key_sha
    }
    { key_line = 0 }
' "$LOCK" > "$records"; then
    echo "fix-lock-self-sha-refs: could not parse $LOCK (awk failed); lock left unchanged" >&2
    rm -f "$tmp" "$records"; exit 2
fi
while IFS=$'\t' read -r lineno repo sha; do
    if keeps_sha_ref "$repo" "$sha"; then continue; fi
    tag=$(lookup_tag "$repo" "$sha")
    if [ -z "$tag" ]; then
        echo "UNRESOLVED  $repo@${sha:0:7}: no tag found for this commit (record left as is; GitHub rejects it)" >&2
        unresolved=$((unresolved + 1)); continue
    fi
    # A tag name Git allows may still carry `&`, `|` or a quote; such a tag is
    # not written into a sed replacement or a YAML scalar from here.
    if ! [[ "$tag" =~ ^[A-Za-z0-9][A-Za-z0-9._/+-]*$ ]]; then
        echo "UNRESOLVED  $repo@${sha:0:7}: resolved tag '$tag' carries characters this fixer will not write into the lock (record left as is)" >&2
        unresolved=$((unresolved + 1)); continue
    fi
    # The ref line is the one directly after the record key; its quote style is kept.
    sed -i "$((lineno + 1))s|ref: \(['\"]\)$sha\1|ref: \1$tag\1|" "$tmp"
    echo "FIXED  $repo@${sha:0:7}  ref: $sha -> $tag"
    fixed=$((fixed + 1))
done < "$records"
rm -f "$records"

if ! cmp -s "$LOCK" "$tmp"; then mv "$tmp" "$LOCK"; else rm -f "$tmp"; fi
if [ "$fixed" -eq 0 ] && [ "$unresolved" -eq 0 ]; then echo "OK (no self-SHA action records): $LOCK"; fi
[ "$fixed" -gt 0 ] && echo "Rewrote $fixed record(s) in $LOCK"
[ "$unresolved" -eq 0 ]
