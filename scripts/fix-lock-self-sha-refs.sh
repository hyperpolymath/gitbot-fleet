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

# Read tag names from stdin and print the most specific: more dots, then longer.
best_tag() { awk '{ n = gsub(/\./, ".", $0); print n "\t" length($0) "\t" $0 }' | sort -t $'\t' -k1,1nr -k2,2nr | head -1 | cut -f3; }

# Print the tag for owner/repo $1 at SHA $2 from TAG_MAP, falling back to
# remote tags unless OFFLINE is set. Print nothing when no tag is found.
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
#
# Only a `uses:` that is a real key counts: a job's key under `jobs:` is a
# reusable call (R), a step's key under `steps:` is an action (A). The same
# text inside a `run: |` script, a fixture under `env:`, or any other key is
# data, so it can neither keep a defective action record nor repair one.
# The sites are read once, with the YAML context the other fixers use.
uses_sites() { # -> lines "R<TAB>owner/repo/.github/workflows/x.yml@ref" | "A<TAB>owner/repo[/path]@ref"
    local wf
    for wf in "$REPO_PATH"/.github/workflows/*.yml "$REPO_PATH"/.github/workflows/*.yaml; do
        [ -f "$wf" ] || continue
        awk '
            function indent_of(s,   t) { t = s; sub(/[^ ].*$/, "", t); return length(t) }
            function plain(s) { return (s !~ /^[[:space:]]*$/ && s !~ /^[[:space:]]*#/) }
            function keyname(s) { sub(/^[[:space:]]*(-[[:space:]]+)?/, "", s); sub(/[[:space:]]*:.*$/, "", s); gsub(/["'"'"']/, "", s); return s }
            function ref_of(s) { sub(/^[[:space:]]*(-[[:space:]]+)?["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*["'"'"']?/, "", s); sub(/["'"'"'[:space:]#].*$/, "", s); return s }
            function nearest_below(ind,   k, p) { p = -1; for (k in keyat) if (k + 0 < ind && k + 0 > p) p = k + 0; return p }
            BEGIN {
                Q = "[\"'"'"']?"
                KEY_RE = "^[[:space:]]*" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:([[:space:]]|$)"
                ITEM_RE = "^[[:space:]]*-([[:space:]]|$)"
                SCALAR_RE = ":[[:space:]]*[|>][-+0-9]*[[:space:]]*(#.*)?$"
                USES_RE = "^" Q "uses" Q "[[:space:]]*:[[:space:]]*" Q
                item_steps = 0; item_indent = -1
            }
            {
                ind = indent_of($0)
                if (in_scalar) { if (plain($0) && ind <= scalar_indent) in_scalar = 0; else next }
                if (!plain($0)) next
                if ($0 ~ ITEM_RE) {
                    # A list item: its parent is the nearest key at its indentation or
                    # shallower (`steps:\n- uses:` puts the dash under the key).
                    p = -1; for (k in keyat) if (k + 0 <= ind && k + 0 > p) p = k + 0
                    for (k in keyat) if (k + 0 > ind) delete keyat[k]
                    item_steps = (p >= 0 && keyat[p] == "steps"); item_indent = ind
                    t = $0; sub(/^[[:space:]]*-[[:space:]]*/, "", t)
                    if (t ~ USES_RE) { if (item_steps) print "A\t" ref_of(t) }
                    else if (t ~ KEY_RE) {
                        keyat[ind + 2] = keyname(t)
                        if (t ~ SCALAR_RE) { in_scalar = 1; scalar_indent = ind + 2 }
                    }
                    next
                }
                if ($0 ~ KEY_RE) {
                    key = keyname($0)
                    for (k in keyat) if (k + 0 >= ind) delete keyat[k]
                    if (ind <= item_indent) item_steps = 0
                    if (key == "uses") {
                        p = nearest_below(ind); g = (p >= 0) ? nearest_below(p) : -1
                        if (p >= 0 && g >= 0 && keyat[g] == "jobs") print "R\t" ref_of($0)
                        else if (item_steps && ind > item_indent) print "A\t" ref_of($0)
                    }
                    keyat[ind] = key
                    if ($0 ~ SCALAR_RE) { in_scalar = 1; scalar_indent = ind }
                }
            }
        ' "$wf"
    done
}
SITES=$(mktemp)
uses_sites > "$SITES"

# For owner/repo $1 at SHA $2, print reusable, action, or unknown; reusable
# takes precedence when both kinds of use exist.
usage_of() { # <owner/repo> <sha>  -> reusable | action | unknown
    if grep -qE "^R	$1/\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml@$2$" "$SITES"; then
        echo reusable
    elif grep -qE "^A	$1(/[A-Za-z0-9_./-]+)?@$2$" "$SITES"; then
        echo action
    else
        echo unknown
    fi
}

# Return 0 if owner/repo $1 at SHA $2 should keep its SHA ref, or 1 otherwise.
# Use workflow classification, then the estate source fallback for unknown use.
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
# A record is read to its end (the next record key, or the end of the 8-space
# block): YAML mapping order is not fixed, so `ref:` may follow `commit:` and
# the other fields — exactly as a hand-added record might write it. Each
# finding carries the line of the `ref:` itself.
records=$(mktemp)
if ! awk '
    /^    ["'"'"'][^"'"'"'@]+@[0-9a-f]+["'"'"']:$/ {
        s = $0; sub(/^    ["'"'"']/, "", s); sub(/["'"'"']:$/, "", s)
        i = index(s, "@"); repo = substr(s, 1, i - 1); sha = substr(s, i + 1)
        if (length(sha) == 40) { inrec = 1; key_repo = repo; key_sha = sha } else inrec = 0
        next
    }
    /^[^[:space:]]/ || /^    [^[:space:]]/ { inrec = 0 }
    inrec && /^        ref: ["'"'"']/ {
        r = $0; sub(/^        ref: /, "", r); gsub(/["'"'"']/, "", r)
        if (r == key_sha) print NR "\t" key_repo "\t" key_sha
    }
' "$LOCK" > "$records"; then
    echo "fix-lock-self-sha-refs: could not parse $LOCK (awk failed); lock left unchanged" >&2
    rm -f "$tmp" "$records" "$SITES"; exit 2
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
    # lineno is the ref: line itself; its quote style is kept.
    sed -i "${lineno}s|ref: \(['\"]\)$sha\1|ref: \1$tag\1|" "$tmp"
    echo "FIXED  $repo@${sha:0:7}  ref: $sha -> $tag"
    fixed=$((fixed + 1))
done < "$records"
rm -f "$records" "$SITES"

if ! cmp -s "$LOCK" "$tmp"; then mv "$tmp" "$LOCK"; else rm -f "$tmp"; fi
if [ "$fixed" -eq 0 ] && [ "$unresolved" -eq 0 ]; then echo "OK (no self-SHA action records): $LOCK"; fi
[ "$fixed" -gt 0 ] && echo "Rewrote $fixed record(s) in $LOCK"
[ "$unresolved" -eq 0 ]
