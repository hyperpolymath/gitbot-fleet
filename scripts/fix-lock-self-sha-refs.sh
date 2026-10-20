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
# One workflow-pin fixer per checkout at a time (see lib/pin-fixer-lock.sh).
. "$SCRIPT_DIR/lib/pin-fixer-lock.sh"
# anchor_defs/2: the anchors a line defines, outside quoted scalars and comments.
ANCHORS_AWK=$(cat "$SCRIPT_DIR/lib/yaml-anchors.awk")
pin_fixer_lock "$REPO_PATH"

# Read tag names from stdin and print the most specific: more dots, then longer.
best_tag() { awk '{ n = gsub(/\./, ".", $0); print n "\t" length($0) "\t" $0 }' | sort -t $'\t' -k1,1nr -k2,2nr | head -1 | cut -f3; }

# Print the tag for owner/repo $1 at SHA $2 from TAG_MAP, falling back to
# remote tags unless OFFLINE is set. Print nothing when no tag is found.
lookup_tag() { # <owner/repo> <sha>
    local repo="$1" sha="$2" tag
    # GitHub resolves an owner/repository in either case and git a commit ID:
    # the map is read case-folded, so `Actions/Checkout` meets `actions/checkout`.
    tag=$(awk -F'\t' -v r="$repo" -v s="$sha" 'tolower($1) == tolower(r) && tolower($2) == tolower(s) { print $3; exit }' "$TAG_MAP" 2>/dev/null || true)
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
# workflow uses directly (a nested dependency, a tree without workflows) is
# not classified by guess: the two estate reusable sources keep their SHA ref
# by name, and any other such record is reported UNRESOLVED and left as is.
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
        awk "$ANCHORS_AWK"'
            function indent_of(s,   t) { t = s; sub(/[^ ].*$/, "", t); return length(t) }
            function plain(s) { return (s !~ /^[[:space:]]*$/ && s !~ /^[[:space:]]*#/) }
            function keyname(s) { sub(/^[[:space:]]*(-[[:space:]]+)?/, "", s); sub(/[[:space:]]*:.*$/, "", s); gsub(/["'"'"']/, "", s); return s }
            function ref_of(s) { sub(/^[[:space:]]*(-[[:space:]]+)?["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*([&!][^[:space:]]*[[:space:]]+)*["'"'"']?/, "", s); sub(/["'"'"'[:space:]].*$/, "", s); return s }
            # A `uses:` whose value is not on its line — a block scalar
            # (`uses: >-`), or nothing but a comment — names its site on the
            # lines below, which this reader skips as scalar text. Such a site
            # cannot be classified; the caller refuses (exit 2) rather than
            # read the record it names as `unknown`.
            # A value given as an alias (`uses: *call`) is the anchor'"'"'s value
            # (`CALL: &call owner/repo/.github/workflows/x.yml@<sha>`), which
            # YAML requires to come first; an alias of no anchor read here
            # cannot be classified either.
            function site(kind, r) {
                if (r ~ /^\*/) r = (substr(r, 2) in anchor) ? anchor[substr(r, 2)] : ""
                if (r == "" || r ~ /^#/ || r ~ /^[|>][-+0-9]*$/) print "F\t" FILENAME ":" FNR
                else print kind "\t" r
            }
            # Every anchor line s defines (lib/yaml-anchors.awk: outside quoted
            # scalars and comments), kept when its value is a reference
            # (`&name a/b@ref`); a later anchor of the same name replaces it,
            # as it does for the aliases after it.
            function note_anchors(s,   d, k) {
                anchor_defs(s, d)
                for (k in d) {
                    if (d[k] ~ /^[A-Za-z0-9_.-]+\/[^@[:space:]]*@[^[:space:]]+$/) anchor[k] = d[k]
                    else delete anchor[k]
                }
            }
            function nearest_below(ind,   k, p) { p = -1; for (k in keyat) if (k + 0 < ind && k + 0 > p) p = k + 0; return p }
            # The line with its trailing comment dropped and every quoted
            # scalar that is neither a key nor the value of a `uses` key
            # blanked to `""`, so a `uses:` written inside a `name:` string
            # or after a `#` names no site. Quotes are walked, not matched by
            # pattern: a `#` or a brace inside a string is text.
            function masked(s,   out, i, n, c, j, body) {
                out = ""; n = length(s); i = 1
                while (i <= n) {
                    c = substr(s, i, 1)
                    if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) break
                    if (c == "\"" || c == "'"'"'") {
                        j = i + 1
                        if (c == "\"") { while (j <= n && substr(s, j, 1) != "\"") { if (substr(s, j, 1) == "\\") j++; j++ } }
                        else { while (j <= n && substr(s, j, 1) != "'"'"'") j++ }
                        body = substr(s, i + 1, j - i - 1)
                        if (substr(s, j + 1) ~ /^[[:space:]]*:/ || out ~ /(^|[{,[:space:]])["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*([&!][^[:space:],}]*[[:space:]]+)*$/) out = out c body c
                        else out = out c c
                        i = j + 1; continue
                    }
                    out = out c; i++
                }
                return out
            }
            # Every `uses` entry of the flow mappings written on the key'"'"'s own
            # line (`call: { uses: ... }`, `- { uses: ... }`, `jobs: { a: { uses: ... },
            # b: { uses: ... } }`), into out[1..n]; returns n. Given the MASKED
            # line: the raw one carries its comment and its strings.
            # out_at[i] is the column of the `{` or `,` that precedes entry i
            # (0 at the start of the line), for flow_kind/3.
            function flow_uses_all(s, out,   n, r, t, base) {
                n = 0; split("", out); split("", out_at); t = s; base = 0
                while (match(t, /(^|[{,])[[:space:]]*["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*([&!][^"'"'"'[:space:],}]*[[:space:]]+)*["'"'"']?[^"'"'"'[:space:],}]+/)) {
                    r = substr(t, RSTART, RLENGTH)
                    out_at[n + 1] = (r ~ /^[{,]/) ? base + RSTART : 0
                    sub(/^.*uses["'"'"']?[[:space:]]*:[[:space:]]*([&!][^"'"'"'[:space:],}]*[[:space:]]+)*["'"'"']?/, "", r)
                    out[++n] = r; base += RSTART + RLENGTH - 1; t = substr(t, RSTART + RLENGTH)
                }
                return n
            }
            # The kind of the flow `uses` entry whose mapping is open at
            # column `at` of masked line s, from the keys that open the
            # brackets around it: under a `[` that `steps:` opens it is an
            # action (A); directly in a mapping a job key opens — inside the
            # `{` that `jobs:` opens on this line, or on a job line (`call:
            # { uses: ... }`) whose block parent is `jobs:` — it is a reusable
            # call (R); anywhere else (`with: { uses: ... }`) it is data ("").
            # `item` marks a `- { ... }` step: only its own entries are its
            # action.
            function flow_kind(s, at, parent, item,   i, c, d, k, seg, kn) {
                d = 0; segs[0] = 1
                for (i = 1; i <= at; i++) {
                    c = substr(s, i, 1)
                    if (c == "{" || c == "[") {
                        seg = substr(s, segs[d], i - segs[d]); kn = ""
                        if (match(seg, /["'"'"']?[A-Za-z0-9_.-]+["'"'"']?[[:space:]]*:[[:space:]]*$/)) { kn = substr(seg, RSTART, RLENGTH); sub(/[[:space:]]*:[[:space:]]*$/, "", kn); gsub(/["'"'"']/, "", kn) }
                        d++; kind[d] = c; kname[d] = kn; segs[d] = i + 1
                    } else if (c == ",") segs[d] = i + 1
                    else if ((c == "}" || c == "]") && d > 0) d--
                }
                if (item) return (d == 1 && kind[1] == "{") ? "A" : ""
                for (k = 1; k <= d; k++) if (kind[k] == "[" && kname[k] == "steps") return "A"
                if (d >= 1 && kind[d] == "{" && kname[d] != "") {
                    if (d >= 2 && kind[d - 1] == "{" && kname[d - 1] == "jobs") return "R"
                    if (d == 1 && parent == "jobs") return "R"
                }
                return ""
            }
            # The brackets a masked line opens and leaves open (negative when
            # it closes more than it opens). Quoted text is already blanked.
            function depth_of(s,   t) { t = s; return gsub(/[[{]/, "", t) - gsub(/[]}]/, "", s) }
            # Every `uses` entry of the flow collection s (masked, possibly
            # joined from several lines), classified by flow_kind/4. `steps`
            # gates an item: only a `- { ... }` under `steps:` is a step.
            function flow_emit(s, parent, item, steps,   m, j, fk) {
                m = flow_uses_all(s, fe)
                for (j = 1; j <= m; j++) {
                    fk = flow_kind(s, out_at[j], parent, item)
                    if (fk != "" && (!item || steps)) site(fk, fe[j])
                }
            }
            # A flow collection that opens on line s and does not close there
            # is read to its closing bracket before it is classified: the
            # lines in between are its entries, never block keys.
            function flow_open(s, parent, item, steps) {
                fdepth = depth_of(s)
                if (fdepth > 0) { facc = s; fparent = parent; fitem = item; fsteps = steps }
                else { fdepth = 0; flow_emit(s, parent, item, steps) }
            }
            BEGIN {
                Q = "[\"'"'"']?"
                KEY_RE = "^[[:space:]]*" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:([[:space:]]|$)"
                KEY_FLOW_RE = "^[[:space:]]*" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:[[:space:]]*[[{]"
                ITEM_RE = "^[[:space:]]*-([[:space:]]|$)"
                SCALAR_RE = ":[[:space:]]*[|>][-+0-9]*[[:space:]]*(#.*)?$"
                USES_RE = "^" Q "uses" Q "[[:space:]]*:[[:space:]]*" Q
                item_steps = 0; item_indent = -1
            }
            {
                ind = indent_of($0)
                if (in_scalar) { if (plain($0) && ind <= scalar_indent) in_scalar = 0; else next }
                if (!plain($0)) next
                note_anchors($0)
                if (fdepth > 0) {
                    ml = masked($0); facc = facc " " ml; fdepth += depth_of(ml)
                    if (fdepth <= 0) { fdepth = 0; flow_emit(facc, fparent, fitem, fsteps) }
                    next
                }
                if ((ml = masked($0)) ~ /^[[:space:]]*[[{]/) {
                    # A flow collection that is a whole line: the root of a
                    # KYAML document, or the value of the key above it
                    # (`steps:` then `[ ... ]`), read as if written after it.
                    p = nearest_below(ind); g = (p >= 0) ? nearest_below(p) : -1
                    flow_open(((p >= 0) ? keyat[p] ": " : "") ml, (g >= 0 ? keyat[g] : ""), 0, 0)
                    next
                }
                if ($0 ~ ITEM_RE) {
                    # A list item: its parent is the nearest key at its indentation or
                    # shallower (`steps:\n- uses:` puts the dash under the key).
                    p = -1; for (k in keyat) if (k + 0 <= ind && k + 0 > p) p = k + 0
                    for (k in keyat) if (k + 0 > ind) delete keyat[k]
                    item_steps = (p >= 0 && keyat[p] == "steps"); item_indent = ind
                    t = $0; sub(/^[[:space:]]*-[[:space:]]*/, "", t)
                    # The step'"'"'s own keys sit at the column of its first one (`- name:`
                    # puts `uses:` under `name`); a bare dash learns it from the next
                    # key line. A deeper `uses:` belongs to `with:`, `env:` or some
                    # other input and is data.
                    item_key = (t == "") ? -1 : length($0) - length(t)
                    if (t ~ USES_RE) { if (item_steps) site("A", ref_of(t)) }
                    else if (t ~ /^\{/) flow_open(masked(t), "", 1, item_steps)
                    else if (t ~ KEY_RE) {
                        keyat[ind + 2] = keyname(t)
                        if (t ~ SCALAR_RE) { in_scalar = 1; scalar_indent = ind + 2 }
                        # An input written as a flow mapping over several lines
                        # (`- with: {`) is read to its end; it names no site.
                        else if ((mt = masked(t)) ~ KEY_FLOW_RE && depth_of(mt) > 0) flow_open(mt, "", 0, 0)
                    }
                    next
                }
                if ($0 ~ KEY_RE) {
                    key = keyname($0)
                    for (k in keyat) if (k + 0 >= ind) delete keyat[k]
                    if (ind <= item_indent) item_steps = 0
                    if (item_steps && item_key < 0 && ind > item_indent) item_key = ind
                    if (key == "uses") {
                        p = nearest_below(ind); g = (p >= 0) ? nearest_below(p) : -1
                        if (p >= 0 && g >= 0 && keyat[g] == "jobs") site("R", ref_of($0))
                        else if (item_steps && ind == item_key) site("A", ref_of($0))
                    } else if ((ml = masked($0)) ~ KEY_FLOW_RE) {
                        # A flow collection as the key'"'"'s value: a job written
                        # `call: { uses: ... }`, or the whole `jobs: { ... }` mapping
                        # on one line, is a reusable call; `steps: [ { uses: ... } ]`
                        # is a list of actions. A flow mapping on one of a step'"'"'s
                        # own keys (`with: { uses: ... }`) is that input'"'"'s data: the
                        # step'"'"'s action is its scalar `uses:` key, never a mapping.
                        # A `{ uses: ... }` inside a job'"'"'s `name:` string or after
                        # a `#` is neither: the masked line has no such text.
                        # Each entry is classified by the keys around it, so a
                        # step inside a one-line `jobs: { b: { steps: [ ... ] } }`
                        # is an action, not a reusable call. A collection that
                        # spans lines (`steps: [` ... `]`) is read whole.
                        p = nearest_below(ind)
                        flow_open(ml, (p >= 0 ? keyat[p] : ""), 0, 0)
                    }
                    keyat[ind] = key
                    if ($0 ~ SCALAR_RE) { in_scalar = 1; scalar_indent = ind }
                }
            }
            # A flow collection that never closes cannot be classified: the
            # caller refuses rather than guess the kind of what it hides.
            END { if (fdepth > 0) print "X\t" FILENAME }
        ' "$wf"
    done
}
SITES=$(mktemp)
uses_sites > "$SITES"
if grep -q '^X' "$SITES"; then
    echo "fix-lock-self-sha-refs: a flow collection in $(grep -m1 '^X' "$SITES" | cut -f2) never closes, so its uses: sites cannot be classified; lock left unchanged" >&2
    rm -f "$SITES"; exit 2
fi
if grep -q '^F' "$SITES"; then
    echo "fix-lock-self-sha-refs: the uses: value at $(grep -m1 '^F' "$SITES" | cut -f2) is a block scalar or continues on the next line, so the site it names cannot be classified; lock left unchanged" >&2
    rm -f "$SITES"; exit 2
fi

# For owner/repo $1 at SHA $2, print reusable, action, or unknown; reusable
# takes precedence when both kinds of use exist. Repository and revision are
# compared as fields, never as a pattern — a `.` in a repository name is a
# dot, so `foo/action.test` is not `foo/actionXtest` — and in lower case:
# GitHub reads a repository name, and git a commit ID, in either case.
usage_of() { # <owner/repo> <sha>  -> reusable | action | unknown
    awk -F'\t' -v r="$1" -v s="$2" '
        BEGIN { r = tolower(r); s = tolower(s); n = length(r) }
        {
            i = index($2, "@"); if (!i) next
            path = tolower(substr($2, 1, i - 1)); rev = tolower(substr($2, i + 1))
            if (rev != s) next
            if ($1 == "R" && substr(path, 1, n + 19) == r "/.github/workflows/" && substr(path, n + 20) ~ /^[a-z0-9_.-]+\.ya?ml$/) reusable = 1
            if ($1 == "A" && (path == r || substr(path, 1, n + 1) == r "/")) action = 1
        }
        END { print (reusable ? "reusable" : (action ? "action" : "unknown")) }' "$SITES"
}

# How the record for owner/repo $1 at SHA $2 is treated: `keep` its SHA ref
# (a reusable workflow, whose lock ref is the SHA), `rewrite` it to a tag (an
# action), or `unknown`. Only a site in this repository's workflows proves an
# action: a record no workflow here uses is a transitive dependency (a
# reusable's own `uses:`), whose edge `owner/repo@sha` says nothing of its
# kind, and a nested reusable's SHA ref is legitimate. Such a record is left
# as is and reported, never rewritten on a guess; the estate's two reusable
# sources are known reusables.
sha_ref_disposition() { # <owner/repo> <sha>  -> keep | rewrite | unknown
    case "$(usage_of "$1" "$2")" in
        reusable) echo keep ;;
        action) echo rewrite ;;
        # GitHub reads `Hyperpolymath/Standards` as hyperpolymath/standards.
        *) case "${1,,}" in hyperpolymath/standards|hyperpolymath/cicd-suite) echo keep ;; *) echo unknown ;; esac ;;
    esac
}

fixed=0; unresolved=0
tmp=$(mktemp); cp "$LOCK" "$tmp"
# Parse first, loop second: a parser that dies inside a process substitution
# would look like "no records" and the script would report OK. POSIX awk only
# (Debian/Ubuntu's default awk is mawk: no match() capture array, no interval
# expressions), so the SHA is checked for content and length separately.
# A record key and its ref may be single-quoted, double-quoted or plain (gh
# actions-lock writes single quotes; a hand-added record — exactly where this
# defect comes from — may not), and the quote style is kept on the rewrite.
# Either may write the commit ID in upper-case hex: git resolves it the same,
# so it is the same self-SHA record, looked up in lower case and rewritten as
# written. A record is read to its end (the next record key, or the end of
# the 8-space block): YAML mapping order is not fixed, so `ref:` may follow
# `commit:` and the other fields — exactly as a hand-added record might write
# it. Each finding carries the line of the `ref:` itself, and the ref as that
# line spells it.
records=$(mktemp)
if ! awk '
    /^    ["'"'"']?[^"'"'"'@[:space:]#]+@[0-9a-fA-F]+["'"'"']?:([[:space:]]+#.*)?$/ {
        s = $0; sub(/^    ["'"'"']?/, "", s); sub(/["'"'"']?:([[:space:]]+#.*)?$/, "", s)
        i = index(s, "@"); repo = substr(s, 1, i - 1); sha = substr(s, i + 1)
        if (length(sha) == 40) { inrec = 1; key_repo = repo; key_sha = sha } else inrec = 0
        next
    }
    /^[^[:space:]]/ || /^    [^[:space:]]/ { inrec = 0 }
    inrec && /^        ["'"'"']?ref["'"'"']?[[:space:]]*:[[:space:]]/ {
        # Quoted either way, or a plain scalar, in either case of hex: the
        # same ref to GitHub. The key may be quoted too (`"ref": ...`).
        r = $0; sub(/^        ["'"'"']?ref["'"'"']?[[:space:]]*:[[:space:]]*/, "", r); sub(/[[:space:]]+#.*$/, "", r); gsub(/["'"'"']/, "", r); sub(/[[:space:]]+$/, "", r)
        if (tolower(r) == tolower(key_sha)) print NR "\t" key_repo "\t" key_sha "\t" r
    }
' "$LOCK" > "$records"; then
    echo "fix-lock-self-sha-refs: could not parse $LOCK (awk failed); lock left unchanged" >&2
    rm -f "$tmp" "$records" "$SITES"; exit 2
fi
while IFS=$'\t' read -r lineno repo sha ref_raw; do
    sha_lc="${sha,,}"
    case "$(sha_ref_disposition "$repo" "$sha_lc")" in
        keep) continue ;;
        unknown)
            echo "UNRESOLVED  $repo@${sha:0:7}: no workflow here uses it (a transitive dependency), so whether it is an action or a reusable workflow cannot be told; record left as is" >&2
            unresolved=$((unresolved + 1)); continue ;;
    esac
    tag=$(lookup_tag "$repo" "$sha_lc")
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
    # lineno is the ref: line itself; its quote style (or none) is kept, and
    # the SHA is matched as that line spells it (its case may differ from the
    # key's).
    # A plain field whose tag YAML would read as a non-string (`true`, `1`,
    # `null`) is written quoted, so the lock keeps a tag, not a boolean.
    q=""
    if pin_fixer_yaml_nonstring "$tag" && ! sed -n "${lineno}p" "$tmp" | grep -qE "^        [\"']?ref[\"']?[[:space:]]*:[[:space:]]*[\"']"; then q="'"; fi
    sed -i "${lineno}s|^\(        ['\"]\{0,1\}ref['\"]\{0,1\}[[:space:]]*:[[:space:]]*\)\(['\"]\{0,1\}\)$ref_raw\2|\1\2$q$tag$q\2|" "$tmp"
    echo "FIXED  $repo@${sha:0:7}  ref: $sha -> $tag"
    fixed=$((fixed + 1))
done < "$records"
rm -f "$records" "$SITES"

if ! cmp -s "$LOCK" "$tmp"; then pin_fixer_replace "$tmp" "$LOCK"; else rm -f "$tmp"; fi
if [ "$fixed" -eq 0 ] && [ "$unresolved" -eq 0 ]; then echo "OK (no self-SHA action records): $LOCK"; fi
[ "$fixed" -gt 0 ] && echo "Rewrote $fixed record(s) in $LOCK"
[ "$unresolved" -eq 0 ]
