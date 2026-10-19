#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-stale-reusable-pin.sh — move a caller off a reusable-workflow pin that is
# KNOWN DEAD. Hypatia rules WF016 (orphan pin) and PI001 via PIN-003 (standards
# pins in a measured dead window); the auto-fix half of
# `reusable_workflow_sha_bump_needs_propagation` for pins the estate has already
# diagnosed.
#
# A caller line
#     uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@<stale>
# is rewritten to the replacement SHA the table names for that pin, and ONLY
# for pins the table names: this script never guesses that "newer is better".
# standards' scripts/lock-selfcheck.sh exists because a newer standards commit
# can be LESS usable than an older one, so each replacement in the table was
# chosen by hand against that check.
#
# If the repository carries .github/workflows/actions.lock, the lock follows
# each rewritten CALLER, not the SHA: in `workflows:` only the rewritten
# workflow's own entry moves to the replacement. In `dependencies:` the stale
# record is renamed when no workflow references it any more, and copied under
# the replacement key (stale record kept) while another workflow still pins
# the stale SHA — a lock key is shared by every reusable of a repository, and
# a caller the table does not cover must keep its record. Records that
# collapse onto one key are merged. A record's nested `uses:` list is NOT
# recomputed: run `gh actions-lock` (or the repository's check-lock-sync.sh)
# afterwards; the script says so when it touches a lock.
#
# Idempotent. Rewrites nothing it does not recognise. Exit 0 whether or not
# anything changed; exit 2 when the table is unreadable or a file carries a
# known-dead pin the parser cannot place as a `uses` key (that file is left
# untouched and named); exit 3 when the pins were rewritten but actions.lock
# was touched and still needs `gh actions-lock` — a dispatcher must not
# record either as a clean success.
#
# Usage:
#   fix-stale-reusable-pin.sh <repo-path>
#   PINS_TABLE=/path/to/table.tsv fix-stale-reusable-pin.sh <repo-path>
#
# Table: scripts/lib/stale-reusable-pins.tsv — reusable_path<TAB>stale_sha<TAB>
# replacement_sha<TAB>reason. reusable_path may end in `/*` to cover every
# reusable of that repository.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PINS_TABLE="${PINS_TABLE:-$SCRIPT_DIR/lib/stale-reusable-pins.tsv}"
REPO_PATH="${1:?Usage: fix-stale-reusable-pin.sh <repo-path>}"

[ -r "$PINS_TABLE" ] || { echo "fix-stale-reusable-pin: table not readable: $PINS_TABLE" >&2; exit 2; }
[ -d "$REPO_PATH/.github/workflows" ] || { echo "OK (no workflows): $REPO_PATH"; exit 0; }

LOCK="$REPO_PATH/.github/workflows/actions.lock"
rewrites=0; lock_touched=0

# Two callers pinned at two different dead SHAs move to the SAME replacement,
# so after substitution the lock can hold the same dependency key twice. A
# duplicate key is a lockfile GitHub refuses. Keep the first record, fold the
# others' nested `uses:` into it (deduplicated, sorted as gh actions-lock sorts),
# and drop them. Rewrite lockfile $1 in place, preserving gh-actions-lock's
# 4-space layout and leaving the file alone when the output is identical.
merge_duplicate_records() {
    local lock="$1" tmp
    tmp=$(mktemp)
    awk '
        # Print dependency cur using its saved header and sorted, unique uses.
        function emit_record(   i, n, out) {
            if (!cur) return
            # Rebuild the record: header lines, then a sorted, unique uses: list.
            print cur
            for (i = 1; i <= nhead[cur]; i++) print head[cur, i]
            n = 0
            for (u in uses_of) if (index(u, cur SUBSEP) == 1) n++
            if (n > 0) {
                print "        uses:"
                fflush()
                cmd = "sort"
                for (u in uses_of) if (index(u, cur SUBSEP) == 1) print "            - " substr(u, length(cur) + 2) | cmd
                close(cmd)
            }
        }
        BEGIN { indep = 0; cur = "" }
        /^dependencies:[[:space:]]*$/ { indep = 1; print; next }
        !indep { print; next }
        /^    '"'"'[^'"'"']+'"'"':$/ {
            key = $0
            if (!(key in seen)) { seen[key] = 1; order[++norder] = key; nhead[key] = 0; first = 1 } else { first = 0 }
            cur = key; next
        }
        /^        uses:[[:space:]]*$/ { next }
        /^            - '"'"'[^'"'"']+'"'"'$/ { u = $0; sub(/^            - /, "", u); uses_of[cur SUBSEP u] = 1; next }
        /^        [a-z_]+: / { if (first) { head[cur, ++nhead[cur]] = $0 } ; next }
        { next }
        END {
            for (k = 1; k <= norder; k++) {
                cur = order[k]
                emit_record()
            }
        }
    ' "$lock" > "$tmp"
    if ! cmp -s "$lock" "$tmp"; then mv "$tmp" "$lock"; else rm -f "$tmp"; fi
}

# Find the first PINS_TABLE row for reusable path $1 at stale SHA $2, including
# wildcard paths. Set replacement/reason and return 0 on a match; otherwise
# clear both variables and return 1.
lookup_replacement() { # <reusable-path> <stale-sha>
    local t_path t_stale t_new t_reason
    replacement=""; reason=""
    while IFS=$'\t' read -r t_path t_stale t_new t_reason; do
        [[ -z "$t_path" || "$t_path" == \#* ]] && continue
        [[ "$t_stale" == "$2" ]] || continue
        if [[ "$t_path" == "$1" ]] || { [[ "$t_path" == */\* ]] && [[ "$1" == "${t_path%\*}"* ]]; }; then
            replacement="$t_new"; reason="$t_reason"; return 0
        fi
    done < "$PINS_TABLE"
    return 1
}

# Print YAML context markers for workflow file $1, one per line that matters:
#   K<n>  line n is a `uses:` key of a job under `jobs:` — the one place a
#         reusable is called;
#   D<n>  line n is a `uses:` key under some other key (`env:`) — data;
#   S<n>  line n is inside a block scalar (`run: |`, a fixture in `env:`) — text.
# A line with none of these that still carries a known-dead pin is one the
# parser cannot place (a flow-style job `rust: { uses: ... }`).
yaml_context() { # <workflow.yml>
    awk '
        # Return the number of leading spaces in source line s.
        function indent_of(s,   t) { t = s; sub(/[^ ].*$/, "", t); return length(t) }
        # Return true when s is neither blank nor a full-line comment.
        function plain(s) { return (s !~ /^[[:space:]]*$/ && s !~ /^[[:space:]]*#/) }
        BEGIN {
            Q = "[\"'"'"']?"
            KEY_RE = "^[[:space:]]*" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:([[:space:]]|$)"
            ITEM_KEY_RE = "^[[:space:]]*-[[:space:]]+" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:([[:space:]]|$)"
            SCALAR_RE = ":[[:space:]]*[|>][-+0-9]*[[:space:]]*(#.*)?$"
        }
        {
            ind = indent_of($0)
            if (in_scalar) { if (plain($0) && ind <= scalar_indent) in_scalar = 0; else { print "S" NR; next } }
            if (!plain($0)) next
            if ($0 ~ KEY_RE) {
                key = $0; sub(/^[[:space:]]*/, "", key); sub(/[[:space:]]*:.*$/, "", key); gsub(/["'"'"']/, "", key)
                for (k in keyat) if (k + 0 >= ind) delete keyat[k]
                if (key == "uses") {
                    p = -1; for (k in keyat) if (k + 0 < ind && k + 0 > p) p = k + 0
                    g = -1; for (k in keyat) if (k + 0 < p && k + 0 > g) g = k + 0
                    if (p >= 0 && g >= 0 && keyat[g] == "jobs") print "K" NR; else print "D" NR
                }
                keyat[ind] = key
                if ($0 ~ SCALAR_RE) { in_scalar = 1; scalar_indent = ind }
            } else if ($0 ~ ITEM_KEY_RE && $0 ~ SCALAR_RE) {
                # `- run: |`: the scalar hangs off the item'"'"'s key column.
                in_scalar = 1; scalar_indent = ind + 2
            }
        }
    ' "$1"
}

# Every `uses:` of an estate reusable at a full SHA, across the workflows.
# Only a `uses:` that is a key of a job under `jobs:` is a call: the same text
# under `env:`, inside a `run: |` script or a fixture, or after a `#` is data
# and must not move anything. A covered pin the parser cannot place at all (a
# flow-style job `rust: { uses: ... }`) makes the whole file UNSUPPORTED: left
# untouched, named on stderr, exit 2 — never "OK". The lock is edited after
# the loop, once every workflow rewrite is known.
USES_KEY_RE="^[[:space:]]*(-[[:space:]]+)?[\"']?uses[\"']?[[:space:]]*:[[:space:]]*[\"']?"
REUSABLE_REF_RE="hyperpolymath/[A-Za-z0-9_.-]+/\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml@[0-9a-f]{40}"
edits=$(mktemp)   # workflow-key<TAB>stale<TAB>replacement<TAB>keep-stale-edge
unsupported=0
while IFS= read -r -d '' wf; do
    ctx=$(yaml_context "$wf")
    klines=$(grep -o '^K[0-9]*' <<<"$ctx" | tr -d K | sort -n || true)
    placed=$(grep -oE '^[KDS][0-9]*' <<<"$ctx" | tr -d KDS || true)
    skip=0
    while IFS= read -r numbered; do
        lineno="${numbered%%:*}"
        grep -qx "$lineno" <<<"$placed" && continue
        ref=$(grep -oE "$REUSABLE_REF_RE" <<<"${numbered#*:}" | head -1)
        [ -n "$ref" ] || continue
        if lookup_replacement "${ref%@*}" "${ref##*@}"; then
            echo "UNSUPPORTED  ${wf#"$REPO_PATH"/}:$lineno: $ref is a known-dead pin but not a \`uses\` key the parser can place (flow-style job mapping?). Fix by hand; the file was not changed." >&2
            skip=1
        fi
    done < <(grep -nE "$REUSABLE_REF_RE" "$wf" | grep -vE "^[0-9]+:[[:space:]]*#" || true)
    if [ "$skip" -eq 1 ]; then unsupported=$((unsupported + 1)); continue; fi
    pending=()
    for lineno in $klines; do
        line=$(sed -n "${lineno}p" "$wf")
        # A job-level call pinned by tag (`@v1`) carries no SHA: nothing for a
        # SHA table to move, so it is a no-op here — not a grep exit 1 that
        # `set -e` would turn into a silent exit 1 of the whole run.
        ref=$(grep -oE "${USES_KEY_RE}${REUSABLE_REF_RE}" <<<"$line" | sed -E "s/${USES_KEY_RE}//" | head -1 || true)
        [ -n "$ref" ] || continue
        path="${ref%@*}"; stale="${ref##*@}"
        lookup_replacement "$path" "$stale" || continue
        [[ "$replacement" =~ ^[0-9a-f]{40}$ ]] || { echo "fix-stale-reusable-pin: bad replacement SHA in table for $stale" >&2; exit 2; }
        sed -i "${lineno}s|^\([[:space:]]*\(-[[:space:]]\+\)\?[\"']\?uses[\"']\?[[:space:]]*:[[:space:]]*[\"']\?$path\)@$stale|\1@$replacement|" "$wf"
        echo "FIXED  ${wf#"$REPO_PATH"/}:$lineno  $path  ${stale:0:7} -> ${replacement:0:7}  ($reason)"
        rewrites=$((rewrites + 1))
        pending+=("$stale"$'\t'"$replacement"$'\t'"${path%%/.github/workflows/*}")
    done
    # A lock edge is per repository@SHA, shared by every reusable of that
    # repository the workflow calls. When a job of this workflow still calls
    # another reusable of the same repository at the stale SHA (a pin the
    # table does not cover), the stale edge must stay next to the new one.
    for entry in ${pending[@]+"${pending[@]}"}; do
        IFS=$'\t' read -r stale replacement repo_prefix <<<"$entry"
        keep=0
        for other in $klines; do
            sed -n "${other}p" "$wf" | grep -qE "${USES_KEY_RE}${repo_prefix}/\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml@${stale}" && keep=1
        done
        printf '%s\t%s\t%s\t%s\n' ".github/workflows/${wf##*/}" "$stale" "$replacement" "$keep" >> "$edits"
    done
done < <(find "$REPO_PATH/.github/workflows" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) -print0)

# Lock: move each rewritten workflow's own `workflows:` edges first.
if [ -f "$LOCK" ] && [ -s "$edits" ]; then
    sort -u "$edits" -o "$edits"
    while IFS=$'\t' read -r wfkey stale new keep; do
        grep -q "$stale" "$LOCK" || continue
        tmp=$(mktemp)
        awk -v wfkey="    '$wfkey':" -v stale="$stale" -v new="$new" -v keep="$keep" '
            /^workflows:[[:space:]]*$/ { sec = "w" }
            /^dependencies:[[:space:]]*$/ { sec = "d" }
            sec == "w" && /^    '"'"'/ { inwf = ($0 == wfkey) }
            sec == "w" && inwf && /^        - / {
                i = index($0, "@" stale "'"'"'")
                if (i) {
                    moved = substr($0, 1, i) new "'"'"'"
                    # keep: an uncovered caller in this workflow still pins the stale
                    # SHA, so the edge is added beside the old one, not replaced.
                    if (keep == "1") { if (!seen[moved]++) { print; $0 = moved } else next } else $0 = moved
                }
            }
            { print }
        ' "$LOCK" > "$tmp"
        if ! cmp -s "$LOCK" "$tmp"; then mv "$tmp" "$LOCK"; lock_touched=1; else rm -f "$tmp"; fi
    done < "$edits"
    # Then the records: rename a stale record nothing references any more;
    # copy it under the replacement key while an edge still points at the
    # stale SHA — a workflow's own edge, or another record's nested `uses:`
    # (a reusable that itself calls the stale revision keeps needing it).
    while IFS=$'\t' read -r stale new; do
        grep -q "$stale" "$LOCK" || continue
        still=$(awk -v stale="$stale" '
            /^ +- '"'"'/ && index($0, "@" stale "'"'"'") { n++ } END { print n + 0 }' "$LOCK")
        tmp=$(mktemp)
        if [ "$still" -eq 0 ]; then
            sed "s/$stale/$new/g" "$LOCK" > "$tmp"
        else
            awk -v stale="$stale" -v new="$new" '
                /^dependencies:[[:space:]]*$/ { sec = "d" }
                sec == "d" && /^    '"'"'/ { inrec = (index($0, "@" stale "'"'"':") > 0) }
                sec == "d" && inrec { c = $0; gsub(stale, new, c); copy[++nc] = c }
                { print }
                END { for (i = 1; i <= nc; i++) print copy[i] }' "$LOCK" > "$tmp"
            echo "NOTE: actions.lock keeps the record for ${stale:0:7}: a workflow the table does not cover still pins it; the replacement got its own record." >&2
        fi
        if ! cmp -s "$LOCK" "$tmp"; then mv "$tmp" "$LOCK"; lock_touched=1; else rm -f "$tmp"; fi
    done < <(cut -f2,3 "$edits" | sort -u)
    # A kept stale edge means the new edge was ADDED, so the stale record is
    # kept too (the branch above copies it); nothing else to do here.
    [ "$lock_touched" -eq 1 ] && merge_duplicate_records "$LOCK"
fi
rm -f "$edits"

if [ "$rewrites" -eq 0 ] && [ "$unsupported" -eq 0 ]; then echo "OK (no known-dead reusable pins): $REPO_PATH"; elif [ "$rewrites" -gt 0 ]; then echo "Rewrote $rewrites pin(s) in $REPO_PATH"; fi
if [ "$lock_touched" -eq 1 ]; then
    echo "WARN: actions.lock named a stale SHA and was rewritten to the replacement; its nested uses: list was NOT recomputed — run \`gh actions-lock\` or scripts/check-lock-sync.sh before pushing. Exit 3: not a clean success." >&2
fi
if [ "$unsupported" -ne 0 ]; then
    echo "fix-stale-reusable-pin: $unsupported file(s) left untouched (UNSUPPORTED above); not a clean success." >&2
    exit 2
fi
[ "$lock_touched" -eq 1 ] && exit 3
exit 0
