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

# Every row is checked before any caller is touched: a malformed row found
# mid-run would leave earlier callers rewritten and the lock never reached.
# The owners the table names are the owners a caller may be pinned to; the
# candidate scan is built from them, so a private table for another owner
# (`octo/example/.github/workflows/*`) finds its callers.
TABLE_OWNERS_RE=""
validate_table() {
    local n=0 v_path v_stale v_new bad=0 owners=""
    # `|| [ -n ... ]`: a final row the file ends on without a newline is a
    # row too — `read` returns 1 on it with the fields filled in.
    while IFS=$'\t' read -r v_path v_stale v_new _ || [ -n "$v_path" ]; do
        n=$((n + 1))
        case "$v_path" in ''|'#'*) continue ;; esac
        [[ "$v_stale" =~ ^[0-9a-f]{40}$ ]] || { echo "fix-stale-reusable-pin: table row $n: bad stale SHA '$v_stale'" >&2; bad=1; }
        [[ "$v_new" =~ ^[0-9a-f]{40}$ ]] || { echo "fix-stale-reusable-pin: table row $n: bad replacement SHA '$v_new' for ${v_stale:0:7}" >&2; bad=1; }
        # The only wildcard is a terminal `/*` (every reusable of the repository);
        # a `*` anywhere else would be matched literally, never as a glob.
        [[ "$v_path" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+/\.github/workflows/([A-Za-z0-9_.-]+|\*)$ ]] || { echo "fix-stale-reusable-pin: table row $n: bad reusable path '$v_path' (the only wildcard is a terminal /*)" >&2; bad=1; }
        owners="$owners${v_path%%/*}"$'\n'
    done < "$PINS_TABLE"
    [ "$bad" -eq 0 ] || { echo "fix-stale-reusable-pin: table $PINS_TABLE is invalid; nothing was changed" >&2; exit 2; }
    TABLE_OWNERS_RE=$(printf '%s' "$owners" | sed -E 's/[.]/\\./g' | sort -u | paste -sd'|' -)
    [ -n "$TABLE_OWNERS_RE" ] || TABLE_OWNERS_RE="hyperpolymath"
}
validate_table
[ -d "$REPO_PATH/.github/workflows" ] || { echo "OK (no workflows): $REPO_PATH"; exit 0; }
# One workflow-pin fixer per checkout at a time (see lib/pin-fixer-lock.sh).
. "$SCRIPT_DIR/lib/pin-fixer-lock.sh"
pin_fixer_lock "$REPO_PATH"

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
            print spelled[cur]
            for (i = 1; i <= nhead[cur]; i++) print head[cur, i]
            n = 0
            for (u in uses_of) if (index(u, cur SUBSEP) == 1) n++
            if (n > 0) {
                print "        uses:"
                fflush()
                cmd = "sort"
                for (u in uses_of) if (index(u, cur SUBSEP) == 1) print "            - " uses_of[u] | cmd
                close(cmd)
            }
        }
        # A record key is the dependency between its quotes — or a plain
        # scalar, as a hand-added record may write it (`actions/checkout@<sha>:`,
        # exactly the kind of record this fixer meets); the key is compared
        # unquoted so `'a@sha':`, `"a@sha":` and `a@sha:` are one record, and
        # the first spelling seen is the one written back. A record this
        # merger did not recognise was not kept: every line of it fell to the
        # catch-all below with no current record, and was dropped.
        function unq(s) { gsub(/["'"'"']/, "", s); return s }
        function record_key(s) { return (s ~ /^    ["'"'"'][^"'"'"']+["'"'"']:$/ || s ~ /^    [^[:space:]"'"'"'#][^[:space:]]*:$/) }
        function uses_entry(s) { return (s ~ /^            - ["'"'"'][^"'"'"']+["'"'"']$/ || s ~ /^            - [^[:space:]"'"'"'#][^[:space:]]*$/) }
        BEGIN { indep = 0; cur = "" }
        /^dependencies:[[:space:]]*$/ { indep = 1; print; next }
        !indep { print; next }
        record_key($0) {
            key = unq($0)
            if (!(key in seen)) { seen[key] = 1; order[++norder] = key; spelled[key] = $0; nhead[key] = 0; first = 1 } else { first = 0 }
            cur = key; next
        }
        /^        uses:[[:space:]]*$/ { next }
        uses_entry($0) { u = $0; sub(/^            - /, "", u); uses_of[cur SUBSEP unq(u)] = u; next }
        /^        [a-z_]+: / { if (first) { head[cur, ++nhead[cur]] = $0 } ; next }
        {
            # A line the merger does not understand is not the merger'"'"'s to drop:
            # it is kept where it stands, under the record it follows.
            if (first) { head[cur, ++nhead[cur]] = $0 }
            next
        }
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
    while IFS=$'\t' read -r t_path t_stale t_new t_reason || [ -n "$t_path" ]; do
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
# A SHA ref ends at a quote, whitespace, a comment or the end of the value: a
# tag that merely STARTS with a dead SHA (`<sha>-patched`) is a different ref
# the table never named, and must not be rewritten into `<replacement>-patched`.
REF_END="([\"'[:space:]#]|$)"
# Git resolves a commit ID in either case: upper-case hex is the same pin,
# looked up in lower case and rewritten as written.
REUSABLE_REF_RE="(${TABLE_OWNERS_RE})/[A-Za-z0-9_.-]+/\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml@[0-9a-fA-F]{40}"
edits=$(mktemp)   # workflow-key<TAB>stale<TAB>replacement<TAB>keep-stale-edge<TAB>repository
lock_stale=0      # a rewritten caller whose lock entry never had the edge: the lock was already out of sync
unsupported=0
while IFS= read -r -d '' wf; do
    ctx=$(yaml_context "$wf")
    klines=$(grep -o '^K[0-9]*' <<<"$ctx" | tr -d K | sort -n || true)
    placed=$(grep -oE '^[KDS][0-9]*' <<<"$ctx" | tr -d KDS || true)
    skip=0
    while IFS= read -r numbered; do
        lineno="${numbered%%:*}"
        grep -qx "$lineno" <<<"$placed" && continue
        ref=$(grep -oE "${REUSABLE_REF_RE}${REF_END}" <<<"${numbered#*:}" | head -1 | sed -E "s/[\"'[:space:]#]\$//" || true)
        [ -n "$ref" ] || continue
        sha="${ref##*@}"
        if lookup_replacement "${ref%@*}" "${sha,,}"; then
            echo "UNSUPPORTED  ${wf#"$REPO_PATH"/}:$lineno: $ref is a known-dead pin but not a \`uses\` key the parser can place (flow-style job mapping?). Fix by hand; the file was not changed." >&2
            skip=1
        fi
    done < <(grep -nE "${REUSABLE_REF_RE}${REF_END}" "$wf" | grep -vE "^[0-9]+:[[:space:]]*#" || true)
    if [ "$skip" -eq 1 ]; then unsupported=$((unsupported + 1)); continue; fi
    pending=()
    for lineno in $klines; do
        line=$(sed -n "${lineno}p" "$wf")
        # A job-level call pinned by tag (`@v1`) carries no SHA: nothing for a
        # SHA table to move, so it is a no-op here — not a grep exit 1 that
        # `set -e` would turn into a silent exit 1 of the whole run.
        ref=$(grep -oE "${USES_KEY_RE}${REUSABLE_REF_RE}${REF_END}" <<<"$line" | sed -E "s/${USES_KEY_RE}//; s/[\"'[:space:]#]\$//" | head -1 || true)
        [ -n "$ref" ] || continue
        path="${ref%@*}"; stale_raw="${ref##*@}"; stale="${stale_raw,,}"
        lookup_replacement "$path" "$stale" || continue
        [[ "$replacement" =~ ^[0-9a-f]{40}$ ]] || { echo "fix-stale-reusable-pin: bad replacement SHA in table for $stale" >&2; exit 2; }
        # ERE: a `$` inside a BRE group is literal to sed, so the boundary
        # alternation needs -E (and a delimiter the pattern does not use).
        sed -i -E "${lineno}s~^([[:space:]]*(-[[:space:]]+)?[\"']?uses[\"']?[[:space:]]*:[[:space:]]*[\"']?$path)@$stale_raw([\"'[:space:]#]|\$)~\1@$replacement\3~" "$wf"
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
            sed -n "${other}p" "$wf" | grep -qiE "${USES_KEY_RE}${repo_prefix}/\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml@${stale}" && keep=1
        done
        printf '%s\t%s\t%s\t%s\t%s\n' ".github/workflows/${wf##*/}" "$stale" "$replacement" "$keep" "$repo_prefix" >> "$edits"
    done
done < <(find "$REPO_PATH/.github/workflows" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) -print0)

# Lock: move each rewritten workflow's own `workflows:` edges first. An edge
# is per repository@SHA: an unrelated repository that happens to sit at the
# same SHA keeps its edge and its record. A rewritten caller whose lock entry
# has no such edge at all was already out of sync with its lock; that is
# said, and the run ends 3 (the lock needs `gh actions-lock`), never 0.
if [ -f "$LOCK" ] && [ -s "$edits" ]; then
    sort -u "$edits" -o "$edits"
    while IFS=$'\t' read -r wfkey stale new keep repo; do
        # The workflow key in either quote style, or none (`gh actions-lock`
        # writes single quotes; a hand edit may not): compared unquoted.
        if ! awk -v wfkey="$wfkey" -v edge="$repo@$stale" '
                function unq(s) { gsub(/["'"'"']/, "", s); return s }
                function bare(s) { sub(/^ +- /, "", s); sub(/[[:space:]]+(#.*)?$/, "", s); return s }
                /^workflows:[[:space:]]*$/ { sec = "w" }
                /^dependencies:[[:space:]]*$/ { sec = "d" }
                sec == "w" && /^    [^[:space:]#]/ { inwf = (unq($0) == "    " wfkey ":") }
                sec == "w" && inwf && /^        - / && (index($0, "'"'"'" edge "'"'"'") || index($0, "\"" edge "\"") || bare($0) == edge) { found = 1 }
                END { exit !found }' "$LOCK"; then
            echo "WARN: actions.lock has no edge '$repo@${stale:0:7}' under '$wfkey': the lock was already out of sync with this caller — run \`gh actions-lock\` or scripts/check-lock-sync.sh before pushing." >&2
            lock_stale=1
            continue
        fi
        tmp=$(mktemp)
        # The lock is read twice: first for the edges this workflow entry
        # already lists, then to move the stale one — an entry that already
        # holds the replacement edge (a job already at the new revision) is
        # never given it a second time.
        awk -v wfkey="$wfkey" -v stale="$stale" -v new="$new" -v keep="$keep" -v repo="$repo" '
            function unq(s) { gsub(/["'"'"']/, "", s); return s }
            FNR == NR {
                if ($0 ~ /^workflows:[[:space:]]*$/) s1 = "w"
                if ($0 ~ /^dependencies:[[:space:]]*$/) s1 = "d"
                if (s1 == "w" && $0 ~ /^    [^[:space:]#]/) w1 = (unq($0) == "    " wfkey ":")
                if (s1 == "w" && w1 && $0 ~ /^        - /) have[unq($0)] = 1
                next
            }
            /^workflows:[[:space:]]*$/ { sec = "w" }
            /^dependencies:[[:space:]]*$/ { sec = "d" }
            sec == "w" && /^    [^[:space:]#]/ { inwf = (unq($0) == "    " wfkey ":") }
            sec == "w" && inwf && /^        - / {
                # Either quote style, or none (a hand-written edge); the edge
                # keeps the spelling it has.
                q = "'"'"'"; i = index($0, q repo "@" stale q)
                if (!i) { q = "\""; i = index($0, q repo "@" stale q) }
                if (i) moved = substr($0, 1, i + length(repo)) "@" new q
                else {
                    t = $0; sub(/^        - /, "", t); sub(/[[:space:]]+(#.*)?$/, "", t)
                    if (t == repo "@" stale) { i = 1; rest = $0; sub(/^        - [^[:space:]]+/, "", rest); moved = "        - " repo "@" new rest }
                }
                if (i) {
                    # keep: an uncovered caller in this workflow still pins the stale
                    # SHA, so the edge is added beside the old one, not replaced.
                    if (keep == "1") print
                    if (unq(moved) in have) next
                    have[unq(moved)] = 1; $0 = moved
                }
            }
            { print }
        ' "$LOCK" "$LOCK" > "$tmp"
        if ! cmp -s "$LOCK" "$tmp"; then mv "$tmp" "$LOCK"; lock_touched=1; else rm -f "$tmp"; fi
    done < "$edits"
    # Then the records: rename a stale record nothing references any more;
    # copy it under the replacement key while an edge still points at the
    # stale SHA — a workflow's own edge, or another record's nested `uses:`
    # (a reusable that itself calls the stale revision keeps needing it).
    while IFS=$'\t' read -r stale new repo; do
        grep -q "$repo@$stale" "$LOCK" || continue
        still=$(awk -v edge="$repo@$stale" '
            function bare(s) { sub(/^ +- /, "", s); sub(/[[:space:]]+(#.*)?$/, "", s); return s }
            /^ +- / && (index($0, "'"'"'" edge "'"'"'") || index($0, "\"" edge "\"") || bare($0) == edge) { n++ } END { print n + 0 }' "$LOCK")
        tmp=$(mktemp)
        # Only THIS repository'"'"'s record moves: its key line, and its ref: and
        # commit: lines. Another repository at the same SHA is a different
        # record and keeps its SHA.
        awk -v stale="$stale" -v new="$new" -v repo="$repo" -v still="$still" '
            /^dependencies:[[:space:]]*$/ { sec = "d" }
            # The record is the one whose unquoted key IS repo@stale: a key that
            # merely contains it (`evilhyperpolymath/standards@<sha>`) is another
            # record, and so is a plain unquoted key (a hand-written record).
            sec == "d" && /^    [^[:space:]#]/ { k = $0; sub(/^    /, "", k); sub(/:$/, "", k); gsub(/["'"'"']/, "", k); inrec = (k == repo "@" stale) }
            sec == "d" && inrec && (/^    [^[:space:]#]/ || /^        (ref|commit):/) {
                if (still == 0) gsub(stale, new)
                else { c = $0; gsub(stale, new, c); copy[++nc] = c }
            }
            sec == "d" && inrec && still != 0 && !(/^    [^[:space:]#]/ || /^        (ref|commit):/) { copy[++nc] = $0 }
            { print }
            END { for (i = 1; i <= nc; i++) print copy[i] }' "$LOCK" > "$tmp"
        [ "$still" -eq 0 ] || echo "NOTE: actions.lock keeps the record for ${stale:0:7}: a workflow the table does not cover still pins it; the replacement got its own record." >&2
        if ! cmp -s "$LOCK" "$tmp"; then mv "$tmp" "$LOCK"; lock_touched=1; else rm -f "$tmp"; fi
    done < <(cut -f2,3,5 "$edits" | sort -u)
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
if [ "$lock_stale" -eq 1 ]; then
    echo "fix-stale-reusable-pin: a rewritten caller had no matching lock edge (WARN above); the lock is out of sync. Exit 3: not a clean success." >&2
    exit 3
fi
[ "$lock_touched" -eq 1 ] && exit 3
exit 0
