#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-stale-reusable-pin.sh — move a caller off a reusable-workflow pin that is
# KNOWN DEAD. Hypatia rules WF027 and PI001 via PIN-003 (standards pins in a
# measured dead window), for the pins scripts/lib/stale-reusable-pins.tsv
# names. WF016's orphan pins are not in that table, so they are not this
# script's until a human records each one's replacement there; the auto-fix half of
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
# untouched and named), or when actions.lock is left listing a dependency
# twice that the record merger cannot fold (named); exit 3 when the pins were
# rewritten but actions.lock was touched and still needs `gh actions-lock` —
# a dispatcher must not record either as a clean success.
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
        # A row that moves a SHA to itself repairs nothing, yet every caller
        # it matches would be reported FIXED on every run.
        [ "$v_new" != "$v_stale" ] || { echo "fix-stale-reusable-pin: table row $n: replacement equals the stale SHA ${v_stale:0:7}; a row must move the pin" >&2; bad=1; }
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
# anchor_defs/2: the anchors a line defines, outside quoted scalars and comments.
ANCHORS_AWK=$(cat "$SCRIPT_DIR/lib/yaml-anchors.awk")

# The value of anchor <name> as an alias on line <n> of <file> sees it: the
# last `&name` defined above that line (lib/yaml-anchors.awk reads anchors
# outside quoted scalars and comments, and a block scalar's content is
# skipped as text), or "" when there is none. A line ending in an escaped
# line break inside a double-quoted scalar (`CALL: &call "a/b/.github/\`) is
# read joined with the lines that continue it, as YAML reads the value; a
# value still open at its line's end (folded into the next line) is not read,
# "" as well.
anchor_value() { # <file> <name> <line>
    awk -v want="$2" -v upto="$3" "$ANCHORS_AWK"'
        function indent_of(s,   t) { t = s; sub(/[^ ].*$/, "", t); return length(t) }
        BEGIN { blk = -1; got = "" }
        NR >= upto { exit }
        {
            plain = ($0 !~ /^[[:space:]]*$/ && $0 !~ /^[[:space:]]*#/)
            if (blk >= 0 && (!plain || indent_of($0) > blk)) next
            blk = -1
            if (!plain) next
            s = $0
            while (yaml_dq_continued(s) && (getline t) > 0) s = yaml_dq_join(s, t)
            if (anchor_defs(s, d) && (want in d)) got = d[want]
            if (s ~ /:[[:space:]]*[|>][-+0-9]*[[:space:]]*(#.*)?$/) { t = s; sub(/[^[:space:]-].*$/, "", t); blk = length(t) }
        }
        END { if (index(got, "\002")) got = ""; print got }' "$1"
}
pin_fixer_lock "$REPO_PATH"

LOCK="$REPO_PATH/.github/workflows/actions.lock"
rewrites=0; lock_touched=0

# Two callers pinned at two different dead SHAs move to the SAME replacement,
# so after substitution the lock can hold the same dependency key twice. A
# duplicate key is a lockfile GitHub refuses. Keep the first record, fold the
# others' nested `uses:` into it (deduplicated, sorted as gh actions-lock sorts),
# and drop them. Keys and entries compare as GitHub resolves them — the
# repository and a commit ID in either case — so `Fixture/Repo@<sha>` and
# `fixture/repo@<sha>` are one record, written back as first spelled. Rewrite lockfile $1 in place, preserving gh-actions-lock's
# 4-space layout and leaving the file alone when the output is identical.
#
# The nested list may be written flow-style on one line (`uses: []`,
# `uses: ['a@x', 'b@y']`); its entries merge like a block list's, and a
# record keeps ONE `uses:` key — the block form when lists were merged, the
# line as written when there was only the one. A record listed twice whose
# `uses:` is a shape this merger cannot read (a flow list over several lines,
# an alias, a scalar) is not folded at all: merging it could drop its entries
# or leave two `uses:` keys. The merger then changes nothing, names the record
# (UNSUPPORTED) and sets merge_refused.
#
# A record written as a flow mapping on its key line (`'a/b@<sha>': { ref:
# ..., commit: ... }`), or any other line at record indentation that is no
# record key this merger reads, is kept as written, in its place, a record of
# its own: never folded, never moved into the record before it. It refuses
# instead, changing nothing, when the dependency such a record names is
# listed again (a fold could not reach it, and the lock would keep the key
# twice), and when a duplicate being folded away holds a line it has no rule
# for (folded, that line would be lost). A top-level key after
# `dependencies:` (`workflows:` written last) ends the section, and what
# follows it is written after the records, as it stood.
merge_duplicate_records() {
    local lock="$1" tmp
    tmp=$(mktemp)
    if ! awk -v sq="'" '
        # Print dependency cur using its saved header and sorted, unique uses.
        function emit_record(   i, n, out) {
            if (!cur) return
            # Rebuild the record: header lines, then a sorted, unique uses: list.
            print spelled[cur]
            for (i = 1; i <= nhead[cur]; i++) print head[cur, i]
            n = 0
            for (u in uses_of) if (index(u, cur SUBSEP) == 1) n++
            # One flow list and nothing merged into it: kept as written.
            if (nlists[cur] == 1 && (cur in flow_line) && nextra[cur] == 0) { print flow_line[cur]; return }
            if (n > 0 || nextra[cur] > 0) {
                print (cur in uses_head ? uses_head[cur] : "        uses:" flow_cmt[cur])
                fflush()
                cmd = "sort"
                for (u in uses_of) if (index(u, cur SUBSEP) == 1) print "            - " uses_of[u] | cmd
                close(cmd)
                for (i = 1; i <= nextra[cur]; i++) print uses_extra[cur, i]
            } else if (cur in flow_line) print flow_line[cur]
        }
        # A one-line flow list: its items, each a quoted or plain scalar (sq is
        # the single quote), go into uses_of as block entries would. 0, with
        # nothing stored, when an item is any other shape — a nested
        # collection, a key, an embedded quote.
        function flow_items(s,   body, n, parts, i, it, k) {
            body = s; sub(/^        ["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*\[/, "", body); sub(/\][[:space:]]*(#.*)?$/, "", body)
            if (body ~ /^[[:space:]]*$/) return 1
            n = split(body, parts, ",")
            for (i = 1; i <= n; i++) {
                it = parts[i]; sub(/^[[:space:]]+/, "", it); sub(/[[:space:]]+$/, "", it)
                if (it == "" && i == n && n > 1) { parts[i] = ""; continue }
                if (!(it ~ ("^" sq "[^\"" sq "]+" sq "$") || it ~ ("^\"[^\"" sq "]+\"$") || it ~ ("^[^][{}:,\"" sq "#[:space:]]+$"))) return 0
                parts[i] = it
            }
            for (i = 1; i <= n; i++) if (parts[i] != "") { k = cur SUBSEP dep_id(unq(parts[i])); if (!(k in uses_of)) uses_of[k] = parts[i] }
            return 1
        }
        # A record key is the dependency between its quotes — or a plain
        # scalar, as a hand-added record may write it (`actions/checkout@<sha>:`,
        # exactly the kind of record this fixer meets); the key is compared
        # unquoted so `'"'"'a@sha'"'"':`, `"a@sha":` and `a@sha:` are one record, and
        # the first spelling seen is the one written back. A record this
        # merger did not recognise was not kept: every line of it fell to the
        # catch-all below with no current record, and was dropped.
        function unq(s) { gsub(/["'"'"']/, "", s); return s }
        # The dependency a record key or entry names, as GitHub resolves it:
        # the repository in either case (`Fixture/Repo` is `fixture/repo`),
        # a commit ID in either case; a tag or branch is a name, kept as is.
        function dep_id(s,   at, j, ref) {
            at = 0; while ((j = index(substr(s, at + 1), "@")) > 0) at += j
            if (!at) return tolower(s)
            ref = substr(s, at + 1)
            if (length(ref) == 40 && ref !~ /[^0-9A-Fa-f]/) ref = tolower(ref)
            return tolower(substr(s, 1, at)) ref
        }
        function record_key(s) { sub(/[[:space:]]+#.*$/, "", s); sub(/[[:space:]]+$/, "", s); return (s ~ /^    ["'"'"'][^"'"'"']+["'"'"']:$/ || s ~ /^    [^[:space:]"'"'"'#][^[:space:]]*:$/) }
        # A nested `uses:` entry, quoted or plain, with or without a trailing
        # comment; it is deduplicated on its value, the comment kept with
        # the first spelling seen.
        # Trailing whitespace (a carriage return too) is no part of a key or
        # an entry: `owner/repo@sha:   ` is the record `owner/repo@sha`.
        function uses_entry(s) { return (s ~ /^            - ["'"'"'][^"'"'"']+["'"'"']([[:space:]]+#.*|[[:space:]]*)$/ || s ~ /^            - [^[:space:]"'"'"'#][^[:space:]]*([[:space:]]+#.*|[[:space:]]*)$/) }
        function entry_value(s) { sub(/^            - /, "", s); sub(/[[:space:]]+#.*$/, "", s); sub(/[[:space:]]+$/, "", s); return unq(s) }
        # The dependency an unread record line names, or "": its key quoted
        # either way, or plain, before the colon (sq is the single quote).
        function line_dep(s,   t) {
            t = s; sub(/^    /, "", t)
            if (match(t, "^(\"[^\"]*\"|" sq "[^" sq "]*" sq ")[[:space:]]*:")) { t = substr(t, 1, RLENGTH); sub(/[[:space:]]*:$/, "", t); return dep_id(unq(t)) }
            if (match(t, "^[^[:space:]\"" sq "#][^[:space:]]*:([[:space:]]|$)")) { t = substr(t, 1, RLENGTH); sub(/:[[:space:]]*$/, "", t); return dep_id(t) }
            return ""
        }
        BEGIN { indep = 0; cur = ""; in_uses = 0; after = 0 }
        after { tail[++ntail] = $0; next }
        /^dependencies:[[:space:]]*$/ { indep = 1; print; next }
        indep && /^[^[:space:]#]/ { after = 1; tail[++ntail] = $0; next }
        !indep { print; next }
        # A record key or one of its own fields ends a nested `uses:` list.
        /^    [^[:space:]]/ || /^        [^[:space:]]/ { in_uses = 0 }
        record_key($0) {
            key = $0; sub(/[[:space:]]+#.*$/, "", key); sub(/[[:space:]]+$/, "", key); key = unq(key)
            sub(/^ +/, "", key); sub(/:$/, "", key); key = dep_id(key)
            if (!(key in seen)) { seen[key] = 1; order[++norder] = key; spelled[key] = $0; nhead[key] = 0; first = 1 } else { first = 0 }
            nrec[key]++; cur = key; next
        }
        /^    [^[:space:]#]/ {
            key = "\034" NR; order[++norder] = key; spelled[key] = $0; nhead[key] = 0; nrec[key] = 1
            d = line_dep($0); if (d != "") { opaque_dep[key] = d; nopaque[d]++ }
            cur = key; first = 1; next
        }
        # What stands between `dependencies:` and its first record (a
        # comment) stays there.
        cur == "" { print; next }
        # The nested list header, quoted or not, with or without a trailing
        # comment (kept as first spelled); only lines inside a list it opens
        # are its entries.
        /^        ["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*(#.*)?$/ { in_uses = 1; nlists[cur]++; if (!(cur in uses_head)) uses_head[cur] = $0; next }
        /^        ["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*\[.*\][[:space:]]*(#.*)?$/ && flow_items($0) {
            nlists[cur]++
            if (!(cur in flow_line)) {
                flow_line[cur] = $0; c = $0; sub(/^.*\]/, "", c)
                flow_cmt[cur] = (c ~ /#/ ? c : "")
            }
            next
        }
        # Any other `uses:` value is one the merger cannot read. The key may
        # be quoted (`"uses":`, `'"'"'uses'"'"':`), as `ref` and `commit` may.
        /^        ["'"'"']?uses["'"'"']?[[:space:]]*:/ { odd[cur] = 1 }
        in_uses && uses_entry($0) { u = $0; sub(/^            - /, "", u); k = cur SUBSEP dep_id(entry_value($0)); if (!(k in uses_of)) uses_of[k] = u; next }
        # Any other line inside the list (a comment, a shape this merger has
        # no rule for) stays under the `uses:` header it came from: moved
        # above it, it would be a child with no parent key.
        in_uses { uses_extra[cur, ++nextra[cur]] = $0; next }
        /^        [a-z_]+: / { if (first) { head[cur, ++nhead[cur]] = $0 } ; next }
        {
            # A line the merger does not understand is not the merger'"'"'s to drop:
            # it is kept where it stands, under the record it follows. Under a
            # duplicate being folded away it has nowhere to stand: unless it is
            # blank or a comment, the merger refuses.
            if (first) { head[cur, ++nhead[cur]] = $0 }
            else if ($0 !~ /^[[:space:]]*(#.*)?$/) { lost[cur] = lost[cur] (lost[cur] == "" ? "" : "; ") NR ": " $0 }
            next
        }
        END {
            for (k = 1; k <= norder; k++) if (odd[order[k]] && nrec[order[k]] > 1) {
                name = spelled[order[k]]; sub(/[[:space:]]+#.*$/, "", name); name = unq(name); sub(/^ +/, "", name); sub(/:$/, "", name)
                printf "UNSUPPORTED  .github/workflows/actions.lock: the record %s is listed %d times and its uses: is a shape the merger cannot fold (a flow list over several lines, an alias, a scalar); the duplicates were left in place. Merge them by hand.\n", name, nrec[order[k]] > "/dev/stderr"
                refused = 1
            }
            for (k = 1; k <= norder; k++) {
                cur = order[k]
                if (cur in lost) {
                    name = spelled[cur]; sub(/[[:space:]]+#.*$/, "", name); name = unq(name); sub(/^ +/, "", name); sub(/:$/, "", name)
                    printf "UNSUPPORTED  .github/workflows/actions.lock: the record %s is listed %d times and a duplicate holds a line the merger has no rule for (line %s); the duplicates were left in place. Merge them by hand.\n", name, nrec[cur], lost[cur] > "/dev/stderr"
                    refused = 1
                }
                if ((cur in opaque_dep) && (nopaque[opaque_dep[cur]] > 1 || (opaque_dep[cur] in seen))) {
                    printf "UNSUPPORTED  .github/workflows/actions.lock: %s is a record the merger cannot read (written as a flow mapping?) and its dependency is listed again; the duplicates were left in place. Merge them by hand.\n", spelled[cur] > "/dev/stderr"
                    refused = 1
                }
            }
            if (refused) exit 4
            for (k = 1; k <= norder; k++) {
                cur = order[k]
                emit_record()
            }
            for (k = 1; k <= ntail; k++) print tail[k]
        }
    ' "$lock" > "$tmp"; then
        rm -f "$tmp"; merge_refused=1; return 0
    fi
    if ! cmp -s "$lock" "$tmp"; then pin_fixer_replace "$tmp" "$lock"; else rm -f "$tmp"; fi
}

# Find the first PINS_TABLE row for reusable path $1 at stale SHA $2, including
# wildcard paths. Set replacement/reason and return 0 on a match; otherwise
# clear both variables and return 1.
lookup_replacement() { # <reusable-path> <stale-sha>
    local t_path t_stale t_new t_reason t_path_lc
    replacement=""; reason=""
    while IFS=$'\t' read -r t_path t_stale t_new t_reason || [ -n "$t_path" ]; do
        [[ -z "$t_path" || "$t_path" == \#* ]] && continue
        t_path_lc="${t_path,,}"
        [[ "$t_stale" == "$2" ]] || continue
        # GitHub resolves an owner or repository name in either case: the
        # table and the caller are compared case-folded.
        if [[ "${t_path,,}" == "${1,,}" ]] || { [[ "$t_path" == */\* ]] && [[ "${1,,}" == "${t_path_lc%\*}"* ]]; }; then
            replacement="$t_new"; reason="$t_reason"; return 0
        fi
    done < "$PINS_TABLE"
    return 1
}

# Does <file> hold, anywhere in its text, a reusable pin the table names as
# known-dead?
dead_ref_in_file() { # <file>
    local ref sha
    while IFS= read -r ref; do
        [ -n "$ref" ] || continue
        sha="${ref##*@}"
        lookup_replacement "${ref%@*}" "${sha,,}" && return 0
    done < <(decode_dq < "$1" | grep -oiE "$REUSABLE_REF_RE" || true)
    return 1
}

# Lines on stdin as YAML reads their double-quoted scalars: escapes decoded
# (`"\x68yperpolymath/..."` is the reference it spells), every other byte as
# written (lib/yaml-anchors.awk, yaml_decode_dq).
decode_dq() { awk "$ANCHORS_AWK"'{ print yaml_decode_dq($0) }'; }

# Line <n> of <file> as YAML reads it: double-quoted escapes decoded, and a
# line ending in an escaped line break inside a double-quoted scalar
# (`uses: "hyperpolymath/standards/.github/workflows/rust-ci-\`) joined with
# the lines that continue it (lib/yaml-anchors.awk).
dq_logical_line() { # <file> <lineno>
    awk -v ln="$2" "$ANCHORS_AWK"'
        NR == ln { j = $0 }
        NR > ln { j = yaml_dq_join(j, $0) }
        NR >= ln && !yaml_dq_continued(j) { print yaml_decode_dq(j); done = 1; exit }
        END { if (!done && j != "") print yaml_decode_dq(j) }' "$1"
}

# Print YAML context markers for workflow file $1, one per line that matters:
#   K<n>  line n is a `uses:` key of a job under `jobs:` — the one place a
#         reusable is called;
#   D<n>  line n is a `uses:` key under some other key (`env:`) — data;
#   S<n>  line n is inside a block scalar (`run: |`, a fixture in `env:`) — text;
#   U<n>  line n continues the value of a job's `uses:` key that holds none on
#         its own line (`uses: >-`, or `uses:` then the reference below): the
#         call is there, but not where the rewrite reads it.
# A line with none of these that still carries a known-dead pin is one the
# parser cannot place (a flow-style job `rust: { uses: ... }`). Keys are
# placed by indentation, which says nothing inside a flow collection opened
# on an earlier line: there a `uses:` key line is K when its indentation
# reads a job key (`rust: {` then `  uses: ...`, as KYAML indents it), and
# otherwise is left unplaced — never D. `jobs: {`, `rust: {`, `uses: ...` may
# all stand in column 0, and that call must be reported, not passed as data.
yaml_context() { # <workflow.yml>
    awk -v sq="'" '
        # Return the number of leading spaces in source line s.
        function indent_of(s,   t) { t = s; sub(/[^ ].*$/, "", t); return length(t) }
        # Return true when s is neither blank nor a full-line comment.
        function plain(s) { return (s !~ /^[[:space:]]*$/ && s !~ /^[[:space:]]*#/) }
        # Can a scalar start after `seen` (the line so far) at flow depth d?
        # Where a value starts (`key: `, a list dash, past any anchor or tag),
        # or inside a flow collection after `[`, `{`, `,` or `:`.
        function scalar_start(seen, d) {
            if (seen ~ VALUE_START_RE) return 1
            return d > 0 && seen ~ /[][{,:][[:space:]]*([&!][^[:space:]]*[[:space:]]+)*$/
        }
        # The flow depth line s leaves, from depth d at its start: `{` and `[`
        # open a collection where a value starts or inside one already open,
        # `}` and `]` close one; quoted scalars and a comment are skipped.
        # (Hypatia reads depth so: PinSites.line_depths/1.)
        function depth_after(s, d,   i, n, c, j, seen) {
            n = length(s); i = 1; seen = ""
            while (i <= n) {
                c = substr(s, i, 1)
                if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) break
                if ((c == "\"" || c == sq) && scalar_start(seen, d)) {
                    j = i + 1
                    if (c == "\"") { while (j <= n && substr(s, j, 1) != "\"") { if (substr(s, j, 1) == "\\") j++; j++ } }
                    else { while (j <= n && (substr(s, j, 1) != sq || substr(s, j + 1, 1) == sq)) j += (substr(s, j, 1) == sq ? 2 : 1) }
                    seen = seen "x"; i = j + 1; continue
                }
                if ((c == "{" || c == "[") && (d > 0 || seen ~ VALUE_START_RE)) d++
                else if ((c == "}" || c == "]") && d > 0) d--
                seen = seen c; i++
            }
            return d
        }
        BEGIN {
            VALUE_START_RE = "(^[[:space:]]*(-[[:space:]]+)*|:[[:space:]]+)([&!][^[:space:]]*[[:space:]]+)*$"
            Q = "[\"'"'"']?"
            KEY_RE = "^[[:space:]]*" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:([[:space:]]|$)"
            ITEM_KEY_RE = "^[[:space:]]*-[[:space:]]+" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:([[:space:]]|$)"
            SCALAR_RE = ":[[:space:]]*[|>][-+0-9]*[[:space:]]*(#.*)?$"
            # A `uses:` key with no value on its line (a block scalar, or nothing).
            OPEN_RE = "^[[:space:]]*" Q "uses" Q "[[:space:]]*:[[:space:]]*(&[^[:space:]]+[[:space:]]*)?([|>][-+0-9]*)?[[:space:]]*(#.*)?$"
        }
        {
            ind = indent_of($0)
            if (in_scalar) { if (plain($0) && ind <= scalar_indent) in_scalar = 0; else { print (in_uses ? "U" : "S") NR; next } }
            in_uses = 0
            if (!plain($0)) next
            d0 = depth; depth = depth_after($0, depth)
            if ($0 ~ KEY_RE) {
                key = $0; sub(/^[[:space:]]*/, "", key); sub(/[[:space:]]*:.*$/, "", key); gsub(/["'"'"']/, "", key)
                for (k in keyat) if (k + 0 >= ind) delete keyat[k]
                if (key == "uses") {
                    p = -1; for (k in keyat) if (k + 0 < ind && k + 0 > p) p = k + 0
                    g = -1; for (k in keyat) if (k + 0 < p && k + 0 > g) g = k + 0
                    if (p >= 0 && g >= 0 && keyat[g] == "jobs") {
                        print "K" NR
                        # The value continues below the key: its lines are the call.
                        if ($0 ~ OPEN_RE) { in_scalar = 1; scalar_indent = ind; in_uses = 1; keyat[ind] = key; next }
                    } else if (d0 == 0) print "D" NR
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
# A SHA ref ends at a quote, whitespace or the end of the value: a tag that
# merely STARTS with a dead SHA (`<sha>-patched`) is a different ref the table
# never named, and must not be rewritten into `<replacement>-patched`. A `#`
# starts a comment only after whitespace, so `<sha>#patched` is one plain
# scalar — that other ref — and a comment is reached through the whitespace.
REF_END="([\"'[:space:]]|$)"
# The fallback that looks for a pin the parser could not place also stops at
# flow punctuation: `{ uses: x@<sha>}` is a flow-style caller, and missing it
# would let the run report OK.
FALLBACK_REF_END="([\"'[:space:],}]|]|$)"
# Git resolves a commit ID in either case, and GitHub an owner or repository
# name: an upper-case spelling is the same pin, matched case-insensitively,
# looked up in lower case and rewritten as written.
REUSABLE_REF_RE="(${TABLE_OWNERS_RE})/[A-Za-z0-9_.-]+/\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml@[0-9a-fA-F]{40}"
# A line with its comment dropped and every quoted scalar that is neither a
# key, the value of a `uses` key (past any anchor or tag) nor the whole start
# of the line (a value continued from the key above it) emptied, quote-aware. The fallback scan
# for pins the parser cannot place reads this, so a dead pin named in a
# trailing comment or inside a `name:` string is data, not an UNSUPPORTED
# caller.
mask_line() {
    printf '%s\n' "$1" | awk -v sq="'" '{
        s = $0; out = ""; n = length(s); i = 1
        tail = "(^|[-{,[:space:]])[\"" sq "]?uses[\"" sq "]?[[:space:]]*:[[:space:]]*([&!][^[:space:],}]*[[:space:]]+)*$"
        while (i <= n) {
            c = substr(s, i, 1)
            if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) break
            if (c == "\"" || c == sq) {
                j = i + 1
                if (c == "\"") { while (j <= n && substr(s, j, 1) != "\"") { if (substr(s, j, 1) == "\\") j++; j++ } }
                else { while (j <= n && (substr(s, j, 1) != sq || substr(s, j + 1, 1) == sq)) j += (substr(s, j, 1) == sq ? 2 : 1) }
                if (substr(s, j + 1) ~ /^[[:space:]]*:/ || tolower(out) ~ tail || out ~ /^[[:space:]]*$/) out = out substr(s, i, j - i + 1)
                else out = out c c
                i = j + 1; continue
            }
            out = out c; i++
        }
        print out
    }'
}
# The value may carry YAML node properties — an anchor or a tag, in either
# order (`uses: &call owner/repo/...@<sha>`, `uses: !!str owner/...@<sha>`).
# The rewrite does not edit such values (every alias of an anchor would
# change with it, and the rewrite reads the reference right after the
# colon), so such a value is reported, never skipped as clean.
ANCHOR_RE="([&!][^[:space:],}]*[[:space:]]+)+"
FALLBACK_USES_RE="(^|[-{,[:space:]])[\"']?uses[\"']?[[:space:]]*:[[:space:]]*(${ANCHOR_RE})?[\"']?${REUSABLE_REF_RE}${FALLBACK_REF_END}"
ANCHORED_USES_RE="(^|[-{,[:space:]])[\"']?uses[\"']?[[:space:]]*:[[:space:]]*${ANCHOR_RE}[\"']?${REUSABLE_REF_RE}${FALLBACK_REF_END}"
# The dead reference a job-level `uses:` line carries, as written, or "".
k_ref() { grep -oiE "${USES_KEY_RE}${REUSABLE_REF_RE}${REF_END}" <<<"$1" | sed -E "s/${USES_KEY_RE}//; s/[\"'[:space:]]\$//" | head -1 || true; }

# Every replacement the run would write, before one is written. Overlapping
# table rows are applied first match first, so two reusables of one
# repository at one stale SHA can be sent to two different replacements.
# One lock edge and one dependency record per repository@SHA cannot carry
# both, and which caller gets which cannot be said in the lock: the run
# refuses, and nothing is changed.
declare -A move_to=()
conflict=0
while IFS= read -r -d '' wf; do
    for lineno in $(yaml_context "$wf" | grep -o '^K[0-9]*' | tr -d K | sort -n || true); do
        ref=$(k_ref "$(sed -n "${lineno}p" "$wf")")
        [ -n "$ref" ] || continue
        stale="${ref##*@}"; stale="${stale,,}"
        lookup_replacement "${ref%@*}" "$stale" || continue
        key="${ref%%/.github/workflows/*}"; key="${key,,}@$stale"
        if [ -n "${move_to[$key]:-}" ] && [ "${move_to[$key]}" != "$replacement" ]; then
            echo "CONFLICT  ${wf#"$REPO_PATH"/}:$lineno: $key would move to ${move_to[$key]:0:7} here and to ${replacement:0:7} elsewhere; one lock edge and one record cannot carry both. Fix the table or the callers by hand; nothing was changed." >&2
            conflict=1
        fi
        move_to[$key]="$replacement"
    done
done < <(find "$REPO_PATH/.github/workflows" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) -print0)
[ "$conflict" -eq 0 ] || exit 2

edits=$(mktemp)   # workflow-key<TAB>stale<TAB>replacement<TAB>keep-stale-edge<TAB>repository
lock_stale=0      # a rewritten caller whose lock entry never had the edge: the lock was already out of sync
unsupported=0
merge_refused=0   # duplicate lock records the merger could not fold (UNSUPPORTED)
while IFS= read -r -d '' wf; do
    ctx=$(yaml_context "$wf")
    klines=$(grep -o '^K[0-9]*' <<<"$ctx" | tr -d K | sort -n || true)
    placed=$(grep -oE '^[KDS][0-9]*' <<<"$ctx" | tr -d KDS || true)
    skip=0
    while IFS= read -r numbered; do
        lineno="${numbered%%:*}"
        # A placed `uses:` key is the rewrite's, unless its value is anchored:
        # the rewrite reads the reference right after the colon, finds none
        # there, and would otherwise report the file clean.
        if grep -qx "$lineno" <<<"$placed"; then
            mask_line "${numbered#*:}" | grep -qiE "$ANCHORED_USES_RE" || continue
        fi
        # Only a `uses:` key outside comments and other scalars can be a
        # caller the parser failed to place (a flow-style job); a dead pin
        # named in a comment or a `name:` string is text.
        # Every such site on the line is checked, each read from the masked
        # line the guard read: a dead ref inside a `name:` string beside a
        # live `uses:` is text, and a live first site does not hide a dead
        # second one.
        while IFS= read -r ref; do
            [ -n "$ref" ] || continue
            sha="${ref##*@}"
            if lookup_replacement "${ref%@*}" "${sha,,}"; then
                echo "UNSUPPORTED  ${wf#"$REPO_PATH"/}:$lineno: $ref is a known-dead pin but not a \`uses\` key the parser can place (a flow-style job mapping, or a value carrying an anchor or tag). Fix by hand; the file was not changed." >&2
                skip=1
            fi
        done < <(mask_line "${numbered#*:}" | grep -oiE "$FALLBACK_USES_RE" | grep -oiE "$REUSABLE_REF_RE" || true)
    done < <(grep -niE "${REUSABLE_REF_RE}${FALLBACK_REF_END}" "$wf" | grep -vE "^[0-9]+:[[:space:]]*#" || true)
    # A job's `uses:` whose value continues below the key (`uses: >-`): the
    # rewrite reads the reference on the key line, finds none, and would
    # report the file clean. A known-dead pin on such a line is reported.
    for lineno in $(grep -o '^U[0-9]*' <<<"$ctx" | tr -d U || true); do
        while IFS= read -r ref; do
            [ -n "$ref" ] || continue
            sha="${ref##*@}"
            if lookup_replacement "${ref%@*}" "${sha,,}"; then
                echo "UNSUPPORTED  ${wf#"$REPO_PATH"/}:$lineno: $ref is a known-dead pin in a \`uses:\` value continued below its key (a block scalar such as \`uses: >-\`). Fix by hand; the file was not changed." >&2
                skip=1
            fi
        done < <(mask_line "$(sed -n "${lineno}p" "$wf" | decode_dq)" | grep -oiE "${REUSABLE_REF_RE}${FALLBACK_REF_END}" | grep -oiE "$REUSABLE_REF_RE" || true)
    done
    # A double-quoted value may spell the reference with escapes (`uses:
    # "\x68yperpolymath/..."`, or a line break escaped and continued on the
    # next line): GitHub runs the decoded call, but the rewrite
    # and the scan above read the bytes as written, find no pin, and would
    # report the file clean. Every line outside data and text that holds an
    # escape is read decoded; a known-dead pin found so at a `uses` key is
    # reported, never rewritten through its escapes.
    textual=$(grep -oE '^[DSU][0-9]*' <<<"$ctx" | tr -d DSU || true)
    while IFS= read -r numbered; do
        lineno="${numbered%%:*}"
        grep -qx "$lineno" <<<"$textual" && continue
        decoded=$(dq_logical_line "$wf" "$lineno")
        [ "$decoded" != "${numbered#*:}" ] || continue
        while IFS= read -r ref; do
            [ -n "$ref" ] || continue
            sha="${ref##*@}"
            if lookup_replacement "${ref%@*}" "${sha,,}"; then
                echo "UNSUPPORTED  ${wf#"$REPO_PATH"/}:$lineno: $ref is a known-dead pin written with escapes in a double-quoted \`uses\` value; the rewrite edits a reference as written only. Fix by hand; the file was not changed." >&2
                skip=1
            fi
        done < <(mask_line "$decoded" | grep -oiE "$FALLBACK_USES_RE" | grep -oiE "$REUSABLE_REF_RE" || true)
    done < <(grep -n "[\\]" "$wf" | grep -vE "^[0-9]+:[[:space:]]*#" || true)
    # A job's `uses:` given as an alias (`uses: *call`) runs the anchor's
    # value, defined elsewhere in the file (`CALL: &call owner/repo/...@<sha>`):
    # the rewrite reads no reference on the key line. A known-dead pin as
    # that value is reported; the anchor is not rewritten from here. An alias
    # whose anchor this reader cannot find (the anchor reader is line-local:
    # `&call` after another item of a flow sequence begun on an earlier line
    # is not seen) may be that pin: in a file that holds a known-dead pin
    # anywhere, it is reported too, never passed as OK.
    # A flow-style job (`rust: { uses: *call }`, `jobs: { rust: { uses:
    # *call } }`) is no key the parser places, but runs its alias all the
    # same: every `uses: *name` outside comments and quoted text, on a line
    # that is neither data nor text, is read the same way.
    alias_lines=$klines
    while IFS= read -r numbered; do
        lineno="${numbered%%:*}"
        grep -qx "$lineno" <<<"$placed" || alias_lines="$alias_lines $lineno"
    done < <(grep -n '[*]' "$wf" | grep -vE "^[0-9]+:[[:space:]]*#" || true)
    for lineno in $alias_lines; do
        while IFS= read -r alias; do
            [ -n "$alias" ] || continue
            value=$(anchor_value "$wf" "$alias" "$lineno")
            if [ -z "$value" ] && dead_ref_in_file "$wf"; then
                echo "UNSUPPORTED  ${wf#"$REPO_PATH"/}:$lineno: \`uses: *$alias\` names an anchor this parser cannot read, and the file holds a known-dead pin it may hold. Fix by hand; the file was not changed." >&2
                skip=1; continue
            fi
            while IFS= read -r ref; do
                [ -n "$ref" ] || continue
                sha="${ref##*@}"
                if lookup_replacement "${ref%@*}" "${sha,,}"; then
                    echo "UNSUPPORTED  ${wf#"$REPO_PATH"/}:$lineno: \`uses: *$alias\` runs $ref, a known-dead pin held by the anchor \`&$alias\`. Fix by hand; the file was not changed." >&2
                    skip=1
                fi
            done < <(printf '%s\n' "$value" | grep -oiE "^${REUSABLE_REF_RE}\$" || true)
        done < <(mask_line "$(sed -n "${lineno}p" "$wf")" | grep -oE "(^|[-{,[:space:]])[\"']?uses[\"']?[[:space:]]*:[[:space:]]*\*[A-Za-z0-9_.-]+" | sed 's/.*\*//' || true)
    done
    if [ "$skip" -eq 1 ]; then unsupported=$((unsupported + 1)); continue; fi
    pending=()
    for lineno in $klines; do
        line=$(sed -n "${lineno}p" "$wf")
        # A job-level call pinned by tag (`@v1`) carries no SHA: nothing for a
        # SHA table to move, so it is a no-op here — not a grep exit 1 that
        # `set -e` would turn into a silent exit 1 of the whole run.
        ref=$(k_ref "$line")
        [ -n "$ref" ] || continue
        path="${ref%@*}"; stale_raw="${ref##*@}"; stale="${stale_raw,,}"
        lookup_replacement "$path" "$stale" || continue
        [[ "$replacement" =~ ^[0-9a-f]{40}$ ]] || { echo "fix-stale-reusable-pin: bad replacement SHA in table for $stale" >&2; exit 2; }
        # ERE: a `$` inside a BRE group is literal to sed, so the boundary
        # alternation needs -E (and a delimiter the pattern does not use).
        # The path as the caller spells it, matched literally (a `.` in it
        # is a dot).
        path_re=$(printf '%s' "$path" | sed -E 's/[][\.^$*+?(){}|~\\]/\\&/g')
        sed -i -E "${lineno}s~^([[:space:]]*(-[[:space:]]+)?[\"']?uses[\"']?[[:space:]]*:[[:space:]]*[\"']?$path_re)@$stale_raw([\"'[:space:]]|\$)~\1@$replacement\3~" "$wf"
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
        # The repository matched literally and the SHA to its end, as the
        # caller matcher does: `<stale>-patched` is another ref, not this one.
        repo_re=$(printf '%s' "$repo_prefix" | sed -E 's/[][\.^$*+?(){}|~\\]/\\&/g')
        for other in $klines; do
            sed -n "${other}p" "$wf" | grep -qiE "${USES_KEY_RE}${repo_re}/\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml@${stale}${REF_END}" && keep=1
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
    # A lock with CRLF line endings is edited as LF — every pass below reads
    # `$`-anchored lines, and a carriage return would hide each record and
    # edge from them — and written back with CRLF once the edits are done.
    lock_crlf=0
    if grep -q $'\r$' "$LOCK"; then
        lock_crlf=1; tmp=$(mktemp)
        sed 's/\r$//' "$LOCK" > "$tmp" && pin_fixer_replace "$tmp" "$LOCK"
    fi
    while IFS=$'\t' read -r wfkey stale new keep repo; do
        # The workflow key in either quote style, or none (`gh actions-lock`
        # writes single quotes; a hand edit may not): compared unquoted.
        # Repository and SHA are compared case-folded (GitHub and git read
        # either case); every line keeps the spelling it has.
        if ! awk -v wfkey="$wfkey" -v edge="$repo@$stale" '
                function unq(s) { gsub(/["'"'"']/, "", s); return s }
                function keyv(s) { sub(/[[:space:]]+#.*$/, "", s); sub(/[[:space:]]+$/, "", s); return unq(s) }
                function bare(s) { sub(/^ +- /, "", s); sub(/[[:space:]]+(#.*)?$/, "", s); return s }
                BEGIN { edge = tolower(edge) }
                /^workflows:[[:space:]]*$/ { sec = "w" }
                /^dependencies:[[:space:]]*$/ { sec = "d" }
                sec == "w" && /^    [^[:space:]#]/ { inwf = (keyv($0) == "    " wfkey ":") }
                # The edge is the list item'"'"'s own value: the same text quoted in
                # a trailing comment of another edge is not it.
                sec == "w" && inwf && /^        - / { if (tolower(unq(bare($0))) == edge) found = 1 }
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
            function keyv(s) { sub(/[[:space:]]+#.*$/, "", s); sub(/[[:space:]]+$/, "", s); return unq(s) }
            # An edge compared by its value: the list dash, a trailing
            # comment, quotes and case are not part of it.
            function edge_val(s) { sub(/^ +- /, "", s); sub(/[[:space:]]+#.*$/, "", s); sub(/[[:space:]]+$/, "", s); return tolower(unq(s)) }
            FNR == NR {
                if ($0 ~ /^workflows:[[:space:]]*$/) s1 = "w"
                if ($0 ~ /^dependencies:[[:space:]]*$/) s1 = "d"
                if (s1 == "w" && $0 ~ /^    [^[:space:]#]/) w1 = (keyv($0) == "    " wfkey ":")
                if (s1 == "w" && w1 && $0 ~ /^        - /) have[edge_val($0)] = 1
                next
            }
            /^workflows:[[:space:]]*$/ { sec = "w" }
            /^dependencies:[[:space:]]*$/ { sec = "d" }
            sec == "w" && /^    [^[:space:]#]/ { inwf = (keyv($0) == "    " wfkey ":") }
            sec == "w" && inwf && /^        - / {
                # Either quote style, or none (a hand-written edge); the edge
                # keeps the spelling it has.
                l = tolower($0); e = tolower(repo "@" stale); i = 0
                # Only an item whose own value is the stale edge moves; the
                # same text in its comment is not the edge. Its first quoted
                # occurrence is then the value, before any comment.
                if (edge_val($0) == e) {
                    q = "'"'"'"; i = index(l, q e q)
                    if (!i) { q = "\""; i = index(l, q e q) }
                    # Everything after the closing quote (a comment) stays.
                    if (i) moved = substr($0, 1, i + length(repo)) "@" new q substr($0, i + length(e) + 2)
                    else {
                        t = l; sub(/^        - /, "", t); sub(/[[:space:]]+(#.*)?$/, "", t)
                        if (t == e) { i = 1; rest = $0; sub(/^        - [^[:space:]]+/, "", rest); spell = substr($0, 11, length(repo)); moved = "        - " spell "@" new rest }
                    }
                }
                if (i) {
                    # keep: an uncovered caller in this workflow still pins the stale
                    # SHA, so the edge is added beside the old one, not replaced.
                    if (keep == "1") print
                    if (edge_val(moved) in have) next
                    have[edge_val(moved)] = 1; $0 = moved
                }
            }
            { print }
        ' "$LOCK" "$LOCK" > "$tmp"
        if ! cmp -s "$LOCK" "$tmp"; then pin_fixer_replace "$tmp" "$LOCK"; lock_touched=1; else rm -f "$tmp"; fi
    done < "$edits"
    # Then the records: rename a stale record nothing references any more;
    # copy it under the replacement key while an edge still points at the
    # stale SHA — a workflow's own edge, or another record's nested `uses:`
    # (a reusable that itself calls the stale revision keeps needing it).
    while IFS=$'\t' read -r stale new repo; do
        grep -qiF "$repo@$stale" "$LOCK" || continue
        still=$(awk -v edge="$repo@$stale" '
            function unq(s) { gsub(/["'"'"']/, "", s); return s }
            function bare(s) { sub(/^ +- /, "", s); sub(/[[:space:]]+(#.*)?$/, "", s); return s }
            BEGIN { edge = tolower(edge) }
            /^ +- / { if (tolower(unq(bare($0))) == edge) n++ } END { print n + 0 }' "$LOCK")
        tmp=$(mktemp)
        # Only THIS repository'"'"'s record moves: its key line, and its ref: and
        # commit: lines. Another repository at the same SHA is a different
        # record and keeps its SHA.
        awk -v stale="$stale" -v new="$new" -v repo="$repo" -v still="$still" '
            /^dependencies:[[:space:]]*$/ { sec = "d" }
            # A top-level key (`workflows:` written after `dependencies:`)
            # ends the section and the record in it: a kept record'"'"'s copy is
            # written there, inside the section, never after what follows.
            sec == "d" && /^[^[:space:]#]/ && !/^dependencies:[[:space:]]*$/ {
                for (c = 1; c <= nc; c++) print copy[c]
                nc = 0; sec = ""; inrec = 0
            }
            # The record is the one whose unquoted key IS repo@stale: a key that
            # merely contains it (`evilhyperpolymath/standards@<sha>`) is another
            # record, and so is a plain unquoted key (a hand-written record).
            # The SHA is replaced in either case wherever it stands in the
            # record'"'"'s own key, ref and commit (own_sub); the rest of the line
            # keeps its spelling.
            function isub(s, old, new,   l, o, i, out) {
                out = ""; l = tolower(s); o = tolower(old)
                while ((i = index(l, o)) > 0) { out = out substr(s, 1, i - 1) new; s = substr(s, i + length(old)); l = substr(l, i + length(old)) }
                return out s
            }
            # The length of the quoted scalar t starts with (`'"'"''"'"'` is a quote
            # inside single quotes, `\` escapes inside double quotes); all of t
            # when it does not close.
            function qlen(t,   q, i, n) {
                q = substr(t, 1, 1); n = length(t); i = 2
                while (i <= n) {
                    if (q == "\"" && substr(t, i, 1) == "\\") { i += 2; continue }
                    if (substr(t, i, 1) == q) { if (q == "'"'"'" && substr(t, i + 1, 1) == "'"'"'") { i += 2; continue }; return i }
                    i++
                }
                return n
            }
            # The length of the flow value t starts with: up to the `,`, `}` or
            # `]` that ends it, or a comment, quoted text and nested brackets
            # passed over.
            function vlen(t,   i, n, c, d) {
                n = length(t); i = 1; d = 0
                while (i <= n) {
                    c = substr(t, i, 1)
                    if (c == "\"" || c == "'"'"'") { i += qlen(substr(t, i)); continue }
                    if (d == 0 && (c == "," || c == "}" || c == "]")) return i - 1
                    if (c == "#" && i > 1 && substr(t, i - 1, 1) ~ /[[:space:]]/) return i - 1
                    if (c == "{" || c == "[") d++
                    else if (c == "}" || c == "]") d--
                    i++
                }
                return n
            }
            # The SHA replaced in the parts of record line s that are the
            # record'"'"'s own: its key, read up to its own colon, and the values
            # of its `ref` and `commit` fields — on their own lines, or as
            # entries of a flow mapping written on the key line (`'"'"'repo@sha'"'"':
            # { ref: ..., commit: ... }`). A nested `uses:` list in that mapping
            # names other revisions, which this fixer does not recompute
            # (`uses: ['"'"'other/action@<sha>'"'"']` at the same SHA is another
            # repository'"'"'s edge), and a comment is text: both are kept as
            # written.
            function own_sub(s,   k, q, j, out, rest, i, n, c, d) {
                if (s !~ /^    [^[:space:]#]/) {
                    # A field line: its key and value, not a comment after them.
                    k = s; s = ""
                    while (k != "") {
                        c = substr(k, 1, 1)
                        if (c == "\"" || c == "'"'"'") { j = qlen(k); s = s substr(k, 1, j); k = substr(k, j + 1); continue }
                        if (c == "#" && s ~ /[[:space:]]$/) break
                        s = s c; k = substr(k, 2)
                    }
                    return isub(s, stale, new) k
                }
                k = substr(s, 5)
                if (k ~ /^["'"'"']/) { q = substr(k, 1, 1); j = index(substr(k, 2), q) + 1 }
                else { match(k, /:([[:space:]]|$)/); j = RSTART - 1 }
                out = isub(substr(s, 1, 4 + j), stale, new); rest = substr(s, 5 + j)
                n = length(rest); i = 1; d = 0
                while (i <= n) {
                    c = substr(rest, i, 1)
                    if (c == "\"" || c == "'"'"'") { j = qlen(substr(rest, i)); out = out substr(rest, i, j); i += j; continue }
                    if (c == "#" && (i == 1 || substr(rest, i - 1, 1) ~ /[[:space:]]/)) { out = out substr(rest, i); break }
                    if (c == "{" || c == "[") d++
                    else if (c == "}" || c == "]") d--
                    out = out c; i++
                    if (d == 1 && (c == "{" || c == ",") && match(substr(rest, i), /^[[:space:]]*["'"'"']?(ref|commit)["'"'"']?[[:space:]]*:[[:space:]]*/)) {
                        out = out substr(rest, i, RLENGTH); i += RLENGTH
                        j = vlen(substr(rest, i)); out = out isub(substr(rest, i, j), stale, new); i += j
                    }
                }
                return out
            }
            sec == "d" && /^    [^[:space:]#]/ {
                k = $0; sub(/^    /, "", k); sub(/[[:space:]]+#.*$/, "", k)
                if (k ~ /^["'"'"']/) { q = substr(k, 1, 1); k = substr(k, 2); k = substr(k, 1, index(k, q) - 1) }
                else sub(/:([[:space:]].*)?$/, "", k)
                inrec = (tolower(k) == tolower(repo "@" stale))
            }
            # The field keys may be quoted (`"ref": ...`, `'"'"'commit'"'"': ...`), as
            # a hand-written record may write them: the same fields.
            sec == "d" && inrec && (/^    [^[:space:]#]/ || /^        ["'"'"']?(ref|commit)["'"'"']?[[:space:]]*:/) {
                if (still == 0) $0 = own_sub($0)
                else copy[++nc] = own_sub($0)
            }
            sec == "d" && inrec && still != 0 && !(/^    [^[:space:]#]/ || /^        ["'"'"']?(ref|commit)["'"'"']?[[:space:]]*:/) { copy[++nc] = $0 }
            { print }
            END { for (i = 1; i <= nc; i++) print copy[i] }' "$LOCK" > "$tmp"
        [ "$still" -eq 0 ] || echo "NOTE: actions.lock keeps the record for ${stale:0:7}: a workflow the table does not cover still pins it; the replacement got its own record." >&2
        if ! cmp -s "$LOCK" "$tmp"; then pin_fixer_replace "$tmp" "$LOCK"; lock_touched=1; else rm -f "$tmp"; fi
    done < <(cut -f2,3,5 "$edits" | sort -u)
    # A kept stale edge means the new edge was ADDED, so the stale record is
    # kept too (the branch above copies it); nothing else to do here.
    [ "$lock_touched" -eq 1 ] && merge_duplicate_records "$LOCK"
    if [ "$lock_crlf" -eq 1 ]; then
        tmp=$(mktemp)
        sed 's/$/\r/' "$LOCK" > "$tmp" && pin_fixer_replace "$tmp" "$LOCK"
    fi
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
if [ "$merge_refused" -ne 0 ]; then
    echo "fix-stale-reusable-pin: actions.lock still lists a dependency twice (UNSUPPORTED above); not a clean success." >&2
    exit 2
fi
if [ "$lock_stale" -eq 1 ]; then
    echo "fix-stale-reusable-pin: a rewritten caller had no matching lock edge (WARN above); the lock is out of sync. Exit 3: not a clean success." >&2
    exit 3
fi
[ "$lock_touched" -eq 1 ] && exit 3
exit 0
