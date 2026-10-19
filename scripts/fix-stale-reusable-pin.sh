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
# If the repository carries .github/workflows/actions.lock and it names the
# stale SHA (a caller onboarded with its reusable's dependency record), the
# SHA is replaced there too — in the workflows: list and in the record's
# key/ref/commit — so the lock stays internally consistent. The record's
# nested `uses:` list is NOT recomputed: run `gh actions-lock` (or the
# repository's check-lock-sync.sh) afterwards; the script says so when it
# touches a lock.
#
# Idempotent. Rewrites nothing it does not recognise. Exit 0 whether or not
# anything changed; exit 2 when the table is unreadable.
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
# and drop them. gh-actions-lock's 4-space layout is preserved.
merge_duplicate_records() {
    local lock="$1" tmp
    tmp=$(mktemp)
    awk '
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
                cur = order[k]; delete done_head
                emit_record()
            }
        }
    ' "$lock" > "$tmp"
    if ! cmp -s "$lock" "$tmp"; then mv "$tmp" "$lock"; else rm -f "$tmp"; fi
}

# Every `uses:` of an estate reusable at a full SHA, across the workflows.
while IFS= read -r -d '' wf; do
    while IFS= read -r line; do
        path="${line%@*}"; stale="${line##*@}"
        [[ "$stale" =~ ^[0-9a-f]{40}$ ]] || continue
        replacement=""; reason=""
        while IFS=$'\t' read -r t_path t_stale t_new t_reason; do
            [[ -z "$t_path" || "$t_path" == \#* ]] && continue
            [[ "$t_stale" == "$stale" ]] || continue
            if [[ "$t_path" == "$path" ]] || { [[ "$t_path" == */\* ]] && [[ "$path" == "${t_path%\*}"* ]]; }; then
                replacement="$t_new"; reason="$t_reason"; break
            fi
        done < "$PINS_TABLE"
        [ -n "$replacement" ] || continue
        [[ "$replacement" =~ ^[0-9a-f]{40}$ ]] || { echo "fix-stale-reusable-pin: bad replacement SHA in table for $stale" >&2; exit 2; }
        sed -i "s|\(uses:[[:space:]]*[\"']\?$path\)@$stale|\1@$replacement|" "$wf"
        echo "FIXED  ${wf#"$REPO_PATH"/}  $path  ${stale:0:7} -> ${replacement:0:7}  ($reason)"
        rewrites=$((rewrites + 1))
        if [ -f "$LOCK" ] && grep -q "$stale" "$LOCK"; then
            sed -i "s/$stale/$replacement/g" "$LOCK"
            merge_duplicate_records "$LOCK"
            lock_touched=1
        fi
    done < <(grep -oE "uses:[[:space:]]*[\"']?hyperpolymath/[A-Za-z0-9_.-]+/\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml@[0-9a-f]{40}" "$wf" | sed -E "s/^uses:[[:space:]]*[\"']?//" | sort -u)
done < <(find "$REPO_PATH/.github/workflows" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) -print0)

if [ "$lock_touched" -eq 1 ]; then
    echo "WARN: actions.lock named a stale SHA and was rewritten to the replacement; its nested uses: list was NOT recomputed — run \`gh actions-lock\` or scripts/check-lock-sync.sh before pushing." >&2
fi
if [ "$rewrites" -eq 0 ]; then echo "OK (no known-dead reusable pins): $REPO_PATH"; else echo "Rewrote $rewrites pin(s) in $REPO_PATH"; fi
