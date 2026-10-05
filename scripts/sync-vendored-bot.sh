#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Pinned sync of a bot whose canonical source is a standalone repository.
#
# A vendored bot directory `bots/<bot>/` carries a lock, `bots/<bot>/FLEET-SYNC.json`:
#
#   {"fleet_owned":[...],"include":[...],"repo":"<git url>","rev":"<40-hex sha>",
#    "schema":"gitbot-fleet.vendor-sync/1","upstream_branch":"main"}
#
# (keys sorted, no insignificant whitespace: the JCS / RFC 8785 form of a
# string-only object, which is what `jq -cS` emits for one), plus one final LF.
#
#   include      upstream paths (file or directory prefix) that are vendored:
#                the crate surface the fleet builds and deploys. Everything else
#                upstream (docs, packaging, per-repo metadata, wiki) stays
#                upstream -- bots/ slots are thin (bots/README.adoc).
#   fleet_owned  paths inside `bots/<bot>/` that belong to the fleet, not to the
#                upstream: the lock itself and the fleet-side canonical-source note.
#                A fleet-owned path is never taken from upstream.
#
# Invariant enforced by --check: the set of (mode, blob, path) entries tracked
# under `bots/<bot>/`, minus fleet_owned, equals the upstream tree at `rev`
# restricted to include -- byte-for-byte, because equal git blob ids mean equal content.
# `rev` must also be reachable from the upstream branch, so the pin cannot point
# at an unreviewed side branch.
#
# Usage
#   scripts/sync-vendored-bot.sh <bot> --check            # CI drift check (index vs pin)
#   scripts/sync-vendored-bot.sh <bot> --sync [--rev SHA] # rewrite bots/<bot> from the pin
#
# --sync stages the result (git index); review with `git diff --cached` and commit.
# --rev updates the pin first; without it the current pin is re-applied.
#
# Exit status
#   0  in sync (or sync applied)
#   1  drift, an invalid lock, or the upstream could not be read
#   2  usage error
#
# Environment
#   SYNC_UPSTREAM_URL  override the lock's `repo` (tests use a local fixture repo)

set -euo pipefail

ROOT="$(git -C "$(dirname "${BASH_SOURCE[0]}")" rev-parse --show-toplevel)"
SCHEMA='gitbot-fleet.vendor-sync/1'

# Print an error prefixed with the tool name and exit 1.
die() {
    printf 'vendored-bot sync: %s\n' "$1" >&2
    exit 1
}

# Print usage and exit 2.
usage() {
    sed -n '/^# Usage/,/^# Exit status/p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//' >&2
    exit 2
}

# Read one string field from the lock; fail if it is absent or not a string.
lock_field() {
    local v
    v="$(jq -er --arg k "$1" '.[$k] | strings' "$LOCK")" || die "lock field '$1' missing or not a string in $LOCK"
    printf '%s' "$v"
}

# Validate the lock: parses, has the expected schema, is in canonical
# (jq -cS) form, and pins a full 40-hex commit id. Validates shape, not
# just presence (AGENTS.md section 5).
validate_lock() {
    [[ -f "$LOCK" ]] || die "no lock at $LOCK"
    jq -e 'type == "object"' "$LOCK" >/dev/null || die "$LOCK is not a JSON object"
    local canon
    canon="$(jq -cS . "$LOCK")"
    [[ "$(cat "$LOCK")" == "$canon" ]] || die "$LOCK is not canonical; expected exactly: $canon"
    [[ "$(lock_field schema)" == "$SCHEMA" ]] || die "$LOCK schema is not $SCHEMA"
    [[ "$(lock_field rev)" =~ ^[0-9a-f]{40}$ ]] || die "$LOCK rev is not a 40-hex commit id"
    [[ "$(lock_field upstream_branch)" =~ ^[A-Za-z0-9._/-]+$ ]] || die "$LOCK upstream_branch is malformed"
    jq -e '(.include | type == "array" and length > 0 and all(type == "string" and length > 0)) and
           (.fleet_owned | type == "array" and all(type == "string")) and
           (.fleet_owned | index("FLEET-SYNC.json") != null)' "$LOCK" >/dev/null \
        || die "$LOCK include (non-empty) / fleet_owned must be string arrays and fleet_owned must list FLEET-SYNC.json"
}

# Write the lock in canonical form with a new rev.
write_lock_rev() {
    local tmp
    tmp="$(mktemp)"
    jq -cS --arg r "$1" '.rev = $r' "$LOCK" >"$tmp"
    mv "$tmp" "$LOCK"
}

# Read `git ls-tree -r` or `git ls-files -s` output on stdin and emit sorted
# "mode blob path" lines. $1 = prefix to strip (entries outside it are dropped),
# $2 = newline-separated keep list (empty = keep all), rest = drop list. A list
# entry matches a path equal to it or under it as a directory.
filter_entries() {
    local strip="$1" keep="$2"
    shift 2
    awk -v strip="$strip" -v keeps="$keep" -v drops="$(printf '%s\n' "$@")" '
        # True when path p equals list entry e or lies under it as a directory.
        function under(p, e) { return e != "" && (p == e || index(p, e "/") == 1) }
        BEGIN { nk = split(keeps, k, "\n"); nd = split(drops, d, "\n") }
        {
            tab = index($0, "\t"); meta = substr($0, 1, tab - 1); path = substr($0, tab + 1)
            split(meta, m, " ")
            # ls-tree: mode type blob ; ls-files -s: mode blob stage
            blob = (m[2] == "blob" || m[2] == "commit" || m[2] == "tree") ? m[3] : m[2]
            if (strip != "") {
                if (index(path, strip) != 1) next
                path = substr(path, length(strip) + 1)
            }
            if (keeps != "") {
                hit = 0
                for (i = 1; i <= nk; i++) if (under(path, k[i])) { hit = 1; break }
                if (!hit) next
            }
            for (i = 1; i <= nd; i++) if (under(path, d[i])) next
            print m[1] " " blob " " path
        }' | LC_ALL=C sort -k3
}

# Fetch upstream branch history (trees and commits only, no blobs) into a
# throwaway bare repo; check rev exists and is reachable from the branch.
# Prints the bare repo path.
fetch_upstream() {
    local url="$1" rev="$2" branch="$3" bare
    bare="$(mktemp -d)/up.git"
    git init -q --bare "$bare"
    git -C "$bare" fetch -q --no-tags --filter=blob:none "$url" "+refs/heads/$branch:refs/heads/$branch" \
        || die "could not fetch $url $branch"
    git -C "$bare" cat-file -e "$rev^{commit}" 2>/dev/null \
        || die "pinned rev $rev is not in $url $branch history"
    git -C "$bare" merge-base --is-ancestor "$rev" "refs/heads/$branch" \
        || die "pinned rev $rev is not reachable from $url $branch"
    printf '%s' "$bare"
}

# Compare the fleet index under bots/<bot>/ with the upstream tree at rev.
check() {
    local url rev branch bare
    url="${SYNC_UPSTREAM_URL:-$(lock_field repo)}"
    rev="$(lock_field rev)"
    branch="$(lock_field upstream_branch)"
    bare="$(fetch_upstream "$url" "$rev" "$branch")"
    local incl
    incl="$(jq -r '.include[]' "$LOCK")"
    mapfile -t owned < <(jq -r '.fleet_owned[]' "$LOCK")

    local want got
    want="$(mktemp)"; got="$(mktemp)"
    git -C "$bare" ls-tree -r --full-tree "$rev" | filter_entries "" "$incl" "${owned[@]}" >"$want"
    git -C "$ROOT" ls-files -s -- "bots/$BOT/" | filter_entries "bots/$BOT/" "" "${owned[@]}" >"$got"
    rm -rf "$(dirname "$bare")"

    for f in "${owned[@]}"; do
        git -C "$ROOT" ls-files --error-unmatch -- "bots/$BOT/$f" >/dev/null 2>&1 \
            || die "fleet-owned path bots/$BOT/$f is not tracked"
    done

    [[ -s "$want" ]] || die "upstream tree at $rev has nothing under include (refusing a vacuous pass)"
    if cmp -s "$want" "$got"; then
        printf 'vendored-bot sync: bots/%s matches %s@%s (%s entries)\n' "$BOT" "$url" "$rev" "$(wc -l <"$want")"
        rm -f "$want" "$got"
        return 0
    fi
    printf 'vendored-bot sync: bots/%s DRIFTS from %s@%s\n' "$BOT" "$url" "$rev" >&2
    # Present per-path differences: < upstream (wanted), > fleet (got).
    diff <(awk '{print $3" "$1" "$2}' "$want") <(awk '{print $3" "$1" "$2}' "$got") | grep '^[<>]' | head -100 >&2 || true
    printf 'Fix: scripts/sync-vendored-bot.sh %s --sync   (or bump the pin with --rev)\n' "$BOT" >&2
    rm -f "$want" "$got"
    return 1
}

# Rewrite bots/<bot>/ from the upstream tree at the pinned rev, keeping
# fleet-owned files, and stage the result.
sync() {
    local url rev branch bare
    url="${SYNC_UPSTREAM_URL:-$(lock_field repo)}"
    [[ -n "$NEW_REV" ]] && { [[ "$NEW_REV" =~ ^[0-9a-f]{40}$ ]] || die "--rev must be a 40-hex commit id"; write_lock_rev "$NEW_REV"; }
    validate_lock
    rev="$(lock_field rev)"
    branch="$(lock_field upstream_branch)"
    bare="$(fetch_upstream "$url" "$rev" "$branch")"
    mapfile -t owned < <(jq -r '.fleet_owned[]' "$LOCK")
    local keep
    keep="$(git -C "$bare" ls-tree -r --full-tree "$rev" | filter_entries "" "$(jq -r '.include[]' "$LOCK")" "${owned[@]}")"
    rm -rf "$(dirname "$bare")"

    git -C "$ROOT" fetch -q --no-tags "$url" "$rev" || die "could not fetch $rev from $url"

    local save
    save="$(mktemp -d)"
    for f in "${owned[@]}"; do
        if [[ -e "$ROOT/bots/$BOT/$f" ]]; then
            mkdir -p "$save/$(dirname "$f")"
            cp -p "$ROOT/bots/$BOT/$f" "$save/$f"
        fi
    done

    git -C "$ROOT" rm -r -q --cached --ignore-unmatch -- "bots/$BOT"
    rm -rf "${ROOT:?}/bots/$BOT"
    # Stage exactly the kept upstream entries (same mode, same blob id).
    printf '%s\n' "$keep" | awk -v pre="bots/$BOT/" 'NF { p = $0; sub(/^[^ ]+ [^ ]+ /, "", p); print $1 "," $2 "," pre p }' \
        | while IFS=, read -r mode blob path; do
            git -C "$ROOT" update-index --add --cacheinfo "$mode,$blob,$path"
        done
    git -C "$ROOT" checkout -- "bots/$BOT"
    cp -pr "$save/." "$ROOT/bots/$BOT/"
    rm -rf "$save"
    for f in "${owned[@]}"; do
        [[ -e "$ROOT/bots/$BOT/$f" ]] && git -C "$ROOT" add -- "bots/$BOT/$f"
    done
    printf 'vendored-bot sync: staged bots/%s from %s@%s\n' "$BOT" "$url" "$rev"
}

BOT="${1:-}"
MODE="${2:-}"
NEW_REV=""
[[ -n "$BOT" && -n "$MODE" ]] || usage
[[ "$BOT" =~ ^[a-z0-9-]+$ ]] || die "bot name '$BOT' is malformed"
shift 2
while [[ $# -gt 0 ]]; do
    case "$1" in
        --rev) NEW_REV="${2:-}"; shift 2 ;;
        *) usage ;;
    esac
done
LOCK="$ROOT/bots/$BOT/FLEET-SYNC.json"

case "$MODE" in
    --check) validate_lock; check ;;
    --sync) [[ -f "$LOCK" ]] || die "no lock at $LOCK"; sync ;;
    *) usage ;;
esac
