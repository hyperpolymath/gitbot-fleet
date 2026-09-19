#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Has the canon moved since the copy was pinned?
#
# Rhodibot applies the RSR rule set from a copy of
# `0-canon/rsr/rsr-criteria-v2.a2ml`, pinned in `bots/rhodibot/canon/pin.toml`.
# The tests check that copy against the pin; this script asks the other
# question -- whether the canon itself has moved since. A canon revision changes
# what every repository is measured against, so it should arrive as a
# reviewable commit, not as a surprise on the next run.
#
# Exit status
#   0  the pin matches the canon, and the canon's own lock agrees with the file
#   1  drift, or the canon could not be read
#
# Environment
#   CANON_PIN     path to the pin file (default: the vendored pin)
#   CANON_REF     standards ref to compare against (default: main)
#   CANON_REMOTE  base URL for raw files (default: raw.githubusercontent)

set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PIN="${CANON_PIN:-$ROOT/bots/rhodibot/canon/pin.toml}"
REF="${CANON_REF:-main}"
REMOTE="${CANON_REMOTE:-https://raw.githubusercontent.com/hyperpolymath/standards}"
TMPDIR_CHECK="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_CHECK"' EXIT

fail() {
    printf 'canon drift: %s\n' "$1" >&2
    exit 1
}

# Read a quoted value from the pin without assuming a TOML parser is installed.
read_pin() {
    awk -F'"' -v key="$1" '$1 ~ "^" key " *=" { print $2; exit }' "$PIN"
}

[ -f "$PIN" ] || fail "no pin file at $PIN"

path="$(read_pin path)"
pinned="$(read_pin sha256)"
repo="$(read_pin repo)"
version="$(read_pin canon_version)"

[ -n "$path" ] || fail "the pin names no path"
[ -n "$pinned" ] || fail "the pin names no sha256"

printf 'canon pin check\n'
printf '  pin      %s@%s\n' "$repo" "$REF"
printf '  file     %s\n' "$path"
printf '  pinned   %s  (canon %s)\n' "$pinned" "$version"

live_file="$TMPDIR_CHECK/criteria.a2ml"
curl -sSfL --max-time 30 "$REMOTE/$REF/$path" -o "$live_file" \
    || fail "could not fetch $REMOTE/$REF/$path (is the ref right, and the repo readable?)"
live="$(sha256sum "$live_file" | cut -d' ' -f1)"
printf '  upstream %s\n' "$live"

if [ "$live" = "$pinned" ]; then
    printf '  -> unchanged since it was pinned\n'
else
    cat >&2 <<EOF

  The canon has moved since this copy was pinned.

  To re-pin:
    1. copy $path over bots/rhodibot/canon/$(basename "$path")
    2. update sha256, canon_version, criteria_version, released and the counts
       in $PIN
    3. run \`cargo test --locked -p rhodibot canon\` and fix anything the
       parser now refuses -- a refusal means the parser is older than the canon
    4. read the diff before committing it: a canon change alters what every
       repository in the estate is measured against
EOF
    exit 1
fi

# The canon's own lock names a hash for this file. If it disagrees with the file
# at the same ref, one of the two is stale, and which one is not this script's
# to guess -- but it is worth stopping for.
lock_file="$TMPDIR_CHECK/canon.lock"
if curl -sSfL --max-time 30 "$REMOTE/$REF/canon.lock" -o "$lock_file"; then
    lock_hash="$(grep -A2 'criteria = {' "$lock_file" | grep -oE '[0-9a-f]{64}' | head -1)"
    if [ -z "$lock_hash" ]; then
        printf '  canon.lock: no criteria pin found (format changed?)\n'
    elif [ "$lock_hash" != "$live" ]; then
        fail "canon.lock pins $lock_hash for $path, but the file at $REF hashes $live"
    else
        printf '  canon.lock agrees\n'
    fi
else
    printf '  canon.lock: not readable at this ref, skipping that comparison\n'
fi

printf '  -> ok\n'
