#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Has the canon moved since the copies were pinned?
#
# Rhodibot applies the RSR rule set from two artefacts, both copied verbatim and
# both pinned in `bots/rhodibot/canon/pin.toml`:
#
#   criteria  `0-canon/rsr/rsr-criteria-v2.a2ml`            -- the rules
#   gates     `.machine_readable/template-capability-gates.toml` -- which
#             capabilities exist, what a preset expands to, which module paths
#             belong to which capability
#
# The tests check the copies against the pin; this script asks the other
# question -- whether the canon itself has moved since. A canon revision changes
# what every repository in the estate is measured against, so it should arrive
# as a reviewable commit, not as a surprise on the next run. The gate table
# matters for the same reason: if a capability is added upstream and this copy
# does not have it, profiles declaring it are refused, and criteria gated on it
# can never apply.
#
# Exit status
#   0  both pins match the canon, and the canon's own lock agrees with both files
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
# Section-aware: both artefacts have a `path` and a `sha256`, so a flat read
# would hand back whichever came first.
read_pin() {
    awk -F'"' -v section="$1" -v key="$2" '
        /^[[:space:]]*\[/ {
            current = $0
            sub(/^[[:space:]]*\[/, "", current)
            sub(/\].*$/, "", current)
            next
        }
        current == section && $1 ~ "^[[:space:]]*" key "[[:space:]]*=" { print $2; exit }
    ' "$PIN"
}

# The hash the canon's own lock records for a slot.
lock_hash() {
    grep -A2 "$1 = {" "$2" | grep -oE '[0-9a-f]{64}' | head -1
}

[ -f "$PIN" ] || fail "no pin file at $PIN"

repo="$(read_pin source repo)"
version="$(read_pin source canon_version)"
criteria_path="$(read_pin source path)"
criteria_pin="$(read_pin source sha256)"
gates_path="$(read_pin gates path)"
gates_pin="$(read_pin gates sha256)"
gates_slot="$(read_pin gates slot)"
gates_version="$(read_pin gates version)"

[ -n "$criteria_path" ] || fail "the pin names no criteria path"
[ -n "$criteria_pin" ] || fail "the pin names no criteria sha256"
[ -n "$gates_path" ] || fail "the pin names no gate table path"
[ -n "$gates_pin" ] || fail "the pin names no gate table sha256"
[ -n "$gates_slot" ] || fail "the pin names no slot for the gate table"

printf 'canon pin check\n'
printf '  pin      %s@%s\n' "$repo" "$REF"

# --- criteria ---------------------------------------------------------------

printf '  rules    %s\n' "$criteria_path"
printf '  pinned   %s  (canon %s)\n' "$criteria_pin" "$version"

live_criteria="$TMPDIR_CHECK/criteria.a2ml"
curl -sSfL --max-time 30 "$REMOTE/$REF/$criteria_path" -o "$live_criteria" \
    || fail "could not fetch $REMOTE/$REF/$criteria_path (is the ref right, and the repo readable?)"
live_criteria_hash="$(sha256sum "$live_criteria" | cut -d' ' -f1)"
printf '  upstream %s\n' "$live_criteria_hash"

if [ "$live_criteria_hash" = "$criteria_pin" ]; then
    printf '  -> unchanged since it was pinned\n'
else
    cat >&2 <<EOF

  The rule set has moved since this copy was pinned.

  To re-pin:
    1. copy $criteria_path over bots/rhodibot/canon/$(basename "$criteria_path")
    2. update sha256, canon_version, criteria_version, released and the counts
       in $PIN
    3. run \`cargo test --locked -p rhodibot canon\` and fix anything the
       parser now refuses -- a refusal means the parser is older than the canon
    4. read the diff before committing it: a canon change alters what every
       repository in the estate is measured against
EOF
    exit 1
fi

# --- gate table -------------------------------------------------------------

printf '  gates    %s\n' "$gates_path"
printf '  pinned   %s  (table %s, slot %s)\n' "$gates_pin" "$gates_version" "$gates_slot"

live_gates="$TMPDIR_CHECK/gates.toml"
curl -sSfL --max-time 30 "$REMOTE/$REF/$gates_path" -o "$live_gates" \
    || fail "could not fetch $REMOTE/$REF/$gates_path (is the ref right, and the repo readable?)"
live_gates_hash="$(sha256sum "$live_gates" | cut -d' ' -f1)"
printf '  upstream %s\n' "$live_gates_hash"

if [ "$live_gates_hash" = "$gates_pin" ]; then
    printf '  -> unchanged since it was pinned\n'
else
    cat >&2 <<EOF

  The gate table has moved since this copy was pinned.

  To re-pin:
    1. copy $gates_path over bots/rhodibot/canon/$(basename "$gates_path")
    2. update [gates] sha256, version, and the \`known\` and \`presets\` counts
       in $PIN
    3. run \`cargo test --locked -p rhodibot canon\` -- a new capability, or a
       criterion gated on one, is exactly what the shape check is for
    4. read the diff before committing it: adding a capability changes which
       criteria apply to which repositories
EOF
    exit 1
fi

# --- the canon's own lock ---------------------------------------------------

# canon.lock records a hash for each slot. If it disagrees with the file at the
# same ref, one of the two is stale, and which one is not this script's to
# guess -- but it is worth stopping for.
lock_file="$TMPDIR_CHECK/canon.lock"
if curl -sSfL --max-time 30 "$REMOTE/$REF/canon.lock" -o "$lock_file"; then
    stale=0
    for pair in "criteria:$criteria_path:$live_criteria_hash" "gates:$gates_path:$live_gates_hash"; do
        slot="${pair%%:*}"
        rest="${pair#*:}"
        path="${rest%%:*}"
        live="${rest##*:}"
        recorded="$(lock_hash "$slot" "$lock_file")"
        if [ -z "$recorded" ]; then
            printf '  canon.lock: no %s pin found (format changed?)\n' "$slot"
        elif [ "$recorded" != "$live" ]; then
            printf 'canon drift: canon.lock pins %s for %s, but the file at %s hashes %s\n' \
                "$recorded" "$path" "$REF" "$live" >&2
            stale=1
        else
            printf '  canon.lock agrees (%s)\n' "$slot"
        fi
    done
    [ "$stale" -eq 0 ] || exit 1
else
    printf '  canon.lock: not readable at this ref, skipping that comparison\n'
fi

printf '  -> ok\n'
