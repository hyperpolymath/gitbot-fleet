#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# PA rule IDs in this repo are panic-attack's, not a second numbering.
#
#   1. bots/panicbot/canon/canonical-rule-ids.json matches the sha256 in
#      bots/panicbot/canon/pin.toml (a copy edited in place fails).
#   2. Every PA rule ID a fix script cites is written `PAnnn (Category)` and
#      agrees with that table. Hypatia pattern IDs (`PA009-shell-unquoted-var`)
#      are a separate namespace and must be back-quoted, so the two cannot be
#      mistaken for each other.
#
# panicbot's translator is held to the same table by
# bots/panicbot/tests/canonical_rule_ids_test.rs.
#
# --upstream additionally compares the pinned copy with panic-attack main
# (network; not run in CI, where a moved upstream should be a reviewed re-pin).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CANON="$ROOT/bots/panicbot/canon"
TABLE="$CANON/canonical-rule-ids.json"
SCRIPTS="${FIX_SCRIPTS_DIR:-$ROOT/scripts}"

fail=0
err() { echo "FAIL: $*" >&2; fail=1; }

pinned=$(sed -n 's/^sha256 = "\([0-9a-f]\{64\}\)"$/\1/p' "$CANON/pin.toml")
actual=$(sha256sum "$TABLE" | cut -d' ' -f1)
[[ "$pinned" == "$actual" ]] || err "canonical-rule-ids.json hashes $actual, pin.toml says $pinned"

declare -A category_of
while IFS=$'\t' read -r id cat; do
    category_of[$id]=$cat
done < <(jq -r '.rules[] | [.rule_id, .category] | @tsv' "$TABLE")
[[ ${#category_of[@]} -gt 0 ]] || err "no rules read from $TABLE"

checked=0
# Only lines that mention a PA code: one grep over all scripts, not a
# subshell per line.
while IFS=: read -r f lineno line; do
    # Back-quoted spans are Hypatia pattern IDs, not panic-attack rule IDs.
    # shellcheck disable=SC2016 # the backticks are literal, not command substitution
    bare=$(printf '%s' "$line" | sed 's/`[^`]*`//g')
    while read -r ref; do
        [[ -n "$ref" ]] || continue
        checked=$((checked + 1))
        id=${ref%% *}
        cat=$(printf '%s' "$ref" | sed -n 's/^PA[0-9]\{3\}b\{0,1\} (\([A-Za-z]*\))$/\1/p')
        if [[ -z "$cat" ]]; then
            err "$(basename "$f"):$lineno: $id is not written as 'PAnnn (Category)' (back-quote Hypatia pattern IDs)"
        elif [[ -z "${category_of[$id]:-}" ]]; then
            err "$(basename "$f"):$lineno: $id is not a panic-attack rule ID"
        elif [[ "${category_of[$id]}" != "$cat" ]]; then
            err "$(basename "$f"):$lineno: $id is ${category_of[$id]} in panic-attack, not $cat"
        fi
    done < <(printf '%s\n' "$bare" | grep -oE '\bPA[0-9]{3}b?\b( \([A-Za-z]*\))?' || true)
done < <(grep -nE '\bPA[0-9]{3}b?\b' "$SCRIPTS"/fix-*.sh || true)
echo "checked $checked PA reference(s) in $(basename "$SCRIPTS")/fix-*.sh"

if [[ "${1:-}" == "--upstream" ]]; then
    repo=$(sed -n 's/^repo = "\(.*\)"$/\1/p' "$CANON/pin.toml")
    path=$(sed -n 's/^path = "\(.*\)"$/\1/p' "$CANON/pin.toml")
    upstream=$(curl --proto '=https' --proto-redir '=https' -sSfL --max-time 30 "https://raw.githubusercontent.com/$repo/main/$path" | sha256sum | cut -d' ' -f1)
    [[ "$upstream" == "$pinned" ]] || err "panic-attack main's table hashes $upstream; re-pin (see pin.toml)"
fi

[[ "$fail" -eq 0 ]] && echo "PASS: PA rule IDs agree with panic-attack's table"
exit "$fail"
