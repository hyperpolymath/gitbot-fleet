#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Offline tests for scripts/sync-vendored-bot.sh against a fixture upstream.
# Every failure mode is a planted positive control: a check that cannot fail
# proves nothing (AGENTS.md section 5, item 4).

set -euo pipefail

SRC="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)/scripts/sync-vendored-bot.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
export GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@example.invalid GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=t@example.invalid
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
pass=0
fail=0

# Record a passing assertion.
ok() { pass=$((pass + 1)); printf 'ok   %s\n' "$1"; }

# Record a failing assertion.
bad() { fail=$((fail + 1)); printf 'FAIL %s\n' "$1"; }

# Assert that the given command exits 0.
expect_pass() {
    local name="$1"; shift
    if "$@" >"$WORK/out" 2>&1; then ok "$name"; else bad "$name"; sed 's/^/     /' "$WORK/out"; fi
}

# Assert that the given command exits non-zero and its output matches a pattern.
expect_fail() {
    local name="$1" pattern="$2"; shift 2
    if "$@" >"$WORK/out" 2>&1; then
        bad "$name (exited 0)"
    elif grep -q -- "$pattern" "$WORK/out"; then
        ok "$name"
    else
        bad "$name (wrong failure)"; sed 's/^/     /' "$WORK/out"
    fi
}

# Write a canonical lock for the fixture bot pinned at the given rev.
write_lock() {
    jq -ncS --arg r "$1" --arg u "file://$WORK/up" \
        '{schema:"gitbot-fleet.vendor-sync/1",repo:$u,upstream_branch:"main",rev:$r,
          include:["Cargo.toml","src"],fleet_owned:["FLEET-SYNC.json","NOTE.adoc"]}' \
        >"$WORK/fleet/bots/demo/FLEET-SYNC.json"
}

# --- fixture upstream: two commits on main, one on a side branch ---
git init -q -b main "$WORK/up"
mkdir -p "$WORK/up/src" "$WORK/up/docs"
printf '[package]\nname = "demo"\n' >"$WORK/up/Cargo.toml"
printf 'fn main() {}\n' >"$WORK/up/src/main.rs"
printf '#!/bin/sh\n' >"$WORK/up/src/run.sh"; chmod +x "$WORK/up/src/run.sh"
printf 'docs stay upstream\n' >"$WORK/up/docs/guide.adoc"
git -C "$WORK/up" add -A && git -C "$WORK/up" commit -q -m one
REV1="$(git -C "$WORK/up" rev-parse HEAD)"
printf 'fn main() { println!("2"); }\n' >"$WORK/up/src/main.rs"
git -C "$WORK/up" commit -q -am two
REV2="$(git -C "$WORK/up" rev-parse HEAD)"
git -C "$WORK/up" checkout -q -b side
printf 'side\n' >"$WORK/up/src/side.rs"
git -C "$WORK/up" add -A && git -C "$WORK/up" commit -q -m side
REV_SIDE="$(git -C "$WORK/up" rev-parse HEAD)"
git -C "$WORK/up" checkout -q main

# --- fixture fleet ---
git init -q -b main "$WORK/fleet"
mkdir -p "$WORK/fleet/scripts" "$WORK/fleet/bots/demo"
cp "$SRC" "$WORK/fleet/scripts/"
printf 'fleet note\n' >"$WORK/fleet/bots/demo/NOTE.adoc"
printf 'stale fork file\n' >"$WORK/fleet/bots/demo/stale.rs"
write_lock "$REV1"
git -C "$WORK/fleet" add -A && git -C "$WORK/fleet" commit -q -m base
T="$WORK/fleet/scripts/sync-vendored-bot.sh"

expect_fail "unsynced copy drifts" "DRIFTS" "$T" demo --check
expect_pass "sync applies the pin" "$T" demo --sync
expect_pass "synced copy matches" "$T" demo --check
[[ -f "$WORK/fleet/bots/demo/NOTE.adoc" ]] && ok "fleet-owned file kept" || bad "fleet-owned file kept"
[[ ! -e "$WORK/fleet/bots/demo/stale.rs" ]] && ok "stale fork file removed" || bad "stale fork file removed"
[[ ! -e "$WORK/fleet/bots/demo/docs" ]] && ok "non-included upstream path not vendored" || bad "non-included upstream path not vendored"
[[ -x "$WORK/fleet/bots/demo/src/run.sh" ]] && ok "executable mode preserved" || bad "executable mode preserved"
git -C "$WORK/fleet" commit -q -m sync

printf 'local edit\n' >>"$WORK/fleet/bots/demo/src/main.rs"; git -C "$WORK/fleet" add -A
expect_fail "edited vendored file drifts" "src/main.rs" "$T" demo --check
git -C "$WORK/fleet" checkout -q HEAD -- bots/demo

printf 'extra\n' >"$WORK/fleet/bots/demo/src/extra.rs"; git -C "$WORK/fleet" add -A
expect_fail "extra file drifts" "src/extra.rs" "$T" demo --check
git -C "$WORK/fleet" rm -q --cached bots/demo/src/extra.rs; rm "$WORK/fleet/bots/demo/src/extra.rs"

chmod -x "$WORK/fleet/bots/demo/src/run.sh"; git -C "$WORK/fleet" add -A
expect_fail "mode change drifts" "src/run.sh" "$T" demo --check
chmod +x "$WORK/fleet/bots/demo/src/run.sh"; git -C "$WORK/fleet" add -A

cp "$WORK/fleet/bots/demo/FLEET-SYNC.json" "$WORK/lock.bak"
jq . "$WORK/lock.bak" >"$WORK/fleet/bots/demo/FLEET-SYNC.json"
expect_fail "pretty-printed lock refused" "not canonical" "$T" demo --check
jq -cS '.rev = "abc123"' "$WORK/lock.bak" >"$WORK/fleet/bots/demo/FLEET-SYNC.json"
expect_fail "short rev refused" "40-hex" "$T" demo --check
jq -cS '.rev = "{\"messa"' "$WORK/lock.bak" >"$WORK/fleet/bots/demo/FLEET-SYNC.json"
expect_fail "error-body rev refused" "40-hex" "$T" demo --check
jq -cS --arg r "$REV_SIDE" '.rev = $r' "$WORK/lock.bak" >"$WORK/fleet/bots/demo/FLEET-SYNC.json"
expect_fail "side-branch rev refused" "not in\|not reachable" "$T" demo --check
jq -cS '.include = []' "$WORK/lock.bak" >"$WORK/fleet/bots/demo/FLEET-SYNC.json"
expect_fail "empty include refused" "include" "$T" demo --check
cp "$WORK/lock.bak" "$WORK/fleet/bots/demo/FLEET-SYNC.json"
expect_pass "restored lock matches" "$T" demo --check

expect_pass "pin bump syncs" "$T" demo --sync --rev "$REV2"
expect_pass "bumped copy matches" "$T" demo --check
grep -q '"2"' "$WORK/fleet/bots/demo/src/main.rs" && ok "bump brought new content" || bad "bump brought new content"
[[ "$(jq -r .rev "$WORK/fleet/bots/demo/FLEET-SYNC.json")" == "$REV2" ]] && ok "lock rev updated" || bad "lock rev updated"
expect_fail "bad --rev refused" "40-hex" "$T" demo --sync --rev nothex

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[[ "$fail" -eq 0 ]]
