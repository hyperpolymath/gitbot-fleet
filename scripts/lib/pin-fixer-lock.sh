#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# pin-fixer-lock.sh — one workflow-pin fixer per repository at a time.
#
# The three fixers registered for WF026 (fix-rust-toolchain-input.sh), WF027 /
# PIN-003 (fix-stale-reusable-pin.sh) and PI006 (fix-lock-self-sha-refs.sh)
# all rewrite .github/workflows/*.yml and actions.lock of the same checkout.
# Under `dispatch-runner.sh --parallel N` two manifest entries for one
# repository can start together, so each fixer takes this lock on the
# repository before it reads anything: their edits never interleave (one's
# `sed -i` landing between another's read and write).
#
# Sourced, then `pin_fixer_lock <repo-path>`. The lock file lives outside the
# checkout (PIN_FIXER_LOCK_DIR, else XDG_RUNTIME_DIR, else TMPDIR), named by
# the checkout's real path, so every fixer run on the same checkout — by any
# path that reaches it — contends for the same file. It is held until the
# fixer exits. A holder that does not go away within PIN_FIXER_LOCK_WAIT
# seconds (default 600) is reported and the fixer exits 2 having changed
# nothing: a dispatcher must not record that run as a clean repair.
pin_fixer_lock() {
    local repo="$1" dir key wait
    dir="${PIN_FIXER_LOCK_DIR:-${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}}"
    wait="${PIN_FIXER_LOCK_WAIT:-600}"
    key=$(cd "$repo" 2>/dev/null && pwd -P || printf '%s' "$repo")
    key=$(printf '%s' "$key" | sha256sum | cut -c1-16)
    if ! command -v flock >/dev/null 2>&1; then
        echo "WARN: flock not found; workflow-pin fixers on $repo are not serialised" >&2
        return 0
    fi
    exec 9>"$dir/pin-fixer-$key.lock"
    flock -w "$wait" 9 || {
        echo "fix-workflow-pins: another workflow-pin fixer holds $repo (waited ${wait}s); nothing was changed" >&2
        exit 2
    }
}
