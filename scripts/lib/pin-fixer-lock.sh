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
# checkout, named by the checkout's real path, so every fixer run on the same
# checkout — by any path that reaches it — contends for the same file. It is
# held until the fixer exits. A holder that does not go away within
# PIN_FIXER_LOCK_WAIT seconds (default 600) is reported and the fixer exits 2
# having changed nothing: a dispatcher must not record that run as a clean
# repair.
#
# The directory is PIN_FIXER_LOCK_DIR when set, else a per-user
# `pin-fixer-<uid>` directory (created mode 0700) under XDG_RUNTIME_DIR, else
# TMPDIR, else /tmp. Opening `>file` follows a symlink, so the file must sit
# where no other user can plant one: the directory is refused — exit 2,
# nothing changed — unless it is a real directory (not a symlink) owned by
# this user and writable by neither group nor others. A predictable name in a
# shared, world-writable /tmp is exactly what this rules out.
pin_fixer_lock() {
    local repo="$1" dir key wait
    if [ -n "${PIN_FIXER_LOCK_DIR:-}" ]; then
        dir="$PIN_FIXER_LOCK_DIR"
    else
        dir="${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}/pin-fixer-$(id -u)"
        mkdir -m 700 "$dir" 2>/dev/null || true
    fi
    if [ -L "$dir" ] || [ ! -d "$dir" ] || [ ! -O "$dir" ] \
       || [ -n "$(find "$dir" -maxdepth 0 \( -perm -0020 -o -perm -0002 \) -print 2>/dev/null)" ]; then
        echo "fix-workflow-pins: lock directory $dir is not a private directory of this user (a symlink, missing, owned by another user, or group/world-writable); nothing was changed" >&2
        exit 2
    fi
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

# Replace <dest> with the rewritten <tmp> the way `sed -i` does: by a rename
# (never a half-written file), keeping <dest>'s mode, and its owner and group
# where this user may set them. A `mktemp` file is created 0600; renamed as it
# is over a 0644 workflow or lock, it would leave the repaired file unreadable
# to everyone else who shares the checkout.
pin_fixer_replace() { # <tmp> <dest>
    chmod --reference="$2" "$1" 2>/dev/null || chmod "$(stat -c %a "$2")" "$1"
    chown --reference="$2" "$1" 2>/dev/null || true
    mv -f "$1" "$2"
}

# Would YAML read <value>, written as a plain (unquoted) scalar, as anything
# but a string? A number (`1`, `1.80` the float 1.8, `1e3`, `0x1f`, `0o17`,
# `0b1`) or a null or boolean spelling as YAML 1.1 and the core schema read
# them (`null`, `true`, `no`, `on`, `y`, ... in any case). A fixer writing
# such a value into a workflow or lock quotes it.
pin_fixer_yaml_nonstring() { # <value>
    [[ "$1" =~ ^([0-9][0-9_]*(\.[0-9_]*)?([eE][-+]?[0-9]+)?|0[xX][0-9a-fA-F_]+|0[oO][0-7_]+|0[bB][01_]+)$ ]] \
        || [[ "${1,,}" =~ ^(null|true|false|yes|no|on|off|y|n)$ ]]
}
