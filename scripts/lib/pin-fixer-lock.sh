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
# Sourced, then `pin_fixer_lock <repo-path>`. The lock — a sentinel directory,
# `pin-fixer-<key>.lock.d`, that every fixer holds, and with flock installed a
# `pin-fixer-<key>.lock` file it takes first — lives outside the checkout,
# keyed by the checkout's real path, so every fixer run on the same checkout
# — by any path that reaches it, with or without flock — contends for the
# same sentinel. It is held until the fixer exits. A holder that does not go away within
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
    # Every fixer holds the directory sentinel; one that has flock takes the
    # flock first, as well. A run with flock and one without (a host and a
    # minimal container sharing the checkout) therefore contend for the same
    # object, the directory, and still never hold the checkout together.
    if command -v flock >/dev/null 2>&1; then
        exec 9>"$dir/pin-fixer-$key.lock"
        flock -w "$wait" 9 || {
            echo "fix-workflow-pins: another workflow-pin fixer holds $repo (waited ${wait}s); nothing was changed" >&2
            exit 2
        }
    fi
    pin_fixer_lock_mkdir "$repo" "$dir/pin-fixer-$key.lock.d" "$wait"
}

# The lock every fixer holds is a directory: `mkdir` either creates it or
# fails because another fixer has, atomically, so two runs never hold it
# together, whether or not either has flock. The holder's EXIT trap removes
# it (an interrupt or termination exits through that trap too). A run killed
# outright leaves it behind; the next fixer then waits, reports the directory
# and its recorded holder, and exits 2 with nothing changed — it is never
# taken over silently (a recorded pid may belong to another pid namespace, so
# a holder cannot be judged gone), and never skipped.
pin_fixer_lock_mkdir() { # <repo> <lock-dir> <wait>
    local repo="$1" ldir="$2" wait="$3" waited=0
    until mkdir "$ldir" 2>/dev/null; do
        if [ "$waited" -ge "$wait" ]; then
            echo "fix-workflow-pins: another workflow-pin fixer holds $repo (waited ${wait}s; the lock is the directory $ldir, held by pid $(cat "$ldir/pid" 2>/dev/null || echo unknown) — remove it by hand only if that run is gone); nothing was changed" >&2
            exit 2
        fi
        sleep 1
        waited=$((waited + 1))
    done
    PIN_FIXER_LOCK_HELD="$ldir"
    echo "$$" > "$ldir/pid"
    trap 'rm -f "$PIN_FIXER_LOCK_HELD/pid"; rmdir "$PIN_FIXER_LOCK_HELD" 2>/dev/null' EXIT
    trap 'exit 129' HUP
    trap 'exit 130' INT
    trap 'exit 143' TERM
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
# but a string? As YAML 1.1 and the core schema read them: a number (`1`,
# `1.80` the float 1.8, `1e3`, `-1`, `.5`, `0x1f`, `0o17`, `0b1`, `1:30` base
# 60, `.inf`, `.nan`), a timestamp (`2026-10-20`, the date — a tag may be
# spelled so — with or without a time), or a null or boolean spelling
# (`null`, `~`, nothing, `true`, `no`, `on`, `y`, ... in any case). A fixer
# writing such a value into a workflow or lock quotes it.
pin_fixer_yaml_nonstring() { # <value>
    [[ -z "$1" || "$1" == "~" ]] \
        || [[ "$1" =~ ^[-+]?([0-9][0-9_]*(\.[0-9_]*)?([eE][-+]?[0-9]+)?|\.[0-9][0-9_]*([eE][-+]?[0-9]+)?|[0-9][0-9_]*(:[0-5]?[0-9])+(\.[0-9_]*)?|0[xX][0-9a-fA-F_]+|0[oO][0-7_]+|0[bB][01_]+|\.(inf|Inf|INF))$ ]] \
        || [[ "$1" =~ ^\.(nan|NaN|NAN)$ ]] \
        || [[ "$1" =~ ^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}(([Tt]|[[:space:]]+)[0-9]{1,2}:[0-9]{2}:[0-9]{2}(\.[0-9]*)?([[:space:]]*(Z|[-+][0-9]{1,2}(:[0-9]{2})?))?)?$ ]] \
        || [[ "${1,,}" =~ ^(null|true|false|yes|no|on|off|y|n)$ ]]
}
