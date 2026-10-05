#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Offline fixture test for scripts/enroll-hypatia-fleet.sh repo discovery.
# The estate files clones by star list (<root>/<LIST>/[<sub>/]<repo>). A
# discovery that only looks one level deep finds almost nothing, so the
# planted control at the end re-runs at depth 1 and must miss repos.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# Create a fake clone (a directory holding a .git directory).
mk_clone() { mkdir -p "$1/.git"; }

mk_clone "$tmp/repos/top"                       # depth 1
mk_clone "$tmp/repos/CORE/listed"               # depth 2
mk_clone "$tmp/repos/LANG/iser/sub-listed"      # depth 3
mk_clone "$tmp/repos/CORE/listed/nested-sub"    # inside a clone: not a repo of its own
mk_clone "$tmp/repos/A/B/C/too-deep"            # depth 4: outside the layout
mkdir -p "$tmp/repos/DX/worktree" && echo 'gitdir: /elsewhere' >"$tmp/repos/DX/worktree/.git"

# Run discovery against the fixture and print the sorted repo names.
names() {
    bash "$root/scripts/enroll-hypatia-fleet.sh" --repos-root "$tmp/repos" --registry "$tmp/reg.json" >/dev/null
    jq -r '.repos[].name' "$tmp/reg.json" | sort | tr '\n' ' '
}

want='listed sub-listed top '
got="$(names)"
if [[ "$got" != "$want" ]]; then
    echo "FAIL discovery: want [$want] got [$got]"
    exit 1
fi
echo "ok   star-list discovery finds depth 1-3 clones only"

got1="$(ENROLL_MAX_DEPTH=1 names)"
if [[ "$got1" == "$want" ]]; then
    echo "FAIL planted control: depth-1 discovery should miss star-list clones"
    exit 1
fi
echo "ok   planted control: depth-1 discovery misses them ([$got1])"
