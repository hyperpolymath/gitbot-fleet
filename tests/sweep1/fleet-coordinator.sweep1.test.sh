#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Sweep 1 (UT, SMK, BLD, P2P) for fleet-coordinator.sh, on the proven-tests kit.
# PTB_LIB must point at kit/lib of proven-tests-and-benches at the SHA in .proven/KIT_PIN.
# Exit 0 pass, 1 fail, 2 nothing ran (no kit, no assertions): absence is not a pass.
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
lib="${PTB_LIB:-}"
[ -n "$lib" ] && [ -f "$lib/ptb.sh" ] && [ -x "$lib/smoke.sh" ] || {
  echo "NO CHECK PERFORMED: set PTB_LIB to kit/lib (see .proven/KIT_PIN)" >&2; exit 2; }
. "$lib/ptb.sh"
fc="$root/fleet-coordinator.sh"

# UT: each public entry, each error branch
ptb_ok    UT-help-exits-0            bash "$fc" --help
ptb_eq    UT-help-banner "Gitbot Fleet Coordinator" "$(bash "$fc" --help | sed -n 1p | cut -d' ' -f1-3)"
ptb_fails UT-unknown-command-fails   bash "$fc" --no-such-command
ptb_eq    UT-unknown-command-names-it 1 "$(bash "$fc" --no-such-command 2>&1 | grep -c 'Unknown command: --no-such-command')"
for c in run-scan scan-supervised process-findings status deploy-bots; do
  ptb_eq  "UT-help-lists-$c" 1 "$(bash "$fc" --help | grep -c "^    $c ")"
done

# SMK: the entry point starts, answers, exits cleanly
ptb_ok    SMK-help       "$lib/smoke.sh" --expect '^Gitbot Fleet Coordinator v[0-9]' -- bash "$fc" --help
ptb_ok    SMK-status     "$lib/smoke.sh" --expect 'Fleet Status' -- bash "$fc" status

# BLD: a shell entry point builds as syntax-clean and executable-by-bash; negative control included
ptb_ok    BLD-syntax-fleet-coordinator   bash -n "$fc"
ptb_ok    BLD-syntax-all-scripts         bash -c 'for f in "$0"/scripts/*.sh; do bash -n "$f" || exit 1; done' "$root"
ptb_fails BLD-negative-control           bash -n <(echo 'if then fi')

# P2P: producer | consumer across one seam (help text -> command inventory), far end real
inv="$(bash "$fc" --help | awk '/^COMMANDS:/{f=1;next} /^$/{f=0} f{print $1}')"
ptb_eq    P2P-help-inventory-nonempty   1 "$([ -n "$inv" ] && echo 1 || echo 0)"
ptb_eq    P2P-inventory-has-status      1 "$(printf '%s\n' "$inv" | grep -cx status)"
ptb_done
