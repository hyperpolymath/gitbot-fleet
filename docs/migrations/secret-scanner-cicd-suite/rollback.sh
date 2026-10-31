#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# rollback.sh — reverse the prepared secret-scanner migration in a checkout.
#
# Three offline reversals, in the opposite order to application:
#   1. remove the migration's cicd-suite closure receipt;
#   2. move the caller's lock edge back to the previous callee;
#   3. move the caller workflow back to the previous callee.
#
# The required contexts do not change in either direction — the caller job is
# `scan` before and after — so a rollback never edits a ruleset. That is the
# whole point of not renaming a context in the forward change.
#
# The default is a DRY RUN. Pass --apply to edit the checkout.
#
# Usage: rollback.sh [--apply] [--root DIR]
# Exit: 0 dry run clean / rollback applied and re-audited; 1 refusal or audit
#       failure; 2 could not verify (missing audit deps).

set -uo pipefail
MIG="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$MIG/../../.." && pwd)"
APPLY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --apply) APPLY=1 ;;
    --root) ROOT="${2:?--root needs a directory}"; shift ;;
    -h|--help) sed -n '2,22p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "rollback: unknown argument: $1" >&2; exit 64 ;;
  esac
  shift
done

fail=0
note() { printf '%s\n' "$*"; }
refuse() { printf 'REFUSE: %s\n' "$*" >&2; fail=1; }

# The migration must be the only thing reverted: if either side has drifted, a
# blind reverse-apply is how a half-migration reaches main.
for p in lock-closure-receipts.tsv.patch actions.lock.patch secret-scanner.yml.patch; do
  if ! ( cd "$ROOT" && patch -p1 --dry-run -s -f -R < "$MIG/patches/$p" >/dev/null 2>&1 ); then
    refuse "$p does not reverse-apply cleanly; inspect the drift before rolling back"
  fi
done

if [ "$fail" -ne 0 ]; then
  note "No files were changed."
  exit 1
fi

if [ "$APPLY" -eq 0 ]; then
  note "Dry run: all three patches reverse-apply cleanly. Re-run with --apply to revert."
  note "The required contexts are 'scan / gitleaks', 'scan / rust-secrets' and 'scan / shell-secrets' on both sides of this change."
  exit 0
fi

for p in lock-closure-receipts.tsv.patch actions.lock.patch secret-scanner.yml.patch; do
  ( cd "$ROOT" && patch -p1 -s -f -R < "$MIG/patches/$p" ) || { refuse "reversing $p failed"; exit 1; }
done

# Re-prove the reverted lock before anyone believes it.
if [ ! -f "$ROOT/scripts/audit-lock-closure.sh" ]; then
  note "NOT RUN: scripts/audit-lock-closure.sh is missing; the reverted lock was not re-audited."
  exit 2
fi
( cd "$ROOT" && bash scripts/audit-lock-closure.sh )
rc=$?
case "$rc" in
  0) note "Rollback applied; the reverted lock is representation-clean and closed against its receipts." ;;
  1) refuse "the reverted lock has findings (audit exit 1); do not push it" ;;
  2) note "Rollback applied, but the reverted lock's closure is NOT RUN (audit exit 2)."; exit 2 ;;
  *) refuse "the revert audit failed (exit $rc)" ;;
esac

if [ "$fail" -ne 0 ]; then exit 1; fi
note "Next: git revert the migration commit (preferred) or commit the reversed files, and let Lock Sync Gate + Secret Scanner report before closing the incident."
