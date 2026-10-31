#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# lock-closure-test.sh — regression tests for scripts/audit-lock-closure.sh.
#
# These fixtures exist because the two failure modes that matter here are both
# invisible to every other gate:
#
#   * a regeneration that DROPS a job-level reusable `workflows:` entry
#     (`gh actions-lock` has no code path that writes one) — nothing dangles,
#     so clauses 1-4 of check-lock-sync.sh stay green;
#   * a regeneration that SHORTENS or removes a record's nested `uses:` list —
#     again nothing dangles, so the lock still "resolves";
#   * a ref that is well-formed enough to parse but malformed for GitHub
#     (subpath pin inside nested uses, refs/tags/ prefix, self-SHA action ref,
#     commit that disagrees with the key).
#
# Each case asserts on the audit's FINDINGS-vs-NOT-RUN split, not just its exit
# code, so a change that starts reporting a fatal defect as "not run" fails.
#
# Usage: bash scripts/tests/lock-closure-test.sh
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
AUDIT="$ROOT/scripts/audit-lock-closure.sh"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/lock-closure-test.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

PASS=0; FAIL=0
pass() { PASS=$((PASS + 1)); printf 'PASS: %s\n' "$1"; }
fail() { FAIL=$((FAIL + 1)); printf 'FAIL: %s\n' "$1"; [ -n "${2:-}" ] && printf '%s\n' "$2" | sed 's/^/      /'; }

RC=0; OUT=""
run_audit() { # <fixture-dir> [extra args...]
  local dir="$1"; shift
  OUT="$(cd "$ROOT" && "$AUDIT" --workflows "$dir/.github/workflows" \
          --receipts "$dir/receipts.tsv" --baseline "$dir/baseline.tsv" \
          --repo-root "$dir" "$@" 2>&1)"
  RC=$?
}

expect_rc() { # <name> <expected> [forbidden-needle]
  if [ "$RC" -eq "$2" ]; then pass "$1 (exit $RC)"; else fail "$1 (expected exit $2, got $RC)" "$OUT"; fi
  if [ -n "${3:-}" ] && printf '%s' "$OUT" | grep -qF "$3"; then
    fail "$1 (output must not contain '$3')" "$OUT"
  fi
}
expect_out() { # <name> <needle>
  if printf '%s' "$OUT" | grep -qF "$2"; then pass "$1"; else fail "$1 (missing '$2')" "$OUT"; fi
}

# Canonical fixture skeleton --------------------------------------------------
new_repo() { # <name>
  local d="$WORK/$1"
  mkdir -p "$d/.github/workflows"
  : > "$d/receipts.tsv"
  : > "$d/baseline.tsv"
  printf '%s' "$d"
}

SHA_75=75bd5994470661a099df562d6d5ad2fe744dbd66
SHA_CK=3d3c42e5aac5ba805825da76410c181273ba90b1
SHA_UA=cf430e030ddbb5b0abf93d22962f4752f3646cd9
SHA_UPA=fc324d3547104276b827a68afc52ff2a11cc49c9
SHA_UB=bbbca2ddaa5d8feaa63e36b76fdaad77386f024f

# A reusable caller whose closure is complete: entry + record + nested edge +
# matching receipt. Reference case for every "dropped" mutation below.
write_good_reusable() { # <fixture-dir>
  cat > "$1/.github/workflows/scan.yml" <<YAML
name: Scan
on:
  pull_request:
jobs:
  scan:
    permissions:
      contents: read
    uses: hyperpolymath/standards/.github/workflows/secret-scanner-reusable.yml@$SHA_75
YAML
  cat > "$1/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/scan.yml':
        - 'hyperpolymath/standards@$SHA_75'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'
    'hyperpolymath/standards@$SHA_75':
        ref: '$SHA_75'
        commit: 'sha1-$SHA_75'
        uses:
            - 'actions/checkout@v7.0.1'
LOCK
  printf 'hyperpolymath/standards\t%s\tactions/checkout@v7.0.1\n' "$SHA_75" > "$1/receipts.tsv"
}

# ---------------------------------------------------------------- reference --
d="$(new_repo good-reusable)"; write_good_reusable "$d"; run_audit "$d"
expect_rc "complete reusable closure is clean" 0
expect_out "complete closure reports OK" "OK:"

d="$(new_repo good-composite)"
cat > "$d/.github/workflows/pages.yml" <<YAML
name: Pages
on: push
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - name: Upload
        uses: actions/upload-pages-artifact@v5.0.0
YAML
cat > "$d/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/pages.yml':
        - 'actions/upload-pages-artifact@v5.0.0'
dependencies:
    'actions/upload-artifact@v7.0.0':
        ref: 'v7.0.0'
        commit: 'sha1-$SHA_UB'
    'actions/upload-pages-artifact@v5.0.0':
        ref: 'v5.0.0'
        commit: 'sha1-$SHA_UPA'
        uses:
            - 'actions/upload-artifact@v7.0.0'
LOCK
printf 'actions/upload-pages-artifact\t%s\tactions/upload-artifact@v7.0.0\n' "$SHA_UPA" > "$d/receipts.tsv"
run_audit "$d"
expect_rc "complete composite closure is clean" 0

# ------------------------------------------- a regeneration that drops closure --
# `gh actions-lock` does not lock a job-level ref at all. Simulate the tool-only
# regeneration: the workflows: entry and its record disappear, nothing dangles.
d="$(new_repo regen-drops-job-ref)"; write_good_reusable "$d"
cat > "$d/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/scan.yml': []
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'
LOCK
: > "$d/receipts.tsv"   # the source is no longer named by any record
run_audit "$d"
expect_rc "tool-only regeneration is not a pass (unlocked job-level ref)" 2
expect_out "the dropped job-level ref is reported NOT RUN" "REUSABLE-UNLOCKED"
expect_out "closure is explicitly called unproven" "closure is unproven"

# Same regeneration, but a hand-added entry with a LEAF record: the fatal shape,
# because a leaf record is present-but-unresolvable for a reusable workflow.
d="$(new_repo regen-leaf-record)"; write_good_reusable "$d"
cat > "$d/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/scan.yml':
        - 'hyperpolymath/standards@$SHA_75'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'
    'hyperpolymath/standards@$SHA_75':
        ref: '$SHA_75'
        commit: 'sha1-$SHA_75'
LOCK
run_audit "$d"
expect_rc "a leaf record for a reusable is a finding" 1
expect_out "leaf reusable closure is reported as dropped" "CLOSURE-DROPPED"

# A shortened nested list: no dangling ref exists, yet an edge is gone.
d="$(new_repo regen-short-list)"; write_good_reusable "$d"
cat > "$d/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/scan.yml':
        - 'hyperpolymath/standards@$SHA_75'
dependencies:
    'hyperpolymath/standards@$SHA_75':
        ref: '$SHA_75'
        commit: 'sha1-$SHA_75'
        uses:
            - 'actions/checkout@v7.0.1'
LOCK
printf 'hyperpolymath/standards\t%s\tactions/checkout@v7.0.1\nhyperpolymath/standards\t%s\tactions/cache@v6.1.0\n' "$SHA_75" "$SHA_75" > "$d/receipts.tsv"
run_audit "$d"
expect_rc "a shortened nested list is a finding" 1
expect_out "the missing edge is named" "actions/cache@v6.1.0"
expect_out "the loss is classified as dropped closure" "dropped closure"

# A composite record shortened to a leaf: the exact class clause 3 cannot see.
d="$(new_repo composite-leaf)"
cat > "$d/.github/workflows/pages.yml" <<YAML
name: Pages
on: push
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/upload-pages-artifact@v5.0.0
YAML
cat > "$d/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/pages.yml':
        - 'actions/upload-pages-artifact@v5.0.0'
dependencies:
    'actions/upload-pages-artifact@v5.0.0':
        ref: 'v5.0.0'
        commit: 'sha1-$SHA_UPA'
LOCK
printf 'actions/upload-pages-artifact\t%s\tactions/upload-artifact@v7.0.0\n' "$SHA_UPA" > "$d/receipts.tsv"
run_audit "$d"
expect_rc "a leaf composite is a finding when its receipt has edges" 1
expect_out "composite closure loss is reported" "CLOSURE-DROPPED"

# An extra, unreviewed edge in a record is equally a finding: the lock would
# claim a dependency the reviewed source does not have.
d="$(new_repo composite-extra)"
cat > "$d/.github/workflows/pages.yml" <<YAML
name: Pages
on: push
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/upload-pages-artifact@v5.0.0
YAML
cat > "$d/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/pages.yml':
        - 'actions/upload-pages-artifact@v5.0.0'
dependencies:
    'actions/setup-node@v6.0.0':
        ref: 'v6.0.0'
        commit: 'sha1-1111111111111111111111111111111111111111'
    'actions/upload-pages-artifact@v5.0.0':
        ref: 'v5.0.0'
        commit: 'sha1-$SHA_UPA'
        uses:
            - 'actions/setup-node@v6.0.0'
LOCK
printf 'actions/upload-pages-artifact\t%s\tactions/upload-artifact@v7.0.0\n' "$SHA_UPA" > "$d/receipts.tsv"
run_audit "$d"
expect_rc "an unreviewed nested edge is a finding" 1
expect_out "the extra edge is reported" "CLOSURE-EXTRA"

# ------------------------------------------------- malformed ref representations --
malformed_case() { # <name> <expected-rule> <lock-body> <workflow-body> [receipts]
  local name="$1" rule="$2" lock="$3" wf="$4" receipts="${5:-}"
  local d; d="$(new_repo "$name")"
  printf '%s\n' "$wf" > "$d/.github/workflows/w.yml"
  printf '%s\n' "$lock" > "$d/.github/workflows/actions.lock"
  [ -n "$receipts" ] && printf '%s\n' "$receipts" > "$d/receipts.tsv"
  run_audit "$d"
  if [ "$RC" -eq 1 ] || [ "$RC" -eq 3 ]; then pass "$name is a finding (exit $RC)"; else fail "$name is a finding (expected 1 or 3, got $RC)" "$OUT"; fi
  expect_out "$name is classified $rule" "$rule"
}

WF_STEP="name: W
on: push
jobs:
  b:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v7.0.1"

malformed_case nested-subpath REF-NESTED-SUBPATH \
"version: 'v0.0.3'
workflows:
    '.github/workflows/w.yml':
        - 'actions/checkout@v7.0.1'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'
        uses:
            - 'github/codeql-action/upload-sarif@$SHA_CK'" "$WF_STEP"

malformed_case refs-prefix REF-REFS-PREFIX \
"version: 'v0.0.3'
workflows:
    '.github/workflows/w.yml':
        - 'actions/checkout@refs/tags/v7.0.1'
dependencies:
    'actions/checkout@refs/tags/v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'" "$WF_STEP"

malformed_case missing-commit REF-NO-COMMIT \
"version: 'v0.0.3'
workflows:
    '.github/workflows/w.yml':
        - 'actions/checkout@v7.0.1'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'" "$WF_STEP"

malformed_case commit-mismatch REF-COMMIT-MISMATCH \
"version: 'v0.0.3'
workflows:
    '.github/workflows/w.yml':
        - 'actions/checkout@$SHA_CK'
dependencies:
    'actions/checkout@$SHA_CK':
        ref: 'v7.0.1'
        commit: 'sha1-2222222222222222222222222222222222222222'" \
"name: W
on: push
jobs:
  b:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@$SHA_CK"

malformed_case case-duplicate REF-CASE-DUP \
"version: 'v0.0.3'
workflows:
    '.github/workflows/w.yml':
        - 'actions/checkout@v7.0.1'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'
    'Actions/Checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'" "$WF_STEP"

malformed_case self-sha-action REF-SELF-SHA-ACTION \
"version: 'v0.0.3'
workflows:
    '.github/workflows/w.yml':
        - 'actions/checkout@$SHA_CK'
dependencies:
    'actions/checkout@$SHA_CK':
        ref: '$SHA_CK'
        commit: 'sha1-$SHA_CK'" \
"name: W
on: push
jobs:
  b:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@$SHA_CK"

malformed_case ambiguous-tag REF-AMBIGUOUS \
"version: 'v0.0.3'
workflows:
    '.github/workflows/w.yml':
        - 'actions/checkout@v7.0.1'
        - 'actions/checkout@$SHA_CK'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'
    'actions/checkout@$SHA_CK':
        ref: 'v7.0.1'
        commit: 'sha1-3333333333333333333333333333333333333333'" \
"name: W
on: push
jobs:
  b:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v7.0.1
      - uses: actions/checkout@$SHA_CK"

# A self-SHA ref is legitimate for a reusable workflow: the reference fixture
# above already proves it (standards@75bd… records ref == commit and exits 0).

# A baseline may acknowledge a REVIEW finding but never a FATAL one.
d="$(new_repo baseline-scope)"; write_good_reusable "$d"
cat > "$d/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/scan.yml':
        - 'hyperpolymath/standards@$SHA_75'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'
    'hyperpolymath/standards@$SHA_75':
        ref: '$SHA_75'
        commit: 'sha1-$SHA_75'
LOCK
printf 'CLOSURE-DROPPED\thyperpolymath/standards@%s\taccepted\tlock-audit\t2026-10-31\n' "$SHA_75" > "$d/baseline.tsv"
run_audit "$d"
expect_rc "a baseline cannot suppress a fatal closure loss" 1
expect_out "the fatal finding is still printed" "CLOSURE-DROPPED"

# A baseline row that matches nothing is NOT RUN, not silence. Without this, a
# stale acknowledgement can hide the regression it was written for -- and a
# baseline pass that runs BEFORE the --online phase marks every REMOTE-* row
# stale, which is how this ordering bug shipped once already.
d="$(new_repo baseline-stale)"; write_good_reusable "$d"
printf 'REF-MUTABLE\tghost/action@main\tretired acknowledgement\tlock-audit\t2026-10-31\n' > "$d/baseline.tsv"
run_audit "$d"
expect_rc "an unmatched baseline row is NOT RUN" 2
expect_out "the stale acknowledgement is named" "BASELINE-STALE"

d="$(new_repo baseline-remote-offline)"; write_good_reusable "$d"
printf 'REMOTE-STALE\tghost/action@main\tonly meaningful with --online\tlock-audit\t2026-10-31\n' > "$d/baseline.tsv"
run_audit "$d"
expect_rc "an online-only baseline row is not called stale offline" 0 "BASELINE-STALE"

# ------------------------------------------------------- unreadable / unknown --
d="$(new_repo unparseable)"
cat > "$d/.github/workflows/w.yml" <<'YAML'
name: W
on: push
jobs:
  b:
    runs-on: ubuntu-latest
    steps:
      - uses: >-
          actions/checkout@v7.0.1
YAML
printf "version: 'v0.0.3'\nworkflows:\n    '.github/workflows/w.yml': []\ndependencies: {}\n" > "$d/.github/workflows/actions.lock"
run_audit "$d"
expect_rc "an unreadable uses: value is NOT RUN, not green" 2
expect_out "the parse refusal is explicit" "WF-PARSE"

d="$(new_repo local-composite-missing)"
cat > "$d/.github/workflows/w.yml" <<'YAML'
name: W
on: push
jobs:
  b:
    runs-on: ubuntu-latest
    steps:
      - uses: ./.github/actions/absent
YAML
printf "version: 'v0.0.3'\nworkflows:\n    '.github/workflows/w.yml': []\ndependencies: {}\n" > "$d/.github/workflows/actions.lock"
run_audit "$d"
expect_rc "a missing local composite is NOT RUN" 2
expect_out "the missing composite is named" "WF-LOCAL-MISSING"

# A local composite's refs belong to the calling workflow (standards#1122).
d="$(new_repo local-composite-closure)"
mkdir -p "$d/.github/actions/x"
cat > "$d/.github/actions/x/action.yml" <<'YAML'
name: x
runs:
  using: composite
  steps:
    - uses: actions/checkout@v7.0.1
YAML
cat > "$d/.github/workflows/w.yml" <<'YAML'
name: W
on: push
jobs:
  b:
    runs-on: ubuntu-latest
    steps:
      - uses: ./.github/actions/x
YAML
cat > "$d/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/w.yml':
        - 'actions/checkout@v7.0.1'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$SHA_CK'
LOCK
run_audit "$d"
expect_rc "a local composite's refs are locked under the caller" 0

# --------------------------------------------------------------- this repo ----
OUT="$(cd "$ROOT" && ./scripts/audit-lock-closure.sh 2>&1)"; RC=$?
expect_rc "this repository's own lock is clean (offline)" 0
expect_out "the repo audit reports every owned edge set reviewed" "every owned edge set matches a reviewed receipt"

printf '\n%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
