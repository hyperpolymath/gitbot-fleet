#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# reattest-pinned-source.sh — move one attested reusable-workflow pin to a new
# commit, in scripts/lib/pinned-reusable-sources.json and
# scripts/lib/lock-closure-receipts.tsv, after the callers have moved.
#
# Dependabot may not bump these pins (.github/dependabot.yml ignores them; see
# scripts/tests/dependabot-attested-ignore-test.sh): it cannot re-attest, and
# its bump breaks 'Pinned source closure' on main (#686). This is the
# deliberate path. It automates ONLY the case that needs no judgement, and
# refuses (exit 3, nothing changed) every other case with the reason:
#
#   - the workflow file is byte-identical at the new commit (same git blob and
#     sha256 as attested), so its `uses:` edges are unchanged; a changed file
#     needs a human review of its new edges and lock records;
#   - every caller recorded in the manifest already calls the new commit;
#   - no other active record shares the old commit (receipt rows are the union
#     over all files called at one owner/repo@sha, so splitting them is a
#     review decision);
#   - the receipt block is the one-header-then-rows layout this file uses.
#
# The receipt rows are re-emitted with audit-lock-closure.sh --emit-receipt,
# never hand-written. Afterwards it runs audit-pinned-reusable-sources.sh when
# the repository has it, and fails if that reports a finding.
#
# Usage:
#   reattest-pinned-source.sh OWNER/REPO PATH NEW_SHA [--source FILE] [--repo-root DIR]
#     PATH      the workflow path inside OWNER/REPO, e.g. .github/workflows/x.yml
#     --source  the file at NEW_SHA, read instead of fetching it (offline, tests)
#
# Exit: 0 re-attested, or already attested at NEW_SHA · 3 refused (needs a
# human) · 64 usage · 69 a tool or the fetch is unavailable.
set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
die() { printf 'reattest-pinned-source: %s\n' "$1" >&2; exit "${2:-3}"; }
usage() { sed -n '/^# Usage:/,/^# Exit:/p' "$0" | sed 's/^# \{0,1\}//' >&2; exit 64; }

[[ $# -ge 3 ]] || usage
REPOSITORY="$1" WF_PATH="$2" NEW="$3"; shift 3
SOURCE="" ROOT=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --source) [[ $# -ge 2 ]] || usage; SOURCE="$2"; shift 2 ;;
    --repo-root) [[ $# -ge 2 ]] || usage; ROOT="$2"; shift 2 ;;
    *) usage ;;
  esac
done
[[ "$REPOSITORY" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || die "OWNER/REPO expected, got ${REPOSITORY@Q}" 64
[[ "$WF_PATH" =~ ^\.github/workflows/[A-Za-z0-9_./-]+\.ya?ml$ && "$WF_PATH" != *..* ]] \
  || die "PATH must be a .github/workflows/*.yml path, got ${WF_PATH@Q}" 64
[[ "$NEW" =~ ^[0-9a-f]{40}$ ]] || die "NEW_SHA must be 40 lowercase hex" 64
for tool in jq git sha256sum; do command -v "$tool" >/dev/null || die "$tool is required" 69; done
if [[ -z "$ROOT" ]]; then ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not in a git work tree; pass --repo-root" 64; fi
MANIFEST="$ROOT/scripts/lib/pinned-reusable-sources.json"
RECEIPTS="$ROOT/scripts/lib/lock-closure-receipts.tsv"
[[ -f "$MANIFEST" && -f "$RECEIPTS" ]] || die "no manifest or receipts under $ROOT/scripts/lib" 64

WORK="$(mktemp -d "${TMPDIR:-/tmp}/reattest.XXXXXX")"; trap 'rm -rf "$WORK"' EXIT

# The one active record for this workflow.
jq -c --arg r "$REPOSITORY" --arg p "$WF_PATH" \
  '[.sources[] | select(.repository == $r and .path == $p and .role == "active")]' "$MANIFEST" > "$WORK/rec.json"
case "$(jq length "$WORK/rec.json")" in
  1) ;;
  0) die "no active record for $REPOSITORY/$WF_PATH in the manifest; attesting a new source is a review, not a move" ;;
  *) die "more than one active record for $REPOSITORY/$WF_PATH; fix the manifest first" ;;
esac
OLD="$(jq -r '.[0].commit' "$WORK/rec.json")"
BLOB="$(jq -r '.[0].blob' "$WORK/rec.json")" SUM="$(jq -r '.[0].content_sha256' "$WORK/rec.json")"
if [[ "$OLD" == "$NEW" ]]; then echo "reattest-pinned-source: $REPOSITORY/$WF_PATH is already attested at $NEW"; exit 0; fi

# Receipt rows are a union per owner/repo@sha: refuse to split a shared one.
shared="$(jq -r --arg r "$REPOSITORY" --arg p "$WF_PATH" --arg c "$OLD" \
  '.sources[] | select(.repository == $r and .commit == $c and .path != $p and .role == "active") | .path' "$MANIFEST")"
[[ -z "$shared" ]] || die "$REPOSITORY@${OLD:0:12} is also attested for: $shared. Its receipt rows are the union over those files, so moving one alone is a review decision; move them together by hand"

# Every recorded caller must already call the new commit.
call="$REPOSITORY/$WF_PATH@"
while read -r wf; do
  [[ -n "$wf" ]] || continue
  [[ -f "$ROOT/$wf" ]] || die "caller $wf is in the manifest but not in the repository"
  grep -qF -- "$call$NEW" "$ROOT/$wf" || die "caller $wf does not call $call$NEW yet; move the caller pin first"
  ! grep -qF -- "$call$OLD" "$ROOT/$wf" || die "caller $wf still calls $call$OLD as well"
done < <(jq -r '.[0].callers[]?.workflow' "$WORK/rec.json" | sort -u)

# The file at the new commit, from --source or fetched.
if [[ -n "$SOURCE" ]]; then
  [[ -f "$SOURCE" ]] || die "--source $SOURCE is not a file" 64
  cp -- "$SOURCE" "$WORK/src.yml"
else
  git init -q --bare "$WORK/fetch.git"
  git -C "$WORK/fetch.git" fetch -q --depth=1 "https://github.com/$REPOSITORY" "$NEW" 2> "$WORK/fetch.err" \
    || die "could not fetch $REPOSITORY@$NEW: $(head -c 300 "$WORK/fetch.err")" 69
  git -C "$WORK/fetch.git" cat-file -p "$NEW:$WF_PATH" > "$WORK/src.yml" 2> "$WORK/fetch.err" \
    || die "$WF_PATH does not exist at $REPOSITORY@$NEW" 3
fi
new_blob="$(git hash-object -- "$WORK/src.yml")"
new_sum="$(sha256sum -- "$WORK/src.yml" | cut -d' ' -f1)"
if [[ "$new_blob" != "$BLOB" || "$new_sum" != "$SUM" ]]; then
  {
    printf 'reattest-pinned-source: %s changed between %s and %s (blob %s -> %s).\n' "$WF_PATH" "${OLD:0:12}" "${NEW:0:12}" "${BLOB:0:12}" "${new_blob:0:12}"
    printf 'reattest-pinned-source: a changed file needs a human review of its uses: edges and lock records; its uses: now are:\n'
    grep -nE '^[[:space:]-]*uses:' "$WORK/src.yml" | sed 's/^/  /' || true
  } >&2
  exit 3
fi

# The receipt block: one '# Reviewed source:' header for this file at OLD,
# followed by exactly the rows for owner/repo<TAB>OLD.
header="# Reviewed source: $REPOSITORY/$WF_PATH@$OLD"
[[ "$(grep -cxF -- "$header" "$RECEIPTS")" -eq 1 ]] || die "expected one receipt header '$header' in $RECEIPTS"
rows_total="$(awk -F'\t' -v r="$REPOSITORY" -v c="$OLD" '$1 == r && $2 == c' "$RECEIPTS" | wc -l)"
rows_after="$(awk -F'\t' -v h="$header" -v r="$REPOSITORY" -v c="$OLD" '
  $0 == h { in_block = 1; next }
  in_block && $1 == r && $2 == c { n++; next }
  { in_block = 0 }
  END { print n + 0 }' "$RECEIPTS")"
[[ "$rows_total" -eq "$rows_after" ]] \
  || die "receipt rows for $REPOSITORY@${OLD:0:12} are not all directly under '$header'; re-attest by hand"
bash "$SELF_DIR/audit-lock-closure.sh" --emit-receipt "$REPOSITORY@$NEW" --source "$WORK/src.yml" > "$WORK/rows.tsv" \
  || die "audit-lock-closure.sh --emit-receipt failed" 69

# Write both files from temporaries, so a failure leaves neither half-updated.
jq --arg r "$REPOSITORY" --arg p "$WF_PATH" --arg old "$OLD" --arg new "$NEW" '
  (.sources[] | select(.repository == $r and .path == $p and .role == "active")) |=
    (.commit = $new | if .source_url then .source_url |= gsub($old; $new) else . end)' "$MANIFEST" > "$WORK/manifest.json"
awk -F'\t' -v h="$header" -v r="$REPOSITORY" -v c="$OLD" -v nh="# Reviewed source: $REPOSITORY/$WF_PATH@$NEW" -v rows="$WORK/rows.tsv" '
  $0 == h { print nh; while ((getline line < rows) > 0) print line; skip = 1; next }
  skip && $1 == r && $2 == c { next }
  { skip = 0; print }' "$RECEIPTS" > "$WORK/receipts.tsv"
cat -- "$WORK/manifest.json" > "$MANIFEST"
cat -- "$WORK/receipts.tsv" > "$RECEIPTS"
echo "reattest-pinned-source: $REPOSITORY/$WF_PATH ${OLD:0:12} -> ${NEW:0:12} (content unchanged: blob ${BLOB:0:12})"

if [[ -f "$ROOT/scripts/audit-pinned-reusable-sources.sh" ]]; then
  out="$(cd "$ROOT" && bash scripts/audit-pinned-reusable-sources.sh 2>&1)" || true
  printf '%s\n' "$out" | grep -E '^(FATAL|pinned reusable sources:)' | sed 's/^/  /'
  if grep -q '^FATAL' <<< "$out"; then die "the pinned-source audit still reports a finding (above)"; fi
else
  echo "reattest-pinned-source: NOT RUN: no scripts/audit-pinned-reusable-sources.sh under $ROOT"
fi
