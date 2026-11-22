#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Tests for scripts/reattest-pinned-source.sh. A fixture repository covers the
# move and every refusal offline. A replay of the real #686 breakage (main at
# dec1e7c, hypatia credential-read-check b93f0615 -> 29a9e4a8) runs when
# github.com is reachable, and is reported NOT RUN, never passed, when not.
#
# Usage: bash scripts/tests/reattest-pinned-source-test.sh
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RE="$ROOT/scripts/reattest-pinned-source.sh"
for t in jq git sha256sum awk; do command -v "$t" >/dev/null || { echo "NOT RUN: needs $t" >&2; exit 2; }; done

pass=0 fail=0 notrun=0
ok() { echo "PASS: $1"; pass=$((pass+1)); }
no() { echo "FAIL: $1"; fail=$((fail+1)); }
check() { if [[ "$2" == "$3" ]]; then ok "$1"; else no "$1 (want ${2@Q}, got ${3@Q})"; fi; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

OLD=1111111111111111111111111111111111111111
NEW=2222222222222222222222222222222222222222
CO=3d3c42e5aac5ba805825da76410c181273ba90b1

# fixture <dir> [caller-ref] -> a minimal repository with one attested source.
fixture() {
  local d="$1" ref="${2:-$NEW}"
  rm -rf "$d"; mkdir -p "$d/.github/workflows" "$d/scripts/lib"
  printf 'on: push\njobs:\n  x:\n    uses: acme/lib/.github/workflows/r.yml@%s\n' "$ref" > "$d/.github/workflows/caller.yml"
  printf 'on: workflow_call\njobs:\n  a:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: actions/checkout@%s\n' "$CO" > "$d/src.yml"
  local blob sum
  blob="$(git hash-object "$d/src.yml")"; sum="$(sha256sum "$d/src.yml" | cut -d' ' -f1)"
  jq -n --arg old "$OLD" --arg blob "$blob" --arg sum "$sum" --arg co "actions/checkout@$CO" '{
    version: 1,
    sources: [{repository: "acme/lib", commit: $old, path: ".github/workflows/r.yml",
               blob: $blob, content_sha256: $sum, role: "active",
               callers: [{workflow: ".github/workflows/caller.yml", job: "x"}],
               uses: [{kind: "step", ref: $co}],
               source_url: ("https://github.com/acme/lib/blob/" + $old + "/.github/workflows/r.yml")}],
    dependencies: {}}' > "$d/scripts/lib/pinned-reusable-sources.json"
  { printf '# SPDX-License-Identifier: MPL-2.0\n# receipts\n\n'
    printf '# Reviewed source: acme/lib/.github/workflows/r.yml@%s\n' "$OLD"
    printf 'acme/lib\t%s\tactions/checkout@%s\n\n' "$OLD" "$CO"
    printf '# Reviewed source: other/x/.github/workflows/y.yml@%s\n' "$OLD"
    printf 'other/x\t%s\tactions/checkout@%s\n' "$OLD" "$CO"; } > "$d/scripts/lib/lock-closure-receipts.tsv"
}
run() { RC=0; bash "$RE" "$@" > "$TMP/out" 2> "$TMP/err" || RC=$?; }
digest() { cat "$1/scripts/lib/pinned-reusable-sources.json" "$1/scripts/lib/lock-closure-receipts.tsv" | sha256sum; }

# --- the move ----------------------------------------------------------------
F="$TMP/f"; fixture "$F"
run acme/lib .github/workflows/r.yml "$NEW" --source "$F/src.yml" --repo-root "$F"
check "an unchanged file at a moved caller pin is re-attested -> exit 0" 0 "$RC"
check "the manifest record's commit moves" "$NEW" "$(jq -r '.sources[0].commit' "$F/scripts/lib/pinned-reusable-sources.json")"
check "its source_url moves with it" "https://github.com/acme/lib/blob/$NEW/.github/workflows/r.yml" "$(jq -r '.sources[0].source_url' "$F/scripts/lib/pinned-reusable-sources.json")"
check "blob, sha256 and uses are untouched" "$(fixture "$TMP/g"; jq -c '.sources[0] | [.blob, .content_sha256, .uses]' "$TMP/g/scripts/lib/pinned-reusable-sources.json")" \
  "$(jq -c '.sources[0] | [.blob, .content_sha256, .uses]' "$F/scripts/lib/pinned-reusable-sources.json")"
check "the receipt header names the new commit" 1 "$(grep -cxF "# Reviewed source: acme/lib/.github/workflows/r.yml@$NEW" "$F/scripts/lib/lock-closure-receipts.tsv")"
check "the receipt rows are re-emitted at the new commit" "acme/lib	$NEW	actions/checkout@$CO" "$(grep -P "^acme/lib\t" "$F/scripts/lib/lock-closure-receipts.tsv")"
check "another source's block (same old sha, other repo) is untouched" "other/x	$OLD	actions/checkout@$CO" "$(grep -P "^other/x\t" "$F/scripts/lib/lock-closure-receipts.tsv")"
check "the post-move audit is reported NOT RUN when the repo lacks it, never passed" 1 "$(grep -c 'NOT RUN' "$TMP/out")"
d1="$(digest "$F")"; run acme/lib .github/workflows/r.yml "$NEW" --source "$F/src.yml" --repo-root "$F"
check "idempotent: a second run says already attested, changes nothing" "0 1 $d1" "$RC $(grep -c 'already attested' "$TMP/out") $(digest "$F")"

# --- refusals: exit 3 and nothing changed ---------------------------------------
refused() { # name reason-regex
  check "$1: refused, exit 3" 3 "$RC"
  check "$1: nothing changed" "$d0" "$(digest "$F")"
  if grep -qE "$2" "$TMP/err"; then ok "$1: the reason says so"; else no "$1: reason $(head -c 200 "$TMP/err")"; fi
}
F="$TMP/f"; fixture "$F"; printf '      - uses: evil/action@%s\n' "$CO" >> "$F/src.yml"; d0="$(digest "$F")"
run acme/lib .github/workflows/r.yml "$NEW" --source "$F/src.yml" --repo-root "$F"
refused "a changed file" "needs a human review of its uses"
if grep -q 'evil/action' "$TMP/err"; then ok "a changed file: its new uses: are listed for review"; else no "a changed file: new uses: not listed"; fi

fixture "$F" "$OLD"; d0="$(digest "$F")"
run acme/lib .github/workflows/r.yml "$NEW" --source "$F/src.yml" --repo-root "$F"
refused "a caller that has not moved" "does not call .* yet; move the caller pin first"

fixture "$F"; jq --arg old "$OLD" '.sources += [.sources[0] | .path = ".github/workflows/s.yml"]' "$F/scripts/lib/pinned-reusable-sources.json" > "$TMP/m" && mv "$TMP/m" "$F/scripts/lib/pinned-reusable-sources.json"; d0="$(digest "$F")"
run acme/lib .github/workflows/r.yml "$NEW" --source "$F/src.yml" --repo-root "$F"
refused "an old commit shared with another attested file" "also attested for: .github/workflows/s.yml"

fixture "$F"; d0="$(digest "$F")"
run acme/lib .github/workflows/none.yml "$NEW" --source "$F/src.yml" --repo-root "$F"
refused "a source with no attested record" "no active record"

fixture "$F"; printf 'acme/lib\t%s\tstray@%s\n' "$OLD" "$CO" >> "$F/scripts/lib/lock-closure-receipts.tsv"; d0="$(digest "$F")"
run acme/lib .github/workflows/r.yml "$NEW" --source "$F/src.yml" --repo-root "$F"
refused "receipt rows outside their block" "not all directly under"

for args in "acme lib x" "acme/lib ../etc/passwd $NEW" "acme/lib .github/workflows/r.yml abc"; do
  read -ra argv <<< "$args"
  RC=0; bash "$RE" "${argv[@]}" --repo-root "$F" >/dev/null 2>&1 || RC=$?
  check "bad arguments ($args): usage exit 64" 64 "$RC"
done

# --- replay the real #686 breakage, when github.com is reachable --------------
if git -C "$ROOT" cat-file -e 'dec1e7ce36dfa62ea121b530108de04cc8321e69^{commit}' 2>/dev/null \
   && timeout 20 git ls-remote https://github.com/hyperpolymath/hypatia HEAD >/dev/null 2>&1; then
  W="$TMP/replay"; git -C "$ROOT" worktree add -q --detach "$W" dec1e7ce36dfa62ea121b530108de04cc8321e69 2>/dev/null
  cp "$RE" "$W/scripts/reattest-pinned-source.sh"
  RC=0; bash "$W/scripts/reattest-pinned-source.sh" hyperpolymath/hypatia .github/workflows/credential-read-check.yml \
    29a9e4a881b6a3cc4cd27fcbc0886ffbadcbc5fe --repo-root "$W" > "$TMP/out" 2> "$TMP/err" || RC=$?
  check "replay #686 on main@dec1e7c: re-attested, and the pinned-source audit is clean" "0 1" \
    "$RC $(grep -c 'findings=0' "$TMP/out")"
  git -C "$ROOT" worktree remove --force "$W"
else
  echo "NOT RUN: replay of #686 (needs github.com and commit dec1e7c in this clone)"; notrun=$((notrun+1))
fi

echo "reattest-pinned-source-test: $pass passed, $fail failed, $notrun not run"
[[ "$fail" -eq 0 ]]
