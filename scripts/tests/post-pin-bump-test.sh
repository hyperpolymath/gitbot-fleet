#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
# Offline fixtures for scripts/post-pin-bump.sh: bare git repos stand in for the
# reusable-workflow source; no network, host configuration or credentials.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HOOK="$ROOT/scripts/post-pin-bump.sh"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/post-pin-bump-test.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
PASS=0; FAIL=0
for tool in git jq timeout; do command -v "$tool" >/dev/null || { echo "Missing test prerequisite: $tool" >&2; exit 64; }; done
[[ -f "$HOOK" ]] || { echo 'Missing hook; prerequisite failure, not a successful rejection.' >&2; exit 64; }

commit_tree() {
  printf '%s\n' "$3" | GIT_AUTHOR_DATE='2026-11-07T00:00:00Z' GIT_COMMITTER_DATE='2026-11-07T00:00:00Z' \
    git -c user.name='Hook fixture' -c user.email='fixture@example.invalid' -c commit.gpgsign=false --git-dir="$1" commit-tree "$2"
}
# source_object REPO TEXT: write TEXT at .github/workflows/reuse.yml; sets NEW_BLOB, NEW_COMMIT.
source_object() {
  local repo="$1" text="$2" index tree
  NEW_BLOB="$(printf '%s\n' "$text" | git --git-dir="$repo" hash-object -w --stdin)"
  index="$(mktemp "$WORK/index.XXXXXX")"; rm "$index"
  GIT_INDEX_FILE="$index" git --git-dir="$repo" read-tree --empty
  GIT_INDEX_FILE="$index" git --git-dir="$repo" update-index --add --cacheinfo "100644,$NEW_BLOB,.github/workflows/reuse.yml"
  tree="$(GIT_INDEX_FILE="$index" git --git-dir="$repo" write-tree)"
  NEW_COMMIT="$(commit_tree "$repo" "$tree" "Fixture $RANDOM$RANDOM")"
  rm "$index"
}

BASE="$WORK/base"; mkdir -p "$BASE/cache/org" "$BASE/repo/.github/workflows" "$BASE/repo/scripts/lib"
SRC="$BASE/cache/org/reusable.git"; git init -q --bare "$SRC"
git init -q --bare "$BASE/cache/org/child.git"
CHILD_SHA="$(commit_tree "$BASE/cache/org/child.git" "$(git --git-dir="$BASE/cache/org/child.git" mktree </dev/null)" 'Fixture child')"
CHILD_KEY="org/child@$CHILD_SHA"
TEXT=$'name: Reusable\non: workflow_call\njobs:\n  build:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: org/child@'"$CHILD_SHA"
source_object "$SRC" "$TEXT"; OLD="$NEW_COMMIT"; BLOB="$NEW_BLOB"
# A new commit holding the SAME bytes (pin-only), a different blob, and different edges.
source_object "$SRC" "$TEXT"; SAME="$NEW_COMMIT"
# Same text means the same blob; a distinct commit comes from the unique message.
source_object "$SRC" "$TEXT"$'\n# changed'; CHANGED="$NEW_COMMIT"
source_object "$SRC" "${TEXT/org\/child@$CHILD_SHA/org\/other@$CHILD_SHA}"; EDGES="$NEW_COMMIT"
cat > "$BASE/repo/.github/workflows/test.yml" <<YAML
name: Test
on: push
jobs:
  gate:
    uses: org/reusable/.github/workflows/reuse.yml@$OLD
YAML
printf 'version: %s\nworkflows:\n    %s:\ndependencies:\n    %s:\n        ref: %s\n        commit: %s\n' \
  "'v0.0.3'" "'.github/workflows/test.yml'" "'$CHILD_KEY'" "'v1'" "'sha1-$CHILD_SHA'" > "$BASE/repo/.github/workflows/actions.lock"
CONTENT="$(git --git-dir="$SRC" cat-file blob "$BLOB" | sha256sum | cut -d' ' -f1)"
jq -n --arg sha "$OLD" --arg blob "$BLOB" --arg cs "$CONTENT" --arg child "$CHILD_SHA" --arg key "$CHILD_KEY" '
 {version:1,sources:[{repository:"org/reusable",commit:$sha,path:".github/workflows/reuse.yml",blob:$blob,content_sha256:$cs,
 role:"active",callers:[{workflow:".github/workflows/test.yml",job:"gate"}],uses:[{kind:"step",ref:$key}]}],
 dependencies:{}}' > "$BASE/repo/scripts/lib/pinned-reusable-sources.json"
printf '# SPDX-License-Identifier: MPL-2.0\n# Reviewed source: org/reusable/.github/workflows/reuse.yml@%s\norg/reusable\t%s\t%s\n' "$OLD" "$OLD" "$CHILD_KEY" > "$BASE/repo/scripts/lib/lock-closure-receipts.tsv"
printf 'scripts/lib/pinned-reusable-sources.json\nscripts/lib/lock-closure-receipts.tsv\n' > "$BASE/repo/.github/post-pin-bump.paths"
mkdir -p "$BASE/repo/scripts"; cp "$ROOT"/scripts/audit-pinned-reusable-sources.sh "$ROOT"/scripts/audit-lock-closure.sh "$BASE/repo/scripts/"
[[ -d "$ROOT/scripts/lib" ]] && for f in "$ROOT"/scripts/lib/*; do [[ -f "$BASE/repo/scripts/lib/$(basename "$f")" ]] || cp "$f" "$BASE/repo/scripts/lib/"; done
cp "$ROOT/scripts/post-pin-bump.sh" "$BASE/repo/scripts/"
git init -q "$BASE/repo"
fix() { CASE="$WORK/$1"; mkdir "$CASE"; cp -a "$BASE/." "$CASE/"; R="$CASE/repo"; }
bump() { sed -i "s/@$OLD/@$1/" "$R/.github/workflows/test.yml"; }
run() { rc=0; (cd "$R" && timeout 30s bash scripts/post-pin-bump.sh --repo-root "$R" --cache "$CASE/cache" > "$CASE/out" 2> "$CASE/err") || rc=$?; }
ok() { PASS=$((PASS+1)); printf 'PASS: %s\n' "$1"; }
bad() { FAIL=$((FAIL+1)); printf 'FAIL: %s\n' "$1"; cat "$CASE/err" 2>/dev/null || true; }
unchanged() { cmp -s "$BASE/repo/scripts/lib/pinned-reusable-sources.json" "$R/scripts/lib/pinned-reusable-sources.json" && cmp -s "$BASE/repo/scripts/lib/lock-closure-receipts.tsv" "$R/scripts/lib/lock-closure-receipts.tsv"; }

fix noop; run
if [[ $rc == 0 && ! -s "$CASE/out" ]] && unchanged; then ok 'no pin moved: exit 0, no output, no change'; else bad 'no pin moved'; fi

fix pin-only; bump "$SAME"; run
paths="$(tr '\0' '\n' < "$CASE/out")"
if [[ $rc == 0 && "$paths" == $'scripts/lib/pinned-reusable-sources.json\nscripts/lib/lock-closure-receipts.tsv' ]] &&
   jq -e --arg s "$SAME" '.sources[0].commit==$s' "$R/scripts/lib/pinned-reusable-sources.json" >/dev/null &&
   grep -q "reuse.yml@$SAME" "$R/scripts/lib/lock-closure-receipts.tsv" && grep -q "^org/reusable	$SAME	" "$R/scripts/lib/lock-closure-receipts.tsv" &&
   ! grep -q "$OLD" "$R/scripts/lib/lock-closure-receipts.tsv" &&
   [[ "$(tr -cd '\0' < "$CASE/out" | wc -c)" == 2 ]]; then ok 'pin-only bump: both files re-attested, paths NUL-separated'; else bad 'pin-only bump'; fi

fix idempotent; bump "$SAME"; run; cp -a "$R/scripts/lib" "$CASE/after"; run
if [[ $rc == 0 && ! -s "$CASE/out" ]] && diff -r "$CASE/after" "$R/scripts/lib" >/dev/null; then ok 'second run is a no-op (idempotent)'; else bad 'idempotent'; fi

for pair in "changed-blob:$CHANGED" "changed-edges:$EDGES"; do
  fix "${pair%%:*}"; bump "${pair#*:}"; run
  if [[ $rc == 1 && ! -s "$CASE/out" ]] && unchanged && grep -q REFUSED "$CASE/err"; then ok "refuses ${pair%%:*}; nothing written"; else bad "${pair%%:*} must be refused"; fi
done

fix other-repo; sed -i "s#org/reusable/#org/elsewhere/#; s#@$OLD#@$SAME#" "$R/.github/workflows/test.yml"; run
if [[ $rc == 1 ]] && unchanged; then ok 'refuses a changed repository (not pin-only)'; else bad 'changed repository'; fi

fix unreachable-source; bump "$SAME"; rm -rf "$CASE/cache/org/reusable.git"; run
if [[ $rc == 1 && ! -s "$CASE/out" ]] && unchanged; then ok 'refuses (not guesses) when the new source is unavailable offline'; else bad 'unavailable source'; fi

fix shared-commit; bump "$SAME"
jq '.sources += [(.sources[0] | .role="retained-review" | .callers=[] | .path=".github/workflows/reuse.yml")]' "$R/scripts/lib/pinned-reusable-sources.json" > "$CASE/m.json"
jq '.sources[1].path=".github/workflows/reuse.yml" | .sources[1].repository="org/reusable"' "$CASE/m.json" > "$R/scripts/lib/pinned-reusable-sources.json"
cp "$R/scripts/lib/pinned-reusable-sources.json" "$CASE/pre.json"; run
if [[ $rc == 1 ]] && cmp -s "$CASE/pre.json" "$R/scripts/lib/pinned-reusable-sources.json"; then ok 'refuses a commit shared with a source that did not move'; else bad 'shared commit'; fi

fix allow-list; bump "$SAME"; printf 'scripts/lib/pinned-reusable-sources.json\n' > "$R/.github/post-pin-bump.paths"; run
if [[ $rc == 1 ]] && unchanged; then ok 'refuses to change a path outside the allow-list'; else bad 'allow-list'; fi

# Fetch policy: POST_PIN_BUMP=1 (no arguments) must make the hook fetch; without it, offline.
fix fetch-policy; bump "$SAME"
mv "$R/scripts/audit-pinned-reusable-sources.sh" "$R/scripts/audit-real.sh"
printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$*" >> "%s/args"\nexec bash "$(dirname "$0")/audit-real.sh" "$@"\n' "$CASE" > "$R/scripts/audit-pinned-reusable-sources.sh"
rc=0; (cd "$R" && POST_PIN_BUMP=1 timeout 30s bash scripts/post-pin-bump.sh --repo-root "$R" --cache "$CASE/cache" > "$CASE/out" 2> "$CASE/err") || rc=$?
if [[ $rc == 0 ]] && grep -q -- '--fetch-sources' "$CASE/args" && ! grep -v -- '--roots-only' "$CASE/args" | grep -qv -- '--fetch-sources'; then ok 'POST_PIN_BUMP=1 turns fetching on for every source audit call'; else bad 'POST_PIN_BUMP=1 did not enable fetching'; fi
fix fetch-offline; bump "$SAME"
mv "$R/scripts/audit-pinned-reusable-sources.sh" "$R/scripts/audit-real.sh"
printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$*" >> "%s/args"\nexec bash "$(dirname "$0")/audit-real.sh" "$@"\n' "$CASE" > "$R/scripts/audit-pinned-reusable-sources.sh"
rc=0; (cd "$R" && env -u POST_PIN_BUMP timeout 30s bash scripts/post-pin-bump.sh --repo-root "$R" --cache "$CASE/cache" > "$CASE/out" 2> "$CASE/err") || rc=$?
if [[ $rc == 0 && -s "$CASE/args" ]] && ! grep -q -- '--fetch-sources' "$CASE/args"; then ok 'without POST_PIN_BUMP the hook stays offline'; else bad 'default is not offline'; fi

# Mutant: if the hook skipped both audits it would accept the changed blob.
fix mutant; bump "$CHANGED"; sed -i 's/^if ! bash "\$AUDIT" "\${audit_args\[@\]}".*/if false; then/' "$R/scripts/post-pin-bump.sh"; run
if [[ $rc == 0 ]] && ! unchanged; then ok 'mutant (both audits disabled) accepts a changed blob, so the audit is what refuses'; else bad 'mutant did not demonstrate the audit is load-bearing'; fi

printf '%s passed, %s failed\n' "$PASS" "$FAIL"
[[ "$FAIL" -eq 0 ]]
