#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
# Offline commit/tree/blob fixtures; no network, host configuration or credentials.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HELPER="$ROOT/scripts/audit-pinned-reusable-sources.sh"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/pinned-reusable-test.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
PASS=0; FAIL=0
for tool in git jq timeout; do command -v "$tool" >/dev/null || { echo "Missing test prerequisite: $tool" >&2; exit 64; }; done

# Set fixture identities only for this invocation. No signing key or host Git
# configuration is used: the property under test is immutable object identity.
commit_tree() {
  local repo="$1" tree="$2" message="$3"
  printf '%s\n' "$message" | GIT_AUTHOR_DATE='2026-11-07T00:00:00Z' GIT_COMMITTER_DATE='2026-11-07T00:00:00Z' \
    git -c user.name='Closure fixture' -c user.email='fixture@example.invalid' -c commit.gpgsign=false \
    --git-dir="$repo" commit-tree "$tree"
}
source_object() {
  local repo="$1" text="$2" mode="${3:-100644}" index tree
  NEW_BLOB="$(printf '%s\n' "$text" | git --git-dir="$repo" hash-object -w --stdin)"
  index="$(mktemp "$WORK/index.XXXXXX")"; rm "$index"
  GIT_INDEX_FILE="$index" git --git-dir="$repo" read-tree --empty
  GIT_INDEX_FILE="$index" git --git-dir="$repo" update-index --add --cacheinfo "$mode,$NEW_BLOB,.github/workflows/reuse.yml"
  GIT_INDEX_FILE="$index" git --git-dir="$repo" update-index --add --cacheinfo "100644,$NEW_BLOB,.github/workflows/other.yml"
  tree="$(GIT_INDEX_FILE="$index" git --git-dir="$repo" write-tree)"
  NEW_COMMIT="$(commit_tree "$repo" "$tree" 'Fixture source')"
  git --git-dir="$repo" update-ref refs/heads/fixture "$NEW_COMMIT"
  rm "$index"
}
BASE="$WORK/base"; mkdir -p "$BASE/cache/org" "$BASE/repo/.github/workflows" "$BASE/repo/scripts/lib"
git init -q --bare "$BASE/cache/org/reusable.git"
git init -q --bare "$BASE/cache/org/child.git"
CHILD_SHA="$(commit_tree "$BASE/cache/org/child.git" "$(git --git-dir="$BASE/cache/org/child.git" mktree </dev/null)" 'Fixture child')"
CHILD_KEY="org/child@$CHILD_SHA"
SOURCE_TEXT=$'name: Reusable\non: workflow_call\njobs:\n  build:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: org/child@'"$CHILD_SHA"
source_object "$BASE/cache/org/reusable.git" "$SOURCE_TEXT"
ROOT_SHA="$NEW_COMMIT"; ROOT_BLOB="$NEW_BLOB"; ROOT_KEY="org/reusable@$ROOT_SHA"
cat > "$BASE/repo/.github/workflows/test.yml" <<YAML
name: Test
on: push
jobs:
  gate:
    uses: org/reusable/.github/workflows/reuse.yml@$ROOT_SHA
YAML
jq -n --arg sha "$ROOT_SHA" --arg blob "$ROOT_BLOB" --arg child "$CHILD_SHA" --arg key "$ROOT_KEY" '
 {version:1,sources:[{repository:"org/reusable",commit:$sha,path:".github/workflows/reuse.yml",blob:$blob,
 role:"active",callers:[{workflow:".github/workflows/test.yml",job:"gate"}],uses:[{kind:"step",ref:("org/child@"+$child)}]}],
 dependencies:{($key):{ref:$sha,commit:("sha1-"+$sha),uses:[("org/child@"+$child)]},("org/child@"+$child):{ref:"v1",commit:("sha1-"+$child)}}}' \
 > "$BASE/repo/scripts/lib/pinned-reusable-sources.json"
cat > "$BASE/repo/.github/workflows/actions.lock" <<YAML
version: 'v0.0.3'
workflows:
    '.github/workflows/test.yml':
        - '$ROOT_KEY'
dependencies:
    '$ROOT_KEY':
        ref: '$ROOT_SHA'
        commit: 'sha1-$ROOT_SHA'
        uses:
            - '$CHILD_KEY'
    '$CHILD_KEY':
        ref: 'v1'
        commit: 'sha1-$CHILD_SHA'
YAML
printf 'org/reusable\t%s\t%s\n' "$ROOT_SHA" "$CHILD_KEY" > "$BASE/repo/receipts.tsv"
: > "$BASE/repo/baseline.tsv"
case_fixture() { CASE="$WORK/$1"; mkdir "$CASE"; cp -a "$BASE/." "$CASE/"; MANIFEST="$CASE/repo/scripts/lib/pinned-reusable-sources.json"; }
edit_manifest() { jq "$@" "$MANIFEST" > "$CASE/edit.json"; mv "$CASE/edit.json" "$MANIFEST"; }
replace_pin() {
  local old="$1" new="$2" file
  for file in "$MANIFEST" "$CASE/repo/.github/workflows/test.yml" "$CASE/repo/.github/workflows/actions.lock" "$CASE/repo/receipts.tsv"; do
    sed "s/$old/$new/g" "$file" > "$CASE/replaced"; mv "$CASE/replaced" "$file"
  done
}
check() {
  local label="$1" expected="$2" rc=0 valid=0
  timeout 15s bash "$HELPER" --json --repo-root "$CASE/repo" --manifest "$MANIFEST" \
    --lock "$CASE/repo/.github/workflows/actions.lock" --receipts "$CASE/repo/receipts.tsv" --cache "$CASE/cache" \
    > "$CASE/result.json" 2> "$CASE/stderr" || rc=$?
  if jq -e 'type=="object" and (.findings|type=="array") and (.not_run|type=="array") and (.verified_roots|type=="array") and (.verified_receipt_sources|type=="array")' "$CASE/result.json" >/dev/null 2>&1; then
    case "$expected" in
      clean) [[ "$rc" == 0 ]] && jq -e '(.findings|length)==0 and (.not_run|length)==0 and (.verified_roots|length)==1 and (.verified_receipt_sources|length)>0' "$CASE/result.json" >/dev/null && valid=1 ;;
      fatal) [[ "$rc" == 1 || "$rc" == 3 ]] && jq -e 'any(.findings[]; .severity=="FATAL" and (.rule|length)>0 and (.detail|length)>0) and (.verified_roots|length)==0 and (.verified_receipt_sources|length)==0' "$CASE/result.json" >/dev/null && valid=1 ;;
      notrun) [[ "$rc" == 2 ]] && jq -e '(.findings|length)==0 and any(.not_run[]; (.rule|length)>0 and (.detail|length)>0) and (.verified_roots|length)==0 and (.verified_receipt_sources|length)==0' "$CASE/result.json" >/dev/null && valid=1 ;;
    esac
  fi
  if [[ "$valid" == 1 ]]; then PASS=$((PASS+1)); printf 'PASS: %s (%s, exit %s)\n' "$label" "$expected" "$rc"
  else FAIL=$((FAIL+1)); printf 'FAIL: %s (expected %s; exit %s)\n' "$label" "$expected" "$rc"; cat "$CASE/result.json" "$CASE/stderr"; fi
}

# Demonstrate old false passes, rather than counting a missing new helper as
# evidence. Run only against the preimplementation candidate; nonzero is red.
if [[ "${1:-}" == --preimplementation-probe ]]; then
  gaps=0
  for attack in wrong-blob omitted-active; do
    case_fixture "$attack"
    if [[ "$attack" == wrong-blob ]]; then edit_manifest '.sources[0].blob="1111111111111111111111111111111111111111"'; else edit_manifest '.sources=[]'; fi
    rc=0
    timeout 15s bash "${CLOSURE_AUDIT_UNDER_TEST:-$ROOT/scripts/audit-lock-closure.sh}" --json --repo-root "$CASE/repo" \
      --workflows "$CASE/repo/.github/workflows" --receipts "$CASE/repo/receipts.tsv" --baseline "$CASE/repo/baseline.tsv" \
      > "$CASE/old.json" 2> "$CASE/old.stderr" || rc=$?
    if [[ "$rc" == 0 ]] && jq -e '(.findings|length)==0 and (.not_run|length)==0' "$CASE/old.json" >/dev/null; then
      printf 'DEMONSTRATED GAP: old audit accepted %s with matching receipt/lock\n' "$attack"; gaps=$((gaps+1))
    else printf 'Probe did not demonstrate %s (exit %s)\n' "$attack" "$rc"; cat "$CASE/old.json" "$CASE/old.stderr"; fi
  done
  [[ "$gaps" == 2 ]] && exit 1
  exit 64
fi
[[ $# == 0 ]] || { echo 'Usage: pinned-reusable-sources-test.sh [--preimplementation-probe]' >&2; exit 64; }
[[ -f "$HELPER" ]] || { echo 'Missing helper; this is a prerequisite failure, not a successful rejection control.' >&2; exit 64; }

case_fixture good; check 'authenticated active source and exact receipt' clean
case_fixture historical; edit_manifest '.sources += [(.sources[0] | .role="retained-review" | .callers=[] | .path=".github/workflows/other.yml")]'; check 'explicit historical review can coexist with active source' clean
case_fixture unrelated-vendor; cat >> "$CASE/repo/.github/workflows/actions.lock" <<YAML
    'unrelated/action@v9':
        ref: 'v9'
        commit: 'sha1-$CHILD_SHA'
YAML
check 'unrelated vendor records are outside this bounded comparison' clean

case_fixture missing-cache; mv "$CASE/cache/org/reusable.git" "$CASE/cache/org/reusable.absent"; check 'missing authenticated cache is NOT RUN' notrun
case_fixture missing-commit; replace_pin "$ROOT_SHA" 9999999999999999999999999999999999999999; check 'absent pinned commit is NOT RUN' notrun
case_fixture wrong-blob; edit_manifest '.sources[0].blob="1111111111111111111111111111111111111111"'; check 'wrong source blob is fatal' fatal
case_fixture wrong-path; edit_manifest '.sources[0].path=".github/workflows/other.yml"'; check 'same blob at another path does not satisfy caller' fatal
case_fixture wrong-repository; edit_manifest '.sources[0].repository="other/reusable"'; check 'source from another repository cannot satisfy active root' fatal
case_fixture mutable-source; edit_manifest '.sources[0].commit="main"'; check 'source catalog requires full immutable SHA' fatal
case_fixture mutable-caller; sed "s/$ROOT_SHA/main/" "$CASE/repo/.github/workflows/test.yml" > "$CASE/wf"; mv "$CASE/wf" "$CASE/repo/.github/workflows/test.yml"; check 'active caller cannot drift to a branch' fatal
case_fixture omitted-active; edit_manifest '.sources=[]'; : > "$CASE/repo/receipts.tsv"; check 'omitted active root is fatal' fatal
case_fixture historical-only; edit_manifest '.sources[0].role="retained-review" | .sources[0].callers=[]'; check 'historical source cannot stand in for active root' fatal
case_fixture wrong-job; edit_manifest '.sources[0].callers[0].job="another"'; check 'exact caller job is bound' fatal
case_fixture wrong-workflow; edit_manifest '.sources[0].callers[0].workflow=".github/workflows/other.yml"'; check 'exact caller workflow is bound' fatal
case_fixture duplicate-source; edit_manifest '.sources += [.sources[0]]'; check 'duplicate source declarations are rejected' fatal
case_fixture dropped-source-edge
edit_manifest --arg root "$ROOT_KEY" '.sources[0].uses=[] | .dependencies[$root].uses=[]'
# Keep lock/catalog/receipts mutually consistent: only the authenticated blob
# reveals the omitted edge, so a receipt-to-lock-only checker cannot pass this.
cat > "$CASE/repo/.github/workflows/actions.lock" <<YAML
version: 'v0.0.3'
workflows:
    '.github/workflows/test.yml':
        - '$ROOT_KEY'
dependencies:
    '$ROOT_KEY':
        ref: '$ROOT_SHA'
        commit: 'sha1-$ROOT_SHA'
    '$CHILD_KEY':
        ref: 'v1'
        commit: 'sha1-$CHILD_SHA'
YAML
: > "$CASE/repo/receipts.tsv"
check 'authenticated source edge cannot disappear from consistent metadata' fatal
case_fixture extra-source-edge
extra="extra/action@$CHILD_SHA"
edit_manifest --arg root "$ROOT_KEY" --arg extra "$extra" --arg sha "$CHILD_SHA" '.sources[0].uses += [{kind:"step",ref:$extra}] | .dependencies[$root].uses += [$extra] | .dependencies[$extra]={ref:"v1",commit:("sha1-"+$sha)}'
awk -v child="            - '$CHILD_KEY'" -v extra="            - '$extra'" '{print} $0==child {print extra}' "$CASE/repo/.github/workflows/actions.lock" > "$CASE/l"; mv "$CASE/l" "$CASE/repo/.github/workflows/actions.lock"
cat >> "$CASE/repo/.github/workflows/actions.lock" <<YAML
    '$extra':
        ref: 'v1'
        commit: 'sha1-$CHILD_SHA'
YAML
printf 'org/reusable\t%s\t%s\n' "$ROOT_SHA" "$extra" >> "$CASE/repo/receipts.tsv"
check 'consistent metadata cannot invent an edge absent from authenticated blob' fatal
case_fixture wrong-edge-kind; edit_manifest '.sources[0].uses[0].kind="job"'; check 'job and step source references remain distinct' fatal
case_fixture missing-receipt; : > "$CASE/repo/receipts.tsv"; check 'missing receipt union is fatal despite valid source' fatal
case_fixture extra-receipt; printf 'org/reusable\t%s\textra/action@v1\n' "$ROOT_SHA" >> "$CASE/repo/receipts.tsv"; check 'retained receipt union cannot contain an extra edge' fatal
case_fixture wrong-receipt-sha; sed "s/$ROOT_SHA/$CHILD_SHA/" "$CASE/repo/receipts.tsv" > "$CASE/r"; mv "$CASE/r" "$CASE/repo/receipts.tsv"; check 'receipt for another source commit cannot satisfy union' fatal
case_fixture changed-child-pin; edit_manifest --arg child "$CHILD_KEY" '.dependencies[$child].commit="sha1-1111111111111111111111111111111111111111"'; check 'shared vendor immutable pin must match supplement' fatal
case_fixture changed-vendor-ref; sed "s/ref: 'v1'/ref: 'v2'/" "$CASE/repo/.github/workflows/actions.lock" > "$CASE/l"; mv "$CASE/l" "$CASE/repo/.github/workflows/actions.lock"; check 'changed shared vendor reviewed ref is fatal' fatal
case_fixture changed-vendor-edge; cat >> "$CASE/repo/.github/workflows/actions.lock" <<YAML
        uses:
            - '$ROOT_KEY'
YAML
check 'vendor and supplement edges must agree' fatal
case_fixture unresolved-child; edit_manifest --arg child "$CHILD_KEY" 'del(.dependencies[$child])'; awk -v key="    '$CHILD_KEY':" '$0==key {exit} {print}' "$CASE/repo/.github/workflows/actions.lock" > "$CASE/l"; mv "$CASE/l" "$CASE/repo/.github/workflows/actions.lock"; check 'omitted supplemental and vendor child is fatal' fatal
case_fixture dependency-cycle; edit_manifest --arg root "$ROOT_KEY" --arg child "$CHILD_KEY" '.dependencies[$child].uses=[$root]'; cat >> "$CASE/repo/.github/workflows/actions.lock" <<YAML
        uses:
            - '$ROOT_KEY'
YAML
check 'cyclic supplemental graph is rejected within time bound' fatal
case_fixture malformed-json; printf '{' > "$MANIFEST"; check 'malformed catalog is fatal' fatal
case_fixture bad-lock; printf 'version: bad\nworkflows: []\ndependencies: []\n' > "$CASE/repo/.github/workflows/actions.lock"; check 'malformed vendor lock cannot yield source verification' fatal
case_fixture duplicate-lock-field
awk '{print} $0=="        ref: '\''v1'\''" {print}' "$CASE/repo/.github/workflows/actions.lock" > "$CASE/l"; mv "$CASE/l" "$CASE/repo/.github/workflows/actions.lock"
check 'duplicate vendor scalar field cannot be certified by dump' fatal
case_fixture replacement; other="$(commit_tree "$CASE/cache/org/reusable.git" "$(git --git-dir="$CASE/cache/org/reusable.git" rev-parse "$ROOT_SHA^{tree}")" 'Replacement')"; git --git-dir="$CASE/cache/org/reusable.git" update-ref "refs/replace/$ROOT_SHA" "$other"; check 'replacement refs cannot authenticate source objects' fatal
case_fixture graft; mkdir -p "$CASE/cache/org/reusable.git/info"; printf '%s %s\n' "$ROOT_SHA" "$CHILD_SHA" > "$CASE/cache/org/reusable.git/info/grafts"; check 'grafts cannot authenticate source objects' fatal
case_fixture corrupt-object; object="$CASE/cache/org/reusable.git/objects/${ROOT_BLOB:0:2}/${ROOT_BLOB:2}"; chmod u+w "$object"; printf 'corrupted loose object\n' > "$object"; check 'tampered Git object bytes are fatal' fatal

# Preserve metadata shape from the real inventory; it is provenance text,
# never a replacement for the structured dependency record.
case_fixture original-lock-text
awk -v child="    '$CHILD_KEY':" 'seen && $0==child {exit} /^    / {seen=1} seen {print}' "$CASE/repo/.github/workflows/actions.lock" > "$CASE/original-record"
edit_manifest --rawfile record "$CASE/original-record" '.sources[0].original_lock_record=$record'
check 'original_lock_record YAML text remains metadata' clean
case_fixture supplemental-only
awk -v child="    '$CHILD_KEY':" '$0==child {exit} {print}' "$CASE/repo/.github/workflows/actions.lock" > "$CASE/l"; mv "$CASE/l" "$CASE/repo/.github/workflows/actions.lock"
check 'declared child can supplement absent vendor record' clean
case_fixture supplemental-ref-mismatch
awk -v child="    '$CHILD_KEY':" '$0==child {exit} {print}' "$CASE/repo/.github/workflows/actions.lock" > "$CASE/l"; mv "$CASE/l" "$CASE/repo/.github/workflows/actions.lock"
edit_manifest --arg child "$CHILD_KEY" '.dependencies[$child].ref="1111111111111111111111111111111111111111"'
check 'injected full-SHA ref cannot disagree with its commit' fatal
case_fixture case-overlap
sed 's/org\/child@/ORG\/CHILD@/g' "$CASE/repo/.github/workflows/actions.lock" > "$CASE/l"; mv "$CASE/l" "$CASE/repo/.github/workflows/actions.lock"
edit_manifest --arg child "$CHILD_KEY" '.dependencies[$child].ref="v2"'
check 'case-folded vendor overlap still detects disagreement' fatal
case_fixture case-collision
edit_manifest --arg child "$CHILD_KEY" --arg upper "ORG/CHILD@$CHILD_SHA" '.dependencies[$upper]=.dependencies[$child]'
check 'case-normalized supplemental key collision is rejected' fatal
case_fixture duplicate-json-key
sed 's/"version": 1,/"version": 1, "version": 1,/' "$MANIFEST" > "$CASE/duplicate.json"; mv "$CASE/duplicate.json" "$MANIFEST"
check 'duplicate JSON mapping keys are rejected' fatal
case_fixture missing-tree
tree="$(git --git-dir="$CASE/cache/org/reusable.git" rev-parse "$ROOT_SHA^{tree}")"
rm "$CASE/cache/org/reusable.git/objects/${tree:0:2}/${tree:2}"
check 'incomplete cache missing a tree is NOT RUN' notrun
case_fixture missing-blob
rm "$CASE/cache/org/reusable.git/objects/${ROOT_BLOB:0:2}/${ROOT_BLOB:2}"
check 'incomplete cache missing a blob is NOT RUN' notrun
case_fixture corrupt-tree
tree="$(git --git-dir="$CASE/cache/org/reusable.git" rev-parse "$ROOT_SHA^{tree}")"
object="$CASE/cache/org/reusable.git/objects/${tree:0:2}/${tree:2}"; chmod u+w "$object"; printf 'corrupted tree\n' > "$object"
check 'tampered tree bytes are fatal' fatal
case_fixture corrupt-commit
object="$CASE/cache/org/reusable.git/objects/${ROOT_SHA:0:2}/${ROOT_SHA:2}"; chmod u+w "$object"; printf 'corrupted commit\n' > "$object"
check 'tampered commit bytes are fatal' fatal
# Unsupported source constructs must remain explicit unavailable evidence,
# rather than pretending local/docker dependencies were independently closed.
for variant in local-action local-reusable nested-reusable docker block-scalar alias merge flow cyclic-alias tagged-action; do
  case_fixture "$variant"
  case "$variant" in
    local-action) text=$'name: R\non: workflow_call\njobs:\n  build:\n    steps:\n      - uses: ./.github/actions/local'; raw='./.github/actions/local'; kind=step ;;
    local-reusable) text=$'name: R\non: workflow_call\njobs:\n  nested:\n    uses: ./.github/workflows/local.yml'; raw='./.github/workflows/local.yml'; kind=job ;;
    nested-reusable) text=$'name: R\non: workflow_call\njobs:\n  nested:\n    uses: org/child/.github/workflows/nested.yml@'"$CHILD_SHA"; raw="org/child/.github/workflows/nested.yml@$CHILD_SHA"; kind=job ;;
    alias) text=$'name: R\non: workflow_call\nshared: &shared\n  uses: org/child@'"$CHILD_SHA"$'\njobs:\n  build:\n    steps:\n      - *shared'; raw="$CHILD_KEY"; kind=step ;;
    merge) text=$'name: R\non: workflow_call\nshared: &shared\n  uses: org/child@'"$CHILD_SHA"$'\njobs:\n  build:\n    steps:\n      - <<: *shared'; raw="$CHILD_KEY"; kind=step ;;
    flow) text=$'name: R\non: workflow_call\njobs:\n  build:\n    steps:\n      - {uses: org/child@'"$CHILD_SHA"'}'; raw="$CHILD_KEY"; kind=step ;;
    cyclic-alias) text=$'name: R\non: workflow_call\ncycle: &cycle\n  self: *cycle\njobs:\n  build:\n    steps:\n      - uses: org/child@'"$CHILD_SHA"; raw="$CHILD_KEY"; kind=step ;;
    tagged-action) text=$'name: R\non: workflow_call\njobs:\n  build:\n    steps:\n      - uses: org/child@v1'; raw='org/child@v1'; kind=step ;;
    docker) text=$'name: R\non: workflow_call\njobs:\n  build:\n    steps:\n      - uses: docker://alpine:3'; raw='docker://alpine:3'; kind=step ;;
    block-scalar) text=$'name: R\non: workflow_call\njobs:\n  build:\n    steps:\n      - uses: >-\n          org/child@'"$CHILD_SHA"; raw="$CHILD_KEY"; kind=step ;;
  esac
  source_object "$CASE/cache/org/reusable.git" "$text"; replace_pin "$ROOT_SHA" "$NEW_COMMIT"
  edit_manifest --arg blob "$NEW_BLOB" --arg raw "$raw" --arg kind "$kind" '.sources[0].blob=$blob | .sources[0].uses=[{kind:$kind,ref:$raw}]'
  expected=notrun; [[ "$variant" != tagged-action ]] || expected=fatal
  check "$variant source evidence cannot claim closure" "$expected"
done
case_fixture symlink; source_object "$CASE/cache/org/reusable.git" 'other.yml' 120000; replace_pin "$ROOT_SHA" "$NEW_COMMIT"; edit_manifest --arg blob "$NEW_BLOB" '.sources[0].blob=$blob'; check 'Git symlink source is not authenticated as a workflow file' fatal
# Exercise the owning entrypoint too: a correct helper cannot protect callers
# if automatic catalog selection or source-cache forwarding is bypassed.
for variant in main-good main-wrong-blob; do
  case_fixture "$variant"
  [[ "$variant" != main-wrong-blob ]] || edit_manifest '.sources[0].blob="1111111111111111111111111111111111111111"'
  rc=0; valid=0
  timeout 15s bash "$ROOT/scripts/audit-lock-closure.sh" --json --repo-root "$CASE/repo" \
    --workflows "$CASE/repo/.github/workflows" --receipts "$CASE/repo/receipts.tsv" \
    --baseline "$CASE/repo/baseline.tsv" --source-cache "$CASE/cache" > "$CASE/main.json" 2> "$CASE/main.stderr" || rc=$?
  if [[ "$variant" == main-good && "$rc" == 0 ]]; then
    jq -e '(.findings|length)==0 and (.not_run|length)==0' "$CASE/main.json" >/dev/null 2>&1 && valid=1
  elif [[ "$variant" == main-wrong-blob && ( "$rc" == 1 || "$rc" == 3 ) ]]; then
    jq -e 'any(.findings[]; .severity=="FATAL" and (.rule|startswith("SOURCE-")))' "$CASE/main.json" >/dev/null 2>&1 && valid=1
  fi
  if [[ "$valid" == 1 ]]; then PASS=$((PASS+1)); printf 'PASS: %s entrypoint authenticates catalog (exit %s)\n' "$variant" "$rc"
  else FAIL=$((FAIL+1)); printf 'FAIL: %s entrypoint did not enforce source authentication (exit %s)\n' "$variant" "$rc"; cat "$CASE/main.json" "$CASE/main.stderr"; fi
done
printf '\n%d passed, %d failed\n' "$PASS" "$FAIL"
[[ "$FAIL" == 0 ]]