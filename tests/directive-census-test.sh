#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Offline fixtures; no actual GitHub API requests.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/bin"
export CENSUS_FIXTURE="$tmp"
printf 'deny = ["no automatic edits"]\r\n' > "$tmp/content"
sha="$(git hash-object --no-filters "$tmp/content")"
export CENSUS_BLOB="$sha"
jq -n --arg content "$(base64 -w0 "$tmp/content")" '{encoding:"base64",content:$content}' > "$tmp/blob.json"
cat > "$tmp/bin/gh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
[[ "$1 $2 $3" == 'api --method GET' ]] || exit 99
endpoint="$4"
printf '%s\n' "$endpoint" >> "$CENSUS_FIXTURE/requests"
case "$endpoint" in
  repos/hyperpolymath/broken/commits/HEAD) echo 'unavailable' >&2; exit 1 ;;
  */commits/HEAD) printf '%040d\n' 1 ;;
  */git/trees/0000000000000000000000000000000000000001\?recursive=1)
    partial=false
    [[ "$endpoint" != *'/partial/'* ]] || partial=true
    jq -n --arg sha "$CENSUS_BLOB" --argjson partial "$partial" '{sha:"fixture-tree",truncated:$partial,tree:[
      {type:"blob",mode:"100644",path:".machine_readable/bot_directives/rra.a2ml",sha:$sha,size:30},
      {type:"blob",mode:"100644",path:"nested/.bot_directives/rra.scm",sha:$sha,size:30},
      {type:"blob",mode:"120000",path:"AGENTS.md",sha:"symlink-blob",size:10},
      {type:"commit",mode:"160000",path:"vendor/component",sha:"submodule"}]}' ;;
  */git/blobs/*) cat "$CENSUS_FIXTURE/blob.json" ;;
  *) echo "Unexpected API path $endpoint" >&2; exit 98 ;;
esac
EOF
chmod +x "$tmp/bin/gh"
export PATH="$tmp/bin:$PATH"
printf '%s\n' '[{"name":".github"},{"name":"partial"},{"name":"broken"}]' > "$tmp/inventory.json"
bash "$root/scripts/audit-directive-census.sh" "$tmp/inventory.json" "$tmp/output" >/dev/null
jq -e 'length == 3 and any(.[]; .repo=="hyperpolymath/.github" and .status=="tree_complete") and any(.[];.status=="partial_tree") and any(.[];.status=="commit_unavailable") and all(.[];.authorization=="not_evaluated")' "$tmp/output/census.json" >/dev/null
bash "$root/scripts/audit-directive-content.sh" "$tmp/output" >/dev/null
jq -e --arg sha "$sha" 'length==1 and .[$sha].retrieval=="retrieved_hash_verified" and .[$sha].semantic_validation=="not_performed"' "$tmp/output/content-index.json" >/dev/null
cmp "$tmp/content" "$tmp/output/blobs/$sha.txt"
[[ "$(grep -c /git/blobs/ "$tmp/requests")" == 1 ]]
echo 'PASS: GET-only pinned census, hidden repo, incomplete tree, unavailable repo, nested directives, submodule/symlink metadata, deduplication and exact CRLF content'
