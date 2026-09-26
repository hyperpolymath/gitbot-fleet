#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Fetch pinned directive/AGENTIC blobs plus related guidance in priority repos.
# Usage: bash scripts/audit-directive-content.sh CENSUS_EVIDENCE_DIR
# Content is NEVER sourced/evaluated. Retrieval is NOT semantic approval.
set -euo pipefail
base="$(cd "${1:?census evidence directory required}" && pwd)"
mkdir -p "$base/blobs"
jq '[.[]|.repo as $repo|.files[]?|
  select(.kind=="bot_directive" or .kind=="agent_permissions" or
    ($repo|test("/(gitbot-fleet|hypatia|cicd-squabbler|echidnabot|echidna|proof-burrower|standards)$")))|
  select(.mode != "120000")|{repo:$repo,sha:.blob_sha,size,path}]|unique_by(.sha)' \
  "$base/census.json" > "$base/blob-tasks.json"
# Validate before constructing API paths or local filenames from the census.
jq -e 'all(.[]; (.repo|test("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")) and (.sha|test("^[a-f0-9]{40}$")))' "$base/blob-tasks.json" >/dev/null
export base
fetch_blob() {
    local repo="$1" sha="$2" dir="$base/blobs" status="unavailable" hash=""
    # Reuse only byte-verified cached content, never trust a filename alone.
    if [[ ! -f "$dir/$sha.txt" ]] || [[ "$(git hash-object --no-filters "$dir/$sha.txt")" != "$sha" ]]; then
        rm -f "$dir/$sha.txt"
        if gh api --method GET "repos/$repo/git/blobs/$sha" > "$dir/$sha.json" 2> "$dir/$sha.error"; then
            if jq -e '.encoding == "base64"' "$dir/$sha.json" >/dev/null; then
                jq -r .content "$dir/$sha.json" | base64 -d > "$dir/$sha.txt"
            fi
        fi
    fi
    if [[ -f "$dir/$sha.txt" ]]; then
        if [[ "$(git hash-object --no-filters "$dir/$sha.txt")" == "$sha" ]]; then
            status="retrieved_hash_verified"
            hash="$(sha256sum "$dir/$sha.txt" | cut -d' ' -f1)"
        else
            status="integrity_error"
            rm -f "$dir/$sha.txt"
        fi
    fi
    jq -n --arg sha "$sha" --arg status "$status" --arg hash "$hash" \
        '{key:$sha,value:{retrieval:$status,sha256:$hash,semantic_validation:"not_performed"}}' > "$dir/$sha.meta.json"
}
export -f fetch_blob
jq -r '.[]|[.repo,.sha]|@tsv' "$base/blob-tasks.json" | xargs -P 6 -n2 bash -c 'set -euo pipefail; fetch_blob "$1" "$2"' _
# Aggregate only this invocation's requested tasks, not stale metadata.
while IFS= read -r sha; do cat "$base/blobs/$sha.meta.json"; done < <(jq -r '.[].sha' "$base/blob-tasks.json") |
    jq -s 'from_entries' > "$base/content-index.json"
jq 'to_entries|group_by(.value.retrieval)|map({status:.[0].value.retrieval,count:length})' "$base/content-index.json"
