#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Read-only GitHub tree census. Does not evaluate grants or execute repo code.
# Usage: bash scripts/audit-directive-census.sh REPO_LIST_JSON OUTPUT_DIR
# Input: gh repo list OWNER --limit 2000 --json name,isPrivate,isArchived,defaultBranchRef,url
# Set CENSUS_OWNER for another owner. Raw evidence belongs outside tracked source.
set -euo pipefail
inventory="${1:?repository list JSON required}"
output="${2:?evidence output directory required}"
owner="${CENSUS_OWNER:-hyperpolymath}"
[[ "$owner" =~ ^[A-Za-z0-9-]+$ ]] || exit 2
mkdir -p "$output/repos"
output="$(cd "$output" && pwd)"
jq -e 'type == "array" and all(.[]; (.name | test("^[A-Za-z0-9_.-]+$")) and .name != "." and .name != "..")' "$inventory" >/dev/null
cp "$inventory" "$output/inventory.json"
export output owner
scan_repo() {
    local name="$1" dir="$output/repos/$1" sha
    mkdir -p "$dir"
    # Resolve a commit first; never fetch a moving branch for subsequent evidence.
    if ! gh api --method GET "repos/$owner/$name/commits/HEAD" --jq .sha > "$dir/commit.txt" 2> "$dir/error.txt"; then
        jq -n --arg repo "$owner/$name" '{repo:$repo,status:"commit_unavailable",authorization:"not_evaluated"}' > "$dir/result.json"
        return
    fi
    sha="$(cat "$dir/commit.txt")"
    if ! gh api --method GET "repos/$owner/$name/git/trees/$sha?recursive=1" > "$dir/tree.json" 2> "$dir/error.txt"; then
        jq -n --arg repo "$owner/$name" --arg sha "$sha" '{repo:$repo,commit:$sha,status:"tree_unavailable",authorization:"not_evaluated"}' > "$dir/result.json"
        return
    fi
    jq --arg repo "$owner/$name" --arg sha "$sha" '
      def candidate:
        test("(^|/)(\\.?bot[_-]directives|\\.gitbot-fleet)(/|$|\\.)";"i") or
        test("(^|/)(AGENTIC|AGENTS|CLAUDE|GEMINI|0-AI-MANIFEST|Trustfile|MUST|TRUST|INTENT|LICEN[CS]E-POLICY|SECURITY)\\.(a2ml|scm|md|adoc|contractile)$";"i") or
        test("(^|/)(copilot-instructions\\.md|[^/]+\\.instructions\\.md|CODEOWNERS|\\.botrc|\\.gitbot-fleet\\.(toml|yml|yaml|json))$";"i");
      {repo:$repo,commit:$sha,tree_sha:.sha,truncated:.truncated,
       status:(if .truncated then "partial_tree" else "tree_complete" end),
       authorization:"not_evaluated",
       files:[.tree[] | select(.type == "blob" and (.path | candidate)) |
         {path,blob_sha:.sha,size,mode,
          kind:(if (.path | test("(^|/)\\.?bot[_-]directives/";"i")) then "bot_directive"
            elif (.path | test("(^|/)AGENTIC\\.";"i")) then "agent_permissions"
            elif (.path | test("gitbot-fleet";"i")) then "fleet_config"
            else "related_guidance" end),
          scope:(if (.path | test("^(\\.machine_readable/|\\.bot_directives/|\\.claude/|\\.github/|[^/]+$)")) then "root_candidate" else "nested_or_legacy_review" end),
          content_review:"pending"}],
       submodules:[.tree[] | select(.type == "commit") | {path,sha}],
       workflow_paths:[.tree[] | select(.type == "blob" and (.path | startswith(".github/workflows/"))) | .path]}
    ' "$dir/tree.json" > "$dir/result.json"
    printf '%s %s\n' "$name" "$(jq -r '.status + " candidates=" + (.files|length|tostring)' "$dir/result.json")"
}
export -f scan_repo
jq -r '.[].name' "$inventory" | xargs -P 6 -n 1 bash -c 'set -euo pipefail; scan_repo "$1"' _
# Aggregate exactly the requested inventory, including dot-prefixed repos;
# never include stale results left by an earlier, larger census.
while IFS= read -r name; do cat "$output/repos/$name/result.json"; done < <(jq -r '.[].name' "$output/inventory.json") |
    jq -s 'sort_by(.repo)' > "$output/census.json"
jq '{repositories:length,status_counts:(group_by(.status)|map({key:.[0].status,value:length})|from_entries),candidate_files:([.[].files[]?]|length),authorization:"not_evaluated"}' "$output/census.json"
