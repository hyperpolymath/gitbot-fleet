#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-close-obsolete-pr.sh — Close PRs whose proposed `uses: …@<sha>`
# is REGRESSIVE vs main's current value. ERR-PR-001.
#
# Root cause (observed 2026-05-27, 6 PRs): two pin-bump PRs land out
# of order — the newer (PR-B) merges first; the earlier (PR-A) still
# proposes the now-stale SHA. Rebasing PR-A reverts main; close-as-
# superseded is the correct action.

set -euo pipefail

URL="${1:?Usage: fix-close-obsolete-pr.sh <pr-url>}"
REPO_NWO=$(echo "$URL" | awk -F'/' '{print $4"/"$5}')

DIFF=$(gh pr diff "$URL" 2>/dev/null)
PR_USES=$(echo "$DIFF" | grep -E "^\+[[:space:]]+uses:[[:space:]]+[^@[:space:]]+@[0-9a-f]{40}" || true)
if [ -z "$PR_USES" ]; then
    echo "SKIP: PR has no `+ uses: …@<sha>` lines"
    exit 0
fi

TOUCHED=$(gh pr view "$URL" --json files --jq '.files[].path' | grep '^\.github/workflows/.*\.ya\?ml$' || true)
[ -z "$TOUCHED" ] && { echo "SKIP: no workflow files touched"; exit 0; }

OBSOLETE=0
for path in $TOUCHED; do
    main_content=$(gh api "repos/${REPO_NWO}/contents/${path}?ref=main" --jq '.content' 2>/dev/null | base64 -d 2>/dev/null || true)
    [ -z "$main_content" ] && continue
    while IFS= read -r pr_line; do
        action=$(echo "$pr_line" | sed -E 's/^\+[[:space:]]+uses:[[:space:]]+([^@[:space:]]+)@.*/\1/')
        pr_sha=$(echo "$pr_line" | sed -E 's/.*@([0-9a-f]{40}).*/\1/')
        main_sha=$(echo "$main_content" | grep -E "uses:[[:space:]]+${action}@[0-9a-f]{40}" | head -1 | sed -E 's/.*@([0-9a-f]{40}).*/\1/')
        if [ -n "$main_sha" ] && [ "$main_sha" != "$pr_sha" ]; then
            echo "OBSOLETE: $path  action=$action  PR=$pr_sha  main=$main_sha"
            OBSOLETE=1
        fi
    done <<< "$PR_USES"
done

if [ "$OBSOLETE" -eq 0 ]; then
    echo "OK: PR is not obsolete"
    exit 0
fi

gh pr close "$URL" --comment "Closed as superseded: main already carries a newer SHA at the workflow location(s) this PR proposes to update. Rebasing would regress main. Refile against current main if a future repin is needed. (ERR-PR-001 — auto-closed via fix-close-obsolete-pr.sh.)"
echo "CLOSED: $URL"
