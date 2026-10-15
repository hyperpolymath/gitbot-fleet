#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Acceptance checks for the documented contract, not runtime enforcement.
# Offline: reads documentation only; never calls GitHub or enables the fleet.
# Run with: bash tests/review-thread-stewardship-test.sh
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Limit assertions to the relevant section so another bot or metarule cannot
# accidentally satisfy them. Collapse wrapping without snapshotting whole docs.
section() {
    local file="$1" heading="$2" level="$3"
    awk -v heading="$heading" -v level="$level" '
        $0 == heading { found = 1; next }
        found && $0 ~ "^" level " " { exit }
        found { print }
    ' "$file"
}
normalize() { awk '{$1=$1; printf "%s ", $0}'; }
require() {
    local label="$1" pattern="$2" content="$3"
    if ! grep -Eq -- "$pattern" <<< "$content"; then
        printf 'FAIL: %s\n' "$label" >&2
        exit 1
    fi
    printf 'PASS: %s\n' "$label"
}

operations="$(section "$root/docs/BOT-OPERATIONS.adoc" \
    '==== Review-Thread Stewardship (metarule)' '====')"
tokens="$(section "$root/docs/TOKEN-ARCHITECTURE.adoc" \
    '=== 3. echidnabot' '===')"

# Keep each guard in its own numbered step; a disclaimer elsewhere must not
# conceal a weakened matching, resolution, or invalid-finding instruction.
step() {
    local number="$1"
    awk -v number="$number" '
        /^[0-9]+\. / {
            if (found) exit
            if ($1 == number ".") found = 1
        }
        found && /^\.\.\.\.$/ { exit }
        found { print }
    ' <<< "$operations" | normalize
}
match_step="$(step 1)"
fix_step="$(step 2)"
reply_step="$(step 3)"
resolve_step="$(step 4)"
invalid_step="$(step 5)"

require 'candidate filtering uses the same PR, file/line class and unresolved threads' \
    'same PR and file/line class only to filter candidate unresolved' "$match_step"
require 'candidate filtering requires a verified finding-to-thread match' \
    'Verify the finding-to-thread match' "$match_step"
require 'reply and resolution require exactly one match' \
    'exactly one match before replying or resolving' "$match_step"
require 'zero or ambiguous matches leave every thread unchanged' \
    'Leave all threads unchanged if zero or multiple matches' "$match_step"
require 'the executor lands the fix on the PR branch' \
    'fix lands on the PR branch \(robot-repo-automaton' "$fix_step"
require 'identity findings cannot be automatically fixed' \
    'HYP-S010 are auto_fixable=false' "$fix_step"
require 'identity direction belongs to the owner' \
    'bot proposes and replies, the owner rules on direction' "$fix_step"
require 'echidnabot replies with both the fix commit and reasoning' \
    'echidnabot replies to the thread with the fix commit and reasoning' "$reply_step"

# REST takes a top-level comment ID; GraphQL takes a review-thread node ID.
reply_endpoint='POST /repos/{owner}/{repo}/pulls/{pr}/comments/{id}/replies'
require 'operations document the REST reply endpoint' \
    "$(sed 's/[][\\.^$*+?(){}|]/\\&/g' <<< "$reply_endpoint")" "$reply_step"
require 'REST replies use the top-level review-comment ID' \
    'where \{id\} is the top-level review-comment ID' "$reply_step"
require 'resolution follows both a landed fix and a posted reply' \
    "only after the required fix lands and Step 3's reply is posted" "$resolve_step"
require 'resolution uses a GraphQL thread ID from the PR' \
    "GraphQL mutation resolveReviewThread \(thread id from the PR's reviewThreads\)" "$resolve_step"
require 'identity threads wait for owner direction, approval and a landed fix' \
    'HYP-S010 stay open until the owner directs and approves a fix and that fix lands' "$resolve_step"
require 'invalid findings take the skip path instead of normal resolution' \
    'For verified-invalid findings, follow Step 5 instead' "$resolve_step"
require 'unaddressed findings must never be resolved' \
    'Never resolve a thread whose finding is not addressed' "$invalid_step"
require 'invalid findings receive a skip explanation and remain unresolved' \
    'verified as no-longer-valid, post a "skipped with a brief reason" reply explaining why and leave the thread unresolved' "$invalid_step"
require 'alignment findings require checking for template residue first' \
    'check which side is the residue before automating' "$(normalize <<< "$operations")"

reply_row="$(awk '/^\| Reply to a review thread \|/ {print}' <<< "$tokens")"
resolve_row="$(awk '/^\| Resolve a review thread \|/ {print}' <<< "$tokens")"
require 'echidnabot owns exactly one reply action and one resolve action' \
    '^1 1$' "$(grep -c '^| Reply to a review thread |' <<< "$tokens") $(grep -c '^| Resolve a review thread |' <<< "$tokens")"
require 'token docs use the same REST endpoint as operations' \
    "$(sed 's/[][\\.^$*+?(){}|]/\\&/g' <<< "${reply_endpoint//\{id\}/\{comment_id\}}")" "$reply_row"
require 'token docs distinguish REST comment IDs from GraphQL thread IDs' \
    'top-level review-comment ID, distinct from the GraphQL review-thread ID' "$reply_row"
require 'review replies require no additional token scope' \
    'no new scope, `repo` covers it' "$reply_row"
require 'token docs name the GraphQL mutation and threadId argument' \
    'mutation resolveReviewThread\(input: \{threadId\}\)' "$resolve_row"
require 'token docs restrict resolution to addressed findings after the reply' \
    'resolves only threads whose finding the fleet addressed, with the fix-commit reply posted first' "$resolve_row"
require 'operations link to the token permission document' \
    'link:TOKEN-ARCHITECTURE\.adoc\[' "$(normalize <<< "$operations")"
require 'token reply action refers back to the operations contract' \
    'BOT-OPERATIONS\.adoc' "$reply_row"
