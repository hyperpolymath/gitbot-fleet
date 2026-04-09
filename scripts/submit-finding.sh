#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Submit Hypatia findings to gitbot-fleet shared-context

set -euo pipefail

# Configuration
FLEET_REPO="${FLEET_REPO:-hyperpolymath/gitbot-fleet}"
FINDINGS_BRANCH="${FINDINGS_BRANCH:-findings-submissions}"
FINDING_FILE="$1"
REPO_NAME="${GITHUB_REPOSITORY:-unknown/unknown}"
COMMIT_SHA="${GITHUB_SHA:-unknown}"
PUSH_TOKEN="${FLEET_PUSH_TOKEN:-${GITHUB_TOKEN:-}}"
DISPATCH_TOKEN="${FLEET_DISPATCH_TOKEN:-$PUSH_TOKEN}"

# Validate FLEET_REPO format (must be owner/repo, no path traversal)
if [[ ! "$FLEET_REPO" =~ ^[a-zA-Z0-9._-]+/[a-zA-Z0-9._-]+$ ]]; then
    echo "Error: Invalid FLEET_REPO format: $FLEET_REPO (expected owner/repo)"
    exit 1
fi

# Validate input
if [ ! -f "$FINDING_FILE" ]; then
    echo "Error: Finding file not found: $FINDING_FILE"
    exit 1
fi

# Validate JSON
if ! jq empty "$FINDING_FILE" 2>/dev/null; then
    echo "Error: Invalid JSON in findings file"
    exit 1
fi

echo "📤 Submitting findings from $REPO_NAME to gitbot-fleet..."

# Create timestamped filename
TIMESTAMP=$(date -u +"%Y%m%d-%H%M%S")
# Sanitize repo slug: replace / with -, strip any path traversal or special chars
REPO_SLUG=$(echo "$REPO_NAME" | tr '/' '-' | tr -cd 'a-zA-Z0-9._-')
TARGET_FILE="shared-context/findings/${REPO_SLUG}/${TIMESTAMP}.json"

# Clone or update fleet repo
FLEET_DIR="/tmp/gitbot-fleet-$$"
trap 'rm -rf "$FLEET_DIR"' EXIT

if [ -n "$PUSH_TOKEN" ]; then
    git clone "https://x-access-token:${PUSH_TOKEN}@github.com/${FLEET_REPO}.git" "$FLEET_DIR"
else
    git clone "git@github.com:${FLEET_REPO}.git" "$FLEET_DIR"
fi

cd "$FLEET_DIR"

# Create or switch to findings branch
git checkout "$FINDINGS_BRANCH" 2>/dev/null || git checkout -b "$FINDINGS_BRANCH"

# Create directory structure
mkdir -p "$(dirname "$TARGET_FILE")"

# Add metadata to finding
jq --arg repo "$REPO_NAME" \
   --arg commit "$COMMIT_SHA" \
   --arg timestamp "$(date -u +"%Y-%m-%dT%H:%M:%SZ")" \
   '
   def submission_meta: {
       repo: $repo,
       commit: $commit,
       submitted_at: $timestamp,
       scanner_version: "hypatia-v2"
   };

   # Normalize input shape:
   # - array    -> {findings: [...]}
   # - object with findings[] -> keep findings
   # - single finding object  -> wrap in findings[]
   if type == "array" then
       {findings: ., submission_metadata: submission_meta}
   elif type == "object" and (has("findings")) and (.findings | type == "array") then
       . + {submission_metadata: submission_meta}
   elif type == "object" then
       {findings: [.] , submission_metadata: submission_meta}
   else
       error("Unsupported findings JSON shape")
   end
   ' "$FINDING_FILE" > "$TARGET_FILE"

# Create/update latest symlink
ln -sf "$(basename "$TARGET_FILE")" "shared-context/findings/${REPO_SLUG}/latest.json"

# Count findings
FINDING_COUNT=$(jq '.findings | length' "$TARGET_FILE" 2>/dev/null || echo 0)

# Commit and push
git add "$TARGET_FILE" "shared-context/findings/${REPO_SLUG}/latest.json"
git config user.name "Hypatia Finding Submitter"
git config user.email "hypatia@reposystem.dev"

git commit -m "findings: $REPO_NAME @ $(date +%Y-%m-%d)

Submitted: $FINDING_COUNT findings
Commit: $COMMIT_SHA
Scanner: hypatia-v2

Automated submission from GitHub Actions."

git push origin "$FINDINGS_BRANCH"

echo "✅ Successfully submitted $FINDING_COUNT findings"
echo "📍 Location: $TARGET_FILE"
echo "🌐 View: https://github.com/${FLEET_REPO}/blob/${FINDINGS_BRANCH}/${TARGET_FILE}"

# Trigger repository_dispatch for fleet intake when token is available.
if [ -n "$DISPATCH_TOKEN" ]; then
    CRITICAL_COUNT=$(jq '[.findings[]? | select((.severity // "") | ascii_downcase == "critical")] | length' "$TARGET_FILE")
    HIGH_COUNT=$(jq '[.findings[]? | select((.severity // "") | ascii_downcase == "high")] | length' "$TARGET_FILE")
    SECRET_COUNT=$(jq '[.findings[]? | select((((.type // "") + " " + (.rule_id // "") + " " + (.message // "")) | ascii_downcase | test("secret|token|private[_-]?key|credential")))] | length' "$TARGET_FILE")

    EVENT_TYPE="hypatia-general-findings"
    if [ "$SECRET_COUNT" -gt 0 ]; then
        EVENT_TYPE="hypatia-secret-alert"
    elif [ "$CRITICAL_COUNT" -gt 0 ] || [ "$HIGH_COUNT" -gt 0 ]; then
        EVENT_TYPE="hypatia-security-alert"
    fi

    dispatch_payload=$(jq -n \
        --arg event_type "$EVENT_TYPE" \
        --arg source_repo "$REPO_NAME" \
        --arg sha "$COMMIT_SHA" \
        --arg branch "$FINDINGS_BRANCH" \
        --arg findings_path "$TARGET_FILE" \
        --arg submitted_at "$(date -u +"%Y-%m-%dT%H:%M:%SZ")" \
        --argjson findings_count "$FINDING_COUNT" \
        --argjson critical "$CRITICAL_COUNT" \
        --argjson high "$HIGH_COUNT" \
        --argjson secret_like "$SECRET_COUNT" \
        '{
          event_type: $event_type,
          client_payload: {
            source_repo: $source_repo,
            sha: $sha,
            findings_count: $findings_count,
            critical: $critical,
            high: $high,
            secret_like: $secret_like,
            findings_branch: $branch,
            findings_path: $findings_path,
            submitted_at: $submitted_at
          }
        }')

    if curl -fsS -X POST "https://api.github.com/repos/${FLEET_REPO}/dispatches" \
        -H "Accept: application/vnd.github+json" \
        -H "Authorization: Bearer ${DISPATCH_TOKEN}" \
        -H "X-GitHub-Api-Version: 2022-11-28" \
        -d "$dispatch_payload" >/dev/null; then
        echo "✅ Triggered repository_dispatch (${EVENT_TYPE}) for ${FLEET_REPO}"
    else
        echo "⚠️  repository_dispatch failed (push already completed)"
    fi
fi

# Backward-compatible webhook trigger (optional).
if [ -n "${FLEET_WEBHOOK_URL:-}" ]; then
    payload=$(jq -n --arg repo "$REPO_NAME" --argjson findings "${FINDING_COUNT:-0}" \
        '{repo: $repo, findings: $findings}')
    curl -fsS -X POST "$FLEET_WEBHOOK_URL" \
        -H "Content-Type: application/json" \
        -d "$payload" >/dev/null
    echo "Triggered fleet webhook"
fi
