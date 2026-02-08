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
REPO_SLUG=$(echo "$REPO_NAME" | tr '/' '-')
TARGET_FILE="shared-context/findings/${REPO_SLUG}/${TIMESTAMP}.json"

# Clone or update fleet repo
FLEET_DIR="/tmp/gitbot-fleet-$$"
trap "rm -rf $FLEET_DIR" EXIT

if [ -n "${GITHUB_TOKEN:-}" ]; then
    git clone "https://x-access-token:${GITHUB_TOKEN}@github.com/${FLEET_REPO}.git" "$FLEET_DIR"
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
   '. + {
       submission_metadata: {
           repo: $repo,
           commit: $commit,
           submitted_at: $timestamp,
           scanner_version: "hypatia-v2"
       }
   }' "$FINDING_FILE" > "$TARGET_FILE"

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

# Trigger fleet processing (if webhook exists)
if [ -n "${FLEET_WEBHOOK_URL:-}" ]; then
    curl -X POST "$FLEET_WEBHOOK_URL" \
        -H "Content-Type: application/json" \
        -d "{\"repo\": \"$REPO_NAME\", \"findings\": $FINDING_COUNT}"
    echo "🔔 Triggered fleet processing"
fi
