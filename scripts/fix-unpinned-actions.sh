#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Fix unpinned GitHub Actions by pinning to SHA

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="$2"

echo "Fixing unpinned actions in $REPO_PATH..."

# Common action SHA pins (updated 2025-12-15)
declare -A ACTION_PINS=(
    ["actions/checkout@v4"]="b4ffde65f46336ab88eb53be808477a3936bae11"
    ["actions/upload-artifact@v4"]="65c79d7f54e76e4e3c7a8f34db0f4ac8b515c478"
    ["github/codeql-action/init@v3"]="662472033e021d55d94146f66f6058822b0b39fd"
    ["github/codeql-action/analyze@v3"]="662472033e021d55d94146f66f6058822b0b39fd"
    ["github/codeql-action/upload-sarif@v3"]="662472033e021d55d94146f66f6058822b0b39fd"
    ["ossf/scorecard-action@v2.4.0"]="62b2cac7ed8198b15735ed49ab1e5cf35480ba46"
    ["trufflesecurity/trufflehog@v3"]="8a8ef8526528d8a4ff3e2c90be08e25ef8efbd9b"
    ["editorconfig-checker/action-editorconfig-checker@main"]="9f8f6065f4db902c0c56cafa67cea18b3ebbb680"
)

# Extract file path from finding
WORKFLOW_FILE=$(jq -r '.location.file' "$FINDING_FILE")
WORKFLOW_PATH="$REPO_PATH/$WORKFLOW_FILE"

if [[ ! -f "$WORKFLOW_PATH" ]]; then
    echo "ERROR: Workflow file not found: $WORKFLOW_PATH"
    exit 1
fi

# Pin actions to SHA
for action_version in "${!ACTION_PINS[@]}"; do
    SHA="${ACTION_PINS[$action_version]}"
    ACTION_NAME="${action_version%@*}"
    VERSION="${action_version#*@}"

    # Replace unpinned action with SHA-pinned version
    if grep -q "uses: ${action_version}" "$WORKFLOW_PATH"; then
        sed -i "s|uses: ${action_version}|uses: ${ACTION_NAME}@${SHA} # ${VERSION}|g" "$WORKFLOW_PATH"
        echo "  ✓ Pinned ${ACTION_NAME} to ${SHA}"
    fi
done

echo "✅ Unpinned actions fixed in $WORKFLOW_FILE"
