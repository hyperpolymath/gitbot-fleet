#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-missing-feedback-integration.sh — Add feedback-o-tron integration if missing
#
# Fixes MissingFeedbackIntegration by creating
# .machine_readable/integrations/feedback-o-tron.a2ml declaring the
# autonomous upstream bug reporting integration.
#
# Usage: fix-missing-feedback-integration.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

TARGET_DIR="$REPO_PATH/.machine_readable/integrations"
TARGET_FILE="$TARGET_DIR/feedback-o-tron.a2ml"

echo "=== Missing Feedback Integration Fix ==="
echo "  Repo: $REPO_PATH"

# Idempotency: skip if already exists
if [[ -f "$TARGET_FILE" ]]; then
    echo "  SKIP: feedback-o-tron.a2ml already exists"
    exit 0
fi

# Create parent directories
mkdir -p "$TARGET_DIR"

# Write template content
cat > "$TARGET_FILE" <<'FEEDBACK'
# SPDX-License-Identifier: PMPL-1.0-or-later
# Feedback-o-Tron Integration — Autonomous Bug Reporting

[integration]
name = "feedback-o-tron"
type = "bug-reporter"
repository = "https://github.com/hyperpolymath/feedback-o-tron"

[reporting-config]
platforms = ["github", "gitlab", "bugzilla"]
deduplication = true
audit-logging = true
auto-file-upstream = "on-external-dependency-failure"
FEEDBACK

echo "  CREATED .machine_readable/integrations/feedback-o-tron.a2ml"
echo ""
echo "Fixed: 1 file created"
exit 0
