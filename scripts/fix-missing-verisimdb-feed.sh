#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-missing-verisimdb-feed.sh — Add VeriSimDB feed integration if missing
#
# Fixes MissingVerisimdbFeed by creating .machine_readable/integrations/verisimdb.a2ml
# declaring the VeriSimDB cross-repo analytics data feed configuration.
#
# Usage: fix-missing-verisimdb-feed.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

TARGET_DIR="$REPO_PATH/.machine_readable/integrations"
TARGET_FILE="$TARGET_DIR/verisimdb.a2ml"

echo "=== Missing VeriSimDB Feed Fix ==="
echo "  Repo: $REPO_PATH"

# Idempotency: skip if already exists
if [[ -f "$TARGET_FILE" ]]; then
    echo "  SKIP: verisimdb.a2ml already exists"
    exit 0
fi

# Create parent directories
mkdir -p "$TARGET_DIR"

# Write template content
cat > "$TARGET_FILE" <<'VERISIMDB'
# SPDX-License-Identifier: PMPL-1.0-or-later
# VeriSimDB Feed — Cross-Repo Analytics Data Store

[integration]
name = "verisimdb"
type = "data-feed"
repository = "https://github.com/hyperpolymath/nextgen-databases"
data-store = "verisimdb-data"

[feed-config]
emit-scan-results = true
emit-build-metrics = true
emit-dependency-graph = true
format = "hexad"
destination = "verisimdb-data/feeds/"
VERISIMDB

echo "  CREATED .machine_readable/integrations/verisimdb.a2ml"
echo ""
echo "Fixed: 1 file created"
exit 0
