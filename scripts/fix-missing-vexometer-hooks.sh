#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-missing-vexometer-hooks.sh — Add vexometer integration if missing
#
# Fixes MissingVexometerHooks by creating
# .machine_readable/integrations/vexometer.a2ml declaring the
# vexometer friction measurement hooks.
#
# Usage: fix-missing-vexometer-hooks.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

TARGET_DIR="$REPO_PATH/.machine_readable/integrations"
TARGET_FILE="$TARGET_DIR/vexometer.a2ml"

echo "=== Missing Vexometer Hooks Fix ==="
echo "  Repo: $REPO_PATH"

# Idempotency: skip if already exists
if [[ -f "$TARGET_FILE" ]]; then
    echo "  SKIP: vexometer.a2ml already exists"
    exit 0
fi

# Create parent directories
mkdir -p "$TARGET_DIR"

# Write template content
cat > "$TARGET_FILE" <<'VEXOMETER'
# SPDX-License-Identifier: PMPL-1.0-or-later
# Vexometer Integration — Irritation Surface Analysis

[integration]
name = "vexometer"
type = "friction-measurement"
repository = "https://github.com/hyperpolymath/vexometer"

[measurement-config]
dimensions = 10
emit-isa-reports = true
lazy-eliminator = true
satellite-interventions = true

[hooks]
cli-tools = "measure-on-error"
ui-panels = "measure-on-interaction"
build-failures = "measure-on-failure"
VEXOMETER

echo "  CREATED .machine_readable/integrations/vexometer.a2ml"
echo ""
echo "Fixed: 1 file created"
exit 0
