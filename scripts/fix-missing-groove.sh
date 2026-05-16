#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-missing-groove.sh — Add Groove manifest to repos that serve HTTP but lack one
#
# Fixes HYP-DOG-008 (HTTP server without Groove endpoint) by generating a
# .well-known/groove/manifest.json using the groove CLI tool.
#
# This script is dispatched by seambot when Hypatia detects DOG-08 findings.
#
# Usage: fix-missing-groove.sh <repo-path> <finding-json>
#
# Requires: groove CLI (installed via `cargo install --path standards/groove-protocol/cli`)

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

MANIFEST_DIR="$REPO_PATH/.well-known/groove"
MANIFEST_FILE="$MANIFEST_DIR/manifest.json"

echo "=== Missing Groove Manifest Fix ==="
echo "  Repo: $REPO_PATH"

# Idempotency: skip if manifest already exists
if [[ -f "$MANIFEST_FILE" ]]; then
    echo "  SKIP: Groove manifest already exists"
    exit 0
fi

# Try using the groove CLI if available
if command -v groove &>/dev/null; then
    echo "  Using groove CLI to generate manifest..."
    groove init --path "$REPO_PATH" || {
        echo "  WARN: groove init failed — falling back to template"
    }
fi

# If groove didn't create it (not installed or failed), use a minimal template
if [[ ! -f "$MANIFEST_FILE" ]]; then
    echo "  Generating minimal Groove manifest template..."

    # Detect service name from repo directory or Cargo.toml
    SERVICE_ID=""
    if [[ -f "$REPO_PATH/Cargo.toml" ]]; then
        SERVICE_ID=$(grep '^name\s*=' "$REPO_PATH/Cargo.toml" | head -1 | sed 's/.*"\(.*\)".*/\1/')
    elif [[ -f "$REPO_PATH/mix.exs" ]]; then
        SERVICE_ID=$(grep 'app:' "$REPO_PATH/mix.exs" | head -1 | sed 's/.*:\([a-z_]*\).*/\1/')
    fi
    SERVICE_ID="${SERVICE_ID:-$(basename "$REPO_PATH")}"

    # Detect version
    VERSION="0.1.0"
    if [[ -f "$REPO_PATH/Cargo.toml" ]]; then
        VERSION=$(grep '^version\s*=' "$REPO_PATH/Cargo.toml" | head -1 | sed 's/.*"\(.*\)".*/\1/')
    fi

    mkdir -p "$MANIFEST_DIR"
    cat > "$MANIFEST_FILE" <<EOF
{
  "groove_version": "1",
  "service_id": "${SERVICE_ID}",
  "service_version": "${VERSION}",
  "capabilities": {
    "custom": {
      "type": "custom",
      "description": "TODO: describe this service's primary capability",
      "protocol": "http",
      "endpoint": "/api",
      "requires_auth": false,
      "panel_compatible": true
    }
  },
  "consumes": [],
  "endpoints": {
    "health": "/health"
  },
  "health": "/health",
  "applicability": ["individual", "team"]
}
EOF
fi

echo "  Created: $MANIFEST_FILE"
echo "  NOTE: Review and customise the manifest — replace 'custom' capability"
echo "        with the correct type from the Groove registry."
echo "  Run 'groove validate --path $REPO_PATH' to check compliance."
