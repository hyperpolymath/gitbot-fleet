#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Auto-generated fix script

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="$2"

bash "$FLEET_DIR/scripts/fix-cors-wildcard.sh" "$REPO_PATH" "$FINDING_FILE"
