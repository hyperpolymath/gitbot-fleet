#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Auto-generated fix script

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="$2"

bash "/var/mnt/eclipse/repos/gitbot-fleet/scripts/fix-cors-wildcard.sh" "$REPO_PATH" "/var/mnt/eclipse/repos/gitbot-fleet/shared-context/fix-batches/vordr-20260206-214126/finding-cors_misconfiguration-2.json"
