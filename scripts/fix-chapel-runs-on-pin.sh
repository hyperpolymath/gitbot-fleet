#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-chapel-runs-on-pin.sh — pin runs-on: ubuntu-22.04 on any Chapel-using job
#
# Driven by hypatia WF024 (chapel-runs-on-pin-22-04 recipe). The Chapel-2.8.0
# .deb (chapel-X.Y.Z-N.ubuntu22.amd64.deb) is ABI-linked to libclang-cpp.so.14
# / LLVM-14 — only present on Ubuntu 22.04. ubuntu-latest / ubuntu-24.04
# resolves the unmet dep with libclang-cpp-18 → `chpl` exit 127 at startup.
#
# Idempotent: skips files already pinned to ubuntu-22.04.

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="${2:-}"

cd "$REPO_PATH"

CHANGED=0
for f in $(find .github/workflows -name '*.yml' -o -name '*.yaml' 2>/dev/null); do
  # Only patch files that actually reference Chapel
  if ! grep -qE 'CHAPEL_DEB_URL|chapel-[0-9]+\.[0-9]+\.[0-9]+-[0-9]+\.ubuntu22|chpl --(version|about)|printchplenv' "$f"; then
    continue
  fi

  if grep -qE '^[[:space:]]+runs-on:[[:space:]]+ubuntu-latest' "$f"; then
    sed -i -E 's|^([[:space:]]+)runs-on:[[:space:]]+ubuntu-latest|\1runs-on: ubuntu-22.04|' "$f"
    echo "  pinned ubuntu-22.04: $f"
    CHANGED=$((CHANGED + 1))
  fi

  # Also normalise the apt install pattern (declarative resolution beats dpkg+fix loop)
  if grep -qF 'sudo dpkg -i /tmp/chapel.deb || sudo apt-get install -f -y' "$f"; then
    sed -i 's|sudo dpkg -i /tmp/chapel.deb || sudo apt-get install -f -y|sudo apt-get install -y /tmp/chapel.deb|' "$f"
    echo "  apt-get install replaces dpkg fallback: $f"
    CHANGED=$((CHANGED + 1))
  fi
done

echo "fix-chapel-runs-on-pin: $CHANGED change(s) applied in $REPO_PATH"
exit 0
