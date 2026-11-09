#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
wf="$tmp/scanner.yml"
check="$root/scripts/check-scanner-caller.rb"
cp "$root/.github/workflows/secret-scanner.yml" "$wf"
ruby "$check" "$wf"
reject() {
  local name=$1
  if ruby "$check" "$wf" >/dev/null 2>&1; then
    echo "FAIL: accepted $name" >&2; exit 1
  fi
  echo "PASS: rejected $name"
  cp "$root/.github/workflows/secret-scanner.yml" "$wf"
}
sed -i 's/fast-pr: false/fast-pr: true/' "$wf"; reject 'fast PR scanning'
printf '    secrets: inherit\n' >> "$wf"; reject 'inherited secrets'
sed -i 's/contents: read/contents: write/' "$wf"; reject 'excess permissions'
sed -i 's/03e858b6015e67cc1f9bc31ac1e29ddc1b6fd409/main/' "$wf"; reject 'mutable suite ref'
printf '\n  scan:\n    uses: bad/repo/.github/workflows/bad.yml@main\n' >> "$wf"; reject 'duplicate job'
echo 'scanner caller negative controls passed'
