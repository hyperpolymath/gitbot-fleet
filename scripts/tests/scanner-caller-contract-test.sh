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

# Each mutation starts from the reviewed caller, so a rejection cannot be
# caused by an unrelated invalid field left over from another case.
sed -i 's/pull_request:/pull_request_target:/' "$wf"; reject 'privileged PR trigger'
sed -i 's/branches: \[main\]/branches: [develop]/' "$wf"; reject 'changed push branch'
sed -i 's/fast-pr: false/fast-pr: "false"/' "$wf"; reject 'string scan policy'
sed -i '/fast-pr: false/d' "$wf"; reject 'missing full-history policy'
sed -i '/fast-pr: false/a\      extra-input: true' "$wf"; reject 'unreviewed suite input'
sed -i '/fast-pr: false/a\      fast-pr: false' "$wf"; reject 'duplicate nested policy key'
sed -i '/^  scan:/a\    if: false' "$wf"; reject 'disabled scanner job'
printf '\n  additional:\n    runs-on: ubuntu-latest\n    steps: []\n' >> "$wf"; reject 'additional job'
printf '\npermissions: {contents: read}\n' >> "$wf"; reject 'duplicate root permissions'
printf '\nextra: &value {key: value}\nreference: *value\n' >> "$wf"; reject 'YAML alias'
printf '\n? [complex, key]\n: value\n' >> "$wf"; reject 'non-scalar YAML key'
printf '\nextra: [unterminated\n' >> "$wf"; reject 'malformed YAML'
printf '' > "$wf"; reject 'empty workflow'
printf '[]\n' > "$wf"; reject 'sequence workflow'

# Quoting the YAML 1.1 boolean-looking key must preserve GitHub's semantics.
sed -i 's/^on:/"on":/' "$wf"
ruby "$check" "$wf"
echo 'scanner caller parser and policy boundary controls passed'
