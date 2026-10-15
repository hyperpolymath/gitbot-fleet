#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Acceptance check for RRA's documented runtime policy. No runtime is started.
# Run with: bash tests/runtime-policy-test.sh
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
policy="$root/robot-repo-automaton/.claude/CLAUDE.md"

section() {
    local heading="$1"
    awk -v heading="$heading" '
        $0 == heading { found = 1; next }
        found && /^### / { exit }
        found { print }
    ' "$policy"
}
allowed="$(section '### ALLOWED Languages & Tools')"
banned="$(section '### BANNED - Do Not Use')"

if [[ -z "$allowed" || -z "$banned" ]]; then
    echo 'FAIL: runtime policy must retain its allowed and banned sections' >&2
    exit 1
fi
if [[ "$(grep -cE '^\| \*\*Deno\*\* \|' <<< "$allowed")" != 1 ]] ||
    ! grep -Eq '^\| \*\*Deno\*\* \| JS runtime only \(tier 2\) \| Secondary to Bun; limited to running compiled ESM/JS\.' <<< "$allowed"; then
    echo 'FAIL: Deno must be allowed once as the secondary tier 2 runtime' >&2
    exit 1
fi
echo 'PASS: Deno is allowed as the secondary tier 2 runtime'
if ! grep -Eq '^\| \*\*Deno\*\* \|[^|]*\|[^|]*Use Bun for all JavaScript dependency installation and management\.' <<< "$allowed"; then
    echo 'FAIL: Deno must delegate JavaScript dependency installation and management to Bun' >&2
    exit 1
fi
echo 'PASS: JavaScript dependency installation and management remain with Bun'
if ! grep -Eq '^\| \*\*Bun\*\* \| JS runtime & package management \(tier 1\) \|' <<< "$allowed"; then
    echo 'FAIL: Bun must remain the primary tier 1 runtime' >&2
    exit 1
fi
echo 'PASS: Bun remains the primary tier 1 runtime'
# Regression: adding Deno to ALLOWED alone leaves contradictory instructions.
if grep -Eiq '^\|[[:space:]*`]*Deno[[:space:]*`]*\|' <<< "$banned"; then
    echo 'FAIL: Deno must not also appear in the banned runtime table' >&2
    exit 1
fi
echo 'PASS: the banned table does not contradict the Deno allowance'
