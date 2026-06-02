#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Smoke test for scripts/propagate-sha-bump.sh.
#
# Exercises the four refusal paths + the DRY_RUN payload-composition path
# without dispatching anything. Does NOT hit the network — all calls
# return before the gh-search step (or in DRY_RUN, skip the dispatch).
set -euo pipefail

cd "$(dirname "$0")/.."

SCRIPT="scripts/propagate-sha-bump.sh"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

pass=0
fail=0

assert_exit() {
    local label="$1" expected_rc="$2"
    local actual_rc="$3"
    if [[ "$actual_rc" -eq "$expected_rc" ]]; then
        echo "ok    $label (rc=$actual_rc)"
        ((pass++))
    else
        echo "FAIL  $label (expected rc=$expected_rc got rc=$actual_rc)"
        ((fail++))
    fi
}

# 1. Wrong rule name → exit 1.
cat > "$tmp/wrong-rule.json" <<'EOF'
{
  "rule": "some_other_rule",
  "source_repo": "hyperpolymath/standards",
  "source_workflow": ".github/workflows/governance-reusable.yml",
  "old_sha": "0011223344556677889900112233445566778899",
  "new_sha": "abcdef0123456789abcdef0123456789abcdef01",
  "pr_title": "x",
  "pr_number": 1
}
EOF
set +e
"$SCRIPT" /ignored "$tmp/wrong-rule.json" >/dev/null 2>&1
assert_exit "wrong rule name → exit 1" 1 $?
set -e

# 2. Malformed SHA → exit 1.
cat > "$tmp/bad-sha.json" <<'EOF'
{
  "rule": "reusable_workflow_sha_bump_needs_propagation",
  "source_repo": "hyperpolymath/standards",
  "source_workflow": ".github/workflows/governance-reusable.yml",
  "old_sha": "not-a-sha",
  "new_sha": "abcdef0123456789abcdef0123456789abcdef01",
  "pr_title": "x",
  "pr_number": 1
}
EOF
set +e
"$SCRIPT" /ignored "$tmp/bad-sha.json" >/dev/null 2>&1
assert_exit "malformed SHA → exit 1" 1 $?
set -e

# 3. old_sha == new_sha → exit 1.
cat > "$tmp/same-sha.json" <<'EOF'
{
  "rule": "reusable_workflow_sha_bump_needs_propagation",
  "source_repo": "hyperpolymath/standards",
  "source_workflow": ".github/workflows/governance-reusable.yml",
  "old_sha": "abcdef0123456789abcdef0123456789abcdef01",
  "new_sha": "abcdef0123456789abcdef0123456789abcdef01",
  "pr_title": "x",
  "pr_number": 1
}
EOF
set +e
"$SCRIPT" /ignored "$tmp/same-sha.json" >/dev/null 2>&1
assert_exit "old_sha == new_sha → exit 1" 1 $?
set -e

# 4. Bad source_repo → exit 1.
cat > "$tmp/bad-repo.json" <<'EOF'
{
  "rule": "reusable_workflow_sha_bump_needs_propagation",
  "source_repo": "evil-org/standards",
  "source_workflow": ".github/workflows/governance-reusable.yml",
  "old_sha": "0011223344556677889900112233445566778899",
  "new_sha": "abcdef0123456789abcdef0123456789abcdef01",
  "pr_title": "x",
  "pr_number": 1
}
EOF
set +e
"$SCRIPT" /ignored "$tmp/bad-repo.json" >/dev/null 2>&1
assert_exit "non-estate source_repo → exit 1" 1 $?
set -e

# 5. Title keyword → exit 0 (REFUSED, not error — this is the expected refusal path).
cat > "$tmp/license-title.json" <<'EOF'
{
  "rule": "reusable_workflow_sha_bump_needs_propagation",
  "source_repo": "hyperpolymath/standards",
  "source_workflow": ".github/workflows/governance-reusable.yml",
  "old_sha": "0011223344556677889900112233445566778899",
  "new_sha": "abcdef0123456789abcdef0123456789abcdef01",
  "pr_title": "ci: bump license header normalisation",
  "pr_number": 1
}
EOF
set +e
out=$("$SCRIPT" /ignored "$tmp/license-title.json" 2>&1)
rc=$?
set -e
assert_exit "license keyword in pr_title → exit 0 (REFUSED)" 0 "$rc"
if printf '%s' "$out" | grep -q "REFUSED"; then
    echo "ok    refusal message printed"
    ((pass++))
else
    echo "FAIL  refusal message missing"
    ((fail++))
fi

# 6. DRY_RUN with valid finding + override TSV → exit 0, payload printed.
cat > "$tmp/good.json" <<'EOF'
{
  "rule": "reusable_workflow_sha_bump_needs_propagation",
  "source_repo": "hyperpolymath/standards",
  "source_workflow": ".github/workflows/governance-reusable.yml",
  "old_sha": "0011223344556677889900112233445566778899",
  "new_sha": "abcdef0123456789abcdef0123456789abcdef01",
  "pr_title": "ci(governance): tighten codeql pin set",
  "pr_number": 999
}
EOF

cat > "$tmp/consumers.tsv" <<'EOF'
hyperpolymath/repo-a	.github/workflows/governance.yml
hyperpolymath/repo-b	.github/workflows/governance.yml
EOF

set +e
out=$(CONSUMERS_TSV_OVERRIDE="$tmp/consumers.tsv" DRY_RUN=true "$SCRIPT" /ignored "$tmp/good.json" 2>/dev/null)
rc=$?
set -e
assert_exit "DRY_RUN with valid finding → exit 0" 0 "$rc"

if printf '%s' "$out" | grep -q '"event_type": "propagate-sha-bump"'; then
    echo "ok    payload contains event_type"
    ((pass++))
else
    echo "FAIL  payload missing event_type"
    ((fail++))
fi

if printf '%s' "$out" | grep -qE '"branch_name": "ci/bump-governance-reusable-abcdef0"'; then
    echo "ok    branch_name slug correct"
    ((pass++))
else
    echo "FAIL  branch_name slug wrong"
    ((fail++))
fi

# Title suffix is synthesised from safe metadata (NOT from pr_title), so it
# should not echo the upstream PR title verbatim.
if printf '%s' "$out" | grep -qE '"title_suffix": "bump .github/workflows/governance-reusable.yml@abcdef0"'; then
    echo "ok    title_suffix synthesised from safe metadata"
    ((pass++))
else
    echo "FAIL  title_suffix wrong"
    ((fail++))
fi

echo ""
echo "passed: $pass  failed: $fail"
[[ "$fail" -eq 0 ]]
