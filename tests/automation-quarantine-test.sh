#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Containment regression tests: isolated copies, no GitHub calls or repo fixes.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/fleet/scripts" "$tmp/fleet/shared-context/findings/fixture" "$tmp/bin"
cp "$root/fleet-coordinator.sh" "$root/run-fleet.sh" "$tmp/fleet/"
cp "$root/scripts/dispatch-runner.sh" "$root/scripts/process-review-findings.sh" "$tmp/fleet/scripts/"
# Any unexpected external publication is a test failure even if wrapped in || true.
for tool in gh git mix; do
    cat > "$tmp/bin/$tool" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$0 $*" >> "$QUARANTINE_SENTINEL"
exit 99
EOF
    chmod +x "$tmp/bin/$tool"
done
export PATH="$tmp/bin:$PATH" QUARANTINE_SENTINEL="$tmp/unexpected-call"
export REPOS_BASE="$tmp/missing-root" FLEET_REPOS_BASE="$tmp/missing-root"
export GITHUB_ACTIONS=true
# No environment setting should unlock the temporary interlock.
export FLEET_ALLOW_WRITES=true FLEET_QUARANTINE=off
cat > "$tmp/fleet/shared-context/findings/fixture/input.json" <<'EOF'
{"submission_metadata":{"repo":"fixture"},"findings":[{"type":"unpinned_action","severity":"medium","auto_fixable":true,"fix_suggestion":"pin action","file":"workflow.yml","line":1}]}
EOF
ln -s input.json "$tmp/fleet/shared-context/findings/fixture/latest.json"
blocked() {
    local rc=0
    bash "$@" > "$tmp/log" 2>&1 || rc=$?
    if [[ "$rc" != 78 ]] || ! grep -q 'BLOCKED:' "$tmp/log"; then
        cat "$tmp/log" >&2
        echo "FAIL: expected explicit quarantine exit 78: $* (got $rc)" >&2
        exit 1
    fi
    echo "PASS: $*"
}
blocked "$tmp/fleet/scripts/dispatch-runner.sh"
blocked "$tmp/fleet/scripts/dispatch-runner.sh" --dry-run
blocked "$tmp/fleet/run-fleet.sh" fix --apply
blocked "$tmp/fleet/fleet-coordinator.sh" process-findings
blocked "$tmp/fleet/fleet-coordinator.sh" generate-rules
blocked "$tmp/fleet/fleet-coordinator.sh" deploy-bots
blocked "$tmp/fleet/scripts/process-review-findings.sh"
# Help remains available without credentials or inventory.
bash "$tmp/fleet/scripts/dispatch-runner.sh" --help >/dev/null
bash "$tmp/fleet/run-fleet.sh" --help >/dev/null
bash "$tmp/fleet/fleet-coordinator.sh" --help >/dev/null
bash "$tmp/fleet/scripts/process-review-findings.sh" --help >/dev/null
[[ ! -e "$tmp/unexpected-call" ]]
[[ ! -e "$tmp/fleet/shared-context/findings/fixture/input.json.processed" ]]
[[ ! -e "$tmp/fleet/shared-context/learning/fix-outcomes.jsonl" ]]
[[ ! -e "$tmp/fleet/shared-context/fix-batches" ]]
[[ ! -e "$tmp/fleet/shared-context/deployment-status.json" ]]
[[ ! -e "$tmp/missing-root" ]]
echo 'PASS: no publication, outcomes, acknowledgement, deployment or target mutations'
