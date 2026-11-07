#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Offline boundary controls: scanner processes are fixtures, Git and hooks are real.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
gate="$root/hooks/pre-push-gate.sh"
installer="$root/scripts/install-pre-push-gate.sh"
scratch=$(mktemp -d)
trap 'rm -rf -- "$scratch"' EXIT
mkdir "$scratch/bin" "$scratch/repo with spaces"
for tool in git jq timeout mktemp rm; do ln -s "$(command -v "$tool")" "$scratch/bin/$tool"; done
git init -q "$scratch/repo with spaces"
cd "$scratch/repo with spaces"
passed=0
export SCANNER_EXIT=0 REPORT_PAYLOAD='[]'
default_updates=$'refs/heads/main 123 refs/heads/main 456\n'
run_gate() {
  local expected=$1 label=$2 rc=0
  printf '%s' "${REF_UPDATES:-$default_updates}" |
    PATH="$scratch/bin" /bin/bash "$gate" origin fixture-remote >"$scratch/result" 2>&1 || rc=$?
  if { [[ "$expected" == pass ]] && (( rc != 0 )); } ||
     { [[ "$expected" == fail ]] && (( rc == 0 )); }; then
    cat "$scratch/result" >&2
    printf 'FAIL: %s (exit %s)\n' "$label" "$rc" >&2
    exit 1
  fi
  if [[ -n "${3:-}" ]] && ! /bin/grep -Fq -- "$3" "$scratch/result"; then
    cat "$scratch/result" >&2
    printf 'FAIL: %s (missing output: %s)\n' "$label" "$3" >&2
    exit 1
  fi
  passed=$((passed + 1))
  printf 'ok %s: %s\n' "$passed" "$label"
}
run_gate fail 'zero scanners blocks'
cat > "$scratch/bin/hypatia-cli.sh" <<'SCANNER'
#!/bin/bash
[[ "$1" == scan && -d "$2" && "$HYPATIA_FORMAT" == json && "$HYPATIA_SEVERITY" == critical ]] || exit 77
printf '%s\n' "$REPORT_PAYLOAD"
exit "$SCANNER_EXIT"
SCANNER
chmod +x "$scratch/bin/hypatia-cli.sh"
run_gate pass 'valid empty Hypatia array succeeds'
git config fleet.prePushScanners 'hypatia hypatia hypatia'
run_gate pass 'repeated Hypatia runs once'
[[ $(grep -c '^Running hypatia ' "$scratch/result") == 1 ]]
git config fleet.prePushScanners 'hypatia hyp'
run_gate fail 'scanner prefix is not mistaken for a duplicate'
grep -q 'unknown configured scanner: hyp$' "$scratch/result"
git config --unset fleet.prePushScanners
REPORT_PAYLOAD='{"message":"failed"}' run_gate fail 'error JSON is not a scan'
REPORT_PAYLOAD='not JSON' run_gate fail 'malformed output blocks'
REPORT_PAYLOAD='' run_gate fail 'empty output blocks'
REPORT_PAYLOAD='[] []' run_gate fail 'multiple reports block'
REPORT_PAYLOAD='[{}]' run_gate fail 'missing severity blocks'
REPORT_PAYLOAD='[{"severity":"typo"}]' run_gate fail 'unknown severity blocks'
REPORT_PAYLOAD='[{"severity":"critical"}]' SCANNER_EXIT=1 run_gate fail 'critical finding blocks'
REPORT_PAYLOAD='[{"severity":"CRITICAL"}]' run_gate fail 'case cannot hide critical finding'
REPORT_PAYLOAD='[{"severity":"critical","advisory":"true"}]' run_gate fail 'malformed advisory flag blocks'
REPORT_PAYLOAD='[{"severity":"critical","advisory":true}]' run_gate pass 'explicit advisory is not blocking'
SCANNER_EXIT=2 run_gate fail 'scanner failure with valid empty array blocks'
SCANNER_EXIT=1 run_gate fail 'failure without blocking finding blocks'
REPORT_PAYLOAD='{"submission_metadata":{"scanner":"hypatia","finding_count":0},"findings":[]}' run_gate pass 'Bash Hypatia envelope succeeds'
REPORT_PAYLOAD='{"submission_metadata":{"scanner":"hypatia","finding_count":1},"findings":[]}' run_gate fail 'inconsistent envelope count blocks'
# Verify severity thresholds, explicit flags and malformed Hypatia reports.
for severity in high medium warn low info; do
  REPORT_PAYLOAD=$(jq -cn --arg severity "$severity" '[{severity:$severity}]') \
    run_gate pass "$severity is below the blocking threshold" 'findings: 1; critical: 0'
done
for flag in suppressed advisory; do
  REPORT_PAYLOAD=$(jq -cn --arg flag "$flag" '[{severity:"critical",($flag):true}]') \
    run_gate pass "$flag excludes critical from totals" 'findings: 0; critical: 0'
  REPORT_PAYLOAD=$(jq -cn --arg flag "$flag" '[{severity:"critical",($flag):false}]') \
    run_gate fail "explicit false $flag retains critical" '1 critical finding(s)'
  for value in null 0 '"true"'; do
    REPORT_PAYLOAD=$(jq -cn --arg flag "$flag" --argjson value "$value" \
    '[{severity:"info",($flag):$value}]') \
    run_gate fail "nonboolean $flag=$value blocks" 'invalid or incomplete report'
  done
done
for payload in null true 0 '"text"' '[null]' '[1]' '[{"severity":null}]' \
    '[{"severity":42}]' \
    '{"submission_metadata":{"scanner":"other","finding_count":0},"findings":[]}' \
    '{"submission_metadata":{"scanner":"hypatia","finding_count":"0"},"findings":[]}'; do
  REPORT_PAYLOAD=$payload run_gate fail "invalid Hypatia schema: $payload" 'invalid or incomplete report'
done
REPORT_PAYLOAD='{"submission_metadata":{"scanner":"hypatia","finding_count":1},"findings":[{"severity":"critical"}]}' \
    SCANNER_EXIT=1 run_gate fail 'Hypatia envelope preserves finding exit and threshold' '1 critical finding(s)'
REPORT_PAYLOAD='[{"severity":"critical","suppressed":true}]' SCANNER_EXIT=1 \
    run_gate fail 'suppression cannot hide a Hypatia failure exit' 'Hypatia failed without a blocking critical finding'
for limit in 1 1800; do
  git config fleet.prePushTimeoutSeconds "$limit"
  run_gate pass "timeout boundary $limit is accepted" "timeout ${limit}s"
done
for limit in 0 1801 -1 01 1.5 invalid; do
  git config fleet.prePushTimeoutSeconds "$limit"
  run_gate fail "invalid timeout $limit blocks" 'must be 1..1800'
done
git config --unset fleet.prePushTimeoutSeconds
git config fleet.prePushScanners unknown
run_gate fail 'unknown scanner blocks' 'unknown configured scanner: unknown'
git config fleet.prePushScanners 'hypatia hypatia'
run_gate pass 'duplicate scanner runs once' 'Completed scanners: 1;'
git config fleet.prePushScanners 'hypatia panic-attack'
run_gate fail 'required missing scanner blocks despite another success'
git config fleet.prePushScanners panic-attack
cat > "$scratch/bin/panic-attack" <<'SCANNER'
#!/bin/bash
[[ "$1" == assail && "$3" == --headless && "$4" == --output-format && "$5" == json && "$6" == --fail-on-incomplete ]] || exit 77
printf '%s\n' "$REPORT_PAYLOAD"
exit "$SCANNER_EXIT"
SCANNER
chmod +x "$scratch/bin/panic-attack"
REPORT_PAYLOAD='{"schema_version":"2.6","coverage":{"status":"complete","analyzed_files":1},"weak_points":[]}' run_gate pass 'actual assail envelope and command flags'
git config fleet.prePushScanners 'panic-attack panic-attack panic-attack'
REPORT_PAYLOAD='{"schema_version":"2.6","coverage":{"status":"complete","analyzed_files":1},"weak_points":[]}' run_gate pass 'repeated panic-attack runs once'
[[ $(grep -c '^Running panic-attack ' "$scratch/result") == 1 ]]
git config fleet.prePushScanners panic-attack
REPORT_PAYLOAD='{"schema_version":"2.6","coverage":{"status":"complete","analyzed_files":1},"weak_points":[{"severity":"Critical"}]}' run_gate fail 'assail uppercase critical blocks'
REPORT_PAYLOAD='{"schema_version":"2.6","coverage":{"status":"complete","analyzed_files":1},"weak_points":[{"severity":"Critical","suppressed":true}]}' run_gate pass 'assail explicit suppression retained'
REPORT_PAYLOAD='{"schema_version":"2.6","coverage":{"status":"partial","analyzed_files":1},"weak_points":[]}' run_gate fail 'incomplete coverage blocks'
REPORT_PAYLOAD='{"schema_version":"2.5","weak_points":[]}' run_gate fail 'legacy unmeasured coverage is not complete'
REPORT_PAYLOAD='{"schema_version":"2.6","coverage":{"status":"complete","analyzed_files":1},"weak_points":[]}' SCANNER_EXIT=1 run_gate fail 'assail failure cannot be replaced by a valid report'
# Coverage and findings are validated independently of scanner exit success.
for coverage in '{}' '{"status":"complete"}' '{"status":"complete","analyzed_files":0}' \
    '{"status":"complete","analyzed_files":-1}' '{"status":"complete","analyzed_files":"1"}' \
    '{"status":"complete","analyzed_files":null}'; do
  REPORT_PAYLOAD=$(jq -cn --argjson coverage "$coverage" \
    '{schema_version:"2.6",coverage:$coverage,weak_points:[]}') \
    run_gate fail "invalid assail coverage: $coverage" 'invalid or incomplete report'
done
for findings in null '{}' '[{"severity":"unknown"}]' '[{"severity":"critical","suppressed":null}]'; do
  REPORT_PAYLOAD=$(jq -cn --argjson findings "$findings" \
    '{schema_version:"2.6",coverage:{status:"complete",analyzed_files:1},weak_points:$findings}') \
    run_gate fail "invalid assail findings: $findings" 'invalid or incomplete report'
done
REPORT_PAYLOAD='{"schema_version":"2.6","coverage":{"status":"complete","analyzed_files":1},"weak_points":[{"severity":"HIGH"},{"severity":"critical","advisory":true}]}' \
    run_gate pass 'assail high is counted and advisory critical excluded' 'findings: 1; critical: 0; high: 1'

# Auto selection must run both available scanners and retain either failure.
# This array is valid Hypatia output but invalid panic-attack output.
git config --unset fleet.prePushScanners
run_gate fail 'auto cannot ignore a failed scanner when another is available' 'panic-attack returned an invalid'
cp "$scratch/bin/panic-attack" "$scratch/panic-good"
cat > "$scratch/bin/panic-attack" <<'SCANNER'
#!/bin/bash
printf '%s\n' '{"schema_version":"2.6","coverage":{"status":"complete","analyzed_files":1},"weak_points":[{"severity":"high"}]}'
SCANNER
REPORT_PAYLOAD='[{"severity":"high"}]' run_gate pass 'auto aggregates both scanners' \
    'Completed scanners: 2; findings: 2; critical: 0; high: 2'
REPORT_PAYLOAD='[{"severity":"critical"}]' run_gate fail 'Hypatia critical survives noncritical assail scan' '1 critical finding(s)'
cp "$scratch/panic-good" "$scratch/bin/panic-attack"
git config fleet.prePushScanners hypatia
git config fleet.prePushTimeoutSeconds 1
cp "$scratch/bin/hypatia-cli.sh" "$scratch/hypatia-good"
printf '#!/bin/bash\n/bin/sleep 5\nprintf "[]\\n"\n' > "$scratch/bin/hypatia-cli.sh"
run_gate fail 'timeout blocks'
cp "$scratch/hypatia-good" "$scratch/bin/hypatia-cli.sh"
git config --unset fleet.prePushTimeoutSeconds

cat > "$scratch/ci-runner" <<'RUNNER'
#!/bin/bash
set -euo pipefail
[[ "$1" == origin && "$2" == fixture-remote ]]
IFS= read -r update
[[ "$update" == 'refs/heads/main 123 refs/heads/main 456' ]]
/bin/bash script-under-test.sh
RUNNER
chmod +x "$scratch/ci-runner"
git config fleet.prePushCiRunner "$scratch/ci-runner"
# The isolated PATH deliberately lacks cat; the executed script uses /bin/cat.
printf 'test "$(/bin/cat fixture.txt)" = accepted\n' > script-under-test.sh
printf 'accepted\n' > fixture.txt
run_gate pass 'consumer runner receives hook args and ref stdin and executes script'
printf 'rejected\n' > fixture.txt
run_gate fail 'changed fixture is executed and blocks'
printf 'exit 23\n' > script-under-test.sh
run_gate fail 'changed script exit propagates'
git config fleet.prePushCiRunner "$scratch/absent"
run_gate fail 'missing configured CI runner blocks'
git config fleet.prePushCiRunner ci-runner
run_gate fail 'relative CI runner path blocks' 'executable absolute path'
chmod -x "$scratch/ci-runner"
git config fleet.prePushCiRunner "$scratch/ci-runner"
run_gate fail 'nonexecutable CI runner blocks' 'executable absolute path'
chmod +x "$scratch/ci-runner"
cat > "$scratch/ci-runner" <<'RUNNER'
#!/bin/bash
[[ "$PWD" == "$EXPECTED_ROOT" ]] || exit 81
/bin/cat > "$CAPTURED_UPDATES"
RUNNER
export EXPECTED_ROOT="$PWD" CAPTURED_UPDATES="$scratch/received-updates"
REF_UPDATES=$'refs/heads/main 123 refs/heads/main 456\nrefs/heads/topic 789 refs/heads/topic 000\n' \
    run_gate pass 'runner receives every update unchanged'
printf '%s\n' 'refs/heads/main 123 refs/heads/main 456' \
    'refs/heads/topic 789 refs/heads/topic 000' > "$scratch/expected-updates"
cmp "$scratch/expected-updates" "$CAPTURED_UPDATES"
# A blocked scan must prevent the runner from executing.
rm "$CAPTURED_UPDATES"
REPORT_PAYLOAD='[{"severity":"critical"}]' \
    run_gate fail 'critical findings prevent CI execution' '1 critical finding(s)'
[[ ! -e "$CAPTURED_UPDATES" ]]
git config --unset fleet.prePushCiRunner
run_gate pass 'unconfigured runner explicitly limits success to static scanning' \
    'Static scan passed; no consumer CI execution gate is configured.'

bash "$installer" "$PWD" > "$scratch/install"
cmp "$gate" .git/hooks/pre-push
[[ -x .git/hooks/pre-push ]]
bash "$installer" "$PWD" > "$scratch/reinstall"
printf '# user hook\n' >> .git/hooks/pre-push
cp .git/hooks/pre-push "$scratch/preserved"
if bash "$installer" "$PWD" > "$scratch/conflict" 2>&1; then
  echo 'FAIL: installer overwrote custom hook' >&2; exit 1
fi
cmp "$scratch/preserved" .git/hooks/pre-push
git config core.hooksPath '.custom hooks'
bash "$installer" "$PWD" > "$scratch/custom"
cmp "$gate" '.custom hooks/pre-push'
# Absolute paths, dangling symlinks and identical nonexecutable files are
# separate preservation cases: the installer must not repair or replace them.
git config core.hooksPath "$scratch/absolute hooks"
bash "$installer" "$PWD" > "$scratch/absolute-install"
cmp "$gate" "$scratch/absolute hooks/pre-push"
[[ -x "$scratch/absolute hooks/pre-push" ]]
rm "$scratch/absolute hooks/pre-push"
cp "$gate" "$scratch/absolute hooks/pre-push"
chmod -x "$scratch/absolute hooks/pre-push"
if bash "$installer" "$PWD" > "$scratch/nonexecutable-conflict" 2>&1; then
  echo 'FAIL: installer changed nonexecutable hook' >&2; exit 1
fi
cmp "$gate" "$scratch/absolute hooks/pre-push"
[[ ! -x "$scratch/absolute hooks/pre-push" ]]
git config --unset core.hooksPath
git -c user.name='Hook fixture' -c user.email='fixture@example.invalid' -c commit.gpgsign=false commit --allow-empty -qm fixture
git worktree add -q "$scratch/linked" -b linked
# Existing custom hook is still preserved when reached through a worktree.
if bash "$installer" "$scratch/linked" > "$scratch/linked-conflict" 2>&1; then
  echo 'FAIL: linked worktree overwrote shared hook' >&2; exit 1
fi
cmp "$scratch/preserved" .git/hooks/pre-push
# Git may resolve a dangling hook symlink to its target when producing an
# absolute hooks path. Refuse installation without creating that target.
git config core.hooksPath "$scratch/absolute hooks"
rm "$scratch/absolute hooks/pre-push"
ln -s "$scratch/missing-hook" "$scratch/absolute hooks/pre-push"
if bash "$installer" "$PWD" > "$scratch/symlink-conflict" 2>&1; then
  cat "$scratch/symlink-conflict" >&2
  echo 'FAIL: installer followed dangling hook symlink and wrote its target' >&2
  exit 1
fi
[[ -L "$scratch/absolute hooks/pre-push" ]]
[[ "$(readlink "$scratch/absolute hooks/pre-push")" == "$scratch/missing-hook" ]]
[[ ! -e "$scratch/missing-hook" ]]
printf 'PASS: %s scanner/runner controls; installation, idempotence, custom hooksPath and worktree preservation\n' "$passed"
