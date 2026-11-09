#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Fleet static pre-push gate. Install with scripts/install-pre-push-gate.sh.
# A missing, failed or malformed scan is not a successful zero-finding scan.
set -euo pipefail

# Report the supplied reason to stderr and exit the hook with status 1.
fail() { printf 'BLOCKED: %s\n' "$*" >&2; exit 1; }
REPO_ROOT=$(git rev-parse --show-toplevel) || fail 'not a Git working tree'
cd "$REPO_ROOT"
for dependency in jq timeout; do
  command -v "$dependency" >/dev/null || fail "$dependency is required"
done

scanners=$(git config --get fleet.prePushScanners || true)
scanners=${scanners:-auto}
limit=$(git config --get fleet.prePushTimeoutSeconds || true)
limit=${limit:-180}
[[ "$limit" =~ ^[1-9][0-9]{0,3}$ ]] && (( limit <= 1800 )) ||
  fail 'fleet.prePushTimeoutSeconds must be 1..1800'
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT
scanners_run=0 critical=0 high=0 total=0

# Run the named scanner (panic-attack or hypatia) with the given executable
# and configured timeout in seconds. Add non-suppressed, non-advisory findings
# to the global counts and increment scanners_run after validating the report.
# Exit the hook on timeout, scanner failure, or invalid or incomplete output.
# Hypatia exit 1 is accepted only with a non-suppressed, non-advisory critical
# finding; the caller blocks on critical findings after scanning.
run_scanner() {
  local scanner=$1 binary=$2 rc=0 counts c h n
  printf 'Running %s (timeout %ss)\n' "$scanner" "$limit"
  if [[ "$scanner" == panic-attack ]]; then
    timeout --kill-after=5s "${limit}s" "$binary" assail "$REPO_ROOT" \
      --headless --output-format json --fail-on-incomplete </dev/null >"$tmp/report" 2>"$tmp/errors" || rc=$?
    (( rc == 0 )) || fail "$scanner exited $rc; no successful scan recorded"
  else
    HYPATIA_FORMAT=json HYPATIA_SEVERITY=critical HYPATIA_EXIT_ZERO=false \
      timeout --kill-after=5s "${limit}s" "$binary" scan "$REPO_ROOT" \
      </dev/null >"$tmp/report" 2>"$tmp/errors" || rc=$?
    (( rc == 0 || rc == 1 )) || fail "$scanner exited $rc; no successful scan recorded"
  fi

  # Slurp rejects multiple JSON documents. Validate the scanner's actual report
  # envelope before counting; an error object or an unknown severity is fatal.
  if ! counts=$(jq -er -s --arg scanner "$scanner" '
    if length != 1 then error("expected one report") else .[0] end
    | if $scanner == "panic-attack" then
        if type == "object" and .schema_version == "2.6" and
           .coverage.status == "complete" and
           (.coverage.analyzed_files | type) == "number" and
           .coverage.analyzed_files > 0 and (.weak_points | type) == "array"
        then .weak_points else error("invalid or incomplete assail report") end
      elif type == "array" then .
      elif type == "object" and .submission_metadata.scanner == "hypatia" and
           (.findings | type) == "array" and
           .submission_metadata.finding_count == (.findings | length)
      then .findings
      else error("invalid Hypatia report") end
    | if all(.[]; type == "object" and (.severity | type) == "string" and
          (.severity | ascii_downcase | IN("critical", "high", "medium", "warn", "low", "info")) and
          ((has("suppressed") | not) or (.suppressed | type) == "boolean") and
          ((has("advisory") | not) or (.advisory | type) == "boolean"))
      then . else error("invalid finding") end
    | map(select(.suppressed != true and .advisory != true) | .severity |= ascii_downcase)
    | [(map(select(.severity == "critical")) | length),
       (map(select(.severity == "high")) | length), length] | @tsv
  ' "$tmp/report" 2>/dev/null); then
    fail "$scanner returned an invalid or incomplete report; no successful scan recorded"
  fi
  IFS=$'\t' read -r c h n <<< "$counts"
  if [[ "$scanner" == hypatia && "$rc" == 1 && "$c" == 0 ]]; then
    fail 'Hypatia failed without a blocking critical finding'
  fi
  critical=$((critical + c)); high=$((high + h)); total=$((total + n))
  scanners_run=$((scanners_run + 1))
  printf '%s: validated report, %s unsuppressed findings, %s critical\n' "$scanner" "$n" "$c"
}

seen=' '
if [[ "$scanners" == auto ]]; then selected='panic-attack hypatia'; else selected=$scanners; fi
read -r -a selected_scanners <<< "$selected"
for scanner in "${selected_scanners[@]}"; do
  case "$seen" in *" $scanner "*) continue ;; esac
  seen="$seen$scanner "
  case "$scanner" in
    panic-attack) binary=$(command -v panic-attack || true) ;;
    hypatia) binary=$(command -v hypatia-cli.sh || true) ;;
    *) fail "unknown configured scanner: $scanner" ;;
  esac
  if [[ -z "$binary" ]]; then
    [[ "$scanners" == auto ]] || fail "required scanner $scanner is unavailable"
    printf 'Unavailable: %s\n' "$scanner"
    continue
  fi
  run_scanner "$scanner" "$binary"
done
(( scanners_run > 0 )) || fail 'no scanner completed; install Hypatia or panic-attack'
printf 'Completed scanners: %s; findings: %s; critical: %s; high: %s\n' \
  "$scanners_run" "$total" "$critical" "$high"
(( critical == 0 )) || fail "$critical critical finding(s); fix before pushing"

# A consumer may configure an executable adapter for its owned CI receipt gate.
# Pass the Git hook arguments and untouched ref-update stdin through to it.
ci_runner=$(git config --get fleet.prePushCiRunner || true)
if [[ -n "$ci_runner" ]]; then
  [[ "$ci_runner" = /* && -x "$ci_runner" ]] || fail 'configured CI runner must be an executable absolute path'
  "$ci_runner" "$@" || fail 'consumer CI execution gate did not pass'
else
  printf 'Static scan passed; no consumer CI execution gate is configured.\n'
fi
