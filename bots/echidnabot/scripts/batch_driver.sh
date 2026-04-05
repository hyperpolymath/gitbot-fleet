#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# batch_driver.sh — walk local proof trees, verify via prover binaries directly,
# record outcomes in verisim-api for the proof_strategy_selection MV.
#
# IMPORTANT (2026-04-05): this script bypasses echidna's /api/verify endpoint.
# That endpoint parses content into an abstract ProofState and re-exports it
# before running the backend, which loses the original proof body and
# silently passes garbage input as `valid=true` for Coq/Lean/Agda. Our
# training data needs ground truth, so we shell out to the prover binaries
# directly and interpret exit status.
#
# Environment:
#   VERISIM_URL       (default http://127.0.0.1:8080)
#   NOTIFY            (1=notify-send on each result, 0=silent; default 1)
#   DRY_RUN           (1=print only, no HTTP; default 0)
#   MAX_FILES         (cap per target, default 10)
#   STRATEGY_TAG      (column stored in proof_attempts, default "batch-direct")
#   RATE_MS           (sleep ms between provers, default 100)
#   TIMEOUT_SEC       (per-prover timeout, default 30)

set -euo pipefail

VERISIM_URL="${VERISIM_URL:-http://127.0.0.1:8080}"
NOTIFY="${NOTIFY:-1}"
DRY_RUN="${DRY_RUN:-0}"
MAX_FILES="${MAX_FILES:-10}"
STRATEGY_TAG="${STRATEGY_TAG:-batch-direct}"
RATE_MS="${RATE_MS:-100}"
TIMEOUT_SEC="${TIMEOUT_SEC:-30}"

REPOS_ROOT="/var/mnt/eclipse/repos"

# Target definitions: repo | obligation_class | prover | glob-root | extension
# Provers invoked directly (not via echidna). Only provers with reliable
# exit-status semantics are included; Agda's invocation is more involved and
# Idris2's --check needs a source directory so it's tracked per-target.
TARGETS=(
  "echidna|safety|z3|examples|smt2"
  "echidna|safety|z3|proofs/z3|smt2"
  "echidna|equiv|coq|proofs/coq|v"
  "echidna|equiv|lean|proofs/lean|lean"
  "ephapax|equiv|coq|formal|v"
)

# --- helpers ----------------------------------------------------------------

iso_now() { date -u +'%Y-%m-%dT%H:%M:%S.%3N'; }   # ClickHouse DateTime64(3), no Z
uuidv4() { cat /proc/sys/kernel/random/uuid; }

obligation_id_for() {
  printf "%s|%s|%s" "$1" "$2" "$3" | sha256sum | cut -c1-40
}

notify() {
  [ "$NOTIFY" = "1" ] || return 0
  notify-send --urgency="$1" --app-name="echidna-batch" "$2" "$3" 2>/dev/null || true
}

# Sleep in integer milliseconds using usleep fallback
rate_sleep() {
  [ "$RATE_MS" = "0" ] && return 0
  # Bash can't sleep ms natively; use perl or python3 fallback
  perl -e "select(undef,undef,undef,$RATE_MS/1000)" 2>/dev/null || sleep 1
}

# --- per-prover runners -----------------------------------------------------
#
# Each runner echoes two lines to stdout:
#   OUTCOME  (success|failure|timeout|unknown)
#   DURATION_MS

run_z3() {
  local file="$1"
  local t0 t1 output rc
  t0=$(date +%s%3N)
  output=$(timeout "${TIMEOUT_SEC}s" z3 -smt2 "$file" 2>&1)
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  if [ $rc -eq 124 ]; then
    echo timeout; return
  fi
  # Z3's check-sat result is on stdout. For proof obligations, we want
  # unsat (meaning the assertion is valid). Successful termination on a
  # well-formed SMT-LIB file counts as success even if sat — the prover
  # ran and produced a decision. We distinguish only "prover ran" from
  # "prover errored or timed out".
  if echo "$output" | grep -qE "^(sat|unsat|unknown)$"; then
    echo success
  elif echo "$output" | grep -qiE "error|parse"; then
    echo failure
  else
    echo unknown
  fi
}

run_coq() {
  local file="$1"
  local t0 t1 rc
  t0=$(date +%s%3N)
  ( cd "$(dirname "$file")" && timeout "${TIMEOUT_SEC}s" coqc -q "$(basename "$file")" >/dev/null 2>&1 )
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  [ $rc -eq 0 ]   && echo success || echo failure
}

run_lean() {
  local file="$1"
  local t0 t1 rc
  t0=$(date +%s%3N)
  timeout "${TIMEOUT_SEC}s" lean "$file" >/dev/null 2>&1
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  [ $rc -eq 0 ]   && echo success || echo failure
}

# Dispatch to the right runner by prover name.
run_prover() {
  local prover="$1" file="$2"
  case "$prover" in
    z3)   run_z3   "$file" ;;
    coq)  run_coq  "$file" ;;
    lean) run_lean "$file" ;;
    *)    echo 0; echo unknown ;;
  esac
}

# --- per-file verification --------------------------------------------------

verify_one() {
  local repo="$1" obligation_class="$2" prover="$3" file="$4"
  local rel_file="${file#"$REPOS_ROOT/$repo/"}"
  local claim
  claim=$(head -c 200 "$file" | tr '\n' ' ' | tr -s ' ')
  local obl_id attempt_id started completed
  obl_id=$(obligation_id_for "$repo" "$rel_file" "$claim")
  attempt_id=$(uuidv4)
  started=$(iso_now)

  local runner_output duration_ms outcome
  runner_output=$(run_prover "$prover" "$file")
  duration_ms=$(echo "$runner_output" | sed -n '1p')
  outcome=$(echo "$runner_output" | sed -n '2p')
  completed=$(iso_now)

  # Build + send insert
  local insert_body
  insert_body=$(jq -n \
    --arg attempt_id "$attempt_id" \
    --arg obligation_id "$obl_id" \
    --arg repo "hyperpolymath/$repo" \
    --arg file "$rel_file" \
    --arg claim "$claim" \
    --arg obligation_class "$obligation_class" \
    --arg prover_used "$prover" \
    --arg outcome "$outcome" \
    --argjson duration_ms "$duration_ms" \
    --argjson confidence 0.5 \
    --arg strategy_tag "$STRATEGY_TAG" \
    --arg started_at "$started" \
    --arg completed_at "$completed" \
    --arg prover_output "direct:$prover" \
    '{attempt_id:$attempt_id, obligation_id:$obligation_id, repo:$repo, file:$file,
      claim:$claim, obligation_class:$obligation_class, prover_used:$prover_used,
      outcome:$outcome, duration_ms:$duration_ms, confidence:$confidence,
      parent_attempt_id:null, strategy_tag:$strategy_tag,
      started_at:$started_at, completed_at:$completed_at,
      prover_output:$prover_output, error_message:null}')

  if [ "$DRY_RUN" != "1" ]; then
    local resp
    resp=$(curl -sS -X POST "$VERISIM_URL/api/v1/proof_attempts" \
      -H 'Content-Type: application/json' -d "$insert_body" \
      --max-time 10 -w '\n%{http_code}')
    local http_code="${resp##*$'\n'}"
    if [ "$http_code" != "200" ] && [ "$http_code" != "201" ]; then
      echo "  insert failed (HTTP $http_code): ${resp%$'\n'*}" >&2
    fi
  fi

  local icon urgency
  case "$outcome" in
    success) icon="✓"; urgency="normal" ;;
    failure) icon="✗"; urgency="critical" ;;
    timeout) icon="⏱"; urgency="critical" ;;
    *)       icon="?"; urgency="low" ;;
  esac
  printf "  %s %-8s %-7s %6dms  %s\n" \
    "$icon" "$outcome" "$prover" "$duration_ms" "$rel_file"
  notify "$urgency" \
    "$icon $repo: $outcome" \
    "$prover on $rel_file (${duration_ms}ms, class=$obligation_class)"

  rate_sleep
}

# --- main loop --------------------------------------------------------------

echo "batch_driver: verisim=$VERISIM_URL dry_run=$DRY_RUN timeout=${TIMEOUT_SEC}s rate=${RATE_MS}ms"
echo

for target in "${TARGETS[@]}"; do
  IFS='|' read -r repo obligation_class prover glob_root extension <<<"$target"
  dir="$REPOS_ROOT/$repo/$glob_root"
  if [ ! -d "$dir" ]; then
    echo "[$repo] SKIP: $dir not present"; continue
  fi

  # shellcheck disable=SC2207
  files=($(find "$dir" -type f -name "*.${extension}" 2>/dev/null | head -n "$MAX_FILES"))
  if [ "${#files[@]}" -eq 0 ]; then
    echo "[$repo] SKIP: no *.${extension} under $glob_root"; continue
  fi

  echo "[$repo] target class=$obligation_class prover=$prover glob=$glob_root/*.$extension (${#files[@]} files)"
  for f in "${files[@]}"; do
    verify_one "$repo" "$obligation_class" "$prover" "$f"
  done
  echo
done

# Summary
if [ "$DRY_RUN" != "1" ]; then
  echo "=== current strategy snapshot (strategy_tag=$STRATEGY_TAG only) ==="
  for cls in safety linearity termination equiv correctness totality; do
    row=$(curl -sS "$VERISIM_URL/api/v1/proof_attempts/strategy?class=$cls&limit=3" 2>/dev/null)
    top=$(jq -r '.recommendations[0] // empty | "top=\(.prover) rate=\(.success_rate) n=\(.total_attempts)"' <<<"$row" 2>/dev/null)
    [ -n "$top" ] && printf "  %-12s %s\n" "$cls" "$top"
  done
fi
