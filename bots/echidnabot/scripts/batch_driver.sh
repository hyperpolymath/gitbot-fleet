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
#   VERISIM_TOKEN     (Bearer token; required when VERISIM_URL points at a
#                      deployed endpoint like nesy-solver-api.fly.dev with
#                      /ingest — the URL should then be the full ingest path)
#   NOTIFY            (1=notify-send on each result, 0=silent; default 1)
#   DRY_RUN           (1=print only, no HTTP; default 0)
#   MAX_FILES         (cap per target, default 10)
#   STRATEGY_TAG      (column stored in proof_attempts, default "batch-direct")
#   RATE_MS           (sleep ms between provers, default 100)
#   TIMEOUT_SEC       (per-prover timeout, default 30)

set -euo pipefail

VERISIM_URL="${VERISIM_URL:-http://127.0.0.1:8080}"
VERISIM_TOKEN="${VERISIM_TOKEN:-}"
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
  "echidna|safety|cvc5|examples|smt2"
  "echidna|safety|cvc5|proofs/z3|smt2"
  "echidna|safety|vampire|proofs/tptp|p"
  "echidna|safety|altergo|examples|smt2"
  "echidna|safety|altergo|proofs/z3|smt2"
  "echidna|invariant|metamath|proofs/metamath|mm"
  "echidna|model-check|cadical|proofs/dimacs|cnf"
  "echidna|correctness|z3|proofs/smt-lib-mined|smt2"
  "echidna|correctness|cvc5|proofs/smt-lib-mined|smt2"
  "echidna|model-check|kissat|proofs/dimacs|cnf"
  "echidna|safety|eprover|proofs/tptp|p"
  "echidna|equiv|coq|proofs/coq|v"
  "echidna|equiv|lean|proofs/lean|lean"
  "echidna|equiv|agda|proofs/agda|agda"
  "echidna|totality|idris2|verification/proofs/idris2|idr"
  "ephapax|equiv|coq|formal|v"
  "ephapax|linearity|idris2|src/formal/Ephapax/Formal|idr"
)
#
# Deferred targets (runners exist but need environment setup):
#   acl2   — /usr/local/bin/acl2 is a podman wrapper that pulls
#            rubengamboa/acl2:latest on every invocation (hangs on pull).
#            Needs a native ACL2 install + properly-certified book dir.
#   vampire — /usr/local/bin/vampire installed, but we have no clean TPTP
#             corpus in any of our repos (/external_corpora/tptp/*.p files
#             are HTML dumps, not problems).
#
# The agda + idris2 targets above will mostly fail because they depend on
# project/standard-library context not present at file-level invocation:
#   • agda files use ℕ / std-lib symbols without explicit import paths set
#     — needs agda-stdlib registered in ~/.agda/libraries
#   • idris2 files expect project-relative module imports (e.g. module
#     Echidna.Verification.*) — need a pack.toml or ipkg build root.
# Their failures are therefore "real" in the sense that CI without those
# setups would also fail; treat accordingly.

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

run_cvc5() {
  local file="$1"
  local t0 t1 output rc
  t0=$(date +%s%3N)
  output=$(timeout "${TIMEOUT_SEC}s" cvc5 "$file" 2>&1)
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  # Same interpretation as Z3: "ran and produced a decision" = success.
  if echo "$output" | grep -qE "^(sat|unsat|unknown)$"; then
    echo success
  elif echo "$output" | grep -qiE "error|parse"; then
    echo failure
  else
    echo unknown
  fi
}

run_dafny() {
  local file="$1"
  local t0 t1 output rc
  t0=$(date +%s%3N)
  output=$(timeout "${TIMEOUT_SEC}s" dafny verify --verification-time-limit=$((TIMEOUT_SEC - 1)) "$file" 2>&1)
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  # Dafny prints "Dafny program verifier finished with N verified, M errors".
  if echo "$output" | grep -qE "finished with [0-9]+ verified, 0 errors"; then
    echo success
  elif echo "$output" | grep -qE "finished with [0-9]+ verified, [1-9][0-9]* errors"; then
    echo failure
  else
    echo unknown
  fi
}

run_alt_ergo() {
  local file="$1"
  local t0 t1 output rc
  t0=$(date +%s%3N)
  output=$(timeout "${TIMEOUT_SEC}s" alt-ergo "$file" 2>&1)
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  # alt-ergo prints "Valid" / "Invalid" / "I don't know" / "Timeout" per goal.
  if echo "$output" | grep -q "Valid" && ! echo "$output" | grep -qE "Invalid|Timeout"; then
    echo success
  elif echo "$output" | grep -qE "Invalid"; then
    echo failure
  elif echo "$output" | grep -qE "Timeout"; then
    echo timeout
  else
    echo unknown
  fi
}

run_metamath() {
  local file="$1"
  local t0 t1 output rc
  t0=$(date +%s%3N)
  output=$(echo "read \"$file\"
verify proof *
exit" | timeout "${TIMEOUT_SEC}s" metamath 2>&1)
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  # metamath exits 0 regardless; parse output for ?Error / "were not proved".
  if echo "$output" | grep -qE "^\?Error|were not proved"; then
    echo failure
  elif echo "$output" | grep -q "All proofs in the database were verified"; then
    echo success
  else
    echo unknown
  fi
}

run_kissat() {
  local file="$1"
  local t0 t1 rc
  t0=$(date +%s%3N)
  timeout "${TIMEOUT_SEC}s" kissat -q "$file" >/dev/null 2>&1
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  # kissat shares cadical's exit code convention: 10=SAT, 20=UNSAT.
  case $rc in
    10|20) echo success ;;
    0)     echo unknown ;;
    *)     echo failure ;;
  esac
}

run_eprover() {
  local file="$1"
  local t0 t1 output rc
  t0=$(date +%s%3N)
  output=$(timeout "${TIMEOUT_SEC}s" eprover --auto-schedule "$file" 2>&1)
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  # E parses SZS status from its banner. "Theorem" or "ContradictoryAxioms"
  # both count as successful termination.
  if echo "$output" | grep -qE "SZS status (Theorem|Unsatisfiable|ContradictoryAxioms)"; then
    echo success
  elif echo "$output" | grep -qE "SZS status (CounterSatisfiable|Satisfiable)"; then
    echo failure
  elif echo "$output" | grep -qE "SZS status (Timeout|GaveUp|ResourceOut)"; then
    echo timeout
  else
    echo unknown
  fi
}

run_cadical() {
  local file="$1"
  local t0 t1 rc
  t0=$(date +%s%3N)
  timeout "${TIMEOUT_SEC}s" cadical -q "$file" >/dev/null 2>&1
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  # cadical exit codes: 10=SAT, 20=UNSAT, 0=unknown. Both 10 and 20 count
  # as "solver ran and produced a decision" → success for our purposes.
  case $rc in
    10|20) echo success ;;
    0)     echo unknown ;;
    *)     echo failure ;;
  esac
}

run_why3() {
  local file="$1"
  local t0 t1 output rc
  t0=$(date +%s%3N)
  output=$(timeout "${TIMEOUT_SEC}s" why3 prove -P z3 "$file" 2>&1)
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  # why3 prints "Prover result is: Valid" on success, "Invalid" / "Timeout" /
  # "Unknown" / "Failure" otherwise. Multiple goals → multiple lines.
  if echo "$output" | grep -q "Prover result is: Valid" && \
     ! echo "$output" | grep -qE "Invalid|Timeout|Unknown|Failure"; then
    echo success
  elif echo "$output" | grep -qE "Invalid|Failure"; then
    echo failure
  elif echo "$output" | grep -q "Timeout"; then
    echo timeout
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

run_agda() {
  local file="$1"
  local t0 t1 rc
  t0=$(date +%s%3N)
  # Must cd into file's directory so Agda resolves module name from filename.
  ( cd "$(dirname "$file")" && timeout "${TIMEOUT_SEC}s" agda "$(basename "$file")" >/dev/null 2>&1 )
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  [ $rc -eq 0 ]   && echo success || echo failure
}

run_idris2() {
  local file="$1"
  local t0 t1 rc
  # Walk up to find the nearest .ipkg → that directory is the source root.
  # Fallback to the file's own directory when no ipkg is present.
  local dir source_dir rel
  dir=$(dirname "$file")
  source_dir="$dir"
  while [ "$dir" != "/" ] && [ "$dir" != "" ]; do
    if ls "$dir"/*.ipkg >/dev/null 2>&1; then
      source_dir="$dir"
      break
    fi
    dir=$(dirname "$dir")
  done
  rel="${file#"$source_dir"/}"
  t0=$(date +%s%3N)
  ( cd "$source_dir" && timeout "${TIMEOUT_SEC}s" idris2 --source-dir . --check "$rel" >/dev/null 2>&1 )
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  [ $rc -eq 0 ]   && echo success || echo failure
}

run_acl2() {
  local file="$1"
  local t0 t1 rc
  t0=$(date +%s%3N)
  # ACL2 reads events from stdin; pipe the file in with a (exit) sentinel.
  # Non-zero exit on certification failure. :q exits the logic mode cleanly.
  ( printf ':q\n(exit)\n' | cat "$file" - | timeout "${TIMEOUT_SEC}s" acl2 >/dev/null 2>&1 )
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  [ $rc -eq 0 ]   && echo success || echo failure
}

run_vampire() {
  local file="$1"
  local t0 t1 output rc
  t0=$(date +%s%3N)
  output=$(timeout "${TIMEOUT_SEC}s" vampire --mode casc -t "${TIMEOUT_SEC}s" "$file" 2>&1)
  rc=$?
  t1=$(date +%s%3N)
  echo $((t1 - t0))
  [ $rc -eq 124 ] && { echo timeout; return; }
  # Vampire prints "% SZS status Theorem" on success, "% SZS status CounterSatisfiable"
  # or "Unsatisfiable" etc. on other outcomes. Exit code isn't reliable; parse SZS.
  if echo "$output" | grep -qE "^% SZS status (Theorem|Unsatisfiable)"; then
    echo success
  elif echo "$output" | grep -qE "^% SZS status (CounterSatisfiable|Satisfiable)"; then
    echo failure
  elif echo "$output" | grep -qE "SZS status (Timeout|GaveUp)"; then
    echo timeout
  else
    echo unknown
  fi
}

# Dispatch to the right runner by prover name.
run_prover() {
  local prover="$1" file="$2"
  case "$prover" in
    z3)       run_z3       "$file" ;;
    cvc5)     run_cvc5     "$file" ;;
    altergo)  run_alt_ergo "$file" ;;
    coq)      run_coq      "$file" ;;
    lean)     run_lean     "$file" ;;
    agda)     run_agda     "$file" ;;
    idris2)   run_idris2   "$file" ;;
    acl2)     run_acl2     "$file" ;;
    vampire)  run_vampire  "$file" ;;
    dafny)    run_dafny    "$file" ;;
    why3)     run_why3     "$file" ;;
    metamath) run_metamath "$file" ;;
    cadical)  run_cadical  "$file" ;;
    kissat)   run_kissat   "$file" ;;
    eprover)  run_eprover  "$file" ;;
    *)        echo 0; echo unknown ;;
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
    local resp http_code
    # Route choice:
    #   - local verisim-api: POST direct to /api/v1/proof_attempts (no auth)
    #   - deployed nesy-solver-api: POST to /ingest with bearer token
    # Detection: VERISIM_TOKEN set ⇒ assume VERISIM_URL points at the
    # nesy-solver-api ingress and use its /ingest endpoint.
    if [ -n "$VERISIM_TOKEN" ]; then
      resp=$(curl -sS -X POST "$VERISIM_URL/ingest" \
        -H 'Content-Type: application/json' \
        -H "Authorization: Bearer $VERISIM_TOKEN" \
        -d "$insert_body" --max-time 10 -w '\n%{http_code}')
    else
      resp=$(curl -sS -X POST "$VERISIM_URL/api/v1/proof_attempts" \
        -H 'Content-Type: application/json' -d "$insert_body" \
        --max-time 10 -w '\n%{http_code}')
    fi
    http_code="${resp##*$'\n'}"
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
