#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# batch_driver.sh — walk local proof trees, verify via echidna, record in verisim-api.
#
# For each configured (repo, glob, prover, obligation_class) target:
#   1. Read proof file content
#   2. POST to echidna /api/verify with (prover, content) → valid/invalid + duration
#   3. POST to verisim-api /api/v1/proof_attempts with outcome + duration
#   4. Emit desktop notification via notify-send
#
# Bypasses echidnabot's clone-based pipeline to bootstrap training data for
# the proof_strategy_selection MV directly from local working trees.
#
# Environment:
#   ECHIDNA_URL       (default http://127.0.0.1:8090)
#   VERISIM_URL       (default http://127.0.0.1:8080)
#   NOTIFY            (1=notify-send on each result, 0=silent; default 1)
#   DRY_RUN           (1=print only, no HTTP; default 0)
#   MAX_FILES         (cap per target, default 10 to avoid flooding)
#   STRATEGY_TAG      (column stored in proof_attempts, default "batch-manual")

set -euo pipefail

ECHIDNA_URL="${ECHIDNA_URL:-http://127.0.0.1:8090}"
VERISIM_URL="${VERISIM_URL:-http://127.0.0.1:8080}"
NOTIFY="${NOTIFY:-1}"
DRY_RUN="${DRY_RUN:-0}"
MAX_FILES="${MAX_FILES:-10}"
STRATEGY_TAG="${STRATEGY_TAG:-batch-manual}"

REPOS_ROOT="/var/mnt/eclipse/repos"

# Target definitions: repo | obligation_class | prover | glob-root | extension
# obligation_class values match ClickHouse Enum8 in proof_attempts.sql:
# safety, linearity, termination, equiv, correctness, confluence, totality,
# invariant, refinement, model-check, other
TARGETS=(
  "echidna|safety|Z3|examples|smt2"
  "echidna|safety|Z3|proofs/z3|smt2"
  "echidna|equiv|Coq|proofs/coq|v"
  "echidna|equiv|Lean|proofs/lean|lean"
  "echidna|equiv|Agda|proofs/agda|agda"
  "echidna|totality|Idris2|verification/proofs/idris2|idr"
  "ephapax|equiv|Coq|formal|v"
  "ephapax|linearity|Idris2|src/formal/Ephapax/Formal|idr"
  "verisimdb|totality|Idris2|verification/proofs/idris2|idr"
)

# --- helpers ----------------------------------------------------------------

prover_lower() {
  # echidna REST names (Z3/Coq/Lean/Idris2/Agda) → verisim-api enum (lowercase)
  case "$1" in
    Z3) echo "z3" ;;
    Coq) echo "coq" ;;
    Lean) echo "lean" ;;
    Idris2) echo "idris2" ;;
    Agda) echo "agda" ;;
    CVC5|Cvc5) echo "cvc5" ;;
    *) echo "other" ;;
  esac
}

obligation_id_for() {
  # Stable SHA-256 prefix of repo|file|claim → hex string. ClickHouse column
  # is String, not UUID, so any stable identifier works.
  printf "%s|%s|%s" "$1" "$2" "$3" | sha256sum | cut -c1-40
}

iso_now() { date -u +'%Y-%m-%dT%H:%M:%S.000'; }  # ClickHouse DateTime64(3), no Z
uuidv4() { cat /proc/sys/kernel/random/uuid; }

notify() {
  # $1 = urgency (normal|critical), $2 = title, $3 = body
  [ "$NOTIFY" = "1" ] || return 0
  notify-send --urgency="$1" --app-name="echidna-batch" "$2" "$3" 2>/dev/null || true
}

# --- per-file verification --------------------------------------------------

verify_one() {
  local repo="$1" obligation_class="$2" prover="$3" file="$4"
  local rel_file="${file#"$REPOS_ROOT/$repo/"}"
  local claim
  claim=$(head -c 200 "$file" | tr '\n' ' ' | tr -s ' ')
  local obl_id
  obl_id=$(obligation_id_for "$repo" "$rel_file" "$claim")
  local attempt_id
  attempt_id=$(uuidv4)
  local started
  started=$(iso_now)

  # POST /api/verify to echidna
  local verify_body
  verify_body=$(jq -Rs --arg p "$prover" '{prover:$p, content:.}' <"$file")

  local t0_ms t1_ms duration_ms
  t0_ms=$(date +%s%3N)
  local verify_resp
  if [ "$DRY_RUN" = "1" ]; then
    verify_resp='{"valid":true,"goals_remaining":0}'
  else
    verify_resp=$(curl -sS -X POST "$ECHIDNA_URL/api/verify" \
      -H 'Content-Type: application/json' -d "$verify_body" \
      --max-time 60 || echo '{"valid":false,"error":"http_failure"}')
  fi
  t1_ms=$(date +%s%3N)
  duration_ms=$((t1_ms - t0_ms))

  local valid outcome
  valid=$(jq -r '.valid // false' <<<"$verify_resp")
  if [ "$valid" = "true" ]; then
    outcome="success"
  elif jq -e 'has("error")' <<<"$verify_resp" >/dev/null; then
    outcome="unknown"
  else
    outcome="failure"
  fi

  local completed
  completed=$(iso_now)
  local prover_lc
  prover_lc=$(prover_lower "$prover")

  # POST /api/v1/proof_attempts to verisim-api
  local insert_body
  insert_body=$(jq -n \
    --arg attempt_id "$attempt_id" \
    --arg obligation_id "$obl_id" \
    --arg repo "hyperpolymath/$repo" \
    --arg file "$rel_file" \
    --arg claim "$claim" \
    --arg obligation_class "$obligation_class" \
    --arg prover_used "$prover_lc" \
    --arg outcome "$outcome" \
    --argjson duration_ms "$duration_ms" \
    --argjson confidence 0.5 \
    --arg strategy_tag "$STRATEGY_TAG" \
    --arg started_at "$started" \
    --arg completed_at "$completed" \
    --arg prover_output "$(printf '%s' "$verify_resp" | head -c 512)" \
    '{attempt_id:$attempt_id, obligation_id:$obligation_id, repo:$repo, file:$file,
      claim:$claim, obligation_class:$obligation_class, prover_used:$prover_used,
      outcome:$outcome, duration_ms:$duration_ms, confidence:$confidence,
      parent_attempt_id:null, strategy_tag:$strategy_tag,
      started_at:$started_at, completed_at:$completed_at,
      prover_output:$prover_output, error_message:null}')

  if [ "$DRY_RUN" != "1" ]; then
    curl -sS -X POST "$VERISIM_URL/api/v1/proof_attempts" \
      -H 'Content-Type: application/json' -d "$insert_body" \
      --max-time 10 >/dev/null || echo "  insert failed" >&2
  fi

  # Emit notification + log line
  local icon urgency
  case "$outcome" in
    success) icon="✓"; urgency="normal" ;;
    failure) icon="✗"; urgency="critical" ;;
    *)       icon="?"; urgency="low" ;;
  esac
  printf "  %s %-8s %-7s %5dms  %s\n" \
    "$icon" "$outcome" "$prover_lc" "$duration_ms" "$rel_file"
  notify "$urgency" \
    "$icon $repo: $outcome" \
    "$prover_lc on $rel_file (${duration_ms}ms, class=$obligation_class)"
}

# --- main loop --------------------------------------------------------------

echo "batch_driver: echidna=$ECHIDNA_URL verisim=$VERISIM_URL dry_run=$DRY_RUN"
echo

for target in "${TARGETS[@]}"; do
  IFS='|' read -r repo obligation_class prover glob_root extension <<<"$target"
  dir="$REPOS_ROOT/$repo/$glob_root"
  if [ ! -d "$dir" ]; then
    echo "[$repo] SKIP: $dir not present"
    continue
  fi

  # shellcheck disable=SC2207
  files=($(find "$dir" -type f -name "*.${extension}" 2>/dev/null | head -n "$MAX_FILES"))
  if [ "${#files[@]}" -eq 0 ]; then
    echo "[$repo] SKIP: no *.${extension} under $glob_root"
    continue
  fi

  echo "[$repo] target class=$obligation_class prover=$prover glob=$glob_root/*.$extension (${#files[@]} files)"
  for f in "${files[@]}"; do
    verify_one "$repo" "$obligation_class" "$prover" "$f"
  done
  echo
done

# Summary
if [ "$DRY_RUN" != "1" ]; then
  echo "=== current strategy snapshot ==="
  for cls in safety linearity termination equiv correctness totality; do
    row=$(curl -sS "$VERISIM_URL/api/v1/proof_attempts/strategy?class=$cls&limit=3" 2>/dev/null)
    top=$(jq -r '.recommendations[0] // empty | "top=\(.prover) rate=\(.success_rate) n=\(.total_attempts)"' <<<"$row" 2>/dev/null)
    [ -n "$top" ] && printf "  %-12s %s\n" "$cls" "$top"
  done
fi
