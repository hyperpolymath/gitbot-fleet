#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# preflight.sh — the gate between "prepared" and "applied" for the cicd-suite
# secret-scanner migration.
#
# It answers three questions and refuses to blur them:
#
#   FINDINGS  the proposed artefact is wrong: a context would change, a secret
#             would be forwarded, fast-PR scanning would be on, the lock closure
#             would lose an edge, or a patch would not apply.
#
#   NOT RUN   the evidence does not exist yet: no live canary receipt for
#             hyperpolymath/unified-dataset-vocab, or jq/network unavailable.
#             Not-run is not a pass, and it blocks the migration.
#
#   PASS      every static property holds AND the live canary receipt is bound
#             to the same callee SHA and every required context reported a real
#             detector result for the subject SHA.
#
# The migration is deliberately NOT applied in this change. The live canary is
# the first proof that the published callee resolves at startup, creates jobs
# and emits real receipts; until it exists, "deploy" would be a guess.
#
# Usage: preflight.sh [--receipt PATH] [--online] [--quiet]
#                     [--check-apply]
#
#   --receipt PATH   canary receipt (default canary/receipt.json)
#   --online         also re-read the live rulesets and the callee commit
#   --check-apply    refuse unless `patch --dry-run` applies every patch to
#                    this checkout (run before applying; it never edits)
#
# Exit: 0 pass, 1 findings, 2 not-run only, 3 both. A non-zero exit MUST block
# the migration; it is not advice.

set -uo pipefail

MIG="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$MIG/../../.." && pwd)"
RECEIPT="$MIG/canary/receipt.json"
ONLINE=0
QUIET=0
CHECK_APPLY=0
SUITE_SHA="4404c3c0474e17753fe7689f3485c02890aac113"
CALLEE="hyperpolymath/cicd-suite/.github/workflows/secret-scanner-reusable.yml@$SUITE_SHA"
FIXTURE="$MIG/fixtures/cicd-suite-$SUITE_SHA-secret-scanner-reusable.yml"

while [ $# -gt 0 ]; do
  case "$1" in
    --receipt) RECEIPT="${2:?--receipt needs a path}"; shift ;;
    --online) ONLINE=1 ;;
    --quiet) QUIET=1 ;;
    --check-apply) CHECK_APPLY=1 ;;
    -h|--help) sed -n '2,40p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "preflight: unknown argument: $1" >&2; exit 64 ;;
  esac
  shift
done

FINDINGS=(); NOTRUN=()
finding() { FINDINGS+=("$1|$2"); }
notrun() { NOTRUN+=("$1|$2"); }

TMP="$(mktemp -d "${TMPDIR:-/tmp}/scanner-preflight.XXXXXX")" || exit 64
trap 'rm -rf "$TMP"' EXIT

# ------------------------------------------------------- inputs and patches --
[ -f "$MIG/patches/secret-scanner.yml.patch" ] || finding PATCH-MISSING "no secret-scanner.yml.patch"
[ -f "$MIG/patches/actions.lock.patch" ] || finding PATCH-MISSING "no actions.lock.patch"
[ -f "$MIG/patches/lock-closure-receipts.tsv.patch" ] || finding PATCH-MISSING "no lock-closure-receipts.tsv.patch"

# The proposed tree is built from the real checkout plus the patches, so every
# check below reads exactly what would land.
mkdir -p "$TMP/tree/scripts/lib"
cp -R "$ROOT/.github" "$TMP/tree/" 2>/dev/null || finding TREE "$ROOT/.github missing"
cp "$ROOT/scripts/audit-lock-closure.sh" "$TMP/tree/scripts/" 2>/dev/null || finding TREE "audit-lock-closure.sh missing"
cp "$ROOT/scripts/lib/lock-closure-receipts.tsv" "$TMP/tree/scripts/lib/" 2>/dev/null || true
cp "$ROOT/scripts/lib/lock-audit-baseline.tsv" "$TMP/tree/scripts/lib/" 2>/dev/null || true

# A prepared migration can subsequently be applied by an approved PR.
# In that case old patch hunks no longer apply; check the final artifacts.
apply_ok=1
already_applied=0
if [ -f "$TMP/tree/.github/workflows/secret-scanner.yml" ] &&
   grep -Fq "uses: $CALLEE" "$TMP/tree/.github/workflows/secret-scanner.yml"; then
  already_applied=1
  source_key="'hyperpolymath/cicd-suite@$SUITE_SHA'"
  # Both caller's workflow map and dependency record must exist.
  count=$(grep -Fc "$source_key" "$TMP/tree/.github/workflows/actions.lock" 2>/dev/null || true)
  if [ "$count" -lt 2 ]; then
    finding LOCK-MISSING "applied scanner lacks both pinned suite caller and dependency records"
    apply_ok=0
  fi
  receipt=$(printf 'hyperpolymath/cicd-suite\t%s\tactions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1' "$SUITE_SHA")
  if ! grep -Fxq "$receipt" "$TMP/tree/scripts/lib/lock-closure-receipts.tsv" 2>/dev/null; then
    finding CLOSURE-NO-RECEIPT "applied scanner lacks the reviewed pinned-suite closure receipt"
    apply_ok=0
  fi
else
  for p in secret-scanner.yml.patch actions.lock.patch lock-closure-receipts.tsv.patch; do
    if ! ( cd "$TMP/tree" && patch -p1 -s -f --no-backup-if-mismatch < "$MIG/patches/$p" ); then
      finding PATCH-APPLY "patch does not apply to the current checkout: $p"
      apply_ok=0
    fi
  done
fi

# The intended migration must be applicable OR already applied with its
# reviewed pinned artifacts. Partial transitions never count as success.
if [ "$CHECK_APPLY" -eq 1 ] && [ "$already_applied" -eq 0 ]; then
  for p in "$MIG"/patches/*.patch; do
    if ! ( cd "$ROOT" && patch -p1 --dry-run -s -f < "$p" >/dev/null 2>&1 ); then
      finding PATCH-APPLY "patch --dry-run failed against $ROOT: $(basename "$p")"
    fi
  done
fi

PROPOSED="$TMP/tree/.github/workflows/secret-scanner.yml"
if [ ! -f "$PROPOSED" ]; then
  finding TREE "proposed secret-scanner.yml not present after patching"
fi

# ---------------------------------------------------------------- fixture ----
if [ ! -f "$FIXTURE" ]; then
  notrun FIXTURE-MISSING "vendored callee workflow is absent: $FIXTURE"
elif [ ! -f "$MIG/fixtures/SHA256SUMS" ]; then
  notrun FIXTURE-CHECKSUM "no fixes/SHA256SUMS, so the vendored callee cannot be trusted"
elif ! ( cd "$MIG/fixtures" && sha256sum -c SHA256SUMS >/dev/null 2>&1 ); then
  finding FIXTURE-CHECKSUM "vendored callee workflow does not match fixtures/SHA256SUMS"
fi

# ------------------------------------------------- static properties of patch --
# Copy stdin without whole-line comments; inline comments remain. Return 1
# when no lines remain, or a higher status on a grep error.
code_only() { grep -vE '^[[:space:]]*#'; }

if [ -f "$PROPOSED" ]; then
  # Contexts are not renamed by this migration.
  grep -qE '^  scan:$' "$PROPOSED" || finding WF-SHAPE "job id 'scan' is required: the callee resolves its own suite pin from .jobs.scan.uses"
  code_only < "$PROPOSED" | grep -qF "uses: $CALLEE" || finding WF-SHAPE "the scan job must use exactly $CALLEE"
  code_only < "$PROPOSED" | grep -qE '^[[:space:]]+fast-pr:[[:space:]]+false[[:space:]]*$' \
    || finding WF-FAST-PR "fast-pr must be explicitly false (got: $(code_only < "$PROPOSED" | grep -E 'fast-pr:' | tr -d ' ' || true))"
  code_only < "$PROPOSED" | grep -qE '^[[:space:]]*fast-pr:[[:space:]]+true' && finding WF-FAST-PR "fast-pr: true would enable introduced-commit PR scanning"

  # No secret inheritance, in any spelling.
  if code_only < "$PROPOSED" | grep -qE '^[[:space:]]*secrets[[:space:]]*:'; then
    finding WF-SECRETS "the caller carries a secrets: block; the callee declares none and needs none"
  fi
  code_only < "$PROPOSED" | grep -qF 'secrets: inherit' && finding WF-SECRETS "secrets: inherit would forward every repository and organisation secret"

  # Triggers unchanged: no new event, no PR-target, no scheduled/full audit.
  for bad in pull_request_target workflow_run schedule workflow_dispatch release; do
    if code_only < "$PROPOSED" | grep -qE "^[[:space:]]*$bad[[:space:]]*:"; then
      finding WF-TRIGGER "trigger '$bad' would be added by the migration"
    fi
  done
  code_only < "$PROPOSED" | grep -qE '^[[:space:]]*pull_request:[[:space:]]*$' || finding WF-TRIGGER "pull_request trigger missing"
  code_only < "$PROPOSED" | grep -qE '^[[:space:]]*push:[[:space:]]*$' || finding WF-TRIGGER "push trigger missing"
  code_only < "$PROPOSED" | grep -qE '^[[:space:]]*branches:[[:space:]]*\[main\][[:space:]]*$' || finding WF-TRIGGER "push must stay limited to main"

  # Least privilege: contents: read is the whole grant.
  for bad in security-events pull-requests id-token packages deployments attestations; do
    if code_only < "$PROPOSED" | grep -qE "^[[:space:]]+$bad[[:space:]]*:"; then
      finding WF-PERMS "needless grant '$bad' would be carried over"
    fi
  done
  if code_only < "$PROPOSED" | grep -qE '^[[:space:]]+actions[[:space:]]*:[[:space:]]*read'; then
    finding WF-PERMS "needless grant 'actions: read' would be carried over"
  fi
fi

# ------------------------------------------------ contexts from the callee ----
# Print sorted, unique scan contexts inferred from the callee workflow. Jobs
# named using matrix.language expand the listed languages as <language>-secrets;
# other jobs use their job IDs. The reader expects the fixture's YAML layout.
derive_contexts() { # <callee workflow>
  awk '
    function flush(   lang, n, L, i) {
      if (job == "") return
      if (name ~ /matrix\.language/) {
        n = split(langs, L, ",")
        for (i = 1; i <= n; i++) if (L[i] != "") print "scan / " L[i] "-secrets"
      } else {
        print "scan / " job
      }
      job = ""; name = ""; langs = ""
    }
    /^jobs:[[:space:]]*$/ { injobs = 1; next }
    !injobs { next }
    /^  [A-Za-z0-9_-]+:[[:space:]]*$/ {
      flush()
      job = $0; sub(/^  /, "", job); sub(/:.*$/, "", job)
      next
    }
    /^    name:[[:space:]]*/ {
      name = $0; sub(/^    name:[[:space:]]*/, "", name); gsub(/["'\'']/, "", name); next
    }
    /^[[:space:]]+language:[[:space:]]*\[/ {
      langs = $0; sub(/.*\[/, "", langs); sub(/\].*/, "", langs); gsub(/[[:space:]]/, "", langs); next
    }
    END { flush() }
  ' "$1" | LC_ALL=C sort -u
}

if [ -f "$FIXTURE" ]; then
  derive_contexts "$FIXTURE" > "$TMP/derived.txt"
  if [ ! -s "$TMP/derived.txt" ]; then
    notrun CONTEXTS "could not derive contexts from the vendored callee workflow"
  else
    grep -vE '^#' "$MIG/required-contexts.txt" | sed '/^[[:space:]]*$/d' | LC_ALL=C sort -u > "$TMP/required.txt"
    while IFS= read -r c; do
      grep -qxF "$c" "$TMP/required.txt" || finding CONTEXT-DROPPED "the callee does not produce required context '$c'"
    done < "$TMP/derived.txt"
    while IFS= read -r c; do
      grep -qxF "$c" "$TMP/derived.txt" || finding CONTEXT-MISSING "required context '$c' is not produced by the callee at $SUITE_SHA"
    done < "$TMP/required.txt"
  fi
  # The callee must declare no workflow_call secret and reference no secrets.
  if grep -qE '^[[:space:]]*secrets[[:space:]]*:' "$FIXTURE"; then
    finding CALLEE-SECRETS "the vendored callee declares workflow_call.secrets; the caller's no-secrets rule would need revisiting"
  fi
  grep -qF 'secrets.' "$FIXTURE" && finding CALLEE-SECRETS "the vendored callee references a secrets context"
fi

# ------------------------------------------------------- lock closure audit ---
if [ -x "$TMP/tree/scripts/audit-lock-closure.sh" ] || [ -f "$TMP/tree/scripts/audit-lock-closure.sh" ]; then
  AUDIT_OUT="$(cd "$TMP/tree" && bash scripts/audit-lock-closure.sh \
      --workflows "$TMP/tree/.github/workflows" \
      --receipts "$TMP/tree/scripts/lib/lock-closure-receipts.tsv" \
      --baseline "$TMP/tree/scripts/lib/lock-audit-baseline.tsv" \
      --repo-root "$TMP/tree" 2>&1)"
  audit_rc=$?
  case "$audit_rc" in
    0) ;;
    1|3) finding LOCK-CLOSURE "the proposed lock is not closed or not representation-clean (audit exit $audit_rc); see below" ;;
    2) notrun LOCK-CLOSURE "the proposed lock's closure is unproven (audit exit 2); see below" ;;
    *) finding LOCK-CLOSURE "audit failed to run (exit $audit_rc)" ;;
  esac
  if { [ "$audit_rc" -eq 1 ] || [ "$audit_rc" -eq 3 ]; } && [ "$QUIET" -eq 0 ]; then
    printf '%s\n' "$AUDIT_OUT" | sed 's/^/    /' >&2
  fi
else
  notrun LOCK-AUDIT "audit-lock-closure.sh is absent from the checkout"
fi

# ------------------------------------------------------------------ canary ----
canary_state="missing"
if [ ! -f "$RECEIPT" ]; then
  notrun CANARY "no live canary receipt at $RECEIPT: the first unified-dataset-vocab live run has not been recorded, so deployment must not expand"
else
  if ! command -v jq >/dev/null 2>&1; then
    notrun CANARY "jq is unavailable, so the canary receipt cannot be validated"
  else
    canary_state="$(jq -r '.result // "malformed"' "$RECEIPT" 2>/dev/null || echo malformed)"
    check="$(jq -r '
      def sha: test("^[0-9a-f]{40}$");
      if .schema_version != 1 then "schema_version"
      elif .result != "pass" then "result"
      elif .subject != "hyperpolymath/unified-dataset-vocab" then "subject"
      elif (.subject_sha // "" | sha | not) then "subject_sha"
      elif .subject_sha == "0000000000000000000000000000000000000000" then "subject_sha_placeholder"
      elif .suite_sha != "'"$SUITE_SHA"'" then "suite_sha"
      elif .fast_pr != false then "fast_pr"
      elif (.secrets_passed | type) != "array" or (.secrets_passed | length) != 0 then "secrets_passed"
      elif .scanned_mode != "full" then "scanned_mode"
      elif (.contexts | type) != "array" or (.contexts | length) != 3 then "contexts"
      elif ([.contexts[].name] | sort) != ["scan / gitleaks","scan / rust-secrets","scan / shell-secrets"] then "context_names"
      elif any(.contexts[]; .conclusion != "success") then "context_conclusion"
      elif any(.contexts[]; (.receipt_sha // "" | sha | not)) then "context_receipt_sha"
      elif any(.contexts[]; .receipt_sha != $subject) then "context_subject_binding"
      elif (.run_url // "" | test("^https://github\\.com/hyperpolymath/unified-dataset-vocab/actions/runs/[0-9]+$") | not) then "run_url"
      else "ok" end
    ' --arg subject "$(jq -r '.subject_sha // ""' "$RECEIPT" 2>/dev/null)" "$RECEIPT" 2>/dev/null || echo "unreadable")"
    if [ "$check" != "ok" ]; then
      case "$check" in
        result)
          if [ "$canary_state" = "not_run" ] || [ -z "$canary_state" ]; then
            notrun CANARY "the receipt at $RECEIPT records result='$canary_state': the live canary has not passed"
          else
            finding CANARY "the receipt records result='$canary_state'; a failing canary must stop the migration"
          fi
          ;;
        suite_sha) finding CANARY "the receipt is bound to a different callee SHA than $SUITE_SHA" ;;
        subject_sha_placeholder) finding CANARY "the receipt carries an all-zero subject SHA (placeholder, not evidence)" ;;
        context_*) finding CANARY "the receipt's contexts are not the three required ones with SHA-bound success results ($check)" ;;
        *) finding CANARY "the receipt is malformed or incomplete ($check)" ;;
      esac
    fi
  fi
fi

# ------------------------------------------------------------------ --online --
if [ "$ONLINE" -eq 1 ]; then
  if ! command -v gh >/dev/null 2>&1; then
    notrun ONLINE "gh is unavailable, so live rulesets and the callee commit were not re-read"
  else
    if ! gh api "repos/hyperpolymath/cicd-suite/commits/$SUITE_SHA" >/dev/null 2>&1; then
      finding REMOTE "the callee commit $SUITE_SHA is not reachable at hyperpolymath/cicd-suite"
    fi
    if contexts="$(gh api repos/hyperpolymath/gitbot-fleet/rulesets/24309193 --jq '.rules[] | select(.type=="required_status_checks") | .parameters.required_status_checks[].context' 2>/dev/null)"; then
      printf '%s\n' "$contexts" | grep -qxF "scan / gitleaks" \
        || finding RULESET "Secret-Scan-Floor no longer requires scan / gitleaks"
    else
      notrun RULESET "could not read ruleset 24309193; required contexts were not re-verified"
    fi
    if contexts="$(gh api repos/hyperpolymath/gitbot-fleet/rulesets/22415578 --jq '.rules[] | select(.type=="required_status_checks") | .parameters.required_status_checks[].context' 2>/dev/null)"; then
      printf '%s\n' "$contexts" > "$TMP/live-contexts.txt"
      for c in "scan / gitleaks" "scan / rust-secrets" "scan / shell-secrets"; do
        grep -qxF "$c" "$TMP/live-contexts.txt" || notrun RULESET "live ruleset 22415578 does not list '$c' (required-contexts.txt may be stale)"
      done
    else
      notrun RULESET "could not read ruleset 22415578; required-contexts.txt was not re-verified"
    fi
  fi
elif [ "$QUIET" -eq 0 ]; then
  notrun ONLINE "live rulesets and the callee commit were not re-read (pass --online to do so)"
fi

# ------------------------------------------------------------------- output ---
if [ "$QUIET" -eq 0 ]; then
  echo "preflight: secret-scanner → cicd-suite@$SUITE_SHA"
  echo "canary: $RECEIPT (result=$canary_state)"
  if [ "${#FINDINGS[@]}" -gt 0 ]; then
    echo
    echo "== FINDINGS (${#FINDINGS[@]}) =="
    for f in "${FINDINGS[@]}"; do printf '%s\t%s\n' "${f%%|*}" "${f#*|}"; done
  fi
  if [ "${#NOTRUN[@]}" -gt 0 ]; then
    echo
    echo "== NOT RUN (${#NOTRUN[@]}) =="
    for n in "${NOTRUN[@]}"; do printf 'NOT-RUN\t%s\t%s\n' "${n%%|*}" "${n#*|}"; done
  fi
  if [ "${#FINDINGS[@]}" -eq 0 ] && [ "${#NOTRUN[@]}" -eq 0 ]; then
    echo
    echo "PASS: static properties hold, lock closure is complete, and the live canary receipt is SHA-bound to this callee."
    echo "The migration may now be applied by the reviewer; this script never edits the checkout."
  fi
fi

if [ "${#FINDINGS[@]}" -gt 0 ] && [ "${#NOTRUN[@]}" -gt 0 ]; then exit 3; fi
if [ "${#FINDINGS[@]}" -gt 0 ]; then exit 1; fi
if [ "${#NOTRUN[@]}" -gt 0 ]; then exit 2; fi
exit 0
