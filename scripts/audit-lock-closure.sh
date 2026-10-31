#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# audit-lock-closure.sh — audit what actions.lock *proves*, and report the rest
# as NOT RUN rather than as a pass.
#
# WHY THIS EXISTS (measured, 2026-10-31, hyperpolymath/gitbot-fleet@83e35bc)
# -------------------------------------------------------------------------
# scripts/check-lock-sync.sh is green and its four clauses hold. It still cannot
# prove closure for two populations, because both are produced outside the
# workflow YAML it compares against:
#
#   1. REMOTE JOB-LEVEL REUSABLE CALLS. `gh actions-lock` v0.2.0 does not lock a
#      job-level `uses:` (upstream #129, re-measured by hyperpolymath/standards
#      2026-10-14). A bare regeneration therefore DROPS the `workflows:` entry
#      for every reusable caller. The dropped edge is not a dangling edge, so
#      clause 3 cannot see it, and clause 1 deliberately treats an absent
#      job-level ref as harmless.
#
#   2. COMPOSITE-ACTION CLOSURE. A record's nested `uses:` list is the closed
#      edge set for an external composite action. Regeneration that shortens or
#      omits that list produces no dangling ref either — there is simply nothing
#      named to resolve — so clauses 1-4 all pass while the composite's real
#      dependencies are no longer represented.
#
# Neither class is observable from `gh actions-lock --verify-local`; standards'
# own regeneration script says so in as many words. What makes them observable
# is a REVIEWED RECEIPT: the union of external `uses:` read from the pinned
# source (the called reusable workflows, or the composite's action.yml) at the
# exact commit the lock records. This script compares the lock against those
# receipts. A record with nested edges and no matching receipt is NOT RUN — not
# green, not red — because the evidence does not exist yet.
#
# FINDINGS vs NOT RUN
# -------------------
# A FINDING is a defect in the artefact as written: a malformed ref, a record
# whose closure is demonstrably short of its receipt, a leaf record where a
# reusable's closure is owed. Findings exit non-zero; FATAL findings cannot be
# baselined, REVIEW findings can (scripts/lib/lock-audit-baseline.tsv) so a
# reviewed, pre-existing representation debt cannot hold the gate red forever
# while still staying visible in every run.
#
# A NOT-RUN is a check this script intended to perform and could not: no
# receipt for a source, no network for --online resolution, a workflow file it
# refuses to parse rather than guess at. Not-run is never counted as a pass.
#
# Exit: 0 clean, 1 findings, 2 not-run only, 3 both.
#
# Usage:
#   scripts/audit-lock-closure.sh [--json] [--online] [--quiet]
#                                [--workflows DIR] [--lock FILE]
#                                [--receipts FILE] [--baseline FILE]
#                                [--repo-root DIR]
#   scripts/audit-lock-closure.sh --emit-receipt OWNER/REPO@SHA --source FILE...
#
# Receipt rows (TSV, '#' comments allowed):
#   owner/repo <TAB> <40-hex commit> <TAB> owner/repo@ref

set -uo pipefail

# Deliberately no `set -e`: every check records a finding or a not-run, and a
# single unreadable input must not truncate the report.

WF_DIR=".github/workflows"
LOCK=""
RECEIPTS="scripts/lib/lock-closure-receipts.tsv"
BASELINE="scripts/lib/lock-audit-baseline.tsv"
REPO_ROOT=""
JSON=0
ONLINE=0
QUIET=0
EMIT_KEY=""
EMIT_SOURCES=()

while [ $# -gt 0 ]; do
  case "$1" in
    --json) JSON=1 ;;
    --online) ONLINE=1 ;;
    --quiet) QUIET=1 ;;
    --workflows) WF_DIR="${2:?--workflows needs a directory}"; shift ;;
    --lock) LOCK="${2:?--lock needs a file}"; shift ;;
    --receipts) RECEIPTS="${2:?--receipts needs a file}"; shift ;;
    --baseline) BASELINE="${2:?--baseline needs a file}"; shift ;;
    --repo-root) REPO_ROOT="${2:?--repo-root needs a directory}"; shift ;;
    --emit-receipt) EMIT_KEY="${2:?--emit-receipt needs owner/repo@sha}"; shift ;;
    --source) EMIT_SOURCES+=("${2:?--source needs a file or directory}"); shift ;;
    -h|--help) sed -n '2,55p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "audit-lock-closure: unknown argument: $1" >&2; exit 64 ;;
  esac
  shift
done

[ -n "$LOCK" ] || LOCK="$WF_DIR/actions.lock"
[ -n "$REPO_ROOT" ] || REPO_ROOT="$(cd "$WF_DIR/../.." 2>/dev/null && pwd)" || REPO_ROOT="."

if [ -z "${AWK:-}" ]; then
  for cand in gawk mawk awk; do
    if command -v "$cand" >/dev/null 2>&1; then AWK="$cand"; break; fi
  done
fi
[ -n "${AWK:-}" ] || { echo "audit-lock-closure: FATAL: no awk" >&2; exit 64; }

TMP="$(mktemp -d "${TMPDIR:-/tmp}/lock-closure.XXXXXX")" || exit 64
trap 'rm -rf "$TMP"' EXIT
: > "$TMP/findings.jsonl"
: > "$TMP/notrun.jsonl"

# ---------------------------------------------------------------- reporting --
F_SEV=(); F_RULE=(); F_SUBJ=(); F_DET=(); F_FIX=()
N_RULE=(); N_SUBJ=(); N_DET=(); N_FIX=()
ACC_SEV=(); ACC_RULE=(); ACC_SUBJ=(); ACC_DET=()

json_finding() { # sev rule subject detail fix
  if [ "$JSON" -eq 1 ]; then
    jq -nc --arg severity "$1" --arg rule "$2" --arg subject "$3" \
      --arg detail "$4" --arg fix "$5" \
      '{severity:$severity,rule:$rule,subject:$subject,detail:$detail,fix:$fix}' \
      >> "$TMP/findings.jsonl"
  fi
}
json_notrun() { # rule subject detail fix
  if [ "$JSON" -eq 1 ]; then
    jq -nc --arg rule "$1" --arg subject "$2" --arg detail "$3" --arg fix "$4" \
      '{rule:$rule,subject:$subject,detail:$detail,fix:$fix}' \
      >> "$TMP/notrun.jsonl"
  fi
}

add_finding() { # severity rule subject detail fix
  local sev="$1" rule="$2" subj="$3" det="$4" fix="$5" fp
  fp="$rule|$subj"
  if [ "$sev" = "REVIEW" ] && [ -n "${BASELINED[$fp]:-}" ]; then
    ACC_SEV+=("$sev"); ACC_RULE+=("$rule"); ACC_SUBJ+=("$subj"); ACC_DET+=("$det")
    return 0
  fi
  F_SEV+=("$sev"); F_RULE+=("$rule"); F_SUBJ+=("$subj"); F_DET+=("$det"); F_FIX+=("$fix")
  json_finding "$sev" "$rule" "$subj" "$det" "$fix"
}

add_notrun() { # rule subject detail fix
  N_RULE+=("$1"); N_SUBJ+=("$2"); N_DET+=("$3"); N_FIX+=("$4")
  json_notrun "$1" "$2" "$3" "$4"
}

# ------------------------------------------------------------- emit receipts --
# Extract every external `uses:` from one file. Emits USE<TAB>kind<TAB>raw or
# UNPARSED<TAB>reason; kind is job or step, derived from the line's shape. A
# value this reader cannot interpret is UNPARSED, never guessed.
extract_uses() { # <file>
  "$AWK" '
    function strip_comment(s,   out, i, n, c, q) {
      out = ""; n = length(s); i = 1; q = ""
      while (i <= n) {
        c = substr(s, i, 1)
        if (q == "") {
          if (c == "\"" || c == "'\''") { q = c; out = out c; i++; continue }
          if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) break
          out = out c; i++; continue
        }
        if (c == q) q = ""
        out = out c; i++
      }
      return out
    }
    {
      line = $0
      sub(/\r$/, "", line)
      if (line ~ /^[[:space:]]*$/) next
      if (line ~ /\t/) { print "UNPARSED\ttab in line"; next }
      match(line, /^[[:space:]]*/); ind = RLENGTH
      if (inblock) {
        if (ind > blockind) next
        inblock = 0
      }
      trimmed = line; sub(/^[[:space:]]+/, "", trimmed)
      if (substr(trimmed, 1, 1) == "#") next
      # A `steps:` block marks its children as step-level. The dash alone is
      # not enough: a step written as `- name: ...` puts `uses:` on the next
      # line with no dash of its own.
      if (in_steps && ind <= steps_ind) in_steps = 0
      if (line ~ /^[[:space:]]*(-[[:space:]]+)?["'\'']?steps["'\'']?[[:space:]]*:[[:space:]]*$/) {
        in_steps = 1; steps_ind = ind; next
      }
      # `uses` must be the key on its line; a flow mapping or a key nested in
      # another scalar is refused rather than parsed.
      if (match(line, /^[[:space:]]*(-[[:space:]]+)?["'\'']?uses["'\'']?[[:space:]]*:/)) {
        head = substr(line, 1, RLENGTH)
        kind = ((head ~ /-/) || (in_steps && ind > steps_ind)) ? "step" : "job"
        rest = substr(line, RLENGTH + 1)
        sub(/^[[:space:]]+/, "", rest)
        rest = strip_comment(rest)
        sub(/[[:space:]]+$/, "", rest)
        if (rest == "" || rest ~ /^[|>]/ || rest ~ /^[*&]/ || rest ~ /^[\[{]/) {
          print "UNPARSED\tuses: value not a plain scalar on this line"; next
        }
        if (rest ~ /[[:space:]]/) { print "UNPARSED\tuses: value contains whitespace"; next }
        if (rest ~ /^["'\'']/) {
          q = substr(rest, 1, 1); rest = substr(rest, 2)
          if (substr(rest, length(rest), 1) != q) { print "UNPARSED\tunterminated quoted uses: value"; next }
          rest = substr(rest, 1, length(rest) - 1)
        }
        print "USE\t" kind "\t" rest
        next
      }
      if (line ~ /(^|[[:space:],{])["'\'']?uses["'\'']?[[:space:]]*:/) {
        print "UNPARSED\tuses: key not at start of line (flow or nested scalar)"
      }
      # A block scalar value belongs to some other key: skip its body rather
      # than reading `uses:`-looking text inside it. A block scalar on `uses:`
      # itself was already refused above, never skipped.
      det = strip_comment(line)
      if (det ~ /(^|[[:space:]-])["'\'']?[A-Za-z0-9_.-]+["'\'']?[[:space:]]*:[[:space:]]*[|>][-+]?[0-9]?[[:space:]]*$/) {
        inblock = 1; blockind = ind; next
      }
    }
  ' "$1"
}

norm_ref() { # raw -> owner/repo@ref ; rc 2 = local/docker (not lock-keyed)
  local raw="$1" path ref owner repo
  case "$raw" in
    ./*|\$/*|docker://*) return 2 ;;
  esac
  path="${raw%@*}"
  ref="${raw##*@}"
  [ -n "$path" ] && [ -n "$ref" ] || return 1
  [ "$path" != "$raw" ] || return 1
  case "$path" in *[[:space:]]*|*"#"*|*"'"*) return 1 ;; esac
  IFS=/ read -r owner repo _ <<< "$path"
  [ -n "$owner" ] && [ -n "$repo" ] || return 1
  printf '%s/%s@%s' "$owner" "$repo" "$ref"
}

is_sha() { [[ "$1" =~ ^[0-9a-f]{40}$ ]]; }

# Union of external uses reachable from one source file or directory tree.
collect_source_uses() { # <path> ; prints normalized refs, one per line
  local src="$1" f kind raw norm
  if [ -d "$src" ]; then
    while IFS= read -r f; do
      collect_source_uses "$f"
    done < <(find "$src" -maxdepth 4 -type f \( -name '*.yml' -o -name '*.yaml' \) | sort)
    return 0
  fi
  [ -f "$src" ] || { echo "MISSING:$src" >&2; return 1; }
  while IFS=$'\t' read -r tag kind raw; do
    case "$tag" in
      USE)
        norm="$(norm_ref "$raw")" || continue
        printf '%s\n' "$norm"
        ;;
      UNPARSED) echo "UNPARSED:$src:$raw" >&2; return 1 ;;
    esac
  done < <(extract_uses "$src")
}

if [ -n "$EMIT_KEY" ]; then
  case "$EMIT_KEY" in
    *@[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]) ;;
    *) echo "audit-lock-closure: --emit-receipt needs owner/repo@<40-hex sha>" >&2; exit 64 ;;
  esac
  [ "${#EMIT_SOURCES[@]}" -gt 0 ] || { echo "audit-lock-closure: --emit-receipt needs --source" >&2; exit 64; }
  repo="${EMIT_KEY%@*}"; sha="${EMIT_KEY##*@}"
  rc=0
  for s in "${EMIT_SOURCES[@]}"; do
    collect_source_uses "$s" >> "$TMP/emit.refs" || rc=1
  done
  LC_ALL=C sort -u "$TMP/emit.refs" | while IFS= read -r ref; do
    printf '%s\t%s\t%s\n' "$repo" "$sha" "$ref"
  done
  exit "$rc"
fi

# ------------------------------------------------------------------- inputs --
[ -f "$LOCK" ] || { echo "audit-lock-closure: FATAL: no lockfile at $LOCK" >&2; exit 64; }
[ -d "$WF_DIR" ] || { echo "audit-lock-closure: FATAL: no workflow directory at $WF_DIR" >&2; exit 64; }

# Baseline: rule <TAB> subject <TAB> reason <TAB> owner <TAB> date
declare -A BASELINED=()
declare -A BASELINE_SEEN=()
if [ -f "$BASELINE" ]; then
  while IFS=$'\t' read -r rule subj reason owner date; do
    case "$rule" in ''|'#'*) continue ;; esac
    [ -n "$subj" ] || continue
    BASELINED["$rule|$subj"]="$reason"
  done < "$BASELINE"
fi

# Receipts: owner/repo <TAB> sha <TAB> nested ref
declare -A RECEIPT=()
declare -A RECEIPT_SRC=()
RECEIPT_LINES=0
if [ -f "$RECEIPTS" ]; then
  while IFS=$'\t' read -r src sha ref; do
    case "$src" in ''|'#'*) continue ;; esac
    RECEIPT_LINES=$((RECEIPT_LINES + 1))
    case "$src" in */*) ;; *) add_finding FATAL RECEIPT-PARSE "$src" "receipt source is not owner/repo" "Fix scripts/lib/lock-closure-receipts.tsv"; continue ;; esac
    if ! is_sha "$sha"; then
      add_finding FATAL RECEIPT-PARSE "$src@$sha" "receipt commit is not a full 40-hex commit" "Receipts must be SHA-bound; re-emit with --emit-receipt"
      continue
    fi
    if [ -z "$ref" ]; then
      add_finding FATAL RECEIPT-PARSE "$src@$sha" "receipt row has no nested ref (use a row-less source for zero nested uses)" "Remove the empty row"
      continue
    fi
    norm="$(norm_ref "$ref")" || { add_finding FATAL RECEIPT-PARSE "$src@$sha" "receipt nested ref '$ref' is not owner/repo@ref" "Nested refs must be bare OWNER/REPO@REF"; continue; }
    [ "$norm" = "$ref" ] || add_finding FATAL RECEIPT-SUBPATH "$src@$sha" "receipt nested ref '$ref' carries a subpath (schema rejects subpath pins)" "Collapse to $norm"
    RECEIPT["$src@$sha|$norm"]=1
    RECEIPT_SRC["$src@$sha"]=1
  done < "$RECEIPTS"
else
  add_notrun RECEIPTS "-" "no receipt file at $RECEIPTS, so no nested closure can be proven" \
    "Add reviewed receipts; see docs/migrations or run --emit-receipt"
fi

# ------------------------------------------------------------- parse the lock --
# Indentation is load-bearing: a workflow key sits at 4 spaces, its entries at
# 8; a dependency record at 4, its fields at 8, its nested uses: items at 12.
# Anchoring on the exact indent is what keeps `uses:` from being read as a key.
cat > "$TMP/lock.awk" <<'AWK'
  BEGIN { section = ""; wf = ""; dep = "" }
  {
    line = $0
    sub(/\r$/, "", line)
    if (line ~ /^[[:space:]]*$/) next
    if (line ~ /^#/) next
    if (line == "workflows:") { section = "workflows"; wf = ""; dep = ""; next }
    if (line == "dependencies:") { section = "dependencies"; wf = ""; dep = ""; next }
    if (line ~ /^[A-Za-z0-9_-]+:/) { section = "other"; next }
    if (section == "workflows") {
      if (line ~ /^    [^ ].*:([[:space:]]*\[\])?$/) {
        k = line; sub(/^    /, "", k); sub(/:([[:space:]]*\[\])?$/, "", k)
        if (k ~ /^'.*'$/) k = substr(k, 2, length(k) - 2)
        if (k == "") { print "PARSE\tworkflow key is empty"; next }
        wf = k; print "WFKEY\t" k
        next
      }
      if (line ~ /^        - /) {
        v = line; sub(/^        - /, "", v)
        if (v == "[]") next
        if (v ~ /^'.*'$/) v = substr(v, 2, length(v) - 2)
        if (wf == "") { print "PARSE\tworkflow entry outside a workflow key: " v; next }
        print "WFENTRY\t" wf "\t" v
        next
      }
      if (line ~ /^        /) { print "PARSE\tworkflow section line is not an entry: " line; next }
      print "PARSE\tworkflow key is not at the canonical 4-space indent: " line
      next
    }
    if (section == "dependencies") {
      if (line ~ /^    [^ ].*:([[:space:]]*\[\])?$/) {
        k = line; sub(/^    /, "", k); sub(/:([[:space:]]*\[\])?$/, "", k)
        if (k ~ /^'.*'$/) k = substr(k, 2, length(k) - 2)
        if (k == "") { print "PARSE\tdependency key is empty"; next }
        dep = k; print "DEPKEY\t" k
        next
      }
      if (line ~ /^            - /) {
        v = line; sub(/^            - /, "", v)
        if (v ~ /^'.*'$/) v = substr(v, 2, length(v) - 2)
        if (dep == "") { print "PARSE\tnested uses outside a dependency record: " v; next }
        print "DEPUSE\t" dep "\t" v
        next
      }
      if (line ~ /^        [A-Za-z0-9_-]+:/) {
        if (dep == "") { print "PARSE\tdependency field outside a record: " line; next }
        f = line; sub(/^        /, "", f)
        name = f; sub(/:.*$/, "", name)
        val = f; sub(/^[^:]*:[[:space:]]*/, "", val)
        if (val ~ /^'.*'$/) val = substr(val, 2, length(val) - 2)
        print "DEPFIELD\t" dep "\t" name "\t" val
        next
      }
      print "PARSE\tdependency section line is not a field: " line
      next
    }
    print "PARSE\tline outside a recognised section: " line
  }
AWK
"$AWK" -f "$TMP/lock.awk" "$LOCK" > "$TMP/lock.tsv"

# GitHub resolves owner and repository case-insensitively; the ref is case
# sensitive. Fold exactly the owner/repo half everywhere a set is keyed, so a
# `Swatinem/rust-cache` use matches its `swatinem/rust-cache` record without
# hiding a genuinely different ref.
fold_ref() {
  local r="$1" path ref
  path="${r%@*}"; ref="${r##*@}"
  printf '%s@%s' "$(printf '%s' "$path" | tr '[:upper:]' '[:lower:]')" "$ref"
}
declare -A DISP=()
disp() { printf '%s' "${DISP[$1]:-$1}"; }

declare -A LOCK_ENTRY=()          # path|folded-ref
declare -A LOCK_ENTRIES=()        # path -> " folded-ref ..."
declare -A WF_ORDER=()
declare -A DEP_REF=()
declare -A DEP_COMMIT=()
declare -A DEP_USES=()
declare -A DEP_HASFIELD=()
declare -A DEP_SEEN=()
declare -A DEP_CASE=()
declare -A DEP_IDX=()             # folded key -> record key
DEP_KEYS=()

while IFS=$'\t' read -r tag a b c; do
  case "$tag" in
    WFKEY)
      if [ -n "${WF_ORDER["$a"]:-}" ]; then
        add_finding FATAL LOCK-DUP-KEY "$a" "workflow key appears more than once in the lockfile" "Keep one key per workflow path"
      fi
      WF_ORDER["$a"]=1
      ;;
    WFENTRY)
      path="$a"; raw="$b"
      norm="$(norm_ref "$raw")" || norm=""
      if [ -z "$norm" ]; then
        add_finding FATAL REF-WF-MALFORMED "$path" "workflow entry '$raw' is not owner/repo@ref" "Write the normalised lock key: owner/repo@ref"
        continue
      fi
      if [ "$norm" != "$raw" ]; then
        add_finding FATAL REF-SUBPATH "$path" "workflow entry '$raw' carries a subpath or owner/repo subkey" "Collapse to $norm"
      fi
      case "${norm##*@}" in
        refs/*) add_finding FATAL REF-REFS-PREFIX "$path" "workflow entry '$norm' uses a refs/ namespaced ref" "Use the bare tag, branch or 40-hex commit" ;;
        "") add_finding FATAL REF-EMPTY "$path" "workflow entry '$norm' has an empty ref" "Pin a tag, branch or commit" ;;
      esac
      f="$(fold_ref "$norm")"; DISP["$f"]="$norm"
      LOCK_ENTRY["$path|$f"]=1
      LOCK_ENTRIES["$path"]="${LOCK_ENTRIES["$path"]:-} $f"
      ;;
    DEPKEY)
      key="$a"
      if [ -n "${DEP_SEEN["$key"]:-}" ]; then
        add_finding FATAL REF-DUP-RECORD "$key" "dependency record appears more than once" "Keep one record per ref"
      fi
      DEP_SEEN["$key"]=1
      norm="$(norm_ref "$key")" || norm=""
      if [ -z "$norm" ] || [ "$norm" != "$key" ]; then
        add_finding FATAL REF-KEY-MALFORMED "$key" "dependency key is not a bare OWNER/REPO@REF" "Keys must be owner/repo@ref, with any action subpath collapsed"
      fi
      lc="$(printf '%s' "$key" | tr '[:upper:]' '[:lower:]')"
      if [ -n "${DEP_CASE["$lc"]:-}" ] && [ "${DEP_CASE["$lc"]}" != "$key" ]; then
        add_finding FATAL REF-CASE-DUP "$key" "record duplicates '${DEP_CASE["$lc"]}' ignoring case" "Keep one record; GitHub resolves owner/repo case-insensitively"
      fi
      DEP_CASE["$lc"]="$key"
      DEP_KEYS+=("$key")
      f="$(fold_ref "$key")"; DEP_IDX["$f"]="$key"; DISP["$f"]="$key"
      ;;
    DEPFIELD)
      key="$a"; field="$b"; val="$c"
      case "$field" in
        ref) DEP_REF["$key"]="$val" ;;
        commit) DEP_COMMIT["$key"]="$val" ;;
        uses) DEP_USES["$key"]="${DEP_USES["$key"]:-} $val" ;;
        owner_id|repo_id)
          [[ "$val" =~ ^[0-9]+$ ]] || add_finding FATAL REF-FIELD "$key" "$field is '$val', not an integer" "Record the numeric GitHub id or omit the field"
          ;;
        *) add_finding REVIEW REF-FIELD "$key" "unknown dependency field '$field'" "Remove it or extend the audit" ;;
      esac
      ;;
    DEPUSE)
      key="$a"; raw="$b"
      norm="$(norm_ref "$raw")" || norm=""
      if [ -z "$norm" ]; then
        add_finding FATAL REF-NESTED-MALFORMED "$key" "nested uses '$raw' is not a plain external ref" "Nested uses must be bare OWNER/REPO@REF (local ./ and docker:// are not lock-keyed)"
        continue
      fi
      if [ "$norm" != "$raw" ]; then
        add_finding FATAL REF-NESTED-SUBPATH "$key" "nested uses '$raw' carries a subpath; the lock schema rejects subpath pins" "Collapse to $norm"
      fi
      f="$(fold_ref "$norm")"; DISP["$f"]="$norm"
      DEP_USES["$key"]="${DEP_USES["$key"]:-} $f"
      ;;
    PARSE)
      add_finding FATAL LOCK-PARSE "$LOCK" "$a" "Make the lockfile match the gh-actions-lock layout; do not hand-edit around it"
      ;;
  esac
done < "$TMP/lock.tsv"

for key in "${DEP_KEYS[@]}"; do
  if [ -z "${DEP_REF["$key"]+x}" ]; then
    add_finding FATAL REF-NO-REF "$key" "record has no ref: field" "Add ref: '<reviewed tag or commit>'"
  fi
  commit="${DEP_COMMIT["$key"]:-}"
  if [ -z "$commit" ]; then
    add_finding FATAL REF-NO-COMMIT "$key" "record has no commit: field" "Add commit: 'sha1-<40-hex>'"
  elif [[ ! "$commit" =~ ^(sha1-)?[0-9a-f]{40}$ ]]; then
    add_finding FATAL REF-COMMIT "$key" "commit '$commit' is not a full sha1-<40-hex> digest" "Resolve the ref to the immutable commit and record it"
  else
    digest="${commit#sha1-}"
    ref="${key##*@}"
    if is_sha "$ref" && [ "$ref" != "$digest" ]; then
      add_finding FATAL REF-COMMIT-MISMATCH "$key" "record key pins $ref but commit is $digest" "A SHA-keyed record's commit must be that same commit"
    fi
    if [ -n "${DEP_REF["$key"]:-}" ] && is_sha "${DEP_REF["$key"]}" && [ "${DEP_REF["$key"]}" != "$digest" ]; then
      add_finding FATAL REF-REF-COMMIT-MISMATCH "$key" "ref '${DEP_REF["$key"]}' and commit '$digest' disagree" "The ref field must name the same immutable commit"
    fi
    DEP_COMMIT["$key"]="$digest"
  fi
done

# --------------------------------------------------------- parse the workflows --
declare -A WF_FILES=()
declare -A STEP_SET=()            # path|folded-ref
declare -A STEP_LIST=()           # path -> " folded-ref ..."
declare -A JOB_LIST=()            # path -> " folded-ref ..."
declare -A USED_ACTION=()
declare -A USED_REUSABLE=()
declare -A LOCAL_SEEN=()

list_has() { # <" ref ref "> <ref>
  case " $1 " in *" $2 "*) return 0 ;; esac
  return 1
}

scan_workflow_file() { # <path> <lock path> [composite]
  local file="$1" lockpath="$2" composite="${3:-0}" tag kind raw norm rc f localpath dir action
  while IFS=$'\t' read -r tag kind raw; do
    case "$tag" in
      USE)
        [ "$composite" -eq 1 ] && kind="step"
        norm="$(norm_ref "$raw")"; rc=$?
        if [ "$rc" -eq 2 ]; then
          # Local action (its refs belong to the calling workflow) or local
          # reusable workflow (that file is its own lock key, so it is not
          # followed).
          [ "$composite" -eq 1 ] && continue
          localpath="${raw%@*}"
          case "$localpath" in
            ./*.yml|./*.yaml) continue ;;
            ./*)
              dir="$REPO_ROOT/${localpath#./}"
              action=""
              [ -f "$dir/action.yml" ] && action="$dir/action.yml"
              [ -z "$action" ] && [ -f "$dir/action.yaml" ] && action="$dir/action.yaml"
              if [ -z "$action" ]; then
                add_notrun WF-LOCAL-MISSING "$lockpath" "local action '$localpath' has no action.yml under $REPO_ROOT" "Restore the composite or fix the path; its refs cannot be audited"
                continue
              fi
              if [ -n "${LOCAL_SEEN["$lockpath|$action"]:-}" ]; then continue; fi
              LOCAL_SEEN["$lockpath|$action"]=1
              scan_workflow_file "$action" "$lockpath" 1
              ;;
            \$/*)
              add_finding FATAL WF-DOLLAR-REF "$lockpath" "uses: $raw is the invalid \$/ local rewrite" "Restore ./ or the external ref"
              ;;
          esac
          continue
        fi
        if [ "$rc" -ne 0 ] || [ -z "$norm" ]; then
          add_finding FATAL REF-WF-UNPARSEABLE "$lockpath" "uses: '$raw' is not a valid external ref" "Fix the ref; an invalid uses: also fails workflow validation"
          continue
        fi
        f="$(fold_ref "$norm")"; DISP["$f"]="$norm"
        if [ "$kind" = "job" ]; then
          if [[ ! "$raw" =~ /\.github/workflows/[^@]+\.ya?ml@ ]]; then
            add_finding FATAL WF-JOB-NOT-REUSABLE "$lockpath" "job-level uses: '$raw' is not a reusable workflow path" "Job-level uses: must be owner/repo/.github/workflows/file.yml@ref (or a local path)"
            continue
          fi
          JOB_LIST["$lockpath"]="${JOB_LIST["$lockpath"]:-} $f"
          USED_REUSABLE["$f"]=1
        else
          if [[ "$raw" =~ /\.github/workflows/[^@]+\.ya?ml@ ]]; then
            add_finding FATAL WF-STEP-REUSABLE "$lockpath" "step-level uses: '$raw' names a workflow file" "A reusable workflow can only be called at job level"
            continue
          fi
          STEP_SET["$lockpath|$f"]=1
          STEP_LIST["$lockpath"]="${STEP_LIST["$lockpath"]:-} $f"
          USED_ACTION["$f"]=1
        fi
        ;;
      UNPARSED)
        add_notrun WF-PARSE "$lockpath" "cannot safely read a uses: value ($raw)" "Rewrite as a plain single-line scalar; the audit refuses to guess"
        ;;
    esac
  done < <(extract_uses "$file")
}

shopt -s nullglob
for wf in "$WF_DIR"/*.yml "$WF_DIR"/*.yaml; do
  key=".github/workflows/$(basename "$wf")"
  WF_FILES["$key"]="$wf"
  scan_workflow_file "$wf" "$key" 0
done
shopt -u nullglob

if [ "${#WF_FILES[@]}" -eq 0 ]; then
  add_finding FATAL WF-NONE "$WF_DIR" "no workflow files found" "Point --workflows at the directory holding the workflows"
fi

# ------------------------------------------------ clause 4: coverage and sync --
for key in "${!WF_FILES[@]}"; do
  if [ -z "${WF_ORDER["$key"]:-}" ]; then
    add_finding FATAL WF-UNLISTED "$key" "workflow file has no key in the lockfile; GitHub refuses such a run at startup (jobs=0)" "Add the path as a lockfile key; a zero-uses workflow takes []"
  fi
done
for key in "${!WF_ORDER[@]}"; do
  if [ -z "${WF_FILES["$key"]:-}" ]; then
    add_finding FATAL WF-MISSING "$key" "lockfile entry names a workflow file that does not exist" "Remove the key or restore the file"
  fi
done

for key in "${!WF_FILES[@]}"; do
  for combined in "${!STEP_SET[@]}"; do
    path="${combined%%|*}"; ref="${combined#*|}"
    [ "$path" = "$key" ] || continue
    if [ -z "${LOCK_ENTRY["$key|$ref"]:-}" ]; then
      add_finding FATAL WF-UNLOCKED "$key" "step-level use '$(disp "$ref")' is missing from this workflow's lock entry" "Add it and close its dependency record; do not hand-add the entry alone"
    fi
  done
  for entry in ${LOCK_ENTRIES["$key"]:-}; do
    if [ -z "${STEP_SET["$key|$entry"]:-}" ] && ! list_has "${JOB_LIST["$key"]:-}" "$entry"; then
      add_finding FATAL WF-ORPHAN "$key" "lock entry '$(disp "$entry")' is not referenced by this workflow's uses:" "Remove it, or restore the use it belonged to"
    fi
  done
  for ref in ${JOB_LIST["$key"]:-}; do
    rec="${DEP_IDX["$ref"]:-}"
    if [ -z "$rec" ]; then
      if [ -z "${LOCK_ENTRY["$key|$ref"]:-}" ]; then
        add_notrun REUSABLE-UNLOCKED "$(disp "$ref")" "job-level call in $key is not in the lockfile; gh actions-lock cannot write it, so its closure is unproven" "Add the entry and a closed record (complete-job-refs + close-lock), or accept the unproven edge"
      else
        add_finding FATAL REUSABLE-NO-RECORD "$(disp "$ref")" "workflow $key lists the job-level ref but there is no dependencies: record (dangling edge)" "Add a record with the callee's nested uses"
      fi
    fi
  done
done

# ------------------------------------------------------------- closure checks --
# Compare every owned edge set against the reviewed receipt for the exact
# commit the record resolves to. A missing receipt is NOT RUN, never green.
for key in "${DEP_KEYS[@]}"; do
  uses="${DEP_USES["$key"]:-}"
  digest="${DEP_COMMIT["$key"]:-}"
  [ -n "$digest" ] || continue
  repo="$(printf '%s' "${key%@*}" | tr '[:upper:]' '[:lower:]')"
  rid="$repo@$digest"
  f="$(fold_ref "$key")"

  if [ -n "$uses" ]; then
    if [ -z "${RECEIPT_SRC["$rid"]:-}" ]; then
      add_notrun CLOSURE-NO-RECEIPT "$rid" "record '$key' names nested uses but no receipt exists for $rid; completeness cannot be proven" "Run: ./scripts/audit-lock-closure.sh --emit-receipt $rid --source <pinned checkout>"
      continue
    fi
    for r in $uses; do
      if [ -z "${RECEIPT["$rid|$r"]:-}" ]; then
        add_finding FATAL CLOSURE-EXTRA "$rid" "record '$key' names '$(disp "$r")', which the reviewed source at $digest does not use" "Remove the unreviewed edge or re-review the pinned source"
      fi
    done
    for combined in "${!RECEIPT[@]}"; do
      src="${combined%%|*}"; r="${combined#*|}"
      [ "$src" = "$rid" ] || continue
      list_has "$uses" "$r" || add_finding FATAL CLOSURE-DROPPED "$rid" "'$(disp "$r")' is reached at $digest but record '$key' does not name it (dropped closure)" "Restore the edge; a shortened nested list produces no dangling ref, so no other gate sees it"
    done
  else
    # A leaf record is correct only when the pinned source really has no
    # external uses. A receipt with rows and a leaf record is a dropped list.
    if [ -n "${RECEIPT_SRC["$rid"]:-}" ]; then
      while IFS='|' read -r src r; do
        [ "$src" = "$rid" ] || continue
        add_finding FATAL CLOSURE-DROPPED "$rid" "'$(disp "$r")' is reached at $digest but record '$key' has no nested uses: at all (closure dropped to a leaf)" "Restore the nested list; a leaf produces no dangling ref, so no other gate sees it"
      done < <(for combined in "${!RECEIPT[@]}"; do printf '%s\n' "$combined"; done)
    elif [ -n "${USED_REUSABLE["$f"]:-}" ]; then
      add_notrun REUSABLE-NO-RECEIPT "$rid" "record '$key' is a leaf but the repo is called as a reusable workflow; a leaf closure is not proven" "Add a receipt for $rid (rows = the called workflow's external uses) or fix the leaf record"
    fi
  fi
done

# self-SHA classification: `ref:` repeating the resolved commit is legitimate
# for a reusable-workflow record and a defect for an action record. The class is
# read from how the workflows use that repository at that revision.
for key in "${DEP_KEYS[@]}"; do
  digest="${DEP_COMMIT["$key"]:-}"
  [ -n "$digest" ] || continue
  [ "${DEP_REF["$key"]:-}" = "$digest" ] || continue
  f="$(fold_ref "$key")"
  is_action=0; is_reusable=0
  [ -n "${USED_ACTION["$f"]:-}" ] && is_action=1
  [ -n "${USED_REUSABLE["$f"]:-}" ] && is_reusable=1
  if [ "$is_action" -eq 1 ] && [ "$is_reusable" -eq 0 ]; then
    add_finding FATAL REF-SELF-SHA-ACTION "$key" "record repeats the commit as ref: for a repo used as an ACTION" "Write the reviewed tag in ref: (commit stays the sha1 digest); a self-SHA action record makes callers die at Set up job"
  elif [ "$is_action" -eq 1 ] && [ "$is_reusable" -eq 1 ]; then
    add_finding REVIEW REF-SELF-SHA-BOTH "$key" "repo is used both as an action and as a reusable workflow at this revision, so the self-SHA ref is ambiguous" "Split the usages or confirm the reusable class is intended"
  elif [ "$is_action" -eq 0 ] && [ "$is_reusable" -eq 0 ]; then
    add_notrun REF-SELF-SHA-UNCLASSIFIED "$key" "no workflow uses this ref directly, so the self-SHA record cannot be classified" "Record the reviewed tag if the repo is used as an action anywhere"
  fi
done

# One reviewed ref must not resolve to two immutable commits: the snapshot is
# then ambiguous, and a regeneration may pick either.
declare -A TAG_DIGEST=()
for key in "${DEP_KEYS[@]}"; do
  repo="$(printf '%s' "${key%@*}" | tr '[:upper:]' '[:lower:]')"
  ref="${DEP_REF["$key"]:-${key##*@}}"
  f="$repo@$ref"
  is_sha "$ref" && continue
  digest="${DEP_COMMIT["$key"]:-}"
  [ -n "$digest" ] || continue
  if [ -n "${TAG_DIGEST["$f"]:-}" ] && [ "${TAG_DIGEST["$f"]}" != "$digest" ]; then
    add_finding REVIEW REF-AMBIGUOUS "$key" "the same reviewed ref resolves to two immutable commits (${TAG_DIGEST["$f"]} and $digest)" "Keep one reviewed snapshot per ref; delete or re-resolve the stale record"
  fi
  TAG_DIGEST["$f"]="$digest"
done

# A mutable branch at step level is not an immutable pin: the lock freezes a
# snapshot, but resolution follows the branch. Flag only conventional branch
# names; anything else is reported by --online, never assumed.
for f in "${!USED_ACTION[@]}"; do
  ref="${f##*@}"
  is_sha "$ref" && continue
  case "$ref" in
    main|master|HEAD|head|develop|trunk)
      add_finding REVIEW REF-MUTABLE "$(disp "$f")" "step-level action pins the mutable branch '$ref'; the lock records a snapshot commit that can drift from the live ref" "Request review: pin a reviewed tag with an immutable commit"
      ;;
  esac
done

# receipt hygiene
for combined in "${!RECEIPT_SRC[@]}"; do
  used=0
  for key in "${DEP_KEYS[@]}"; do
    repo="$(printf '%s' "${key%@*}" | tr '[:upper:]' '[:lower:]')"
    digest="${DEP_COMMIT["$key"]:-}"
    [ "$repo@$digest" = "$combined" ] && used=1 && break
  done
  [ "$used" -eq 0 ] && add_finding REVIEW RECEIPT-UNUSED "$combined" "receipt exists for a source@commit no record resolves to" "Remove the stale receipt or update the record's commit"
done
# ------------------------------------------------------------------- --online --
if [ "$ONLINE" -eq 1 ]; then
  if ! command -v gh >/dev/null 2>&1; then
    add_notrun REMOTE-UNREACHABLE "-" "--online requested but gh is not installed" "Install gh or run without --online"
  else
    declare -A CHECKED=()
    for key in "${DEP_KEYS[@]}"; do
      repo="${key%@*}"; digest="${DEP_COMMIT["$key"]:-}"
      [ -n "$digest" ] || continue
      [ -n "${CHECKED["$repo|$digest"]:-}" ] && continue
      CHECKED["$repo|$digest"]=1
      out="$(gh api "repos/$repo/commits/$digest" 2>&1)"; rc=$?
      if [ "$rc" -ne 0 ]; then
        if printf '%s' "$out" | grep -q 'HTTP 404'; then
          add_finding FATAL REMOTE-COMMIT "$repo@$digest" "recorded commit does not exist in $repo (HTTP 404)" "Re-resolve the ref; a present-but-unresolvable record is fatal at startup"
        else
          add_notrun REMOTE-UNREACHABLE "$repo@$digest" "commit lookup failed: $(printf '%s' "$out" | head -1)" "Re-run with network and gh auth"
        fi
      fi
    done
    declare -A REMOTE_FILE=()
    for key in "${DEP_KEYS[@]}"; do
      ref="${key##*@}"
      is_sha "$ref" && continue
      repo="${key%@*}"; digest="${DEP_COMMIT["$key"]:-}"
      [ -n "$digest" ] || continue
      rf="${REMOTE_FILE["$repo"]:-}"
      if [ -z "$rf" ]; then
        rf="$TMP/remote.$(printf '%s' "$repo" | tr '/' '_')"
        if ! git ls-remote --tags --heads "https://github.com/$repo" > "$rf" 2>/dev/null || [ ! -s "$rf" ]; then
          add_notrun REMOTE-UNREACHABLE "$repo" "could not list refs at $repo" "Re-run with network; do not rewrite the recorded commit without review"
          REMOTE_FILE["$repo"]="skip"
          continue
        fi
        REMOTE_FILE["$repo"]="$rf"
      fi
      [ "$rf" = "skip" ] && continue
      # An annotated tag has two lines: the tag object and its peeled ^{}
      # commit. The lock records the COMMIT, so prefer the peeled line.
      live="$(awk -v t="refs/tags/$ref^{}" '$2 == t { print $1; exit }' "$rf")"
      [ -n "$live" ] || live="$(awk -v t="refs/tags/$ref" '$2 == t { print $1; exit }' "$rf")"
      [ -n "$live" ] || live="$(awk -v t="refs/heads/$ref" '$2 == t { print $1; exit }' "$rf")"
      if [ -z "$live" ]; then
        add_notrun REMOTE-REF-UNRESOLVED "$key" "could not resolve '$ref' at $repo" "Resolve manually; do not rewrite the recorded commit without review"
        continue
      fi
      if [ "$live" != "$digest" ]; then
        add_finding REVIEW REMOTE-STALE "$key" "the live ref '$ref' resolves to $live, but the lock records $digest" "Decide deliberately: keep the reviewed snapshot or re-resolve; never let regeneration pick silently"
      fi
    done
  fi
fi

for fp in "${!BASELINED[@]}"; do
  # This pass MUST run after --online: a REMOTE-* row can only match a finding the
  # network phase produces. Running it earlier marks every online row stale.
  if [ "$ONLINE" -eq 0 ]; then
    case "$fp" in REMOTE-*) continue ;; esac
  fi
  seen=0
  for i in "${!F_RULE[@]}"; do [ "${F_RULE[$i]}|${F_SUBJ[$i]}" = "$fp" ] && seen=1 && break; done
  for i in "${!ACC_RULE[@]}"; do [ "${ACC_RULE[$i]}|${ACC_SUBJ[$i]}" = "$fp" ] && seen=1 && break; done
  [ "$seen" -eq 0 ] && add_notrun BASELINE-STALE "$fp" "baseline entry matches no current finding" "Remove it so a real regression cannot hide behind a stale acknowledgement"
done

# --------------------------------------------------------------------- output --
n_fatal=0; n_review=0
for i in "${!F_SEV[@]}"; do
  [ "${F_SEV[$i]}" = "FATAL" ] && n_fatal=$((n_fatal + 1)) || n_review=$((n_review + 1))
done
n_notrun=${#N_RULE[@]}
n_accepted=${#ACC_RULE[@]}

if [ "$JSON" -eq 1 ]; then
  jq -s --argjson fatal "$n_fatal" --argjson review "$n_review" \
    --argjson not_run "$n_notrun" --argjson accepted "$n_accepted" \
    --arg lock "$LOCK" --arg wf "$WF_DIR" \
    '{lock:$lock,workflows:$wf,
      summary:{fatal:$fatal,review:$review,accepted:$accepted,not_run:$not_run},
      findings:.[0], not_run:.[1]}' \
    "$TMP/findings.jsonl" "$TMP/notrun.jsonl"
else
  echo "audit-lock-closure: $LOCK"
  echo "workflows: $WF_DIR"
  if [ "${#F_SEV[@]}" -gt 0 ]; then
    echo
    echo "== FINDINGS (${#F_SEV[@]}) =="
    for i in "${!F_SEV[@]}"; do
      printf '%s\t%s\t%s\n' "${F_SEV[$i]}" "${F_RULE[$i]}" "${F_SUBJ[$i]}"
      printf '        %s\n' "${F_DET[$i]}"
      printf '        fix: %s\n' "${F_FIX[$i]}"
    done
  fi
  if [ "$n_accepted" -gt 0 ]; then
    echo
    echo "== ACCEPTED (baselined REVIEW findings: $n_accepted) =="
    for i in "${!ACC_RULE[@]}"; do
      printf 'ACCEPTED\t%s\t%s\n' "${ACC_RULE[$i]}" "${ACC_SUBJ[$i]}"
      printf '        %s\n' "${ACC_DET[$i]}"
    done
  fi
  if [ "$n_notrun" -gt 0 ]; then
    echo
    echo "== NOT RUN ($n_notrun) =="
    for i in "${!N_RULE[@]}"; do
      printf 'NOT-RUN\t%s\t%s\n' "${N_RULE[$i]}" "${N_SUBJ[$i]}"
      printf '        %s\n' "${N_DET[$i]}"
      printf '        next: %s\n' "${N_FIX[$i]}"
    done
  fi
  if [ "$n_fatal" -eq 0 ] && [ "$n_review" -eq 0 ] && [ "$n_notrun" -eq 0 ]; then
    echo
    echo "OK: lock is representation-clean and every owned edge set matches a reviewed receipt."
  elif [ "$n_fatal" -eq 0 ] && [ "$n_review" -eq 0 ]; then
    echo
    echo "INCOMPLETE: no findings, but $n_notrun check(s) did not run; this is not a pass."
  fi
  echo
  echo "summary: fatal=$n_fatal review=$n_review accepted=$n_accepted not-run=$n_notrun"
fi

if [ $((n_fatal + n_review)) -gt 0 ] && [ "$n_notrun" -gt 0 ]; then exit 3; fi
if [ $((n_fatal + n_review)) -gt 0 ]; then exit 1; fi
if [ "$n_notrun" -gt 0 ]; then exit 2; fi
exit 0
