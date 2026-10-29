#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# fleet-report-back.sh — run every fleet member a repository has enrolled and
# write one report-back envelope per member.
#
# The Hypatia verdict grid (standards: docs/VERDICT-GRID-SPEC.adoc, rendered by
# scripts/verdict-grid.sh) gives each gitbot-fleet member named in the scanned
# repository's `.gitbot-fleet/enrolment.json` one row. The row stays orange
# ("enrolled, but no report-back was received") until a file
# `.gitbot-fleet/report-backs/<member>.json` is present when the grid renders.
# Nothing in the estate produced those files before this script existed, which
# is why every fleet row on every PR read orange.
#
# This script is the producer. It is deliberately a thin shell driver over the
# adapters' EXISTING local-directory JSON modes: it adds no analysis of its own,
# it only translates each adapter's output into the envelope the grid reads:
#
#   { "reporter": "<member>", "title": "<title>", "status": "ran",
#     "reason": null, "ref": "<gitbot-fleet commit>",
#     "findings": [ { "rule_id", "severity", "file", "line", "message" } ] }
#
# status is one of ran | not_run | error | not_applicable; reason is required
# unless the member ran. Severity words are the fleet's own (error, warning,
# info, suggestion) or the scanner words (critical, high, medium, low); the
# grid normalises both. File paths are made relative to the repository root so
# the grid's change-scope (delta) grading can match them against the diff.
#
# Honesty rules, in order of precedence:
#   * A member whose adapter is missing or crashed is `error` with the reason,
#     never silently skipped: an enrolled member that stays silent is exactly
#     what the orange row exists to report, so the reason must be stated.
#   * A member that cannot run in this environment (echidnabot without an
#     ECHIDNA engine, the reserved sustainabot slot) is `not_run` with the
#     reason. It renders orange, like silence, but the comment now says WHY.
#   * `not_applicable` is a statement about the REPOSITORY and belongs in the
#     enrolment manifest (`enrolled: false`, with a reason), not here. This
#     script never writes it.
#   * Members marked `enrolled: false` are not run and get no file: the grid
#     renders them grey from the manifest alone.
#   * Nothing is ever written into the scanned repository. The envelopes go to
#     --out, which the workflow uploads as an artifact; the lane places them
#     under .gitbot-fleet/report-backs/ at render time. A committed envelope
#     would be stale evidence, so none is ever committed.
#
# Usage:
#   fleet-report-back.sh --repo DIR --out DIR [--enrolment FILE] [--bin-dir DIR]
#                        [--member NAME]... [--fleet-ref SHA] [--summary FILE]
#                        [--timeout SECONDS]
#   fleet-report-back.sh --plan [--enrolment FILE] [--member NAME]...
#
#   --repo        the checkout to report on (its root; paths are made relative)
#   --out         directory receiving <member>.json (created)
#   --enrolment   manifest; default <repo>/.gitbot-fleet/enrolment.json
#   --bin-dir     directory searched for adapter binaries before PATH
#   --member      run only these members (repeatable); they must be enrolled
#                 unless --enrolment is explicitly "" (no manifest)
#   --fleet-ref   gitbot-fleet commit recorded as `ref`; default: this
#                 checkout's HEAD, else "unknown"
#   --summary     append a Markdown table of the members and their status
#   --timeout     seconds allowed per adapter (default 300)
#   --plan        print "member<TAB>crate-dir<TAB>binary" for every member
#                 that would run, so a workflow can build only those crates
#
# Exit 0 whenever envelopes were written (whatever they say); 2 on usage
# error. An adapter failing never fails this script: the failure is the
# envelope's content.

set -euo pipefail

FLEET_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Print the usage text to stderr and terminate the script with status 2.
usage() {
  sed -n '/^# Usage:/,/^# error\./p' "$0" | sed 's/^# \{0,1\}//' >&2
  exit 2
}

# ── member table ─────────────────────────────────────────────────────────────
# member -> crate directory under bots/ (empty: no adapter) and binary name.
# Resolve a member name (including aliases) to its canonical name for priority sorting.
canonical_name_of() {
  case "$1" in
    finishingbot|finishbot|finishing-bot) echo finishingbot ;;
    *) echo "$1" ;;
  esac
}

crate_dir_of() {
  case "$1" in
    rhodibot)         echo rhodibot ;;
    panicbot)         echo panicbot ;;
    seambot)          echo seambot ;;
    finishingbot|finishbot|finishing-bot) echo finishingbot ;;
    glambot)          echo glambot ;;
    accessibilitybot) echo accessibilitybot ;;
    cipherbot)        echo cipherbot ;;
    echidnabot)       echo "" ;;   # no local mode: never built for a report-back
    sustainabot)      echo "" ;;
    *)                echo "" ;;
  esac
}

# Print the canonical priority order for fleet members.
# Source: .machine_readable/member-priority.json (single source of truth).
# Falls back to a hardcoded order if the file is missing/corrupt.
priority_order() {
  local pri_file="$FLEET_DIR/.machine_readable/member-priority.json"
  if [ -f "$pri_file" ] && jq -e '.priority | type == "array"' "$pri_file" >/dev/null 2>&1; then
    jq -r '.priority[]' "$pri_file"
  else
    # Fallback hardcoded order matching the canonical priority
    cat <<'EOF'
rhodibot
echidnabot
sustainabot
panicbot
glambot
seambot
finishingbot
accessibilitybot
cipherbot
robot-repo-automaton
hypatia
EOF
  fi
}

# Sort an array of member names by canonical priority.
# Input: member names on stdin (one per line)
# Output: member names sorted by priority (unknown members last, alphabetical)
sort_by_priority() {
  local pri_file="$FLEET_DIR/.machine_readable/member-priority.json"
  local order_map
  if [ -f "$pri_file" ] && jq -e '.priority | type == "array"' "$pri_file" >/dev/null 2>&1; then
    order_map=$(jq -r '.priority | to_entries | map("\(.value)\t\(.key)") | .[]' "$pri_file")
  else
    # Fallback hardcoded order
    order_map=$(cat <<'EOF'
rhodibot	0
echidnabot	1
sustainabot	2
panicbot	3
glambot	4
seambot	5
finishingbot	6
accessibilitybot	7
cipherbot	8
robot-repo-automaton	9
hypatia	10
EOF
)
  fi
  # Use awk to sort: known members by priority, unknown members last (alphabetical)
  awk -v order="$order_map" '
    BEGIN {
      split(order, lines, "\n");
      for (i in lines) {
        split(lines[i], kv, "\t");
        if (kv[1] != "") priority[kv[1]] = kv[2] + 0;
      }
    }
    {
      member = $0;
      if (member in priority) {
        print priority[member] "\t" member;
      } else {
        print 999 "\t" member;
      }
    }
  ' | sort -n -k1,1 | cut -f2-
}
# Print the adapter executable name for a member, or an empty line if unknown
# or unsupported locally. The finishingbot aliases all use finishing-bot.
binary_of() {
  case "$1" in
    finishingbot|finishbot|finishing-bot) echo finishing-bot ;;
    rhodibot|panicbot|seambot|glambot|accessibilitybot|cipherbot) echo "$1" ;;
    *) echo "" ;;
  esac
}
# Print the built-in display title, falling back to the member name.
default_title_of() {
  case "$1" in
    rhodibot)         echo "rhodibot (RSR compliance)" ;;
    panicbot)         echo "panicbot (panic recovery, error handling)" ;;
    echidnabot)       echo "echidnabot (formal verification)" ;;
    seambot)          echo "seambot (integration health)" ;;
    finishingbot)     echo "finishingbot (release readiness)" ;;
    accessibilitybot) echo "accessibilitybot (WCAG)" ;;
    glambot)          echo "glambot (presentation quality)" ;;
    cipherbot)        echo "cipherbot (cryptographic hygiene)" ;;
    sustainabot)      echo "sustainabot (eco/econ)" ;;
    *)                echo "$1" ;;
  esac
}

# ── argument parsing ─────────────────────────────────────────────────────────
repo="" out="" enrol="__default__" bindir="" fleet_ref="" summary="" plan=false
timeout_s=300
members=()
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    --out) out="$2"; shift 2 ;;
    --enrolment) enrol="$2"; shift 2 ;;
    --bin-dir) bindir="$2"; shift 2 ;;
    --member) members+=("$2"); shift 2 ;;
    --fleet-ref) fleet_ref="$2"; shift 2 ;;
    --summary) summary="$2"; shift 2 ;;
    --timeout) timeout_s="$2"; shift 2 ;;
    --plan) plan=true; shift ;;
    -h|--help) usage ;;
    *) echo "unknown option: $1" >&2; usage ;;
  esac
done

if [ "$plan" = false ]; then
  if [ -z "$repo" ] || [ -z "$out" ]; then usage; fi
  [ -d "$repo" ] || { echo "not a directory: $repo" >&2; exit 2; }
  repo="$(cd "$repo" && pwd)"
fi
if [ "$enrol" = "__default__" ]; then
  if [ -n "$repo" ]; then enrol="$repo/.gitbot-fleet/enrolment.json"; else enrol=""; fi
fi
command -v jq >/dev/null 2>&1 || { echo "jq is required" >&2; exit 2; }
# GNU timeout reads 0 as "no limit", which would let one hung adapter spend the
# whole job; the limit must be a positive integer.
[[ "$timeout_s" =~ ^[1-9][0-9]*$ ]] || { echo "--timeout must be a positive integer (0 would disable the limit)" >&2; exit 2; }

# ── enrolment ────────────────────────────────────────────────────────────────
# Which members run. An explicit --member list wins; otherwise every reporter
# the manifest marks `enrolled: true`. A manifest that cannot be read is NOT
# treated as "nobody enrolled": the grid already renders that as one orange
# row, so this script runs nothing and says so.
enrolment_json='null'
if [ -n "$enrol" ]; then
  if [ -s "$enrol" ] && jq -e 'type == "object" and (.reporters | type) == "object"' "$enrol" >/dev/null 2>&1; then
    enrolment_json=$(jq -c . "$enrol")
  elif [ -e "$enrol" ]; then
    echo "warning: enrolment manifest unreadable: $enrol" >&2
    enrolment_json='"unreadable"'
  fi
fi

if [ "${#members[@]}" -eq 0 ]; then
  if [ "$enrolment_json" = 'null' ] || [ "$enrolment_json" = '"unreadable"' ]; then
    members=()
  else
    mapfile -t members < <(jq -r '.reporters | to_entries[] | select(.value.enrolled == true) | .key' <<<"$enrolment_json")
  fi
else
  # Honour the manifest even when members are named explicitly: a member the
  # repository marked not enrolled is grey by its own statement, and running
  # it anyway would contradict that statement in the same comment.
  if [ "$enrolment_json" != 'null' ] && [ "$enrolment_json" != '"unreadable"' ]; then
    kept=()
    for m in "${members[@]}"; do
      if jq -e --arg m "$m" '.reporters[$m].enrolled == false' <<<"$enrolment_json" >/dev/null 2>&1; then
        echo "note: $m is marked not enrolled in $enrol; skipped" >&2
      else
        kept+=("$m")
      fi
    done
    members=("${kept[@]+"${kept[@]}"}")
  fi
fi

# A member name comes from the scanned repository's manifest, so it is untrusted:
# it becomes a tab-delimited plan field, an envelope file name and an adapter
# argument. Only a slug may pass; anything else is dropped with a warning (a
# traversal or a tab in a key must never reach the build or the output tree).
slugs=()
for m in "${members[@]+"${members[@]}"}"; do
  if [[ "$m" =~ ^[a-z][a-z0-9-]{0,63}$ ]]; then slugs+=("$m")
  else echo "warning: ignoring member $(printf '%q' "$m"): a member name is lowercase letters, digits and hyphens" >&2; fi
done
members=("${slugs[@]+"${slugs[@]}"}")

# Sort members by canonical priority order (F1: ONE CANONICAL PRIORITY)
# The driver sorts enrolled members by it regardless of manifest order.
if [ "${#members[@]}" -gt 1 ]; then
  # Resolve aliases to canonical names for sorting, but keep original names for output
  declare -A canon_map
  for m in "${members[@]}"; do
    canon_map["$m"]=$(canonical_name_of "$m")
  done
  # Sort using a Schwartzian transform: decorate with priority, sort, undecorate
  mapfile -t members < <(
    for m in "${members[@]}"; do
      c="${canon_map[$m]}"
      # Get priority from file or use 999 for unknown
      if [ -f "$FLEET_DIR/.machine_readable/member-priority.json" ] && jq -e ".priority | type == "array"" "$FLEET_DIR/.machine_readable/member-priority.json" >/dev/null 2>&1; then
        pri=$(jq -r --arg c "$c" ".priority | index($c) // 999" "$FLEET_DIR/.machine_readable/member-priority.json")
      else
        # Fallback hardcoded order
        case "$c" in
          rhodibot) pri=0 ;;
          echidnabot) pri=1 ;;
          sustainabot) pri=2 ;;
          panicbot) pri=3 ;;
          glambot) pri=4 ;;
          seambot) pri=5 ;;
          finishingbot) pri=6 ;;
          accessibilitybot) pri=7 ;;
          cipherbot) pri=8 ;;
          robot-repo-automaton) pri=9 ;;
          hypatia) pri=10 ;;
          *) pri=999 ;;
        esac
      fi
      printf "%d	%s
" "$pri" "$m"
    done | sort -n | cut -f2-
  )
fi
if [ "$plan" = true ]; then
  for m in "${members[@]+"${members[@]}"}"; do
    printf '%s\t%s\t%s\n' "$m" "$(crate_dir_of "$m")" "$(binary_of "$m")"
  done
  exit 0
fi

mkdir -p "$out"
out="$(cd "$out" && pwd)"

if [ -z "$fleet_ref" ]; then
  fleet_ref=$(git -C "$FLEET_DIR" rev-parse HEAD 2>/dev/null || echo unknown)
fi

# Print the member's nonempty manifest title, or its default display title.
title_of() {
  local m="$1" t=""
  if [ "$enrolment_json" != 'null' ] && [ "$enrolment_json" != '"unreadable"' ]; then
    t=$(jq -r --arg m "$m" '.reporters[$m].title // empty' <<<"$enrolment_json")
  fi
  # The title rides into jq as an argument; a manifest is PR-controlled, so it is
  # bounded here (120 characters) rather than trusted to fit the command line.
  if [ -n "$t" ]; then echo "${t:0:120}"; else default_title_of "$m"; fi
}

# Print the executable found in bindir first, then PATH; return 1 if absent.
find_binary() {
  local name="$1"
  if [ -n "$bindir" ] && [ -x "$bindir/$name" ]; then echo "$bindir/$name"; return 0; fi
  command -v "$name" 2>/dev/null || return 1
}

# Get adapter version for a member (from cargo pkg version or binary --version)
adapter_version_of() {
  local m="$1" bin crate version
  crate=$(crate_dir_of "$m")
  if [ -z "$crate" ]; then echo ""; return; fi
  # Try to get version from built binary first
  if bin=$(find_binary "$(binary_of "$m")"); then
    if version=$("$bin" --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9.-]+)?' | head -1); then
      echo "$version"
      return
    fi
  fi
  # Fallback: read from Cargo.toml
  if [ -f "$FLEET_DIR/bots/$crate/Cargo.toml" ]; then
    version=$(grep '^version' "$FLEET_DIR/bots/$crate/Cargo.toml" | head -1 | sed -E 's/.*"([^"]+)".*/\1/')
    if [ -n "$version" ]; then echo "$version"; return; fi
  fi
  echo "unknown"
}

# Get underlying tool version for members that wrap external tools
tool_version_of() {
  local m="$1" version
  case "$m" in
    panicbot)
      # panicbot wraps panic-attack
      if command -v panic-attack >/dev/null 2>&1; then
        version=$(panic-attack --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9.-]+)?' | head -1)
        [ -n "$version" ] && echo "panic-attack:$version" && return
      fi
      # Try from pinned SHA in workflow
      if [ -f "$FLEET_DIR/.github/workflows/fleet-report-back-reusable.yml" ]; then
        version=$(grep -oE 'PANIC_ATTACK_SHA: [0-9a-f]{40}' "$FLEET_DIR/.github/workflows/fleet-report-back-reusable.yml" | cut -d' ' -f2 | cut -c1-12)
        [ -n "$version" ] && echo "panic-attack:$version" && return
      fi
      echo "panic-attack:unknown"
      ;;
    echidnabot)
      # echidnabot uses ECHIDNA engine
      if [ -n "${ECHIDNA_URL:-}" ]; then
        echo "echidna-engine:remote"
      else
        echo "echidna-engine:unconfigured"
      fi
      ;;
    *)
      echo ""
      ;;
  esac
}

# ── envelope writers ─────────────────────────────────────────────────────────
# Every envelope goes through one writer so the shape cannot drift between
# members. Output is key-sorted, compact, one final LF: the I-JSON/JCS shape
# the estate's JSON policy asks of every .json it writes (standards
# 3-practice/JSON-POLICY.adoc), as far as jq can produce it.
# Write or replace out/<member>.json using the supplied status, reason, and
# findings JSON array. An empty reason becomes null; title and ref come from
# the current run. Temporary-file, JSON, and output I/O failures are not caught.
# F3: ENVELOPE PROVENANCE — include adapter+tool version pins, evidence class,
# and semantic fingerprint per finding for fleet-wide de-duplication.
write_envelope() { # member status reason findings_json
  local m="$1" status="$2" reason="$3" findings="$4" ftmp
  # The findings go in through a FILE, never argv: a few hundred findings
  # exceed ARG_MAX and jq dies with "Argument list too long" (seen live on
  # gitbot-fleet's own PR 626 with panicbot's 146 findings).
  ftmp=$(mktemp); printf '%s' "$findings" > "$ftmp"

  local adapter_ver tool_ver
  adapter_ver=$(adapter_version_of "$m")
  tool_ver=$(tool_version_of "$m")

  # Enrich findings with evidence_class and semantic_fingerprint using jq
  # (F3: ENVELOPE PROVENANCE). The enrichment is done inline in jq for performance.
  jq -S -c -n --arg reporter "$m" --arg title "$(title_of "$m")" --arg status "$status" \
    --arg reason "$reason" --arg ref "$fleet_ref" \
    --arg adapter_version "$adapter_ver" --arg tool_version "$tool_ver" \
    --arg member "$m" \
    --slurpfile findings "$ftmp" \
    'def evidence_class:
      if .evidence_class then .evidence_class
      else
        if $member == "panicbot" then
          (if .rule_id | startswith("PA0") then "panic-attack:unsafe-code"
           elif .rule_id | startswith("PA1") then "panic-attack:panic-path"
           elif .rule_id | startswith("PA2") then "panic-attack:command-injection"
           elif .rule_id | startswith("PA3") then "panic-attack:hardcoded-secret"
           elif .rule_id | startswith("PA4") then "panic-attack:unsafe-ffi"
           elif .rule_id | startswith("PA5") then "panic-attack:unsafe-deser"
           elif .rule_id | startswith("PA6") then "panic-attack:race-condition"
           elif .rule_id | startswith("PA7") then "panic-attack:resource-leak"
           elif .rule_id | startswith("PA8") then "panic-attack:unchecked-error"
           elif .rule_id | startswith("PA9") then "panic-attack:path-traversal"
           else "panic-attack:unknown" end)
        elif $member == "rhodibot" then "rsr:file-presence"
        elif $member == "cipherbot" then "cipherbot:crypto-hygiene"
        elif $member == "glambot" or $member == "accessibilitybot" then "wcag:accessibility"
        elif $member == "seambot" then "seam:drift-detection"
        elif $member == "finishingbot" then "finishingbot:release-readiness"
        else "fleet:" + (.category // "unknown") end
      end;
    def semantic_fingerprint:
      # Normalized fingerprint: base64 of "rule_id|file|line|category" (lowercase).
      # The standards grid can recompute sha256 from these components for de-duplication.
      # Use ascii_downcase (jq 1.6 compatible) instead of lower.
      ((.rule_id // "?" | ascii_downcase) + "|" + (.file // "") + "|" + (.line // 0 | tostring) + "|" + (.category // "unknown" | ascii_downcase)) | @base64;
    {
      reporter: $reporter, title: $title, status: $status,
      reason: (if $reason == "" then null else $reason end),
      ref: $ref,
      adapter_version: (if $adapter_version == "unknown" then null else $adapter_version end),
      tool_version: (if $tool_version == "" then null else $tool_version end),
      findings: ($findings[0] | map(. + {evidence_class: evidence_class, semantic_fingerprint: semantic_fingerprint}))
    }' > "$out/$m.json"
  rm -f "$ftmp"
}

# Emit a jq rel filter that strips the repo/ prefix and then one leading ./;
# nonstrings become empty strings and other paths are unchanged. Adapters differ:
# accessibilitybot prints absolute paths, glambot and finishing-bot "./x",
# seambot and rhodibot repository-relative. The grid matches paths against
# `git diff --name-only`, which is repository-relative with no "./".
relativize_filter() { # jq filter text; expects $repo bound
  cat <<'JQ'
def rel: if type != "string" then "" else
  (if startswith($repo + "/") then .[($repo | length) + 1:] else . end)
  | (if startswith("./") then .[2:] else . end) end;
JQ
}

# Run an adapter, capturing stdout/stderr separately. Non-zero exit with valid
# JSON on stdout is still a run: several adapters exit 1 when their findings
# would block a release, and that is a finding, not a failure.
# Run in repo with timeout_s seconds allowed. Set ADAPTER_OUT and ADAPTER_ERR
# to temporary capture files and ADAPTER_RC to the exit status (124 on timeout)
# without propagating it. The caller must remove the files; creation failures
# are not caught.
run_adapter() { # binary args... ; sets ADAPTER_OUT, ADAPTER_ERR, ADAPTER_RC
  local bin="$1"; shift
  ADAPTER_OUT=$(mktemp); ADAPTER_ERR=$(mktemp); ADAPTER_RC=0
  # --bin-dir is also prepended to PATH: panicbot finds panic-attack with `which`.
  ( cd "$repo" && PATH="${bindir:+$bindir:}$PATH" RUST_LOG=off timeout "$timeout_s" "$bin" "$@" ) > "$ADAPTER_OUT" 2> "$ADAPTER_ERR" || ADAPTER_RC=$?
}
# Print up to 240 characters of the first nonblank ADAPTER_ERR line excluding
# Stack backtrace headers, or an ADAPTER_RC diagnostic if no line is selected.
first_err_line() {
  local l
  l=$(grep -v '^\s*$' "$ADAPTER_ERR" 2>/dev/null | grep -v '^Stack backtrace' | head -n1 | cut -c1-240)
  [ -n "$l" ] && echo "$l" || echo "exit status $ADAPTER_RC with no diagnostic"
}
# Remove the most recent adapter's stdout and stderr capture files.
cleanup_adapter() { rm -f "$ADAPTER_OUT" "$ADAPTER_ERR"; }

# Translate when stdout parses with the given jq program (which must yield the
# findings array), accepting nonzero adapter exits with usable output. Exit
# status 124 always produces a timeout error; invalid output produces an error
# with a stderr diagnostic. Bind $repo for relativize_filter, write the member's
# envelope, and remove the capture files. Envelope I/O failures are not caught.
translate_or_error() { # member jq-program
  local m="$1" prog="$2" findings
  if [ "$ADAPTER_RC" -eq 124 ]; then
    write_envelope "$m" error "adapter exceeded the ${timeout_s}s timeout" '[]'
  elif findings=$(jq -c --arg repo "$repo" "$(relativize_filter) $prog" "$ADAPTER_OUT" 2> "$ADAPTER_ERR.jq") \
       && [ -n "$findings" ] && jq -e 'type == "array"' <<<"$findings" >/dev/null 2>&1; then
    write_envelope "$m" ran "" "$findings"
  elif grep -q 'unexpected output shape' "$ADAPTER_ERR.jq" 2>/dev/null; then
    # The shape guard fired: valid JSON, but not what this adapter documents.
    write_envelope "$m" error "adapter output had an $(grep -o 'unexpected output shape: [^(]*' "$ADAPTER_ERR.jq" | head -n1 | sed 's/ *$//')" '[]'
  else
    write_envelope "$m" error "adapter produced no readable JSON: $(first_err_line)" '[]'
  fi
  rm -f "$ADAPTER_ERR.jq"
  cleanup_adapter
}

# Every adapter program begins with its shape guard: a crash or a regression that
# prints `{}` or `42` must become an error envelope, never a clean scan with no
# findings (the per-member filters would otherwise default a missing array to []).
SHAPE_ARRAY='if type != "array" then error("unexpected output shape: expected an array") else . end | '
SHAPE_OBJ_FINDINGS='if type != "object" or (.findings | type) != "array" then error("unexpected output shape: expected an object with a findings array") else . end | '
# finishing-bot and glambot print their analyzers keyed by name (an object, or
# an array), each with a findings array; an empty set means the run died.
SHAPE_ANALYZERS='if (type == "object" or type == "array") and length > 0 and all(.[]; type == "object" and (.findings | type) == "array") then . else error("unexpected output shape: expected analyzer objects each with a findings array") end | '

# One finding shape for every adapter: { rule_id, severity, file, line, message }
FINDING='{rule_id: (.rule_id // .id // "?" | tostring), severity: (.severity // "unknown" | tostring),
          file: ((.file // .location // "") | rel), line: ((.line // 0) | if type == "number" then . else 0 end),
          message: (.message // "" | tostring)}'

# Run one member against repo and write its translated report-back envelope.
# echidnabot and sustainabot yield not_run, even with ECHIDNA_URL configured;
# unknown members, unavailable binaries, timeouts, and unusable output yield
# error envelopes. Envelope I/O failures are not converted to adapter errors.
run_member() {
  local m="$1" bin crate
  crate=$(crate_dir_of "$m")
  case "$m" in
    echidnabot)
      # echidnabot dispatches proofs to an ECHIDNA engine; it has no local
      # mode. Without an engine the honest row is not_run WITH the reason.
      if [ -z "${ECHIDNA_URL:-}" ]; then
        write_envelope "$m" not_run "no ECHIDNA engine endpoint is configured for this repository (ECHIDNA_URL unset); echidnabot verifies proofs by dispatching to an engine and has no local mode" '[]'
      else
        write_envelope "$m" not_run "ECHIDNA_URL is set, but this driver does not yet dispatch to an engine; the row turns green or red once that leg lands in gitbot-fleet" '[]'
      fi
      return ;;
    sustainabot)
      write_envelope "$m" not_run "reserved fleet slot: no sustainabot adapter exists (bots/sustainabot/README.adoc); OikosBot is a separate project" '[]'
      return ;;
  esac
  if [ -z "$crate" ]; then
    write_envelope "$m" error "unknown fleet member: no adapter is registered under this name" '[]'
    return
  fi
  if ! bin=$(find_binary "$(binary_of "$m")"); then
    write_envelope "$m" error "adapter binary '$(binary_of "$m")' not found (searched ${bindir:-<no --bin-dir>} and PATH); build bots/$crate" '[]'
    return
  fi
  case "$m" in
    rhodibot)
      # canon --path: file-presence criteria against the vendored RSR canon.
      # "satisfied" rows are not findings; missing > deprecated > relocated.
      run_adapter "$bin" canon --path "$repo" --format json
      translate_or_error "$m" "$SHAPE_OBJ_FINDINGS"'
        [ .findings[]
          | select(.severity != "satisfied")
          # A relocated or deprecated file is reported at the path where it WAS
          # found, so the grid can match it against the PR diff; a missing one
          # can only be named by where the canon keeps it.
          | { rule_id: .id,
              file: ((if .severity == "missing" then (.canon_keeps_it_at // (.expected // [])[0] // "")
                      else ((.found // [])[0] // .canon_keeps_it_at // (.expected // [])[0] // "") end) | tostring),
              line: 0,
              severity: ({missing: "error", deprecated: "warning", relocated: "info"}[.severity] // .severity),
              message: ("\(.name): \(.severity)" + (if (.details // []) | length > 0 then " — " + ((.details // []) | join("; ")) else "" end)) }
          | .file |= rel ]' ;;
    panicbot)
      # `scan` translates panic-attack output into fleet Findings and writes
      # nothing. (`fleet` mode would write a .panicbot/*.a2ml debt register
      # into the scanned tree; A2ML is retired, so it is not used.)
      run_adapter "$bin" --format json scan "$repo"
      translate_or_error "$m" "$SHAPE_ARRAY [ .[] | $FINDING ]" ;;
    cipherbot)
      run_adapter "$bin" --format json scan "$repo"
      translate_or_error "$m" "$SHAPE_ARRAY [ .[] | $FINDING ]" ;;
    seambot)
      run_adapter "$bin" --path "$repo" --format json check
      translate_or_error "$m" "$SHAPE_OBJ_FINDINGS"'
        [ .findings[]
          | { rule_id: ("SEAM-" + (.check // "?" | tostring)), severity: (.severity // "unknown" | tostring),
              file: ((.location // "") | rel), line: 0, message: (.message // "" | tostring) } ]' ;;
    finishingbot|finishbot|finishing-bot)
      run_adapter "$bin" --path "$repo" --format json audit
      translate_or_error "$m" "$SHAPE_ANALYZERS [ .[] | .findings[] | $FINDING ]" ;;
    glambot)
      run_adapter "$bin" --path "$repo" --format json audit
      translate_or_error "$m" "$SHAPE_ANALYZERS [ .[] | .findings[] | $FINDING ]" ;;
    accessibilitybot)
      run_adapter "$bin" fleet "$repo"
      translate_or_error "$m" "$SHAPE_OBJ_FINDINGS [ .findings[] | $FINDING ]" ;;
    *)
      write_envelope "$m" error "no invocation is defined for $m" '[]' ;;
  esac
}

if [ "${#members[@]}" -eq 0 ]; then
  echo "fleet-report-back: no enrolled members (manifest: ${enrol:-none}); nothing written" >&2
fi
for m in "${members[@]+"${members[@]}"}"; do
  run_member "$m"
  echo "fleet-report-back: $m -> $(jq -r '.status + (if .status == "ran" then " (\(.findings | length) finding(s))" else ": " + (.reason // "") end)' "$out/$m.json")"
done

# ── summary ──────────────────────────────────────────────────────────────────
# Print a Markdown summary of this run from the members' envelope files, or a
# note if no members were selected. Envelope read/parse errors are not caught.
render_summary() {
  echo "### gitbot-fleet report-backs"
  echo
  echo "ref \`$fleet_ref\` · repository \`$repo\` · manifest \`${enrol:-none}\`"
  echo
  if [ "${#members[@]}" -eq 0 ]; then
    echo "_No member is enrolled, so no report-back was produced._"
    return
  fi
  echo "| Member | Status | Findings | Reason |"
  echo "|---|---|---:|---|"
  for m in "${members[@]}"; do
    jq -r '"| \(.reporter) | \(.status) | \(.findings | length) | \(.reason // "") |"' "$out/$m.json"
  done
}
if [ -n "$summary" ]; then render_summary >> "$summary"; fi
exit 0
