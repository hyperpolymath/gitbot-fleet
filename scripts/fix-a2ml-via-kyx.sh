#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# fix-a2ml-via-kyx.sh — convert a repository's retired .a2ml files with kyX,
# or fail loudly. Owner directive 2026-11-17: rid the estate of A2ML,
# opportunistically, when a bot sees it as a priority-1 task (rhodibot
# `rhodibot.no-a2ml` / RSR-A2ML-001).
#
# Conversion is kyX's front end and nothing else (D313): this script NEVER
# hand-converts, never guesses a field, and never edits a deed. For each
# tracked .a2ml it either
#
#   * converts it: kyX translates it under the standards mapping spec, and the
#     output must pass every gate below; the deed is staged, the .a2ml is
#     staged for deletion, and a PR body is written; or
#   * refuses it, with every reason, and writes a P1 issue body. Nothing in the
#     tree changes for a refused file.
#
# The gates, all of them, for every converted file:
#   1. kyX exits 0. kyX is fail-closed: a table, field or value shape its
#      mapping spec does not list is refused with its line (exit 1).
#   2. kyX drops nothing. kyX reports what spec §4 lets it drop (empty values,
#      comment lines, @directives) on stderr; any such line is information
#      that does not reach the deed, so it is a refusal. A2ML_ACCEPT_SPEC_DROPS=1
#      accepts drops the spec sanctions and lists each in the PR body instead
#      (an owner decision, off by default).
#   3. The deed parses under the canon grammar: standards
#      `deed_lint.js --ecosystem --root <repo>` (deed.abnf + ecosystem.abnf).
#      A .k9/.k9.ncl output, when a kyX front end emits one, must pass
#      standards `k9-validate.sh --strict` (K9_VALIDATE); none does today.
#   4. No value is lost: scripts/lib/a2ml-census.js checks that every non-empty
#      scalar value in the source appears in the deed (deed_lint --ast-json).
#      Independent of kyX, and conservative: a value a spec rewrites reads as
#      lost, so the error is always towards refusing.
#
# kyX translates ECOSYSTEM.a2ml only (metadatastician/kyx README, "Status");
# every other family is refused here as "no kyX front end", with the mapping
# spec that will decide it when there is one. An ECOSYSTEM.a2ml is converted
# only at a canonical location (.machine_readable/descriptiles/, 6a2/, or the
# retired direct .machine_readable/ layout), into
# .machine_readable/descriptiles/ECOSYSTEM.deed, and only when the repository
# has exactly one chora deed: kyX reads the id from it and never mints one.
#
# Writes in the repository: only the staged deed and the staged deletion of
# the .a2ml it replaces. Never commits, never pushes, never opens anything:
# the caller opens a PR on a branch (never the default branch) and the P1
# issue. Report files go to A2ML_REPORT_DIR (default: a fresh temp directory,
# printed), never into the tree:
#   a2ml-report.json    {converted: [...], refused: [...], ...}
#   a2ml-pr.md          when something was converted
#   a2ml-p1-issue.md    when something was refused (fingerprinted, so a caller
#                       can find an open issue for the same refusal)
#
# Environment (needed only when a file is routed to kyX):
#   KYX               path to kyX's built Bun module (_build/kyx/kyx.bun.js)
#   KYX_COMMIT        the 40-hex kyX commit it was built from (recorded)
#   DEED_LINT         path to standards 1-formats/deed/tools/deed_lint.js
#   STANDARDS_COMMIT  the 40-hex standards commit DEED_LINT is from (recorded)
#   AFFIRMED          YYYY-MM-DD for :affirmed (default: today, UTC)
#   K9_VALIDATE       standards 1-formats/k9/tools/k9-validate.sh (k9 outputs)
#
# Idempotent: a converted file is gone on the next run; a refused one is
# refused again with the same fingerprint.
#
# Exit: 0 no .a2ml left (converted, or none to begin with) · 2 refused and
# nothing converted (P1 issue) · 3 partial: some converted (PR), some refused
# (P1 issue) — a dispatcher must not record 2 or 3 as a clean success · 64
# usage · 69 a file needs kyX and a tool or its provenance is missing.
#
# outcome-predicate: --check
#
# Usage:
#   fix-a2ml-via-kyx.sh <repo-path> [finding.json]
#   fix-a2ml-via-kyx.sh --check <repo-path>     print the number of tracked .a2ml
set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CENSUS="$SELF_DIR/lib/a2ml-census.js"

usage() { sed -n '/^# Usage:/,/^set -euo/p' "$0" | sed '$d; s/^# \{0,1\}//' >&2; exit 64; }
die() { printf 'fix-a2ml-via-kyx: %s\n' "$1" >&2; exit "${2:-2}"; }

# The tracked .a2ml files, NUL-safe, sorted, case-insensitive suffix.
a2ml_files() {
  git -C "$1" ls-files -z | tr '\0' '\n' | grep -iE '\.a2ml$' | LC_ALL=C sort || true
}

check_mode=0
if [[ "${1:-}" == "--check" ]]; then check_mode=1; shift; fi
[[ $# -ge 1 && $# -le 2 ]] || usage
REPO="$1"
[[ -d "$REPO" ]] || die "not a directory: $REPO" 64
git -C "$REPO" rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not a git work tree: $REPO" 64
REPO="$(cd "$REPO" && git rev-parse --show-toplevel)"

if [[ "$check_mode" -eq 1 ]]; then
  a2ml_files "$REPO" | grep -c . || true
  exit 0
fi

mapfile -t FILES < <(a2ml_files "$REPO")
if [[ "${#FILES[@]}" -eq 0 ]]; then
  echo "fix-a2ml-via-kyx: no .a2ml in $REPO; nothing to do"
  exit 0
fi

for f in "${FILES[@]}"; do
  case "$f" in
    *$'\n'*|*$'\t'*|-*) die "refusing a tracked path with a control character or leading dash: ${f@Q}" 2 ;;
  esac
done

REPORT_DIR="${A2ML_REPORT_DIR:-}"
if [[ -z "$REPORT_DIR" ]]; then REPORT_DIR="$(mktemp -d "${TMPDIR:-/tmp}/a2ml-kyx.XXXXXX")"; fi
mkdir -p "$REPORT_DIR"
case "$(cd "$REPORT_DIR" && pwd)/" in "$REPO"/*) die "A2ML_REPORT_DIR must be outside the repository" 64 ;; esac
WORK="$(mktemp -d "${TMPDIR:-/tmp}/a2ml-kyx-work.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

: > "$WORK/converted.jsonl"
: > "$WORK/refused.jsonl"

refuse() { # path family spec reason...
  local path="$1" family="$2" spec="$3"; shift 3
  jq -nc --arg path "$path" --arg family "$family" --arg spec "$spec" \
    --args '{path:$path, family:$family, spec:(if $spec=="" then null else $spec end), reasons:$ARGS.positional}' \
    "$@" >> "$WORK/refused.jsonl"
}

# family <path> -> "family<TAB>spec<TAB>kyx(0|1)"; the table rhodibot's
# canon/a2ml.rs reports from, so the finding and the fixer agree.
family_of() {
  local lower name
  lower="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')"
  name="${lower##*/}"
  case "$name" in
    ecosystem.a2ml) printf 'ECOSYSTEM\tmeta-ecosystem-to-repo-deed.adoc\t1\n' ;;
    meta.a2ml) printf 'META\tmeta-ecosystem-to-repo-deed.adoc\t0\n' ;;
    state.a2ml) printf 'STATE\tstate-v1-decision.adoc\t0\n' ;;
    agentic.a2ml) printf 'AGENTIC\tagentic-neurosym-playbook-to-repo-deed.adoc\t0\n' ;;
    neurosym.a2ml) printf 'NEUROSYM\tagentic-neurosym-playbook-to-repo-deed.adoc\t0\n' ;;
    playbook.a2ml) printf 'PLAYBOOK\tagentic-neurosym-playbook-to-repo-deed.adoc\t0\n' ;;
    clade.a2ml) printf 'CLADE\tclade-to-repo-deed.adoc\t0\n' ;;
    0-ai-manifest.a2ml) printf 'AI-MANIFEST\tai-manifest-decision.adoc\t0\n' ;;
    anchor.a2ml) printf 'ANCHOR\t\t0\n' ;;
    rsr-profile.a2ml) printf 'RSR-PROFILE\t\t0\n' ;;
    rsr-criteria-v2.a2ml) printf 'RSR-CRITERIA\trsr-criteria-to-praxis-deed.adoc\t0\n' ;;
    *)
      if [[ "$lower" == *bot_directives/* ]]; then printf 'BOT-DIRECTIVES\t\t0\n'
      elif [[ "$name" == *file.a2ml ]]; then printf 'CONTRACTILE\t\t0\n'
      else printf 'UNMAPPED\t\t0\n'; fi ;;
  esac
}

# Canonical ECOSYSTEM.a2ml sources (anything else is refused, not guessed).
is_canonical_ecosystem() {
  case "$1" in
    .machine_readable/descriptiles/ECOSYSTEM.a2ml|.machine_readable/6a2/ECOSYSTEM.a2ml|.machine_readable/ECOSYSTEM.a2ml) return 0 ;;
    *) return 1 ;;
  esac
}
TARGET=".machine_readable/descriptiles/ECOSYSTEM.deed"

need_tools() {
  local missing=()
  [[ -n "${KYX:-}" && -f "${KYX:-}" ]] || missing+=("KYX (kyX's built kyx.bun.js)")
  [[ "${KYX_COMMIT:-}" =~ ^[0-9a-f]{40}$ ]] || missing+=("KYX_COMMIT (40-hex)")
  [[ -n "${DEED_LINT:-}" && -f "${DEED_LINT:-}" ]] || missing+=("DEED_LINT (standards deed_lint.js)")
  [[ "${STANDARDS_COMMIT:-}" =~ ^[0-9a-f]{40}$ ]] || missing+=("STANDARDS_COMMIT (40-hex)")
  command -v bun >/dev/null 2>&1 || missing+=("bun")
  command -v jq >/dev/null 2>&1 || missing+=("jq")
  if [[ "${#missing[@]}" -gt 0 ]]; then
    printf 'fix-a2ml-via-kyx: a file needs kyX, and these are missing: %s\n' "${missing[*]}" >&2
    printf 'fix-a2ml-via-kyx: nothing was changed; this is an operator error, not the repository'"'"'s\n' >&2
    exit 69
  fi
}

AFFIRMED="${AFFIRMED:-$(date -u +%Y-%m-%d)}"
[[ "$AFFIRMED" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || die "AFFIRMED must be YYYY-MM-DD" 64
ACCEPT_DROPS="${A2ML_ACCEPT_SPEC_DROPS:-0}"
[[ "$ACCEPT_DROPS" == 0 || "$ACCEPT_DROPS" == 1 ]] || die "A2ML_ACCEPT_SPEC_DROPS must be 0 or 1" 64

# Exactly one chora deed at the root (D312), or none to convert against.
shopt -s nullglob
choras=("$REPO"/*_chora.deed)
shopt -u nullglob

eco_sources=()
for f in "${FILES[@]}"; do
  IFS=$'\t' read -r fam spec kyx <<< "$(family_of "$f")"
  if [[ "$kyx" != 1 ]]; then
    if [[ -n "$spec" ]]; then
      refuse "$f" "$fam" "$spec" "kyX has no front end for $fam yet; $spec decides where it goes. Do not hand-convert (D313)."
    else
      refuse "$f" "$fam" "" "no mapping spec decides $fam yet, and kyX has no front end for it. Do not hand-convert (D313)."
    fi
    continue
  fi
  if ! is_canonical_ecosystem "$f"; then
    refuse "$f" "$fam" "$spec" "not at a canonical descriptile location, so it may describe another tree (a template, a vendored copy); kyX needs the chora deed of the tree it describes."
    continue
  fi
  eco_sources+=("$f")
done

if [[ "${#eco_sources[@]}" -gt 1 ]]; then
  for f in "${eco_sources[@]}"; do
    refuse "$f" ECOSYSTEM meta-ecosystem-to-repo-deed.adoc "more than one ECOSYSTEM.a2ml (${eco_sources[*]}) for one $TARGET; which is authoritative is a human decision."
  done
  eco_sources=()
fi

for src in "${eco_sources[@]}"; do
  spec=meta-ecosystem-to-repo-deed.adoc
  if [[ "${#choras[@]}" -ne 1 ]]; then
    refuse "$src" ECOSYSTEM "$spec" "kyX reads :canonical-name and the chora id from the repository's one *_chora.deed and never mints one (U1); found ${#choras[@]}."
    continue
  fi
  if [[ -e "$REPO/$TARGET" ]]; then
    refuse "$src" ECOSYSTEM "$spec" "$TARGET already exists; it is never overwritten. Reconcile the two by hand-review, not by hand-conversion."
    continue
  fi
  need_tools
  out="$WORK/ECOSYSTEM.deed"; rm -f "$out" "$WORK/kyx.err" "$WORK/ast.json"
  rc=0
  bun "$KYX" a2ml ecosystem "$REPO/$src" --chora "${choras[0]}" --affirmed "$AFFIRMED" --out "$out" \
    > "$WORK/kyx.out" 2> "$WORK/kyx.err" || rc=$?
  mapfile -t kyx_err < "$WORK/kyx.err"
  if [[ "$rc" -eq 1 ]]; then
    mapfile -t why < <(grep '^kyx: refused: ' "$WORK/kyx.err" | sed 's/^kyx: refused: /kyX refused: /')
    [[ "${#why[@]}" -gt 0 ]] || why=("kyX refused it (exit 1) without a reason line: ${kyx_err[*]:-}")
    refuse "$src" ECOSYSTEM "$spec" "${why[@]}"
    continue
  elif [[ "$rc" -ne 0 || ! -s "$out" ]]; then
    refuse "$src" ECOSYSTEM "$spec" "kyX failed (exit $rc), so nothing is known about the conversion: ${kyx_err[*]:-no output}"
    continue
  fi
  mapfile -t drops < <(grep '^kyx: dropped' "$WORK/kyx.err" || true)
  if [[ "${#drops[@]}" -gt 0 && "$ACCEPT_DROPS" != 1 ]]; then
    refuse "$src" ECOSYSTEM "$spec" "not lossless: kyX dropped content the deed does not carry (spec §4); A2ML_ACCEPT_SPEC_DROPS is off:" "${drops[@]}"
    continue
  fi
  if ! bun "$DEED_LINT" --ecosystem --root "$REPO" "$out" > "$WORK/lint.out" 2>&1; then
    mapfile -t lint < "$WORK/lint.out"
    refuse "$src" ECOSYSTEM "$spec" "kyX's output does not pass standards deed_lint --ecosystem --root (canon grammar):" "${lint[@]}"
    continue
  fi
  for k9 in "$WORK"/*.k9 "$WORK"/*.k9.ncl; do
    [[ -e "$k9" ]] || continue
    if [[ -z "${K9_VALIDATE:-}" ]] || ! bash "$K9_VALIDATE" --strict "$k9" > "$WORK/k9.out" 2>&1; then
      refuse "$src" ECOSYSTEM "$spec" "a k9 output does not pass standards k9-validate.sh --strict (or K9_VALIDATE is unset)"
      continue 2
    fi
  done
  if ! bun "$DEED_LINT" --ast-json "$out" > "$WORK/ast.json" 2> "$WORK/ast.err"; then
    refuse "$src" ECOSYSTEM "$spec" "deed_lint --ast-json could not read kyX's output: $(head -c 400 "$WORK/ast.err")"
    continue
  fi
  crc=0
  census_json="$(bun "$CENSUS" check "$REPO/$src" "$WORK/ast.json" 2> "$WORK/census.err")" || crc=$?
  if [[ "$crc" -ne 0 ]]; then
    if [[ "$crc" -eq 1 ]]; then
      mapfile -t lost < <(jq -r '.missing[] | "value not found in the deed: " + (tojson)' <<< "$census_json")
      refuse "$src" ECOSYSTEM "$spec" "not lossless: the value census found source values the deed does not carry:" "${lost[@]}"
    else
      refuse "$src" ECOSYSTEM "$spec" "the value census could not read the source, so losslessness is unproven: $(head -c 400 "$WORK/census.err")"
    fi
    continue
  fi
  # Every gate passed: stage the deed and the deletion.
  mkdir -p "$REPO/$(dirname "$TARGET")"
  cp "$out" "$REPO/$TARGET"
  git -C "$REPO" add -- "$TARGET"
  git -C "$REPO" rm -q -- "$src"
  jq -nc --arg source "$src" --arg target "$TARGET" --arg kyx "$KYX_COMMIT" --arg std "$STANDARDS_COMMIT" \
    --arg affirmed "$AFFIRMED" --argjson census "$census_json" --args \
    '{source:$source, target:$target, kyx_commit:$kyx, standards_commit:$std, affirmed:$affirmed,
      values_checked:$census.source_values, spec_drops:$ARGS.positional}' \
    "${drops[@]}" >> "$WORK/converted.jsonl"
done

repo_name="$(basename "$REPO")"
jq -n --arg repo "$repo_name" --arg accept "$ACCEPT_DROPS" \
  --slurpfile converted "$WORK/converted.jsonl" --slurpfile refused "$WORK/refused.jsonl" \
  '{repo:$repo, accept_spec_drops:($accept=="1"), converted:$converted, refused:$refused}' \
  > "$REPORT_DIR/a2ml-report.json"
rm -f "$REPORT_DIR/a2ml-pr.md" "$REPORT_DIR/a2ml-p1-issue.md"

n_conv="$(jq '.converted | length' "$REPORT_DIR/a2ml-report.json")"
n_ref="$(jq '.refused | length' "$REPORT_DIR/a2ml-report.json")"

if [[ "$n_conv" -gt 0 ]]; then
  jq -r '
    "## Retire A2ML: kyX conversion\n",
    "A2ML is retired (D99, D269c, D312, D313). Each file below was converted by **kyX** under its standards mapping spec. Nothing was converted by hand.\n",
    "| Source (deleted) | Deed (added) | kyX | standards | values checked |",
    "|---|---|---|---|---|",
    (.converted[] | "| `\(.source)` | `\(.target)` | `\(.kyx_commit[0:12])` | `\(.standards_commit[0:12])` | \(.values_checked) |"),
    "",
    "Gates passed for every file: kyX exit 0 (fail-closed), " + (if .accept_spec_drops then "spec §4 drops accepted by A2ML_ACCEPT_SPEC_DROPS=1 and listed below" else "no content dropped" end) + ", `deed_lint --ecosystem --root` (canon grammar), and the value census (every source value is in the deed).",
    (if any(.converted[]; (.spec_drops|length) > 0) then ("\nDropped under spec §4:\n" + ([.converted[] | .spec_drops[] | "- " + .] | join("\n"))) else empty end),
    "",
    "Review it like any PR: the deed is owner-facing, and :affirmed records the date of this conversion.\n",
    "_Opened by gitbot-fleet `scripts/fix-a2ml-via-kyx.sh` (opportunistic A2ML retirement)._"
  ' "$REPORT_DIR/a2ml-report.json" > "$REPORT_DIR/a2ml-pr.md"
fi

if [[ "$n_ref" -gt 0 ]]; then
  fp="$(jq -cS '[.refused[] | {path, reasons}]' "$REPORT_DIR/a2ml-report.json" | sha256sum | cut -c1-16)"
  jq -r --arg fp "$fp" '
    "<!-- a2ml-kyx:fingerprint=\($fp) -->",
    "## P1: retired A2ML that kyX cannot convert (\(.refused | length) file(s))\n",
    "A2ML is retired (D99, D269c, D312, D313), and conversion is kyX'"'"'s front end only (D313). These files could not be converted **losslessly by kyX**, so nothing was changed. **Do not hand-convert them.**\n",
    (.refused | group_by(.family)[] |
      "### \(.[0].family)" + (if .[0].spec then " — `1-formats/deed/mappings/\(.[0].spec)`" else " — no mapping spec yet" end) + "\n",
      (.[] | "- `\(.path)`\n" + ([.reasons[] | "  - " + .] | join("\n"))),
      ""),
    "What unblocks each: a kyX front end for the family (metadatastician/kyx), a mapping spec where there is none, or a fix to the source that kyX names.\n",
    "_Opened by gitbot-fleet `scripts/fix-a2ml-via-kyx.sh`; it will not open a second issue for the same refusal._"
  ' "$REPORT_DIR/a2ml-report.json" > "$REPORT_DIR/a2ml-p1-issue.md"
fi

echo "fix-a2ml-via-kyx: $n_conv converted, $n_ref refused; reports in $REPORT_DIR"
if [[ "$n_ref" -gt 0 ]]; then
  jq -r '.refused[] | "REFUSED\t\(.path)\t\(.reasons[0])"' "$REPORT_DIR/a2ml-report.json" >&2
fi
if [[ "$n_ref" -eq 0 ]]; then exit 0; fi
if [[ "$n_conv" -gt 0 ]]; then exit 3; fi
exit 2
