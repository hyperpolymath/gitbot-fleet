#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# detect-rsr-drift.sh — is the RSR rule table still describing the estate?
#
# Rhodibot validates repositories against the RSR rule table in
# bots/rhodibot/src/rsr.rs. That table is the *source of truth*, and a rule
# table can rot in two distinct ways:
#
#   1. FALSE DEMAND — a Required check names a path that (almost) no repository
#      has. The historical case is LICENSE.txt: 0 of 269 repositories carried
#      it, so every repo failed a "Required" check it could not satisfy.
#   2. STALE WINDOW — a check is deliberately held at an advisory severity
#      while a migration window is open, and the window closed without anyone
#      raising the severity. The live case is .well-known/security.txt, whose
#      canonical location moved to www/.well-known/ (#53).
#
# This script measures the estate against the source table and reports both.
# It is deliberately offline-first: given a directory of per-repo file listings
# it needs no network and no token at all.
#
# It can additionally probe the live GitHub API for evidence of a *deployed*
# runner, because a rule table that is not running anywhere enforces nothing.
#
# Exit codes: 0 = clean, 1 = drift/rot found, 2 = could not determine.

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

log_info()  { printf '%b[INFO]%b %s\n'  "$BLUE"   "$NC" "$*" >&2; }
log_warn()  { printf '%b[WARN]%b %s\n'  "$YELLOW" "$NC" "$*" >&2; }
log_error() { printf '%b[FAIL]%b %s\n'  "$RED"    "$NC" "$*" >&2; }

# ---------------------------------------------------------------- defaults ---
SOURCE=""
TREES_DIR=""
ESTATE_LIST=""
PROBE_OWNER=""
PROBE_COUNT=5
APP_SLUG="rhodibot"
JSON=0
ROT_THRESHOLD=50          # Required check below this prevalence = rot
MIN_CENSUS="${MIN_CENSUS:-50}"   # below this the census is a sample, not a census
MIGRATION_TARGET="www/.well-known/security.txt"
MIGRATION_LEGACY=".well-known/security.txt"
DRIFT_FOUND=0
INDETERMINATE=0
n_rot_real=0

while [ $# -gt 0 ]; do
    case "$1" in
        --source)      SOURCE="$2"; shift 2 ;;
        --trees-dir)   TREES_DIR="$2"; shift 2 ;;
        --estate-list) ESTATE_LIST="$2"; shift 2 ;;
        --probe)       PROBE_OWNER="$2"; shift 2 ;;
        --probe-count) PROBE_COUNT="$2"; shift 2 ;;
        --app-slug)    APP_SLUG="$2"; shift 2 ;;
        --rot-threshold) ROT_THRESHOLD="$2"; shift 2 ;;
        --json)        JSON=1; shift ;;
        -h|--help)
            sed -n '2,30p' "$0" | sed 's/^# \{0,1\}//'
            exit 0 ;;
        *) log_error "unknown option: $1"; exit 2 ;;
    esac
done

# Locate the rule table if not given explicitly.
if [ -z "$SOURCE" ]; then
    for cand in "bots/rhodibot/src/rsr.rs" "$(dirname "$0")/../bots/rhodibot/src/rsr.rs"; do
        [ -f "$cand" ] && SOURCE="$cand" && break
    done
fi
if [ -z "$SOURCE" ] || [ ! -f "$SOURCE" ]; then
    log_error "cannot find rhodibot rule table (--source bots/rhodibot/src/rsr.rs)"
    exit 2
fi

TMPDIR_RUN="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_RUN"' EXIT

printf '%b%s%b\n' "$BOLD" "RSR rule-table drift detector" "$NC" >&2
printf '  source: %s\n' "$SOURCE" >&2
printf '\n' >&2

# ---------------------------------------------------- 1. parse the source ---
# Records open on `CheckDef {` / `BannedPattern {` and close on a line that is
# just `},`. Within a record the first `name: "…"` and first `severity: (…)`
# belong together. Brace depth is tracked so nested braces cannot confuse it.

parse_table() {
    local array_name="$1" struct_name="$2"
    # Record boundaries cannot be found by brace counting alone: line 365 of the
    # canonical rsr.rs is `},    BannedPattern {` — a closer and an opener on one
    # line. Counting braces there opens and immediately closes a record with no
    # name, silently dropping the entry that follows (historically the
    # .well-known/security.txt ban, i.e. the migration's own kill-switch).
    #
    # So: a record opens on any line containing `<Struct> {`, flushing any
    # record still open; and closes on a line that is nothing but `},` / `}`.
    awk -v arr="$array_name" -v st="$struct_name" '
        function flush() {
            if (open && name != "") print name "\t" sev
            open=0; name=""; sev=""
        }
        $0 ~ "^pub const " arr { inarr=1; next }
        inarr && /^\];/ { flush(); inarr=0 }
        !inarr { next }
        $0 ~ st "[ \t]*\\{" {
            flush()
            open=1
        }
        open {
            if (name == "" && match($0, /name: "[^"]+"/))
                name = substr($0, RSTART+7, RLENGTH-8)
            if (sev == "" && match($0, /severity: \([^)]*\)/)) {
                # The match is `severity: (A, B, C, D)`; take only what is
                # between the parens. "severity: " is 10 chars, plus the "(".
                sev = substr($0, RSTART + 11, RLENGTH - 12)
                gsub(/Severity::/, "", sev); gsub(/[ \t]/, "", sev)
            }
            if ($0 ~ /^[ \t]*\},?[ \t]*$/) flush()
        }
        END { flush() }
    ' "$SOURCE"
}

parse_table "REQUIRED_FILES" "CheckDef"          > "$TMPDIR_RUN/required.tsv"
parse_table "BANNED_PATTERNS" "BannedPattern"    > "$TMPDIR_RUN/banned.tsv"

n_req=$(wc -l < "$TMPDIR_RUN/required.tsv" | tr -d ' ')
n_ban=$(wc -l < "$TMPDIR_RUN/banned.tsv"   | tr -d ' ')

# Severity is read positionally later (field 1,2 = minimal,standard), so a
# malformed tuple silently shifts every downstream verdict. Assert the shape
# rather than trusting the substring arithmetic.
sev_shape='^(Optional|Recommended|Required),(Optional|Recommended|Required),(Optional|Recommended|Required),(Optional|Recommended|Required)$'
bad_sev=$( { cut -f2 "$TMPDIR_RUN/required.tsv"; cut -f2 "$TMPDIR_RUN/banned.tsv"; } \
           | grep -vcE "$sev_shape" || true )
if [ "$bad_sev" != "0" ]; then
    log_error "severity tuples did not parse cleanly ($bad_sev malformed):"
    { cut -f1,2 "$TMPDIR_RUN/required.tsv"; cut -f1,2 "$TMPDIR_RUN/banned.tsv"; } \
        | grep -vE "	$sev_shape" | sed 's/^/    /' >&2 || true
    exit 2
fi

# Self-check: the number of parsed records must equal the number of literal
# record openings, or we are reading a different table than we think.
# Self-check: the number of parsed records must equal the number of literal
# record openings, or we are reading a different table than we think. The
# opener is matched anywhere on the line, because an opener can share a line
# with the previous record's closer; struct/impl definitions are excluded.
expect_req=$(grep -E 'CheckDef \{' "$SOURCE" | grep -vcE '^pub struct|^impl ' || true)
expect_ban=$(grep -E 'BannedPattern \{' "$SOURCE" | grep -vcE '^pub struct|^impl ' || true)

if [ "$n_req" != "$expect_req" ] || [ "$n_ban" != "$expect_ban" ]; then
    log_error "parser self-check failed: parsed $n_req/$expect_req CheckDef" \
              "and $n_ban/$expect_ban BannedPattern records"
    exit 2
fi
log_info "rule table: $n_req required-file checks, $n_ban banned patterns"

# Required workflows live in a plain tuple list inside the checks function.
grep -oE '^\s+\("[a-z-]+\.yml",' "$SOURCE" | grep -oE '"[a-z-]+\.yml"' | tr -d '"' \
    | sort -u > "$TMPDIR_RUN/workflows.txt"
n_wf=$(wc -l < "$TMPDIR_RUN/workflows.txt" | tr -d ' ')
log_info "rule table: $n_wf required workflows"

# --------------------------------------------------- 2. estate conformance ---
# A repository is represented by a plain file listing (one path per line), as
# produced by `git ls-tree -r --name-only` or the GitHub trees API.

census_total=0
if [ -n "$TREES_DIR" ] && [ -d "$TREES_DIR" ]; then
    if [ -n "$ESTATE_LIST" ] && [ -f "$ESTATE_LIST" ]; then
        mapfile -t REPOS < <(sed '/^$/d' "$ESTATE_LIST")
    else
        mapfile -t REPOS < <(find "$TREES_DIR" -maxdepth 1 -type f -printf '%f\n' | sort)
    fi

    : > "$TMPDIR_RUN/present.tsv"
    : > "$TMPDIR_RUN/rootlegacy.tsv"

    for repo in "${REPOS[@]}"; do
        tree="$TREES_DIR/$repo"
        [ -f "$tree" ] || { INDETERMINATE=1; continue; }
        census_total=$((census_total + 1))

        while IFS=$'\t' read -r path sev; do
            [ -z "$path" ] && continue
            if grep -qxF "$path" "$tree"; then
                printf '%s\t%s\n' "$path" "$repo" >> "$TMPDIR_RUN/present.tsv"
            fi
        done < "$TMPDIR_RUN/required.tsv"

        # Legacy location residue: the migration's own finish line.
        if grep -qxF '.well-known/security.txt' "$tree" \
           && ! grep -qxF 'www/.well-known/security.txt' "$tree"; then
            printf '%s\n' "$repo" >> "$TMPDIR_RUN/rootlegacy.tsv"
        fi

        while IFS=$'\t' read -r path sev; do
            [ -z "$path" ] && continue
            if grep -qxF "$path" "$tree"; then
                printf '%s\t%s\n' "$path" "$repo" >> "$TMPDIR_RUN/bannedpresent.tsv"
            fi
        done < "$TMPDIR_RUN/banned.tsv"
    done

    log_info "census: $census_total repositories measured"
fi

# ------------------------------------------------------- 3. live deployment ---
# The rule table only matters if something is running it. Look for the bot's
# observable footprint: check runs attributed to its GitHub App.
probe_apps=""
probe_runs=0
probe_repos=0
if [ -n "$PROBE_OWNER" ]; then
    token="${GH_TOKEN:-${GITHUB_TOKEN:-}}"
    if [ -z "$token" ]; then
        log_warn "--probe needs GH_TOKEN or GITHUB_TOKEN; skipping live probe"
        INDETERMINATE=1
    else
        mapfile -t PROBE_REPOS < <(sed '/^$/d' "${ESTATE_LIST:-/dev/null}" | head -n "$PROBE_COUNT")
        if [ "${#PROBE_REPOS[@]}" -eq 0 ]; then
            log_warn "--probe needs --estate-list to choose repos; skipping"
            INDETERMINATE=1
        fi
        : > "$TMPDIR_RUN/apps.txt"
        for repo in "${PROBE_REPOS[@]}"; do
            resp=$(curl -sS \
                -H "Authorization: Bearer $token" \
                -H "Accept: application/vnd.github+json" \
                "https://api.github.com/repos/$PROBE_OWNER/$repo/commits/HEAD/check-runs?per_page=100" \
                2>/dev/null || echo '{}')
            probe_repos=$((probe_repos + 1))
            got=$(printf '%s' "$resp" | jq -r '.check_runs | length' 2>/dev/null || echo 0)
            case "$got" in ''|*[!0-9]*) got=0; INDETERMINATE=1 ;; esac
            probe_runs=$((probe_runs + got))
            printf '%s' "$resp" | jq -r '.check_runs[]?.app.slug // empty' 2>/dev/null \
                >> "$TMPDIR_RUN/apps.txt" || true
        done
        probe_apps=$(sort -u "$TMPDIR_RUN/apps.txt" | paste -sd, - )
        log_info "live probe: $probe_repos repos, $probe_runs check runs, apps=[$probe_apps]"
    fi
fi

deploy_verdict="NOT-PROBED"
if [ -n "$probe_apps" ]; then
    if printf '%s' ",$probe_apps," | grep -qF ",$APP_SLUG,"; then
        deploy_verdict="LIVE"
    else
        deploy_verdict="ABSENT"
    fi
fi

# --------------------------------------------------------------- 4. report ---
# The window posture is declared in the source table itself.
window_posture="unknown"
canon_sev=$(grep -P '^www/\.well-known/security\.txt\t' "$TMPDIR_RUN/required.tsv" | cut -f2 || true)
ban_sev=$(grep -P '^\.well-known/security\.txt\t' "$TMPDIR_RUN/banned.tsv" | cut -f2 || true)
if [ -n "$canon_sev" ] && [ -n "$ban_sev" ]; then
    canon_min_std=$(printf '%s' "$canon_sev" | cut -d, -f1,2)
    ban_min_std=$(printf '%s' "$ban_sev" | cut -d, -f1,2)
    if [ "$ban_min_std" = "Optional,Optional" ] || [ "$canon_min_std" = "Optional,Optional" ]; then
        window_posture="advisory"
    else
        window_posture="enforced"
    fi
fi

declare -a ROT_ROWS=()
while IFS=$'\t' read -r path sev; do
    [ -z "$path" ] && continue
    std="$(printf '%s' "$sev" | cut -d, -f2)"
    count=0
    [ -f "$TMPDIR_RUN/present.tsv" ] && \
        count=$(grep -cF "$(printf '%s\t' "$path")" "$TMPDIR_RUN/present.tsv" || true)
    pct=0
    [ "$census_total" -gt 0 ] && pct=$(( count * 100 / census_total ))

    flag=""
    if [ "$census_total" -gt 0 ]; then
        if [ "$count" -eq 0 ]; then
            flag="DEAD"
        elif [ "$std" = "Required" ] && [ "$pct" -lt "$ROT_THRESHOLD" ]; then
            flag="ROT"
        fi
    fi
    # A zero-prevalence check that names the destination of a migration the
    # source itself still declares advisory is pending work, not rot. Reporting
    # it as rot would make this detector cry wolf on every sweep in progress.
    if [ "$flag" = "DEAD" ] && [ "$path" = "$MIGRATION_TARGET" ] && [ "$window_posture" = "advisory" ]; then
        flag="PENDING-MIGRATION"
    fi
    # A small census cannot distinguish "absent from the estate" from "absent
    # from this sample": .machine_readable/STATE.a2ml is 13/269 estate-wide but
    # 0/8 in a sample of eight. Flags are reported either way, but only a real
    # census is allowed to fail the run — weak evidence must not block.
    if [ "$census_total" -gt 0 ] && [ "$census_total" -lt "$MIN_CENSUS" ] \
       && { [ "$flag" = "ROT" ] || [ "$flag" = "DEAD" ]; }; then
        flag="$flag(sample)"
        INDETERMINATE=1
    fi
    [ "$flag" = "ROT" ] || [ "$flag" = "DEAD" ] && n_rot_real=$((n_rot_real + 1)) || true
    [ -n "$flag" ] && ROT_ROWS+=("$(printf '%s\t%s\t%s\t%s\t%s\t%s' "$flag" "$path" "$count" "$census_total" "$pct" "$std")")
done < "$TMPDIR_RUN/required.tsv"

n_root_legacy=0
[ -f "$TMPDIR_RUN/rootlegacy.tsv" ] && n_root_legacy=$(wc -l < "$TMPDIR_RUN/rootlegacy.tsv" | tr -d ' ')

migrated=0
if [ "$census_total" -gt 0 ]; then
    migrated=$(( census_total - n_root_legacy ))
fi
mig_pct=0
[ "$census_total" -gt 0 ] && mig_pct=$(( migrated * 100 / census_total ))

if [ "$census_total" -gt 0 ] && [ "$census_total" -lt "$MIN_CENSUS" ]; then
    # A sample of mostly-migrated repos reaches mig_pct=100 and would then
    # advise raising .well-known checks to Required — advice that would break
    # every repository the sample missed. Refuse to draw that conclusion.
    drift_window="SAMPLE-TOO-SMALL"
    INDETERMINATE=1
elif [ "$mig_pct" -eq 100 ] && [ "$census_total" -gt 0 ] && [ "$window_posture" = "advisory" ]; then
    drift_window="WINDOW-CLOSED-BUT-STILL-ADVISORY"
elif [ "$mig_pct" -lt 100 ] && [ "$window_posture" = "enforced" ]; then
    drift_window="ENFORCED-BEFORE-MIGRATION-COMPLETE"
else
    drift_window="OK"
fi

# SAMPLE-TOO-SMALL is indeterminate, not drift: refusing to judge is not the
# same as finding something wrong, and it must not block a commit.
if [ "$n_rot_real" -gt 0 ] \
   || { [ "$drift_window" != "OK" ] && [ "$drift_window" != "SAMPLE-TOO-SMALL" ]; } \
   || [ "$deploy_verdict" = "ABSENT" ]; then
    DRIFT_FOUND=1
fi

if [ "$JSON" -eq 1 ]; then
    jq -n \
        --arg source "$SOURCE" \
        --argjson required "$n_req" \
        --argjson banned "$n_ban" \
        --argjson workflows "$n_wf" \
        --argjson census "$census_total" \
        --arg deploy "$deploy_verdict" \
        --arg apps "$probe_apps" \
        --argjson runs "$probe_runs" \
        --argjson rootlegacy "$n_root_legacy" \
        --argjson migrated_pct "$mig_pct" \
        --arg window "$drift_window" \
        --arg posture "$window_posture" \
        --argjson rot "$(printf '%s\n' "${ROT_ROWS[@]:-}" | jq -R -s 'split("\n") | map(select(length>0) | split("\t") | {flag:.[0],path:.[1],present:(.[2]|tonumber),total:(.[3]|tonumber),percent:(.[4]|tonumber),severity:.[5]})')" \
        '{source:$source, rule_table:{required_files:$required,banned_patterns:$banned,required_workflows:$workflows},
          estate:{measured:$census}, deployment:{verdict:$deploy, apps:$apps, check_runs:$runs},
          migration:{repos_on_legacy_root:$rootlegacy, migrated_percent:$migrated_pct, window_posture:$posture, verdict:$window},
          rot:$rot, drift_found:($rot|length>0)}'
    exit $([ "$DRIFT_FOUND" -eq 1 ] && echo 1 || { [ "$INDETERMINATE" -eq 1 ] && echo 2 || echo 0; })
fi

printf '%b1. DEPLOYMENT%b\n' "$BOLD" "$NC"
printf '   verdict: %s\n' "$deploy_verdict"
if [ -n "$probe_apps" ]; then
    printf '   evidence: %s repos, %s check runs; apps seen: %s\n' \
        "$probe_repos" "$probe_runs" "$probe_apps"
    if [ "$deploy_verdict" = "ABSENT" ]; then
        printf '   %bno check run in the estate is attributed to %s.%b\n' "$YELLOW" "$APP_SLUG" "$NC"
        printf '   RSR compliance is therefore enforced only by in-repo workflows,\n'
        printf '   and the rule table below is a specification, not a running control.\n'
    fi
fi
printf '\n'

printf '%b2. RULE TABLE%b (source of truth)\n' "$BOLD" "$NC"
printf '   %s required-file checks, %s banned patterns, %s required workflows\n' \
    "$n_req" "$n_ban" "$n_wf"
printf '\n'

printf '%b3. RULE TABLE vs ESTATE%b\n' "$BOLD" "$NC"
if [ "$census_total" -eq 0 ]; then
    printf '   (no census — pass --trees-dir to measure)\n'
else
    printf '   measured against %s repositories\n' "$census_total"
    if [ "$census_total" -lt "$MIN_CENSUS" ]; then
        printf '   %bSAMPLE, not a census: below %s repos, absence proves little.%b\n' \
            "$YELLOW" "$MIN_CENSUS" "$NC"
        printf '   Flags are marked (sample) and cannot fail the run.\n'
    fi
    printf '   %-34s %7s %6s  %s\n' "CHECK" "PRESENT" "PCT" "SEVERITY"
    while IFS=$'\t' read -r path sev; do
        [ -z "$path" ] && continue
        std="$(printf '%s' "$sev" | cut -d, -f2)"
        count=$(grep -cF "$(printf '%s\t' "$path")" "$TMPDIR_RUN/present.tsv" 2>/dev/null || true)
        count=${count:-0}; count=$(printf '%s' "$count" | head -n1)
        pct=$(( count * 100 / census_total ))
        mark=""
        if [ "$count" -eq 0 ]; then mark="${RED}DEAD${NC}"
        elif [ "$std" = "Required" ] && [ "$pct" -lt "$ROT_THRESHOLD" ]; then mark="${YELLOW}ROT${NC}"
        fi
        [ "$path" = "$MIGRATION_TARGET" ] && [ "$count" -eq 0 ] && [ "$window_posture" = "advisory" ] \
            && mark="${BLUE}PENDING-MIGRATION${NC}"
        if [ -n "$mark" ] && [ "$census_total" -lt "$MIN_CENSUS" ]; then
            mark="$mark${YELLOW}(sample)${NC}"
        fi
        printf '   %-34s %4s/%-4s %5s%%  %-12s %b\n' "$path" "$count" "$census_total" "$pct" "$std" "$mark"
    done < "$TMPDIR_RUN/required.tsv"
fi
printf '\n'

printf '%b4. MIGRATION WINDOW%b (root .well-known/ -> www/.well-known/)\n' "$BOLD" "$NC"
printf '   repos still on legacy root path: %s\n' "$n_root_legacy"
printf '   migrated: %s%%\n' "$mig_pct"
printf '   declared posture in source: %s\n' "$window_posture"
case "$drift_window" in
    WINDOW-CLOSED-BUT-STILL-ADVISORY)
        printf '   %bVERDICT: the migration is complete but the checks are still advisory.%b\n' "$YELLOW" "$NC"
        printf '   Raise www/.well-known/security.txt to Required at minimal/standard,\n'
        printf '   and .well-known/security.txt to Required at minimal/standard,\n'
        printf '   as the comment in %s instructs.\n' "$SOURCE" ;;
    ENFORCED-BEFORE-MIGRATION-COMPLETE)
        printf '   %bVERDICT: enforcing before the sweep finished — %s repos would fail.%b\n' \
            "$RED" "$n_root_legacy" "$NC" ;;
    SAMPLE-TOO-SMALL)
        printf '   %bVERDICT: cannot judge — the census is a sample of %s repos.%b\n' \
            "$YELLOW" "$census_total" "$NC"
        printf '   Concluding "window closed" from a sample would advise raising\n'
        printf '   severities that most repositories would then fail.\n' ;;
    OK)
        printf '   %bVERDICT: posture matches estate reality.%b\n' "$GREEN" "$NC" ;;
esac
if [ "$n_root_legacy" -gt 0 ] && [ "$n_root_legacy" -le 25 ]; then
    printf '   remaining repos:\n'
    sed 's/^/     - /' "$TMPDIR_RUN/rootlegacy.tsv"
fi
printf '\n'

printf '%bSUMMARY%b\n' "$BOLD" "$NC"
if [ "$DRIFT_FOUND" -eq 1 ]; then
    printf '   %bDRIFT FOUND%b\n' "$RED" "$NC"
else
    printf '   %bno drift%b\n' "$GREEN" "$NC"
fi
if [ "$INDETERMINATE" -eq 1 ]; then
    printf '   %bsome inputs were unavailable — result is partial%b\n' "$YELLOW" "$NC"
fi

if [ "$DRIFT_FOUND" -eq 1 ]; then exit 1; fi
if [ "$INDETERMINATE" -eq 1 ]; then exit 2; fi
exit 0
