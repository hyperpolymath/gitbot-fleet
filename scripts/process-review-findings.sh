#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# process-review-findings.sh — Create GitHub issues for review-tier findings
#
# Reads pending findings from shared-context/findings/pending/ and creates
# GitHub issues with appropriate labels and suggested remediation steps.
#
# Substitute-tier findings suggest proven module replacements that need
# human review to integrate properly. Issues are created per-repo with
# all findings for that repo aggregated into a single issue.
#
# Usage:
#   process-review-findings.sh [OPTIONS]
#
# Options:
#   --dry-run         Show what would be created without creating
#   --repo REPO       Only process findings for a specific repo
#   --limit N         Process at most N repos
#   --label LABEL     Additional label for issues (default: safety-triangle)

set -euo pipefail

FLEET_BASE="${FLEET_BASE:-/var/mnt/eclipse/repos/gitbot-fleet}"
PENDING_DIR="${FLEET_BASE}/shared-context/findings/pending"
GH_OWNER="hyperpolymath"

DRY_RUN=false
FILTER_REPO=""
LIMIT=0
EXTRA_LABEL=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)    DRY_RUN=true; shift ;;
        --repo)       FILTER_REPO="$2"; shift 2 ;;
        --limit)      LIMIT="$2"; shift 2 ;;
        --label)      EXTRA_LABEL="$2"; shift 2 ;;
        -h|--help)    head -20 "$0" | tail -18; exit 0 ;;
        *)            echo "Unknown option: $1" >&2; exit 1 ;;
    esac
done

if [[ ! -d "$PENDING_DIR" ]]; then
    echo "No pending findings at: $PENDING_DIR"
    exit 0
fi

FINDING_COUNT=$(ls "$PENDING_DIR"/*.json 2>/dev/null | wc -l)
echo "=== Review Finding Processor ==="
echo "  Pending:  $FINDING_COUNT findings"
echo "  Dry run:  $DRY_RUN"
[[ -n "$FILTER_REPO" ]] && echo "  Repo:     $FILTER_REPO"
echo ""

# Group findings by repo
declare -A REPO_FINDINGS
for f in "$PENDING_DIR"/*.json; do
    [[ ! -f "$f" ]] && continue
    repo=$(jq -r '.repo' "$f" 2>/dev/null)
    [[ -z "$repo" || "$repo" == "null" ]] && continue
    [[ -n "$FILTER_REPO" && "$repo" != "$FILTER_REPO" ]] && continue
    REPO_FINDINGS["$repo"]+="$f "
done

REPO_COUNT=${#REPO_FINDINGS[@]}
echo "Found findings for $REPO_COUNT repos"
echo ""

CREATED=0
SKIPPED=0

for repo in $(echo "${!REPO_FINDINGS[@]}" | tr ' ' '\n' | sort); do
    if [[ "$LIMIT" -gt 0 && "$CREATED" -ge "$LIMIT" ]]; then
        echo "Limit reached ($LIMIT repos)"
        break
    fi

    files=(${REPO_FINDINGS[$repo]})
    finding_count=${#files[@]}

    echo "  $repo: $finding_count findings"

    # Build issue body
    body="## Safety Triangle Review Findings\n\n"
    body+="Hypatia identified **$finding_count** finding(s) that need human review.\n"
    body+="These are **substitute-tier** findings — a safer proven module exists but integration requires review.\n\n"
    body+="### Findings\n\n"

    for f in "${files[@]}"; do
        [[ ! -f "$f" ]] && continue
        tier=$(jq -r '.tier // "unknown"' "$f")
        category=$(jq -r '.category // "unknown"' "$f")
        description=$(jq -r '.description // "No description"' "$f")
        confidence=$(jq -r '.confidence // 0' "$f")
        proven_module=$(jq -r '.proven_module // "none"' "$f")
        proven_modules=$(jq -r '.proven_modules // [] | join(", ")' "$f")
        formally_proven=$(jq -r '.formally_proven // false' "$f")
        recipe_id=$(jq -r '.recipe_id // "none"' "$f")
        severity=$(jq -r '.severity // "Medium"' "$f")

        body+="#### \`$category\` ($severity)\n"
        body+="- **Description:** $description\n"
        body+="- **Confidence:** $confidence\n"
        body+="- **Recipe:** \`$recipe_id\`\n"

        if [[ "$proven_module" != "none" && "$proven_module" != "null" ]]; then
            body+="- **Suggested replacement:** \`proven/$proven_module\`"
            if [[ "$proven_modules" != "" ]]; then
                body+=" (alternatives: $proven_modules)"
            fi
            body+="\n"
            if [[ "$formally_proven" == "true" ]]; then
                body+="- **Formally verified** in Idris2 (highest assurance)\n"
            fi
        fi
        body+="\n"
    done

    body+="### Next Steps\n\n"
    body+="1. Review each finding and assess whether the suggested proven module is appropriate\n"
    body+="2. Check that language bindings exist for this repo's primary language\n"
    body+="3. Import the proven module and replace the unsafe pattern\n"
    body+="4. Run tests to verify behavior is preserved\n"
    body+="5. Close this issue when all findings are addressed\n\n"
    body+="---\n"
    body+="_Created by hypatia via gitbot-fleet safety triangle pipeline._\n"

    title="safety-triangle: $finding_count substitute-tier finding(s) need review"
    labels="safety-triangle,substitute-tier,hypatia"
    [[ -n "$EXTRA_LABEL" ]] && labels+=",${EXTRA_LABEL}"

    if [[ "$DRY_RUN" == "true" ]]; then
        echo "    (dry-run) Would create issue: '$title'"
        echo "    Labels: $labels"
        ((CREATED++)) || true
    else
        # Check if gh is available and repo exists on GitHub
        if command -v gh &>/dev/null; then
            if gh repo view "$GH_OWNER/$repo" &>/dev/null 2>&1; then
                if echo -e "$body" | gh issue create \
                    --repo "$GH_OWNER/$repo" \
                    --title "$title" \
                    --body-file - \
                    --label "$labels" 2>/dev/null; then
                    echo "    Issue created"
                    ((CREATED++)) || true

                    # Move processed findings to done/
                    mkdir -p "$FLEET_BASE/shared-context/findings/done"
                    for f in "${files[@]}"; do
                        [[ -f "$f" ]] && mv "$f" "$FLEET_BASE/shared-context/findings/done/"
                    done
                else
                    echo "    WARN: gh issue create failed (labels may not exist)"
                    ((SKIPPED++)) || true
                fi
            else
                echo "    SKIP: $repo not found on GitHub"
                ((SKIPPED++)) || true
            fi
        else
            echo "    SKIP: gh CLI not available"
            ((SKIPPED++)) || true
        fi
    fi
done

echo ""
echo "=== Summary ==="
echo "  Issues created: $CREATED"
echo "  Skipped:        $SKIPPED"

if [[ "$DRY_RUN" == "true" ]]; then
    echo ""
    echo "(Dry run — no issues were created)"
fi
