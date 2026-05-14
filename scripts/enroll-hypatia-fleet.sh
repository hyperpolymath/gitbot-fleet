#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later

set -euo pipefail

usage() {
    cat <<USAGE
usage: $(basename "$0") [options]

Discover and enroll repositories for gitbot-fleet/hypatia maintenance coverage.

Options:
  --repos-root <path>   Root containing repos (default: /var$REPOS_DIR)
  --registry <path>     Registry JSON output
                        (default: shared-context/enrollment/repos.json)
  --apply               Write enrollment directives into discovered repos
  --help, -h            Show this help
USAGE
}

repos_root="/var$REPOS_DIR"
registry=""
apply=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --repos-root)
            repos_root="${2:-}"
            shift 2
            ;;
        --registry)
            registry="${2:-}"
            shift 2
            ;;
        --apply)
            apply=true
            shift
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        *)
            echo "error: unknown argument: $1" >&2
            usage >&2
            exit 2
            ;;
    esac
done

if ! command -v jq >/dev/null 2>&1; then
    echo "error: jq is required" >&2
    exit 127
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
fleet_root="$(cd "${script_dir}/.." && pwd)"

repos_root="$(cd "$repos_root" && pwd)"
if [[ -z "$registry" ]]; then
    registry="${fleet_root}/shared-context/enrollment/repos.json"
elif [[ "$registry" != /* ]]; then
    registry="${fleet_root}/${registry}"
fi

mkdir -p "$(dirname "$registry")"

tmp_objects="$(mktemp)"
trap 'rm -f "$tmp_objects"' EXIT

old_registry="$registry"
declare -A known
if [[ -f "$old_registry" ]]; then
    while IFS= read -r repo_name; do
        [[ -n "$repo_name" ]] && known["$repo_name"]=1
    done < <(jq -r '.repos[]?.name' "$old_registry" 2>/dev/null || true)
fi

enrolled_count=0
applied_count=0

while IFS= read -r repo_path; do
    repo_name="$(basename "$repo_path")"
    [[ "$repo_name" == ".git" ]] && continue

    has_machine_readable=false
    has_bot_directives=false
    has_a2ml_meta=false
    has_a2ml_state=false
    has_maint_script=false
    has_hypatia_signal=false
    already_enrolled=false
    applied=false

    [[ -d "$repo_path/.machine_readable" ]] && has_machine_readable=true
    [[ -d "$repo_path/.machine_readable/bot_directives" ]] && has_bot_directives=true
    [[ -f "$repo_path/.machine_readable/META.a2ml" ]] && has_a2ml_meta=true
    [[ -f "$repo_path/.machine_readable/STATE.a2ml" ]] && has_a2ml_state=true
    if [[ -x "$repo_path/scripts/maintenance/run-maintenance.sh" || -x "$repo_path/run-maintenance.sh" ]]; then
        has_maint_script=true
    fi
    if find "$repo_path/.github/workflows" -type f \( -name '*.yml' -o -name '*.yaml' \) -print 2>/dev/null | xargs -r rg -i -l 'hypatia|submit-finding|panic-attack' >/dev/null 2>&1; then
        has_hypatia_signal=true
    fi
    if [[ -f "$repo_path/.machine_readable/bot_directives/FLEET-ENROLLMENT.a2ml" ]]; then
        already_enrolled=true
    fi

    is_new=false
    if [[ -z "${known[$repo_name]:-}" ]]; then
        is_new=true
    fi

    if [[ "$apply" == true && "$has_machine_readable" == true ]]; then
        mkdir -p "$repo_path/.machine_readable/bot_directives"
        cat > "$repo_path/.machine_readable/bot_directives/FLEET-ENROLLMENT.a2ml" <<DIRECTIVE
# SPDX-License-Identifier: PMPL-1.0-or-later
# Auto-managed by gitbot-fleet/scripts/enroll-hypatia-fleet.sh

[enrollment]
fleet = true
hypatia = true
maintenance-hard-pass-required = true
registered-at = "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
DIRECTIVE
        applied=true
        already_enrolled=true
        applied_count=$((applied_count + 1))
    fi

    if [[ "$already_enrolled" == true ]]; then
        enrolled_count=$((enrolled_count + 1))
    fi

    jq -n \
        --arg name "$repo_name" \
        --arg path "$repo_path" \
        --argjson is_new "$is_new" \
        --argjson enrolled "$already_enrolled" \
        --argjson applied "$applied" \
        --argjson has_machine_readable "$has_machine_readable" \
        --argjson has_bot_directives "$has_bot_directives" \
        --argjson has_a2ml_meta "$has_a2ml_meta" \
        --argjson has_a2ml_state "$has_a2ml_state" \
        --argjson has_maint_script "$has_maint_script" \
        --argjson has_hypatia_signal "$has_hypatia_signal" \
        '{
            name: $name,
            path: $path,
            is_new_since_last_registry: $is_new,
            enrolled: $enrolled,
            enrollment_applied_now: $applied,
            capabilities: {
                machine_readable: $has_machine_readable,
                bot_directives: $has_bot_directives,
                meta_a2ml: $has_a2ml_meta,
                state_a2ml: $has_a2ml_state,
                maintenance_script: $has_maint_script,
                hypatia_signal: $has_hypatia_signal
            }
        }' >> "$tmp_objects"
done < <(find "$repos_root" -mindepth 1 -maxdepth 1 -type d -exec test -d '{}/.git' ';' -print | sort)

jq -s \
    --arg generated_at "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
    --arg repos_root "$repos_root" \
    --arg registry "$registry" \
    --argjson apply "$apply" \
    --argjson enrolled_count "$enrolled_count" \
    --argjson applied_count "$applied_count" \
    '{
        generated_at: $generated_at,
        repos_root: $repos_root,
        registry_path: $registry,
        apply_mode: $apply,
        summary: {
            total_repos: length,
            enrolled_repos: $enrolled_count,
            enrollment_applied_now: $applied_count
        },
        repos: sort_by(.name)
    }' "$tmp_objects" > "$registry"

echo "registry written: $registry"
jq '.summary' "$registry"
