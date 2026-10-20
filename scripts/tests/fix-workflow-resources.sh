#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Resource boundaries for the workflow fixers changed in PR #615. Run alone
# or through fix-workflow-pins.sh. All mutations and command stubs stay in
# disposable fixtures; no GitHub requests or background processes are needed.
set -euo pipefail
# Child shells must use fixture commands rather than a user's startup file.
unset BASH_ENV
fleet="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
fx="$(mktemp -d)"
trap 'rm -rf "$fx"' EXIT
mkdir -m 700 "$fx/tmp" "$fx/locks"
export TMPDIR="$fx/tmp" PIN_FIXER_LOCK_DIR="$fx/locks" PIN_FIXER_LOCK_WAIT=0
export TOOLCHAIN=stable
pass=0; fail=0

check() {
    local label="$1"; shift
    if "$@"; then
        pass=$((pass + 1)); echo "  ok   $label"
    else
        fail=$((fail + 1)); echo "  FAIL $label" >&2
    fi
}
run_fix() { rc=0; out=$("$@" 2>&1) || rc=$?; }
empty_dir() { [ -z "$(find "$1" -mindepth 1 -print -quit)" ]; }
same_inputs() {
    cmp -s "$1/.github/workflows/ci.yml" "$2/.github/workflows/ci.yml" &&
        cmp -s "$1/.github/workflows/actions.lock" "$2/.github/workflows/actions.lock"
}

stale=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
good=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
rust_sha=cccccccccccccccccccccccccccccccccccccccc
action_sha=dddddddddddddddddddddddddddddddddddddddd
printf 'fixture/reusables/.github/workflows/*\t%s\t%s\tfixture repair\n' "$stale" "$good" > "$fx/pins.tsv"
printf 'fixture/action\t%s\tv1.2.3\n' "$action_sha" > "$fx/tags.tsv"
export PINS_TABLE="$fx/pins.tsv" TAG_MAP="$fx/tags.tsv"

make_repo() {
    mkdir -p "$1/.github/workflows"
    cat > "$1/.github/workflows/ci.yml" <<YML
jobs:
  reusable:
    uses: fixture/reusables/.github/workflows/ci.yml@$stale
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: dtolnay/rust-toolchain@$rust_sha
      - uses: fixture/action@$action_sha
YML
    cat > "$1/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'fixture/reusables@$stale'
        - 'fixture/action@$action_sha'
dependencies:
    'fixture/reusables@$stale':
        ref: '$stale'
        commit: 'sha1-$stale'
    'fixture/action@$action_sha':
        ref: '$action_sha'
        commit: 'sha1-$action_sha'
LOCK
}

# Acquire through the production helper before starting the child. This
# handshake is synchronous: zero-wait contention cannot race a sleep.
with_pin_lock() {
    # Keep this shell alive after the child command: Bash may otherwise exec
    # the last command, letting the child replace the holder's descriptor.
    bash -c '. "$1"; pin_fixer_lock "$2"; shift 2; "$@"; rc=$?; exit "$rc"' _ \
        "$fleet/scripts/lib/pin-fixer-lock.sh" "$@"
}

# Every fixer must share the lock even through a symlink, and a lock for one
# checkout must not stop a different checkout with the same basename.
for kind in toolchain stale-pin self-sha; do
    case_root="$fx/$kind"
    repo="$case_root/locked/repo"
    other="$case_root/independent/repo"
    make_repo "$repo"
    make_repo "$other"
    cp -R "$repo" "$case_root/before"
    ln -s "$repo" "$case_root/alias"
    args=()
    case "$kind" in
        toolchain)
            script="$fleet/scripts/fix-rust-toolchain-input.sh"
            target="$case_root/alias/.github/workflows/ci.yml"
            expected_rc=0 ;;
        stale-pin)
            script="$fleet/scripts/fix-stale-reusable-pin.sh"
            target="$case_root/alias/."
            expected_rc=3 ;;
        self-sha)
            script="$fleet/scripts/fix-lock-self-sha-refs.sh"
            target="$case_root/alias"
            args=(--offline)
            expected_rc=0 ;;
    esac
    run_fix with_pin_lock "$repo" bash "$script" "$target" "${args[@]}"
    check "$kind refuses a held lock through an alias" test "$rc" -eq 2
    check "$kind explains lock contention" grep -q 'another workflow-pin fixer holds' <<<"$out"
    check "$kind leaves both files untouched while locked" same_inputs "$repo" "$case_root/before"

    run_fix with_pin_lock "$repo" bash "$script" "$other" "${args[@]}"
    check "$kind can repair another checkout with the same basename" test "$rc" -eq "$expected_rc"
    check "$kind actually changes the independent checkout" test "$(same_inputs "$other" "$case_root/before" && echo same || echo changed)" = changed
    check "$kind keeps the locked checkout intact during the independent repair" same_inputs "$repo" "$case_root/before"

    run_fix bash "$script" "$target" "${args[@]}"
    check "$kind repairs after the holder exits" test "$rc" -eq "$expected_rc"
    check "$kind produces the same repair through an alias" same_inputs "$repo" "$other"
    run_fix bash "$script" "$target" "${args[@]}"
    check "$kind succeeds without further edits on rerun" test "$rc" -eq 0
    check "$kind rerun preserves both files exactly" same_inputs "$repo" "$other"
done

# Check the helper's remaining directory guards directly, without relying on
# a particular editor. A configured directory must already be private.
mkdir -m 770 "$fx/group-writable"
printf 'keep this file\n' > "$fx/regular-file"
cp "$fx/regular-file" "$fx/regular-file.before"
for invalid_dir in "$fx/missing" "$fx/regular-file" "$fx/group-writable"; do
    run_fix env PIN_FIXER_LOCK_DIR="$invalid_dir" bash -c \
        '. "$1"; pin_fixer_lock "$2"; echo acquired' _ \
        "$fleet/scripts/lib/pin-fixer-lock.sh" "$repo"
    check "lock helper rejects $(basename "$invalid_dir")" test "$rc" -eq 2
    check "lock helper diagnoses $(basename "$invalid_dir")" grep -q 'not a private directory' <<<"$out"
done
check "a file passed as the lock directory is preserved" cmp -s "$fx/regular-file.before" "$fx/regular-file"
check "a missing configured lock directory is not silently created" test ! -e "$fx/missing"

# XDG_RUNTIME_DIR takes precedence over TMPDIR. The default child directory
# is private even when the base is shared; an explicit directory wins over
# both environment defaults.
mkdir -m 1777 "$fx/runtime" "$fx/fallback"
run_fix env -u PIN_FIXER_LOCK_DIR XDG_RUNTIME_DIR="$fx/runtime" TMPDIR="$fx/fallback" \
    bash -c '. "$1"; pin_fixer_lock "$2"' _ "$fleet/scripts/lib/pin-fixer-lock.sh" "$repo"
check "the helper can use XDG_RUNTIME_DIR" test "$rc" -eq 0
check "the runtime lock directory has mode 0700" test "$(stat -c %a "$fx/runtime/pin-fixer-$(id -u)")" = 700
check "TMPDIR stays untouched when XDG_RUNTIME_DIR is available" empty_dir "$fx/fallback"
make_repo "$fx/explicit-lock-repo"
runtime_locks_before=$(find "$fx/runtime" -type f | wc -l)
explicit_locks_before=$(find "$PIN_FIXER_LOCK_DIR" -type f | wc -l)
run_fix env XDG_RUNTIME_DIR="$fx/runtime" TMPDIR="$fx/fallback" \
    bash -c '. "$1"; pin_fixer_lock "$2"' _ "$fleet/scripts/lib/pin-fixer-lock.sh" "$fx/explicit-lock-repo"
check "an explicit private lock directory overrides both defaults" test "$rc" -eq 0
check "an explicit lock directory receives the new lock" \
    test "$(find "$PIN_FIXER_LOCK_DIR" -type f | wc -l)" -eq "$((explicit_locks_before + 1))"
check "an explicit lock directory leaves runtime locks unchanged" \
    test "$(find "$fx/runtime" -type f | wc -l)" -eq "$runtime_locks_before"
check "an explicit lock directory does not use the TMPDIR fallback" empty_dir "$fx/fallback"

# The timeout fixer changed to mktemp plus an EXIT trap in a per-file
# subshell. Cover mutation, no-op and failure after both temporaries exist.
timeouts="$fx/timeouts"
mkdir -p "$timeouts/.github/workflows" "$timeouts/tmp" "$timeouts/bin"
cat > "$timeouts/before.yml" <<'YML'
jobs:
  first:
    runs-on: ubuntu-latest
    steps: []
  existing:
    runs-on: ubuntu-latest
    timeout-minutes: 42
    steps: []
  last:
    runs-on: ubuntu-latest
    steps: []
YML
cat > "$timeouts/expected.yml" <<'YML'
jobs:
  first:
    runs-on: ubuntu-latest
    timeout-minutes: 17
    steps: []
  existing:
    runs-on: ubuntu-latest
    timeout-minutes: 42
    steps: []
  last:
    runs-on: ubuntu-latest
    timeout-minutes: 17
    steps: []
YML
cp "$timeouts/before.yml" "$timeouts/.github/workflows/a.yml"
cp "$timeouts/before.yml" "$timeouts/.github/workflows/b.yaml"
run_fix env TMPDIR="$timeouts/tmp" DEFAULT_TIMEOUT=17 bash "$fleet/scripts/fix-workflow-timeout-minutes.sh" "$timeouts"
check "timeout fixer repairs both workflow extensions" test "$rc" -eq 0
for workflow in a.yml b.yaml; do
    check "timeout fixer preserves existing limits and repairs multiple jobs in $workflow" \
        cmp -s "$timeouts/expected.yml" "$timeouts/.github/workflows/$workflow"
done
check "timeout fixer removes temporary files after repairs" empty_dir "$timeouts/tmp"
run_fix env TMPDIR="$timeouts/tmp" DEFAULT_TIMEOUT=99 bash "$fleet/scripts/fix-workflow-timeout-minutes.sh" "$timeouts"
check "timeout no-op succeeds with a different default" test "$rc" -eq 0
check "timeout no-op preserves the repaired workflow" cmp -s "$timeouts/expected.yml" "$timeouts/.github/workflows/a.yml"
check "timeout no-op removes its scan temporary" empty_dir "$timeouts/tmp"
cat > "$timeouts/bin/sed" <<'SH'
#!/usr/bin/env bash
echo 'fixture: insertion failed' >&2
exit 47
SH
chmod +x "$timeouts/bin/sed"
cp "$timeouts/before.yml" "$timeouts/failure.yml"
run_fix env TMPDIR="$timeouts/tmp" PATH="$timeouts/bin:$PATH" \
    bash "$fleet/scripts/fix-workflow-timeout-minutes.sh" "$timeouts/failure.yml"
check "timeout insertion errors remain failures" test "$rc" -eq 47
check "timeout insertion failure preserves the source workflow" cmp -s "$timeouts/before.yml" "$timeouts/failure.yml"
check "timeout insertion failure removes both temporary files" empty_dir "$timeouts/tmp"

# Exercise the changed allowlist temporary's lifecycle with an inert gh.
# Unknown commands fail, so a new API call cannot escape to the real CLI.
policy="$fx/policy"
mkdir -p "$policy/bin"
cat > "$policy/bin/gh" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$POLICY_CASE/calls"
case "$*" in
    'api repos/hyperpolymath/standards/contents/.machine_readable/bot_exclusion_registry.a2ml --jq .content')
        printf 'excluded = []\n' | base64 ;;
    'api users/fixture-owner --jq .type') printf 'User\n' ;;
    'api repos/fixture-owner/demo/actions/permissions')
        printf '%s\n' '{"allowed_actions":"selected","sha_pinning_required":true}' ;;
    'api repos/fixture-owner/demo/actions/permissions/selected-actions --jq .patterns_allowed|length')
        printf '0\n' ;;
    'api repos/hyperpolymath/standards/actions/permissions/selected-actions')
        case "$POLICY_SCENARIO" in
            fetch-failure) exit 48 ;;
            empty) printf '%s\n' '{"patterns_allowed":[]}' ;;
            invalid) printf 'not json\n' ;;
            *) printf '%s\n' '{"patterns_allowed":["fixture/*"]}' ;;
        esac ;;
    'api --method PUT repos/fixture-owner/demo/actions/permissions/selected-actions --input '*)
        [ "$POLICY_SCENARIO" != put-failure ] || exit 47
        cp "${@: -1}" "$POLICY_CASE/applied.json" ;;
    *) echo "unexpected fixture gh call: $*" >&2; exit 99 ;;
esac
SH
chmod +x "$policy/bin/gh"
for scenario in success fetch-failure empty invalid put-failure; do
    policy_case="$policy/$scenario"
    mkdir -p "$policy_case/tmp"
    run_fix env TMPDIR="$policy_case/tmp" PATH="$policy/bin:$PATH" \
        POLICY_CASE="$policy_case" POLICY_SCENARIO="$scenario" HYPATIA_AUTOMATION=on \
        bash "$fleet/scripts/fix-actions-policy.sh" fixture-owner --repo demo --apply
    case "$scenario" in success) expected_rc=0 ;; put-failure) expected_rc=47 ;; *) expected_rc=2 ;; esac
    check "allowlist $scenario reports the expected status" test "$rc" -eq "$expected_rc"
    check "allowlist $scenario reaches the download path" \
        grep -qx 'api repos/hyperpolymath/standards/actions/permissions/selected-actions' "$policy_case/calls"
    check "allowlist $scenario leaves no temporary files" empty_dir "$policy_case/tmp"
    if [ "$scenario" = success ]; then
        check "the downloaded allowlist reaches the mocked PUT intact" \
            jq -e '.patterns_allowed == ["fixture/*"]' "$policy_case/applied.json"
    else
        check "allowlist $scenario never applies a payload" test ! -e "$policy_case/applied.json"
    fi
done

# A user-supplied allowlist is not owned by the fixer, including on failure.
printf '%s\n' '{"patterns_allowed":["fixture/*"]}' > "$policy/curated.json"
cp "$policy/curated.json" "$policy/curated.before"
for scenario in success put-failure; do
    policy_case="$policy/curated-$scenario"
    mkdir -p "$policy_case/tmp"
    run_fix env TMPDIR="$policy_case/tmp" PATH="$policy/bin:$PATH" \
        POLICY_CASE="$policy_case" POLICY_SCENARIO="$scenario" HYPATIA_AUTOMATION=on \
        bash "$fleet/scripts/fix-actions-policy.sh" fixture-owner --repo demo --apply --allowlist "$policy/curated.json"
    expected_rc=0; [ "$scenario" != put-failure ] || expected_rc=47
    check "curated allowlist $scenario reports the expected status" test "$rc" -eq "$expected_rc"
    check "curated allowlist $scenario preserves the caller's file" cmp -s "$policy/curated.before" "$policy/curated.json"
    check "curated allowlist $scenario needs no temporary file" empty_dir "$policy_case/tmp"
    check "curated allowlist $scenario never downloads a replacement" \
        test "$(grep -c '^api repos/hyperpolymath/standards/actions/permissions/selected-actions$' "$policy_case/calls" || true)" -eq 0
done

# The maintenance report now persists under XDG_STATE_HOME with a private
# umask. A fixture runner exercises the real wrapper without running scans.
maintenance="$fx/maintenance"
mkdir -p "$maintenance/repo/scripts/maintenance" "$maintenance/case"
cat > "$maintenance/repo/scripts/maintenance/run-maintenance.sh" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "$MAINTENANCE_CASE/args"
while [ "$#" -gt 0 ]; do
    case "$1" in
        --repo) shift 2 ;;
        --output) output="$2"; shift 2 ;;
        --strict|--fail-on-warn) shift ;;
        *) exit 99 ;;
    esac
done
printf '%s\n' "$output" > "$MAINTENANCE_CASE/output-path"
printf '%s\n' '{"summary":{"fail":0,"warn":0}}' > "$output"
SH
chmod +x "$maintenance/repo/scripts/maintenance/run-maintenance.sh"
run_fix env XDG_STATE_HOME="$maintenance/state with spaces" REPOS_BASE="$maintenance/no-repos" \
    PANIC_ATTACK_BIN= MAINTENANCE_CASE="$maintenance/case" \
    bash -c 'umask 022; exec bash "$@"' _ "$fleet/scripts/maintenance-hard-pass.sh" --repo "$maintenance/repo"
check "maintenance writes its default report successfully" test "$rc" -eq 0
report=$(cat "$maintenance/case/output-path")
check "maintenance default report lives under XDG_STATE_HOME" \
    test "$(dirname "$report")" = "$maintenance/state with spaces/gitbot-fleet/maintenance"
check "maintenance default report is private despite the caller's umask" test "$(stat -c %a "$report")" = 600
check "maintenance passes the strict gate to its runner" grep -qx -- --strict "$maintenance/case/args"
check "maintenance passes the warning gate to its runner" grep -qx -- --fail-on-warn "$maintenance/case/args"
run_fix env XDG_STATE_HOME="$maintenance/unused-state" REPOS_BASE="$maintenance/no-repos" \
    PANIC_ATTACK_BIN= MAINTENANCE_CASE="$maintenance/case" \
    bash "$fleet/scripts/maintenance-hard-pass.sh" --repo "$maintenance/repo" --output "$maintenance/explicit/report.json"
check "maintenance accepts an explicit report path" test "$rc" -eq 0
check "maintenance preserves the explicit output path" \
    test "$(cat "$maintenance/case/output-path")" = "$maintenance/explicit/report.json"
check "an explicit maintenance report does not create the default state directory" test ! -e "$maintenance/unused-state"

echo "fix-workflow-resources: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
