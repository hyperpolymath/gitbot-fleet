#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Isolated contract tests for the prepared scanner migration.
# Usage: bash scripts/tests/secret-scanner-migration-test.sh
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/scanner-migration-test.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
TREE="$WORK/tree"
MIG="$TREE/docs/migrations/secret-scanner-cicd-suite"
mkdir -p "$TREE/docs/migrations" "$TREE/scripts" "$WORK/bin"
cp -R "$ROOT/.github" "$TREE/"
cp -R "$ROOT/scripts/lib" "$TREE/scripts/"
cp -R "$ROOT/docs/migrations/secret-scanner-cicd-suite" "$TREE/docs/migrations/"

# The audit and GitHub API have their own contracts. Stub them here so these
# tests exercise how preflight and rollback interpret each result offline.
cat > "$TREE/scripts/audit-lock-closure.sh" <<'STUB'
#!/usr/bin/env bash
exit "${TEST_AUDIT_RC:-0}"
STUB
cat > "$WORK/bin/gh" <<'STUB'
#!/usr/bin/env bash
case "$2" in
  repos/hyperpolymath/cicd-suite/commits/*) exit "${TEST_REMOTE_RC:-0}" ;;
  repos/hyperpolymath/gitbot-fleet/rulesets/24309193)
    [ "${TEST_FLOOR:-ok}" = ok ] && echo 'scan / gitleaks'
    exit 0 ;;
  repos/hyperpolymath/gitbot-fleet/rulesets/22415578)
    [ "${TEST_RULESET:-ok}" = ok ] || exit 1
    printf '%s\n' 'scan / gitleaks' 'scan / rust-secrets' 'scan / shell-secrets' ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$WORK/bin/gh"
export PATH="$WORK/bin:$PATH"

SHA=1234567890abcdef1234567890abcdef12345678
jq --arg sha "$SHA" '.result = "pass" | .subject_sha = $sha |
  .contexts |= map(.conclusion = "success" | .receipt_sha = $sha)' \
  "$MIG/canary/receipt.example.json" > "$WORK/good.json"
PASS=0; FAIL=0; RC=0; OUT=''
check() { # name, expected exit, expected output (empty means no output assertion)
  if [ "$RC" -eq "$2" ] && { [ -z "$3" ] || printf '%s\n' "$OUT" | grep -qF -- "$3"; }; then
    PASS=$((PASS + 1)); printf 'PASS: %s\n' "$1"
  else
    FAIL=$((FAIL + 1)); printf 'FAIL: %s (expected exit %s and %s; got exit %s)\n%s\n' "$1" "$2" "$3" "$RC" "$OUT"
  fi
}
# Prevent shell startup hooks from putting a real gh ahead of the fixture stub.
preflight() { OUT="$(BASH_ENV= /bin/bash "$MIG/preflight.sh" "$@" 2>&1)"; RC=$?; }
rollback() { OUT="$(BASH_ENV= /bin/bash "$MIG/rollback.sh" --root "$TREE" "$@" 2>&1)"; RC=$?; }

preflight --online --check-apply --receipt "$WORK/good.json"
check 'valid SHA-bound receipt and proposed patches pass' 0 'PASS: static properties hold'
preflight --online --receipt "$WORK/missing.json"
check 'missing receipt is not run' 2 'NOT-RUN'
preflight --online --receipt "$MIG/canary/receipt.example.json"
check 'example receipt cannot authorize migration' 2 "result='not_run'"
preflight --receipt "$WORK/good.json"
check 'offline verification is reported as not run' 2 'NOT-RUN'
preflight --quiet --receipt "$WORK/good.json"
check 'quiet offline mode accepts valid static evidence' 0 ''

# Each mutation starts from a valid receipt, isolating the reason for refusal.
receipt_case() { # name, jq mutation, expected diagnostic
  jq "$2" "$WORK/good.json" > "$WORK/case.json"
  preflight --online --receipt "$WORK/case.json"
  check "$1" 1 "$3"
}
receipt_case 'failed canary blocks migration' '.result = "fail"' "result='fail'"
receipt_case 'schema version is required' '.schema_version = 2' '(schema_version)'
receipt_case 'subject is bound to the canary repository' '.subject = "example/other"' '(subject)'
receipt_case 'subject SHA must be full lowercase hex' '.subject_sha = "abc"' '(subject_sha)'
receipt_case 'placeholder subject SHA is rejected' '.subject_sha = "0000000000000000000000000000000000000000"' 'all-zero subject SHA'
receipt_case 'callee SHA must match' '.suite_sha = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"' 'different callee SHA'
receipt_case 'fast PR scanning is rejected' '.fast_pr = true' '(fast_pr)'
receipt_case 'forwarded secrets are rejected' '.secrets_passed = ["TOKEN"]' '(secrets_passed)'
receipt_case 'full scan is required' '.scanned_mode = "pr-range"' '(scanned_mode)'
receipt_case 'three contexts are required' '.contexts = .contexts[:2]' '(contexts)'
receipt_case 'duplicate context names are rejected' '.contexts[1].name = .contexts[0].name' '(context_names)'
receipt_case 'skipped detector is rejected' '.contexts[0].conclusion = "skipped"' '(context_conclusion)'
receipt_case 'malformed detector receipt SHA is rejected' '.contexts[0].receipt_sha = "abc"' '(context_receipt_sha)'
receipt_case 'detector receipt must bind to subject SHA' '.contexts[0].receipt_sha = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"' '(context_subject_binding)'
receipt_case 'run URL must belong to canary repository' '.run_url = "https://github.com/example/other/actions/runs/1"' '(run_url)'
printf '{invalid json\n' > "$WORK/bad.json"
preflight --online --receipt "$WORK/bad.json"
check 'unreadable JSON is a finding' 1 'unreadable'

# Findings and missing evidence must be visible together, with distinct exit 3.
TEST_REMOTE_RC=1 preflight --online --receipt "$WORK/missing.json"
check 'finding plus missing evidence exits 3' 3 'REMOTE'
TEST_FLOOR=missing preflight --online --receipt "$WORK/good.json"
check 'missing required live floor context is a finding' 1 'RULESET'
TEST_RULESET=unavailable preflight --online --receipt "$WORK/good.json"
check 'unreadable live ruleset is not run' 2 'NOT-RUN'
for audit_rc in 1 2 3; do
  TEST_AUDIT_RC="$audit_rc" preflight --online --receipt "$WORK/good.json"
  case "$audit_rc" in 1|3) expected=1 ;; 2) expected=2 ;; esac
  check "audit exit $audit_rc is propagated as a gate result" "$expected" 'LOCK-CLOSURE'
done

cp "$MIG/patches/secret-scanner.yml.patch" "$WORK/original.patch"
sed -i 's/^+      fast-pr: false$/+      fast-pr: true/' "$MIG/patches/secret-scanner.yml.patch"
preflight --online --receipt "$WORK/good.json"
check 'unsafe fast-pr in proposed caller is rejected' 1 'WF-FAST-PR'
cp "$WORK/original.patch" "$MIG/patches/secret-scanner.yml.patch"
sed -i 's/^+      fast-pr: false$/+      secrets: inherit/' "$MIG/patches/secret-scanner.yml.patch"
preflight --online --receipt "$WORK/good.json"
check 'secret inheritance in proposed caller is rejected' 1 'WF-SECRETS'
cp "$WORK/original.patch" "$MIG/patches/secret-scanner.yml.patch"
sed -i 's/^+      fast-pr: false$/+      security-events: write/' "$MIG/patches/secret-scanner.yml.patch"
preflight --online --receipt "$WORK/good.json"
check 'excess permissions in proposed caller are rejected' 1 'WF-PERMS'
cp "$WORK/original.patch" "$MIG/patches/secret-scanner.yml.patch"
printf '\n# changed\n' >> "$MIG/fixtures/cicd-suite-4404c3c0474e17753fe7689f3485c02890aac113-secret-scanner-reusable.yml"
preflight --online --receipt "$WORK/good.json"
check 'tampered vendored callee is rejected' 1 'FIXTURE-CHECKSUM'
cp "$ROOT/docs/migrations/secret-scanner-cicd-suite/fixtures/cicd-suite-4404c3c0474e17753fe7689f3485c02890aac113-secret-scanner-reusable.yml" "$MIG/fixtures/"
mv "$MIG/fixtures/SHA256SUMS" "$WORK/SHA256SUMS"
preflight --online --receipt "$WORK/good.json"
check 'missing fixture checksum is not run' 2 'FIXTURE-CHECKSUM'
mv "$WORK/SHA256SUMS" "$MIG/fixtures/SHA256SUMS"
printf '%s\n' 'scan / missing-detector' >> "$MIG/required-contexts.txt"
preflight --online --receipt "$WORK/good.json"
check 'required context absent from callee is rejected' 1 'CONTEXT-MISSING'
cp "$ROOT/docs/migrations/secret-scanner-cicd-suite/required-contexts.txt" "$MIG/required-contexts.txt"

# Apply only to the disposable tree. Dry run and refusal must leave it intact.
for p in secret-scanner.yml.patch actions.lock.patch lock-closure-receipts.tsv.patch; do
  (cd "$TREE" && patch -p1 -s -f < "$MIG/patches/$p") || exit 1
done
files=(.github/workflows/secret-scanner.yml .github/workflows/actions.lock scripts/lib/lock-closure-receipts.tsv)
(cd "$TREE" && sha256sum "${files[@]}") > "$WORK/applied.sha"
rollback
check 'rollback defaults to dry run' 0 'Dry run:'
if (cd "$TREE" && sha256sum -c "$WORK/applied.sha" >/dev/null); then RC=0; else RC=1; fi
OUT=''; check 'dry run does not modify migration files' 0 ''
printf '\ndrift\n' >> "$TREE/.github/workflows/secret-scanner.yml"
# Drift inside a patch hunk, rather than an unrelated append, must refuse all reversals.
sed -i 's/fast-pr: false/fast-pr: true/' "$TREE/.github/workflows/secret-scanner.yml"
(cd "$TREE" && sha256sum "${files[@]}") > "$WORK/drift.sha"
rollback --apply
check 'rollback refuses drift before applying any patch' 1 'REFUSE:'
if (cd "$TREE" && sha256sum -c "$WORK/drift.sha" >/dev/null); then RC=0; else RC=1; fi
OUT=''; check 'refused rollback leaves all three files intact' 0 ''
# Restore the applied version and check the complete reverse transaction.
cp "$ROOT/.github/workflows/secret-scanner.yml" "$TREE/.github/workflows/secret-scanner.yml"
(cd "$TREE" && patch -p1 -s -f < "$MIG/patches/secret-scanner.yml.patch") || exit 1
rollback --apply
check 'rollback applies and re-audits' 0 'Rollback applied;'
for file in "${files[@]}"; do
  if cmp -s "$ROOT/$file" "$TREE/$file"; then RC=0; else RC=1; fi
  OUT=''; check "rollback restores $file byte for byte" 0 ''
done

# Audit failure and incomplete evidence remain visible after a real reversal.
for audit_rc in 1 2; do
  for p in secret-scanner.yml.patch actions.lock.patch lock-closure-receipts.tsv.patch; do
    (cd "$TREE" && patch -p1 -s -f < "$MIG/patches/$p") || exit 1
  done
  TEST_AUDIT_RC="$audit_rc" rollback --apply
  check "rollback propagates audit exit $audit_rc" "$audit_rc" 'audit exit'
done

printf '\n%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
