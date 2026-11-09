#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# CI-only disposable experiment: do not modify the checkout or suppress audits.
set -euo pipefail
: "${RUNNER_TEMP:?GitHub runner temp directory is required}"
root=$(git rev-parse --show-toplevel)
tree="$RUNNER_TEMP/gsbot-sqlx-candidate"
git worktree add --detach "$tree" HEAD
python3 - "$tree/bots/gsbot/Cargo.toml" <<'PY'
import pathlib
import sys
p = pathlib.Path(sys.argv[1])
s = p.read_text()
old = 'sqlx = { version = "0.8.6", default-features = false, features = [\n  "runtime-tokio-rustls", "sqlite", "macros", "chrono", "migrate",\n] }'
new = 'sqlx = { git = "https://github.com/launchbadge/sqlx", rev = "ae659a0ca4e5e6708e589abc216f734deddb8e29", default-features = false, features = [\n  "runtime-tokio", "tls-rustls", "sqlite", "macros", "chrono", "migrate",\n] }'
if s.count(old) != 1:
    raise SystemExit('unexpected SQLx dependency; refusing candidate')
p.write_text(s.replace(old, new))
PY
cd "$tree/bots/gsbot"
cargo update -p sqlx
cargo update -p libsqlite3-sys --precise 0.39.1
cd "$tree"
git diff --binary -- bots/gsbot/Cargo.toml bots/gsbot/Cargo.lock > "$RUNNER_TEMP/gsbot-candidate.patch"
echo "::notice title=GSBot candidate SHA256::$(sha256sum "$RUNNER_TEMP/gsbot-candidate.patch" | cut -d ' ' -f 1)"
gzip -n -c "$RUNNER_TEMP/gsbot-candidate.patch" | base64 -w0 | fold -w 2500 | awk '{printf "::notice title=GSBot candidate %03d::%s\n", NR, $0}'
cd "$tree/bots/gsbot"
run_checked() {
  local label=$1 log=$2 rc
  shift 2
  set +e
  "$@" > "$log" 2>&1
  rc=$?
  set -e
  cat "$log"
  if [[ $rc -ne 0 ]]; then
    detail=$(grep -Ei -C 3 'error|RUSTSEC-|failed|vulnerab' "$log" | head -c 5000 || true)
    [[ -n "$detail" ]] || detail="Inspect the full $label log"
    detail=${detail//'%'/'%25'}; detail=${detail//$'\r'/'%0D'}; detail=${detail//$'\n'/'%0A'}
    echo "::error::$label failed (exit $rc): $detail"
    exit "$rc"
  fi
}
run_checked 'GSBot candidate tests' "$RUNNER_TEMP/gsbot-candidate-tests.log" cargo test --locked --all-targets
run_checked 'GSBot candidate advisory audit' "$RUNNER_TEMP/gsbot-candidate-audit.log" cargo deny --locked --config deny.toml check advisories --warn unmaintained
