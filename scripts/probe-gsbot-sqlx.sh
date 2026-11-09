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
# Candidate-only refactor: materialize fixed column lists into string
# literals so SQLx can verify these queries are static. User-provided values
# remain bound parameters. Refuse to guess if the source shape changes.
python3 - "$tree/bots/gsbot/src/services.rs" <<'PY'
import pathlib
import re
import sys
p = pathlib.Path(sys.argv[1])
s = p.read_text()
columns = {}
for name in ('MATERIAL_COLS', 'GARMENT_COLS', 'BRAND_COLS', 'USER_COLS'):
    pattern = rf'const {name}: &str = "((?:[^"\\]|\\.)*)";'
    match = re.search(pattern, s, flags=re.S)
    if not match:
        raise SystemExit(f'unexpected {name} definition')
    columns[name] = re.sub(r'\\\n\s*', '', match.group(1))
    s = s[:match.start()] + s[match.end():]
join = re.compile(r'let q = format!\(\s*"SELECT \{\} FROM materials m \\\n\s*JOIN garment_materials gm ON gm.material_id = m.id \\\n\s*WHERE gm.garment_id = \?1",\s*MATERIAL_COLS\s*\.split\(", "\)\s*\.map\(\|c\| format!\("m\.\{c\}"\)\)\s*\.collect::<Vec<_>>\(\)\s*\.join\(", "\)\s*\);', re.S)
qualified = ', '.join('m.' + col for col in columns['MATERIAL_COLS'].split(', '))
s, count = join.subn(f'let q = "SELECT {qualified} FROM materials m JOIN garment_materials gm ON gm.material_id = m.id WHERE gm.garment_id = ?1";', s)
if count != 1:
    raise SystemExit('unexpected materials join; refusing candidate')
pattern = re.compile(r'let q = format!\(\s*"((?:[^"\\]|\\.)*)"\s*\);', re.S)
def make_literal(match):
    value = match.group(1)
    for name, cols in columns.items():
        value = value.replace('{' + name + '}', cols)
    if '{' in value or '}' in value:
        raise SystemExit('unexpected interpolation')
    return 'let q = "' + value + '";'
s, count = pattern.subn(make_literal, s)
if count != 11 or 'let q = format!' in s:
    raise SystemExit(f'unexpected query count: {count}')
# `q` is now a &'static str, not a String reference.
s, count = re.subn(r'(sqlx::query_as::<_, (?:Material|Garment|Brand|User)>)\(&q\)', r'\1(q)', s)
if count != 12:
    raise SystemExit(f'unexpected query_as count: {count}')
p.write_text(s)
PY
cd "$tree/bots/gsbot"
cargo update -p sqlx
cargo update -p libsqlite3-sys --precise 0.39.1
cd "$tree"
git diff --binary -- bots/gsbot/Cargo.toml bots/gsbot/Cargo.lock bots/gsbot/src/services.rs > "$RUNNER_TEMP/gsbot-candidate.patch"
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
