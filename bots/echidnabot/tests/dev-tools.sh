#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Offline control-flow tests only: these do NOT validate downloaded tools/images.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."
repo=$PWD
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/bin" "$tmp/installed"
export TOOL_TEST_LOG="$tmp/calls"
export PATH="$tmp/bin:$PATH"
export MISE_BIN_DIR="$tmp/installed"

for tool in cmake pkg-config cc; do
    printf '#!/bin/sh\nexit 0\n' > "$tmp/bin/$tool"
    chmod +x "$tmp/bin/$tool"
done
cat > "$tmp/installed/mise" <<'MOCK'
#!/usr/bin/env bash
set -eu
if [[ $1 == --version ]]; then echo '2026.10.6 linux-x64'; exit; fi
printf '%s\n' "$*" >> "$TOOL_TEST_LOG"
MOCK
chmod +x "$tmp/installed/mise"
bash scripts/bootstrap.sh > "$tmp/output"
grep -Fx "trust $repo/mise.toml" "$TOOL_TEST_LOG"
grep -Fx 'install' "$TOOL_TEST_LOG"
grep -Fx 'exec -- cargo --version' "$TOOL_TEST_LOG"
grep -Fx 'exec -- just --version' "$TOOL_TEST_LOG"

# A failed download must fail closed, without claiming readiness.
rm "$tmp/installed/mise"
printf '#!/bin/sh\nexit 22\n' > "$tmp/bin/curl"
chmod +x "$tmp/bin/curl"
if bash scripts/bootstrap.sh > "$tmp/output" 2>&1; then
    echo 'FAIL: failed download reported success' >&2; exit 1
fi
if grep -q Ready "$tmp/output"; then exit 1; fi

# An untrusted archive must not be extracted/installed.
cat > "$tmp/bin/curl" <<'MOCK'
#!/usr/bin/env bash
while [[ $# -gt 0 ]]; do
    if [[ $1 == --output ]]; then printf 'bad archive' > "$2"; exit 0; fi
    shift
done
exit 1
MOCK
if bash scripts/bootstrap.sh > "$tmp/output" 2>&1; then
    echo 'FAIL: invalid checksum accepted' >&2; exit 1
fi
test ! -e "$tmp/installed/mise"

cat > "$tmp/bin/podman" <<'MOCK'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$TOOL_TEST_LOG"
if [[ $1 == image && ${MOCK_IMAGE_MISSING:-0} == 1 ]]; then exit 1; fi
MOCK
chmod +x "$tmp/bin/podman"
: > "$TOOL_TEST_LOG"
bash scripts/dev-container.sh --build cargo test --locked --lib trust::
grep -Fx 'build --target development --tag localhost/echidnabot-dev --file Containerfile .' "$TOOL_TEST_LOG"
grep -F -- '--userns=keep-id:uid=1000,gid=1000' "$TOOL_TEST_LOG"
grep -F -- "--volume $repo:/workspace:Z" "$TOOL_TEST_LOG"
grep -F -- 'localhost/echidnabot-dev cargo test --locked --lib trust::' "$TOOL_TEST_LOG"
: > "$TOOL_TEST_LOG"
if MOCK_IMAGE_MISSING=1 bash scripts/dev-container.sh > "$tmp/output" 2>&1; then
    echo 'FAIL: missing image accepted' >&2; exit 1
fi
if grep -q '^run ' "$TOOL_TEST_LOG"; then exit 1; fi
printf 'PASS: offline development-tool control-flow tests (no real builds performed)\n'
