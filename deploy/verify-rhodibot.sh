#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proof of life for a rhodibot deployment.
#
# Checks the deployment from the outside in: what a visitor can reach, what is
# deliberately unreachable, and what the origin itself reports about its
# credentials. Every check prints PASS, FAIL or SKIP -- SKIP means the check
# could not be made on this host (no systemctl, not root), never that it passed.
#
#   RHODIBOT_HOST=rhodibot.example.org sudo -E ./deploy/verify-rhodibot.sh
#
# Exits non-zero if any check fails.

set -uo pipefail

HOST="${RHODIBOT_HOST:-}"
ORIGIN="${RHODIBOT_ORIGIN:-http://127.0.0.1:3000}"
KEY_PATH="${RHODIBOT_KEY_PATH:-/etc/fleet/rhodibot-app.pem}"

fails=0
pass() { printf '  \033[32mPASS\033[0m %s\n' "$1"; }
fail() { printf '  \033[31mFAIL\033[0m %s\n' "$1"; fails=$((fails + 1)); }
skip() { printf '  \033[33mSKIP\033[0m %s\n' "$1"; }

code() { curl -sS -o /dev/null -w '%{http_code}' --max-time 10 "$@" 2>/dev/null || echo "000"; }

echo "rhodibot deployment check"
echo "  origin: $ORIGIN"
echo "  public: ${HOST:+https://$HOST}${HOST:-<unset: set RHODIBOT_HOST to check the tunnel>}"
echo

echo "origin (the process behind the tunnel)"
health="$(curl -sS --max-time 10 "$ORIGIN/health" 2>/dev/null)"
if [ -n "$health" ]; then
    pass "GET /health answers"
    mode="$(printf '%s' "$health" | sed -n 's/.*"credentials":"\([^"]*\)".*/\1/p')"
    case "$mode" in
        misconfigured) fail "/health reports misconfigured credentials" ;;
        "")            skip "/health carries no credential mode (older build?)" ;;
        *)             pass "/health reports credentials: $mode" ;;
    esac
else
    fail "GET /health does not answer at $ORIGIN (is the unit running?)"
fi

# The origin must not be reachable from anything but this host: a service that
# binds 0.0.0.0 while a tunnel also publishes it is open twice.
#
# Checked against rhodibot's own sockets rather than against the port. Asking
# "who is listening on 3000?" flags any unrelated process that happens to share
# the number; asking "where is rhodibot listening?" is the actual question.
pid="$(pgrep -x rhodibot | head -1)"
if [ -z "$pid" ]; then
    skip "rhodibot is not running: cannot check the bind address (run this on the host, with the unit up)"
elif ! command -v ss >/dev/null 2>&1; then
    skip "ss not available: cannot check the bind address"
else
    addrs="$(ss -ltnpH 2>/dev/null | grep -F "pid=$pid," | awk '{print $4}' | sort -u)"
    if [ -z "$addrs" ]; then
        skip "no listening sockets visible for pid $pid (needs root?)"
    elif printf '%s\n' "$addrs" | grep -qvE '^(127\.0\.0\.1|\[::1\]|::1):'; then
        fail "rhodibot (pid $pid) is listening on a non-loopback address: $(printf '%s ' $addrs)"
    else
        pass "rhodibot (pid $pid) listens on loopback only: $(printf '%s ' $addrs)"
    fi
fi

echo
echo "through the tunnel (what GitHub and the internet see)"
if [ -z "$HOST" ]; then
    skip "RHODIBOT_HOST unset: skipping public checks"
else
    c="$(code "https://$HOST/health")"
    [ "$c" = "200" ] && pass "GET /health -> 200" || fail "GET /health -> $c (expected 200)"

    # An unsigned delivery must be refused. This is the one check that proves
    # both that the tunnel reaches the origin *and* that the webhook secret is
    # configured: with no secret set, the handler accepts anything.
    c="$(code -X POST -H 'content-type: application/json' -d '{}' "https://$HOST/webhook")"
    case "$c" in
        401) pass "POST /webhook without a signature -> 401" ;;
        200) fail "POST /webhook without a signature -> 200: GITHUB_WEBHOOK_SECRET is not set" ;;
        *)   fail "POST /webhook without a signature -> $c (expected 401)" ;;
    esac

    echo
    echo "boundary (must not be reachable)"
    c="$(code "https://$HOST/")"
    [ "$c" = "404" ] && pass "GET / -> 404" || fail "GET / -> $c (expected 404)"

    c="$(code "https://$HOST/api/check/hyperpolymath/gitbot-fleet")"
    [ "$c" = "404" ] && pass "GET /api/check/... -> 404 (quota-spending endpoint stays private)" \
                     || fail "GET /api/check/... -> $c (expected 404)"

    c="$(code "https://$HOST/webhook")"
    case "$c" in
        405) pass "GET /webhook -> 405 (path routed, method rejected by the origin)" ;;
        404) pass "GET /webhook -> 404 (blocked at the tunnel)" ;;
        *)   fail "GET /webhook -> $c (expected 405 or 404)" ;;
    esac
fi

echo
echo "host"
if command -v systemctl >/dev/null 2>&1; then
    for unit in rhodibot.service cloudflared-rhodibot.service; do
        state="$(systemctl is-active "$unit" 2>/dev/null)"
        [ "$state" = "active" ] && pass "$unit is active" || fail "$unit is $state"
    done
else
    skip "systemctl not available: cannot check the units"
fi

if [ -e "$KEY_PATH" ]; then
    perms="$(stat -c '%a %U:%G' "$KEY_PATH" 2>/dev/null)"
    case "$perms" in
        6[04]0\ root:fleet|600\ root:root|640\ root:root) pass "App key $KEY_PATH is $perms" ;;
        *) fail "App key $KEY_PATH is $perms (want 600 or 640, owned by root, group fleet)" ;;
    esac
else
    skip "no App key at $KEY_PATH: the App is not registered on this host yet"
fi

echo
if [ "$fails" -eq 0 ]; then
    echo "no failures"
else
    echo "$fails check(s) failed"
fi
exit $((fails > 0))
