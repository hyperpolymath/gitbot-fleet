#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Install the pinned mise binary and this repository's Rust/Just toolchain.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

version=2026.10.6
bin_dir=${MISE_BIN_DIR:-"$HOME/.local/bin"}
for cmd in curl tar gzip sha256sum cc cmake pkg-config git; do
    command -v "$cmd" >/dev/null || {
        printf 'Missing prerequisite: %s. See docs/DEVELOPMENT.adoc.\n' "$cmd" >&2
        exit 1
    }
done
case "$(uname -s)/$(uname -m)" in
    Linux/x86_64)
        arch=x64
        checksum=5135da6b71e6857efb22b2eb8d0fbc0061d061005a4994f1e7f8975d548a3e92
        ;;
    Linux/aarch64|Linux/arm64)
        arch=arm64
        checksum=60f0e34ea2088e822797393ed3d3b50d58dd9b45687006b31ac66ef68e99a2f4
        ;;
    *)
        echo 'Bootstrap supports Linux x86_64/aarch64; on other hosts use a Podman Linux VM.' >&2
        exit 1
        ;;
esac
mkdir -p "$bin_dir"
if [[ ! -x "$bin_dir/mise" ]] || [[ $("$bin_dir/mise" --version) != "$version "* ]]; then
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    curl --fail --location --retry 3 --connect-timeout 20 --max-time 300 \
        "https://github.com/jdx/mise/releases/download/v$version/mise-v$version-linux-$arch.tar.gz" \
        --output "$tmp/mise.tar.gz"
    printf '%s  %s\n' "$checksum" "$tmp/mise.tar.gz" | sha256sum --check --status
    tar -xzf "$tmp/mise.tar.gz" -C "$tmp" mise/bin/mise
    install -m 0755 "$tmp/mise/bin/mise" "$bin_dir/mise"
fi
export PATH="$bin_dir:$PATH"
# Explicit invocation means the user has elected to trust this checkout.
mise trust "$PWD/mise.toml"
mise install
mise exec -- cargo --version
mise exec -- just --version
printf '\nReady. Add %s to PATH, then run: mise run build && mise run test\n' "$bin_dir"
