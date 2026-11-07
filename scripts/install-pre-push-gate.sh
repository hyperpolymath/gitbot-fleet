#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Install without replacing an existing custom hook or changing core.hooksPath.
set -euo pipefail
repo=${1:?usage: install-pre-push-gate.sh REPOSITORY}
source_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
repo=$(git -C "$repo" rev-parse --show-toplevel)
# Resolve the directory through Git, but preserve the final hook entry itself:
# resolving hooks/pre-push can follow a dangling symlink to an absent target.
hook_dir=$(git -C "$repo" rev-parse --path-format=absolute --git-path hooks)
hook="$hook_dir/pre-push"
source_hook="$source_root/hooks/pre-push-gate.sh"
if [[ -e "$hook" || -L "$hook" ]]; then
  if cmp -s "$source_hook" "$hook" && [[ -x "$hook" ]]; then
    printf 'Already installed: %s\n' "$hook"
    exit 0
  fi
  printf 'Refusing to replace existing hook: %s\nCompose it with the fleet gate explicitly.\n' "$hook" >&2
  exit 1
fi
mkdir -p "$(dirname "$hook")"
# noclobber prevents replacing a hook created after the check above.
(set -o noclobber; cat "$source_hook" > "$hook")
chmod +x "$hook"
printf 'Installed: %s\n' "$hook"
printf 'The gate requires a working scanner. Configure fleet.prePushScanners to require a particular scanner, and fleet.prePushCiRunner for the consumer CI receipt gate.\n'
