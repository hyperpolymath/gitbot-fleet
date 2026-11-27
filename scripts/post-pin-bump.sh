#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
#
# Post-pin-bump hook: the repo's half of the "single pin writer" contract.
#
# The Action pins applier (metadatastician/standards) moves third-party
# `uses:` SHAs and actions.lock. It cannot write scripts/lib/*, and it skips
# first-party owners, so a bump of a pinned reusable-workflow commit leaves the
# source closure files (the manifest and the receipts) describing the old
# commit and main goes red. This hook re-attests the PIN-ONLY case and refuses
# everything else:
#
#   * a pin-only bump means the caller's reusable workflow now points at a new
#     commit of the SAME repository and path, and the workflow file there is
#     byte-identical (same blob, same content_sha256, same direct edges);
#   * anything else (another blob, another repository or path, a commit shared
#     with a source that did not move, edges that changed, unreadable input) is
#     refused with a non-zero exit and NO file is changed. A human reviews it.
#
# New source is fetched as DATA over git by scripts/audit-pinned-reusable-sources.sh
# into its isolated bare cache and is never checked out or executed.
#
# Output: the paths the hook changed, NUL-separated, on stdout (nothing when no
# pin moved). Messages go to stderr. Exit 0 = nothing to do or re-attested,
# 1 = refused or self-audit failed (files restored), 64 = usage.
#
# Fetching new source is on when POST_PIN_BUMP=1 (set by the applier) or with
# --fetch-sources; otherwise the hook is offline and reads only the cache.
#
# Usage: post-pin-bump.sh [--repo-root DIR] [--cache DIR] [--fetch-sources]
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CACHE="${PINNED_SOURCE_CACHE:-${XDG_CACHE_HOME:-${HOME:-/tmp}/.cache}/fleet-pinned-sources}"
# The Action pins applier runs this hook with no arguments and POST_PIN_BUMP=1 (the
# contract in metadatastician/standards), so that variable also turns fetching on.
FETCH=0
[[ "${POST_PIN_BUMP:-}" != 1 ]] || FETCH=1
while [[ "$#" -gt 0 ]]; do
  case "$1" in
    --repo-root) ROOT="${2:?repository root required}"; shift ;;
    --cache) CACHE="${2:?cache directory required}"; shift ;;
    --fetch-sources) FETCH=1 ;;
    -h|--help) printf '%s\n' 'Usage: post-pin-bump.sh [--repo-root DIR] [--cache DIR] [--fetch-sources]'; exit 0 ;;
    *) printf 'post-pin-bump: unknown argument: %s\n' "$1" >&2; exit 64 ;;
  esac
  shift
done
ROOT="$(cd "$ROOT" && pwd)"
for tool in git jq awk sha256sum cmp; do
  command -v "$tool" >/dev/null || { printf 'post-pin-bump: missing prerequisite: %s\n' "$tool" >&2; exit 2; }
done

AUDIT="$SCRIPT_DIR/audit-pinned-reusable-sources.sh"
MANIFEST_REL='scripts/lib/pinned-reusable-sources.json'
RECEIPTS_REL='scripts/lib/lock-closure-receipts.tsv'
ALLOW="$ROOT/.github/post-pin-bump.paths"
MANIFEST="$ROOT/$MANIFEST_REL"
RECEIPTS="$ROOT/$RECEIPTS_REL"

TMP="$(mktemp -d "${TMPDIR:-/tmp}/post-pin-bump.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT
refuse() { printf 'post-pin-bump: REFUSED: %s\n' "$*" >&2; exit 1; }

[[ -f "$ALLOW" ]] || refuse "missing allow-list $ALLOW"
[[ -f "$MANIFEST" && -f "$RECEIPTS" ]] || refuse 'closure manifest or receipts missing'
[[ -x "$AUDIT" || -f "$AUDIT" ]] || refuse 'audit script missing'

audit_args=(--repo-root "$ROOT" --cache "$CACHE")
[[ "$FETCH" -eq 0 ]] || audit_args+=(--fetch-sources)

# 1. The live remote-reusable roots, read by the audit's own parser. Anything
#    the parser cannot read stops here.
bash "$AUDIT" --repo-root "$ROOT" --roots-only > "$TMP/roots.json" 2> "$TMP/roots.err" ||
  refuse "workflow roots unreadable: $(tr '\n' ' ' < "$TMP/roots.err")"

# 2. Which active sources moved? One (index, old, new) per moved source.
jq -rn --slurpfile roots "$TMP/roots.json" --slurpfile m "$MANIFEST" '
  $m[0].sources as $src | $roots[0] as $actual |
  [ range(0; $src|length) as $i | $src[$i] | select(.role=="active") as $s |
    ($s.callers | map(. as $c | ($actual | map(select(.workflow==$c.workflow and .job==$c.job)))[0])) as $found |
    if ($found|any(.==null)) then "ERR\t\($s.repository)@\($s.commit)\tcaller no longer present in the workflows"
    elif ($found|any((.repository != ($s.repository|ascii_downcase)) or .path != $s.path)) then "ERR\t\($s.repository)@\($s.commit)\trepository or path of a caller changed (not pin-only)"
    elif ($found|map(.commit)|unique|length) != 1 then "ERR\t\($s.repository)@\($s.commit)\tcallers disagree on the commit"
    elif $found[0].commit != $s.commit then "BUMP\t\($i)\t\($s.commit)\t\($found[0].commit)"
    else empty end ] | .[]' > "$TMP/plan.tsv"

if grep -q '^ERR' "$TMP/plan.tsv"; then refuse "$(grep '^ERR' "$TMP/plan.tsv" | tr '\t' ' ' | tr '\n' ';')"; fi
if [[ ! -s "$TMP/plan.tsv" ]]; then exit 0; fi

# 3. Rewrite into temporary copies, never into the tree.
cp "$MANIFEST" "$TMP/manifest.json"
cp "$RECEIPTS" "$TMP/receipts.tsv"
# Plan rows grouped by (repository, old commit): sources that share an old commit
# and ALL move together to the same new commit are one group and are rewritten
# once. A sharer that stays put, or members moving to different targets, is refused.
: > "$TMP/members.tsv"
while IFS=$'\t' read -r _ idx old new; do
  [[ "$idx" =~ ^[0-9]+$ && "$old" =~ ^[0-9a-f]{40}$ && "$new" =~ ^[0-9a-f]{40}$ ]] || refuse 'malformed plan row'
  jq -r --argjson i "$idx" --arg old "$old" --arg new "$new" \
    '[(.sources[$i].repository|ascii_downcase), $old, $new, ($i|tostring)] | @tsv' "$TMP/manifest.json" >> "$TMP/members.tsv"
done < <(grep '^BUMP' "$TMP/plan.tsv")
while IFS=$'\t' read -r repo old; do
  targets="$(awk -F'\t' -v r="$repo" -v o="$old" '$1==r && $2==o {print $3}' "$TMP/members.tsv" | LC_ALL=C sort -u)"
  [[ "$(printf '%s\n' "$targets" | wc -l)" -eq 1 ]] ||
    refuse "$repo@$old moves to conflicting commits ($(printf '%s' "$targets" | tr '\n' ' ')); review by hand"
  new="$targets"
  idxs="$(awk -F'\t' -v r="$repo" -v o="$old" '$1==r && $2==o {print $4}' "$TMP/members.tsv" | paste -sd, -)"
  # Receipt rows and the comment cover every source of repo@old: a commit shared
  # with a source outside this bump plan cannot be re-pointed by guessing.
  shared="$(jq -r --arg r "$repo" --arg c "$old" --argjson ids "[$idxs]" \
    '[.sources|to_entries[]|select((.key as $k | $ids|index($k)|not) and (.value.repository|ascii_downcase)==$r and .value.commit==$c)]|length' "$TMP/manifest.json")"
  [[ "$shared" -eq 0 ]] || refuse "$repo@$old is shared with $shared other source(s) that did not move; review by hand"
  # Re-point each member's commit and its recognised provenance fields (source_url,
  # local_file), by literal split/join. Any other mention of $old is refused below.
  jq --argjson ids "[$idxs]" --arg old "$old" --arg new "$new" '
    def swap: if type=="string" then split($old)|join($new) else . end;
    reduce $ids[] as $i (.; .sources[$i] |= (.commit=$new | (if has("source_url") then .source_url|=swap else . end) | (if has("local_file") then .local_file|=swap else . end)))' \
    "$TMP/manifest.json" > "$TMP/manifest.next"
  mv "$TMP/manifest.next" "$TMP/manifest.json"
  # Receipts: rows "repo<TAB>old<TAB>..." and "# Reviewed source: ...repo/...@old" lines,
  # literal matching only (no regex on data).
  awk -F'\t' -v OFS='\t' -v repo="${repo,,}" -v old="$old" -v new="$new" '
    BEGIN { tag = "# Reviewed source: " }
    substr($0, 1, length(tag)) == tag {
      i = index(tolower($0), repo "/")
      if (i > 0) { j = index($0, "@" old); if (j > 0 && j > i) $0 = substr($0, 1, j) new substr($0, j + 1 + length(old)) }
      print; next
    }
    $0 !~ /^#/ && tolower($1) == repo && $2 == old { $2 = new }
    { print }' "$TMP/receipts.tsv" > "$TMP/receipts.next"
  mv "$TMP/receipts.next" "$TMP/receipts.tsv"
  if grep -qF "$old" "$TMP/receipts.tsv"; then
    # The only tolerated mention is prose in the header comment block.
    grep -F "$old" "$TMP/receipts.tsv" | grep -qv '^#' && refuse "receipts still name $old after the rewrite"
  fi
  # Any other mention of the old commit in the manifest (e.g. dependency keys) is
  # something this hook did not review.
  if jq -e --arg c "$old" '[.. | strings | select(contains($c))] | length > 0' "$TMP/manifest.json" >/dev/null; then
    refuse "manifest still names $old after the rewrite (dependency records); review by hand"
  fi
done < <(cut -f1,2 "$TMP/members.tsv" | LC_ALL=C sort -u)

# 4. The audit is the authority: with the rewritten copies it must find, at the
#    NEW commit, the same blob (a fetched blob mismatch is a FATAL finding) and
#    the same direct edges, with every receipt consistent. Not-run is a refusal.
if ! bash "$AUDIT" "${audit_args[@]}" --manifest "$TMP/manifest.json" --receipts "$TMP/receipts.tsv" > "$TMP/audit.out" 2>&1; then
  refuse "the new commit does not re-attest as pin-only: $(tr '\n' ' ' < "$TMP/audit.out" | cut -c1-600)"
fi

# 5. content_sha256, from the cached object the audit just verified by hash.
while IFS=$'\t' read -r _ idx _ new; do
  want="$(jq -r --argjson i "$idx" '.sources[$i].content_sha256 // empty' "$TMP/manifest.json")"
  [[ -n "$want" ]] || continue
  repo="$(jq -r --argjson i "$idx" '.sources[$i].repository|ascii_downcase' "$TMP/manifest.json")"
  blob="$(jq -r --argjson i "$idx" '.sources[$i].blob' "$TMP/manifest.json")"
  got="$(env GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 git --no-replace-objects --git-dir "$CACHE/$repo.git" cat-file blob "$blob" | sha256sum | cut -d' ' -f1)" ||
    refuse "cannot read verified blob $blob of $repo@$new"
  [[ "$got" = "$want" ]] || refuse "content_sha256 of $repo@$new differs from the manifest"
done < <(grep '^BUMP' "$TMP/plan.tsv")

# 6. Only the allow-listed paths may change.
: > "$TMP/changed"
cmp -s "$MANIFEST" "$TMP/manifest.json" || printf '%s\n' "$MANIFEST_REL" >> "$TMP/changed"
cmp -s "$RECEIPTS" "$TMP/receipts.tsv" || printf '%s\n' "$RECEIPTS_REL" >> "$TMP/changed"
while IFS= read -r p; do
  grep -qxF -- "$p" "$ALLOW" || refuse "$p is not in .github/post-pin-bump.paths"
done < "$TMP/changed"

# 7. Install, self-audit the real tree. Any failure (a write that stops half way, or
#    a failed self-audit) restores BOTH files, so the pair never describes different commits.
cp -p "$MANIFEST" "$TMP/manifest.orig"; cp -p "$RECEIPTS" "$TMP/receipts.orig"
restore() {
  # Attempt BOTH restores independently: a failed first one must not skip the second.
  local failed=0 keep
  cat "$TMP/manifest.orig" > "$MANIFEST" || failed=1
  cat "$TMP/receipts.orig" > "$RECEIPTS" || failed=1
  [[ "$failed" -eq 1 ]] || return 0
  # $TMP is deleted on exit, so keep the originals somewhere that survives it. If
  # that copy fails too, disarm the deletion and leave them in $TMP itself.
  if keep="$(mktemp -d "${TMPDIR:-/tmp}/post-pin-bump-backup.XXXXXX")" && cp -p "$TMP/manifest.orig" "$TMP/receipts.orig" "$keep/"; then
    refuse "cannot restore the closure files after a failed install; originals kept in $keep (restore from there or from git)"
  fi
  trap - EXIT
  refuse "cannot restore the closure files after a failed install; originals kept in $TMP (copy to a separate backup dir failed; restore from there or from git)"
}
if ! { cat "$TMP/manifest.json" > "$MANIFEST" && cat "$TMP/receipts.tsv" > "$RECEIPTS"; }; then
  restore; refuse 'writing the closure files failed (files restored)'
fi
if ! bash "$AUDIT" "${audit_args[@]}" > "$TMP/final.out" 2>&1; then
  restore
  refuse "self-audit failed after the rewrite (files restored): $(tr '\n' ' ' < "$TMP/final.out" | cut -c1-600)"
fi
while IFS= read -r p; do printf '%s\0' "$p"; done < "$TMP/changed"
