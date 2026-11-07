#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Verify reviewed direct reusable-workflow edges against immutable Git objects.
# This reads source as data; it never checks out or executes fetched content.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MANIFEST=""; LOCK=""; RECEIPTS=""; CACHE="${PINNED_SOURCE_CACHE:-${XDG_CACHE_HOME:-${HOME:-/tmp}/.cache}/fleet-pinned-sources}"
FETCH=0; JSON=0
while [[ "$#" -gt 0 ]]; do
  case "$1" in
    --manifest|--source-manifest) MANIFEST="${2:?manifest file required}"; shift ;;
    --repo-root) ROOT="${2:?repository root required}"; shift ;;
    --lock) LOCK="${2:?lock file required}"; shift ;;
    --receipts) RECEIPTS="${2:?receipt file required}"; shift ;;
    --cache|--source-cache) CACHE="${2:?cache directory required}"; shift ;;
    --fetch-sources) FETCH=1 ;;
    --json) JSON=1 ;;
    -h|--help) printf '%s\n' 'Usage: audit-pinned-reusable-sources.sh [--manifest FILE] [--repo-root DIR] [--lock FILE] [--receipts FILE] [--cache DIR] [--fetch-sources] [--json]'; exit 0 ;;
    *) printf 'Unknown argument: %s\n' "$1" >&2; exit 64 ;;
  esac
  shift
done
MANIFEST="${MANIFEST:-$ROOT/scripts/lib/pinned-reusable-sources.json}"
LOCK="${LOCK:-$ROOT/.github/workflows/actions.lock}"
RECEIPTS="${RECEIPTS:-$ROOT/scripts/lib/lock-closure-receipts.tsv}"
for tool in git jq awk sha256sum; do
  command -v "$tool" >/dev/null || { printf 'Missing verifier prerequisite: %s\n' "$tool" >&2; exit 2; }
done
TMP="$(mktemp -d "${TMPDIR:-/tmp}/pinned-sources.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT
: > "$TMP/findings"; : > "$TMP/notrun"; : > "$TMP/roots"; : > "$TMP/groups"
printf '{}\n' > "$TMP/supplement.json"
finding() {
  local rule="$1" subject="$2" detail="$3"
  jq -nc --arg rule "$rule" --arg subject "$subject" --arg detail "$detail" \
    '{severity:"FATAL",rule:$rule,subject:$subject,detail:$detail,fix:"Review the source-bound manifest and immutable objects; do not waive closure evidence"}' >> "$TMP/findings"
}
notrun() {
  local rule="$1" subject="$2" detail="$3"
  jq -nc --arg rule "$rule" --arg subject "$subject" --arg detail "$detail" \
    '{rule:$rule,subject:$subject,detail:$detail,fix:"Provide authenticated source objects and supported parseable input; fetched source is never executed"}' >> "$TMP/notrun"
}
finish() {
  local f n rc=0
  f="$(jq -s length "$TMP/findings")"; n="$(jq -s length "$TMP/notrun")"
  [[ "$f" -eq 0 ]] || rc=1
  [[ "$n" -eq 0 ]] || rc=$((rc + 2))
  if [[ "$rc" -ne 0 ]]; then
    : > "$TMP/roots"; : > "$TMP/groups"; printf '{}\n' > "$TMP/supplement.json"
  fi
  jq -n --slurpfile findings "$TMP/findings" --slurpfile not_run "$TMP/notrun" \
    --slurpfile roots "$TMP/roots" --slurpfile groups "$TMP/groups" --slurpfile deps "$TMP/supplement.json" \
    '{findings:$findings,not_run:$not_run,verified_roots:$roots,verified_receipt_sources:($groups|unique),dependencies:$deps[0]}' > "$TMP/report.json"
  if [[ "$JSON" -eq 1 ]]; then cat "$TMP/report.json"; else
    jq -r '.findings[] | "FATAL\t\(.rule)\t\(.subject)\t\(.detail)"' "$TMP/report.json"
    jq -r '.not_run[] | "NOT-RUN\t\(.rule)\t\(.subject)\t\(.detail)"' "$TMP/report.json"
    printf 'pinned reusable sources: findings=%s not-run=%s verified-roots=%s (direct source edges; no fetched execution)\n' "$f" "$n" "$(jq '.verified_roots|length' "$TMP/report.json")"
  fi
  return "$rc"
}
if [[ ! -f "$MANIFEST" ]]; then notrun SOURCE-MISSING "$MANIFEST" 'No source manifest'; finish; exit $?; fi
# Streaming paths retain duplicate object keys which ordinary jq object
# parsing would discard. Container-close events identify their parent path.
if ! jq --stream -es 'map(if length==2 then .[0] else .[0][0:-1] end) as $paths | ($paths|length)==($paths|unique|length)' "$MANIFEST" >/dev/null 2>&1; then
  finding SOURCE-MANIFEST "$MANIFEST" 'Duplicate JSON keys, multiple documents or malformed JSON'; finish; exit $?
fi
if ! jq -e '
  def sha: type=="string" and test("^[0-9a-f]{40}$");
  def repo: type=="string" and test("^[A-Za-z0-9][A-Za-z0-9_.-]*/[A-Za-z0-9][A-Za-z0-9_.-]*$");
  def path: type=="string" and test("^\\.github/workflows/[A-Za-z0-9_.-]+\\.ya?ml$");
  .version==1 and (.sources|type=="array" and length>0) and (.dependencies|type=="object") and
  all(.sources[];
    (.repository|repo) and (.commit|sha) and (.blob|sha) and (.path|path) and
    (.role=="active" or .role=="retained-review") and (.callers|type=="array") and
    (if .role=="active" then (.callers|length>0) else (.callers|length==0) end) and
    all(.callers[]; (.workflow|path) and (.job|type=="string" and test("^[A-Za-z_][A-Za-z0-9_-]*$"))) and
    (.uses|type=="array") and all(.uses[]; (.kind=="step" or .kind=="job") and (.ref|type=="string" and length>0))) and
  ([.sources[]|[(.repository|ascii_downcase),.commit,.path]]|length)==([.sources[]|[(.repository|ascii_downcase),.commit,.path]]|unique|length) and
  all(.dependencies|to_entries[];
    (.key|test("^[A-Za-z0-9][A-Za-z0-9_.-]*/[A-Za-z0-9][A-Za-z0-9_.-]*@[^[:space:]@]+$")) and
    (.value|type=="object" and (.ref|type=="string" and test("^[^[:space:]@]+$")) and
      (.commit|type=="string" and test("^sha1-[0-9a-f]{40}$")) and
      (if has("uses") then (.uses|type=="array" and all(.[];type=="string")) else true end) and
      (if has("owner_id") then (.owner_id|type=="number" and .>=0 and floor==.) else true end) and
      (if has("repo_id") then (.repo_id|type=="number" and .>=0 and floor==.) else true end) and
      (if has("original_lock_record") then (.original_lock_record|type=="string") else true end)))
' "$MANIFEST" >/dev/null 2>&1; then
  finding SOURCE-MANIFEST "$MANIFEST" 'Malformed, duplicate or unsupported source manifest'; finish; exit $?
fi

# Preserve the strict vendor parser rather than introducing another YAML
# dependency. Its data-only mode exits before calling this verifier.
if ! bash "$SCRIPT_DIR/audit-lock-closure.sh" --dump-lock-records --lock "$LOCK" \
  --workflows "$ROOT/.github/workflows" --repo-root "$ROOT" --receipts "$RECEIPTS" --baseline /dev/null > "$TMP/vendor.json" 2> "$TMP/vendor.err"; then
  finding SOURCE-DEPENDENCY "$LOCK" 'Vendor lock or receipt input cannot be parsed safely'; finish; exit $?
fi
jq '.dependencies | with_entries(.value |= with_entries(select(.key=="ref" or .key=="commit" or .key=="uses" or .key=="owner_id" or .key=="repo_id")))' "$MANIFEST" > "$TMP/supplement.json"
# Repository names are case-insensitive; ref spelling is not. Reject a
# collision before normalization, then compare records in one key space.
normalize_records() {
  local input="$1" output="$2"
  jq -e '
    def key: capture("^(?<repository>[^@]+)@(?<ref>[^@]+)$") | ((.repository|ascii_downcase)+"@"+.ref);
    (keys|map(key)) as $keys | ($keys|length)==($keys|unique|length)
  ' "$input" >/dev/null 2>&1 || return 1
  jq '
    def key: capture("^(?<repository>[^@]+)@(?<ref>[^@]+)$") | ((.repository|ascii_downcase)+"@"+.ref);
    with_entries(.key |= key | if (.value|has("uses")) then .value.uses |= map(key) else . end)
  ' "$input" > "$output"
}
for graph in vendor supplement; do
  if ! normalize_records "$TMP/$graph.json" "$TMP/$graph.normalized.json"; then
    finding SOURCE-DEPENDENCY "$graph" 'Malformed or case-colliding dependency keys'; finish; exit $?
  fi
  mv "$TMP/$graph.normalized.json" "$TMP/$graph.json"
done
jq -s '.[0] * .[1]' "$TMP/vendor.json" "$TMP/supplement.json" > "$TMP/graph.json"

normalize_ref() {
  local raw="$1" path ref
  [[ "$raw" != *[[:space:]]* && "$raw" != *'${{'* ]] || return 1
  [[ "$raw" =~ ^([A-Za-z0-9][A-Za-z0-9_.-]*/[A-Za-z0-9][A-Za-z0-9_.-]*)(/[^@]+)?@([^@]+)$ ]] || return 1
  path="${BASH_REMATCH[1]}"; ref="${BASH_REMATCH[3]}"
  printf '%s@%s\n' "${path,,}" "$ref"
}

# Conservative block-YAML reader matching the existing auditor's profile.
# A flow/scalar/anchor spelling we cannot read is evidence missing, not green.
extract_uses() {
  local workflow_file="$1"
  awk '
    function uncomment(s, i,c,q,out) {
      q="";out=""
      for(i=1;i<=length(s);i++) {c=substr(s,i,1);if(q=="") {if(c=="\""||c=="\047")q=c;else if(c=="#"&&(i==1||substr(s,i-1,1)~/[[:space:]]/))break} else if(c==q)q="";out=out c}return out
    }
    {
      sub(/\r$/, "");line=$0
      if(line~/^[[:space:]]*$/||line~/^[[:space:]]*#/)next
      if(line~/\t/){print "UNPARSED\ttab in YAML";next}
      match(line,/^[[:space:]]*/);ind=RLENGTH
      if(block){if(ind>blockind)next;block=0}
      if(line~/^[[:space:]]*<<[[:space:]]*:/||line~/(^|[[:space:]])[&*][A-Za-z0-9_-]+/){print "UNPARSED\tunsupported YAML alias or merge";next}
      if(line~/^["\047]?jobs["\047]?[[:space:]]*:[[:space:]]*$/){jobs=1;ji=ind;jind=-1;job="";next}
      if(jobs&&ind<=ji){jobs=0;job=""}
      if(jobs&&ind>ji){
        if(jind<0)jind=ind
        if(ind==jind){s=uncomment(line);sub(/^[[:space:]]+/,"",s);sub(/:[[:space:]]*$/,"",s);gsub(/^["\047]|["\047]$/,"",s);if(s!~/^[A-Za-z_][A-Za-z0-9_-]*$/){print "UNPARSED\tjob key not a block scalar key";job=""}else job=s}
      }
      if(steps&&ind<=si)steps=0
      if(line~/^[[:space:]]*(-[[:space:]]+)?["\047]?steps["\047]?[[:space:]]*:[[:space:]]*$/){steps=1;si=ind;next}
      if(match(line,/^[[:space:]]*(-[[:space:]]+)?["\047]?uses["\047]?[[:space:]]*:/)){
        head=substr(line,1,RLENGTH);kind=(head~/-/||(steps&&ind>si))?"step":"job";raw=uncomment(substr(line,RLENGTH+1));sub(/^[[:space:]]+/,"",raw);sub(/[[:space:]]+$/,"",raw)
        if(raw==""||raw~/^[|>*&\[{]/||raw~/[[:space:]]/){print "UNPARSED\tuses value not a single plain scalar";next}
        if(raw~/^["\047]/){q=substr(raw,1,1);if(substr(raw,length(raw),1)!=q){print "UNPARSED\tunterminated uses quote";next};raw=substr(raw,2,length(raw)-2)}
        print "USE\t"kind"\t"raw"\t"job;next
      }
      if(line~/(^|[[:space:],{])["\047]?uses["\047]?[[:space:]]*:/)print "UNPARSED\tuses in unsupported YAML shape"
      s=uncomment(line);if(s~/(^|[[:space:]-])["\047]?[A-Za-z0-9_.-]+["\047]?[[:space:]]*:[[:space:]]*[|>][-+]?[0-9]?[[:space:]]*$/){block=1;blockind=ind}
    }
  ' "$workflow_file"
}

# Discover the exact live caller/job/path roots independently of the catalog.
shopt -s nullglob
for workflow in "$ROOT"/.github/workflows/*.yml "$ROOT"/.github/workflows/*.yaml; do
  caller=".github/workflows/$(basename "$workflow")"
  while IFS=$'\t' read -r tag kind raw job; do
    if [[ "$tag" = UNPARSED ]]; then notrun SOURCE-PARSE "$caller" "$kind"; continue; fi
    [[ "$kind" = job ]] || continue
    if [[ "$raw" == ./* ]]; then continue; fi
    if [[ ! "$raw" =~ ^([A-Za-z0-9][A-Za-z0-9_.-]*/[A-Za-z0-9][A-Za-z0-9_.-]*)/(\.github/workflows/[A-Za-z0-9_.-]+\.ya?ml)@([0-9a-f]{40})$ ]] || [[ -z "$job" ]]; then
      finding SOURCE-PIN "$caller/$job" "Remote reusable must have an exact workflow path and full lowercase 40-hex source pin: $raw"; continue
    fi
    jq -nc --arg workflow "$caller" --arg job "$job" --arg repository "${BASH_REMATCH[1],,}" --arg path "${BASH_REMATCH[2]}" --arg commit "${BASH_REMATCH[3]}" \
      '{workflow:$workflow,job:$job,repository:$repository,path:$path,commit:$commit}' >> "$TMP/roots"
  done < <(extract_uses "$workflow")
done
shopt -u nullglob
jq -s 'sort_by(.workflow,.job)' "$TMP/roots" > "$TMP/actual-roots.json"
jq '[.sources[]|select(.role=="active")|. as $s|.callers[]|{workflow,job,repository:($s.repository|ascii_downcase),path:$s.path,commit:$s.commit}]|sort_by(.workflow,.job)' "$MANIFEST" > "$TMP/expected-roots.json"
if ! cmp -s "$TMP/actual-roots.json" "$TMP/expected-roots.json"; then
  finding SOURCE-ACTIVE-MISSING "$MANIFEST" 'Active caller/job/path/commit bindings differ from the actual workflow roots (historical paths cannot satisfy active calls)'
fi

# Isolate Git configuration and transports; raw object hashes are checked in
# addition to tree membership. Replacement refs, grafts and symlinks fail shut.
git_data() {
  env -u GIT_DIR -u GIT_WORK_TREE -u GIT_INDEX_FILE -u GIT_OBJECT_DIRECTORY -u GIT_ALTERNATE_OBJECT_DIRECTORIES \
    -u GIT_CONFIG -u GIT_CONFIG_PARAMETERS -u GIT_CONFIG_COUNT GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 GIT_NO_REPLACE_OBJECTS=1 GIT_NO_LAZY_FETCH=1 \
    git --no-replace-objects -c core.hooksPath=/dev/null -c core.fsmonitor=false -c credential.helper= -c http.extraHeader= \
    -c protocol.allow=never -c protocol.https.allow=always -c http.followRedirects=false -c fetch.fsckObjects=true \
    --git-dir "$BARE" "$@"
}
verify_object() {
  local type="$1" oid="$2" dest="$3" actual
  git_data cat-file "$type" "$oid" > "$dest" 2>/dev/null || return 1
  actual="$(git_data hash-object -t "$type" --stdin < "$dest")" || return 1
  [[ "$actual" = "$oid" ]]
}
object_failure() {
  local type="$1" oid="$2" subject="$3"
  if git_data cat-file -e "$oid" 2>/dev/null || [[ -e "$BARE/objects/${oid:0:2}/${oid:2}" ]]; then
    finding SOURCE-ORIGIN "$subject" "Cached $type object is present but corrupt, has the wrong type, or fails its raw content hash"
  else
    notrun SOURCE-MISSING "$subject" "Offline cache lacks required $type object $oid"
  fi
}
source_bytes() {
  local repository="$1" commit="$2" path="$3" blob="$4" dest="$5" tree entry mode type oid name part i
  BARE="$CACHE/$repository.git"
  for entry in "$CACHE" "$CACHE/${repository%/*}" "$BARE"; do
    if [[ -L "$entry" ]]; then finding SOURCE-ORIGIN "$repository@$commit" 'Symlink in source-cache path'; return 1; fi
  done
  if [[ ! -d "$BARE" ]] && [[ "$FETCH" -eq 1 ]]; then
    mkdir -p "$(dirname "$BARE")"
    if ! env GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 git -c core.hooksPath=/dev/null init --bare --object-format=sha1 "$BARE" >/dev/null 2>&1; then
      notrun SOURCE-MISSING "$repository@$commit" 'Cannot initialize isolated bare source cache'; return 1
    fi
  fi
  if [[ ! -d "$BARE" ]]; then notrun SOURCE-MISSING "$repository@$commit" 'No authenticated bare cache; bootstrap with --fetch-sources'; return 1; fi
  if [[ "$(git_data rev-parse --is-bare-repository 2>/dev/null)" != true ]]; then finding SOURCE-ORIGIN "$repository@$commit" 'Source cache is not a bare Git repository'; return 1; fi
  if [[ -e "$BARE/info/grafts" ]] || [[ -L "$BARE/info/grafts" ]] || [[ -n "$(git_data for-each-ref --format='%(refname)' refs/replace/ 2>/dev/null)" ]] ||
     git_data config --local --get-regexp '^(include\.|includeif\.|url\.|extensions\.partialclone|remote\..*\.promisor)' >/dev/null 2>&1; then
    finding SOURCE-ORIGIN "$repository@$commit" 'Cache has grafts, replacement refs or source-rewriting configuration'; return 1
  fi
  if ! verify_object commit "$commit" "$TMP/commit"; then
    if git_data cat-file -e "$commit" 2>/dev/null || [[ -e "$BARE/objects/${commit:0:2}/${commit:2}" ]]; then
      finding SOURCE-ORIGIN "$repository@$commit" 'Cached commit exists but its raw content/type/hash is corrupt or mismatched'; return 1
    fi
    if [[ "$FETCH" -eq 1 ]]; then
      if ! git_data fetch --quiet --no-tags --depth=1 --no-recurse-submodules "https://github.com/$repository.git" "$commit" > "$TMP/fetch.out" 2> "$TMP/fetch.err"; then
        notrun SOURCE-MISSING "$repository@$commit" 'Exact public HTTPS source fetch failed'; return 1
      fi
    else notrun SOURCE-MISSING "$repository@$commit" 'Requested commit absent or corrupt in offline cache'; return 1; fi
  fi
  if ! verify_object commit "$commit" "$TMP/commit"; then finding SOURCE-ORIGIN "$repository@$commit" 'Commit content hash does not match requested origin'; return 1; fi
  tree="$(sed -n '1s/^tree //p' "$TMP/commit")"
  [[ "$tree" =~ ^[0-9a-f]{40}$ ]] || { finding SOURCE-ORIGIN "$repository@$commit" 'Invalid commit tree'; return 1; }
  IFS=/ read -r -a parts <<< "$path"
  for ((i=0;i<${#parts[@]};i++)); do
    part="${parts[$i]}"
    if ! verify_object tree "$tree" "$TMP/tree"; then object_failure tree "$tree" "$repository@$commit/$path"; return 1; fi
    entry="$(git_data ls-tree "$tree" -- "$part")"
    IFS=$' \t' read -r mode type oid name <<< "$entry"
    if [[ "$name" != "$part" ]]; then finding SOURCE-BLOB "$repository@$commit/$path" 'Requested source path absent from exact commit tree'; return 1; fi
    if [[ "$i" -lt "$((${#parts[@]}-1))" ]]; then
      if [[ "$mode" != 040000 ]] || [[ "$type" != tree ]]; then finding SOURCE-BLOB "$repository@$commit/$path" 'Non-tree or symlink in source path'; return 1; fi
      tree="$oid"
    else
      if [[ "$mode" != 100644 && "$mode" != 100755 ]] || [[ "$type" != blob ]] || [[ "$oid" != "$blob" ]]; then
        finding SOURCE-BLOB "$repository@$commit/$path" 'Blob identity or regular-file mode differs from manifest'; return 1
      fi
      if ! verify_object blob "$blob" "$dest"; then object_failure blob "$blob" "$repository@$commit/$path"; return 1; fi
    fi
  done
}

: > "$TMP/observed.jsonl"
index=0
while IFS= read -r source; do
  repository="$(jq -r '.repository|ascii_downcase' <<< "$source")"; commit="$(jq -r .commit <<< "$source")"
  path="$(jq -r .path <<< "$source")"; blob="$(jq -r .blob <<< "$source")"; subject="$repository@$commit/$path"
  index=$((index+1)); bytes="$TMP/source-$index.yml"
  source_bytes "$repository" "$commit" "$path" "$blob" "$bytes" || continue
  parse_incomplete=0
  : > "$TMP/uses"
  while IFS=$'\t' read -r tag kind raw job; do
    if [[ "$tag" = UNPARSED ]]; then notrun SOURCE-PARSE "$subject" "$kind"; parse_incomplete=1; continue; fi
    if [[ "$kind" = job ]]; then notrun SOURCE-LOCAL "$subject" 'Nested reusable calls are outside this reviewed direct-action source profile'; parse_incomplete=1; continue; fi
    if ! ref="$(normalize_ref "$raw")"; then notrun SOURCE-LOCAL "$subject" "Unsupported local, Docker, dynamic or malformed uses: $raw"; parse_incomplete=1; continue; fi
    if [[ ! "${ref##*@}" =~ ^[0-9a-f]{40}$ ]]; then finding SOURCE-PIN "$subject" "Authenticated direct remote uses must have a literal full 40-hex source pin: $raw"; continue; fi
    jq -nc --arg kind "$kind" --arg ref "$raw" '{kind:$kind,ref:$ref}' >> "$TMP/uses"
  done < <(extract_uses "$bytes")
  [[ "$parse_incomplete" -eq 0 ]] || continue
  jq -s 'unique_by(.kind,.ref)|sort_by(.kind,.ref)' "$TMP/uses" > "$TMP/actual-uses"
  jq '.uses|map({kind,ref})|unique_by(.kind,.ref)|sort_by(.kind,.ref)' <<< "$source" > "$TMP/expected-uses"
  if ! cmp -s "$TMP/actual-uses" "$TMP/expected-uses"; then
    finding SOURCE-EDGE-DROPPED "$subject" 'Reviewed direct edge set differs from the exact immutable source'; continue
  fi
  jq -nc --arg repository "$repository" --arg commit "$commit" --slurpfile uses "$TMP/actual-uses" \
    '{repository:$repository,commit:$commit,uses:$uses[0]}' >> "$TMP/observed.jsonl"
done < <(jq -c '.sources[]' "$MANIFEST")

# Do not turn absent authenticated data into a misleading edge mismatch.
# No source-derived records are exported unless the whole evidence pass is
# complete, including every retained-review path.
if [[ -s "$TMP/findings" ]] || [[ -s "$TMP/notrun" ]]; then finish; exit $?; fi

# A retained source may extend an existing reviewed union, but cannot create
# an unrelated historical dependency population with no current caller.
while IFS= read -r group; do
  repository="$(jq -r .repository <<< "$group")"; commit="$(jq -r .commit <<< "$group")"; rid="$repository@$commit"
  if ! jq -e --arg r "$repository" --arg c "$commit" 'any(.sources[]; (.repository|ascii_downcase)==$r and .commit==$c and .role=="active")' "$MANIFEST" >/dev/null; then
    finding SOURCE-ACTIVE-MISSING "$rid" 'Retained-review evidence has no active source group'; continue
  fi
  : > "$TMP/group-edges"
  while IFS= read -r raw; do normalize_ref "$raw" >> "$TMP/group-edges" || finding SOURCE-DEPENDENCY "$rid" "Invalid edge: $raw"; done < <(jq -r '.uses[].ref' <<< "$group")
  LC_ALL=C sort -u "$TMP/group-edges" > "$TMP/group-normalized"
  awk -F '\t' -v repo="$repository" -v sha="$commit" '$0!~/^#/ && tolower($1)==repo && $2==sha {print $3}' "$RECEIPTS" | LC_ALL=C sort -u > "$TMP/receipt-edges"
  if ! cmp -s "$TMP/group-normalized" "$TMP/receipt-edges"; then finding SOURCE-RECEIPT "$rid" 'Authenticated reviewed source union differs from the active receipt rows'; fi
  edges="$(jq -Rsc 'split("\n")|map(select(length>0))' "$TMP/group-normalized")"
  if jq -e --arg k "$rid" 'has($k)' "$TMP/graph.json" >/dev/null; then
    prior="$(jq -c --arg k "$rid" '.[$k].uses//[]|sort|unique' "$TMP/graph.json")"
    if [[ "$prior" != "$(jq -c 'sort|unique' <<< "$edges")" ]]; then finding SOURCE-DEPENDENCY "$rid" 'Source parent record differs from its authenticated direct-edge union'; fi
  else
    jq --arg k "$rid" --arg sha "$commit" --argjson edges "$edges" '.+{($k):{ref:$sha,commit:("sha1-"+$sha),uses:$edges}}' "$TMP/supplement.json" > "$TMP/next.json"
    mv "$TMP/next.json" "$TMP/supplement.json"
    jq -s '.[0]*.[1]' "$TMP/vendor.json" "$TMP/supplement.json" > "$TMP/graph.json"
  fi
  jq -nc --arg rid "$rid" '$rid' >> "$TMP/groups"
done < <(jq -sc 'sort_by(.repository,.commit)|group_by(.repository,.commit)|map({repository:.[0].repository,commit:.[0].commit,uses:[.[].uses[]]})[]' "$TMP/observed.jsonl")

# Validate identities, exact overlaps and all dependency targets without
# claiming recursive source authentication beyond the reviewed direct sets.
while IFS= read -r entry; do
  key="$(jq -r .key <<< "$entry")"; normalized="$(normalize_ref "$key")" || normalized=""
  if [[ "$normalized" != "$key" ]] || ! jq -e '.value|(.ref|type=="string" and test("^[^[:space:]@]+$")) and (.commit|type=="string" and test("^sha1-[0-9a-f]{40}$")) and ((.uses//[])|type=="array")' <<< "$entry" >/dev/null; then
    finding SOURCE-DEPENDENCY "$key" 'Malformed supplemental dependency identity'; continue
  fi
  ref="$(jq -r .value.ref <<< "$entry")"; digest="$(jq -r .value.commit <<< "$entry")"; named="${key##*@}"
  if [[ "$ref" =~ ^[0-9a-f]{40}$ ]] && [[ "$digest" != "sha1-$ref" ]]; then finding SOURCE-DEPENDENCY "$key" 'Full-SHA ref and resolved commit disagree'; fi
  if [[ "$ref" == sha1-* ]]; then finding SOURCE-DEPENDENCY "$key" 'Dependency ref must not use the commit digest prefix'; fi
  if [[ "$named" =~ ^[0-9a-f]{40}$ ]]; then
    [[ "$digest" = "sha1-$named" ]] || finding SOURCE-DEPENDENCY "$key" 'SHA key and resolved commit disagree'
  elif [[ "$named" != "$ref" ]]; then finding SOURCE-DEPENDENCY "$key" 'Named ref and dependency ref disagree'; fi
  if jq -e --arg k "$key" 'has($k)' "$TMP/vendor.json" >/dev/null; then
    left="$(jq -cS --arg k "$key" '.[$k]|{ref,commit,uses:(.uses//[]|sort)} + (if has("owner_id") then {owner_id} else {} end) + (if has("repo_id") then {repo_id} else {} end)' "$TMP/vendor.json")"
    right="$(jq -cS '.value|{ref,commit,uses:(.uses//[]|sort)} + (if has("owner_id") then {owner_id} else {} end) + (if has("repo_id") then {repo_id} else {} end)' <<< "$entry")"
    [[ "$left" = "$right" ]] || finding SOURCE-DEPENDENCY "$key" 'Shared vendor and supplemental records disagree'
  fi
done < <(jq -c 'to_entries[]' "$TMP/supplement.json")
declare -A VISITING=() DONE=()
visit() {
  local key="$1" depth="$2" child
  [[ -z "${DONE[$key]:-}" ]] || return 0
  if [[ "$depth" -gt 100 ]] || [[ -n "${VISITING[$key]:-}" ]]; then finding SOURCE-DEPENDENCY "$key" 'Cyclic or excessively deep supplemental graph'; return 0; fi
  if ! jq -e --arg k "$key" 'has($k)' "$TMP/graph.json" >/dev/null; then finding SOURCE-DEPENDENCY "$key" 'Required direct/transitive dependency record is missing'; return 0; fi
  VISITING["$key"]=1
  while IFS= read -r child; do
    if ! normalized="$(normalize_ref "$child")" || [[ "$child" != "$normalized" ]]; then finding SOURCE-DEPENDENCY "$key" "Invalid nested edge: $child"; continue; fi
    visit "$child" "$((depth+1))"
  done < <(jq -r --arg k "$key" '.[$k].uses//[]|.[]' "$TMP/graph.json")
  unset 'VISITING[$key]'; DONE["$key"]=1
}
while IFS= read -r key; do visit "$key" 0; done < <(jq -r 'keys[]' "$TMP/supplement.json")
finish
