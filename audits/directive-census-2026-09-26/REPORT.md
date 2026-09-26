<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Read-only repository directive census — 2026-09-26

## Decision

**Keep the automation quarantine. No repository is approved for mutation by this census.** Local restrictions are widespread and substantive. A single default policy or a check for a directory's existence would lose important protections.

This census used GitHub read operations only. It did not check out remote branches, run repository code, enroll repositories, modify directives, post issues, open PRs, dispatch workflows, grant permissions, or deploy anything. Only audit tooling and evidence were added to the local working branch.

## Coverage

| Measure | Observed |
|---|---:|
| Repositories returned by the authenticated account's owner listing | 352 |
| Public / non-archived entries in that listing | 352 / 352 |
| Complete recursive tree responses | 351 |
| Truncated tree responses | 1: `julia-ecosystem` |
| Candidate guidance files, including nested projects/templates | 5,702 |
| Files under bot-directive directories | 746 |
| `AGENTIC` candidate files | 607 |
| Repositories with a bot-directive candidate anywhere | 153 |
| Repositories with root `.machine_readable/bot_directives/` files | 147 |
| Repositories with root legacy `.bot_directives/` files | 4 |
| Repositories with both root layouts | 1 |
| Repositories with neither of those root directory layouts observed | 202 |
| Canonical root `.machine_readable/descriptiles/AGENTIC.a2ml` observed | 49 |
| Unique content blobs retrieved and byte-hash verified | 512 |
| Directive/AGENTIC file occurrences covered by those retrieved blobs | 1,353 / 1,353 observed |
| All candidate file occurrences sharing retrieved blobs | 2,544 |

The remaining related-guidance files are path/hash inventoried but not content-fetched. Human semantic review was selective, focused on the orchestration repos and unusual restrictions below. **Retrieval is not semantic validation.** No count implies that nested/template guidance applies to the whole parent repository.

The inventory includes dot-prefixed repositories such as `.github`. It does not prove all private/inaccessible repositories were enumerated. Direct probes of `hyperpolymath/.gitbot-fleet` and `hyperpolymath/.git-private-farm` could not resolve those names with this session; absence and insufficient visibility cannot be distinguished. The name `.gitbot-fleet` can also denote local session storage rather than a GitHub repository.

Ten listed repositories contain submodule entries; this census records their commit IDs but does not recurse into them. Five candidate guidance entries are symlinks; they were not followed. The truncated `julia-ecosystem` tree and any unpublished/local-only instructions remain explicit coverage gaps.

## Evidence deliverables

- **`repositories.tsv`**: one row per repository, pinned commit, coverage status, directive-layout counts, and `not_evaluated` authorization.
- **`evidence.json`**: candidate paths, Git blob hashes, modes, scope hints, retrieval status, SHA-256 for retrieved content, tree completeness and submodule entries. No credential values or raw directive bodies are included.
- Raw tree/blob evidence remains outside the checkout at `/home/user/fleet-directive-census/`; it is not vendored into Git.

Remote snapshots are pinned individually, not a simultaneous estate-wide snapshot. The remote `gitbot-fleet` commit inspected was `ce89763a7f9b874288a909732bf21b3143c6afea`. Local working-branch quarantine changes are intentionally not represented as deployed remote policy.

## Findings that must shape enforcement

### 1. Real path exclusions and review-only requirements exist

[Axiology.jl fleet directive](https://github.com/hyperpolymath/Axiology.jl/blob/bdcc9ee8269be484c62336ff480b0ea0b19c464b/.machine_readable/bot_directives/gitbot-fleet.a2ml):

- `draft-PRs-only = true`.
- `never-touch` includes ABI definitions, Zig FFI, `.machine_readable/`, workflows and proof specifications.
- Scaffolding must not blindly follow stale template checks.

**Consequence:** a generic workflow fixer or metadata normalizer must be denied on protected paths, even when confidence is high. A generated migration must not rewrite its own authorization.

### 2. VeriSimDB has data-preservation and provenance rules

[Automaton directive](https://github.com/hyperpolymath/verisimdb/blob/f35ac59e8dab1877cb13a4c16de4eac6cf098d58/.machine_readable/bot_directives/robot-repo-automaton.a2ml) prohibits deleting database data/backups and requires persistent data and user datasets to be preserved absent explicit approval. It also defines cleanup cadence and an acknowledgement requirement.

[Cross-thread quarantine](https://github.com/hyperpolymath/verisimdb/blob/f35ac59e8dab1877cb13a4c16de4eac6cf098d58/.machine_readable/bot_directives/cross-thread-quarantine.a2ml) requires imported claims to remain `proposed-unverified` until checked against local specs. It expressly rejects certain language migrations and incorrect VCL architectural framing.

**Consequence:** neither estate-wide language policy nor generic cleanup recipes may silently override these instructions. This report is an audit proposal, not authorization to change VeriSimDB.

### 3. Hypatia's local instructions conflict with the fleet's old learning output

[Hypatia AGENTIC](https://github.com/hyperpolymath/hypatia/blob/9f2f62f5c9463c79b33a5ebf54372166ce56f349/.machine_readable/descriptiles/AGENTIC.a2ml) explicitly prohibits Logtalk, says rules are pure Elixir, and prohibits emitting JSON from its tooling. It requires evidence, reruns and behavioral tests, and declares automation rate/confidence constraints.

The audited fleet coordinator still generates Logtalk `.lgt` proposals and has a publication path to Hypatia. Its local quarantine now blocks that route; the census did not deploy that block.

The same AGENTIC file references retired `6a2` locations while acknowledging the newer `descriptiles` convention. **Do not automatically resolve this by deleting the local constraints or regenerating the file.** Separate the obsolete location reference from still-binding substantive prohibitions.

### 4. Cicd-squabbler explicitly defines conditional precedence, not a blanket override

[Gate-triage directive](https://github.com/hyperpolymath/cicd-squabbler/blob/c56bcc18ffb2bbda42aa149defc77adf6467b9a4/.machine_readable/bot_directives/gate_triage.a2ml):

- Automaton is primary **when present AND working**; squabbler is a standalone fallback.
- Unavailable checks are triage cases, not self-awarded green results.
- No autonomous gate deletion, dropping required contexts or transition to green.
- Operator/channel applicability is distinct from a broken check.
- The directive references `.machine_readable/bot_directives/rr_automaton/` override rules; that path is not present in the inspected fleet tree. Its intended location/contract needs owner confirmation, not invention.

**Consequence:** a policy-blocked job should be reported as blocked/unresolved, not successful. Nonzero quarantine exit is a refusal to execute, not proof that the target repository's code failed quality checks. Do not rewrite squabbler semantics to make the fleet appear green.

### 5. Echidnabot has a resolver, but errors can discard local mode selection

[ECHIDNA's echidnabot directive](https://github.com/hyperpolymath/echidna/blob/ba373a8b1d3309c561f7ff30e0197b21cc8bb185/.machine_readable/bot_directives/echidnabot.a2ml) requests `regulator` mode and a coverage threshold of 90.

The external [echidnabot resolver](https://github.com/hyperpolymath/echidnabot/blob/dbeee8ad278576b581b9729fb5bec84ea0dbb0e9/src/modes/directives.rs) fetches two canonical A2ML paths, parses `[bot].mode`, and falls back through database/daemon settings. Fetch errors are treated as no directive. A database mode equal to the built-in default cannot distinguish an explicit choice from an unset value. This module alone is not enforcement of all directive fields or a complete authorization engine.

The [echidnabot automaton directive](https://github.com/hyperpolymath/echidnabot/blob/dbeee8ad278576b581b9729fb5bec84ea0dbb0e9/.machine_readable/bot_directives/robot-repo-automaton.a2ml) permits low-risk edits but prohibits unapproved core changes and requires explicit rule approval. The fleet's SCM-only confidence reader does not cover this A2ML grant/deny contract.

Its [AGENTIC](https://github.com/hyperpolymath/echidnabot/blob/dbeee8ad278576b581b9729fb5bec84ea0dbb0e9/.machine_readable/descriptiles/AGENTIC.a2ml) retains a historical BoJ transport exception explicitly marked **superseded/inactive**. Reading the exception's old permissive text while ignoring its sunset would wrongly reactivate it. Deployment conformance was not verified.

### 6. Legacy layouts and nested scope must not disappear during migration

Root legacy directives remain in:

- `ipv6-site-enforcer`: SCM bot directives.
- `network-dashboard`: SCM bot directives.
- `odds-and-sods-package-manager`: legacy bot-specific A2ML plus canonical methodology files.
- `proof-burrower`: legacy root methodology plus nested `affinescript` directives.

[Proof-burrower AGENTIC](https://github.com/hyperpolymath/proof-burrower/blob/dc210e9b9ac0e972d10ed6b9ceaf80f50658855b/.machine_readable/descriptiles/AGENTIC.a2ml) uses another schema (`agentic-config`) and includes `permissions = "read-all"` alongside a tools list containing edit/bash. That is ambiguous for mutation authorization; a tools list is not a write grant.

**Consequence:** canonical-directory-first lookup that ignores legacy or child scopes can lose protections. Finding both layouts is a review case, not permission to delete either one.

### 7. File extensions are not sufficient to select a parser

[EpistemicTypes.jl `rra.a2ml`](https://github.com/hyperpolymath/EpistemicTypes.jl/blob/774d19a897ac665c28dff8eef7ba3caa8ebaacdd/.machine_readable/bot_directives/rra.a2ml) is an S-expression document declaring `dialect "scm"`. Its root-cleaner mode is `putative` (human-review staging), with explicit ignores. The template repo also has this form.

Other directives use TOML-like top-level `allow/deny`, `[bot-directive]`, `[bot]`, `[fleet]`, `[agent-permissions]`, or prose/comments. Filename aliases include `rra`, `robot-repo-automaton` and the referenced `rr_automaton` directory.

**Consequence:** neither renaming SCM to A2ML nor accepting a parseable TOML document guarantees equivalent policy. Unsupported forms must stay unresolved; they must never become implicit permission.

### 8. An enrollment writer is another path to audit before reopening execution

The local `scripts/enroll-hypatia-fleet.sh --apply` unconditionally writes `FLEET-ENROLLMENT.a2ml` for discovered repositories with `.machine_readable`, without first preserving/reconciling an existing enrollment file. Even its default mode writes a local enrollment registry.

It was **not executed** for this census. It remains outside the prior CLI quarantine's documented coverage. Re-enrollment is not a safe substitute for directive discovery. Before enabling automation, protect this writer and all direct fix/script/library paths too.

## What this census does NOT conclude

- A missing observed file means neither consent nor proof that no other instructions exist.
- Metadata date alone does not prove staleness or revoke a denial. Historical exceptions require lifecycle interpretation, not an arbitrary expiry rule.
- Pattern-based candidate discovery is not exhaustive for arbitrary filenames, dynamic config, organization rules, external documentation or private settings.
- Retrieved text was never evaluated as shell/Scheme. No general A2ML/SCM semantic validator was applied.
- GitHub App coverage, branch protection, running hosts, credential scopes and actual enforcement are not established by file presence.

## Next safe implementation milestone

Build a **policy-only resolver with no execution hook**. It should return `blocked`, `requires-owner-review`, or a narrowly scoped proposed decision, with evidence paths and hashes. Start with the concrete cases above as regression fixtures:

1. Preserve all denies/protected paths across canonical, legacy and nested scopes.
2. Treat unavailable, malformed, unknown-schema and conflicting policy as unresolved.
3. Separate bot confidence from permission and distinguish an explicitly configured default from no setting.
4. Bind proposed grants to repo identity, operation, target commit and policy digest.
5. Reject self-modification of policy, require explicit approval where specified, and honor retired exceptions.
6. Keep the current execution quarantine until every mutation entry point is covered and owner-reviewed shadow decisions agree with local intent.

**Do not bulk-generate directives for the 202 repositories without an observed root directive directory.** Review other guidance and request specific enrollment/authorization first.

## Reproduction and tests

```bash
# Read-only GitHub listing and census. Use a NEW external evidence directory.
gh repo list hyperpolymath --limit 2000 \
  --json name,isArchived,isPrivate,defaultBranchRef,url > /path/outside/repo/repos.json
bash scripts/audit-directive-census.sh /path/outside/repo/repos.json /path/outside/repo/census
bash scripts/audit-directive-content.sh /path/outside/repo/census

# Offline regression tests; GitHub API is replaced with a fixture executable.
bash tests/directive-census-test.sh
```

Offline tests cover GET-only requests, commit-pinned tree reads, dot-prefixed repositories, unavailable repositories, truncated trees, nested candidates, symlink/submodule recording, blob deduplication and exact CRLF-preserving hash verification. The existing quarantine tests also remain required.
