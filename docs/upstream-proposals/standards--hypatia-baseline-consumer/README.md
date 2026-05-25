<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
# Proposal: `hyperpolymath/standards` — `.hypatia-baseline.json` consumer

**Target repo:** `hyperpolymath/standards`
**Author:** drafted from `hyperpolymath/gitbot-fleet` issue triage of PR #198
**Date:** 2026-05-24

## Problem

`hyperpolymath/standards/.github/workflows/governance-reusable.yml@main` runs
`Language / package anti-pattern policy` against every repo that calls it.
The rule scans the working tree for banned-language files (e.g. ReScript `.res`)
and fails the gate if any are present.

Repos in the estate carry a per-repo `.hypatia-baseline.json` listing
acknowledged findings — same shape as the Hypatia findings themselves
(`severity`, `rule_module`, `type`, `file`). The convention is well-formed,
in active use (`hyperpolymath/gitbot-fleet/.hypatia-baseline.json` has 59
entries), and structurally exactly what an exemption mechanism needs.

**But the governance gate does not consume it.** PR #198
(`hyperpolymath/gitbot-fleet#198`) has been blocked for hours by the
`banned_language_file` rule firing on exactly the `.res` files that are
*already listed* in that repo's baseline. The PR author tried inventing a
second format (`.hypatia-ignore`, never read by anything) as a workaround.

## Fix

Make `governance-reusable.yml` read the calling repo's
`.hypatia-baseline.json` and filter findings before the gate decides.

This proposal ships:

1. **`scripts/apply-baseline.sh`** — pure bash + jq, no external deps.
   Reads a findings file + the calling repo's baseline, emits filtered
   findings, exits non-zero only if unfiltered blocking findings remain.
2. **`.machine_readable/hypatia-baseline.schema.json`** — JSON Schema
   formalising the file shape. Includes the existing required fields
   (`severity`, `rule_module`, `type`, `file`) plus three optional
   forward-compatible extensions (`file_pattern`, `severity_override`,
   `expires_at`, `note`).
3. **`workflows/governance-reusable.yml.patch`** — the YAML diff to wire
   the baseline step into the existing reusable workflow. Drop-in.
4. **`docs/HYPATIA-BASELINE-FORMAT.adoc`** — authoritative format doc.
5. **`docs/EXEMPTION-MECHANISMS.adoc`** — convention doc clarifying when
   to use `.hypatia-baseline.json` vs the estate-wide
   `bot_exclusion_registry.a2ml` vs (proposed) per-PR exemptions.

## File map

| File in this proposal | Target path in `hyperpolymath/standards` |
|---|---|
| `scripts/apply-baseline.sh` | `scripts/apply-baseline.sh` |
| `.machine_readable/hypatia-baseline.schema.json` | `.machine_readable/hypatia-baseline.schema.json` |
| `workflows/governance-reusable.yml.patch` | apply to `.github/workflows/governance-reusable.yml` |
| `docs/HYPATIA-BASELINE-FORMAT.adoc` | `docs/HYPATIA-BASELINE-FORMAT.adoc` |
| `docs/EXEMPTION-MECHANISMS.adoc` | `docs/EXEMPTION-MECHANISMS.adoc` |

## Rollout plan

1. Land `apply-baseline.sh` + schema + docs in `standards/` (no behaviour
   change yet).
2. Land the workflow patch in **advisory mode** first (gate still passes
   on suppressed findings, but logs unfiltered count). One week of soak.
3. Flip to **blocking mode**: gate fails only on findings *not* matched
   by baseline.
4. Open coordinator tracking issues per consumer repo to either
   (a) accept new findings into baseline, or (b) fix the underlying code.

## Why not just merge `.hypatia-ignore` support too?

Two formats with overlapping semantics is what got us into this mess.
The proposal kills `.hypatia-ignore` explicitly (see
`docs/EXEMPTION-MECHANISMS.adoc`). If a per-PR exemption slot is needed,
it should be a designed mechanism (PR-body marker or label), not another
drive-by file convention.

## Companion upstream work (out of scope for this PR)

- **`hyperpolymath/hypatia`** scanner should emit `baseline_status` on
  every finding (`new` / `acknowledged` / `expired`). That lets the gate
  do baseline matching at finding-emission time and produce richer PR
  comments. Tracked separately.
- **`hyperpolymath/hypatia`** should grow a `--stale-baseline-check`
  mode that fails if the baseline references files that no longer exist
  (after `hyperpolymath/gitbot-fleet#148` migrates the ReScript subtree,
  most of its baseline entries will become ghosts).
