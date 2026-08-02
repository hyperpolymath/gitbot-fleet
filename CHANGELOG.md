<!--
SPDX-License-Identifier: CC-BY-SA-4.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Changelog

All notable changes to `gitbot-fleet` will be documented in this file.

This file is generated from conventional commits by the
[`changelog-reusable.yml`](https://github.com/hyperpolymath/standards/blob/main/.github/workflows/changelog-reusable.yml)
workflow (`hyperpolymath/standards#206`). Adopt the workflow in this repo's CI to keep this file in sync automatically — see
[`templates/cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml)
for the canonical config.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- feat(sustainabot): migrate bot-integration from ReScript to AffineScr… (#199)
- feat(scripts): add fix-tracked-package-lock.sh for standards#67 enforcement (#170)
- feat(robot-repo-automaton): canonical RSR skeleton generator (rsr-template-repo#48) (#160)
- feat(rhodibot): add offline `check` CLI subcommand for CI gating (#150)
- feat(gsbot)!: faithful Rust/SPARK port — eliminate Python from the fleet (#145)

### Fixed

- fix(examples): migrate SafeDOMExample.affine to current AffineScript grammar (closes #208) (#210)
- fix(sustainabot): parse-blocking OCaml-isms and HANDLE-keyword name (Refs #148) (#206)
- fix(licence): clear scaffold-placeholder leak (isolated; dirty repo) (#167)
- fix(ci): pin upload-artifact to valid SHA in fix-unpinned-actions.sh generator (Refs standards#48) (#163)
- fix(ci): bump a2ml/k9-validate-action pins to canonical (standards#85) (#162)
- fix(ci): sync hypatia-scan.yml to canonical (kill cd-scanner build drift) (#161)
- fix: resolve gitbot-fleet legacy hypatia findings at source; de-baseline (#152)
- fix(ci): adopt canonical hypatia-scan.yml (env.HOME/scanner-layout + Comment-step gate) (#151)
- fix(shared-context): clear pre-existing clippy -D warnings debt (item B) (#149)
- fix: stagger estate push to stop CI thundering-herd (#147)

### Documentation

- docs(archive): session record for sustainabot #148 hand-port validation (#209)
- docs: promote stub .adoc to faithful AsciiDoc from canonical .md (Item 11 Group A) (#159)

### CI

- ci: redistribute concurrency-cancel guard to read-only check workflows (#165)
- ci(hypatia-scan): restore hardened workflow (supersedes #129) (#142)
- ci(workflow): adopt hardened hypatia-scan from hyperpolymath/hypatia#237 (#126)
- ci: bump actions/upload-artifact SHA to current v4 (#123)
- ci(hypatia-scan): wire FLEET_DISPATCH_TOKEN to its own secret

## Pre-history

Prior commits to this file's introduction are recorded in git history but not formally classified into Keep-a-Changelog sections. To backfill, run `git cliff -o CHANGELOG.md` locally using the canonical [`cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml) — this is one-shot mechanical work.

---

<!-- This file was seeded by the 2026-05-26 estate tech-debt audit follow-up (Row-2 Phase 3); see [`hyperpolymath/standards/docs/audits/2026-05-26-estate-documentation-debt.md`](https://github.com/hyperpolymath/standards/blob/main/docs/audits/2026-05-26-estate-documentation-debt.md). -->
