# Branch Protection Setup: Static Analysis Gate

> SPDX-License-Identifier: PMPL-1.0-or-later

This document explains how to wire the `static-analysis-gate.yml` workflow into
GitHub branch protection so that every PR must pass panic-attack and hypatia
before merging.

---

## Required Status Checks

In your repository's **Settings > Branches > Branch protection rules** for
`main` (and/or `master`), enable:

| Status check name                         | Source workflow              |
|-------------------------------------------|-----------------------------|
| `panic-attack assail`                     | `static-analysis-gate.yml`  |
| `Hypatia neurosymbolic scan`              | `static-analysis-gate.yml`  |
| `Deposit findings for gitbot-fleet`       | `static-analysis-gate.yml`  |

### Recommended branch protection settings

- **Require status checks to pass before merging** — enabled
- **Require branches to be up to date before merging** — enabled
- **Include administrators** — enabled (lead by example)
- **Restrict who can push to matching branches** — optional, recommended

> **Note:** The `Hypatia Security Scan` check from the older `hypatia-scan.yml`
> workflow is a *separate* status check. You may keep both or retire the older
> one; `static-analysis-gate.yml` is the unified replacement.

---

## Enabling the Workflow in Any Repo

### From the RSR template

Repos bootstrapped from `rsr-template-repo` already include
`.github/workflows/static-analysis-gate.yml`. Replace `hyperpolymath` with your
GitHub org/user name:

```bash
sed -i 's/hyperpolymath/hyperpolymath/g' .github/workflows/static-analysis-gate.yml
```

### Adding to an existing repo

Copy the workflow file into the target repo:

```bash
cp /path/to/rsr-template-repo/.github/workflows/static-analysis-gate.yml \
   your-repo/.github/workflows/static-analysis-gate.yml

cd your-repo
sed -i 's/hyperpolymath/hyperpolymath/g' .github/workflows/static-analysis-gate.yml
git add .github/workflows/static-analysis-gate.yml
git commit -m "ci: add static analysis gate for branch protection"
git push
```

Then add the status checks in **Settings > Branches** as described above.

---

## Local Pre-Push Hook

A local hook mirrors the CI gate so developers catch critical findings before
pushing. Install it from `gitbot-fleet/hooks/pre-push-gate.sh`:

```bash
# Symlink (recommended — always picks up updates)
ln -sf ~/Documents/hyperpolymath-repos/gitbot-fleet/hooks/pre-push-gate.sh \
       .git/hooks/pre-push

# Or copy
cp ~/Documents/hyperpolymath-repos/gitbot-fleet/hooks/pre-push-gate.sh \
   .git/hooks/pre-push
chmod +x .git/hooks/pre-push
```

The hook gracefully degrades: if neither panic-attack nor hypatia is installed
locally, it prints a notice and allows the push (CI will catch it).

---

## How Findings Flow Back to Hypatia for Learning

```
Developer pushes / opens PR
        |
        v
static-analysis-gate.yml runs
        |
        +---> panic-attack assail  ---> findings JSON artifact
        +---> hypatia scan         ---> findings JSON artifact
        |
        v
deposit-findings job
        |
        +---> Combines both into unified-findings.json
        +---> Uploads as "unified-findings" artifact (90-day retention)
        |
        v
gitbot-fleet scanner (scheduled)
        |
        +---> Queries GitHub API for repos with unified-findings artifacts
        +---> Downloads and ingests into hypatia's learning corpus
        +---> Feeds rhodibot / echidnabot / sustainabot for pattern recognition
        |
        v
Hypatia learning engine
        |
        +---> Updates neurosymbolic rules based on recurring patterns
        +---> Feeds improved rules back into hypatia-cli.sh
        +---> Cycle repeats with better detection on next scan
```

### Artifact-based ingestion

The `deposit-findings` job uploads a `unified-findings` artifact to each
workflow run. The gitbot-fleet's `learning-monitor.sh` script periodically:

1. Lists recent workflow runs across enrolled repos.
2. Downloads `unified-findings` artifacts.
3. Parses the JSON envelope (`schema_version`, `repository`, `commit_sha`,
   `timestamp`, `findings[]`).
4. Deduplicates findings already in the learning corpus.
5. Submits new findings to hypatia's pattern database.

No secrets or special tokens are needed beyond the default `GITHUB_TOKEN` —
artifacts are readable by the repo owner.

---

## Note on oikos/sustainabot

The `sustainabot` bot was registered in the gitbot-fleet as a sustainability
and maintenance scanner, but was never fully wired into the CI pipeline.
`static-analysis-gate.yml` replaces the role sustainabot was intended to fill:

- **What sustainabot was supposed to do:** Run periodic checks and flag
  maintenance debt.
- **What static-analysis-gate does instead:** Runs on every PR and push,
  combining panic-attack (code quality / dangerous patterns) and hypatia
  (neurosymbolic security analysis) into a single required gate.
- **sustainabot's remaining role:** It can still operate as a *scheduled*
  scanner for repos that have not yet adopted the gate workflow. Over time,
  as all repos adopt `static-analysis-gate.yml`, sustainabot's workload
  naturally decreases to zero.

The gitbot-fleet coordinator (`fleet-coordinator.sh`) should be updated to
recognise `unified-findings` artifacts as the primary input channel, replacing
the ad-hoc sustainabot submission path.

---

## Checklist for New Repos

- [ ] Copy `static-analysis-gate.yml` into `.github/workflows/`
- [ ] Replace `hyperpolymath` placeholder
- [ ] Push to trigger first run
- [ ] Add required status checks in branch protection settings
- [ ] Install local pre-push hook (optional but recommended)
- [ ] Verify `unified-findings` artifact appears after first run
