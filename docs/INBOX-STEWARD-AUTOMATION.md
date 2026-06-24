// SPDX-License-Identifier: CC-BY-SA-4.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
// Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

# Inbox Steward Automation System

**Status:** Active & Operational  
**Last Updated:** 2026-06-03  
**Version:** 1.0.0  

---

## Overview

The Inbox Steward Automation System is a closed-loop automation pipeline that:

1. **Monitors** pull requests across the repository fleet
2. **Validates** they pass all required CICD checks
3. **Auto-merges** qualifying PRs from trusted contributors
4. **Learns** patterns from merged PRs
5. **Propagates** learned rules to all repos
6. **Monitors** compliance across the entire fleet

This creates a self-improving automation system where lessons from one repo benefit all repos.

---

## System Architecture

```
┌─────────────────────────┐     ┌─────────────────────────┐     ┌─────────────────────────┐
│   gitbot-fleet           │────▶│   .git-private-farm     │────▶│      hypatia            │
│ (inbox-steward.yml)      │     │ (inbox-steward-          │     │ (inbox-steward-intake.yml)│
│                          │     │  propagate.yml           │     │                          │
│ 1. Monitors PRs          │     │ 1. Receives ruleset     │     │ 1. Receives reports     │
│ 2. Validates CICD passes  │     │    updates from Hypatia │     │ 2. Analyzes patterns    │
│ 3. Auto-merges if clean  │     │ 2. Applies to all repos  │     │ 3. Updates ruleset      │
│ 4. Dispatches to farm    │     │ 3. Triggers re-scanning │     │ 4. Dispatches to farm   │
│                          │     │ 4. Validates            │     │                          │
└─────────────────────────┘     └─────────────┬───────────┘     └─────────────┬───────────┘
                                              │                           │
                                              ▼                           ▼
                                       ┌─────────────────────────────────────┐
                                       │       CLOSED-LOOP FEEDBACK          │
                                       │  PRs → Patterns → Rules → Propagation │
                                       └─────────────────────────────────────┘
```

---

## Components

### 1. Inbox Steward (gitbot-fleet)

**File:** `.github/workflows/inbox-steward.yml`

**Purpose:** Automatically process PRs through the CICD gate

**Triggers:**
- Pull request events (opened, synchronize, ready_for_review, converted_to_draft, review_requested)
- Pull request review events (submitted, dismissed)
- Check suite completion
- Workflow run completion (Dogfood Gate, Scorecard Enforcer, Hypatia Security Scan, Static Analysis Gate)
- Scheduled (every 15 minutes)
- Manual (workflow_dispatch)

**Jobs:**

#### identify-passed-prs
- Scans all open PRs in the repository
- Filters for PRs that:
  - Are not draft
  - Have mergeable_state of "clean", "has_hooks", or "blocked"
  - Have all required checks passed (no failures)
- Outputs: List of PRs ready for processing

#### validate-prs
- Validates each PR against merge criteria:
  - Dogfood Gate passed
  - Scorecard Enforcer passed (if applicable)
  - Hypatia Security Scan passed
  - No blocking reviews (CHANGES_REQUESTED)
  - Has approvals OR from trusted contributor
- Outputs: Validated PRs and auto-merge candidates

#### auto-merge-prs
- Auto-merges qualifying PRs with squash merge
- Requires: Trusted contributor (hyperpolymath, dependabot, renovate) OR has approvals
- Dispatches success event to .git-private-farm
- Records results in shared-context/inbox-steward/

#### dispatch-to-hypatia
- Sends stewardship report to Hypatia
- Includes: Total PRs checked, validated count, auto-merged count
- Trigger: Always runs if identify or validate succeeded

#### summary
- Generates GitHub Actions summary
- Shows: PRs checked, passed checks, auto-merged count, failures

---

### 2. Inbox Steward Intake (hypatia)

**File:** `.github/workflows/inbox-steward-intake.yml`

**Purpose:** Process steward reports and learn patterns

**Triggers:**
- repository_dispatch (inbox-steward-report)
- Manual (workflow_dispatch)

**Jobs:**

#### record-report
- Records steward report to `data/inbox-steward-reports/`
- Commits report to git history
- Format: `{timestamp, source_repo, total_prs, validated_prs, auto_merged_prs, run_url}`

#### analyze-patterns
- Analyzes last 20 merged PRs from source repo
- Identifies patterns:
  - Common file types changed
  - Workflow file changes (frequency)
  - Common fix types (from PR titles)
  - Average PR size (additions/deletions)
- Suggests rule updates:
  - High workflow changes → Enhance workflow validation
  - Frequent dependency updates → Enable Dependabot automation
  - Large PRs → Warn on PR size limits

#### update-ruleset
- Updates `.hypatia-baseline.json` with learned rules
- Auto-applies critical/high priority updates
- Creates rule update file in `data/ruleset-updates/`
- Commits changes to git

#### dispatch-to-farm
- Sends propagation event to .git-private-farm
- Includes: patterns, rule_updates, action

#### summary
- Generates GitHub Actions summary
- Shows: Report metrics, analysis results, actions taken, rule updates applied

---

### 3. Inbox Steward Propagate (.git-private-farm)

**File:** `.github/workflows/inbox-steward-propagate.yml`

**Purpose:** Apply learned rules across the fleet

**Triggers:**
- repository_dispatch (inbox-steward-propagate)
- Manual (workflow_dispatch)

**Jobs:**

#### parse-propagation
- Parses the propagation event from Hypatia
- Extracts: source_repo, patterns, rule_updates, action

#### identify-targets
- Gets all repos from `farm-manifest.json` (`.repos | keys[]`)
- Identifies which repos need each rule type:
  - workflow_hygiene → Repos with `.github/workflows/`
  - dependency_automation → Repos with dependabot.yml or renovate.json
  - pr_size_limit → All repos
  - Default → All repos

#### apply-rules
- For each target repo:
  - Clones the repo
  - Applies each rule update (creates missing workflow files)
  - Commits to a branch
  - Opens a PR for review
- Logs all actions to `shared-context/inbox-steward-propagate/`

#### trigger-rescan
- Triggers dogfood-gate and hypatia-scan in updated repos
- Ensures new rules are validated

#### validate-propagation
- Verifies rules were applied correctly
- Checks for presence of workflow files

#### summary
- Generates GitHub Actions summary
- Shows: Propagation metrics, targets identified, rules applied, verification results

---

### 4. Inbox Steward Monitor (.git-private-farm)

**File:** `.github/workflows/inbox-steward-monitor.yml`

**Purpose:** Verify automation applies to all repos

**Triggers:**
- Scheduled (weekly, Sundays at 00:00 UTC)
- After Inbox Steward Propagate completes
- Manual (workflow_dispatch)

**Jobs:**

#### scan-farm
- Gets all repos from `farm-manifest.json`
- Outputs: Total repo count

#### check-compliance
- For each repo, checks for required workflows:
  - `inbox-steward.yml`
  - `dogfood-gate.yml`
  - `hypatia-scan.yml`
  - `scorecard-enforcer.yml`
- Identifies compliant and non-compliant repos

#### analyze-gaps
- Analyzes non-compliant repos
- Identifies:
  - Most commonly missing workflows
  - Repos with no automation at all
  - Compliance gaps by workflow type

#### report-to-hypatia
- Sends compliance report to Hypatia
- Includes: Total repos, compliant count, non-compliant count, gap analysis

#### generate-report
- Creates detailed GitHub Actions summary
- Generates markdown report file

---

## Merge Criteria

For a PR to be auto-merged by Inbox Steward:

| Criteria | Required Value | Notes |
|----------|----------------|-------|
| Draft status | `false` | Must not be a draft PR |
| Mergeable state | `clean`, `has_hooks`, or `blocked` | GitHub merge queue states |
| All checks passed | `true` | No failed check runs |
| Dogfood Gate | `success` | Required |
| Scorecard Enforcer | `success` | Required if exists |
| Hypatia Security Scan | `success` | Required |
| Blocking reviews | `0` | No CHANGES_REQUESTED reviews |
| Author | Trusted contributor OR has approval | Trusted: hyperpolymath, dependabot, renovate |

---

## Ruleset (Hypatia Baseline)

The following rules were added to `.hypatia-baseline.json`:

### Workflow Audit Rules

| Rule ID | Severity | Type | Action | Reason |
|---------|----------|------|--------|--------|
| inbox_steward_missing | low | workflow_audit | create | Inbox steward automation for PR processing |
| inbox_steward_intake_missing | low | workflow_audit | create | Hypatia intake for inbox steward reports |

### Inbox Automation Rules

| Rule ID | Severity | Type | Action | Reason |
|---------|----------|------|--------|--------|
| IA001 | medium | inbox_automation | enable_auto_merge | Enable auto-merge for trusted contributors |
| IA002 | medium | inbox_automation | require_dogfood_gate | All PRs must pass Dogfood Gate before auto-merge |
| IA003 | medium | inbox_automation | require_scorecard | All PRs must pass Scorecard Enforcer before auto-merge |
| IA004 | high | inbox_automation | require_hypatia_scan | All PRs must pass Hypatia Security Scan before auto-merge |
| IA005 | low | inbox_automation | trusted_contributors | Define trusted contributors (hyperpolymath, dependabot, renovate) |
| IA006 | medium | inbox_automation | propagate_rules | Propagate learned rules across fleet |
| IA007 | low | inbox_automation | monitor_application | Monitor application to all repos |

---

## Secrets Required

All workflows require these GitHub Secrets:

### FARM_PAT (in all repos)
- **Purpose:** Cross-repository API access
- **Permissions:**
  - `repo` (full control)
  - `workflow` (full control)
  - `security-events` (read/write)
  - `repository-projects` (read)
  - `actions` (read)
- **Scope:** All repositories in the org

### HYPATIA_DISPATCH_PAT (in gitbot-fleet and .git-private-farm)
- **Purpose:** Dispatch events to Hypatia
- **Permissions:** Same as FARM_PAT
- **Scope:** hyperpolymath/hypatia repository

---

## Files Modified

### gitbot-fleet
- `.github/workflows/inbox-steward.yml` (NEW)

### hypatia
- `.github/workflows/inbox-steward-intake.yml` (NEW)
- `.hypatia-baseline.json` (MODIFIED)

### dot-git-private-farm
- `.github/workflows/inbox-steward-propagate.yml` (NEW)
- `.github/workflows/inbox-steward-monitor.yml` (NEW)

---

## Execution History

### 2026-06-03 - Initial Deployment

**PRs Processed:**
1. gitbot-fleet#258 - Dependabot deps update
   - Status: ✅ MERGED
   - Commit: e61a1a60e9081e724a580347d7854c090d980677

2. gitbot-fleet#257 - Hypatia closed-loop contract
   - Status: ✅ MERGED
   - Commit: 2c3123177f510a0275a27bf362685128cf5bb22f

3. hypatia#434 - Idris proof surface gating
   - Status: ✅ MERGED
   - Commit: cc6e5e28098ca46e76d2ae2d3e8ebc6a646fe489

**Workflow Runs:**
- gitbot-fleet/inbox-steward: SUCCESS (26922966999)
- All configs clean and on main branches

---

## Testing

### Dry Run Mode
```bash
# Test inbox-steward without actual merges
gh workflow run inbox-steward.yml -f dry_run=true
```

### Manual Trigger
```bash
# Run inbox-steward manually
gh workflow run inbox-steward.yml -f dry_run=false

# Run monitor
cd dot-git-private-farm
gh workflow run inbox-steward-monitor.yml -f dry_run=false
```

### Verify Configuration
```bash
# Check all workflows exist
ls -la .github/workflows/inbox-steward*.yml

# Validate JSON syntax
jq empty .github/workflows/inbox-steward.yml
jq empty .github/workflows/inbox-steward-intake.yml
jq empty .github/workflows/inbox-steward-propagate.yml
jq empty .github/workflows/inbox-steward-monitor.yml

# Validate baseline
jq empty .hypatia-baseline.json
```

---

## Troubleshooting

### Workflow Parsing Errors
If you see `Unexpected symbol: '$'` in workflow validation:
- Ensure bash expressions are not used inside GitHub Actions expressions
- Use bash variables instead: `VAR="${{ needs.job.outputs.value }}"` then `echo "$VAR"`

### Checkout Failures
If checkout steps fail:
- Use `actions/checkout@v4` instead of specific SHAs
- Use `secrets.GITHUB_TOKEN` for checkout
- Use `secrets.FARM_PAT` for API calls

### Missing farm-manifest.json
The propagate and monitor workflows require `farm-manifest.json`:
- Ensure it exists in .git-private-farm
- Structure: `.repos` is an object with repo names as keys
- Extract repos with: `jq -r '.repos | keys[]' farm-manifest.json`

### Rate Limiting
- Workflows make many API calls
- Consider rate limiting with `sleep` between calls
- Use `FARM_PAT` with appropriate scopes

---

## Future Enhancements

1. **Auto-approval for Dependabot:** Automatically approve Dependabot PRs that pass checks
2. **PR Size Enforcement:** Block PRs that exceed size limits
3. **Auto-labeling:** Apply labels based on PR content
4. **Slack Notifications:** Notify when PRs are auto-merged
5. **Metrics Dashboard:** Track automation metrics over time

---

## Contacts

- **System Owner:** hyperpolymath
- **Repository:** hyperpolymath/gitbot-fleet
- **Documentation:** This file
- **Status:** Active & Operational

---

*Generated by Mistral Vibe. Co-Authored-By: Mistral Vibe <vibe@mistral.ai>*
