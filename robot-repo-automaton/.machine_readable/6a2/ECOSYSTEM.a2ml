;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Project relationship mapping for robot-repo-automaton

(ecosystem
  (version "1.0")
  (name "robot-repo-automaton")
  (type "automation-executor")
  (purpose "Tier 3 Executor - applies automated repository fixes with confidence-based gating")

  (position-in-ecosystem
    (role "tier-3-executor")
    (layer "automation")
    (description "The only bot that modifies code. Takes findings from Tier 1 Verifiers and Tier 2 Finishers, classifies fix confidence, and applies or proposes fixes."))

  (related-projects
    (upstream
      (hypatia
        (relationship "rule-provider")
        (description "Neurosymbolic CI/CD intelligence platform providing verified rulesets")
        (integration "Fetches Rule and Ruleset types, executes rules, reports outcomes to learning pipeline")
        (tier "engine")
        (api-types "Rule" "Ruleset" "RulePattern" "RuleFix"))
      (gitbot-fleet
        (relationship "coordinator")
        (description "Orchestrates specialized bots whose findings trigger automaton actions")
        (integration "Uses gitbot-shared-context for finding publication and fix outcome reporting")
        (tier "coordinator")))
    (downstream
      (target-repositories
        (relationship "managed-targets")
        (description "All repositories in hyperpolymath ecosystem that receive automated fixes")))
    (sibling
      (gitvisor
        (relationship "sibling-tool")
        (description "Unified dashboard for visualizing automation status and repository health"))
      (rhodium-standard-repositories
        (relationship "standards-source")
        (description "Defines RSR compliance rules that automaton enforces"))
      (panic-attacker
        (relationship "sibling-scanner")
        (description "Security vulnerability scanner whose findings can trigger automaton fixes"))
      (verisimdb
        (relationship "data-store")
        (description "Stores scan results and fix outcomes for cross-repo analysis"))))

  (what-this-is
    "Tier 3 Executor in the gitbot-fleet hierarchy"
    "The only bot that actually modifies repository code"
    "Applies four fix types: delete, modify, create, disable"
    "Uses confidence thresholds to gate fix application"
    "Reports fix outcomes to hypatia's neurosymbolic learning loop"
    "Manages preventive git hooks (pre-commit, pre-push, commit-msg)"
    "Parses ERROR-CATALOG.scm for known fix patterns")

  (what-this-is-not
    "Not the rule engine - that is hypatia"
    "Not a bot coordinator - that is gitbot-fleet"
    "Not a dashboard - that is gitvisor"
    "Not a scanner - scanning is done by detectors and panic-attacker"
    "Not a standards document - that is rhodium-standard-repositories"))
