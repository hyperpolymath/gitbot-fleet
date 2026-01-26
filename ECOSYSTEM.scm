;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Project relationship mapping for gitbot-fleet

(ecosystem
  (version "1.0")
  (name "gitbot-fleet")
  (type "bot-orchestration")
  (purpose "Central orchestration for specialized quality-enforcement bots across the hyperpolymath ecosystem")

  (position-in-ecosystem
    (role "fleet-coordinator")
    (layer "orchestration")
    (description "Coordinates six specialized bots, manages shared context, and sequences their execution for comprehensive repository analysis"))

  (related-projects
    (upstream
      (git-dispatcher
        (relationship "parent-hub")
        (description "Central coordination hub for all Git automation - gitbot-fleet is a satellite")
        (integration "Receives coordination signals from dispatcher"))
      (hypatia
        (relationship "central-engine")
        (description "Neurosymbolic CI/CD intelligence platform - central rules engine and coordinator")
        (integration "Provides verified rulesets, coordinates fleet execution, manages learning pipeline")
        (tier "engine")))
    (downstream
      (robot-repo-automaton
        (relationship "action-executor")
        (description "Executes fixes identified by fleet bots")
        (integration "Sends action requests when bots identify fixable issues")
        (tier "executor")))
    (fleet-members
      (rhodibot
        (type "compliance-bot")
        (purpose "RSR structural compliance validation")
        (tech "Rust")
        (repo "hyperpolymath/rhodibot"))
      (echidnabot
        (type "verification-bot")
        (purpose "Formal mathematical and statistical verification")
        (tech "Rust")
        (repo "hyperpolymath/echidnabot"))
      (sustainabot
        (type "analysis-bot")
        (purpose "Ecological carbon intensity and economic efficiency analysis")
        (tech "Polyglot: Haskell, OCaml, ReScript, Rust")
        (repo "hyperpolymath/bots/git-eco-bot"))
      (glambot
        (type "presentation-bot")
        (purpose "Visual polish, accessibility WCAG, SEO validation")
        (tech "Rust")
        (repo "hyperpolymath/bots/glambot"))
      (seambot
        (type "architecture-bot")
        (purpose "Seam boundary hygiene and architectural integrity")
        (tech "Rust")
        (repo "hyperpolymath/bots/seambot"))
      (finishbot
        (type "release-bot")
        (purpose "Release readiness gating and placeholder validation")
        (tech "Rust")
        (repo "hyperpolymath/finishbot")))
    (sibling
      (git-hud
        (relationship "visualization-layer")
        (description "Dashboard for viewing fleet status and bot findings"))))

  (shared-context-layer
    (purpose "Enable cross-bot data sharing and coordinated analysis")
    (components
      (results-api "Bots publish findings to shared context")
      (prompt-sharing "Common prompts and templates for bot interactions")
      (sequencing "Coordinate execution order - e.g., seambot before finishbot")))

  (bot-modes
    (consultant "Reports findings without taking action")
    (advisor "Suggests fixes with explanations")
    (regulator "Enforces rules and blocks non-compliant changes"))

  (what-this-is
    "The coordination layer for all quality-enforcement bots"
    "A fleet of six specialized bots covering compliance, verification, sustainability, presentation, architecture, and release"
    "A shared context layer enabling cross-bot data sharing"
    "The orchestration point for comprehensive repository analysis"
    "A satellite of git-dispatcher in the larger automation ecosystem")

  (what-this-is-not
    "Not the rule engine - that is hypatia"
    "Not the action executor - that is robot-repo-automaton"
    "Not a single bot - it coordinates multiple specialized bots"
    "Not the central dispatcher - that is git-dispatcher"
    "Not a dashboard - that is git-hud")
  (opsm-integration
    (relationship "core")
    (description "execution fleet for OPSM batch operations.")
    (direction "opsm -> gitbot-fleet"))
)
