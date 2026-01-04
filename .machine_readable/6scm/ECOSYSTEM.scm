;; SPDX-License-Identifier: MPL-2.0
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;; ECOSYSTEM.scm - Position in the ecosystem
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "gitbot-fleet")
  (type "orchestration")
  (purpose "Central coordination for repository quality bots")

  (position-in-ecosystem
    (parent-project "git-dispatcher")
    (relationship "satellite")
    (role "bot-fleet-orchestration")
    (update-protocol
      (notify-parent #t)
      (sync-frequency "on-release")))

  (member-bots
    (rhodibot
      (repo "hyperpolymath/rhodibot")
      (purpose "RSR structural compliance")
      (status "active"))
    (echidnabot
      (repo "hyperpolymath/echidnabot")
      (purpose "Formal verification and fuzzing")
      (status "active"))
    (oikos
      (repo "hyperpolymath/oikos")
      (purpose "Ecological and economic standards")
      (status "active"))
    (glambot
      (repo "hyperpolymath/glambot")
      (purpose "Presentation quality")
      (status "complete"))
    (seambot
      (repo "hyperpolymath/seambot")
      (purpose "Integration testing")
      (status "active"))
    (finishing-bot
      (repo "hyperpolymath/finishing-bot")
      (purpose "Release readiness")
      (status "complete")))

  (related-projects
    (git-dispatcher
      (relationship "parent")
      (integration "webhook-routing"))
    (rhodium-standard-repositories
      (relationship "standard-source")
      (integration "compliance-rules"))
    (cicd-hyper-a
      (relationship "sibling")
      (integration "ci-automation")))

  (what-this-is
    ("Central coordination for the bot fleet")
    ("Documentation of bot purposes and interfaces")
    ("Shared configuration and context management")
    ("Fleet-wide orchestration logic"))

  (what-this-is-not
    ("Not the bot implementations themselves")
    ("Not a monorepo containing all bot code")
    ("Not a replacement for git-dispatcher")))
