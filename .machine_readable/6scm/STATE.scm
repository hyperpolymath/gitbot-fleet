;; SPDX-License-Identifier: MPL-2.0
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;; STATE.scm - Current project state
;; Media-Type: application/state+scheme

(state
  (metadata
    (version "1.0")
    (schema-version "RSR-2026")
    (created "2026-01-04")
    (updated "2026-01-04")
    (project "gitbot-fleet")
    (repo "https://github.com/hyperpolymath/gitbot-fleet"))

  (project-context
    (name "Gitbot Fleet")
    (tagline "Bot fleet for repository quality enforcement")
    (tech-stack "Rust" "Guile Scheme"))

  (current-position
    (phase "initial-setup")
    (overall-completion 10)
    (components
      (rhodibot (status "active") (location "separate-repo"))
      (echidnabot (status "active") (location "separate-repo"))
      (oikos (status "active") (location "separate-repo"))
      (glambot (status "complete") (location "separate-repo"))
      (seambot (status "active") (location "separate-repo"))
      (finishing-bot (status "complete") (location "separate-repo")))
    (working-features
      ("Bot coordination documentation")
      ("RSR 2026 structure")))

  (route-to-mvp
    (milestones
      (milestone-1 "Fleet Orchestration"
        (items
          ("Implement shared context layer")
          ("Create bot communication protocol")
          ("Add fleet-wide configuration")))
      (milestone-2 "Integration"
        (items
          ("Connect to git-dispatcher")
          ("Implement webhook handlers")
          ("Add GitHub App configuration")))))

  (blockers-and-issues
    (critical ())
    (high-priority ())
    (medium-priority
      ("Individual bot repos need consolidation planning"))
    (low-priority ()))

  (critical-next-actions
    (immediate
      ("Document bot interfaces")
      ("Create shared types"))
    (this-week
      ("Design orchestration layer"))
    (this-month
      ("Implement fleet coordination")))

  (session-history
    (snapshot "2026-01-04"
      (accomplishments
        ("Repository created")
        ("RSR 2026 structure established")
        ("Documentation created")))))
