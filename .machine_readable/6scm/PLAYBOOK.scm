;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;; PLAYBOOK.scm - Operational runbook
;; Media-Type: application/playbook+scheme

(playbook
  (version "1.0")
  (project "gitbot-fleet")

  (runbooks
    (adding-new-bot
      (description "How to add a new bot to the fleet")
      (steps
        ("Create new bot repo with RSR 2026 structure")
        ("Implement bot in Rust with standard interfaces")
        ("Add bot entry to ECOSYSTEM.scm member-bots")
        ("Update README.adoc bot roster")
        ("Configure webhook in git-dispatcher")
        ("Add to fleet orchestration config")))

    (deploying-fleet-update
      (description "How to deploy fleet-wide changes")
      (steps
        ("Update shared configuration")
        ("Test with single bot first")
        ("Roll out to remaining bots")
        ("Verify via git-dispatcher dashboard")))

    (incident-response
      (description "How to handle bot misbehavior")
      (steps
        ("Identify affected bot via logs")
        ("Disable bot webhook in git-dispatcher")
        ("Investigate root cause")
        ("Fix and test in isolation")
        ("Re-enable with monitoring"))))

  (maintenance
    (daily ())
    (weekly
      ("Review bot action logs")
      ("Check for dependency updates"))
    (monthly
      ("Audit bot permissions")
      ("Review compliance metrics")))

  (escalation
    (level-1 "Check bot logs and restart if needed")
    (level-2 "Disable problematic bot, notify maintainer")
    (level-3 "Disable entire fleet, investigate systematically")))
