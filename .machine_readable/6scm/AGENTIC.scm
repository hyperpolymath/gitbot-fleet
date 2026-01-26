;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;; AGENTIC.scm - AI agent interaction patterns
;; Media-Type: application/agentic+scheme

(agentic
  (version "1.0")
  (project "gitbot-fleet")

  (agent-capabilities
    (can-read
      ("Bot configurations")
      ("Fleet status")
      ("Bot documentation"))
    (can-suggest
      ("New bot additions")
      ("Configuration changes")
      ("Architecture improvements"))
    (cannot
      ("Deploy bots directly")
      ("Modify webhook configurations")
      ("Access bot secrets")))

  (interaction-patterns
    (status-query
      (trigger "What is the fleet status?")
      (response "Read STATE.scm and summarize bot statuses"))
    (bot-info
      (trigger "Tell me about <bot-name>")
      (response "Read ECOSYSTEM.scm member-bots section"))
    (add-bot-guidance
      (trigger "How do I add a new bot?")
      (response "Read PLAYBOOK.scm adding-new-bot runbook")))

  (context-requirements
    (always-read
      ("README.adoc")
      (".machine_readable/6scm/STATE.scm")
      (".machine_readable/6scm/ECOSYSTEM.scm"))
    (read-on-demand
      (".machine_readable/6scm/META.scm")
      (".machine_readable/6scm/PLAYBOOK.scm")
      (".machine_readable/6scm/CICD-PATTERNS.scm")))

  (cicd-interaction-patterns
    (fix-workflow-security
      (trigger "Fix CI/CD issues in <repo>")
      (response "Read CICD-PATTERNS.scm, apply sha-pins and error-patterns"))
    (update-sha-pins
      (trigger "Update SHA pins for GitHub Actions")
      (response "Read CICD-PATTERNS.scm sha-pins section, apply to workflows"))
    (detect-codeql-issues
      (trigger "Check CodeQL configuration")
      (response "Use codeql-language-detection rules from CICD-PATTERNS.scm")))

  (safety-boundaries
    (require-human-approval
      ("Adding new bots to fleet")
      ("Changing bot permissions")
      ("Modifying orchestration logic"))
    (automated-allowed
      ("Reading fleet status")
      ("Generating documentation")
      ("Suggesting improvements"))))
