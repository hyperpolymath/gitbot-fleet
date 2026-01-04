;; SPDX-License-Identifier: MPL-2.0
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
      (".machine_readable/6scm/PLAYBOOK.scm")))

  (safety-boundaries
    (require-human-approval
      ("Adding new bots to fleet")
      ("Changing bot permissions")
      ("Modifying orchestration logic"))
    (automated-allowed
      ("Reading fleet status")
      ("Generating documentation")
      ("Suggesting improvements"))))
