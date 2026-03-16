;; SPDX-License-Identifier: PMPL-1.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for robot-repo-automaton

(define agentic-config
  `((version . "1.0.0")
    (project . "robot-repo-automaton")

    (patterns
      ((automated-fixing
         (focus . ("idempotency" "safety" "correctness"))
         (check-for
           ("Verify fix doesn't break tests"
            "Ensure fix is idempotent (running twice = same result)"
            "Check for unintended side effects"
            "Validate commit message follows convention")))

       (error-catalog-maintenance
         (workflow
           ("1. Bots report errors they can't fix"
            "2. Analyze patterns in unfixable errors"
            "3. Add new patterns to ERROR-CATALOG.scm"
            "4. Implement fix actions in Rust"))
         (constraints
           ("Never auto-fix without confidence threshold"
            "Always dry-run before live execution"
            "Require human approval for destructive fixes")))))

    (constraints
      ((languages
         (primary . "rust")
         (config . "toml")
         (catalog . "guile-scheme"))

       (banned . ("typescript" "node" "python" "go"))

       (safety-rules
         ("Fixes must be reversible"
          "Never delete user data without backup"
          "Always create git commits (not direct file edits)"
          "Respect .gitignore patterns"))))))
