;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational runbook for robot-repo-automaton

(define playbook
  `((version . "1.0.0")
    (project . "robot-repo-automaton")

    (procedures
      ((execute-fix
         (steps
           ("1. Load ERROR-CATALOG.scm"
            "2. Match error pattern to fix action"
            "3. Apply fix with git operations"
            "4. Create commit with standardized message"
            "5. Open PR or push directly (based on config)"))
         (troubleshooting
           ((issue . "Fix action failed")
            (solution . "Check git status, verify permissions, review error log"))))

       (register-with-hypatia
         (steps
           ("1. Configure registry URL in config.toml"
            "2. Fetch ruleset index from hypatia"
            "3. Cache rulesets locally"
            "4. Report execution results back to hypatia")))

       (add-new-fix-pattern
         (steps
           ("1. Add error pattern to ERROR-CATALOG.scm"
            "2. Define fix action in Rust"
            "3. Add test case"
            "4. Update documentation"
            "5. Test on sample repo before fleet deployment")))))

    (alerts
      ((high-priority
         (trigger . "Fix creates breaking change")
         (response
           ("1. Revert commit immediately"
            "2. Analyze why detection failed"
            "3. Add test case to prevent recurrence"))
         (escalation . "Disable auto-fix until root cause resolved"))))))
