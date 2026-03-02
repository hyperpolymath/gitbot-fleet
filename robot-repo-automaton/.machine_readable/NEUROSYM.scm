;; SPDX-License-Identifier: PMPL-1.0-or-later
;; NEUROSYM.scm - Neurosymbolic integration config for robot-repo-automaton

(define neurosym-config
  `((version . "1.0.0")
    (project . "robot-repo-automaton")

    (symbolic-layer
      ((type . "pattern-matching-executor")
       (reasoning . "rule-based-automation")
       (verification . "rust-type-system")
       (guarantees
         ("Fix actions are deterministic"
          "Git operations are atomic"
          "Error patterns match precisely"
          "Execution is idempotent"))))

    (neural-layer
      ((llm-guidance
         (model . "claude-sonnet-4-5-20250929")
         (use-cases
           ("Suggest new fix patterns from error logs"
            "Generate commit messages for fixes"
            "Explain fix rationale in PR descriptions"
            "Prioritize which errors to fix first"))
         (constraints
           ("Never execute fixes without symbolic verification"
            "Always explain reasoning for suggested fixes"
            "Must respect repository-specific constraints")))))

    (integration
      ((executor-pattern
         "Symbolic rules define safe fixes -> LLM explains context -> Git commits preserve history")

       (feedback-loop
         "Bot execution results + User PR reviews -> Pattern refinement -> Catalog updates")))))
