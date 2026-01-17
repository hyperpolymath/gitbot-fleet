;; SPDX-License-Identifier: PMPL-1.0
;; STATE.scm - Project state for gitbot-fleet

(state
  (metadata
    (version "0.1.0")
    (schema-version "1.0")
    (created "2024-06-01")
    (updated "2025-01-17")
    (project "gitbot-fleet")
    (repo "hyperpolymath/gitbot-fleet"))

  (project-context
    (name "Gitbot Fleet")
    (tagline "Bot fleet for repository quality enforcement across the ecosystem")
    (tech-stack ("rescript" "deno")))

  (current-position
    (phase "alpha")
    (overall-completion 20)
    (working-features
      ("Fleet architecture"
       "Bot coordination design"))))
