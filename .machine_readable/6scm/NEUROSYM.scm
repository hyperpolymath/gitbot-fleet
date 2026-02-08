;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;; NEUROSYM.scm - Neurosymbolic integration config
;; Media-Type: application/neurosym+scheme

(neurosym
  (version "1.0")
  (project "gitbot-fleet")

  (symbolic-layer
    (knowledge-representation
      (format "scheme-sexp")
      (schemas
        ("bot-definition" (fields name purpose status repo))
        ("fleet-action" (fields bot target action timestamp))
        ("compliance-result" (fields repo bot passed issues))))
    (reasoning-rules
      (bot-selection
        "Select bot based on action type and repo characteristics")
      (conflict-resolution
        "Prevent multiple bots from conflicting actions on same file")
      (priority-ordering
        "rhodibot > glambot > finishbot for structural changes")))

  (neural-layer
    (embeddings
      (model "code-embedding")
      (use-cases
        ("Similarity search for bot applicability")
        ("Issue classification")))
    (generation
      (model "code-gen")
      (use-cases
        ("Suggesting fixes")
        ("Generating documentation"))))

  (integration-points
    (symbolic-to-neural
      ("Bot selection rules inform embedding queries")
      ("Compliance schemas structure generation prompts"))
    (neural-to-symbolic
      ("Generated suggestions validated against rules")
      ("Embeddings inform priority ordering")))

  (verification
    (formal-methods
      ("Bot action sequences verified for conflicts")
      ("State transitions validated"))
    (testing
      ("Property-based testing for rule consistency")
      ("Fuzzing for edge cases"))))
