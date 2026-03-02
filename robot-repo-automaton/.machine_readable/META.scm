;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Project metadata and architectural decisions

(define project-meta
  `((version . "1.0.0")
    (architecture-decisions
      ((adr-001
        ((title . "Use S-expressions for error catalog format")
         (status . "accepted")
         (date . "2025-12-05")
         (context . "Need a machine-readable format for defining error types, detection methods, and fix patterns")
         (decision . "Use Guile Scheme S-expressions parsed by the lexpr crate")
         (rationale . "Consistent with ecosystem SCM files, human-readable, flexible structure, no schema needed")))
       (adr-002
        ((title . "Three-tier confidence system for fix gating")
         (status . "accepted")
         (date . "2026-02-08")
         (context . "Robot-repo-automaton must not blindly apply every fix. Some fixes are safe, others need review.")
         (decision . "Classify fixes as High/Medium/Low confidence. Auto-apply High, propose Medium/Low for review.")
         (rationale . "Prevents destructive automated changes. Per-repo overrides via .bot_directives/ allow repos to tune aggressiveness.")))
       (adr-003
        ((title . "Safety-first modify operations with rollback")
         (status . "accepted")
         (date . "2026-02-08")
         (context . "File modifications can break things. Need safety guarantees.")
         (decision . "Snapshot file content before modification. Reject binary files. Rollback on write failure.")
         (rationale . "Ensures no data loss from automated modifications. Binary file detection prevents corruption.")))
       (adr-004
        ((title . "Template-based file creation with variable expansion")
         (status . "accepted")
         (date . "2026-02-08")
         (context . "Creating missing standard files (LICENSE, SECURITY.md, .editorconfig) needs consistent content")
         (decision . "Embed templates via include_str! with variable expansion for REPO, OWNER, LICENSE, YEAR, AUTHOR, EMAIL")
         (rationale . "No external template directory dependency. Templates ship with the binary.")))))
    (development-practices
      ((code-style . "rustfmt + clippy strict")
       (security . "openssf-scorecard")
       (versioning . "semver")
       (documentation . "asciidoc")
       (branching . "trunk-based")
       (testing . "59 tests across unit and integration, tempfile-based isolation")
       (ci . "GitHub Actions with CodeQL, scorecard, SHA-pinned actions")))
    (design-rationale
      ((rust-choice . "Performance-critical CLI tool with async GitHub API calls and git operations")
       (fleet-integration . "Uses gitbot-shared-context crate for structured finding exchange between bots")
       (hypatia-bridge . "Scaffolded for future live API integration; currently uses placeholder rulesets")))))
