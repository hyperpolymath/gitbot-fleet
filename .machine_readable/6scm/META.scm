;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 hyperpolymath
;; META.scm - Architecture decisions and development practices
;; Media-Type: application/meta+scheme

(meta
  (version "1.0")
  (project "gitbot-fleet")

  (architecture-decisions
    (adr-001
      (status "accepted")
      (date "2026-01-04")
      (title "Separate bot repositories with fleet coordination")
      (context "Bots have different release cycles and maintainers")
      (decision "Keep bots in separate repos, use gitbot-fleet for orchestration")
      (consequences
        ("Each bot can evolve independently")
        ("Fleet repo provides coordination layer")
        ("Shared types via crate dependencies")))

    (adr-002
      (status "accepted")
      (date "2026-01-04")
      (title "Rust as primary implementation language")
      (context "Need performance, safety, and WASM support")
      (decision "All bots implemented in Rust")
      (consequences
        ("Type safety across fleet")
        ("Can compile to WASM for browser")
        ("Shared crates possible")))

    (adr-003
      (status "accepted")
      (date "2026-01-04")
      (title "git-dispatcher as parent coordinator")
      (context "Need central point for Git automation")
      (decision "gitbot-fleet is satellite of git-dispatcher")
      (consequences
        ("Clear hierarchy")
        ("Webhook routing via dispatcher")
        ("Consistent automation patterns"))))

  (development-practices
    (code-style
      (language "Rust")
      (formatter "rustfmt")
      (linter "clippy")
      (edition "2021"))

    (security
      (spdx-headers "required")
      (crypto-standard "CRYPTO-STANDARD.scm")
      (dependency-pinning "SHA-pinned"))

    (testing
      (unit-tests "required")
      (integration-tests "recommended")
      (fuzzing "ClusterFuzzLite"))

    (versioning
      (scheme "semver")
      (changelog "CHANGELOG.adoc"))

    (documentation
      (format "AsciiDoc")
      (api-docs "rustdoc"))

    (branching
      (strategy "trunk-based")
      (main-branch "main")))

  (design-rationale
    (why-separate-repos
      "Different bots have different release cadences and domain expertise requirements")
    (why-rust
      "Performance-critical automation needs predictable latency and memory safety")
    (why-fleet-coordination
      "Bots need to share context and avoid conflicting actions on same repo")))
