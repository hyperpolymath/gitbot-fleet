;; SPDX-License-Identifier: MPL-2.0
;; SPDX-FileCopyrightText: 2025-2026 hyperpolymath
;; CICD-PATTERNS.scm - CI/CD error patterns and fixes for bot learning
;; Media-Type: application/cicd-patterns+scheme
;; Generated: 2026-01-05 from cicd-hyper-a audit session

(cicd-patterns
  (version "1.0")
  (project "gitbot-fleet")
  (reference-repo "cicd-hyper-a")

  ;; SHA pins for commonly used GitHub Actions
  ;; Bots should use these when SHA-pinning actions
  (sha-pins
    (actions/checkout
      "8e8c483db84b4bee98b60c0593521ed34d9990e8"
      "v6.0.1")
    (actions/upload-artifact
      "b7c566a772e6b6bfb58ed0dc250532a479d7789f"
      "v6.0.0")
    (actions/configure-pages
      "983d7736d9b0ae728b81ab479565c72886d7745b"
      "v5")
    (actions/upload-pages-artifact
      "56afc609e74202658d3ffba0e8f6dda462b719fa"
      "v3")
    (actions/deploy-pages
      "d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e"
      "v4")
    (actions/cache
      "0c45773b623bea8c8e75f6c82b208c3cf94ea4f9"
      "v4")
    (actions/jekyll-build-pages
      "44a6e6beabd48582f863aeeb6cb2151cc1716697"
      "v1")
    (github/codeql-action/init
      "1b168cd39490f61582a9beae412bb7057a6b2c4e"
      "v3.28.1")
    (github/codeql-action/analyze
      "1b168cd39490f61582a9beae412bb7057a6b2c4e"
      "v3.28.1")
    (denoland/setup-deno
      "11b63cf76cfcafb4e43f97b6cad24d8e8438f62d"
      "v1")
    (dtolnay/rust-toolchain
      "6d9817901c499d6b02debbb57edb38d33daa680b"
      "stable")
    (Swatinem/rust-cache
      "779680da715d629ac1d338a641029a2f4372abb5"
      "v2")
    (erlef/setup-beam
      "5304e04ea2b355f03681464e683d92e3b2f18451"
      "v1.18.2")
    (ossf/scorecard-action
      "62b2cac7ed8198b15735ed49ab1e5cf35480ba46"
      "v2.4.0")
    (trufflesecurity/trufflehog
      "ef6e76c3c4023279497fab4721ffa071a722fd05"
      "v3.92.4")
    (editorconfig-checker/action-editorconfig-checker
      "9f8f6065f4db902c0c56cafa67cea18b3ebbb680"
      "main"))

  ;; Error patterns and their fixes
  (error-patterns
    ;; Workflow Security Errors
    (unpinned-actions
      (id "workflow-security-001")
      (severity "high")
      (pattern "uses: .+@v[0-9]+")
      (description "GitHub Action using version tag instead of SHA pin")
      (fix "Replace @vN with @SHA # vN format")
      (auto-fixable #t))

    (missing-permissions
      (id "workflow-security-002")
      (severity "high")
      (pattern "^name:.*\\n(?!.*permissions:)")
      (description "Workflow missing explicit permissions declaration")
      (fix "Add 'permissions: read-all' after 'on:' block")
      (auto-fixable #t))

    (missing-spdx-header
      (id "workflow-security-003")
      (severity "low")
      (pattern "^name:")
      (description "Workflow file missing SPDX license header")
      (fix "Add '# SPDX-License-Identifier: AGPL-3.0-or-later' as first line")
      (auto-fixable #t))

    (workflow-linter-self-detection
      (id "workflow-security-004")
      (severity "low")
      (pattern "grep -rn \"uses:\".*grep -v")
      (description "Workflow linter grep matches its own comments")
      (fix "Add 'grep -v \"^\\s*#\"' and 'grep -v \"# Pattern:\\|# Find any\"' filters")
      (auto-fixable #t))

    (missing-toolchain-input
      (id "workflow-security-005")
      (severity "medium")
      (pattern "dtolnay/rust-toolchain@[a-f0-9]+\\s*$")
      (description "dtolnay/rust-toolchain missing required 'toolchain' input")
      (fix "Add 'with: toolchain: stable' (or nightly/beta)")
      (auto-fixable #t))

    ;; CodeQL Errors
    (codeql-language-mismatch
      (id "codeql-001")
      (severity "medium")
      (pattern "language:.*javascript.*typescript")
      (description "CodeQL configured for languages not present in repo")
      (fix "Detect actual languages, use 'actions' for workflow-only repos")
      (auto-fixable #t))

    (codeql-outdated-sha
      (id "codeql-002")
      (severity "medium")
      (pattern "github/codeql-action.*@662472033e021d55d94146f66f6058822b0b39fd")
      (description "CodeQL action using outdated SHA")
      (fix "Update to @1b168cd39490f61582a9beae412bb7057a6b2c4e # v3.28.1")
      (auto-fixable #t)))

  ;; CodeQL language detection rules
  (codeql-language-detection
    (rules
      (no-source-code
        (condition "No .js/.ts/.py/.go/.rs files in src/ or lib/")
        (recommended-language "actions")
        (note "Scans workflow files only"))
      (javascript-typescript
        (condition ".js/.ts/.jsx/.tsx files in src/ or lib/")
        (recommended-language "javascript-typescript"))
      (rust-only
        (condition "Only .rs files, no other languages")
        (recommended-language "actions")
        (note "CodeQL rust support is limited, use 'actions' instead"))
      (python
        (condition ".py files in src/ or lib/")
        (recommended-language "python"))
      (go
        (condition ".go files in src/ cmd/ or pkg/")
        (recommended-language "go"))))

  ;; Bot capabilities for CI/CD fixing
  (bot-capabilities
    (rhodibot
      (can-fix
        ("unpinned-actions")
        ("missing-permissions")
        ("missing-spdx-header"))
      (detection-method "grep-based scanning"))
    (echidnabot
      (can-fix
        ("codeql-language-mismatch")
        ("codeql-outdated-sha"))
      (detection-method "yaml parsing"))
    (seambot
      (can-propagate
        ("workflow templates")
        ("sha-pins updates"))
      (propagation-scope "fleet-wide")))

  ;; Quick reference for common fixes
  (quick-reference
    (add-permissions
      "Add after 'on:' block:"
      "permissions: read-all")
    (sha-pin-format
      "Format: uses: action/name@SHA # version")
    (spdx-header
      "First line: # SPDX-License-Identifier: AGPL-3.0-or-later")
    (rust-toolchain-fix
      "uses: dtolnay/rust-toolchain@SHA\\n        with:\\n          toolchain: stable"))

  ;; CLI Tool Reference
  (cli-tool
    (name "cicd-fixer")
    (location "cicd-hyper-a/target/release/cicd-fixer")
    (commands
      (scan
        (usage "cicd-fixer scan <repo-path>")
        (description "Scan a repository for CI/CD issues"))
      (fix
        (usage "cicd-fixer fix <repo-path> [--dry-run]")
        (description "Fix auto-fixable issues"))
      (batch
        (usage "cicd-fixer batch <repos-dir> [--fix] [--dry-run] [--limit N]")
        (description "Process multiple repositories"))
      (pins
        (usage "cicd-fixer pins")
        (description "List known SHA pins"))
      (catalog
        (usage "cicd-fixer catalog")
        (description "List error patterns")))))
