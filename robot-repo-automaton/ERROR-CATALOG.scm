;; SPDX-License-Identifier: MPL-2.0
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ERROR-CATALOG.scm - Machine-readable catalog of common repo security errors
;; Format: Guile Scheme (homoiconic, parseable by robot-repo-cleaner)
;; Updated: 2026-07-29

(define error-catalog
  '((metadata
     (format-version . "1.0")
     (schema-version . "2026-07-29")
     (purpose . "Learning rules for propagating fixes across repositories")
     (generator . "robot-repo-bot/Claude analysis"))

    ;;=========================================================================
    ;; ERROR CATEGORY: DUPLICATE WORKFLOWS
    ;;=========================================================================
    (error-type
     (id . "ERR-WF-001")
     (name . "duplicate-codeql-workflows")
     (severity . "high")
     (category . "workflow-duplication")
     (description . "Repository has both codeql.yml AND codeql-analysis.yml")

     (detection
      (method . "file-existence")
      (files . (".github/workflows/codeql.yml" ".github/workflows/codeql-analysis.yml"))
      (condition . "both-exist"))

     (affected-repos
      ("ada-loom-registry" "affinescript" "asdfghj" "blue-screen-of-app"
       "checky-monkey" "conative-gating" "disinfo-nsai-detector" "esn"
       "fslint-plugin-api" "fslint-plugin-sdk" "git-eco-bot" "grimrepo-scripts"
       "hackenbush-ssg" "idaptik" "indieweb2-bastion" "kith" "llm-unify-core"
       "llm-verify" "lol" "lsm" "modshells" "php-aegis" "poly-container-mcp"
       "poly-db-mcp" "polyglot-i18n" "poly-iac-mcp" "poly-observability-mcp"
       "poly-queue-mcp" "poly-secret-mcp" "poly-ssg-mcp" "proof-of-work"
       "rhodium-standard-repositories-fix" "robot-repo-bot" "sanctify-php"
       "scaffoldia" "supernorma" "synapse-release" "thejeffparadox"
       "tree-navigator" "ubicity" "union-policy-parsers" "wordpress-wharf"
       "wp-audit-toolkit"))

     (fix
      (action . "delete")
      (target . ".github/workflows/codeql-analysis.yml")
      (reason . "codeql.yml is the canonical file with build-mode support"))

     (commit-message . "chore: remove duplicate codeql-analysis.yml workflow"))

    ;;=========================================================================
    ;; ERROR CATEGORY: CODEQL LANGUAGE MISMATCH
    ;;=========================================================================
    (error-type
     (id . "ERR-WF-002")
     (name . "codeql-language-mismatch")
     (severity . "medium")
     (category . "workflow-misconfiguration")
     (description . "CodeQL configured to scan languages not present in repository")

     (detection
      (method . "language-detection")
      (check . "compare workflow matrix.language against repo file extensions")
      (extension-map
       ((".js" ".jsx" ".ts" ".tsx" ".mjs") . "javascript-typescript")
       ((".py") . "python")
       ((".go") . "go")
       ((".java" ".kt") . "java-kotlin")
       ((".rb") . "ruby")
       ((".rs") . "rust")
       ((".cs") . "csharp")
       ((".cpp" ".c" ".h" ".hpp") . "cpp")
       ((".swift") . "swift")))

     (examples
      ((repo . "robot-repo-bot")
       (configured . ("javascript" "python" "go" "java" "ruby"))
       (actual . ())
       (issue . "No source code files exist"))
      ((repo . "bunsenite")
       (configured . ("go" "python" "javascript-typescript"))
       (actual . ("rust"))
       (issue . "Only Rust exists, should only scan rust")))

     (fix
      (action . "modify")
      (target . ".github/workflows/codeql.yml")
      (modification . "update matrix.language to match detected languages")
      (fallback . "disable workflow if no scannable languages"))

     (commit-message . "fix: align CodeQL language matrix with repo contents"))

    ;;=========================================================================
    ;; ERROR CATEGORY: BROKEN COMPREHENSIVE-QUALITY
    ;;=========================================================================
    (error-type
     (id . "ERR-WF-003")
     (name . "comprehensive-quality-broken")
     (severity . "medium")
     (category . "workflow-misconfiguration")
     (description . "comprehensive-quality.yml references services/tools not configured")

     (detection
      (method . "file-existence")
      (files . (".github/workflows/comprehensive-quality.yml")))

     (affected-repos
      ("ada-loom-registry" "asdfghj" "blue-screen-of-app" "bunsenite"
       "checky-monkey" "conative-gating" "czech-file-knife" "disinfo-nsai-detector"
       "esn" "fslint-plugin-api" "fslint-plugin-sdk" "git-eco-bot" "grimrepo-scripts"
       "hackenbush-ssg" "idaptik" "indieweb2-bastion" "januskey" "kith"
       "llm-unify-core" "llm-verify" "lol" "lsm" "modshells" "php-aegis"
       "poly-container-mcp" "poly-db-mcp" "polyglot-i18n" "poly-iac-mcp"
       "poly-observability-mcp" "poly-queue-mcp" "poly-secret-mcp" "poly-ssg-mcp"
       "proof-of-work" "rhodium-standard-repositories" "rhodium-standard-repositories-fix"
       "robot-repo-bot" "robot-vacuum-cleaner" "sanctify-php" "scaffoldia"
       "supernorma" "synapse-release" "thejeffparadox" "tree-navigator" "ubicity"
       "union-policy-parsers" "wordpress-wharf" "wp-audit-toolkit"))

     (fix
      (action . "delete")
      (target . ".github/workflows/comprehensive-quality.yml")
      (reason . "Consistently fails due to missing configuration"))

     (commit-message . "chore: remove broken comprehensive-quality.yml workflow"))

    ;;=========================================================================
    ;; ERROR CATEGORY: MIRROR WITHOUT SECRETS
    ;;=========================================================================
    (error-type
     (id . "ERR-WF-004")
     (name . "mirror-missing-secrets")
     (severity . "low")
     (category . "workflow-secrets")
     (description . "mirror.yml references secrets not configured at org/repo level")

     (detection
      (method . "secret-reference")
      (required-secrets . ("GITLAB_TOKEN" "BITBUCKET_TOKEN"))
      (required-vars . ("GITLAB_MIRROR_ENABLED" "BITBUCKET_MIRROR_ENABLED"))
      (note . "Workflow has conditionals but still shows as failed in UI"))

     (affected-repos . 59)  ; Count from scan

     (fix-options
      ((option . "configure-secrets")
       (description . "Set up org-level secrets for mirroring")
       (steps
        ("GitHub Settings > Secrets > Actions > Organization secrets"
         "Add GITLAB_TOKEN with GitLab PAT"
         "Add BITBUCKET_TOKEN with Bitbucket App Password"
         "Set vars: GITLAB_MIRROR_ENABLED=true, BITBUCKET_MIRROR_ENABLED=true")))

      ((option . "remove-workflow")
       (description . "Delete mirror.yml if mirroring not needed")
       (action . "delete")
       (target . ".github/workflows/mirror.yml"))))

    ;;=========================================================================
    ;; ERROR CATEGORY: DATADOG WITHOUT CONFIG
    ;;=========================================================================
    (error-type
     (id . "ERR-WF-005")
     (name . "datadog-without-config")
     (severity . "medium")
     (category . "workflow-secrets")
     (description . "datadog-synthetics.yml runs without DATADOG_API_KEY/APP_KEY")

     (detection
      (method . "file-existence")
      (files . (".github/workflows/datadog-synthetics.yml"))
      (required-secrets . ("DATADOG_API_KEY" "DATADOG_APP_KEY")))

     (affected-repos . ("llm-verify"))

     (fix
      (action . "delete")
      (target . ".github/workflows/datadog-synthetics.yml")
      (reason . "Datadog not configured for these repos"))

     (commit-message . "chore: remove unconfigured Datadog workflow"))

    ;;=========================================================================
    ;; ERROR CATEGORY: EXCESSIVE WORKFLOWS
    ;;=========================================================================
    (error-type
     (id . "ERR-WF-006")
     (name . "excessive-workflow-count")
     (severity . "note")
     (category . "workflow-hygiene")
     (description . "Repository has 15+ workflows, many likely inapplicable")

     (detection
      (method . "file-count")
      (threshold . 15)
      (path . ".github/workflows/"))

     (examples
      ((repo . "llm-verify")
       (count . 33)
       (note . "Many language-specific workflows for unused languages")))

     (recommendation . "Audit workflows and remove those not applicable to repo"))

    ;;=========================================================================
    ;; ERROR CATEGORY: MISSING SHA PINS (Remediated 2025-12-13)
    ;;=========================================================================
    (error-type
     (id . "ERR-SEC-001")
     (name . "unpinned-actions")
     (severity . "high")
     (category . "security")
     (description . "GitHub Actions using version tags instead of SHA pins")
     (status . "fixed")

     (detection
      (method . "regex")
      (pattern . "uses:\\s+[\\w-]+/[\\w-]+@v[0-9]")
      (negative-pattern . "uses:\\s+[\\w-]+/[\\w-]+@[a-f0-9]{40}"))

     (fix
      (action . "modify")
      (modification . "Replace version tags with full SHA hashes")
      (example
       (before . "uses: actions/checkout@v4")
       (after . "uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4.1.1")))

     (openssf-scorecard-check . "Pinned-Dependencies"))

    ;;=========================================================================
    ;; ERROR CATEGORY: MISSING PERMISSIONS
    ;;=========================================================================
    (error-type
     (id . "ERR-SEC-002")
     (name . "missing-permissions")
     (severity . "high")
     (category . "security")
     (description . "Workflow missing top-level permissions declaration")
     (status . "fixed")

     (detection
      (method . "yaml-parse")
      (check . "workflow lacks 'permissions:' at top level"))

     (fix
      (action . "modify")
      (modification . "Add 'permissions: read-all' at workflow level")
      (example . "permissions: read-all"))

     (openssf-scorecard-check . "Token-Permissions"))

    ;;=========================================================================
    ;; ERROR CATEGORY: MISSING SPDX HEADER
    ;;=========================================================================
    (error-type
     (id . "ERR-LIC-001")
     (name . "missing-spdx-header")
     (severity . "low")
     (category . "licensing")
     (description . "Workflow file missing SPDX license identifier")
     (status . "fixed")

     (detection
      (method . "regex")
      (pattern . "^# SPDX-License-Identifier:")
      (file-types . (".yml" ".yaml" ".jl" ".rs" ".ex" ".exs")))

     (fix
      (action . "modify")
      (modification . "Add SPDX header as first line")
      (template . "# SPDX-License-Identifier: MPL-2.0")))

    ;;=========================================================================
    ;; PROPAGATION RULES
    ;;=========================================================================
    (propagation
     (methods
      ((method . "robot-repo-cleaner")
       (description . "Julia script that applies fixes across all repos")
       (script . "~/fix-github-workflows.sh"))

      ((method . "pre-commit-hook")
       (description . "Validate workflows before commit")
       (location . ".github/hooks/validate-workflows.yml"))

      ((method . "ci-check")
       (description . "GitHub Action that validates workflow hygiene")
       (workflow . ".github/workflows/workflow-linter.yml"))

      ((method . "claude-md-rules")
       (description . "Rules embedded in CLAUDE.md for AI assistants")
       (location . ".claude/CLAUDE.md")))

     (priority-order
      ("ERR-SEC-001" "ERR-SEC-002" "ERR-WF-001" "ERR-WF-002"
       "ERR-WF-003" "ERR-WF-005" "ERR-WF-004" "ERR-LIC-001")))

    ;;=========================================================================
    ;; STATISTICS (as of 2025-12-15)
    ;;=========================================================================
    (statistics
     (total-repos-scanned . 85)
     (repos-with-workflows . 72)
     (issues-found
      (duplicate-codeql . 43)
      (comprehensive-quality . 47)
      (mirror-yml . 59)
      (codeql-language-mismatch . "~40 estimated"))
     (fixed-previously
      (sha-pins . "~114 workflows")
      (permissions . "~114 workflows")
      (spdx-headers . "~114 workflows")))))

    ;;=========================================================================
    ;; ERROR CATEGORY: UNSATISFIABLE MERGE GATES
    ;; Added 2026-07-29. Every entry below was MEASURED across all 418 active
    ;; estate repos, not inferred. They share one shape: the board reads green
    ;; or blocked for a reason no workflow edit can address, because the check
    ;; never ran at all.
    ;;=========================================================================
    (error-type
     (id . "ERR-GATE-001")
     (name . "phantom-required-context")
     (severity . "critical")
     (category . "unsatisfiable-gate")
     (description . "A required status-check context that no job ever emits. Every PR sits permanently BLOCKED, and because an absent check produces NO check-run, `gh pr checks` shows nothing wrong.")

     (detection
      (method . "set-difference")
      (sources . ("classic branch protection" "ACTIVE branch-target RULESETS"))
      (sample . "PR HEAD SHAs, not the default branch")
      (condition . "required-context absent from observed check-runs AND commit statuses"))

     (measured
      (date . "2026-07-29")
      (ruleset-context-rows . 236)
      (note . "Rulesets hold most estate requirements; classic protection alone sees almost none."))

     (variants
      ("invented name -- hypatia-scan, emitted by nothing"
       "case mismatch -- required `codeql`, real job is `CodeQL`"
       "filename as context -- required `.github/dependabot.yml`"))

     (remediation
      (action . "remove-context-or-rename-to-match-a-real-job")
      (api . "DELETE /repos/{o}/{r}/branches/{b}/protection/required_status_checks/contexts")
      (warning . "Use that surgical sub-resource. A full PUT .../protection REPLACES the object and silently drops required_signatures / enforce_admins.")
      (warning-2 . "In a ruleset, emptying required_status_checks returns HTTP 422 -- the whole rule must be dropped. A ruleset PUT also replaces, so rebuild from a live GET and preserve bypass_actors.")))

    (error-type
     (id . "ERR-GATE-002")
     (name . "required-context-never-runs-on-pull-request")
     (severity . "critical")
     (category . "unsatisfiable-gate")
     (description . "The producing job exists and is healthy, but is push-, schedule- or dynamic-triggered and never runs on pull_request. It therefore cannot satisfy a PR gate however green it looks on main.")

     (detection
      (method . "compare-two-surfaces")
      (condition . "context PRESENT on default-branch HEAD but ABSENT from every sampled PR head"))

     (measured
      (date . "2026-07-29")
      (canonical-case . "Dependabot")
      (emitter . "GitHub's managed dynamic/dependabot/dependabot-updates runner")
      (check-runs-on-main . 10)
      (check-runs-on-pr-head . 0)
      (repos-affected . 25)
      (note . "Sampling main scores this HEALTHY -- a false negative. It is the likely root of the estate's --admin merge drift."))

     (remediation
      (action . "add-pull_request-trigger-or-stop-requiring-on-prs")))

    (error-type
     (id . "ERR-GATE-003")
     (name . "dead-action-pin")
     (severity . "critical")
     (category . "unsatisfiable-gate")
     (description . "A `uses:` ref pointing at a DELETED action repository. Actions resolves refs at RUN time, and an unresolvable ref produces NO check run -- not a red one. The repo can be fully green with its scanning entirely absent.")

     (detection
      (method . "resolve-every-uses-ref-against-the-api")
      (condition . "repository 404, or SHA not found"))

     (measured
      (date . "2026-07-29")
      (repos . 69)
      (references . 135)
      (dead . ("hyperpolymath/a2ml-validate-action" "hyperpolymath/k9-validate-action"))
      (live . ("hyperpolymath/a2ml-ecosystem/validate-action" "hyperpolymath/k9-ecosystem/validate-action")))

     (remediation
      (action . "REPOINT-not-vendor")
      (rationale . "Vendoring the script into each consumer creates one drifting copy per repo -- this estate's most recurring failure mode. Repointing is one line per file.")
      (warning . "Actions does NOT follow repository renames in `uses:`; a rename is a genuine break.")))

    (error-type
     (id . "ERR-GATE-004")
     (name . "actions-policy-refuses-every-reusable")
     (severity . "critical")
     (category . "unsatisfiable-gate")
     (description . "allowed_actions=selected with an EMPTY patterns_allowed permits only GitHub-owned and verified actions. Every org reusable is refused at parse time, so runs die as startup_failure with ZERO jobs and no check run.")

     (detection
      (method . "repository-settings")
      (api . "GET /repos/{o}/{r}/actions/permissions and .../selected-actions")
      (condition . "allowed_actions == selected AND patterns_allowed is empty"))

     (measured
      (date . "2026-07-29")
      (scanned . 418)
      (broken . 2)
      (proof . "governance.yml went startup_failure/0 jobs -> success/10 jobs with NO file change -- settings only."))

     (remediation
      (action . "populate-patterns_allowed-or-set-allowed_actions-all")
      (warning . "This is a SETTINGS fault. No workflow edit can fix it, and a startup_failure run cannot be re-run -- it has no jobs. Verify by dispatching a fresh run and asserting jobs > 0.")))

    (error-type
     (id . "ERR-GATE-005")
     (name . "empty-jobs-map")
     (severity . "high")
     (category . "unsatisfiable-gate")
     (description . "A workflow whose `jobs:` key is present but has no uncommented job. An empty jobs map is invalid, so the run fails with zero jobs, permanently, on every matching event.")

     (detection
      (method . "parse-workflow")
      (condition . "`jobs:` present AND no uncommented `  <name>:` beneath it"))

     (measured
      (date . "2026-07-29")
      (repos . 20)
      (file . "e2e.yml -- an un-instantiated scaffold, every job block commented out"))

     (remediation
      (action . "instantiate-a-real-job-or-delete-the-workflow")
      (warning . "Do NOT write a job that passes without testing anything -- that converts a dead gate into a fake one, which is worse because it is read as evidence.")))


;; End of ERROR-CATALOG.scm
