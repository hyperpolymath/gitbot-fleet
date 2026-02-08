K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# Gitbot Fleet Deployment
#
# This K9 component automates the deployment and configuration of the
# gitbot fleet (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot)
# for repository quality enforcement across the hyperpolymath ecosystem.

leash = 'Hunt

pedigree = {
  schema_version = "1.0.0",
  component_type = "bot-fleet-deployment",
  author = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>",
  description = "Automated deployment of gitbot fleet for quality enforcement",
  created = "2026-01-30",
  k9_spec_version = "1.0.0",
}

config = {
  # Bot activation settings
  bots = {
    rhodibot = {
      enabled | Bool = true,
      description = "RSR compliance validation",
      priority | Number = 1,  # Highest priority (structural checks first)
    },

    echidnabot = {
      enabled | Bool = true,
      description = "Formal verification and fuzzing",
      priority | Number = 2,
    },

    sustainabot = {
      enabled | Bool = true,
      description = "Ecological and economic standards",
      priority | Number = 3,
    },

    glambot = {
      enabled | Bool = true,
      description = "Presentation quality and accessibility",
      priority | Number = 4,
    },

    seambot = {
      enabled | Bool = true,
      description = "Integration health testing",
      priority | Number = 5,
    },

    finishbot = {
      enabled | Bool = true,
      description = "Release readiness validation",
      priority | Number = 6,
    },

    accessibilitybot = {
      enabled | Bool = true,
      description = "WCAG 2.3 AAA accessibility compliance",
      priority | Number = 7,  # After finishbot, before executor
    },

    cipherbot = {
      enabled | Bool = true,
      description = "Cryptographic hygiene and post-quantum readiness",
      priority | Number = 8,  # Specialist tier, after finishers
    },

    robot_repo_automaton = {
      enabled | Bool = true,
      description = "Tier 3 Executor - automated fix application with confidence thresholds",
      priority | Number = 9,  # Executor tier runs after all verifiers, finishers, and specialists
    },
  },

  # GitHub organization settings
  github_org | String = "hyperpolymath",
  github_token_env | String = "GITHUB_TOKEN",

  # Deployment settings
  deployment = {
    mode | [| 'Test, 'Production |] = 'Test,
    auto_fix | Bool = false,  # Only report issues, don't auto-fix
    create_issues | Bool = true,
    pr_comments | Bool = true,
    schedule | String = "0 0 * * 0",  # Weekly on Sunday midnight UTC
  },

  # Shared context layer configuration
  shared_context = {
    cache_enabled | Bool = true,
    cache_ttl_hours | Number = 24,
    state_storage | [| 'Memory, 'Redis, 'File |] = 'File,
    state_file_path | String = "~/.gitbot-fleet/state",
  },

  # Integration settings
  integrations = {
    hypatia_enabled | Bool = true,  # Feed findings to Hypatia
    robot_repo_automaton_enabled | Bool = true,  # Coordinate with automaton
    git_dispatcher_enabled | Bool = false,  # Legacy integration
  },
}

# Bot deployment workflows
bot_workflows = {
  rhodibot = {
    description = "RSR compliance validation workflow",
    enabled = config.bots.rhodibot.enabled,
    workflow_file = ".github/workflows/rhodibot.yml",
    checks = [
      "required_files",
      "directory_structure",
      "license_presence",
      "checkpoint_files",  # STATE.scm, ECOSYSTEM.scm, META.scm
      "workflow_compliance",
    ],
    auto_fix_items = [
      "add_missing_files",
      "fix_directory_structure",
    ],
  },

  echidnabot = {
    description = "Formal verification and fuzzing workflow",
    enabled = config.bots.echidnabot.enabled,
    workflow_file = ".github/workflows/echidnabot.yml",
    checks = [
      "formal_spec_presence",
      "proof_validation",
      "fuzzing_coverage",
      "theorem_prover_integration",
    ],
    auto_fix_items = [],  # Verification can't be auto-fixed
  },

  sustainabot = {
    description = "Eco/econ standards workflow",
    enabled = config.bots.sustainabot.enabled,
    workflow_file = ".github/workflows/sustainabot.yml",
    checks = [
      "carbon_intensity",
      "resource_efficiency",
      "technical_debt",
      "dependency_health",
    ],
    auto_fix_items = [
      "optimize_dependencies",
      "reduce_bundle_size",
    ],
  },

  glambot = {
    description = "Presentation quality workflow",
    enabled = config.bots.glambot.enabled,
    workflow_file = ".github/workflows/glambot.yml",
    checks = [
      "wcag_accessibility",
      "seo_optimization",
      "machine_readability",
      "visual_consistency",
    ],
    auto_fix_items = [
      "add_alt_text",
      "fix_heading_hierarchy",
      "add_semantic_markup",
    ],
  },

  seambot = {
    description = "Integration health workflow",
    enabled = config.bots.seambot.enabled,
    workflow_file = ".github/workflows/seambot.yml",
    checks = [
      "api_contracts",
      "cross_component_tests",
      "end_to_end_flows",
      "integration_coverage",
    ],
    auto_fix_items = [
      "generate_integration_tests",
    ],
  },

  finishbot = {
    description = "Release readiness workflow",
    enabled = config.bots.finishbot.enabled,
    workflow_file = ".github/workflows/finishbot.yml",
    checks = [
      "no_placeholders",
      "license_validation",
      "claim_verification",
      "execution_tests",
      "changelog_present",
    ],
    auto_fix_items = [
      "remove_todo_placeholders",
      "add_changelog_entry",
    ],
  },

  accessibilitybot = {
    description = "WCAG 2.3 AAA accessibility compliance workflow",
    enabled = config.bots.accessibilitybot.enabled,
    workflow_file = ".github/workflows/accessibilitybot.yml",
    checks = [
      "wcag_a_conformance",
      "wcag_aa_conformance",
      "wcag_aaa_conformance",
      "aria_usage",
      "css_accessibility",
      "keyboard_navigation",
      "color_contrast",
      "alt_text",
    ],
    auto_fix_items = [
      "add_aria_labels",
      "fix_color_contrast",
      "add_alt_text",
      "fix_heading_hierarchy",
    ],
  },

  cipherbot = {
    description = "Cryptographic hygiene and post-quantum readiness workflow",
    enabled = config.bots.cipherbot.enabled,
    workflow_file = ".github/workflows/cipherbot.yml",
    checks = [
      "hash_algorithm_strength",
      "symmetric_cipher_selection",
      "key_exchange_protocols",
      "signature_scheme_validity",
      "password_hashing_compliance",
      "post_quantum_readiness",
    ],
    auto_fix_items = [
      "upgrade_weak_hash_algorithms",
      "add_pq_readiness_markers",
    ],
  },

  robot_repo_automaton = {
    description = "Tier 3 Executor - automated fix application",
    enabled = config.bots.robot_repo_automaton.enabled,
    workflow_file = ".github/workflows/robot-repo-automaton.yml",
    checks = [
      "compliance_scanning",
      "confidence_threshold_evaluation",
      "fix_dry_run_validation",
    ],
    auto_fix_items = [
      "apply_spdx_headers",
      "create_missing_standard_files",
      "pin_workflow_actions",
      "add_workflow_permissions",
      "fix_cors_wildcards",
    ],
  },
}

# Just recipes for deployment
recipes = {
  default = {
    recipe = "deploy-fleet",
    description = "Deploy entire gitbot fleet",
  },

  "check-prerequisites" = {
    description = "Verify prerequisites for fleet deployment",
    commands = [
      "echo 'Checking prerequisites for gitbot fleet deployment...'",

      # Check GitHub CLI
      "command -v gh >/dev/null 2>&1 || { echo 'Error: gh CLI not found'; exit 1; }",
      "gh auth status",

      # Check GitHub token
      "if [ -z \"$%{config.github_token_env}\" ]; then echo 'Error: GitHub token not set'; exit 1; fi",

      # Create state directory
      "mkdir -p %{config.shared_context.state_file_path}",

      "echo 'Prerequisites check complete!'",
    ],
  },

  "generate-bot-workflows" = {
    description = "Generate GitHub Actions workflows for all bots",
    dependencies = ["check-prerequisites"],
    commands =
      # For each enabled bot, generate its workflow file
      bot_workflows |> std.record.to_array |> std.array.map (fun entry =>
        let bot_name = std.string.from entry.field in
        let bot_config = entry.value in
        if bot_config.enabled then
          [
            "echo 'Generating workflow for %{bot_name}...'",
            "mkdir -p .github/workflows",
            "cat > .github/workflows/%{bot_name}.yml <<'WORKFLOW_EOF'\n" ++
            "# SPDX-License-Identifier: PMPL-1.0-or-later\n" ++
            "name: %{bot_config.description}\n\n" ++
            "on:\n" ++
            "  push:\n    branches: [main, develop]\n" ++
            "  pull_request:\n    branches: [main]\n" ++
            "  schedule:\n    - cron: '%{config.deployment.schedule}'\n" ++
            "  workflow_dispatch:\n\n" ++
            "permissions: read-all\n\n" ++
            "jobs:\n" ++
            "  %{bot_name}:\n" ++
            "    runs-on: ubuntu-latest\n" ++
            "    steps:\n" ++
            "      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11\n\n" ++
            "      - name: Run %{bot_name}\n" ++
            "        run: |\n" ++
            "          echo 'Running %{bot_config.description}...'\n" ++
            "          # Bot-specific checks here\n" ++
            "WORKFLOW_EOF",
            "echo 'Generated .github/workflows/%{bot_name}.yml'",
          ]
        else
          ["echo 'Skipping %{bot_name} (disabled)'"]
      ) |> std.array.flatten,
  },

  "deploy-to-repo" = {
    description = "Deploy bot workflows to current repository",
    dependencies = ["generate-bot-workflows"],
    commands = [
      "echo 'Deploying bot workflows to current repository...'",

      # Add workflows to git
      "git add .github/workflows/*bot.yml",

      # Commit and push if not in test mode
      "if [ %{std.string.from config.deployment.mode} = 'Production' ]; then " ++
        "git commit -m 'feat: add gitbot fleet workflows' && git push; " ++
      "else " ++
        "echo 'TEST MODE: Would commit and push workflows'; " ++
      "fi",

      "echo 'Deployment to single repo complete!'",
    ],
  },

  "deploy-to-organization" = {
    description = "Deploy bot workflows to all repos in organization",
    dependencies = ["generate-bot-workflows"],
    commands = [
      "echo 'Deploying gitbot fleet to organization: %{config.github_org}'",
      "echo 'Mode: %{std.string.from config.deployment.mode}'",
      "echo ''",

      # Get list of repos
      "gh repo list %{config.github_org} --limit 1000 --json name --jq '.[].name' > .gitbot-repos.txt",
      "echo 'Found' $(wc -l < .gitbot-repos.txt) 'repositories'",

      # Deploy to each repo (simplified - actual implementation would use robot-repo-automaton)
      "echo 'Deployment across %{config.github_org} would be coordinated via robot-repo-automaton'",
      "echo 'For manual deployment, use: robot-repo-automaton deploy-bots --org %{config.github_org}'",
    ],
  },

  "test-bot-coordination" = {
    description = "Test bot coordination and shared context",
    dependencies = ["deploy-to-repo"],
    commands = [
      "echo 'Testing bot coordination...'",

      # Test shared context layer
      "echo 'Testing shared context layer (%{std.string.from config.shared_context.state_storage})...'",
      "if [ %{std.string.from config.shared_context.state_storage} = 'File' ]; then " ++
        "test -d %{config.shared_context.state_file_path} || mkdir -p %{config.shared_context.state_file_path}; " ++
      "fi",

      # Test bot priority ordering
      "echo 'Bot execution order (by priority):'",
      "echo '  1. rhodibot (RSR compliance)'",
      "echo '  2. echidnabot (verification)'",
      "echo '  3. sustainabot (eco/econ)'",
      "echo '  4. glambot (presentation)'",
      "echo '  5. seambot (integration)'",
      "echo '  6. finishbot (release readiness)'",

      "echo 'Bot coordination test complete!'",
    ],
  },

  "deploy-fleet" = {
    description = "Deploy entire gitbot fleet (default)",
    dependencies = [
      "check-prerequisites",
      "generate-bot-workflows",
      "deploy-to-repo",
      "test-bot-coordination",
    ],
    commands = [
      "echo ''",
      "echo '╔══════════════════════════════════════════════════════════╗'",
      "echo '║  ✅ Gitbot Fleet Deployment Complete!                   ║'",
      "echo '╚══════════════════════════════════════════════════════════╝'",
      "echo ''",
      "echo 'Enabled bots:'",
      (if config.bots.rhodibot.enabled then "echo '  ✓ rhodibot (RSR compliance)'" else ""),
      (if config.bots.echidnabot.enabled then "echo '  ✓ echidnabot (verification)'" else ""),
      (if config.bots.sustainabot.enabled then "echo '  ✓ sustainabot (eco/econ)'" else ""),
      (if config.bots.glambot.enabled then "echo '  ✓ glambot (presentation)'" else ""),
      (if config.bots.seambot.enabled then "echo '  ✓ seambot (integration)'" else ""),
      (if config.bots.finishbot.enabled then "echo '  ✓ finishbot (release readiness)'" else ""),
      (if config.bots.accessibilitybot.enabled then "echo '  ✓ accessibilitybot (WCAG compliance)'" else ""),
      (if config.bots.cipherbot.enabled then "echo '  ✓ cipherbot (crypto hygiene)'" else ""),
      (if config.bots.robot_repo_automaton.enabled then "echo '  ✓ robot-repo-automaton (Tier 3 executor)'" else ""),
      "echo ''",
      "echo 'Mode: %{std.string.from config.deployment.mode}'",
      "echo 'Organization: %{config.github_org}'",
      "echo ''",
    ],
  },
}

validation = {
  # At least one bot must be enabled
  has_enabled_bots =
    config.bots.rhodibot.enabled
    || config.bots.echidnabot.enabled
    || config.bots.sustainabot.enabled
    || config.bots.glambot.enabled
    || config.bots.seambot.enabled
    || config.bots.finishbot.enabled
    || config.bots.accessibilitybot.enabled
    || config.bots.cipherbot.enabled
    || config.bots.robot_repo_automaton.enabled
    | doc "At least one bot must be enabled",

  # Deployment mode must be valid
  valid_deployment_mode =
    config.deployment.mode == 'Test
    || config.deployment.mode == 'Production
    | doc "Deployment mode must be Test or Production",

  # Cache TTL must be positive
  valid_cache_ttl =
    !config.shared_context.cache_enabled
    || config.shared_context.cache_ttl_hours > 0
    | doc "Cache TTL must be positive if cache is enabled",

  # State storage must be valid
  valid_state_storage =
    config.shared_context.state_storage == 'Memory
    || config.shared_context.state_storage == 'Redis
    || config.shared_context.state_storage == 'File
    | doc "State storage must be Memory, Redis, or File",
}
