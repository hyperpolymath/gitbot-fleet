# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# justfile — gitbot-fleet
# Run with: just <recipe>

set shell := ["bash", "-euo", "pipefail", "-c"]

# Default recipe: show help
import? "contractile.just"

default:
    @just --list

# Build robot-repo-automaton (Rust executor)
build:
    cd robot-repo-automaton && OPENSSL_NO_VENDOR=1 cargo build --release

# Run robot-repo-automaton tests
test:
    cd robot-repo-automaton && OPENSSL_NO_VENDOR=1 cargo test

# Build shared-context library
build-shared:
    cd shared-context && cargo build

# Run fleet coordinator
coordinate *ARGS:
    bash fleet-coordinator.sh {{ARGS}}

# Scan supervised inventory (supports ~/.git-private-farm.scm and ~/.git-private-repos)
scan-supervised *ARGS:
    bash fleet-coordinator.sh scan-supervised --process {{ARGS}}

# Run dispatch runner with a manifest
dispatch manifest:
    bash scripts/dispatch-runner.sh "{{manifest}}"

# Process review findings (dry-run by default)
review *ARGS:
    bash scripts/process-review-findings.sh --dry-run {{ARGS}}

# Scan a repo for compliance issues
scan repo:
    robot-repo-automaton/target/release/robot-repo-automaton scan "{{repo}}"

# Fix a repo with PR creation
fix repo:
    robot-repo-automaton/target/release/robot-repo-automaton fix "{{repo}}" --create-pr

# Run hypatia security scan
hypatia-scan:
    @echo "Running hypatia neurosymbolic scan..."
    @if command -v hypatia-v2 &>/dev/null; then \
        hypatia-v2 . --severity=critical --severity=high; \
    else \
        echo "hypatia-v2 not found — run via CI workflow instead"; \
    fi

# Run panic-attack static analysis
panic-scan:
    @if [ -x "/var$REPOS_DIR/panic-attacker/target/release/panic-attack" ]; then \
        /var$REPOS_DIR/panic-attacker/target/release/panic-attack assail . --verbose; \
    else \
        echo "panic-attack not built — run 'cd /var$REPOS_DIR/panic-attacker && cargo build --release'"; \
    fi

# Run release maintenance hard-pass on a target repository
maintenance-hard-pass repo *ARGS:
    bash scripts/maintenance-hard-pass.sh --repo "{{repo}}" {{ARGS}}

# Discover and register repo coverage for gitbot-fleet/hypatia
enroll-repos repos_root="/var$REPOS_DIR" apply="false":
    @if [ "{{apply}}" = "true" ]; then \
        bash scripts/enroll-hypatia-fleet.sh --repos-root "{{repos_root}}" --apply; \
    else \
        bash scripts/enroll-hypatia-fleet.sh --repos-root "{{repos_root}}"; \
    fi

# Check license compliance
license-check:
    @echo "Checking for banned AGPL-3.0 headers..."
    @if grep -rl "AGPL-3.0" --include='*.sh' --include='*.rs' --include='*.scm' --include='*.yml' . 2>/dev/null; then \
        echo "FAIL: Found AGPL-3.0 headers"; \
        exit 1; \
    else \
        echo "PASS: No AGPL-3.0 headers found"; \
    fi

# Validate SCM files are in .machine_readable/ only
check-scm:
    @for f in STATE.scm META.scm ECOSYSTEM.scm; do \
        if [ -f "$$f" ]; then \
            echo "ERROR: $$f found in root"; exit 1; \
        fi; \
    done
    @echo "PASS: No SCM files in root"

# Clean all build artifacts
clean:
    cd robot-repo-automaton && cargo clean
    cd shared-context && cargo clean
    @echo "Cleaned."

# Run panic-attacker pre-commit scan
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"

# ═══════════════════════════════════════════════════════════════════════════════
# ONBOARDING & DIAGNOSTICS
# ═══════════════════════════════════════════════════════════════════════════════

# Check all required toolchain dependencies and report health
doctor:
    #!/usr/bin/env bash
    echo "═══════════════════════════════════════════════════"
    echo "  Gitbot Fleet Doctor — Toolchain Health Check"
    echo "═══════════════════════════════════════════════════"
    echo ""
    PASS=0; FAIL=0; WARN=0
    check() {
        local name="$1" cmd="$2" min="$3"
        if command -v "$cmd" >/dev/null 2>&1; then
            VER=$("$cmd" --version 2>&1 | head -1)
            echo "  [OK]   $name — $VER"
            PASS=$((PASS + 1))
        else
            echo "  [FAIL] $name — not found (need $min+)"
            FAIL=$((FAIL + 1))
        fi
    }
    check "just"              just      "1.25" 
    check "git"               git       "2.40" 
# Optional tools
if command -v panic-attack >/dev/null 2>&1; then
    echo "  [OK]   panic-attack — available"
    PASS=$((PASS + 1))
else
    echo "  [WARN] panic-attack — not found (pre-commit scanner)"
    WARN=$((WARN + 1))
fi
    echo ""
    echo "  Result: $PASS passed, $FAIL failed, $WARN warnings"
    if [ "$FAIL" -gt 0 ]; then
        echo "  Run 'just heal' to attempt automatic repair."
        exit 1
    fi
    echo "  All required tools present."

# Attempt to automatically install missing tools
heal:
    #!/usr/bin/env bash
    echo "═══════════════════════════════════════════════════"
    echo "  Gitbot Fleet Heal — Automatic Tool Installation"
    echo "═══════════════════════════════════════════════════"
    echo ""
if ! command -v just >/dev/null 2>&1; then
    echo "Installing just..."
    cargo install just 2>/dev/null || echo "Install just from https://just.systems"
fi
    echo ""
    echo "Heal complete. Run 'just doctor' to verify."

# Guided tour of the project structure and key concepts
tour:
    #!/usr/bin/env bash
    echo "═══════════════════════════════════════════════════"
    echo "  Gitbot Fleet — Guided Tour"
    echo "═══════════════════════════════════════════════════"
    echo ""
    echo '// SPDX-License-Identifier: PMPL-1.0-or-later'
    echo ""
    echo "Key directories:"
    echo "  docs/                     Documentation" 
    echo "  tests/                    Test suite" 
    echo "  .github/workflows/        CI/CD workflows" 
    echo "  .machine_readable/        Machine-readable metadata" 
    echo ""
    echo "Quick commands:"
    echo "  just doctor    Check toolchain health"
    echo "  just heal      Fix missing tools"
    echo "  just help-me   Common workflows"
    echo "  just default   List all recipes"
    echo ""
    echo "Read more: README.adoc, EXPLAINME.adoc"

# Show help for common workflows
help-me:
    #!/usr/bin/env bash
    echo "═══════════════════════════════════════════════════"
    echo "  Gitbot Fleet — Common Workflows"
    echo "═══════════════════════════════════════════════════"
    echo ""
echo "FIRST TIME SETUP:"
echo "  just doctor           Check toolchain"
echo "  just heal             Fix missing tools"
echo "" 
echo "PRE-COMMIT:"
echo "  just assail           Run panic-attacker scan"
echo ""
echo "LEARN:"
echo "  just tour             Guided project tour"
echo "  just default          List all recipes" 


# Print the current CRG grade (reads from READINESS.md '**Current Grade:** X' line)
crg-grade:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    echo "$$grade"

# Generate a shields.io badge markdown for the current CRG grade
# Looks for '**Current Grade:** X' in READINESS.md; falls back to X
crg-badge:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    case "$$grade" in \
      A) color="brightgreen" ;; B) color="green" ;; C) color="yellow" ;; \
      D) color="orange" ;; E) color="red" ;; F) color="critical" ;; \
      *) color="lightgrey" ;; esac; \
    echo "[![CRG $$grade](https://img.shields.io/badge/CRG-$$grade-$$color?style=flat-square)](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)"
