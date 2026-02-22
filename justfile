# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# justfile — gitbot-fleet
# Run with: just <recipe>

set shell := ["bash", "-euo", "pipefail", "-c"]

# Default recipe: show help
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
    @if [ -x "/var/mnt/eclipse/repos/panic-attacker/target/release/panic-attack" ]; then \
        /var/mnt/eclipse/repos/panic-attacker/target/release/panic-attack assail . --verbose; \
    else \
        echo "panic-attack not built — run 'cd /var/mnt/eclipse/repos/panic-attacker && cargo build --release'"; \
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
