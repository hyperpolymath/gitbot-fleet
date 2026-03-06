#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-security-policy.sh — Add SECURITY.md to repositories that lack one
#
# Fixes Scorecard SC-016 (Security-Policy) by creating a SECURITY.md file
# with vulnerability disclosure policy based on the RSR template.
#
# Usage: fix-security-policy.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Security Policy Fix ==="
echo "  Repo: $REPO_PATH"

# Check if SECURITY.md already exists
for loc in "$REPO_PATH/SECURITY.md" "$REPO_PATH/.github/SECURITY.md" "$REPO_PATH/security.md"; do
    if [[ -f "$loc" ]]; then
        echo "  SKIP: Security policy already exists at ${loc#$REPO_PATH/}"
        exit 0
    fi
done

# Extract repo name from path
REPO_NAME=$(basename "$REPO_PATH")

# Create SECURITY.md
cat > "$REPO_PATH/SECURITY.md" <<SECURITY_MD
# Security Policy

## Reporting a Vulnerability

If you discover a security vulnerability in this project, please report it
responsibly. **Do not create a public GitHub issue for security vulnerabilities.**

### How to Report

1. **Email**: Send details to j.d.a.jewell@open.ac.uk
2. **GitHub Security Advisory**: Use the [Security tab](https://github.com/hyperpolymath/${REPO_NAME}/security/advisories/new) to create a private advisory

### What to Include

- Description of the vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (if any)

### Response Timeline

- **Acknowledgement**: Within 48 hours
- **Initial assessment**: Within 1 week
- **Fix timeline**: Depends on severity, typically within 30 days

### Supported Versions

| Version | Supported |
|---------|-----------|
| Latest  | Yes       |

### Scope

This policy applies to the ${REPO_NAME} repository and its direct dependencies
managed by the hyperpolymath project.

## Security Scanning

This repository is monitored by [Hypatia](https://github.com/hyperpolymath/hypatia),
which performs continuous neurosymbolic security analysis across the ecosystem.
SECURITY_MD

echo "  CREATED SECURITY.md"
echo ""
echo "Fixed: 1 file created"
