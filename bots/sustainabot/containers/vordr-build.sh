#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
#
# Oikos Container Build Script
#
# Builds all container images using Vörðr (Svalinn OCI runtime)
# https://github.com/hyperpolymath/svalinn

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_step() {
    echo -e "${CYAN}[STEP]${NC} $1"
}

# Check for container runtime (prefer vordr, fallback to podman)
detect_runtime() {
    if command -v vordr &> /dev/null; then
        echo "vordr"
    elif command -v podman &> /dev/null; then
        echo "podman"
    else
        log_error "No container runtime found."
        log_info "Install Vörðr: https://github.com/hyperpolymath/svalinn"
        log_info "Or fallback: sudo dnf install podman"
        exit 1
    fi
}

RUNTIME=$(detect_runtime)
log_info "Using container runtime: ${RUNTIME}"

# Display Vörðr version if available
if [[ "$RUNTIME" == "vordr" ]]; then
    log_info "Vörðr version: $(vordr --version 2>/dev/null || echo 'unknown')"
fi

cd "$PROJECT_ROOT"

# Ensure ReScript is pre-compiled
if [[ ! -f "bot-integration/src/Oikos.res.js" ]]; then
    log_warn "ReScript not compiled. Running build..."
    cd bot-integration
    if command -v deno &> /dev/null; then
        deno task build:rescript
    else
        log_error "Deno not found. Please install Deno and run: deno task build:rescript"
        exit 1
    fi
    cd "$PROJECT_ROOT"
fi

# Build main Oikos image
log_step "Building oikos:latest..."
$RUNTIME build \
    --tag oikos:latest \
    --file containers/Containerfile \
    --progress=plain \
    .

# Build policy engine image
log_step "Building oikos-policy:latest..."
$RUNTIME build \
    --tag oikos-policy:latest \
    --file containers/Containerfile.policy \
    --progress=plain \
    .

# Tag images with version
VERSION=$(git describe --tags --always 2>/dev/null || echo "0.1.0-beta")
log_info "Tagging images with version: $VERSION"

$RUNTIME tag oikos:latest "oikos:$VERSION"
$RUNTIME tag oikos-policy:latest "oikos-policy:$VERSION"

# List built images
log_info "Built images:"
$RUNTIME images | grep oikos || true

log_info "Build complete!"
echo ""
echo "To run the stack:"
if [[ "$RUNTIME" == "vordr" ]]; then
    echo "  vordr run -d -p 3000:3000 --name oikos oikos:latest"
    echo ""
    echo "To run with compose (requires podman-compose or docker-compose):"
fi
echo "  cd containers && podman-compose up -d"
echo ""
echo "To push to registry:"
echo "  $RUNTIME push oikos:$VERSION"
