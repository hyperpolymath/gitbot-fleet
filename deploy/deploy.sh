#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Fleet deployment script - Automated deployment for production

set -euo pipefail

# Configuration
FLEET_VERSION="0.2.0"
INSTALL_DIR="${INSTALL_DIR:-/opt/gitbot-fleet}"
CONFIG_DIR="${CONFIG_DIR:-/etc/fleet}"
DATA_DIR="${DATA_DIR:-/var/lib/fleet}"
FLEET_USER="${FLEET_USER:-fleet}"
FLEET_GROUP="${FLEET_GROUP:-fleet}"
DEPLOYMENT_MODE="${DEPLOYMENT_MODE:-systemd}"  # systemd or docker

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Check if running as root
check_root() {
    if [[ $EUID -ne 0 ]]; then
        log_error "This script must be run as root (use sudo)"
        exit 1
    fi
}

# Create fleet user and group
create_user() {
    log_info "Creating fleet user and group..."

    if ! getent group "$FLEET_GROUP" > /dev/null 2>&1; then
        groupadd --system "$FLEET_GROUP"
        log_success "Created group: $FLEET_GROUP"
    else
        log_info "Group $FLEET_GROUP already exists"
    fi

    if ! getent passwd "$FLEET_USER" > /dev/null 2>&1; then
        useradd --system --gid "$FLEET_GROUP" \
                --home-dir "$DATA_DIR" \
                --shell /usr/sbin/nologin \
                --comment "Gitbot Fleet Service User" \
                "$FLEET_USER"
        log_success "Created user: $FLEET_USER"
    else
        log_info "User $FLEET_USER already exists"
    fi
}

# Create required directories
create_directories() {
    log_info "Creating directory structure..."

    mkdir -p "$INSTALL_DIR"/{dashboard,shared-context,deploy}
    mkdir -p "$CONFIG_DIR"
    mkdir -p "$DATA_DIR"/{context,logs}

    # Set ownership
    chown -R "$FLEET_USER:$FLEET_GROUP" "$DATA_DIR"
    chown -R root:root "$INSTALL_DIR"
    chown -R root:root "$CONFIG_DIR"

    # Set permissions
    chmod 755 "$INSTALL_DIR"
    chmod 750 "$CONFIG_DIR"
    chmod 750 "$DATA_DIR"

    log_success "Directory structure created"
}

# Build binaries
build_binaries() {
    log_info "Building Fleet binaries..."

    if ! command -v cargo &> /dev/null; then
        log_error "Cargo not found. Please install Rust: https://rustup.rs"
        exit 1
    fi

    cd "$(dirname "$0")/.."

    # Build dashboard
    log_info "Building dashboard..."
    cd dashboard
    cargo build --release
    cp target/release/fleet-dashboard "$INSTALL_DIR/dashboard/"
    cp -r static "$INSTALL_DIR/dashboard/"

    # Build CLI
    log_info "Building fleet CLI..."
    cd ../shared-context/fleet-cli
    cargo build --release
    cp target/release/fleet "$INSTALL_DIR/shared-context/"

    log_success "Binaries built and installed"
}

# Install systemd services
install_systemd() {
    log_info "Installing systemd services..."

    cp deploy/systemd/*.service /etc/systemd/system/
    systemctl daemon-reload

    log_success "Systemd services installed"
}

# Create default configuration
create_config() {
    log_info "Creating default configuration..."

    cat > "$CONFIG_DIR/dashboard.env" <<EOF
# Fleet Dashboard Configuration
# Generated on $(date -u +"%Y-%m-%d %H:%M:%S UTC")

# Repository to monitor
FLEET_REPO_PATH=/srv/repositories
FLEET_REPO_NAME=default

# Logging level (debug, info, warn, error)
RUST_LOG=fleet_dashboard=info

# Server bind address (default: 127.0.0.1:8080)
# FLEET_BIND_ADDR=127.0.0.1:8080
EOF

    chmod 640 "$CONFIG_DIR/dashboard.env"
    chown root:"$FLEET_GROUP" "$CONFIG_DIR/dashboard.env"

    log_success "Configuration created at $CONFIG_DIR/dashboard.env"
}

# Deploy with Docker
deploy_docker() {
    log_info "Deploying with Docker Compose..."

    if ! command -v docker &> /dev/null; then
        log_error "Docker not found. Please install Docker first."
        exit 1
    fi

    cd "$(dirname "$0")/.."

    # Create .env file
    cat > .env <<EOF
HOST_REPO_PATH=/srv/repositories
FLEET_REPO_NAME=default
RUST_LOG=info
EOF

    # Start services
    docker compose up -d

    log_success "Fleet deployed with Docker Compose"
    log_info "Dashboard available at: http://localhost:8080"
}

# Deploy with systemd
deploy_systemd() {
    log_info "Deploying with systemd..."

    create_user
    create_directories
    build_binaries
    install_systemd
    create_config

    # Enable and start dashboard
    systemctl enable fleet-dashboard.service
    systemctl start fleet-dashboard.service

    log_success "Fleet deployed with systemd"
    log_info "Check status: systemctl status fleet-dashboard"
}

# Show deployment status
show_status() {
    echo
    log_info "Deployment Status:"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

    if [[ "$DEPLOYMENT_MODE" == "docker" ]]; then
        docker compose ps
    else
        systemctl status fleet-dashboard.service --no-pager || true
    fi

    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo
    log_info "Dashboard URL: http://localhost:8080"
    log_info "API Health:    http://localhost:8080/api/health"
    log_info "Configuration: $CONFIG_DIR"
    log_info "Data:          $DATA_DIR"
    log_info "Logs:          journalctl -u fleet-dashboard -f"
    echo
}

# Uninstall fleet
uninstall() {
    log_warn "Uninstalling Fleet..."

    if [[ "$DEPLOYMENT_MODE" == "docker" ]]; then
        docker compose down -v
        log_success "Docker containers removed"
    else
        systemctl stop fleet-dashboard.service || true
        systemctl disable fleet-dashboard.service || true
        rm -f /etc/systemd/system/fleet-*.service
        systemctl daemon-reload
        log_success "Systemd services removed"
    fi

    read -p "Remove installation directory $INSTALL_DIR? [y/N] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        rm -rf "$INSTALL_DIR"
        log_success "Installation directory removed"
    fi

    read -p "Remove data directory $DATA_DIR? [y/N] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        rm -rf "$DATA_DIR"
        log_success "Data directory removed"
    fi

    log_success "Uninstallation complete"
}

# Main deployment flow
main() {
    log_info "Fleet Deployment Script v$FLEET_VERSION"
    log_info "Mode: $DEPLOYMENT_MODE"
    echo

    case "${1:-deploy}" in
        deploy)
            check_root
            if [[ "$DEPLOYMENT_MODE" == "docker" ]]; then
                deploy_docker
            else
                deploy_systemd
            fi
            show_status
            ;;
        uninstall)
            check_root
            uninstall
            ;;
        status)
            show_status
            ;;
        *)
            echo "Usage: $0 {deploy|uninstall|status}"
            echo
            echo "Environment variables:"
            echo "  DEPLOYMENT_MODE    systemd or docker (default: systemd)"
            echo "  INSTALL_DIR        Installation directory (default: /opt/gitbot-fleet)"
            echo "  CONFIG_DIR         Configuration directory (default: /etc/fleet)"
            echo "  DATA_DIR           Data directory (default: /var/lib/fleet)"
            echo "  FLEET_USER         Service user (default: fleet)"
            echo "  FLEET_GROUP        Service group (default: fleet)"
            exit 1
            ;;
    esac
}

main "$@"
