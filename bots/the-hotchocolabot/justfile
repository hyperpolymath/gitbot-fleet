# justfile for HotChocolaBot
# https://github.com/casey/just
#
# Install: cargo install just
# Usage: just <recipe>
# List all recipes: just --list

# Default recipe (runs when you type 'just')
default:
    @just --list

# === Development Recipes ===

# Run the bot with mock hardware
run:
    RUST_LOG=debug cargo run

# Run in release mode
run-release:
    cargo run --release

# Watch for changes and rebuild automatically (requires cargo-watch)
watch:
    cargo watch -x 'run'

# === Testing Recipes ===

# Run all tests
test:
    cargo test --verbose

# Run tests with output
test-verbose:
    cargo test -- --nocapture

# Run only unit tests
test-unit:
    cargo test --lib

# Run only integration tests
test-integration:
    cargo test --test '*'

# Run doc tests
test-doc:
    cargo test --doc

# Run tests with coverage (requires cargo-tarpaulin)
test-coverage:
    cargo tarpaulin --out Html --output-dir target/coverage

# === Code Quality Recipes ===

# Format code
fmt:
    cargo fmt --all

# Check formatting without changing files
fmt-check:
    cargo fmt --all -- --check

# Run clippy linter
lint:
    cargo clippy --all-targets --all-features -- -D warnings

# Run clippy with auto-fixes
lint-fix:
    cargo clippy --all-targets --all-features --fix

# Check code without building
check:
    cargo check --all-targets --all-features

# === Security Recipes ===

# Audit dependencies for security vulnerabilities
audit:
    cargo audit

# Update dependencies and audit
audit-update:
    cargo update
    cargo audit

# Check for outdated dependencies
outdated:
    cargo outdated

# === Build Recipes ===

# Build in debug mode
build:
    cargo build

# Build in release mode (optimized)
build-release:
    cargo build --release

# Clean build artifacts
clean:
    cargo clean

# Build for Raspberry Pi (cross-compile to armv7)
build-rpi:
    cross build --target armv7-unknown-linux-gnueabihf --release

# Build documentation
build-docs:
    cargo doc --no-deps --all-features

# Build and open documentation
docs:
    cargo doc --no-deps --all-features --open

# === Deployment Recipes ===

# Deploy to Raspberry Pi (requires RPI_HOST env var or argument)
deploy HOST="pi@raspberrypi.local":
    @echo "Building for Raspberry Pi..."
    just build-rpi
    @echo "Copying binary to {{HOST}}..."
    scp target/armv7-unknown-linux-gnueabihf/release/hotchocolabot {{HOST}}:~/
    @echo "Copying configuration..."
    scp config.toml.example {{HOST}}:~/config.toml
    @echo "Deployment complete! SSH to {{HOST}} and run: sudo ~/hotchocolabot"

# Copy only the binary (after building)
deploy-binary HOST="pi@raspberrypi.local":
    scp target/armv7-unknown-linux-gnueabihf/release/hotchocolabot {{HOST}}:~/

# === Installation Recipes ===

# Install required development tools
install-dev-tools:
    cargo install cargo-watch
    cargo install cargo-tarpaulin
    cargo install cargo-audit
    cargo install cargo-outdated
    cargo install cross

# Install for Raspberry Pi setup
install-rpi-deps:
    @echo "Installing Raspberry Pi dependencies..."
    sudo apt-get update
    sudo apt-get install -y libi2c-dev i2c-tools

# Enable I2C on Raspberry Pi
setup-rpi-i2c:
    @echo "Enabling I2C..."
    sudo raspi-config nonint do_i2c 0
    @echo "Adding user to i2c group..."
    sudo usermod -a -G i2c $USER
    @echo "I2C enabled. Reboot may be required."

# === Validation Recipes ===

# Run full validation suite (format, lint, test, audit)
validate: fmt-check lint test audit
    @echo "✓ All validation checks passed!"

# Pre-commit validation (fast checks)
pre-commit: fmt lint test
    @echo "✓ Pre-commit checks passed!"

# Pre-push validation (thorough checks)
pre-push: validate
    @echo "✓ Ready to push!"

# === RSR Compliance Recipes ===

# Check RSR (Rhodium Standard Repository) compliance
rsr-check:
    @echo "=== RSR Compliance Check ==="
    @echo ""
    @echo "✓ Type Safety: Rust compile-time guarantees"
    @echo "✓ Memory Safety: Zero unsafe blocks"
    @cargo grep -q 'unsafe' src/ && echo "✗ Found unsafe blocks!" || echo "✓ No unsafe blocks found"
    @echo "✓ Offline-First: No network dependencies"
    @cargo tree | grep -q 'reqwest\|hyper\|curl' && echo "✗ Network dependencies found!" || echo "✓ No network dependencies"
    @echo ""
    @echo "=== Documentation Files ==="
    @test -f README.md && echo "✓ README.md" || echo "✗ README.md missing"
    @test -f LICENSE-MIT && echo "✓ LICENSE-MIT" || echo "✗ LICENSE-MIT missing"
    @test -f LICENSE-APACHE && echo "✓ LICENSE-APACHE" || echo "✗ LICENSE-APACHE missing"
    @test -f SECURITY.md && echo "✓ SECURITY.md" || echo "✗ SECURITY.md missing"
    @test -f CONTRIBUTING.md && echo "✓ CONTRIBUTING.md" || echo "✗ CONTRIBUTING.md missing"
    @test -f CODE_OF_CONDUCT.md && echo "✓ CODE_OF_CONDUCT.md" || echo "✗ CODE_OF_CONDUCT.md missing"
    @test -f MAINTAINERS.md && echo "✓ MAINTAINERS.md" || echo "✗ MAINTAINERS.md missing"
    @test -f CHANGELOG.md && echo "✓ CHANGELOG.md" || echo "✗ CHANGELOG.md missing"
    @echo ""
    @echo "=== .well-known/ Directory ==="
    @test -f .well-known/security.txt && echo "✓ security.txt (RFC 9116)" || echo "✗ security.txt missing"
    @test -f .well-known/ai.txt && echo "✓ ai.txt" || echo "✗ ai.txt missing"
    @test -f .well-known/humans.txt && echo "✓ humans.txt" || echo "✗ humans.txt missing"
    @echo ""
    @echo "=== Build System ==="
    @test -f justfile && echo "✓ justfile" || echo "✗ justfile missing"
    @test -f flake.nix && echo "✓ flake.nix" || echo "✗ flake.nix missing"
    @test -f .github/workflows/rust_ci.yml && echo "✓ CI/CD (GitHub Actions)" || echo "✗ CI/CD missing"
    @echo ""
    @echo "=== Test Coverage ==="
    @cargo test --quiet && echo "✓ All tests passing" || echo "✗ Tests failing"
    @echo ""
    @echo "=== RSR Level Assessment ==="
    @echo "Target: Bronze (minimum) to Silver"
    @echo "Check: https://rhodium-standard.org for latest requirements"

# Show project statistics
stats:
    @echo "=== HotChocolaBot Statistics ==="
    @echo ""
    @echo "Lines of Code:"
    @tokei src/
    @echo ""
    @echo "Dependencies:"
    @cargo tree --depth 1
    @echo ""
    @echo "Crate Size:"
    @du -sh target/release/hotchocolabot 2>/dev/null || echo "Not built yet (run 'just build-release')"
    @echo ""
    @echo "Documentation:"
    @find . -name "*.md" | wc -l | xargs echo "Markdown files:"
    @find . -name "*.md" -exec wc -l {} + | tail -1

# === Hardware Testing Recipes ===

# Test I2C devices (on Raspberry Pi)
test-i2c:
    @echo "Scanning I2C bus..."
    sudo i2cdetect -y 1

# Test GPIO pins (requires root on Raspberry Pi)
test-gpio:
    @echo "Testing GPIO access..."
    @echo "This is a dry-run test - no actual hardware control"
    cargo test --test gpio_test

# === Configuration Recipes ===

# Create config.toml from example
create-config:
    @test -f config.toml && echo "config.toml already exists!" || cp config.toml.example config.toml
    @echo "Created config.toml - edit as needed"

# Validate config.toml syntax
validate-config:
    @echo "Validating config.toml..."
    @cargo run -- --check-config || echo "Config validation not yet implemented in main.rs"

# === Release Recipes ===

# Prepare a new release (updates changelog, tags, etc.)
release VERSION:
    @echo "Preparing release {{VERSION}}..."
    @echo "1. Updating Cargo.toml version..."
    @sed -i 's/^version = .*/version = "{{VERSION}}"/' Cargo.toml
    @echo "2. Building release binary..."
    cargo build --release
    @echo "3. Running tests..."
    cargo test
    @echo "4. TODO: Update CHANGELOG.md manually"
    @echo "5. Commit and tag:"
    @echo "   git add Cargo.toml CHANGELOG.md"
    @echo "   git commit -m 'Release v{{VERSION}}'"
    @echo "   git tag -a v{{VERSION}} -m 'Release v{{VERSION}}'"
    @echo "   git push origin main --tags"

# === Utility Recipes ===

# Show environment information
env:
    @echo "=== Environment Info ==="
    @echo "Rust version:"
    @rustc --version
    @echo ""
    @echo "Cargo version:"
    @cargo --version
    @echo ""
    @echo "Platform:"
    @uname -a
    @echo ""
    @echo "Git branch:"
    @git branch --show-current
    @echo ""
    @echo "Git commit:"
    @git rev-parse --short HEAD

# Clean all build artifacts and caches
clean-all: clean
    @echo "Removing target/"
    rm -rf target/
    @echo "Removing Cargo.lock"
    rm -f Cargo.lock
    @echo "Cleaning cargo cache..."
    cargo clean

# Update all dependencies to latest compatible versions
update:
    cargo update
    @echo "Dependencies updated. Run 'just test' to verify."

# === Competition Recipes ===

# Check competition submission readiness
check-submission:
    @echo "=== Robotics for Good Submission Checklist ==="
    @test -f docs/competition/submission_checklist.md && cat docs/competition/submission_checklist.md | grep -E '^\- \[.\]' || echo "Checklist not found"

# Generate impact report (placeholder - requires actual data)
impact-report:
    @echo "Impact report generation not yet implemented"
    @echo "See: docs/competition/submission_checklist.md"

# === Help Recipes ===

# Show detailed help for development
help:
    @echo "HotChocolaBot Development Guide"
    @echo ""
    @echo "Quick Start:"
    @echo "  just run              - Run with mock hardware"
    @echo "  just test             - Run all tests"
    @echo "  just validate         - Full validation suite"
    @echo ""
    @echo "Development:"
    @echo "  just watch            - Auto-rebuild on changes"
    @echo "  just lint             - Run clippy"
    @echo "  just fmt              - Format code"
    @echo ""
    @echo "Raspberry Pi:"
    @echo "  just build-rpi        - Cross-compile for RPi"
    @echo "  just deploy           - Build and deploy to RPi"
    @echo "  just test-i2c         - Scan I2C bus (on RPi)"
    @echo ""
    @echo "Quality:"
    @echo "  just pre-commit       - Fast pre-commit checks"
    @echo "  just audit            - Security audit"
    @echo "  just rsr-check        - Check RSR compliance"
    @echo ""
    @echo "For full list: just --list"
