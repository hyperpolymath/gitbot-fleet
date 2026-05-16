# SPDX-License-Identifier: PMPL-1.0-or-later
# Garment Sustainability Bot — Justfile (Rust/SPARK)
# https://github.com/casey/just

default:
    @just --list

# Build (debug)
build:
    cargo build

# Build optimised release binaries
release:
    cargo build --release

# Run all tests
test:
    cargo test --all-targets

# Run the bot
run:
    cargo run --bin gsbot

# Load sample data into the database
load-data:
    cargo run --bin gsbot-load-fixtures

# Export database to JSON
export-data:
    cargo run --bin gsbot-export-data

# Backup database
backup:
    cargo run --bin gsbot-backup-db

# Lint (clippy, warnings as errors) — invoked by Mustfile
lint:
    cargo clippy --all-targets -- -D warnings

# Format check — invoked by Mustfile
fmt:
    cargo fmt --all -- --check

# Apply formatting
format:
    cargo fmt --all

# Clean build artefacts
clean:
    cargo clean

# Initialise project (build + load data)
init: build load-data
    @echo "✅ Project initialised! Set DISCORD_TOKEN in .env, then 'just run'"

# RSR compliance check
rsr-check:
    @echo "📋 RSR Compliance Check"
    @echo "======================="
    @test -f README.adoc && echo "  ✓ README.adoc" || echo "  ✗ README.adoc"
    @test -f LICENSE && echo "  ✓ LICENSE" || echo "  ✗ LICENSE"
    @test -f MAINTAINERS.adoc && echo "  ✓ MAINTAINERS.adoc" || echo "  ✗ MAINTAINERS.adoc"
    @test -f CHANGELOG.adoc && echo "  ✓ CHANGELOG.adoc" || echo "  ✗ CHANGELOG.adoc"
    @test -f Cargo.toml && echo "  ✓ Cargo.toml" || echo "  ✗ Cargo.toml"
    @test -f Justfile && echo "  ✓ Justfile" || echo "  ✗ Justfile"
    @test -f Mustfile && echo "  ✓ Mustfile" || echo "  ✗ Mustfile"
    @test -d tests && echo "  ✓ tests/" || echo "  ✗ tests/"
    @test -f Containerfile && echo "  ✓ Containerfile" || echo "  ✗ Containerfile"
    @test -f docker-compose.yml && echo "  ✓ docker-compose.yml" || echo "  ✗ docker-compose.yml"

# Validate project health
validate: rsr-check test lint
    @echo "✅ Validation complete!"

# Security audit
security:
    cargo audit || true

# Docker
docker-build:
    docker build -t gsbot:latest -f Containerfile .

docker-up:
    docker compose up -d

docker-down:
    docker compose down

docker-logs:
    docker compose logs -f bot

# Release tag
tag-release version:
    git tag -a v{{version}} -m "Release {{version}}"
    git push origin v{{version}}
