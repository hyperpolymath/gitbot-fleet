# Garment Sustainability Bot - Justfile
# https://github.com/casey/just

# Default recipe (show help)
default:
    @just --list

# Install dependencies
install:
    pip install -r requirements.txt
    pip install -e .

# Install development dependencies
install-dev:
    pip install -r requirements.txt
    pip install -e ".[dev]"

# Run all tests
test:
    pytest tests/ -v

# Run tests with coverage
test-cov:
    pytest tests/ -v --cov=src --cov-report=term-missing --cov-report=html

# Run linting checks
lint:
    black --check src/ tests/
    flake8 src/ tests/ --max-line-length=120 --extend-ignore=E203,W503
    -mypy src/ --ignore-missing-imports
    -pylint src/ --max-line-length=120 --disable=C0111,R0903

# Format code with black
format:
    black src/ tests/

# Clean up generated files
clean:
    find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
    find . -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null || true
    find . -type f -name "*.pyc" -delete
    rm -rf .pytest_cache .mypy_cache .coverage htmlcov dist build

# Run the bot
run:
    python src/bot/main.py

# Load sample data into database
load-data:
    python scripts/load_fixtures.py

# Export database to JSON
export-data:
    python scripts/export_data.py

# Backup database
backup:
    python scripts/backup_db.py

# Initialize project (install + load data)
init: install load-data
    @echo "✅ Project initialized! Edit .env and then run 'just run'"

# Check RSR compliance
rsr-check:
    @echo "📋 RSR Compliance Check"
    @echo "======================="
    @echo ""
    @echo "Documentation:"
    @test -f README.md && echo "  ✓ README.md" || echo "  ✗ README.md"
    @test -f LICENSE && echo "  ✓ LICENSE" || echo "  ✗ LICENSE"
    @test -f SECURITY.md && echo "  ✓ SECURITY.md" || echo "  ✗ SECURITY.md"
    @test -f CONTRIBUTING.md && echo "  ✓ CONTRIBUTING.md" || echo "  ✗ CONTRIBUTING.md"
    @test -f CODE_OF_CONDUCT.md && echo "  ✓ CODE_OF_CONDUCT.md" || echo "  ✗ CODE_OF_CONDUCT.md"
    @test -f MAINTAINERS.md && echo "  ✓ MAINTAINERS.md" || echo "  ✗ MAINTAINERS.md"
    @test -f CHANGELOG.md && echo "  ✓ CHANGELOG.md" || echo "  ✗ CHANGELOG.md"
    @echo ""
    @echo ".well-known/:"
    @test -f .well-known/security.txt && echo "  ✓ security.txt" || echo "  ✗ security.txt"
    @test -f .well-known/ai.txt && echo "  ✓ ai.txt" || echo "  ✗ ai.txt"
    @test -f .well-known/humans.txt && echo "  ✓ humans.txt" || echo "  ✗ humans.txt"
    @echo ""
    @echo "Build System:"
    @test -f justfile && echo "  ✓ justfile" || echo "  ✗ justfile"
    @test -f Makefile && echo "  ✓ Makefile" || echo "  ✗ Makefile"
    @test -f pytest.ini && echo "  ✓ pytest.ini" || echo "  ✗ pytest.ini"
    @echo ""
    @echo "CI/CD:"
    @test -f .github/workflows/main.yml && echo "  ✓ GitHub Actions" || echo "  ✗ GitHub Actions"
    @echo ""
    @echo "Tests:"
    @test -d tests && echo "  ✓ tests/" || echo "  ✗ tests/"
    @echo ""
    @echo "Docker:"
    @test -f Dockerfile && echo "  ✓ Dockerfile" || echo "  ✗ Dockerfile"
    @test -f docker-compose.yml && echo "  ✓ docker-compose.yml" || echo "  ✗ docker-compose.yml"

# Validate project health
validate: rsr-check test lint
    @echo ""
    @echo "✅ Validation complete!"

# Security check
security:
    pip install safety
    safety check

# Update dependencies
update:
    pip install --upgrade pip
    pip install --upgrade -r requirements.txt

# Create a new release (tag and push)
release version:
    @echo "Creating release {{version}}..."
    git tag -a v{{version}} -m "Release {{version}}"
    git push origin v{{version}}
    @echo "✅ Release v{{version}} created!"

# Run docker build
docker-build:
    docker build -t gsbot:latest .

# Run docker compose
docker-up:
    docker-compose up -d

# Stop docker compose
docker-down:
    docker-compose down

# View docker logs
docker-logs:
    docker-compose logs -f bot

# Check code stats
stats:
    @echo "📊 Project Statistics"
    @echo "===================="
    @echo ""
    @echo "Python files:"
    @find src -name "*.py" | wc -l
    @echo ""
    @echo "Lines of code:"
    @find src -name "*.py" -exec wc -l {} + | tail -1
    @echo ""
    @echo "Test files:"
    @find tests -name "*.py" | wc -l
    @echo ""
    @echo "Documentation files:"
    @find . -maxdepth 2 -name "*.md" | wc -l

# Watch tests (requires entr)
watch-test:
    find src tests -name "*.py" | entr -c pytest tests/ -v

# Watch and run bot (requires entr)
watch-run:
    find src -name "*.py" | entr -r python src/bot/main.py

# Create virtual environment
venv:
    python3 -m venv venv
    @echo "✅ Virtual environment created!"
    @echo "Activate with: source venv/bin/activate"

# Quick setup for new contributors
quickstart: venv install-dev load-data
    @echo "✅ Quick start complete!"
    @echo "Next steps:"
    @echo "  1. source venv/bin/activate"
    @echo "  2. cp .env.example .env"
    @echo "  3. Edit .env with your Discord token"
    @echo "  4. just run"
