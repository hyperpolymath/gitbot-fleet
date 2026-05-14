# Quick start script for Garment Sustainability Bot (Windows)
# This script sets up the development environment

Write-Host "🌱 Garment Sustainability Bot - Quick Start" -ForegroundColor Green
Write-Host "===========================================" -ForegroundColor Green
Write-Host ""

# Check Python version
Write-Host "📋 Checking Python version..." -ForegroundColor Cyan
$pythonVersion = python --version 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Error: Python not found. Please install Python 3.9 or higher" -ForegroundColor Red
    exit 1
}

Write-Host "✅ $pythonVersion detected" -ForegroundColor Green
Write-Host ""

# Create virtual environment
if (-not (Test-Path "venv")) {
    Write-Host "📦 Creating virtual environment..." -ForegroundColor Cyan
    python -m venv venv
    Write-Host "✅ Virtual environment created" -ForegroundColor Green
} else {
    Write-Host "✅ Virtual environment already exists" -ForegroundColor Green
}
Write-Host ""

# Activate virtual environment
Write-Host "🔄 Activating virtual environment..." -ForegroundColor Cyan
& "venv\Scripts\Activate.ps1"
Write-Host "✅ Virtual environment activated" -ForegroundColor Green
Write-Host ""

# Upgrade pip
Write-Host "⬆️  Upgrading pip..." -ForegroundColor Cyan
python -m pip install --upgrade pip --quiet
Write-Host "✅ Pip upgraded" -ForegroundColor Green
Write-Host ""

# Install dependencies
Write-Host "📚 Installing dependencies..." -ForegroundColor Cyan
pip install -r requirements.txt --quiet
Write-Host "✅ Dependencies installed" -ForegroundColor Green
Write-Host ""

# Install package in development mode
Write-Host "🔧 Installing package in development mode..." -ForegroundColor Cyan
pip install -e . --quiet
Write-Host "✅ Package installed" -ForegroundColor Green
Write-Host ""

# Check if .env exists
if (-not (Test-Path ".env")) {
    Write-Host "📝 Creating .env file..." -ForegroundColor Cyan
    Copy-Item .env.example .env
    Write-Host "✅ .env file created" -ForegroundColor Green
    Write-Host ""
    Write-Host "⚠️  IMPORTANT: Edit .env and add your Discord bot token!" -ForegroundColor Yellow
    Write-Host "   DISCORD_TOKEN=your_token_here" -ForegroundColor Yellow
    Write-Host ""
} else {
    Write-Host "✅ .env file already exists" -ForegroundColor Green
    Write-Host ""
}

# Create data directory
if (-not (Test-Path "data")) {
    Write-Host "📁 Creating data directory..." -ForegroundColor Cyan
    New-Item -ItemType Directory -Path "data" | Out-Null
    Write-Host "✅ Data directory created" -ForegroundColor Green
}

# Load sample data
Write-Host "📊 Loading sample data..." -ForegroundColor Cyan
python scripts/load_fixtures.py
Write-Host "✅ Sample data loaded" -ForegroundColor Green
Write-Host ""

# Run tests
Write-Host "🧪 Running tests..." -ForegroundColor Cyan
pytest tests/ -v --tb=short -q
Write-Host "✅ Tests passed" -ForegroundColor Green
Write-Host ""

Write-Host "===========================================" -ForegroundColor Green
Write-Host "✅ Setup complete!" -ForegroundColor Green
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Cyan
Write-Host "1. Edit .env and add your Discord bot token"
Write-Host "2. Run the bot: python src/bot/main.py"
Write-Host ""
Write-Host "Available commands:" -ForegroundColor Cyan
Write-Host "  python -m pytest        - Run tests"
Write-Host "  python src/bot/main.py  - Run the bot"
Write-Host ""
Write-Host "Need help? Check README.md or CONTRIBUTING.md" -ForegroundColor Cyan
Write-Host "===========================================" -ForegroundColor Green
