#!/bin/bash

# Quick start script for Garment Sustainability Bot
# This script sets up the development environment

set -e

echo "🌱 Garment Sustainability Bot - Quick Start"
echo "==========================================="
echo ""

# Check Python version
echo "📋 Checking Python version..."
python_version=$(python3 --version 2>&1 | awk '{print $2}')
required_version="3.9"

if ! python3 -c "import sys; exit(0 if sys.version_info >= (3, 9) else 1)"; then
    echo "❌ Error: Python 3.9 or higher is required"
    echo "   Current version: $python_version"
    exit 1
fi

echo "✅ Python $python_version detected"
echo ""

# Create virtual environment
if [ ! -d "venv" ]; then
    echo "📦 Creating virtual environment..."
    python3 -m venv venv
    echo "✅ Virtual environment created"
else
    echo "✅ Virtual environment already exists"
fi
echo ""

# Activate virtual environment
echo "🔄 Activating virtual environment..."
source venv/bin/activate
echo "✅ Virtual environment activated"
echo ""

# Upgrade pip
echo "⬆️  Upgrading pip..."
pip install --upgrade pip -q
echo "✅ Pip upgraded"
echo ""

# Install dependencies
echo "📚 Installing dependencies..."
pip install -r requirements.txt -q
echo "✅ Dependencies installed"
echo ""

# Install package in development mode
echo "🔧 Installing package in development mode..."
pip install -e . -q
echo "✅ Package installed"
echo ""

# Check if .env exists
if [ ! -f ".env" ]; then
    echo "📝 Creating .env file..."
    cp .env.example .env
    echo "✅ .env file created"
    echo ""
    echo "⚠️  IMPORTANT: Edit .env and add your Discord bot token!"
    echo "   DISCORD_TOKEN=your_token_here"
    echo ""
else
    echo "✅ .env file already exists"
    echo ""
fi

# Create data directory
if [ ! -d "data" ]; then
    echo "📁 Creating data directory..."
    mkdir -p data
    echo "✅ Data directory created"
fi

# Load sample data
echo "📊 Loading sample data..."
python scripts/load_fixtures.py
echo "✅ Sample data loaded"
echo ""

# Run tests
echo "🧪 Running tests..."
pytest tests/ -v --tb=short -q
echo "✅ Tests passed"
echo ""

echo "=========================================="
echo "✅ Setup complete!"
echo ""
echo "Next steps:"
echo "1. Edit .env and add your Discord bot token"
echo "2. Run the bot: python src/bot/main.py"
echo "   Or use: make run"
echo ""
echo "Available commands:"
echo "  make test      - Run tests"
echo "  make lint      - Check code quality"
echo "  make format    - Format code"
echo "  make run       - Run the bot"
echo ""
echo "Need help? Check README.md or CONTRIBUTING.md"
echo "=========================================="
