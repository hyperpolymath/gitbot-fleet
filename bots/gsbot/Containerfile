# Garment Sustainability Bot Dockerfile
FROM python:3.11-slim

# Set working directory
WORKDIR /app

# Set environment variables
ENV PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1

# Install system dependencies
RUN apt-get update && apk add --no-cache -y --no-install-recommends \
    gcc \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements first for better caching
COPY requirements.txt .

# Install Python dependencies
RUN pip install --upgrade pip && \
    pip install -r requirements.txt

# Copy project files
COPY . .

# Install the package
RUN pip install -e .

# Create data directory
RUN mkdir -p /app/data

# Set up volume for persistent data
VOLUME ["/app/data"]

# Expose health check port (if implemented)
EXPOSE 8080

# Run the bot
CMD ["python", "src/bot/main.py"]
