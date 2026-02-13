// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2024-2025 hyperpolymath

# Oikos Bot Deployment Guide

## Prerequisites

### Required
- Podman or Vörðr (preferred)
- Deno 2.1+ (for local development)
- Git

### Optional (for full stack)
- ArangoDB 3.11+
- Virtuoso 7.2+
- GHC 9.4+ (for Haskell analyzer development)

## Quick Start

### 1. Build Container Image

```bash
# From repository root (uses Vörðr if available, falls back to Podman)
./containers/vordr-build.sh

# Or directly with Podman
podman build -t oikos:latest -f containers/Containerfile .
```

### 2. Run Container

```bash
# Basic run
podman run -p 3000:3000 oikos:latest

# With environment variables
podman run -p 3000:3000 \
  -e BOT_MODE=advisor \
  -e ANALYSIS_ENDPOINT=http://analyzer:8080 \
  -e GITHUB_WEBHOOK_SECRET=your-secret \
  oikos:latest
```

### 3. Verify Health

```bash
curl http://localhost:3000/health
# {"status":"healthy","mode":"advisor"}
```

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `PORT` | HTTP server port | `3000` |
| `BOT_MODE` | Operation mode (`advisor`, `consultant`, `regulator`) | `advisor` |
| `ANALYSIS_ENDPOINT` | Haskell analyzer endpoint | `http://localhost:8080/analyze` |
| `GITHUB_WEBHOOK_SECRET` | GitHub webhook secret | (none) |
| `GITLAB_WEBHOOK_SECRET` | GitLab webhook token | (none) |
| `ARANGO_URL` | ArangoDB connection URL | `http://localhost:8529` |
| `VIRTUOSO_URL` | Virtuoso SPARQL endpoint | `http://localhost:8890/sparql` |

### Bot Modes

- **Advisor**: Provides suggestions as PR comments (default, non-blocking)
- **Consultant**: Detailed analysis with confidence scores
- **Regulator**: Enforces policies, can block merges on violations

## Full Stack Deployment

### Using Podman Compose

```yaml
# compose.yaml
version: "3.9"

services:
  oikos:
    build:
      context: .
      dockerfile: containers/Containerfile
    ports:
      - "3000:3000"
    environment:
      - BOT_MODE=advisor
      - ANALYSIS_ENDPOINT=http://analyzer:8080
      - ARANGO_URL=http://arangodb:8529
      - VIRTUOSO_URL=http://virtuoso:8890/sparql
    depends_on:
      - analyzer
      - arangodb
      - virtuoso

  analyzer:
    image: docker.io/haskell:9.4-slim
    working_dir: /app
    volumes:
      - ./analyzers/code-haskell:/app
    command: cabal run oikos-analyzer -- --server --port 8080
    ports:
      - "8080:8080"

  arangodb:
    image: docker.io/arangodb:3.11
    environment:
      - ARANGO_ROOT_PASSWORD=oikos-dev
    ports:
      - "8529:8529"
    volumes:
      - arango-data:/var/lib/arangodb3

  virtuoso:
    image: docker.io/openlink/virtuoso-opensource-7:7.2
    environment:
      - DBA_PASSWORD=oikos-dev
    ports:
      - "8890:8890"
      - "1111:1111"
    volumes:
      - virtuoso-data:/database

volumes:
  arango-data:
  virtuoso-data:
```

```bash
podman-compose up -d
```

### Database Setup

#### ArangoDB

```bash
# Load schema
podman exec -it oikos-arangodb-1 arangosh \
  --server.username root \
  --server.password oikos-dev \
  < database/arango/schema.js
```

#### Virtuoso

```bash
# Load ontology
podman exec -it oikos-virtuoso-1 isql 1111 dba oikos-dev exec="
  DB.DBA.TTLP_MT(file_to_string_output('/database/ontology.ttl'),
                 '', 'http://oikos-bot.dev/ontology');
"
```

## GitHub App Setup

### Option A: Manifest Flow (Recommended)

The fastest way to register the GitHub App using the manifest file:

1. **Start the manifest flow server** (one-time setup):

   ```bash
   cd bot-integration
   deno run --allow-net --allow-env src/manifest-flow.res.js
   ```

2. **Or register manually via GitHub**:

   Visit: `https://github.com/settings/apps/new`

   Copy the contents of [`.github/app.yml`](.github/app.yml) and paste into the
   manifest field, or use this direct link (after deploying):

   ```
   https://your-domain.com/github/manifest-flow
   ```

3. **Save the credentials** returned by GitHub:
   - `APP_ID` - Your app's numeric ID
   - `WEBHOOK_SECRET` - Auto-generated webhook secret
   - `PRIVATE_KEY` - PEM file for signing JWTs

   ```bash
   export GITHUB_APP_ID="123456"
   export GITHUB_WEBHOOK_SECRET="generated-secret"
   export GITHUB_PRIVATE_KEY_PATH="/path/to/oikos-bot.pem"
   ```

### Option B: Manual Registration

If you prefer manual setup:

1. Go to GitHub Settings → Developer settings → GitHub Apps → New GitHub App
2. Configure:
   - **Name**: Oikos Bot (your-org)
   - **Homepage URL**: `https://your-domain.com`
   - **Webhook URL**: `https://your-domain.com/webhooks/github`
   - **Webhook Secret**: Generate and save this
   - **Permissions**:
     - Repository: Contents (Read), Pull requests (Read & Write), Checks (Read & Write)
     - Organization: Members (Read)
   - **Events**: Pull request, Push

### Install the App

After registration (either option):

1. Go to your GitHub App settings
2. Click "Install App"
3. Select repositories to monitor

### Configure Oikos Bot

```bash
export GITHUB_WEBHOOK_SECRET="your-webhook-secret"
export GITHUB_APP_ID="your-app-id"
export GITHUB_PRIVATE_KEY_PATH="/path/to/private-key.pem"
```

## GitLab Integration

### 1. Configure Webhook

1. Go to Project → Settings → Webhooks
2. Add webhook:
   - **URL**: `https://your-domain.com/webhooks/gitlab`
   - **Secret token**: Generate and save this
   - **Triggers**: Merge request events, Push events

### 2. Configure Oikos Bot

```bash
export GITLAB_WEBHOOK_SECRET="your-webhook-token"
```

## Production Deployment

### Kubernetes (Helm)

```bash
# Add Helm repo (when available)
helm repo add oikos https://charts.oikos-bot.dev

# Install
helm install oikos oikos/oikos-bot \
  --set mode=advisor \
  --set github.webhookSecret=$GITHUB_WEBHOOK_SECRET \
  --set persistence.enabled=true
```

### Systemd (Bare Metal)

```ini
# /etc/systemd/system/oikos-bot.service
[Unit]
Description=Oikos Bot Code Analysis Platform
After=network.target

[Service]
Type=simple
User=oikos
Environment=PORT=3000
Environment=BOT_MODE=advisor
ExecStart=/usr/local/bin/deno run \
  --allow-net --allow-env --allow-read \
  /opt/oikos-bot/bot-integration/src/Oikos.res.js
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
```

```bash
sudo systemctl enable --now oikos-bot
```

## Monitoring

### Metrics Endpoint

```bash
curl http://localhost:3000/metrics
```

### Health Check

```bash
# Container health check built-in
podman inspect --format='{{.State.Health.Status}}' oikos

# Manual check
curl -f http://localhost:3000/health || exit 1
```

### Logging

Logs are JSON-formatted for easy parsing:

```json
{"timestamp":"2024-12-08T10:00:00.000Z","level":"info","message":"Starting Oikos Bot","data":{"port":3000,"mode":"advisor"}}
```

## Troubleshooting

### Container won't start

1. Check logs: `podman logs oikos`
2. Verify Deno permissions: ensure `--allow-net`, `--allow-env`, `--allow-read`
3. Check port availability: `ss -tlnp | grep 3000`

### Webhook not receiving events

1. Verify webhook URL is publicly accessible
2. Check webhook secret matches
3. Review GitHub/GitLab webhook delivery logs

### Analysis failing

1. Ensure Haskell analyzer is running: `curl http://localhost:8080/health`
2. Check ANALYSIS_ENDPOINT environment variable
3. Review analyzer logs

### Database connection issues

1. Verify database is running: `podman ps`
2. Check connection URLs in environment
3. Ensure database credentials are correct

## Development

### Local Development

```bash
# Bot integration (ReScript/Deno)
cd bot-integration
deno task dev

# Haskell analyzer
cd analyzers/code-haskell
cabal run oikos-analyzer -- --path /path/to/analyze

# Build ReScript
npm install  # Only for ReScript compiler
npm run build:rescript
```

### Running Tests

```bash
# Bot integration tests
cd bot-integration
deno task test

# Haskell tests
cd analyzers/code-haskell
cabal test

# Policy engine tests
cd policy-engine
python -m pytest
```

## License

AGPL-3.0-or-later - See LICENSE file for details.
