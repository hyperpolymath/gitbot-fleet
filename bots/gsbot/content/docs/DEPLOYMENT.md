+++
title = "DEPLOYMENT"
weight = 1
+++

# Deployment Guide

This guide covers deployment options for the Garment Sustainability Bot.

> Implementation: **Rust** (`poise`/`serenity`, `sqlx` + **SQLite only**).
> Ported from a now-deleted Python prototype; behaviour preserved. There is
> no Python runtime, no virtualenv, and no Postgres.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Local Development](#local-development)
- [Docker Deployment](#docker-deployment)
- [Cloud Deployment](#cloud-deployment)
- [Production Considerations](#production-considerations)
- [Monitoring](#monitoring)
- [Troubleshooting](#troubleshooting)

## Prerequisites

### Required

- A Rust toolchain (stable; `cargo`)
- Discord Bot Token
- Git (for cloning repository)

### Recommended

- https://github.com/casey/just[`just`] for the convenience recipes
- Docker / Podman (for containerised deployment)

## Local Development

### Quick Start

```bash
git clone https://github.com/hyperpolymath/gsbot.git
cd gsbot
cp .env.example .env        # then edit .env: set DISCORD_TOKEN
just init                   # build + load sample data
just run                    # run the bot
```

### Manual Setup

1. **Clone repository:**
```bash
git clone https://github.com/hyperpolymath/gsbot.git
cd gsbot
```

2. **Configure environment:**
```bash
cp .env.example .env
# Edit .env and add your Discord token
```

3. **Build:**
```bash
just build            # or: cargo build (use --release for production)
```

4. **Initialize database (optional sample data):**
```bash
just load-data        # or: cargo run --bin gsbot-load-fixtures
```
Migrations (`migrations/0001_init.sql`) are applied automatically at
startup via `sqlx::migrate!`; no separate migration command is required.

5. **Run bot:**
```bash
just run              # or: cargo run --bin gsbot
```

## Docker Deployment

### Using Docker Compose (Recommended)

1. **Configure environment:**
```bash
cp .env.example .env
# Edit .env with your Discord token
```

2. **Build and run** (compose builds with `dockerfile: Containerfile`):
```bash
docker compose up -d
```

3. **View logs:**
```bash
docker compose logs -f bot
```

4. **Stop bot:**
```bash
docker compose down
```

### Using Docker / Podman only

1. **Build image:**
```bash
docker build -t gsbot:latest -f Containerfile .
```

2. **Run container** (multi-stage image; non-root `gsbot` user;
   `ENTRYPOINT ["/usr/local/bin/gsbot"]`):
```bash
docker run -d \
  --name gsbot \
  --env-file .env \
  -v "$(pwd)/data:/app/data" \
  -v "$(pwd)/logs:/app/logs" \
  gsbot:latest
```

3. **View logs:**
```bash
docker logs -f gsbot
```

### Persistence

The SQLite database lives under `/app/data` (a declared `VOLUME`). Mount a
host directory or named volume there so data survives container restarts.
**SQLite is the only supported backend — there is no Postgres option.**

## Cloud Deployment

The bot is a single static-ish Rust binary plus a SQLite file. Any host
that can run a Linux container or a long-lived process works. Provide a
persistent volume for `data/` (the SQLite DB) and set `DISCORD_TOKEN`.

### Container hosts (Fly.io, Render, Railway, etc.)

1. Connect the repository or push the image built from `Containerfile`.
2. Set environment variables in the dashboard (at minimum `DISCORD_TOKEN`;
   optionally `DISCORD_PREFIX`, `DISCORD_ADMIN_IDS`, `LOG_FILE`).
3. Attach a persistent volume mounted at `/app/data`.
4. Deploy.

### VM (e.g. cloud Ubuntu instance)

1. **Provision a Linux VM** and SSH in.

2. **Install a Rust toolchain and git**, e.g. via rustup:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
sudo apt update && sudo apt install -y git
```

3. **Clone and build:**
```bash
git clone https://github.com/hyperpolymath/gsbot.git
cd gsbot
cargo build --release --bin gsbot
```

4. **Configure environment:**
```bash
cp .env.example .env
# edit .env: set DISCORD_TOKEN
```

5. **Set up a systemd service** — create `/etc/systemd/system/gsbot.service`:

```text
[Unit]
Description=Garment Sustainability Bot
After=network.target

[Service]
Type=simple
User=ubuntu
WorkingDirectory=/home/ubuntu/gsbot
EnvironmentFile=/home/ubuntu/gsbot/.env
ExecStart=/home/ubuntu/gsbot/target/release/gsbot
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

6. **Start the service:**
```bash
sudo systemctl daemon-reload
sudo systemctl enable gsbot
sudo systemctl start gsbot
sudo systemctl status gsbot
```

## Production Considerations

### Environment Variables

Essential for production (see `src/config.rs` for the full list):

```text
# Bot
DISCORD_TOKEN=your_production_token
DISCORD_PREFIX=!
DISCORD_ADMIN_IDS=comma,separated,ids

# Database (SQLite only)
DATABASE_URL=sqlite:///app/data/gsbot.db
DATABASE_ECHO=false

# Caching
ENABLE_CACHING=true
CACHE_TTL=3600
CACHE_MAXSIZE=5000

# Logging
LOG_LEVEL=INFO
LOG_FILE=/app/logs/gsbot.log
```

### Database

This bot uses **SQLite only**. The schema lives in
`migrations/0001_init.sql` and is applied automatically at startup via
`sqlx::migrate!` — there is no external migration tool to run.

#### Backup Strategy

Use the bundled backup binary (keeps the last 10 backups):

```bash
cargo run --bin gsbot-backup-db          # or: just backup
# restore from a backup:
cargo run --bin gsbot-backup-db restore <backup-file>
```

Schedule it from cron, e.g.:

```bash
# crontab -e
0 2 * * * cd /home/ubuntu/gsbot && ./target/release/gsbot-backup-db
```

You can also export to JSON:

```bash
cargo run --bin gsbot-export-data        # or: just export-data
```

### Security

1. **Use environment variables / `.env`** for secrets (git-excluded).
2. **Run as non-root** (the Containerfile already uses the `gsbot` user).
3. **Keep dependencies current** and audited:
   ```bash
   cargo update
   cargo audit          # or: just security
   ```
4. **Lint clean** before deploying:
   ```bash
   cargo clippy --all-targets -- -D warnings
   ```

### Performance

#### Caching

In-process cache (`src/cache.rs`); tune via `ENABLE_CACHING`, `CACHE_TTL`,
`CACHE_MAXSIZE`.

#### Database Connection Pooling

`sqlx` uses a connection pool (configured in `src/db.rs`, max 5
connections). SQLite is single-writer; keep the DB on fast local storage.

### Logging

1. **File logging:**
```text
LOG_FILE=/app/logs/gsbot.log
```

2. **Log rotation** — create `/etc/logrotate.d/gsbot`:

```text
/app/logs/gsbot.log {
    daily
    rotate 14
    compress
    delaycompress
    notifempty
    create 0640 gsbot gsbot
}
```

### Resource Limits

systemd:

```text
[Service]
MemoryMax=512M
CPUQuota=50%
```

Docker Compose:

```text
services:
  bot:
    deploy:
      resources:
        limits:
          cpus: '0.5'
          memory: 512M
```

## Monitoring

### Health Checks

```bash
# Process status
systemctl status gsbot      # systemd
docker ps                   # Docker

# Logs
tail -f /app/logs/gsbot.log
docker compose logs -f bot
```

### Metrics

Track:

- Command usage
- Response times
- Error rates
- Database query performance
- Memory usage
- Cache hit rates

Consider Prometheus + Grafana or an error-tracking service.

### Alerts

Set up alerts for bot downtime, high error rates, high memory usage, and
database access issues.

## Troubleshooting

### Bot won't start

1. **Check logs:**
```bash
tail -f /app/logs/gsbot.log
docker compose logs bot
```

2. **Verify token is set** (`DISCORD_TOKEN` is required; `Config::validate`
   fails fast if missing).

3. **Rebuild:**
```bash
cargo build --release --bin gsbot
```

### Database errors

1. **Check the DB file path** matches `DATABASE_URL`
   (`sqlite:///.../gsbot.db`); the parent directory is created on startup.
2. **Migrations** are applied automatically via `sqlx::migrate!` — inspect
   logs for migration errors.
3. **Inspect the database** with the `sqlite3` CLI if needed:
```bash
sqlite3 data/gsbot.db '.tables'
```

### High memory usage

1. **Check the process:**
```bash
ps aux | grep gsbot
```

2. **Reduce cache size:**
```text
CACHE_MAXSIZE=1000
```

3. **Restart:**
```bash
systemctl restart gsbot
docker compose restart bot
```

### Commands not responding

1. Check bot status in Discord.
2. Verify gateway intents are enabled (MESSAGE_CONTENT is required).
3. Check the command prefix matches `DISCORD_PREFIX`.
4. Review error logs.

### Performance issues

1. Ensure caching is enabled.
2. Keep the SQLite file on fast local storage.
3. Review logs at a higher `LOG_LEVEL`.
4. Consider more resources.

## Scaling

### Horizontal Scaling

SQLite is single-writer and local; horizontal scaling of writers is not
supported by design. For higher load, scale vertically or shard the bot at
the gateway level (future work).

### Vertical Scaling

- More CPU cores
- More RAM
- Faster local storage (SSD/NVMe)

## Maintenance

### Regular Tasks

- **Daily**: monitor logs and errors
- **Weekly**: review performance metrics
- **Monthly**: update dependencies, back up the database
- **Quarterly**: security audit (`cargo audit`), performance review

### Updates

1. Test in development first.
2. Back up the database (`gsbot-backup-db`).
3. Update dependencies:
   ```bash
   cargo update
   ```
4. Rebuild (`cargo build --release`); migrations apply on next startup.
5. Restart the bot and monitor.

### Rollback Procedure

If an update fails:

1. Stop the bot.
2. Restore the database from a backup
   (`gsbot-backup-db restore <file>`).
3. Revert code to the previous version and rebuild.
4. Restart the bot and investigate.

## Support

- GitHub Issues: https://github.com/hyperpolymath/gsbot/issues
- Documentation: README.adoc, CLAUDE.md
- Architecture: docs/ARCHITECTURE.md
- API docs: docs/API.md
