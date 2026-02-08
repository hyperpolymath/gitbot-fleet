# Fleet Dashboard

Real-time web dashboard for monitoring and controlling the gitbot-fleet.

## Features

- **Real-time Health Monitoring**: Live health status with WebSocket updates every 5 seconds
- **Fleet Overview**: System metrics, bot status, and findings at a glance
- **Health Scoring**: 0-100 health score with automatic status determination
- **Alert System**: Real-time alerts for failures, anomalies, and issues
- **Bot Status**: Individual bot tracking with execution state
- **Findings Explorer**: Browse and filter findings from all bots
- **Multi-format Reports**: Export reports as Markdown, JSON, or HTML

## Quick Start

```bash
# Set environment variables (optional)
export FLEET_REPO_PATH=/path/to/repo
export FLEET_REPO_NAME=my-repo

# Start the dashboard server
cargo run --bin fleet-dashboard

# Open in browser
open http://localhost:8080
```

## API Endpoints

### Health Status
```
GET /api/health
```
Returns comprehensive fleet health data including:
- Overall health status (Healthy/Degraded/Unhealthy/Critical)
- Health score (0-100)
- Bot health status
- Tier health metrics
- System metrics
- Active alerts

### Fleet Status
```
GET /api/status
```
Returns session summary with bot counts, findings, and release status.

### Findings
```
GET /api/findings?bot=Rhodibot&severity=Error&limit=50
```
Query parameters:
- `bot` - Filter by bot name
- `severity` - Filter by severity (Error, Warning, Info, Suggestion)
- `limit` - Maximum results (default: 100, max: 1000)

### Reports
```
GET /api/report/markdown
GET /api/report/json
GET /api/report/html
```
Generate fleet report in specified format.

### Bots
```
GET /api/bots
```
List all registered bots with execution status.

### WebSocket
```
WS /ws
```
Real-time health updates pushed every 5 seconds.

## Architecture

The dashboard is built with:
- **Backend**: Rust + Axum web framework
- **Frontend**: Vanilla HTML/CSS/JavaScript
- **Real-time**: WebSocket for live updates
- **Storage**: Shared context from gitbot-shared-context

## Configuration

Environment variables:
- `FLEET_REPO_PATH` - Path to repository being monitored (default: ".")
- `FLEET_REPO_NAME` - Repository name (default: "unknown")
- `RUST_LOG` - Logging level (e.g., "fleet_dashboard=debug")

## Development

```bash
# Build
cargo build

# Run with logging
RUST_LOG=fleet_dashboard=debug cargo run

# Run tests
cargo test
```

## Security

The dashboard binds to `127.0.0.1:8080` (localhost only) by default. To expose externally:

1. Use a reverse proxy (nginx, Caddy, Traefik)
2. Add authentication at the proxy level
3. Enable HTTPS/TLS

**Warning**: The dashboard provides read/write access to fleet operations. Do not expose to untrusted networks without authentication.

## Integration

The dashboard integrates with:
- **gitbot-shared-context**: Fleet coordination and state
- **Fleet Health System**: Real-time monitoring and alerts
- **Fleet Reporting**: Multi-format report generation

## License

SPDX-License-Identifier: PMPL-1.0-or-later
