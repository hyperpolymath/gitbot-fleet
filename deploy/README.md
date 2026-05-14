# Fleet Deployment Guide

Production deployment automation for gitbot-fleet.

## Quick Start

### Option 1: Docker Compose (Recommended)

```bash
# Deploy with Docker
sudo DEPLOYMENT_MODE=docker ./deploy/deploy.sh deploy

# Check status
./deploy/deploy.sh status

# View logs
docker compose logs -f dashboard
```

### Option 2: Systemd (Bare Metal)

```bash
# Deploy with systemd
sudo ./deploy/deploy.sh deploy

# Check status
sudo systemctl status fleet-dashboard

# View logs
sudo journalctl -u fleet-dashboard -f
```

## Deployment Modes

### Docker Compose

**Pros:**
- Isolated environment
- Easy rollback
- Portable across systems
- No system-level dependencies

**Cons:**
- Requires Docker installed
- Slight performance overhead
- Extra layer of abstraction

**Use when:**
- You want containerized deployment
- Testing or staging environments
- Multi-environment deployments

### Systemd

**Pros:**
- Native system integration
- Lower overhead
- Direct access to system resources
- Better for single-server production

**Cons:**
- Requires system setup
- User/group management
- More complex uninstall

**Use when:**
- Production bare-metal deployment
- Maximum performance needed
- Long-running production service

## Configuration

### Environment Variables

Create `/etc/fleet/dashboard.env` (systemd) or `.env` (Docker):

```bash
# Repository Configuration
FLEET_REPO_PATH=/srv/repositories
FLEET_REPO_NAME=my-project

# Logging
RUST_LOG=fleet_dashboard=info,tower_http=debug

# Server (optional)
FLEET_BIND_ADDR=127.0.0.1:8080
```

### Repository Setup

The fleet monitors a repository at `FLEET_REPO_PATH`. Ensure:

1. **Read access**: Fleet user must read the repository
2. **Git repository**: Must be a valid git repo
3. **Persistent storage**: Don't use tmpfs or volatile storage

```bash
# Example setup
sudo mkdir -p /srv/repositories
sudo git clone https://github.com/org/repo /srv/repositories/my-repo
sudo chown -R fleet:fleet /srv/repositories
```

## Directory Structure

```
/opt/gitbot-fleet/          # Installation
├── dashboard/
│   ├── fleet-dashboard     # Binary
│   └── static/             # Web UI
└── shared-context/
    └── fleet               # CLI binary

/etc/fleet/                 # Configuration
└── dashboard.env           # Environment file

/var/lib/fleet/             # Data
├── context/                # Fleet context data
└── logs/                   # Application logs
```

## Security Hardening

### Systemd Service

The systemd service includes extensive security hardening:

- **No new privileges**: Prevents privilege escalation
- **Private /tmp**: Isolated temporary directory
- **Protected system**: Read-only system directories
- **Limited syscalls**: Restricted system call access
- **Memory protection**: W^X enforcement
- **Namespace isolation**: Restricted namespaces
- **Resource limits**: CPU and memory quotas

### Network Exposure

By default, the dashboard binds to `127.0.0.1:8080` (localhost only).

**For external access**, use a reverse proxy:

#### Nginx

```nginx
server {
    listen 443 ssl http2;
    server_name fleet.example.com;

    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

#### Caddy

```
fleet.example.com {
    reverse_proxy localhost:8080
}
```

### Firewall

```bash
# Allow only from reverse proxy
sudo ufw allow from 127.0.0.1 to any port 8080

# Or specific subnet
sudo ufw allow from 10.0.0.0/24 to any port 8080
```

## Health Monitoring

### Systemd Watchdog

The service includes systemd watchdog support:

```bash
# Check watchdog status
systemctl show fleet-dashboard | grep Watchdog

# Manually trigger watchdog
systemd-notify WATCHDOG=1
```

### Health Endpoint

```bash
# Check health via API
curl http://localhost:8080/api/health

# Monitor in watch mode
watch -n 5 'curl -s http://localhost:8080/api/health | jq .status'
```

### Integration with Monitoring

#### Prometheus

```yaml
scrape_configs:
  - job_name: 'fleet-dashboard'
    static_configs:
      - targets: ['localhost:8080']
    metrics_path: '/api/health'
```

#### Grafana Alert

```yaml
- alert: FleetUnhealthy
  expr: fleet_health_score < 50
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "Fleet health degraded"
```

## Backup and Recovery

### Backup Fleet Context

```bash
# Systemd
sudo tar czf fleet-backup-$(date +%Y%m%d).tar.gz \
    /var/lib/fleet/context \
    /etc/fleet

# Docker
docker compose exec dashboard tar czf - /var/lib/fleet > \
    fleet-backup-$(date +%Y%m%d).tar.gz
```

### Restore

```bash
# Systemd
sudo systemctl stop fleet-dashboard
sudo tar xzf fleet-backup-YYYYMMDD.tar.gz -C /
sudo systemctl start fleet-dashboard

# Docker
docker compose down
docker compose up -d
docker compose exec dashboard tar xzf - -C / < fleet-backup-YYYYMMDD.tar.gz
```

## Upgrading

### Docker

```bash
# Pull latest
git pull origin main

# Rebuild and restart
docker compose down
docker compose up -d --build
```

### Systemd

```bash
# Stop service
sudo systemctl stop fleet-dashboard

# Build new version
cd /path/to/gitbot-fleet
git pull origin main
cargo build --release -p fleet-dashboard

# Update binary
sudo cp target/release/fleet-dashboard /opt/gitbot-fleet/dashboard/

# Restart service
sudo systemctl start fleet-dashboard
```

## Troubleshooting

### Dashboard Won't Start

```bash
# Check logs
sudo journalctl -u fleet-dashboard -n 50

# Verify binary
/opt/gitbot-fleet/dashboard/fleet-dashboard --help

# Check permissions
ls -la /var/lib/fleet
```

### WebSocket Connection Fails

```bash
# Check if dashboard is listening
sudo ss -tlnp | grep 8080

# Test WebSocket locally
websocat ws://localhost:8080/ws

# Check firewall
sudo ufw status
```

### High Memory Usage

```bash
# Check current usage
systemctl status fleet-dashboard | grep Memory

# Adjust limits in service file
sudo systemctl edit fleet-dashboard
```

Add:
```ini
[Service]
MemoryMax=256M
```

### Port Already in Use

```bash
# Find process using port 8080
sudo lsof -i :8080

# Change port in config
# Edit /etc/fleet/dashboard.env
FLEET_BIND_ADDR=127.0.0.1:8081
```

## Uninstall

```bash
# Complete removal
sudo ./deploy/deploy.sh uninstall

# Manual cleanup if needed
sudo userdel fleet
sudo groupdel fleet
sudo rm -rf /opt/gitbot-fleet /etc/fleet /var/lib/fleet
```

## Production Checklist

- [ ] Deploy behind reverse proxy (nginx/Caddy)
- [ ] Enable HTTPS/TLS
- [ ] Configure firewall rules
- [ ] Set up monitoring/alerting
- [ ] Configure automated backups
- [ ] Set log rotation
- [ ] Test health checks
- [ ] Document repository locations
- [ ] Plan upgrade strategy
- [ ] Test disaster recovery

## Performance Tuning

### For High-Traffic Deployments

```bash
# Increase file descriptors
echo "fs.file-max = 65536" | sudo tee -a /etc/sysctl.conf
sudo sysctl -p

# Adjust service limits
sudo systemctl edit fleet-dashboard
```

```ini
[Service]
LimitNOFILE=65536
MemoryMax=1G
CPUQuota=200%
```

### Database Tuning

For large repositories with many findings, consider:

- Increase memory limits
- Add Redis cache layer
- Implement pagination
- Archive old sessions

## Support

- Issues: https://github.com/hyperpolymath/gitbot-fleet/issues
- Docs: https://github.com/hyperpolymath/gitbot-fleet
- License: PMPL-1.0-or-later

## License

SPDX-License-Identifier: PMPL-1.0-or-later
