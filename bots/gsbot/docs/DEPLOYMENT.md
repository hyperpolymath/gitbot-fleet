# Deployment Guide

This guide covers different deployment options for the Garment Sustainability Bot.

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

- Python 3.9 or higher
- Discord Bot Token
- Git (for cloning repository)

### Recommended

- Docker and Docker Compose (for containerized deployment)
- PostgreSQL (for production)
- Redis (for distributed caching in production)

## Local Development

### Quick Start

Use the quick start script:

**Linux/Mac:**
```bash
./scripts/quick_start.sh
```

**Windows:**
```powershell
.\scripts\quick_start.ps1
```

### Manual Setup

1. **Clone repository:**
```bash
git clone https://github.com/Hyperpolymath/gsbot.git
cd gsbot
```

2. **Create virtual environment:**
```bash
python3 -m venv venv
source venv/bin/activate  # Linux/Mac
# or
venv\Scripts\activate  # Windows
```

3. **Install dependencies:**
```bash
pip install -r requirements.txt
pip install -e .
```

4. **Configure environment:**
```bash
cp .env.example .env
# Edit .env and add your Discord token
```

5. **Initialize database:**
```bash
python scripts/load_fixtures.py
```

6. **Run bot:**
```bash
python src/bot/main.py
```

## Docker Deployment

### Using Docker Compose (Recommended)

1. **Configure environment:**
```bash
cp .env.example .env
# Edit .env with your Discord token
```

2. **Build and run:**
```bash
docker-compose up -d
```

3. **View logs:**
```bash
docker-compose logs -f bot
```

4. **Stop bot:**
```bash
docker-compose down
```

### Using Docker only

1. **Build image:**
```bash
docker build -t gsbot .
```

2. **Run container:**
```bash
docker run -d \
  --name gsbot \
  --env-file .env \
  -v $(pwd)/data:/app/data \
  gsbot
```

3. **View logs:**
```bash
docker logs -f gsbot
```

### Docker with PostgreSQL

Uncomment the PostgreSQL section in `docker-compose.yml`:

```yaml
services:
  bot:
    environment:
      - DATABASE_URL=postgresql://gsbot:password@db:5432/gsbot
    depends_on:
      - db

  db:
    image: postgres:15-alpine
    environment:
      - POSTGRES_DB=gsbot
      - POSTGRES_USER=gsbot
      - POSTGRES_PASSWORD=password
    volumes:
      - postgres_data:/var/lib/postgresql/data

volumes:
  postgres_data:
```

Then run:
```bash
docker-compose up -d
```

## Cloud Deployment

### Heroku

1. **Create Heroku app:**
```bash
heroku create your-app-name
```

2. **Add PostgreSQL:**
```bash
heroku addons:create heroku-postgresql:hobby-dev
```

3. **Set environment variables:**
```bash
heroku config:set DISCORD_TOKEN=your_token_here
heroku config:set DISCORD_PREFIX=!
```

4. **Create Procfile:**
```
worker: python src/bot/main.py
```

5. **Deploy:**
```bash
git push heroku main
```

6. **Scale worker:**
```bash
heroku ps:scale worker=1
```

### AWS (EC2)

1. **Launch EC2 instance** (Ubuntu 22.04 recommended)

2. **SSH into instance:**
```bash
ssh -i your-key.pem ubuntu@your-instance-ip
```

3. **Install dependencies:**
```bash
sudo apt update
sudo apt install -y python3.11 python3-pip git
```

4. **Clone and setup:**
```bash
git clone https://github.com/Hyperpolymath/gsbot.git
cd gsbot
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
pip install -e .
```

5. **Configure environment:**
```bash
cp .env.example .env
nano .env  # Add your Discord token
```

6. **Setup systemd service:**

Create `/etc/systemd/system/gsbot.service`:

```ini
[Unit]
Description=Garment Sustainability Bot
After=network.target

[Service]
Type=simple
User=ubuntu
WorkingDirectory=/home/ubuntu/gsbot
Environment=PATH=/home/ubuntu/gsbot/venv/bin
ExecStart=/home/ubuntu/gsbot/venv/bin/python src/bot/main.py
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

7. **Start service:**
```bash
sudo systemctl daemon-reload
sudo systemctl enable gsbot
sudo systemctl start gsbot
```

8. **Check status:**
```bash
sudo systemctl status gsbot
```

### DigitalOcean

Similar to AWS EC2, or use Docker:

1. **Create Droplet** (Docker pre-installed)

2. **SSH and clone:**
```bash
ssh root@your-droplet-ip
git clone https://github.com/Hyperpolymath/gsbot.git
cd gsbot
```

3. **Configure and run:**
```bash
cp .env.example .env
nano .env  # Add Discord token
docker-compose up -d
```

### Railway / Render

1. **Connect GitHub repository**

2. **Set environment variables** in dashboard:
   - `DISCORD_TOKEN`
   - `DATABASE_URL` (provided by service)

3. **Deploy** automatically on git push

## Production Considerations

### Environment Variables

Essential for production:

```env
# Bot
DISCORD_TOKEN=your_production_token
DISCORD_PREFIX=!
DISCORD_ADMIN_IDS=comma,separated,ids

# Database
DATABASE_URL=postgresql://user:pass@host:5432/dbname
DATABASE_ECHO=False

# Caching
ENABLE_CACHING=True
CACHE_TTL=3600
CACHE_MAXSIZE=5000

# Logging
LOG_LEVEL=INFO
LOG_FILE=/var/log/gsbot/bot.log
```

### Database

#### PostgreSQL Setup

1. **Install PostgreSQL:**
```bash
sudo apt install postgresql postgresql-contrib
```

2. **Create database:**
```sql
CREATE DATABASE gsbot;
CREATE USER gsbot WITH PASSWORD 'secure_password';
GRANT ALL PRIVILEGES ON DATABASE gsbot TO gsbot;
```

3. **Update connection string:**
```env
DATABASE_URL=postgresql://gsbot:secure_password@localhost:5432/gsbot
```

4. **Run migrations:**
```bash
alembic upgrade head
```

#### Backup Strategy

Automated backups:

```bash
# Add to crontab: crontab -e
0 2 * * * /path/to/gsbot/venv/bin/python /path/to/gsbot/scripts/backup_db.py
```

Or use PostgreSQL pg_dump:

```bash
pg_dump -U gsbot -d gsbot -F c -f backup.dump
```

### Security

1. **Use environment variables** for secrets
2. **Enable SSL/TLS** for database connections
3. **Regular updates:**
   ```bash
   pip install --upgrade -r requirements.txt
   ```
4. **Security scanning:**
   ```bash
   pip install safety
   safety check
   ```

### Performance

#### Caching

For distributed setups, use Redis:

```python
# In config/settings.py
REDIS_URL = os.getenv("REDIS_URL", "redis://localhost:6379")
```

#### Database Connection Pooling

For PostgreSQL, use connection pooling:

```python
from sqlalchemy.pool import QueuePool

engine = create_engine(
    DATABASE_URL,
    poolclass=QueuePool,
    pool_size=10,
    max_overflow=20
)
```

### Logging

1. **File logging:**
```env
LOG_FILE=/var/log/gsbot/bot.log
```

2. **Log rotation:**

Create `/etc/logrotate.d/gsbot`:

```
/var/log/gsbot/bot.log {
    daily
    rotate 14
    compress
    delaycompress
    notifempty
    create 0640 ubuntu ubuntu
}
```

### Resource Limits

Set resource limits in systemd service:

```ini
[Service]
MemoryLimit=512M
CPUQuota=50%
```

Or in Docker:

```yaml
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

Monitor bot status:

```bash
# Check if process is running
systemctl status gsbot  # systemd
docker ps  # Docker

# Check logs
tail -f /var/log/gsbot/bot.log
docker logs -f gsbot
```

### Metrics

Track important metrics:

- Command usage
- Response times
- Error rates
- Database query performance
- Memory usage
- Cache hit rates

Consider using:
- Prometheus + Grafana
- DataDog
- New Relic
- Sentry (error tracking)

### Alerts

Set up alerts for:
- Bot downtime
- High error rates
- High memory usage
- Database connection issues

## Troubleshooting

### Bot won't start

1. **Check logs:**
```bash
tail -f /var/log/gsbot/bot.log
docker logs gsbot
```

2. **Verify token:**
```bash
echo $DISCORD_TOKEN
```

3. **Check permissions:**
```bash
ls -la src/bot/main.py
```

### Database errors

1. **Check connection:**
```bash
psql $DATABASE_URL
```

2. **Run migrations:**
```bash
alembic upgrade head
```

3. **Check permissions:**
```sql
\l  -- List databases
\du -- List users
```

### High memory usage

1. **Check process:**
```bash
ps aux | grep python
```

2. **Adjust cache settings:**
```env
CACHE_MAXSIZE=1000  # Reduce if needed
```

3. **Restart bot:**
```bash
systemctl restart gsbot
docker-compose restart bot
```

### Commands not responding

1. **Check bot status** in Discord
2. **Verify intents** are enabled
3. **Check command prefix** matches
4. **Review error logs**

### Performance issues

1. **Enable caching** if disabled
2. **Check database** performance
3. **Review query logs**
4. **Consider upgrading** resources

## Scaling

### Horizontal Scaling

For multiple bot instances:

1. Use shared PostgreSQL database
2. Use Redis for caching
3. Load balance across instances

### Vertical Scaling

Increase resources:
- More CPU cores
- More RAM
- Faster storage (SSD)
- Better network

## Maintenance

### Regular Tasks

- **Daily**: Monitor logs and errors
- **Weekly**: Review performance metrics
- **Monthly**: Update dependencies, backup database
- **Quarterly**: Security audit, performance review

### Updates

1. **Test in development** first
2. **Backup database** before updating
3. **Update dependencies:**
   ```bash
   pip install --upgrade -r requirements.txt
   ```
4. **Run migrations** if needed
5. **Restart bot**
6. **Monitor for issues**

### Rollback Procedure

If update fails:

1. **Stop bot**
2. **Restore database** from backup
3. **Revert code** to previous version
4. **Restart bot**
5. **Investigate issues**

## Support

- GitHub Issues: https://github.com/Hyperpolymath/gsbot/issues
- Documentation: README.md, CONTRIBUTING.md
- Architecture: docs/ARCHITECTURE.md
- API docs: docs/API.md
