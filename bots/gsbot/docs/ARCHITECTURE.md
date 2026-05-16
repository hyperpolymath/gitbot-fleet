# Architecture Documentation

## Overview

The Garment Sustainability Bot is a **Rust** application (ported from a
now-deleted Python prototype, behaviour preserved). It is built with a
modular architecture that separates Discord wiring, the command surface, a
service/persistence layer, and a pure correctness-critical scoring kernel.

Stack: `poise` 0.6 over `serenity` 0.12 (Discord), `sqlx` 0.8 + SQLite
(persistence), `tokio` (async), `tracing` (logging), `dotenvy` (config),
`anyhow`/`thiserror` (errors).

## System Architecture

```text
┌─────────────────────────────────────────────────────────────┐
│                       Discord Platform                       │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│              Bot Layer (poise 0.6 / serenity 0.12)           │
│  src/bot.rs (intents, presence, on_error mapping)            │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐      │
│  │sustain-  │  │materials │  │  brands  │  │  user_   │      │
│  │ability.rs│  │   .rs    │  │   .rs    │  │commands  │      │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘      │
│                ┌──────────┐  commands/ (one module per cog)  │
│                │ admin.rs │  + mod.rs (registry, is_admin)   │
│                └──────────┘                                  │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│   domain.rs — PURE scoring kernel (the SPARK seam)           │
│   no I/O · total · stable C ABI in `mod ffi`                 │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│        Service Layer (src/services.rs, sustainability.rs)    │
│  query/service logic over sqlx · analyzer helpers            │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│        Data Layer (sqlx 0.8, src/models.rs, src/db.rs)       │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐      │
│  │materials │  │ garments │  │  brands  │  │  users   │      │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘      │
│  migrations/0001_init.sql applied via sqlx::migrate!         │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                   SQLite Database (only)                     │
└─────────────────────────────────────────────────────────────┘
```

## Component Details

### Bot Layer

**Location**: `src/bot.rs`, `src/commands/`

Handles Discord interactions via poise/serenity:

- **`bot.rs`**: builds the `poise::Framework`, sets gateway intents
  (GUILDS, GUILD_MESSAGES, MESSAGE_CONTENT, GUILD_MEMBERS), presence, and
  maps framework errors to user-facing messages (`on_error`).
- **`commands/`**: one module per former discord.py cog —
  `sustainability.rs`, `materials.rs`, `brands.rs`, `user_commands.rs`,
  `admin.rs`. `commands/mod.rs` holds the command registry (`all()`),
  embed colour helpers, and the `is_admin` gate.

**Key features**:
- `async`/`.await` (tokio) for Discord interactions
- Error handling and `tracing` logging
- Admin permission checks (`is_admin`: ID in `DISCORD_ADMIN_IDS` or guild
  Administrator permission)
- Message formatting with serenity embeds

### Domain Kernel — the SPARK Seam

**Location**: `src/domain.rs`

A **pure, total, correctness-critical** scoring kernel: no I/O, no
allocation in the numeric core, total over its documented domain. It holds
all the scoring formulas (`material_overall_score`, `grade`,
`lifespan_multiplier`, `garment_sustainability_score`,
`environmental_impact`, `add_points`, `rank`, `brand_rating_summary`,
`impact_category`, `material_recommendation`).

`mod ffi` exposes the numeric core under a **stable C ABI**:
`gsbot_material_overall_score`, `gsbot_lifespan_multiplier`,
`gsbot_level_for_points` (`#[no_mangle] extern "C"`). This is the
architecture's **verification seam**: a formally-verified SPARK/Ada module
can export the same symbols and be linked in place of the Rust bodies via
the hyperpolymath Zig-FFI / Idris2-ABI pattern, with no caller changes —
callers go through the safe Rust wrappers, so substitution is transparent.

### Service Layer

**Location**: `src/services.rs`, `src/sustainability.rs`

Business logic and data access:

- **`services.rs`**: typed `sqlx` queries — get-by-name, search,
  alternatives, top-rated, leaderboard, user get-or-create/update.
- **`sustainability.rs`**: analyzer helpers (tips, impact category text).

### Data Layer

**Location**: `src/models.rs`, `src/db.rs`, `migrations/`

- **`models.rs`**: row structs for materials, garments, brands, users.
- **`db.rs`**: opens the SQLite pool (`SqlitePoolOptions`, max 5
  connections) and applies migrations via `sqlx::migrate!`.
- **`migrations/0001_init.sql`**: schema, embedded at compile time and
  applied automatically at startup. **SQLite only — no Postgres.**

### Configuration

**Location**: `src/config.rs`

- `.env` loaded via `dotenvy`
- Environment variable loading with defaults and validation
- Feature flags (`ENABLE_CACHING`, `ENABLE_ANALYTICS`)
- `validate()` fails fast if `DISCORD_TOKEN` is missing

### Utilities

- **`src/logging.rs`**: `tracing` console output + optional file logging
  (`tracing-appender`)
- **`src/cache.rs`**: in-process cache for performance
- **`src/fixtures.rs`**: sample-data loader
- **`src/bin/`**: `gsbot-load-fixtures`, `gsbot-export-data`,
  `gsbot-backup-db`

## Data Flow

### Command Execution Flow

```text
1. User sends Discord command
   ↓
2. serenity gateway receives message
   ↓
3. poise routes to the matching command (src/commands/*.rs)
   ↓
4. Command parses/validates arguments
   ↓
5. Service layer (services.rs) queries via sqlx
   ↓
6. domain.rs computes scores (pure kernel)
   ↓
7. Results formatted into a serenity embed
   ↓
8. User points updated (add_points kernel + users table)
   ↓
9. Response sent via poise reply
```

### Database Query Flow

```text
Command → services.rs → sqlx → SQLite
                 ↓
               cache.rs (if ENABLE_CACHING)
                 ↓
               Result
```

## Design Patterns

### Separation of Concerns

- **Presentation**: serenity embeds and formatting (`commands/`)
- **Correctness core**: pure kernel (`domain.rs`)
- **Business/data access**: `services.rs`, `models.rs`, `db.rs`

### Shared State

A `Data { db: SqlitePool, config: Config }` is constructed once in
`Framework::setup` and handed to every command via `Context`:

```rust
let db = &ctx.data().db;
let prefix = &ctx.data().config.discord_prefix;
```

### Service Functions

Data access is centralised in service types:

```rust
MaterialService::get_by_name(db, "cotton").await?;
GarmentService::get_alternatives(db, &garment).await?;
```

### Command Attributes

Commands and their aliases are declared with the poise attribute macro:

```rust
#[poise::command(prefix_command, aliases("sus", "score"))]
pub async fn sustainability(ctx: Context<'_>, garment: String)
    -> Result<(), Error> { /* ... */ }
```

## Database Schema

### Entity Relationships

```text
materials ◄────────┐
    │              │ Many-to-Many (garment_materials)
    └────────► garments

brands  (standalone)
users   (standalone — tracks Discord users)
```

### Key Tables

- `materials`: material definitions and metrics
- `garments`: garment types and properties
- `garment_materials`: association table (garment_id, material_id, percentage)
- `brands`: brand information and ratings
- `users`: user profiles and gamification

(See `migrations/0001_init.sql` for the exact columns and indexes.)

## Scalability Considerations

### Current Architecture (Small Scale)

- SQLite database (the only supported backend)
- In-process caching
- Single bot instance

### Future Enhancements

- Redis caching
- Multiple bot instances with sharding
- Web dashboard with read API
- Metrics and monitoring
- Formal verification of the `domain.rs` kernel in SPARK/Ada, linked
  through the existing C-ABI seam

## Testing Strategy

### Unit Tests

In-crate `#[cfg(test)]` tests in isolation — notably the `domain.rs`
kernel tests that pin the scoring formulas (SPARK-ready).

### Integration / All Targets

`cargo test --all-targets` exercises binaries and library; tests use
`tempfile` / in-memory SQLite for fast, isolated runs.

## Configuration Management

### Environment Variables

`DISCORD_TOKEN` (required), `DISCORD_PREFIX`, `DISCORD_ADMIN_IDS`,
`DATABASE_URL` (SQLite only), `DATABASE_ECHO`, `CACHE_TTL`,
`CACHE_MAXSIZE`, `LOG_LEVEL`, `LOG_FILE`, `API_TIMEOUT`,
`API_RETRY_COUNT`, `ENABLE_CACHING`, `ENABLE_ANALYTICS`. See
`src/config.rs`.

### Validation

`Config::validate()` runs on startup to fail fast (missing token, data
directories).

## Error Handling

### Levels

1. **Command level**: user-friendly messages (the `on_error` mapping in
   `bot.rs`)
2. **Application level**: `anyhow::Error` (aliased as `Error`); typed
   errors via `thiserror`
3. **Database level**: `sqlx` result propagation

### Logging

- `tracing` console output
- Optional file output via `tracing-appender` (`LOG_FILE`)
- Level via `LOG_LEVEL`

## Security

### Input Validation

- User inputs validated by command argument parsing
- SQL injection prevented via `sqlx` bind parameters
- Admin command permission checks (`commands::is_admin`)

### Secrets Management

- Secrets via environment variables / `.env` (git-excluded)
- No hardcoded credentials

## Performance

### Caching

- In-process cache (`src/cache.rs`), TTL/size configurable

### Database

- Indexes on frequently queried fields (see migration)
- `sqlx` connection pool (max 5 connections)

## Extension Points

### Adding New Commands

1. Add a `#[poise::command]` fn in `src/commands/`
2. Register it in `commands::all()` (`src/commands/mod.rs`)
3. Add service methods if needed; update docs and tests

### Adding New Models

1. Add a migration under `migrations/`
2. Define the row struct in `src/models.rs`
3. Add service methods in `src/services.rs`
4. Add fixtures in `src/fixtures.rs`; add tests

### Adding External APIs

1. Add a service in `src/services.rs`
2. Add configuration settings in `src/config.rs`
3. Implement caching and rate limiting

## Deployment

### Container

- Multi-stage `Containerfile` (rust builder → debian-bookworm-slim
  runtime), non-root `gsbot` user, `ENTRYPOINT ["/usr/local/bin/gsbot"]`
- `docker-compose.yml` builds with `dockerfile: Containerfile`; SQLite
  only

### CI/CD

- Fleet-level GitHub Actions, including the Hypatia security scan that
  self-scans this repository
- `cargo test --all-targets`, `cargo clippy --all-targets -- -D warnings`,
  `cargo fmt --all -- --check`

## Monitoring

### Logging

- Structured `tracing` logging, level per `LOG_LEVEL`, optional file
  rotation via `tracing-appender`

### Metrics (Future)

- Command usage statistics, response times, error rates

## Documentation

- Rustdoc comments, this architecture doc, the API reference, deployment
  guide, and `CLAUDE.md` for AI agents

## Future Considerations

1. **Formal verification**: prove the `domain.rs` numeric core in SPARK/Ada
   and link it through the existing C-ABI seam (no caller changes)
2. **Sharding**: scale across multiple gateway shards
3. **Read API**: optional web/JSON read surface
4. **Richer analytics**: opt-in usage metrics
