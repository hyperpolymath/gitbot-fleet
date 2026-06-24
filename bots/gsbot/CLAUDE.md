# CLAUDE.md — Garment Sustainability Bot (gsbot)

> **This bot is Rust/SPARK.** The hyperpolymath estate bans Python, and the
> Hypatia security scanner self-scans this repository — any banned-language
> file is a CRITICAL finding. **Never introduce Python or any banned-language
> file.** The estate Rust/SPARK standard governs over any generic template,
> including any older content that may once have lived in this file.

## Project Overview

`gsbot` is a Discord bot promoting sustainability in the garment and fashion
industry. It answers material/garment/brand sustainability queries, gives care
advice and alternatives, and gamifies engagement with points, levels and a
leaderboard.

It was ported from a now-deleted Python prototype to **Rust** (with a
designed-in SPARK seam). Behaviour is preserved; the implementation is Rust.

- Crate: `gsbot` v0.2.0
- License: `MPL-2.0` (SPDX header on every source file)
- Edition: 2021

## Technology Stack (real, not aspirational)

- **Discord framework:** `poise` 0.6 over `serenity` 0.12 (prefix commands)
- **Persistence:** `sqlx` 0.8 + **SQLite only** (no Postgres)
- **Migrations:** `migrations/0001_init.sql`, applied at startup via
  `sqlx::migrate!`
- **Async runtime:** `tokio`
- **Config / env:** `dotenvy` (`.env` loaded at startup)
- **Logging:** `tracing` + `tracing-subscriber` + `tracing-appender`
- **Errors:** `anyhow` (application) + `thiserror` (typed)
- **Misc:** `chrono`, `rand`, `serde` / `serde_json`

## Crate Layout

```
src/
├── main.rs              # gsbot binary entry point
├── bot.rs               # poise/serenity wiring, intents, error mapping
├── lib.rs               # library root: Data, Context, Error type aliases
├── config.rs            # Config::from_env() — env vars + defaults
├── db.rs                # SQLite pool + sqlx::migrate!
├── domain.rs            # PURE scoring kernel — the SPARK seam (see below)
├── models.rs            # row structs
├── services.rs          # query/service layer
├── sustainability.rs    # analyzer helpers (tips, categories)
├── cache.rs             # in-process cache
├── fixtures.rs          # sample-data loader
├── logging.rs           # tracing init
├── commands/            # one module per former cog
│   ├── mod.rs           # command registry (all()), colours, is_admin gate
│   ├── sustainability.rs# sustainability, alternatives, care, tips
│   ├── materials.rs     # impact, compare, search
│   ├── brands.rs        # brands
│   ├── user_commands.rs # profile, leaderboard, setpreference
│   └── admin.rs         # loaddata, stats, announce (admin-gated)
└── bin/                 # gsbot-load-fixtures, gsbot-export-data, gsbot-backup-db
migrations/0001_init.sql # materials, garments, brands, users, garment_materials
```

Binaries: `gsbot` (the bot), `gsbot-load-fixtures`, `gsbot-export-data`,
`gsbot-backup-db`. Plus the `gsbot` library crate.

## The SPARK Seam — `src/domain.rs`

`src/domain.rs` is the **correctness-critical, pure** scoring kernel: no I/O,
no allocation in the numeric core, total over its documented domain. It
exposes a stable **C ABI** in `mod ffi` with `#[no_mangle] extern "C"`
functions:

- `gsbot_material_overall_score`
- `gsbot_lifespan_multiplier`
- `gsbot_level_for_points`

This is the architecture's **verification seam**: a formally-verified
SPARK/Ada module can later export the same symbols and be linked in place of
the Rust bodies via the hyperpolymath Zig-FFI / Idris2-ABI pattern, with **no
caller changes**. Callers go through the safe Rust wrappers, so substitution
is transparent. Keep this module pure and total; do not add I/O here.

## Build / Test / Lint (via `Justfile` or cargo directly)

```bash
just build          # cargo build
just release        # cargo build --release
just test           # cargo test --all-targets
just run            # cargo run --bin gsbot
just load-data      # cargo run --bin gsbot-load-fixtures
just export-data    # cargo run --bin gsbot-export-data
just backup         # cargo run --bin gsbot-backup-db
just lint           # cargo clippy --all-targets -- -D warnings
just fmt            # cargo fmt --all -- --check
just format         # cargo fmt --all
just clean          # cargo clean
just docker-build   # docker build -t gsbot:latest -f Containerfile .
just docker-up      # docker compose up -d
```

Tests are `cargo test --all-targets` (there is no pytest). Lint must pass
clippy with warnings denied. Format with `cargo fmt`.

## Environment Variables (`src/config.rs`)

| Variable | Default | Notes |
|---|---|---|
| `DISCORD_TOKEN` | — | **Required** |
| `DISCORD_PREFIX` | `!` | Command prefix |
| `DISCORD_ADMIN_IDS` | empty | Comma-separated Discord user IDs |
| `DATABASE_URL` | `sqlite:///<base>/data/gsbot.db` | SQLite only |
| `DATABASE_ECHO` | `false` | |
| `CACHE_TTL` | `3600` | seconds |
| `CACHE_MAXSIZE` | `1000` | |
| `LOG_LEVEL` | `INFO` | |
| `LOG_FILE` | unset | optional log file path |
| `API_TIMEOUT` | `30` | seconds |
| `API_RETRY_COUNT` | `3` | |
| `ENABLE_CACHING` | `true` | |
| `ENABLE_ANALYTICS` | `false` | |

## Command Surface (prefix commands; default prefix `!`)

- `sustainability <garment>` (aliases `sus`, `score`)
- `alternatives <garment>` (alias `alt`)
- `care <garment>`
- `tips`
- `impact <material>` (alias `material`)
- `compare <material1> <material2>`
- `search <query>`
- `brands [name]` (alias `brand`)
- `profile` (aliases `stats`, `me`)
- `leaderboard` (aliases `lb`, `top`)
- `setpreference <materials|budget|priority> <value>` (alias `pref`)
- `loaddata` — **admin**
- `stats` — **admin**
- `announce <message>` — **admin**

Admin gate (`commands::is_admin`): author ID is in `DISCORD_ADMIN_IDS`, or the
invoking guild member has the Administrator permission.

## Development Guidelines

- **No Python, ever.** No `pip`, no pytest, no virtualenv, no `requirements.txt`.
  Docs may only use `rust`, `bash`, `sh`, `sql`, `toml`, `json`, `dockerfile`,
  or `text` code fences.
- Keep `domain.rs` pure/total; it is the SPARK substitution boundary.
- Preserve observable behaviour when porting/refactoring (point awards, embed
  fields, command names and aliases mirror the original cogs).
- Every source file carries the `SPDX-License-Identifier: CC-BY-SA-4.0`
  header.
- Conventional, atomic commits; feature branches off `main`.
- Run `just lint` and `just test` before opening a PR.

## License

`MPL-2.0`. See `LICENSE`.
