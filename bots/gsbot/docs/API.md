# API Documentation

> Implementation: **Rust** — `poise` 0.6 over `serenity` 0.12, persistence
> via `sqlx` 0.8 + SQLite. Prefix commands. (Ported from a now-deleted
> Python prototype; behaviour preserved.)

## Discord Bot Commands

All commands use the prefix `!` (configurable via `DISCORD_PREFIX` in `.env`)

### Sustainability Commands

#### !sustainability [garment]

Get sustainability score and environmental impact for a garment.

**Usage:**
```
!sustainability organic cotton t-shirt
!sus linen dress
!score hemp jeans
```

**Response:**
- Sustainability score (0-100)
- Impact category
- Water usage
- Carbon footprint
- Energy consumption
- Materials used
- Expected lifespan

**Points:** +5

---

#### !alternatives [garment]

Find more sustainable alternatives to a garment.

**Usage:**
```
!alternatives polyester jacket
!alt conventional cotton t-shirt
```

**Response:**
- List of alternatives with higher sustainability scores
- Scores and descriptions

**Points:** +5

---

#### !care [garment]

Get care instructions to extend garment life.

**Usage:**
```
!care wool sweater
```

**Response:**
- Specific care instructions
- General care tips
- Washing frequency recommendations

**Points:** +3

---

#### !tips

Get random sustainability tips.

**Usage:**
```
!tips
```

**Response:**
- 5 random sustainability tips

**Points:** +2

---

### Material Commands

#### !impact [material]

View detailed environmental impact of a material.

**Usage:**
```
!impact linen
!material organic cotton
```

**Response:**
- Overall sustainability score and grade
- Material type
- Environmental scores breakdown
- Production metrics
- Biodegradability
- Recycling potential
- Recommendation

**Points:** +5

---

#### !compare [material1] [material2]

Compare two materials across sustainability metrics.

**Usage:**
```
!compare cotton polyester
!compare hemp bamboo
```

**Response:**
- Overall scores comparison
- Category-by-category comparison
- Winner for each category

**Points:** +7

---

#### !search [query]

Search for garments and materials.

**Usage:**
```
!search organic
!search cotton
```

**Response:**
- Matching materials
- Matching garments
- Count of results

**Points:** None

---

### Brand Commands

#### !brands [name]

Search for sustainable brands or view top-rated brands.

**Usage:**
```
!brands patagonia
!brands
!brand eileen fisher
```

**Without name:**
- Top 10 sustainable brands
- Overall ratings
- Rating summaries

**With name:**
- Brand details
- Environmental rating
- Labor rating
- Animal welfare rating
- Certifications
- Country and price range
- Transparency score
- Good On You rating

**Points:** +5

---

### User Commands

#### !profile

View your sustainability profile and statistics.

**Usage:**
```
!profile
!stats
!me
```

**Response:**
- Rank and level
- Sustainability points
- Query count
- Progress to next level
- Preferences

**Points:** None

---

#### !leaderboard

View top sustainability champions.

**Usage:**
```
!leaderboard
!lb
!top
```

**Response:**
- Top 10 users
- Levels and points
- Ranks
- Your position if not in top 10

**Points:** None

---

#### !setpreference [type] [value]

Set your sustainability preferences.

**Usage:**
```
!setpreference materials organic cotton, linen
!setpreference budget $$
!setpreference priority environmental
!pref budget $$$
```

**Types:**
- `materials`: Comma-separated list of preferred materials
- `budget`: $, $$, $$$, or $$$$
- `priority`: environmental, social, animal_welfare, or all

**Points:** None

---

### Admin Commands

*Requires administrator permissions or admin role*

#### !loaddata

Load sample data into the database.

**Usage:**
```
!loaddata
```

**Response:**
- Count of materials loaded
- Count of garments loaded
- Count of brands loaded

**Points:** None

---

#### !stats

View bot statistics.

**Usage:**
```
!stats
```

**Response:**
- Guild count
- Tracked users
- Bot latency
- Database counts

**Points:** None

---

#### !announce [message]

Send an announcement to all guilds.

**Usage:**
```
!announce Important update: New features available!
```

**Response:**
- Success/failure count

**Points:** None

---

## Gamification System

### Points

Users earn points for using sustainability commands:

| Action | Points |
|--------|--------|
| Check sustainability | +5 |
| Find alternatives | +5 |
| Check impact | +5 |
| Check brands | +5 |
| Compare materials | +7 |
| Get care tips | +3 |
| Read tips | +2 |

### Levels

- Level up every 100 points
- Level = (Points / 100) + 1

### Ranks

Based on level achieved:

| Level | Rank |
|-------|------|
| 1-4 | Sustainability Learner |
| 5-9 | Conscious Consumer |
| 10-14 | Green Enthusiast |
| 15-19 | Eco Warrior |
| 20+ | Sustainability Champion |

---

## Data Models

Rows live in SQLite (schema: `migrations/0001_init.sql`) and are mapped to
Rust structs in `src/models.rs`; the query/service layer is in
`src/services.rs`. All correctness-critical scoring lives in the pure
`src/domain.rs` kernel (the SPARK seam — see ARCHITECTURE.md).

### Material

Represents fabric materials with environmental metrics.
Table: `materials`.

**Fields:**
- `name`: Material name
- `material_type`: natural, synthetic, semi_synthetic, recycled, organic
- `description`: Material description
- `water_usage_score`: 0-100
- `carbon_footprint_score`: 0-100
- `biodegradability_score`: 0-100
- `chemical_usage_score`: 0-100
- `energy_consumption_score`: 0-100
- Production metrics (water, CO2, energy per kg)
- Properties (biodegradable, recyclable, durable)

**Kernel functions (`domain.rs`):**
- `material_overall_score([f64; 5]) -> f64`: mean of the five sub-scores
- `grade(f64) -> &str`: letter grade A+ … F
- C-ABI export: `gsbot_material_overall_score`

---

### Garment

Represents clothing items with sustainability information.
Table: `garments` (linked to materials via `garment_materials`).

**Fields:**
- `name`: Garment name
- `category`: shirt, pants, dress, etc.
- `description`: Garment description
- `materials`: List of Material objects
- `typical_weight_kg`: Weight in kg
- `expected_lifespan_years`: Years
- `typical_wears`: Number of wears
- `care_instructions`: Care text
- `sustainability_score`: 0-100

**Kernel functions (`domain.rs`):**
- `garment_sustainability_score(&[f64], Option<f64>) -> f64`: mean material
  score × lifespan multiplier, capped at 100 (50.0 if no materials)
- `lifespan_multiplier(Option<f64>) -> f64`: ≥5y→1.2, ≥3y→1.1, <1y→0.8, else 1.0
- `environmental_impact(&[MaterialImpactInputs], Option<f64>)`: water/carbon/
  energy strings (or "Unknown")
- C-ABI export: `gsbot_lifespan_multiplier`

---

### Brand

Represents fashion brands with sustainability ratings.
Table: `brands`.

**Fields:**
- `name`: Brand name
- `description`: Brand description
- `website`: URL
- `overall_rating`: 0-100
- `environmental_rating`: 0-100
- `labor_rating`: 0-100
- `animal_welfare_rating`: 0-100
- Certifications (B Corp, Fair Trade, etc.)
- `country`: Country of origin
- `price_range`: $, $$, $$$, $$$$
- `good_on_you_rating`: Rating string

**Kernel functions (`domain.rs`):**
- `brand_rating_summary(f64) -> &str`: human-readable rating summary

---

### User

Tracks Discord users for gamification.
Table: `users`.

**Fields:**
- `discord_id`: Unique Discord ID
- `username`: Discord username
- `sustainability_points`: Total points
- `level`: Current level
- `queries_count`: Number of queries
- `preferred_materials`: Comma-separated
- `budget_range`: $-$$$$
- `sustainability_priority`: environmental, social, etc.

**Kernel functions (`domain.rs`):**
- `add_points(Leveling, i64) -> Leveling`: accumulate points, increment query
  count, ratchet level (`points / 100 + 1`, never decreasing)
- `rank(i64) -> &str`: rank string from level
- C-ABI export: `gsbot_level_for_points`

---

## Error Handling

All commands include error handling:

- **Command not found**: Suggests using `!help`
- **Missing arguments**: Shows required parameters
- **Database errors**: User-friendly error message
- **Permission errors**: Access denied message

Errors are logged for debugging while showing clean messages to users.

---

## Caching

Performance optimization through caching:

- **TTL Cache**: Time-based expiration (default 1 hour)
- **LRU Cache**: Size-based eviction
- **Query Cache**: Database query results

Configurable via environment variables:
```text
ENABLE_CACHING=true
CACHE_TTL=3600
CACHE_MAXSIZE=1000
```

In-process cache lives in `src/cache.rs`.

---

## Database

### Connection

**SQLite only** (no Postgres). `DATABASE_URL` uses the `sqlite:///` form;
internally it is normalised to a `sqlx` URL (`src/config.rs`).

```text
DATABASE_URL=sqlite:///<base>/data/gsbot.db
```

### Migrations

The schema is `migrations/0001_init.sql` and is applied automatically at
startup (and by `gsbot-load-fixtures`) via `sqlx::migrate!`. To add a
migration, add a new timestamped `.sql` file under `migrations/`; it is
embedded at compile time and applied on next startup. No external migration
tool is used.

---

## Extension Guide

### Adding a New Command

1. **Add a `#[poise::command]` function** in the appropriate module under
   `src/commands/` (e.g. `materials.rs`):

```rust
/// Command description.
#[poise::command(prefix_command, aliases("mc"))]
pub async fn mycommand(
    ctx: Context<'_>,
    #[description = "An argument"] arg: String,
) -> Result<(), Error> {
    gsbot::typing(&ctx).await;
    let db = &ctx.data().db;
    // ... your logic; award points via the domain kernel ...
    crate::commands::say(&ctx, format!("Response: {arg}")).await
}
```

2. **Register it** in `commands::all()` in `src/commands/mod.rs`.
3. **Add tests** (`#[cfg(test)]`) and update documentation.

### Adding a New Model

1. **Add a table** to a new migration under `migrations/`.
2. **Define the row struct** in `src/models.rs`.
3. **Add query/service methods** in `src/services.rs`.
4. **Add fixtures** in `src/fixtures.rs`.
5. **Write tests** (`cargo test --all-targets`).

---

## Best Practices

### Command Design

- Use clear, descriptive command names
- Provide aliases for common commands
- Include helpful error messages
- Use embeds for formatted responses
- Add emojis for visual appeal
- Track user engagement with points

### Performance

- Use caching for expensive operations
- Batch database queries when possible
- Use `async`/`.await` properly (tokio); the shared `SqlitePool` is cloneable
- Keep the `domain.rs` kernel pure and total

### Security

- Validate all user inputs
- Use parameterised queries (`sqlx` bind parameters)
- Check permissions for admin commands (`commands::is_admin`)
- Never expose internal errors to users (see the `on_error` mapping)
- Keep secrets in environment variables
