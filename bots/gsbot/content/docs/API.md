+++
title = "API"
weight = 1
+++

# API Documentation

## Discord Bot Commands

All commands use the prefix `!` (configurable in `.env`)

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

### Material

Represents fabric materials with environmental metrics.

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

**Methods:**
- `calculate_overall_score()`: Returns average of all scores
- `get_grade()`: Returns letter grade A+ to F

---

### Garment

Represents clothing items with sustainability information.

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

**Methods:**
- `calculate_sustainability_score()`: Based on materials and lifespan
- `get_environmental_impact()`: Returns water, carbon, energy metrics

---

### Brand

Represents fashion brands with sustainability ratings.

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

**Methods:**
- `get_certification_badges()`: List of certifications
- `get_rating_summary()`: Human-readable summary

---

### User

Tracks Discord users for gamification.

**Fields:**
- `discord_id`: Unique Discord ID
- `username`: Discord username
- `sustainability_points`: Total points
- `level`: Current level
- `queries_count`: Number of queries
- `preferred_materials`: Comma-separated
- `budget_range`: $-$$$$
- `sustainability_priority`: environmental, social, etc.

**Methods:**
- `add_points(points)`: Add points and handle level-up
- `get_rank()`: Get current rank string

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
```env
ENABLE_CACHING=True
CACHE_TTL=3600
CACHE_MAXSIZE=1000
```

---

## Database

### Connection

SQLite by default, configurable to PostgreSQL:

```env
DATABASE_URL=sqlite:///data/gsbot.db
# or
DATABASE_URL=postgresql://user:pass@localhost/dbname
```

### Migrations

Use Alembic for database migrations:

```bash
# Create migration
alembic revision --autogenerate -m "description"

# Run migrations
alembic upgrade head

# Rollback
alembic downgrade -1
```

---

## Extension Guide

### Adding a New Command

1. **Create command in appropriate cog:**

```python
@commands.command(name="mycommand")
async def my_command(self, ctx: commands.Context, arg: str):
    """Command description."""
    async with ctx.typing():
        db = next(get_db())
        try:
            # Your logic
            user = UserService.get_or_create(db, ctx.author.id, ctx.author.name)
            user.add_points(5)
            UserService.update(db, user)

            await ctx.send("Response")
        finally:
            db.close()
```

2. **Add tests**
3. **Update documentation**

### Adding a New Model

1. **Define model** in `src/models/`
2. **Add service methods** in `src/services/`
3. **Create migration** with Alembic
4. **Add fixtures** in `scripts/load_fixtures.py`
5. **Write tests**

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
- Use async/await properly
- Close database sessions

### Security

- Validate all user inputs
- Use parameterized queries (SQLAlchemy handles this)
- Check permissions for admin commands
- Never expose internal errors to users
- Keep secrets in environment variables
