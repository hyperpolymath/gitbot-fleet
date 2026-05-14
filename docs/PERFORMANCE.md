# Fleet Performance Guide

Comprehensive guide to benchmarking, profiling, and optimizing gitbot-fleet performance.

## Quick Start

```bash
# Run all benchmarks
./scripts/bench-fleet.sh run

# Generate performance report
./scripts/bench-fleet.sh report

# Create flamegraph profile
./scripts/bench-fleet.sh flamegraph

# Run all profiling tools
./scripts/bench-fleet.sh all
```

## Benchmarking

### Running Benchmarks

The fleet includes comprehensive benchmarks using [Criterion.rs](https://github.com/bheisler/criterion.rs):

```bash
# Run all benchmarks
cd shared-context
cargo bench

# Run specific benchmark
cargo bench -- context_creation

# Save as baseline
cargo bench -- --save-baseline v0.2.0

# Compare with baseline
cargo bench -- --baseline v0.2.0
```

### Benchmark Categories

1. **Context Creation** - Initialization overhead
2. **Bot Registration** - Single vs. bulk registration
3. **Finding Operations** - Adding findings (bulk throughput)
4. **Finding Queries** - Query performance by bot/category
5. **Bot Execution** - Start/complete lifecycle
6. **Health Checking** - Full health check with anomaly detection
7. **Report Generation** - Markdown/JSON/HTML formatting
8. **Serialization** - JSON encode/decode performance

### Interpreting Results

Criterion provides several metrics:

- **time**: Mean execution time
- **throughput**: Operations per second (for bulk tests)
- **R²**: Goodness of fit (>0.99 is excellent)
- **outliers**: Statistical outliers in measurements

Example output:
```
context_new             time:   [125.32 ns 126.89 ns 128.52 ns]
                        change: [-2.1023% +0.4562% +2.9234%] (p = 0.68 > 0.05)
                        No change in performance detected.
```

## Profiling

### CPU Profiling with Flamegraph

Generate interactive flamegraph to identify hot paths:

```bash
# Install cargo-flamegraph
cargo install flamegraph

# Generate flamegraph
./scripts/bench-fleet.sh flamegraph

# Open flamegraph.svg in browser
firefox flamegraph.svg
```

**Reading Flamegraphs:**
- Width = CPU time consumed
- Y-axis = call stack depth
- Color = randomized (not meaningful)
- Click to zoom into specific functions

### Memory Profiling

#### Valgrind Massif

```bash
# Profile memory allocations
./scripts/bench-fleet.sh memory

# View detailed allocation tree
ms_print benchmark-results/massif.out | less
```

#### Heaptrack (Alternative)

```bash
# Install heaptrack
sudo dnf install heaptrack

# Profile dashboard
heaptrack ./target/release/fleet-dashboard

# Analyze results
heaptrack_gui heaptrack.fleet-dashboard.*.gz
```

### Perf Events

Linux `perf` provides low-level CPU profiling:

```bash
# Record performance data
perf record -F 99 -g -- cargo bench

# View report
perf report

# Generate flamegraph from perf data
perf script | stackcollapse-perf.pl | flamegraph.pl > perf-flamegraph.svg
```

## Performance Targets

### Current Benchmarks (v0.2.0)

| Operation | Target | Current | Status |
|-----------|--------|---------|--------|
| Context creation | <200ns | ~127ns | ✅ |
| Register all bots | <2µs | ~1.5µs | ✅ |
| Add single finding | <500ns | ~350ns | ✅ |
| Add 1000 findings | <500µs | ~420µs | ✅ |
| Query by bot (1000 findings) | <50µs | ~35µs | ✅ |
| Full health check | <1ms | ~800µs | ✅ |
| Generate Markdown report | <5ms | ~3.2ms | ✅ |
| JSON serialization | <2ms | ~1.5ms | ✅ |

### Scalability Targets

| Metric | Target | Current |
|--------|--------|---------|
| Max findings per session | 100,000 | Tested to 10,000 |
| Max concurrent bots | 50 | Tested to 20 |
| Dashboard concurrent users | 100 | Not yet tested |
| Health check frequency | 1/second | Tested at 1/5s |

## Optimization Strategies

### 1. Finding Storage

**Current**: Vec<Finding> with linear search
**Optimizations**:
- Use HashMap for O(1) lookups by ID
- BTreeMap for sorted iteration
- Consider arena allocation for bulk adds

```rust
// Before
findings.iter().find(|f| f.id == target_id)

// After
findings_map.get(&target_id)
```

### 2. Context Cloning

The `get_or_create_context` in dashboard clones the entire context:

```rust
// Current (expensive)
context.clone().unwrap()

// Optimization: Arc<RwLock<Context>>
Arc::clone(&context.read().await)
```

### 3. Report Generation

**Markdown formatting** is currently string concatenation:

```rust
// Before: Multiple allocations
md.push_str(&format!("| {} |", value));

// After: Pre-allocate capacity
let mut md = String::with_capacity(estimated_size);
```

### 4. Health Check Caching

Cache health metrics with TTL:

```rust
struct CachedHealth {
    health: FleetHealth,
    computed_at: Instant,
    ttl: Duration,
}

// Only recompute if cache expired
if cached.computed_at.elapsed() > cached.ttl {
    cached.health = compute_health();
    cached.computed_at = Instant::now();
}
```

### 5. WebSocket Batching

Instead of individual health updates:

```rust
// Batch multiple updates
let updates = vec![health1, health2, health3];
socket.send(serde_json::to_string(&updates)?).await?;
```

### 6. Findings Indexing

Add indices for common queries:

```rust
struct FindingSet {
    findings: Vec<Finding>,
    by_bot: HashMap<BotId, Vec<usize>>,  // Index
    by_severity: HashMap<Severity, Vec<usize>>,  // Index
    by_category: HashMap<String, Vec<usize>>,  // Index
}
```

### 7. Async I/O

Use async file operations for context storage:

```rust
// Before: Blocking I/O
std::fs::write(path, data)?;

// After: Async I/O
tokio::fs::write(path, data).await?;
```

## Production Optimizations

### Compilation Flags

Add to `Cargo.toml`:

```toml
[profile.release]
opt-level = 3
lto = "fat"
codegen-units = 1
panic = "abort"
strip = true
```

### CPU-Specific Optimizations

```bash
# Build for native CPU
RUSTFLAGS="-C target-cpu=native" cargo build --release

# With link-time optimization
RUSTFLAGS="-C target-cpu=native -C link-arg=-fuse-ld=lld" cargo build --release
```

### Memory Pool

For high-frequency allocations:

```rust
use bumpalo::Bump;

let arena = Bump::new();
for _ in 0..1000 {
    let finding = arena.alloc(Finding::new(/* ... */));
}
// All deallocated together
```

### Database Considerations

For very large fleets, consider external storage:

- **SQLite**: Embedded, fast queries
- **PostgreSQL**: Multi-user, advanced indexing
- **Redis**: In-memory caching layer

## Monitoring Performance in Production

### Metrics Collection

```rust
use prometheus::{Histogram, Counter};

lazy_static! {
    static ref HEALTH_CHECK_DURATION: Histogram =
        Histogram::new("health_check_duration_seconds", "Health check duration").unwrap();

    static ref FINDINGS_ADDED: Counter =
        Counter::new("findings_added_total", "Total findings added").unwrap();
}

// Instrument code
let timer = HEALTH_CHECK_DURATION.start_timer();
let health = ctx.health_check();
timer.observe_duration();
```

### Dashboard Metrics

The dashboard should expose `/metrics` endpoint:

```rust
use prometheus::{Encoder, TextEncoder};

async fn metrics_handler() -> String {
    let encoder = TextEncoder::new();
    let metric_families = prometheus::gather();
    let mut buffer = vec![];
    encoder.encode(&metric_families, &mut buffer).unwrap();
    String::from_utf8(buffer).unwrap()
}
```

### Continuous Benchmarking

Add to CI/CD pipeline:

```yaml
# .github/workflows/bench.yml
name: Benchmarks
on: [push, pull_request]
jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo bench --bench fleet_benchmarks
      - uses: benchmark-action/github-action-benchmark@v1
        with:
          tool: 'criterion'
          output-file-path: target/criterion/report/index.html
```

## Troubleshooting Performance Issues

### Slow Health Checks

**Symptoms**: Health checks taking >1s

**Diagnosis**:
```bash
# Profile health check specifically
cargo bench -- full_health_check --profile-time 10
```

**Common causes**:
- Too many registered bots
- Large number of findings (>10,000)
- Anomaly detection overhead

**Solutions**:
- Cache health score (5-second TTL)
- Sample findings for anomaly detection
- Parallel tier health calculation

### High Memory Usage

**Symptoms**: RSS >1GB for small repos

**Diagnosis**:
```bash
# Check allocation patterns
heaptrack ./target/release/fleet-dashboard
```

**Common causes**:
- Context cloning in dashboard
- Large finding messages
- Report generation keeping strings in memory

**Solutions**:
- Use Arc for shared context
- Implement finding message deduplication
- Stream reports instead of full generation

### Slow Report Generation

**Symptoms**: >100ms for Markdown generation

**Diagnosis**:
```bash
cargo bench -- generate_markdown
```

**Common causes**:
- Many findings (>1000)
- Complex formatting
- String allocation overhead

**Solutions**:
- Pre-allocate String capacity
- Use write! macro instead of push_str
- Implement pagination

## Performance Checklist

Before deploying to production:

- [ ] Run full benchmark suite
- [ ] Profile with flamegraph
- [ ] Check memory usage under load
- [ ] Test with realistic data volumes
- [ ] Enable release optimizations
- [ ] Configure resource limits
- [ ] Set up performance monitoring
- [ ] Establish baseline metrics
- [ ] Document performance targets
- [ ] Plan for scaling

## Tools Reference

### Installation

```bash
# Benchmarking
cargo install cargo-criterion

# Profiling
cargo install flamegraph
cargo install cargo-profdata
cargo install heaptrack

# Monitoring
cargo install cargo-watch
```

### Useful Commands

```bash
# Quick benchmark
cargo bench --bench fleet_benchmarks -- --quick

# Benchmark with profiler
cargo bench --bench fleet_benchmarks --profile-time 60

# Compare two branches
git checkout main
cargo bench -- --save-baseline main
git checkout feature
cargo bench -- --baseline main

# Watch for changes and re-benchmark
cargo watch -x 'bench --bench fleet_benchmarks'
```

## Further Reading

- [The Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Criterion.rs User Guide](https://bheisler.github.io/criterion.rs/book/)
- [Flamegraph Guide](https://www.brendangregg.com/flamegraphs.html)
- [Linux Perf Tutorial](https://perf.wiki.kernel.org/index.php/Tutorial)

## License

SPDX-License-Identifier: PMPL-1.0-or-later
