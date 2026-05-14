# SustainaBot Quickstart

## Build

```bash
cargo build --release
```

## Usage

### Analyze a file

```bash
./target/release/sustainabot analyze path/to/file.rs
```

Supports: Rust (.rs), JavaScript (.js, .mjs), TypeScript (.ts)

### Analyze sustainabot itself (dogfooding!)

```bash
./target/release/sustainabot self-analyze
```

This proves we practice what we preach - the analyzer itself is efficient.

### JSON output

```bash
./target/release/sustainabot analyze file.rs --format json
```

## What It Does

SustainaBot analyzes code for:

- **Energy consumption** (Joules)
- **Carbon footprint** (gCO2e)
- **Execution time** (milliseconds)
- **Memory usage** (bytes)

And provides:
- **Eco score** (0-100) - environmental impact
- **Econ score** (0-100) - algorithmic efficiency
- **Quality score** (0-100) - code quality
- **Overall health** (weighted combination)

## Example Output

```
📍 Function: nested_loops_function
   Resources:
     Energy:   7.10 J
     Carbon:   0.0009 gCO2e
     Time:     35.50 ms

   Health Index:
     Eco:      80.4/100
     Overall:  75.1/100

   Recommendations:
     • Consider algorithm optimization to reduce nested iterations
```

## The Eclexia Connection

- **Phase 1** (DONE): Analyzer built with Eclexia principles (explicit resource types)
- **Phase 2** (Next): Policy engine runs Eclexia code from `policies/*.ecl`
- **Phase 3+**: Migrate core to Eclexia as language matures

## Philosophy

> "The best ecological code analyzer is one that's ecological itself."

SustainaBot demonstrates that efficient, resource-aware code is not just theory - it works in practice.
