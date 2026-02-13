<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<!-- SPDX-FileCopyrightText: 2024-2025 hyperpolymath -->

# Oikos Bot Claude Code Instructions

This file provides ecological and economic context for Claude Code when working
on this repository. These instructions help ensure AI-assisted development
follows sustainable software practices.

## System Context

You are working on a repository that is monitored by Oikos Bot, an ecological and
economic code analysis platform. When writing or modifying code, you should
consider the following sustainability principles.

## Core Principles

### 1. Carbon Awareness (Weight: 40%)

Every line of code has a carbon footprint. Consider:

- **Algorithmic Complexity**: Prefer O(n) over O(n²), O(log n) over O(n)
- **Resource Usage**: Minimize memory allocations and CPU cycles
- **I/O Efficiency**: Batch operations, use caching strategically
- **Idle Behavior**: Sleep efficiently, avoid polling

When writing loops, ask: "Will this execute millions of times in production?"

### 2. Economic Efficiency (Weight: 30%)

Apply economic thinking to code:

- **Pareto Optimality**: When making trade-offs, ensure no wasted opportunity
- **Allocative Efficiency**: Put resources where they create most value
- **Technical Debt**: Track and minimize debt accumulation
- **Opportunity Cost**: Consider what else could be done with those resources

When creating abstractions, ask: "Does this justify its complexity?"

### 3. Quality Metrics (Weight: 30%)

Maintain code quality:

- **Complexity**: Keep cyclomatic complexity under 10 per function
- **Coupling**: Minimize dependencies between modules
- **Coverage**: Ensure test coverage for critical paths
- **Documentation**: Document trade-offs and non-obvious decisions

## Current Repository Status

<!-- oikos-bot:status-start -->
```
Health Index: {{health_index}}/100
Eco Score:    {{eco_score}}/100
Econ Score:   {{econ_score}}/100
Quality:      {{quality_score}}/100

Pareto Status: {{pareto_status}}
Policy Level:  {{policy_level}}
```
<!-- oikos-bot:status-end -->

## Specific Guidance

### When Writing New Code

1. **Start with efficiency**: Choose efficient algorithms from the start
2. **Use established patterns**: Prefer patterns known to be eco-friendly
3. **Document trade-offs**: Explain why you chose one approach over another
4. **Consider scale**: What happens when this runs 1M times?

### When Refactoring

1. **Check current metrics**: Understand the eco/econ profile first
2. **Target hotspots**: Focus on files with low scores
3. **Measure improvement**: Verify changes improved metrics
4. **Avoid regression**: Don't improve one metric at cost of others

### When Reviewing

1. **Check for anti-patterns**: Busy waiting, N+1 queries, unbounded caching
2. **Validate trade-offs**: Are documented trade-offs justified?
3. **Consider alternatives**: Is there a more efficient approach?
4. **Think long-term**: How will this scale?

## Code Patterns Reference

### Efficient Patterns

```python
# Memoization for expensive computations
from functools import lru_cache

@lru_cache(maxsize=1000)
def expensive_computation(x):
    return complex_algorithm(x)
```

```python
# Connection pooling
from sqlalchemy import create_engine
from sqlalchemy.pool import QueuePool

engine = create_engine(url, poolclass=QueuePool, pool_size=5)
```

```python
# Lazy evaluation with generators
def process_large_file(path):
    with open(path) as f:
        for line in f:  # Lazy, one line at a time
            yield process_line(line)
```

```python
# Event-driven waiting
import asyncio

async def wait_for_result():
    result = await event.wait()  # Efficient waiting
    return process(result)
```

### Anti-Patterns to Avoid

```python
# DON'T: Busy waiting
while not condition:
    time.sleep(0.01)  # Still wastes cycles

# DON'T: N+1 queries
for user in users:
    orders = db.query(Order).filter(Order.user_id == user.id).all()

# DON'T: Unbounded caching
cache = {}  # Will grow forever
def get_cached(key):
    if key not in cache:
        cache[key] = expensive_fetch(key)
    return cache[key]
```

## Integration with Oikos Bot

When you make changes:

1. Oikos Bot will analyze PRs and provide feedback
2. Policy violations may block merges (in regulator mode)
3. Recommendations are based on learned patterns from successful refactoring
4. The praxis loop means your improvements help train better policies

## Trade-Off Documentation Template

When making significant trade-offs, use this format:

```
# TRADE-OFF: [Brief description]
# Objectives: [List competing objectives]
# Decision: [What you chose]
# Rationale: [Why this is Pareto optimal for this context]
# Metrics Impact:
#   - Carbon: [+/-X%]
#   - Performance: [+/-X%]
#   - Complexity: [+/-X]
# Reviewed: [date]
```

## Questions for Self-Review

Before committing, ask yourself:

1. Would I be comfortable if this code ran 10M times per day?
2. Have I documented any non-obvious trade-offs?
3. Is there a simpler way to achieve the same result?
4. Will future maintainers understand why I made these choices?

---

*Maintained by Oikos Bot | Last updated: {{timestamp}}*
