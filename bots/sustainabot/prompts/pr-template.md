<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<!-- SPDX-FileCopyrightText: 2024-2025 hyperpolymath -->

## Description

<!-- Describe your changes here -->

## Oikos Bot Checklist

Before submitting, please verify:

### Carbon Efficiency
- [ ] Reviewed algorithm complexity (avoided O(n²) when O(n log n) is possible)
- [ ] Minimized unnecessary computations
- [ ] Used caching/memoization where appropriate
- [ ] Batched I/O operations where possible

### Energy Patterns
- [ ] No busy-waiting patterns introduced
- [ ] Used event-driven approaches for waiting
- [ ] Implemented connection pooling for external resources
- [ ] Resources are released promptly (context managers, try-finally)

### Resource Allocation
- [ ] Memory usage is bounded
- [ ] No unbounded caching without eviction
- [ ] Large data processed lazily where possible
- [ ] No resource leaks

### Trade-Off Documentation
- [ ] Any trade-offs are documented with rationale
- [ ] Pareto optimality considered for design decisions
- [ ] Technical debt (if introduced) is documented and justified

### Quality
- [ ] Cyclomatic complexity is reasonable (< 10 per function)
- [ ] Tests added for new functionality
- [ ] Documentation updated if needed

## Eco Impact

<!-- oikos-bot will fill this section automatically -->

**Estimated Changes:**
- Carbon Impact: [ ] Improved [ ] Neutral [ ] Needs Review
- Energy Impact: [ ] Improved [ ] Neutral [ ] Needs Review
- Quality Impact: [ ] Improved [ ] Neutral [ ] Needs Review

## Related Issues

<!-- Link related issues here -->
Closes #

## Additional Notes

<!-- Any additional context for reviewers -->
