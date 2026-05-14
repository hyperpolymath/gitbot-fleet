// SPDX-License-Identifier: PMPL-1.0-or-later
// SustainaBot Policy: Memory Efficiency
//
// Allocation pattern rules to catch wasteful memory usage.

def exceeds_allocation_limit(bytes: Float) -> Bool
    @requires: energy < 0.05J
{
    bytes > 1048576.0  // 1MB
}

def high_allocation_count(count: Float) -> Bool
    @requires: energy < 0.05J
{
    count > 100.0
}

def evaluate_memory(allocation_bytes: Float, allocation_count: Float) -> Bool
    @requires: energy < 0.5J
{
    exceeds_allocation_limit(allocation_bytes) || high_allocation_count(allocation_count)
}
