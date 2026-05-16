// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Calibration framework for resource estimation.
//!
//! Replaces naive `complexity * 0.1 J` with pattern-based resource profiles
//! producing ranges (min, typical, max) instead of single numbers.

use sustainabot_metrics::{Confidence, Energy, Duration, Memory, ResourceProfile};
use crate::carbon::estimate_carbon;

/// Resource estimate with min/typical/max range
#[derive(Debug, Clone)]
pub struct ResourceRange {
    pub min: ResourceProfile,
    pub typical: ResourceProfile,
    pub max: ResourceProfile,
    pub confidence: Confidence,
}

impl ResourceRange {
    /// Return the typical estimate as a single profile
    pub fn as_typical(&self) -> &ResourceProfile {
        &self.typical
    }
}

/// Operation categories for calibrated estimates
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperationKind {
    /// HashMap/BTreeMap lookup
    HashLookup,
    /// Sorting (comparison-based)
    Sort,
    /// File I/O (read or write)
    FileIO,
    /// Network call (HTTP request)
    NetworkCall,
    /// Heap allocation
    Allocation,
    /// String operations (format, concat)
    StringOp,
    /// Mathematical computation
    MathCompute,
    /// Generic/unknown operation
    Generic,
}

/// Calibrated resource profiles for known operation patterns.
///
/// These are expert estimates that will be refined with profiling data.
/// All values assume a modern x86_64 system at ~50W TDP.
pub fn estimate_operation(kind: OperationKind, n: usize) -> ResourceRange {
    match kind {
        OperationKind::HashLookup => {
            // O(1) amortized, ~50ns per lookup
            let count = n.max(1) as f64;
            ResourceRange {
                min: profile(0.000001 * count, 0.00005 * count, 64 * n.max(1)),
                typical: profile(0.000005 * count, 0.0001 * count, 128 * n.max(1)),
                max: profile(0.00005 * count, 0.001 * count, 256 * n.max(1)),
                confidence: Confidence::Calibrated,
            }
        }
        OperationKind::Sort => {
            // O(n log n), ~100ns per comparison
            let nf = n.max(1) as f64;
            let nlogn = nf * nf.log2().max(1.0);
            ResourceRange {
                min: profile(0.00001 * nlogn, 0.0001 * nlogn, 8 * n),
                typical: profile(0.00005 * nlogn, 0.0005 * nlogn, 16 * n),
                max: profile(0.0005 * nlogn, 0.005 * nlogn, 32 * n),
                confidence: Confidence::Calibrated,
            }
        }
        OperationKind::FileIO => {
            // ~1ms per syscall, ~10μJ per 4KB page
            let pages = (n / 4096).max(1) as f64;
            ResourceRange {
                min: profile(0.01 * pages, 0.5 * pages, n),
                typical: profile(0.05 * pages, 2.0 * pages, n + 4096),
                max: profile(0.5 * pages, 20.0 * pages, n + 65536),
                confidence: Confidence::Estimated,
            }
        }
        OperationKind::NetworkCall => {
            // ~50ms per request, ~0.5J for a typical HTTPS request
            ResourceRange {
                min: profile(0.05, 10.0, 4096),
                typical: profile(0.5, 50.0, 65536),
                max: profile(5.0, 500.0, 1_048_576),
                confidence: Confidence::Estimated,
            }
        }
        OperationKind::Allocation => {
            // ~10ns per allocation, ~1nJ per byte
            let bytes = n.max(1) as f64;
            ResourceRange {
                min: profile(0.000001 * bytes / 1000.0, 0.00001, n),
                typical: profile(0.00001 * bytes / 1000.0, 0.0001, n + 64),
                max: profile(0.0001 * bytes / 1000.0, 0.001, n + 4096),
                confidence: Confidence::Calibrated,
            }
        }
        OperationKind::StringOp => {
            // ~100ns per string operation, proportional to length
            let len = n.max(1) as f64;
            ResourceRange {
                min: profile(0.000005 * len, 0.0001 * len, n + 64),
                typical: profile(0.00005 * len, 0.001 * len, 2 * n + 128),
                max: profile(0.0005 * len, 0.01 * len, 4 * n + 256),
                confidence: Confidence::Estimated,
            }
        }
        OperationKind::MathCompute => {
            // ~5ns per FP operation
            let ops = n.max(1) as f64;
            ResourceRange {
                min: profile(0.0000005 * ops, 0.000005 * ops, 0),
                typical: profile(0.000005 * ops, 0.00005 * ops, 0),
                max: profile(0.00005 * ops, 0.0005 * ops, 0),
                confidence: Confidence::Calibrated,
            }
        }
        OperationKind::Generic => {
            // Fallback: linear in complexity
            let c = n.max(1) as f64;
            ResourceRange {
                min: profile(0.01 * c, 0.05 * c, 512 * n.max(1)),
                typical: profile(0.1 * c, 0.5 * c, 2048 * n.max(1)),
                max: profile(1.0 * c, 5.0 * c, 8192 * n.max(1)),
                confidence: Confidence::Unknown,
            }
        }
    }
}

/// Estimate resources for a function based on its complexity and detected patterns.
pub fn calibrated_estimate(complexity: usize, patterns: &[String]) -> ResourceProfile {
    // Start with generic estimate based on complexity
    let base = estimate_operation(OperationKind::Generic, complexity);
    let mut result = base.typical.clone();

    // Apply pattern-based adjustments
    for pattern in patterns {
        let multiplier = match pattern.as_str() {
            "nested-loops" => 3.0,
            "busy-wait" => 5.0,
            "string-concat-in-loop" => 2.0,
            "clone-in-loop" => 1.5,
            "unbuffered-io" => 3.0,
            "large-allocation" => 2.0,
            "redundant-allocation" => 1.2,
            _ => 1.0,
        };

        result.energy = Energy::joules(result.energy.0 * multiplier);
        result.duration = Duration::milliseconds(result.duration.0 * multiplier);
        result.carbon = estimate_carbon(result.energy);
    }

    result
}

fn profile(energy_j: f64, duration_ms: f64, memory_bytes: usize) -> ResourceProfile {
    let energy = Energy::joules(energy_j);
    ResourceProfile {
        energy,
        duration: Duration::milliseconds(duration_ms),
        carbon: estimate_carbon(energy),
        memory: Memory::bytes(memory_bytes),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_lookup_range() {
        let range = estimate_operation(OperationKind::HashLookup, 1000);
        assert!(range.min.energy.0 < range.typical.energy.0);
        assert!(range.typical.energy.0 < range.max.energy.0);
        assert_eq!(range.confidence, Confidence::Calibrated);
    }

    #[test]
    fn test_calibrated_estimate_with_patterns() {
        let base = calibrated_estimate(10, &[]);
        let with_pattern = calibrated_estimate(10, &["nested-loops".to_string()]);
        // Pattern should increase energy
        assert!(with_pattern.energy.0 > base.energy.0);
    }

    #[test]
    fn test_network_call_energy() {
        let range = estimate_operation(OperationKind::NetworkCall, 1);
        // Network calls should be significant energy users
        assert!(range.typical.energy.0 >= 0.1);
    }
}
