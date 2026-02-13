// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Pattern detection for inefficient code

/// A detected inefficiency pattern with metadata
#[derive(Debug, Clone)]
pub struct PatternMatch {
    /// Machine-readable pattern name (e.g. "nested-loops")
    pub name: String,
    /// Human-readable description
    pub description: String,
    /// Concrete fix suggestion for SARIF output
    pub suggestion: Option<String>,
    /// Estimated energy impact multiplier (1.0 = normal, >1 = worse)
    pub impact_multiplier: f64,
}

/// Detect problematic patterns in code
pub fn detect_patterns(_source: &str, node: &tree_sitter::Node) -> Vec<PatternMatch> {
    let mut patterns = Vec::new();

    // Check nested loops
    let loop_depth = count_loop_depth(node);
    if loop_depth >= 3 {
        patterns.push(PatternMatch {
            name: "nested-loops".to_string(),
            description: format!("Deeply nested loops (depth {}): O(n^{}) complexity", loop_depth, loop_depth),
            suggestion: Some("Consider algorithm optimization to reduce nested iterations. \
                Use hash maps for lookups, sort + binary search, or restructure as flat iteration."
                .to_string()),
            impact_multiplier: loop_depth as f64,
        });
    }

    // Check for busy-wait loops (loop/while with no sleep/await/yield)
    if has_busy_wait(_source, node) {
        patterns.push(PatternMatch {
            name: "busy-wait".to_string(),
            description: "Loop without sleep, await, or yield burns CPU continuously".to_string(),
            suggestion: Some("Replace busy-wait with async/await, tokio::time::sleep, \
                std::thread::sleep, or a channel recv."
                .to_string()),
            impact_multiplier: 5.0,
        });
    }

    // Check for string concatenation in loops
    if has_string_concat_in_loop(_source, node) {
        patterns.push(PatternMatch {
            name: "string-concat-in-loop".to_string(),
            description: "String concatenation inside loop causes repeated allocation".to_string(),
            suggestion: Some("Use String::with_capacity() and push_str(), or collect with \
                iterators instead of concatenating in a loop."
                .to_string()),
            impact_multiplier: 2.0,
        });
    }

    // Check for clone in loops
    if has_clone_in_loop(_source, node) {
        patterns.push(PatternMatch {
            name: "clone-in-loop".to_string(),
            description: ".clone() inside loop body causes repeated deep copies".to_string(),
            suggestion: Some("Consider borrowing instead of cloning, or move the clone \
                outside the loop if the value doesn't change per iteration."
                .to_string()),
            impact_multiplier: 1.5,
        });
    }

    // Check for unbuffered I/O
    if has_unbuffered_io(_source, node) {
        patterns.push(PatternMatch {
            name: "unbuffered-io".to_string(),
            description: "Read/write without BufReader/BufWriter causes excessive syscalls".to_string(),
            suggestion: Some("Wrap the reader/writer with BufReader/BufWriter for \
                buffered I/O to reduce system call overhead."
                .to_string()),
            impact_multiplier: 3.0,
        });
    }

    // Check for large allocations
    if has_large_allocation(_source, node) {
        patterns.push(PatternMatch {
            name: "large-allocation".to_string(),
            description: "Large heap allocation (>1MB) detected".to_string(),
            suggestion: Some("Review memory allocation size. Consider streaming, \
                chunked processing, or memory-mapped files for large data."
                .to_string()),
            impact_multiplier: 2.0,
        });
    }

    // Check for redundant allocations (.to_string()/.to_owned() where borrow suffices)
    if has_redundant_to_string(_source, node) {
        patterns.push(PatternMatch {
            name: "redundant-allocation".to_string(),
            description: "Unnecessary .to_string()/.to_owned() where a borrow would suffice".to_string(),
            suggestion: Some("Accept &str instead of String where possible to avoid \
                unnecessary heap allocation."
                .to_string()),
            impact_multiplier: 1.2,
        });
    }

    patterns
}

fn count_loop_depth(node: &tree_sitter::Node) -> usize {
    let is_loop = matches!(
        node.kind(),
        "for_expression"
            | "while_expression"
            | "loop_expression"
            | "for_statement"
            | "while_statement"
    );

    let mut max_child_depth = 0;
    let mut cursor = node.walk();

    if cursor.goto_first_child() {
        loop {
            let child_depth = count_loop_depth(&cursor.node());
            max_child_depth = max_child_depth.max(child_depth);

            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }

    if is_loop {
        1 + max_child_depth
    } else {
        max_child_depth
    }
}

/// Check if a loop body contains no sleep/await/yield/recv — indicating a busy wait
fn has_busy_wait(source: &str, node: &tree_sitter::Node) -> bool {
    let is_loop = matches!(
        node.kind(),
        "loop_expression" | "while_expression"
    );

    if !is_loop {
        // Recurse into children to find loops
        let mut cursor = node.walk();
        if cursor.goto_first_child() {
            loop {
                if has_busy_wait(source, &cursor.node()) {
                    return true;
                }
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
        return false;
    }

    // Found a loop — check if its body text mentions sleep/await/yield/recv
    let text = match node.utf8_text(source.as_bytes()) {
        Ok(t) => t,
        Err(_) => return false,
    };

    // If the loop body contains any blocking/yielding call, it's not a busy wait
    let has_yield = text.contains("sleep")
        || text.contains("await")
        || text.contains("yield")
        || text.contains("recv")
        || text.contains("park")
        || text.contains("wait")
        || text.contains(".await");

    !has_yield
}

/// Check if any loop body contains string concatenation
fn has_string_concat_in_loop(source: &str, node: &tree_sitter::Node) -> bool {
    find_in_loop_body(node, |child, _src| {
        // Look for binary_expression with "+" operator on strings
        // or format! macro calls
        child.kind() == "binary_expression" || child.kind() == "macro_invocation"
    }, source)
}

/// Check if any loop body contains .clone() calls
fn has_clone_in_loop(source: &str, node: &tree_sitter::Node) -> bool {
    find_in_loop_body(node, |child, src| {
        if child.kind() == "call_expression" || child.kind() == "method_call_expression" {
            if let Ok(text) = child.utf8_text(src.as_bytes()) {
                return text.contains(".clone()");
            }
        }
        false
    }, source)
}

/// Check for File::open/create without BufReader/BufWriter nearby
fn has_unbuffered_io(source: &str, node: &tree_sitter::Node) -> bool {
    let text = match node.utf8_text(source.as_bytes()) {
        Ok(t) => t,
        Err(_) => return false,
    };

    let has_file_io = text.contains("File::open") || text.contains("File::create");
    let has_buffering = text.contains("BufReader") || text.contains("BufWriter");

    has_file_io && !has_buffering
}

/// Check for large allocations: Vec::with_capacity(>1_000_000) or vec![0; large]
fn has_large_allocation(source: &str, node: &tree_sitter::Node) -> bool {
    let text = match node.utf8_text(source.as_bytes()) {
        Ok(t) => t,
        Err(_) => return false,
    };

    // Simple heuristic: look for large numeric literals near allocation calls
    if text.contains("with_capacity") || text.contains("vec![") {
        // Check for numbers > 1_000_000
        for word in text.split(|c: char| !c.is_ascii_digit() && c != '_') {
            let clean: String = word.chars().filter(|c| c.is_ascii_digit()).collect();
            if let Ok(n) = clean.parse::<u64>() {
                if n > 1_000_000 {
                    return true;
                }
            }
        }
    }
    false
}

/// Check for .to_string() or .to_owned() calls that may be redundant
fn has_redundant_to_string(source: &str, node: &tree_sitter::Node) -> bool {
    let text = match node.utf8_text(source.as_bytes()) {
        Ok(t) => t,
        Err(_) => return false,
    };

    // Count occurrences as a heuristic — many .to_string() in one function is suspicious
    let to_string_count = text.matches(".to_string()").count();
    let to_owned_count = text.matches(".to_owned()").count();

    (to_string_count + to_owned_count) >= 5
}

/// Find a pattern match inside loop bodies
fn find_in_loop_body(
    node: &tree_sitter::Node,
    predicate: impl Fn(&tree_sitter::Node, &str) -> bool + Copy,
    source: &str,
) -> bool {
    let is_loop = matches!(
        node.kind(),
        "for_expression"
            | "while_expression"
            | "loop_expression"
            | "for_statement"
            | "while_statement"
    );

    if is_loop {
        // Search inside loop body for the pattern
        return subtree_matches(node, predicate, source);
    }

    // Recurse to find loops
    let mut cursor = node.walk();
    if cursor.goto_first_child() {
        loop {
            if find_in_loop_body(&cursor.node(), predicate, source) {
                return true;
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    false
}

/// Check if any node in the subtree matches the predicate
fn subtree_matches(
    node: &tree_sitter::Node,
    predicate: impl Fn(&tree_sitter::Node, &str) -> bool + Copy,
    source: &str,
) -> bool {
    if predicate(node, source) {
        return true;
    }
    let mut cursor = node.walk();
    if cursor.goto_first_child() {
        loop {
            if subtree_matches(&cursor.node(), predicate, source) {
                return true;
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_match_fields() {
        let pm = PatternMatch {
            name: "test".to_string(),
            description: "Test pattern".to_string(),
            suggestion: Some("Fix it".to_string()),
            impact_multiplier: 1.5,
        };
        assert_eq!(pm.name, "test");
        assert!(pm.suggestion.is_some());
        assert!(pm.impact_multiplier > 1.0);
    }
}
