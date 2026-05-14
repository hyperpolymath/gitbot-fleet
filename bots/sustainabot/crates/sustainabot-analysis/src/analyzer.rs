// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Core analysis engine using tree-sitter AST

use crate::carbon::estimate_carbon;
use crate::language::Language;
use crate::patterns::detect_patterns;
use anyhow::{Context, Result};
use std::fs;
use std::path::Path;
use sustainabot_metrics::*;
use tree_sitter::{Parser, Tree};

pub struct Analyzer {
    _language: Language,
    parser: Parser,
    file_path: Option<String>,
}

impl Analyzer {
    pub fn new(language: Language) -> Result<Self> {
        let mut parser = Parser::new();
        parser
            .set_language(&language.parser())
            .context("Failed to set parser language")?;

        Ok(Analyzer {
            _language: language,
            parser,
            file_path: None,
        })
    }

    pub fn analyze_file(&mut self, path: &Path) -> Result<Vec<AnalysisResult>> {
        let source = fs::read_to_string(path)
            .with_context(|| format!("Failed to read file: {}", path.display()))?;

        self.file_path = Some(path.display().to_string());
        let results = self.analyze_source(&source);
        self.file_path = None;
        results
    }

    pub fn analyze_source(&mut self, source: &str) -> Result<Vec<AnalysisResult>> {
        let tree = self
            .parser
            .parse(source, None)
            .context("Failed to parse source")?;

        self.analyze_tree(source, &tree)
    }

    fn analyze_tree(&self, source: &str, tree: &Tree) -> Result<Vec<AnalysisResult>> {
        let mut results = Vec::new();

        // Walk the AST and analyze each function
        let root = tree.root_node();
        let mut cursor = root.walk();

        self.visit_node(source, &root, &mut cursor, &mut results);

        Ok(results)
    }

    fn visit_node(
        &self,
        source: &str,
        node: &tree_sitter::Node,
        cursor: &mut tree_sitter::TreeCursor,
        results: &mut Vec<AnalysisResult>,
    ) {
        // Analyze functions
        if self.is_function_node(node) {
            if let Some(result) = self.analyze_function(source, node) {
                results.push(result);
            }
        }

        // Recurse to children
        if cursor.goto_first_child() {
            loop {
                let child = cursor.node();
                self.visit_node(source, &child, cursor, results);

                if !cursor.goto_next_sibling() {
                    break;
                }
            }
            cursor.goto_parent();
        }
    }

    fn is_function_node(&self, node: &tree_sitter::Node) -> bool {
        matches!(
            node.kind(),
            "function_item"          // Rust
            | "function_declaration" // JS
            | "arrow_function"       // JS
            | "method_declaration"   // JS
            | "function_definition"  // Python
        )
    }

    fn analyze_function(&self, source: &str, node: &tree_sitter::Node) -> Option<AnalysisResult> {
        let location = self.node_location(source, node)?;

        // Estimate resources based on code patterns
        let complexity = self.estimate_complexity(node);
        let resources = self.estimate_resources(complexity);

        // Detect problematic patterns
        let pattern_matches = detect_patterns(source, node);
        let patterns: Vec<String> = pattern_matches.iter().map(|p| p.name.clone()).collect();
        let recommendations = self.generate_recommendations(&patterns);

        // Derive rule_id and suggestion from most significant pattern
        let (rule_id, suggestion) = if let Some(pm) = pattern_matches.first() {
            (
                format!("sustainabot/{}", pm.name),
                pm.suggestion.clone(),
            )
        } else {
            ("sustainabot/general".to_string(), None)
        };

        // Calculate scores
        let eco_score = self.calculate_eco_score(&resources);
        let econ_score = self.calculate_econ_score(complexity);
        let quality_score = self.calculate_quality_score(complexity);

        let health = HealthIndex::compute(eco_score, econ_score, quality_score);

        let end = node.end_position();

        Some(AnalysisResult {
            location,
            resources,
            health,
            recommendations,
            rule_id,
            suggestion,
            end_location: Some((end.row + 1, end.column + 1)),
            confidence: sustainabot_metrics::Confidence::Estimated,
        })
    }

    fn node_location(&self, source: &str, node: &tree_sitter::Node) -> Option<CodeLocation> {
        let start = node.start_position();
        let end = node.end_position();
        let name = self.extract_function_name(source, node);

        Some(CodeLocation {
            file: self
                .file_path
                .clone()
                .unwrap_or_else(|| String::from("<source>")),
            line: start.row + 1,
            column: start.column + 1,
            end_line: Some(end.row + 1),
            end_column: Some(end.column + 1),
            name,
        })
    }

    fn extract_function_name(&self, source: &str, node: &tree_sitter::Node) -> Option<String> {
        // Try to find name node (language-specific)
        let mut cursor = node.walk();
        if cursor.goto_first_child() {
            loop {
                let child = cursor.node();
                if child.kind() == "identifier" {
                    return Some(child.utf8_text(source.as_bytes()).ok()?.to_string());
                }
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
        None
    }

    fn estimate_complexity(&self, node: &tree_sitter::Node) -> usize {
        // Simple complexity: count nodes
        let mut count = 1;
        let mut cursor = node.walk();

        if cursor.goto_first_child() {
            loop {
                count += self.estimate_complexity(&cursor.node());
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
            cursor.goto_parent();
        }

        count
    }

    fn estimate_resources(&self, complexity: usize) -> ResourceProfile {
        // Baseline estimates (will be improved with profiling data)
        let energy = Energy::joules(complexity as f64 * 0.1);
        let duration = Duration::milliseconds(complexity as f64 * 0.5);
        let carbon = estimate_carbon(energy);
        let memory = Memory::kilobytes(complexity * 2);

        ResourceProfile {
            energy,
            duration,
            carbon,
            memory,
        }
    }

    fn calculate_eco_score(&self, resources: &ResourceProfile) -> EcoScore {
        // Lower resource usage = higher score
        // Baseline: 100J = 50 score, scale logarithmically
        let energy_score = (100.0 - (resources.energy.0.ln() * 10.0)).max(0.0);
        EcoScore::new(energy_score)
    }

    fn calculate_econ_score(&self, complexity: usize) -> EconScore {
        // Lower complexity = higher efficiency
        let score = (100.0 - (complexity as f64 * 0.5)).max(0.0);
        EconScore::new(score)
    }

    fn calculate_quality_score(&self, complexity: usize) -> f64 {
        // Simple quality metric based on complexity
        (100.0 - (complexity as f64 * 0.3)).max(0.0)
    }

    fn generate_recommendations(&self, patterns: &[String]) -> Vec<String> {
        let mut recs = Vec::new();

        for pattern in patterns {
            match pattern.as_str() {
                "busy-wait" => recs.push("Replace busy-wait loop with async/await or blocking sleep".to_string()),
                "nested-loops" => recs.push("Consider algorithm optimization to reduce nested iterations".to_string()),
                "large-allocation" => recs.push("Review memory allocation - consider streaming or chunking".to_string()),
                "string-concat-in-loop" => recs.push("Use String::with_capacity + push_str or collect with iterators".to_string()),
                "clone-in-loop" => recs.push("Consider borrowing instead of cloning inside loop body".to_string()),
                "unbuffered-io" => recs.push("Wrap File with BufReader/BufWriter to reduce syscall overhead".to_string()),
                "redundant-allocation" => recs.push("Accept &str instead of String where borrow suffices".to_string()),
                _ => {}
            }
        }

        if recs.is_empty() {
            recs.push("Code looks efficient - keep up the good work!".to_string());
        }

        recs
    }
}
