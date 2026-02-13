// SPDX-License-Identifier: PMPL-1.0-or-later
//! Directory scanner for running accessibility analysis across a project.
//!
//! Walks directory trees, identifies applicable files, and runs analyzers.

use crate::analyzers;
use crate::fleet::{FindingSet, WcagLevel};
use std::path::Path;
use tracing::info;
use walkdir::WalkDir;

/// File extensions to scan
const SCANNABLE_EXTENSIONS: &[&str] = &[
    "html", "htm", "css", "jsx", "tsx", "svelte", "vue",
];

/// Directories to skip
const SKIP_DIRS: &[&str] = &[
    "node_modules", ".git", "target", "dist", "build",
    "_build", "vendor", ".next", ".nuxt", "coverage",
];

/// Scan a directory for accessibility issues
pub fn scan_directory(dir: &Path, level: WcagLevel) -> anyhow::Result<FindingSet> {
    let mut all_findings = FindingSet::new();
    let mut files_scanned = 0;

    info!("Scanning directory: {}", dir.display());

    for entry in WalkDir::new(dir)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| {
            // Skip hidden and excluded directories
            let name = e.file_name().to_str().unwrap_or("");
            if e.file_type().is_dir() {
                return !SKIP_DIRS.contains(&name) && !name.starts_with('.');
            }
            true
        })
    {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => continue,
        };

        if !entry.file_type().is_file() {
            continue;
        }

        let path = entry.path();
        let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");

        if !SCANNABLE_EXTENSIONS.contains(&ext) {
            continue;
        }

        let content = match std::fs::read_to_string(path) {
            Ok(c) => c,
            Err(e) => {
                info!("Skipping {}: {}", path.display(), e);
                continue;
            }
        };

        let file_findings = analyzers::analyze_file(path, &content, level);
        all_findings.extend(file_findings.findings);
        files_scanned += 1;
    }

    info!("Scanned {} files, found {} issues", files_scanned, all_findings.len());

    Ok(all_findings)
}

/// Scan a single file for accessibility issues
pub fn scan_file(path: &Path, level: WcagLevel) -> anyhow::Result<FindingSet> {
    let content = std::fs::read_to_string(path)?;
    Ok(analyzers::analyze_file(path, &content, level))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_nonexistent_dir() {
        let result = scan_directory(Path::new("/nonexistent/path"), WcagLevel::AAA);
        // Should succeed with empty findings (walkdir handles missing dirs gracefully)
        assert!(result.is_ok());
    }
}
