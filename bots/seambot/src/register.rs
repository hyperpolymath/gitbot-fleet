// SPDX-License-Identifier: PMPL-1.0-or-later

//! Seam register initialization and management

use anyhow::{Context, Result};
use std::path::Path;
use tokio::fs;
use tracing::info;

use crate::seam::{
    Invariant, RegisterMetadata, Seam, SeamRegister, SeamType, Severity, VerificationType,
};

/// Load seam register from a JSON file (synchronous)
pub fn load_register(path: &Path) -> Result<SeamRegister> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read seam register from {}", path.display()))?;
    let register: SeamRegister = serde_json::from_str(&content)
        .with_context(|| format!("Failed to parse seam register from {}", path.display()))?;
    Ok(register)
}

/// Initialize seam infrastructure in a repository
pub async fn init_seam_infrastructure(repo_path: &Path, force: bool) -> Result<()> {
    let seams_dir = repo_path.join("spec/seams");
    let checklists_dir = seams_dir.join("checklists");
    let conformance_dir = seams_dir.join("conformance");
    let freeze_stamps_dir = seams_dir.join("freeze-stamps");

    // Create directories
    fs::create_dir_all(&checklists_dir).await?;
    fs::create_dir_all(&conformance_dir).await?;
    fs::create_dir_all(&freeze_stamps_dir).await?;

    info!("Created seam directories");

    // Create seam register
    let register_path = seams_dir.join("seam-register.json");
    if register_path.exists() && !force {
        info!("Seam register already exists, skipping (use --force to overwrite)");
    } else {
        let register = create_example_register(repo_path);
        let content = serde_json::to_string_pretty(&register)?;
        fs::write(&register_path, content).await?;
        info!("Created seam register at {}", register_path.display());
    }

    // Create seam register AsciiDoc
    let register_adoc_path = seams_dir.join("seam-register.adoc");
    if !register_adoc_path.exists() || force {
        let content = create_register_adoc();
        fs::write(&register_adoc_path, content).await?;
        info!("Created seam register documentation at {}", register_adoc_path.display());
    }

    // Create example checklist
    let example_checklist_path = checklists_dir.join("example-seam.adoc");
    if !example_checklist_path.exists() || force {
        let content = create_example_checklist();
        fs::write(&example_checklist_path, content).await?;
        info!("Created example checklist");
    }

    // Create example conformance
    let example_conformance_path = conformance_dir.join("example-seam-conformance.adoc");
    if !example_conformance_path.exists() || force {
        let content = create_example_conformance();
        fs::write(&example_conformance_path, content).await?;
        info!("Created example conformance");
    }

    // Create .gitkeep in freeze-stamps
    let gitkeep_path = freeze_stamps_dir.join(".gitkeep");
    if !gitkeep_path.exists() {
        fs::write(&gitkeep_path, "").await?;
    }

    info!("Seam infrastructure initialized successfully");
    Ok(())
}

fn create_example_register(repo_path: &Path) -> SeamRegister {
    let repo_name = repo_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown")
        .to_string();

    SeamRegister {
        version: "1.0".to_string(),
        repository: repo_name,
        seams: vec![
            Seam {
                id: "example-seam".to_string(),
                name: "Example Seam".to_string(),
                description: "An example seam to demonstrate the structure".to_string(),
                side_a: vec!["module-a".to_string()],
                side_b: vec!["module-b".to_string()],
                seam_type: SeamType::Module,
                invariants: vec![
                    Invariant {
                        id: "no-hidden-channels".to_string(),
                        description: "All communication must go through declared interfaces".to_string(),
                        verification: VerificationType::Manual,
                        severity: Severity::Error,
                    },
                ],
                introduced_at: Some("i1".to_string()),
                frozen: false,
                ring: 1,
                checklist_path: Some("spec/seams/checklists/example-seam.adoc".to_string()),
                conformance_paths: vec![
                    "spec/seams/conformance/example-seam-conformance.adoc".to_string(),
                ],
                boundary_path: "src/module-a".to_string(),
                declared_dependencies: vec![],
            },
        ],
        cross_repo_seams: Vec::new(),
        metadata: RegisterMetadata {
            updated_at: chrono::Utc::now().to_rfc3339(),
            updated_by: "seambot init".to_string(),
            commit_hash: None,
        },
    }
}

fn create_register_adoc() -> String {
    r#"// SPDX-License-Identifier: PMPL-1.0-or-later
= Seam Register
:toc: macro
:toclevels: 3

This document describes the architectural seams in this repository.

toc::[]

== Overview

Seams are first-class artifacts representing architectural boundaries between components.
This register tracks all declared seams and their properties.

== Seam Inventory

=== Example Seam

[cols="1,3"]
|===
| ID | `example-seam`
| Type | Module
| Ring | 1 (Standard)
| Status | Active
|===

==== Components

* Side A: `module-a`
* Side B: `module-b`

==== Invariants

* `no-hidden-channels`: All communication must go through declared interfaces

==== Artifacts

* Checklist: `spec/seams/checklists/example-seam.adoc`
* Conformance: `spec/seams/conformance/example-seam-conformance.adoc`

== Machine-Readable Register

The canonical machine-readable register is at `seam-register.json`.

== Governance

=== Adding a New Seam

1. Define the seam in `seam-register.json`
2. Create a checklist in `checklists/`
3. Add at least one conformance example in `conformance/`
4. Run `seambot check` to validate

=== Freezing Seams

When a stage is frozen (fN), all seams touched by that stage must be stamped:

1. Run `seambot freeze-check --stage fN`
2. Create freeze stamp in `freeze-stamps/fN.json`
3. Document in release notes

=== Cross-Repo Seams

Seams that span multiple repositories are managed via git-loom.
Reference them in `cross_repo_seams` section of the register.
"#.to_string()
}

fn create_example_checklist() -> String {
    r#"// SPDX-License-Identifier: PMPL-1.0-or-later
= Seam Checklist: Example Seam
:checklist:

== Pre-Implementation

* [ ] Seam boundaries are clearly defined
* [ ] Interface contracts are documented
* [ ] Invariants are specified
* [ ] No hidden channels exist

== Implementation

* [ ] Interface types are explicit (no `any` types)
* [ ] Error handling crosses seam correctly
* [ ] Logging includes seam context
* [ ] Tests cover seam boundary

== Post-Implementation

* [ ] Conformance example added
* [ ] Documentation updated
* [ ] Seam register updated

== Review

* [ ] Peer review completed
* [ ] Seam owner approved
* [ ] No new hidden channels introduced
"#.to_string()
}

fn create_example_conformance() -> String {
    r#"// SPDX-License-Identifier: PMPL-1.0-or-later
= Conformance Example: Example Seam

== Purpose

This document demonstrates correct usage of the example-seam boundary.

== Correct Usage

.module-a calling module-b
[source,rust]
----
// Correct: Using the declared interface
let result = module_b::api::process(input)?;
----

== Anti-Patterns

.Hidden channel via global state
[source,rust]
----
// WRONG: Bypassing the seam via global state
GLOBAL_STATE.write().unwrap().set_value(input);
module_b::internal::read_from_global();
----

.Hidden channel via environment
[source,rust]
----
// WRONG: Communicating via environment variables
std::env::set_var("HIDDEN_DATA", input);
module_b::internal::check_env();
----

== Invariant Verification

=== no-hidden-channels

Verification method: Manual review

Checklist:
* [ ] No global/static mutable state shared across seam
* [ ] No environment variable communication
* [ ] No file system side-channels
* [ ] All data flows through declared interfaces
"#.to_string()
}
