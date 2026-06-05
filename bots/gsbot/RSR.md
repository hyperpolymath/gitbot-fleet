# Rhodium Standard Repository (RSR) Compliance

## Overview

The Garment Sustainability Bot strives to comply with the Rhodium Standard Repository (RSR) framework, which defines comprehensive standards for repository organization, documentation, and development practices.

## RSR Compliance Level: **Bronze**

We currently achieve **Bronze-level** RSR compliance. The project is
implemented in **Rust** (with a designed-in SPARK verification seam — see
`src/domain.rs`); it was ported from a now-deleted Python prototype.

## Compliance Checklist

### ✅ Documentation (Complete)

- ✅ README.md - Comprehensive project overview
- ✅ LICENSE - Mozilla Public License 2.0
- ✅ SECURITY.md - Security policy and vulnerability reporting
- ✅ CONTRIBUTING.md - Contribution guidelines
- ✅ CODE_OF_CONDUCT.md - Community conduct expectations
- ✅ MAINTAINERS.md - Maintainer information and governance
- ✅ CHANGELOG.md - Version history and changes

### ✅ .well-known/ Directory (Complete)

- ✅ security.txt (RFC 9116) - Security contact information
- ✅ ai.txt - AI training and usage policy
- ✅ humans.txt - Attribution and project information

### ✅ Build System (Complete)

- ✅ Justfile - Just build recipes (build/test/run/lint/format/...)
- ✅ Mustfile - Mandatory checks (invokes `just lint` / `just fmt`)
- ✅ Cargo.toml - Crate manifest and dependency management
- ✅ Containerfile + docker-compose.yml - Containerised build
- ✅ migrations/0001_init.sql - Schema (applied via `sqlx::migrate!`)
- ✅ .editorconfig - Editor consistency

### ✅ CI/CD (Complete)

- ✅ Fleet-level GitHub Actions workflows (`.github/workflows/`), including
  the Hypatia security scan that self-scans this repository
- ✅ Automated testing (`cargo test --all-targets`)
- ✅ Code quality checks (`cargo clippy --all-targets -- -D warnings`,
  `cargo fmt --all -- --check`)
- ✅ Banned-language enforcement (Hypatia / ts-blocker / npm-bun-blocker)
- ✅ Build verification

### ✅ Testing (Complete)

- ✅ Unit tests (in-crate `#[cfg(test)]`, e.g. the `domain.rs` kernel tests)
- ✅ `cargo test --all-targets` (uses `tempfile` / in-memory SQLite)
- ✅ Pure-kernel tests pin the scoring formulas (SPARK-ready)
- ✅ CI/CD integration

### ✅ TPCF (Complete)

- ✅ TPCF.md - Tri-Perimeter Contribution Framework declaration
- ✅ Perimeter 3 (Community Sandbox)
- ✅ Clear contribution model
- ✅ Transparent governance

### ✅ Type Safety (Complete)

- ✅ Compile-time static typing (Rust)
- ✅ `cargo clippy` with warnings denied in CI
- ✅ Typed errors via `thiserror`; `anyhow` at the application boundary
- ✅ The correctness-critical `domain.rs` kernel is pure and total

**Assessment**: Full compile-time type safety. The kernel is structured for
formal verification (SPARK seam).

### ✅ Memory Safety (Complete)

- ✅ Rust ownership/borrowing — no GC, no manual `free`
- ✅ No `unsafe` in the application logic; the only `extern "C"` surface is
  the deliberate, pure C-ABI in `domain::ffi`
- ✅ Safe Rust wrappers in front of the FFI symbols

**Assessment**: Memory-safe by construction.

### ⚠️ Offline-First (Partial)

- ✅ Core functionality works offline (database, logic)
- ✅ No required network calls for base operations
- ⚠️ Discord bot requires network for Discord API
- ⚠️ Optional external integrations may need network

**Assessment**: Mostly offline-capable except for Discord communication (inherent to bot nature).

### ⚠️ Zero Dependencies (Not Applicable)

- ❌ Has dependencies (poise/serenity, sqlx, tokio, tracing, etc.)
- ℹ️ Crates are vetted and security-scanned (`cargo audit`)
- ℹ️ `Cargo.lock` pins the exact dependency graph

**Assessment**: Not zero-dependency. Acceptable for Bronze level.

### ✅ Reproducible Builds (Partial)

- ✅ `Cargo.lock` pins exact crate versions
- ✅ Multi-stage `Containerfile` for containerised reproducibility
- ⚠️ No guix.scm yet (Guix is the canonical packager; add if needed)

**Assessment**: Reproducible via `Cargo.lock` + the Containerfile.

## RSR Level Definitions

### Bronze Level (Current)

**Required:**
- ✅ All documentation files
- ✅ .well-known/ directory
- ✅ Build system (Justfile + Mustfile + Cargo)
- ✅ CI/CD pipeline
- ✅ Test suite (`cargo test --all-targets`)
- ✅ TPCF declaration
- ✅ Compile-time type safety (Rust)

**Achieved:** Yes (Bronze compliant)

### Silver Level

**Additional requirements:**
- Formal verification (SPARK, TLA+, or equivalent)
- Zero critical dependencies or all dependencies verified
- Reproducible builds (Guix)
- Multi-language verification
- Security audit

**Status:** Partially seeded. The *SPARK seam* (`src/domain.rs` — pure,
total, stable C ABI in `mod ffi`) is in place so a formally-verified
SPARK/Ada module can be substituted for the numeric core with no caller
changes. Full verification not yet pursued.

### Gold Level

**Additional requirements:**
- Complete formal verification
- Zero dependencies
- Mathematical proofs of correctness
- Security certification
- Academic peer review

**Status:** Not applicable (research-grade requirements)

## Compliance by Category

| Category | Status | Notes |
|----------|--------|-------|
| Documentation | ✅ Complete | 7 core docs + 3 .well-known |
| Build System | ✅ Complete | Justfile + Mustfile + Cargo |
| CI/CD | ✅ Complete | Fleet GitHub Actions + Hypatia self-scan |
| Testing | ✅ Complete | `cargo test --all-targets` |
| TPCF | ✅ Complete | Perimeter 3 declared |
| Type Safety | ✅ Complete | Compile-time static typing (Rust) |
| Memory Safety | ✅ Complete | Rust ownership; safe wrappers over FFI |
| Offline-First | ⚠️ Partial | Core logic offline, Discord needs network |
| Zero Deps | ❌ No | Uses vetted crates; `Cargo.lock` pinned |
| Reproducible | ✅ Complete | `Cargo.lock` + Containerfile |

## Verification

Run RSR compliance check:

```bash
just rsr-check
```

Or manually:

```bash
# Check documentation
ls -la *.md *.adoc .well-known/

# Check build system
ls -la Justfile Mustfile Cargo.toml

# Check tests
cargo test --all-targets

# Check CI/CD
ls -la ../../.github/workflows/
```

## Continuous Improvement

### Immediate Priorities

- ✅ All Bronze requirements met

### Future Enhancements (Optional)

- [ ] Add a guix.scm for hermetic builds
- [ ] Add more integration tests
- [ ] Add a recurring `cargo audit` security gate
- [ ] Formally verify the `domain.rs` numeric core in SPARK/Ada and link it
      through the existing C-ABI seam (Silver level)

### Not Planned

- Zero dependencies (impractical given the Discord/SQL stack)
- Memory safety proofs (Rust ownership already guarantees this)

## RSR Benefits

### For Contributors

- **Clear structure**: Know where to find things
- **Standards compliance**: Familiar patterns
- **Quality signals**: High-quality project indicators
- **Documentation**: Everything is documented

### For Users

- **Trust**: Well-documented, tested, reviewed
- **Security**: Security policy and scanning
- **Transparency**: Open processes and governance
- **Support**: Clear channels for help

### For Maintainers

- **Best practices**: Framework for organization
- **Consistency**: Standard structure across projects
- **Automation**: CI/CD and tooling
- **Governance**: Clear decision-making processes

## Relationship to Other Standards

### RSR vs. Other Standards

- **RSR**: Comprehensive repository standards
- **REUSE**: License compliance (complementary)
- **OpenSSF**: Security best practices (overlap)
- **CII Best Practices**: Security and development (overlap)

### Integration

RSR integrates well with:
- OpenSSF Best Practices Badge
- REUSE compliance
- CII Badge criteria
- GitHub's Security features

## Tools and Automation

### Compliance Checking

```bash
# Run RSR compliance check
just rsr-check

# Run all validation
just validate

# Check specific aspects
just test        # Testing compliance
just lint        # Code quality compliance
just security    # Security compliance
```

### Continuous Compliance

CI/CD pipeline ensures:
- Tests always pass
- Code quality maintained
- Security scanned
- Documentation updated

## Exceptions and Adaptations

### Rust/SPARK Notes

1. **Type Safety**: compile-time static typing (Rust)
2. **Memory Safety**: Rust ownership/borrowing; safe wrappers over the
   deliberate, pure `domain::ffi` C-ABI
3. **Dependencies**: vetted crates, `Cargo.lock`-pinned
4. **Build System**: Cargo + Justfile + Mustfile
5. **Verification seam**: `src/domain.rs` is pure and total and is
   substitutable by a formally-verified SPARK/Ada module with no caller
   changes

### Discord Bot Specific

1. **Offline-First**: Bot needs Discord API (network required)
2. **Real-time**: Interactive commands require connectivity

These are inherent to the bot's purpose and documented.

## Compliance History

### Version 0.2.0 (Current)

- ✅ Full Rust/SPARK port; Python prototype removed in its entirety
- ✅ Type Safety and Memory Safety upgraded to Complete (Rust)
- ✅ SPARK seam (`src/domain.rs`) in place toward Silver-level verification
- ✅ Build system (Justfile + Mustfile + Cargo)
- ✅ CI/CD pipeline operational (fleet workflows + Hypatia self-scan)
- ✅ Test suite (`cargo test --all-targets`)
- ✅ TPCF Perimeter 3 declared

### Version 0.1.0 (Python era — historical)

- ✅ Bronze-level RSR compliance achieved for the Python prototype

## Questions?

- **About RSR**: See this document
- **About TPCF**: See TPCF.md
- **About contributing**: See CONTRIBUTING.md
- **About the project**: See README.md

## References

- RSR Framework: Part of broader Rhodium initiative
- RFC 9116 (security.txt): https://www.rfc-editor.org/rfc/rfc9116.html
- TPCF: See TPCF.md

---

**RSR Level**: Bronze (Silver seam in place via `src/domain.rs`)
**TPCF Perimeter**: 3 (Community Sandbox)
**Last Verified**: 2026-05-16
**Next Review**: 2026-09-01
