# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Planned
- Hardware assembly and testing
- Workshop pilot delivery (3 sessions, 15+ students)
- Competition video production
- Impact assessment data collection
- Competition submission (Robotics for Good 2025-2026)

## [0.1.0] - 2024-11-22

### Added
- **Core Rust Implementation**
  - Main control system with async/await (tokio)
  - Hardware abstraction layer (HAL) with traits
  - Mock implementations for development without hardware
  - Configuration management system (TOML-based)
  - Safety monitoring with state machines (CNO principles)
  - GPIO-controlled peristaltic pumps
  - I2C temperature sensor support (TMP102)
  - I2C LCD display support (16x2)
  - Emergency stop button integration
  - Status LED control
  - Educational mode with configurable observation delays

- **Hardware Documentation**
  - Complete Bill of Materials (BOM) with UK suppliers (£212-353)
  - Detailed wiring diagrams with pin assignments
  - Step-by-step assembly instructions (6 phases)
  - Component mounting guidelines
  - Testing and calibration procedures
  - Troubleshooting guide
  - Safety protocols

- **Educational Materials**
  - 2.5-hour workshop curriculum
  - Pre/post assessment surveys (knowledge and attitudes)
  - 6 student activity worksheets:
    - Mystery Box Predictions
    - Component Detective
    - System Architecture Mapping
    - Code Logic Exploration
    - Safety Systems Investigation
    - Engineering Design Decisions
  - Facilitator observation checklists
  - Impact measurement framework
  - Differentiation strategies (ages 12-18)

- **Competition Materials**
  - Robotics for Good 2025-2026 submission checklist
  - Video script template (4-5 minute documentary)
  - Partnership letter templates (schools, makerspaces)
  - Impact report framework
  - Competition alignment strategy

- **Development Infrastructure**
  - GitHub Actions CI/CD (test, lint, audit)
  - Cross-compilation for Raspberry Pi (armv7)
  - Automated release workflow
  - CONTRIBUTING.md with coding standards
  - Dual MIT/Apache-2.0 licensing

- **Documentation**
  - Comprehensive README with setup instructions
  - Architecture diagrams
  - CLAUDE.md project guidelines
  - Configuration examples (config.toml.example)
  - Cross-compilation guide

- **RSR Compliance (this release)**
  - SECURITY.md with threat model and safety guidelines
  - CODE_OF_CONDUCT.md (Contributor Covenant 2.1 + educational addendum)
  - MAINTAINERS.md with governance model
  - CHANGELOG.md (this file)
  - .well-known/ directory (security.txt, ai.txt, humans.txt)
  - justfile with build automation
  - flake.nix for Nix reproducibility

### Changed
- N/A (initial release)

### Deprecated
- N/A (initial release)

### Removed
- N/A (initial release)

### Fixed
- N/A (initial release)

### Security
- Zero `unsafe` blocks in codebase (Rust memory safety)
- Temperature validation (max/min thresholds)
- Pump runtime limits (prevents overflow)
- Emergency stop integration
- State machine verification (prevents invalid states)
- Cargo audit in CI/CD

## Release Strategy

### Versioning Scheme

- **0.x.x**: Pre-1.0 development, workshop testing phase
- **1.0.0**: First production-ready release (after competition submission)
- **1.x.x**: Feature additions, educational material updates
- **2.x.x**: Major architectural changes or breaking API changes

### Version Increments

- **MAJOR** (X.0.0): Breaking changes to configuration, API, or hardware requirements
- **MINOR** (0.X.0): New features, educational materials, non-breaking hardware additions
- **PATCH** (0.0.X): Bug fixes, documentation updates, minor improvements

### Release Process

1. **Update CHANGELOG.md** (move items from Unreleased to new version)
2. **Update version** in Cargo.toml and Cargo.lock
3. **Run tests**: `cargo test --all`
4. **Build release binary**: `cargo build --release`
5. **Cross-compile**: `cargo build --target armv7-unknown-linux-gnueabihf --release`
6. **Create git tag**: `git tag -a v0.1.0 -m "Release v0.1.0"`
7. **Push tag**: `git push origin v0.1.0`
8. **GitHub Actions** automatically creates release with binaries
9. **Announce** in discussions, social media, workshop venues

## Roadmap

### v0.2.0 (Planned - Q1 2025)

- [ ] Workshop pilot results integration
- [ ] Calibration refinement based on real testing
- [ ] Additional recipe templates
- [ ] Web-based configuration UI (optional)
- [ ] Flow sensor integration (optional)

### v0.3.0 (Planned - Q2 2025)

- [ ] Multi-language support (workshop materials translations)
- [ ] Advanced educational modules (formal verification, TLA+)
- [ ] Alternative hardware guides (different pumps, sensors)
- [ ] Cost-reduced version (<£200)

### v1.0.0 (Planned - Q3 2025)

- [ ] Competition submission complete
- [ ] 30+ students trained
- [ ] Impact data published
- [ ] Comprehensive testing complete
- [ ] Documentation finalized
- [ ] Production-ready release

### Future Possibilities

- Arduino/ESP32 port
- WASM-based simulator
- VR/AR workshop supplement
- Academic paper publication (ECER, ICSE)
- Workshop facilitator training program
- Integration with school curricula

## Breaking Changes Policy

We take breaking changes seriously, especially for educational use:

1. **Deprecation Warning**: Feature marked deprecated in release N
2. **Migration Guide**: Provided in release N
3. **Grace Period**: At least one minor version (N+1)
4. **Removal**: Feature removed in release N+2 or next major version

Example:
- v0.5.0: Feature X deprecated, migration guide provided
- v0.6.0: Feature X still works but warns
- v0.7.0 or v1.0.0: Feature X removed

## Migration Guides

### Upgrading from Pre-Release to v0.1.0

No migration needed - initial release.

### Future Migrations

Will be documented here as breaking changes occur.

## Contributors

See git commit history for detailed attribution.

**Founding Contributors**:
- [Your Name] - Project conception, implementation, educational design
- UAL Creative Communities - MechCC - Institutional support

**Special Thanks**:
- Robotics for Good Youth Challenge - Inspiration and framework
- Rust Embedded Working Group - Technical guidance
- Open-source hardware community - Component selection advice

---

**Changelog Maintained By**: Project maintainers (see MAINTAINERS.md)
**Format**: [Keep a Changelog](https://keepachangelog.com/)
**Versioning**: [Semantic Versioning](https://semver.org/)
**Last Updated**: 2024-11-22
