# Accessibilitybot — Sonnet Task Plan (NEW BOT)

## Context

Accessibilitybot is a NEW Tier 2 (Finisher) bot in the gitbot-fleet ecosystem. It enforces WCAG 2.3 AAA compliance, ARIA correctness, semantic HTML/XML integrity, and accessibility best practices across repositories.

**This bot does not yet exist.** This plan describes creating it from scratch using the RSR template.

**Philosophy**: Accessibility is not optional. CSS-first, HTML-second. Full WCAG 2.3 AAA compliance. Semantic XML with ARIA. This aligns with the user's explicit security/accessibility requirements.

---

## Task 0: Scaffold the Repository

### 0.1 Clone from RSR template
```bash
cd ~/Documents/hyperpolymath-repos
git clone https://github.com/hyperpolymath/rsr-template-repo accessibilitybot
cd accessibilitybot
rm -rf .git && git init -b main
```

### 0.2 Set up Rust workspace
```bash
cargo init --name accessibilitybot
```

### 0.3 Cargo.toml
```toml
[package]
name = "accessibilitybot"
version = "0.1.0"
edition = "2021"
license = "PMPL-1.0-or-later"
authors = ["Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"]
description = "WCAG 2.3 AAA Accessibility Compliance Bot for gitbot-fleet"

[dependencies]
gitbot-shared-context = { path = "../gitbot-fleet/shared-context" }
clap = { version = "4", features = ["derive"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
anyhow = "1"
tracing = "0.1"
tracing-subscriber = "0.3"
walkdir = "2"
regex = "1"
scraper = "0.20"  # HTML parsing + CSS selectors
```

### 0.4 Standard RSR files
- All 6 SCM files in `.machine_readable/`
- Bot directives in `.bot_directives/`
- Contractiles in `contractiles/`
- 16+ standard workflows in `.github/workflows/`
- SPDX headers on all files

---

## Task 1: Core WCAG Analyzers

### 1.1 Image Alt Text Analyzer (`src/analyzers/alt_text.rs`)
- Scan HTML/JSX/TSX files for `<img>` tags
- Check: every `<img>` has a non-empty `alt` attribute
- Check: alt text is descriptive (not just "image", "photo", "icon")
- Check: decorative images use `alt=""` (empty, not missing)
- Severity: Error for missing alt, Warning for generic alt text
- WCAG criterion: 1.1.1 Non-text Content (Level A)

### 1.2 Color Contrast Analyzer (`src/analyzers/contrast.rs`)
- Parse CSS files for color/background-color pairs
- Calculate contrast ratio using WCAG 2.3 algorithm
- AAA requirements: 7:1 for normal text, 4.5:1 for large text
- Check: inline styles in HTML also comply
- Severity: Error for below AA (4.5:1), Warning for below AAA (7:1)
- WCAG criterion: 1.4.6 Contrast (Enhanced) (Level AAA)

### 1.3 Semantic HTML Analyzer (`src/analyzers/semantic.rs`)
- Check: pages use semantic elements (`<header>`, `<nav>`, `<main>`, `<article>`, `<footer>`)
- Check: headings follow proper hierarchy (no skipping from h1 to h3)
- Check: lists use `<ul>`/`<ol>`/`<dl>` (not styled `<div>`s)
- Check: no `<div>` soup — flag pages with >20 nested divs and no semantic elements
- Check: `<table>` has `<thead>`/`<tbody>`, `<th>` with scope
- Severity: Warning for missing semantics, Note for suggestions
- WCAG criterion: 1.3.1 Info and Relationships (Level A)

### 1.4 ARIA Validator (`src/analyzers/aria.rs`)
- Check: ARIA roles match element semantics (no `role="button"` on `<div>` when `<button>` works)
- Check: required ARIA attributes present (e.g., `aria-label` on icon-only buttons)
- Check: `aria-hidden="true"` not applied to focusable elements
- Check: live regions (`aria-live`) used appropriately
- Check: no redundant ARIA (e.g., `role="navigation"` on `<nav>`)
- Severity: Error for invalid ARIA, Warning for redundant ARIA
- WCAG criterion: 4.1.2 Name, Role, Value (Level A)

### 1.5 Keyboard Navigation Analyzer (`src/analyzers/keyboard.rs`)
- Check: all interactive elements are keyboard-focusable
- Check: tab order follows visual layout (no positive `tabindex`)
- Check: focus indicators visible (not `outline: none` without replacement)
- Check: no keyboard traps (modals/dialogs have escape mechanism)
- Check: skip navigation links present
- Severity: Error for keyboard traps, Warning for missing focus styles
- WCAG criterion: 2.1.1 Keyboard (Level A), 2.4.7 Focus Visible (Level AA)

### 1.6 Form Accessibility Analyzer (`src/analyzers/forms.rs`)
- Check: every `<input>` has associated `<label>` (via `for`/`id` or wrapping)
- Check: required fields indicated (not just by color)
- Check: error messages are descriptive and associated with fields
- Check: autocomplete attributes present where appropriate
- Check: form validation errors announced to screen readers
- Severity: Error for missing labels, Warning for missing autocomplete
- WCAG criterion: 1.3.5 Identify Input Purpose (Level AA), 3.3.2 Labels (Level A)

### 1.7 Media Accessibility Analyzer (`src/analyzers/media.rs`)
- Check: `<video>` has captions/subtitles track
- Check: `<audio>` has transcript
- Check: auto-playing media can be paused/stopped
- Check: no content that flashes more than 3 times per second
- Severity: Error for missing captions, Warning for missing transcripts
- WCAG criterion: 1.2.1 Audio-only/Video-only (Level A), 2.3.1 Three Flashes (Level A)

### 1.8 Language and Text Analyzer (`src/analyzers/language.rs`)
- Check: `<html>` has `lang` attribute
- Check: language changes marked with `lang` attribute on containing element
- Check: abbreviations explained on first use
- Check: reading level appropriate (Flesch-Kincaid analysis, target grade 9 for AAA)
- Severity: Error for missing lang, Note for reading level
- WCAG criterion: 3.1.1 Language of Page (Level A), 3.1.5 Reading Level (Level AAA)

---

## Task 2: CSS-First Analysis

Per the user's philosophy: "CSS-first, HTML-second"

### 2.1 CSS Analysis Module (`src/analyzers/css.rs`)
- Check: responsive design (media queries present for mobile/tablet/desktop)
- Check: font sizes use relative units (rem/em, not px)
- Check: line height ≥ 1.5 for body text (WCAG 1.4.12)
- Check: text spacing adjustable without loss of content
- Check: no `!important` on user-agent stylesheet overrides that break accessibility
- Check: `prefers-reduced-motion` media query respected
- Check: `prefers-color-scheme` supported (dark mode)
- Check: `prefers-contrast` supported (high contrast mode)
- Check: no `display: none` on elements that should be screen-reader visible (use `.sr-only` pattern instead)

---

## Task 3: CLI Interface

### 3.1 Subcommands
```
accessibilitybot check <dir>           # Run all WCAG checks
accessibilitybot analyze <file>        # Single file analysis
accessibilitybot report <dir>          # Generate SARIF report
accessibilitybot fleet <dir>           # Run as fleet member
accessibilitybot audit <url>           # Audit live page (future)
```

### 3.2 Flags
```
--level <a|aa|aaa>     # WCAG conformance level (default: aaa)
--format <text|json|sarif>
--output <file>
--fix                  # Auto-fix where possible
--verbose
```

### 3.3 Output
- Text: human-readable findings with WCAG criterion references
- JSON: structured findings
- SARIF: for IDE/CI integration (reuse sustainabot-sarif crate or similar)

---

## Task 4: Auto-Fix Capability

### 4.1 Safe fixes (auto-apply)
- Add `alt=""` to decorative images
- Add `lang` attribute to `<html>`
- Add `<label>` wrapper to `<input>` with adjacent text
- Add `scope` to `<th>` elements
- Add `role="main"` to primary content div
- Replace `outline: none` with visible focus style

### 4.2 Suggested fixes (propose only)
- Suggest semantic element replacements for div soup
- Suggest ARIA labels for unlabeled interactive elements
- Suggest color alternatives for contrast failures
- Suggest caption tracks for media elements

---

## Task 5: Fleet Integration

### 5.1 BotId
- Add `BotId::Accessibilitybot` to the gitbot-shared-context BotId enum
- Or use a string identifier if the enum can't be extended

### 5.2 Finding categories
- `"accessibility/wcag-a"` — Level A violations
- `"accessibility/wcag-aa"` — Level AA violations
- `"accessibility/wcag-aaa"` — Level AAA violations
- `"accessibility/aria"` — ARIA-specific issues
- `"accessibility/css"` — CSS accessibility issues

### 5.3 Finding metadata
- Include WCAG criterion reference (e.g., "1.1.1")
- Include WCAG level (A/AA/AAA)
- Include fix suggestion
- Include impact assessment (who is affected: blind, low-vision, motor, cognitive)

### 5.4 Bot modes
- **Verifier**: Block PRs with Level A violations
- **Advisor**: Comment with all findings, don't block
- **Consultant**: Only analyze when @accessibilitybot is mentioned
- **Regulator**: Enforce minimum WCAG level compliance

---

## Task 6: Ecosystem Integration

### 6.1 Hypatia Integration
- Accessibility findings feed into Hypatia's learning loop
- Pattern: recurring WCAG violations across repos → organization-wide policy proposals

### 6.2 a2ml Manifest Integration
- Read `0-AI-MANIFEST.a2ml` for repo-specific accessibility requirements
- Some repos may declare "no UI components" → skip HTML/CSS analysis

### 6.3 k9-svc Integration
- Validate that k9 service endpoints serve accessible API documentation
- Check API error responses include accessible error messages

### 6.4 Cipherbot Coordination
- Accessibility + security overlap: ensure CAPTCHA alternatives exist, form encryption doesn't break screen readers

### 6.5 Sustainabot Coordination
- Accessibility improvements that REDUCE resource usage (semantic HTML is lighter than div soup)
- Cross-reference: inaccessible code is often ALSO inefficient code

---

## Task 7: Tests

### 6.1 Per-analyzer tests
Each analyzer needs:
- Test with accessible HTML → no findings
- Test with inaccessible HTML → correct findings with correct WCAG criteria
- Test with edge cases (SVG images, custom elements, web components)

### 6.2 Integration test
- Test repo with mixed HTML/CSS/JSX files
- Verify correct number and types of findings
- Verify SARIF output is valid
- Verify fleet findings serialize correctly

### 6.3 Fixture files
Create test fixtures:
- `tests/fixtures/accessible.html` — fully compliant page
- `tests/fixtures/inaccessible.html` — page with many violations
- `tests/fixtures/partial.html` — page with some issues
- `tests/fixtures/styles.css` — CSS with contrast issues

### Verification
- `cargo test` — minimum 30 tests, all passing
- `cargo check` — zero errors
- `accessibilitybot check tests/fixtures/` — produces expected findings
