// SPDX-License-Identifier: PMPL-1.0-or-later
//! Accessibilitybot - WCAG 2.3 AAA Accessibility Compliance Bot
//!
//! Part of the gitbot-fleet ecosystem. Accessibilitybot is a Tier 2 (Finisher)
//! bot that enforces WCAG 2.3 AAA compliance, ARIA correctness, semantic HTML/XML
//! integrity, and accessibility best practices across repositories.
//!
//! ## Philosophy
//!
//! Accessibility is not optional. CSS-first, HTML-second. Full WCAG 2.3 AAA
//! compliance. Semantic XML with ARIA.
//!
//! ## Analyzers
//!
//! - **Alt Text** (1.1.1): Image alternative text validation
//! - **Contrast** (1.4.3/1.4.6): Color contrast ratio checking
//! - **Semantic** (1.3.1): Semantic HTML structure validation
//! - **ARIA** (4.1.2): ARIA role and attribute validation
//! - **Keyboard** (2.1.1/2.4.7): Keyboard accessibility checking
//! - **Forms** (3.3.2/1.3.5): Form label and autocomplete validation
//! - **Media** (1.2.1/1.2.2): Video caption and audio transcript checking
//! - **Language** (3.1.1/3.1.5): Language attribute and reading level
//! - **CSS** (1.4.4/1.4.12): CSS accessibility best practices

pub mod analyzers;
pub mod fleet;
pub mod report;
pub mod scanner;
