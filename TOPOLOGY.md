<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# Gitbot Fleet — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              OPERATOR / ADMIN           │
                        │        (Fleet Coordinator, Dashy)       │
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           GITBOT FLEET HUB              │
                        │                                         │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │ rhodibot  │  │  echidnabot       │  │
                        │  │ (RSR)     │  │  (Verification)   │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        │        │                 │              │
                        │  ┌─────▼─────┐  ┌────────▼──────────┐  │
                        │  │ sustaina- │  │  shared-context   │  │
                        │  │ bot (Eco) │  │  layer            │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │           AUXILIARY BOTS                │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ glambot   │  │ seambot   │  │ finish││
                        │  │ (UI/SEO)  │  │ (Integr)  │  │ bot   ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │          TARGET ECOSYSTEM               │
                        │      (All Hyperpolymath Repos)          │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile / Docker  .machine_readable/  │
                        │  OPSM Integration   0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE FLEET
  rhodibot (RSR Compliance)         ██████████ 100%    Structural checks active
  echidnabot (Verification)         ██████████ 100%    Math/Statistical proofs stable
  sustainabot (Ecological)          ██████████ 100%    Carbon scoring verified
  Shared Context Layer              ██████████ 100%    Inter-bot state management

SUPPORT BOTS
  glambot (Presentation)            ██████████ 100%    WCAG & SEO checks stable
  seambot (Integration)             ██████████ 100%    Cross-component tests active
  finishbot (Release)               ██████████ 100%    Validation checklist complete

INFRASTRUCTURE
  Fleet Coordinator                 ██████████ 100%    Batch orchestration stable
  Dashboard (Dashy)                 ████████░░  80%    Real-time status refining
  .machine_readable/                ██████████ 100%    STATE tracking active

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ██████████ 100%    Full quality fleet active
```

## Key Dependencies

```
Shared Context ───► rhodibot ───────► sustainabot ──────► finishbot
     │                 │                 │                  │
     ▼                 ▼                 ▼                  ▼
 echidnabot ─────► glambot ────────► seambot ────────► Release Ready
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
