# Proof Requirements

## Current state
- ABI directory exists (template-level)
- No dangerous patterns found
- 100K lines; bot fleet for repository quality enforcement
- Claims: "formal mathematical/statistical verification"

## What needs proving
- **Bot action safety**: Prove that automated bot actions (PRs, issues, fixes) are reversible and do not destroy repository state
- **Confidence threshold correctness**: Prove that the robot-repo-automaton confidence thresholds correctly gate automated merges (no auto-merge below threshold)
- **Rule conflict resolution**: Prove that when multiple bots (rhodibot, echidnabot, sustainabot, etc.) produce conflicting recommendations, the resolution is deterministic and documented
- **Rate limiting**: Prove bots respect GitHub API rate limits and do not cause cascading failures across the fleet

## Recommended prover
- **Idris2** — For bot action safety and confidence threshold properties
- **Lean4** — For algebraic properties of confidence scoring if modeled as an ordered semiring

## Priority
- **MEDIUM** — The fleet manages automated repository changes. Incorrect bot behavior at scale could damage many repos. The "formal verification" claim should be substantiated.

## Template ABI Cleanup (2026-03-29)

Template ABI removed -- was creating false impression of formal verification.
The removed files (Types.idr, Layout.idr, Foreign.idr) contained only RSR template
scaffolding with unresolved {{PROJECT}}/{{AUTHOR}} placeholders and no domain-specific proofs.
