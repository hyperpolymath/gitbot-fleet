-- SPDX-License-Identifier: MPL-2.0
-- hypatia: allow code_safety/sorry -- Deliberate negative fixture checked by protocol_contract::checked_in_negative_proofs_are_detected.
-- Dogfood fixture: deliberately incomplete Lean4 proof using sorry.
-- Lean compiles with a warning; Regulator-mode axiom scan will treat
-- sorry as a blocking violation once wired.

theorem unproven (P Q : Prop) (_ : P) : Q := sorry
