-- SPDX-License-Identifier: MPL-2.0
-- Dogfood fixture: trivial passing Lean4 proof.

theorem identity (A : Prop) (h : A) : A := h
