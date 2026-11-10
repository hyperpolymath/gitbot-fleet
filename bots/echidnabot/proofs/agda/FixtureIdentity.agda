-- SPDX-License-Identifier: MPL-2.0
{-# OPTIONS --safe --without-K --cubical-compatible --exact-split --no-sized-types --no-guardedness #-}

-- Semantic reconstruction of the constructive identity shared by the
-- positive Lean/Coq dogfood fixtures, not a translation receipt.
-- Agda Set differs from Rocq Prop and Lean Prop; correspondence is restricted
-- to the propositional identity schema, not arbitrary universe conversion.
module FixtureIdentity where

identity : (A : Set) → A → A
identity A evidence = evidence
