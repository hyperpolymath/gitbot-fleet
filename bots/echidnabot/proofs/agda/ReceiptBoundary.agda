-- SPDX-License-Identifier: MPL-2.0
{-# OPTIONS --safe --without-K --cubical-compatible --exact-split --no-sized-types --no-guardedness #-}

-- Theory only: no theorem here establishes behaviour of Rust, JSON parsers,
-- network routing, a prover, or cryptographic authentication.
module ReceiptBoundary where

-- Small constructive core, with no standard-library dependency.
data Empty : Set where

record Unit : Set where
  constructor unit

-- A receipt carries evidence of some explicitly named type. There is no
-- implicit operation converting that evidence into the claimed proposition.
record Warrant (Claim : Set) : Set₁ where
  constructor warrant
  field
    Evidence : Set
    evidence : Evidence

-- Soundness is extra structure. It cannot be obtained just by receiving a
-- token, validating its JSON shape, or assigning it a confidence level.
record SoundWarrant {Claim : Set} (w : Warrant Claim) : Set where
  constructor sound-warrant
  field
    sound : Warrant.Evidence w → Claim

receive-with-soundness : {Claim : Set} → (w : Warrant Claim) →
                        SoundWarrant w → Claim
receive-with-soundness w checked =
  SoundWarrant.sound checked (Warrant.evidence w)

-- A token can exist even when the claim has no inhabitant.
empty-claim-token : Warrant Empty
empty-claim-token = warrant Unit unit

-- Concrete constructive counterexample to any *unconditional* factivity
-- interface for arbitrary receipt/warrant tokens.
no-unconditional-reflection :
  ((Claim : Set) → Warrant Claim → Claim) → Empty
no-unconditional-reflection reflect = reflect Empty empty-claim-token

-- In particular this receipt cannot possess a soundness function.
no-soundness-for-empty-token : SoundWarrant empty-claim-token → Empty
no-soundness-for-empty-token checked =
  receive-with-soundness empty-claim-token checked
