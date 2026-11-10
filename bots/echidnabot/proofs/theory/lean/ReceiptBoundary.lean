-- SPDX-License-Identifier: MPL-2.0
-- Independent re-proof candidate, NOT checked evidence. Core only; no Mathlib.
-- Type 0 scope must be compared with Agda Set and Rocq Set explicitly.
namespace EchidnabotReceiptBoundary

structure ReceiptWarrant (Claim : Type) : Type 1 where
  Evidence : Type
  evidence : Evidence

structure SoundReceiptWarrant {Claim : Type} (w : ReceiptWarrant Claim) where
  sound : w.Evidence → Claim

/-- Derive a claim from a receipt warrant and an explicit soundness witness. -/
def receiveWithSoundness {Claim : Type}
    (w : ReceiptWarrant Claim) (checked : SoundReceiptWarrant w) : Claim :=
  checked.sound w.evidence

def emptyClaimToken : ReceiptWarrant Empty :=
  { Evidence := Unit, evidence := () }

/-- An unconditional reflection function for every claim yields a contradiction. -/
def noUnconditionalReflection
    (reflect : (Claim : Type) → ReceiptWarrant Claim → Claim) : Empty :=
  reflect Empty emptyClaimToken

/-- A soundness witness for the empty-claim token yields a contradiction. -/
def noSoundnessForEmptyToken
    (checked : SoundReceiptWarrant emptyClaimToken) : Empty :=
  receiveWithSoundness emptyClaimToken checked

#print axioms receiveWithSoundness
#print axioms noUnconditionalReflection
#print axioms noSoundnessForEmptyToken

end EchidnabotReceiptBoundary
