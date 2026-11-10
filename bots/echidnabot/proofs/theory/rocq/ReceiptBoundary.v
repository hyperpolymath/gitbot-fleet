(* SPDX-License-Identifier: MPL-2.0 *)
(* Independent re-proof candidate, NOT checked evidence. No libraries,
   axioms or admitted obligations. The explicit small-type scope must be
   compared with Agda Set before correspondence is claimed. *)

Record ReceiptWarrant (Claim : Set) : Type := {
  evidence_type : Set;
  evidence_token : evidence_type
}.

Arguments evidence_type {Claim} _.
Arguments evidence_token {Claim} _.

Record SoundReceiptWarrant {Claim : Set} (w : ReceiptWarrant Claim) : Type := {
  sound : evidence_type w -> Claim
}.

Arguments sound {Claim w} _ _.

(** Derive a claim from a receipt warrant and an explicit soundness witness. *)
Definition receive_with_soundness {Claim : Set}
    (w : ReceiptWarrant Claim) (checked : SoundReceiptWarrant w) : Claim :=
  sound checked (evidence_token w).

Definition empty_claim_token : ReceiptWarrant Empty_set :=
  {| evidence_type := unit; evidence_token := tt |}.

(** An unconditional reflection function for every claim yields a contradiction. *)
Definition no_unconditional_reflection
    (reflect : forall Claim : Set, ReceiptWarrant Claim -> Claim) : Empty_set :=
  reflect Empty_set empty_claim_token.

(** A soundness witness for the empty-claim token yields a contradiction. *)
Definition no_soundness_for_empty_token
    (checked : SoundReceiptWarrant empty_claim_token) : Empty_set :=
  receive_with_soundness empty_claim_token checked.

Print Assumptions receive_with_soundness.
Print Assumptions no_unconditional_reflection.
Print Assumptions no_soundness_for_empty_token.
