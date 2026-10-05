(* SPDX-License-Identifier: MPL-2.0 *)
(* Dogfood fixture: trivial passing Coq proof — exercises the
   proofs/** path trigger in .github/workflows/echidnabot.yml. *)

Theorem identity : forall (A : Prop), A -> A.
Proof.
  intros A HA.
  exact HA.
Qed.
