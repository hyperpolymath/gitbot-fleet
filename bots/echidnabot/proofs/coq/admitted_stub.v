(* SPDX-License-Identifier: MPL-2.0 *)
(* hypatia: allow code_safety/admitted -- Deliberate negative fixture checked by protocol_contract::checked_in_negative_proofs_are_detected. *)
(* Dogfood fixture: deliberately admitted Coq proof. coqc exits 0 with
   an admit warning; echidnabot's axiom scanner (src/trust/axiom_tracker)
   detects the Admitted. token and — once Regulator mode is wired — will
   block merges. Until then this round-trips through CI with a warning. *)

Theorem unproven : forall (P Q : Prop), P -> Q.
Proof.
  intros P Q HP.
Admitted.
