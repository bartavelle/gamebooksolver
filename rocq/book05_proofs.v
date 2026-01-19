Require Import books_proofs.
Require Import book05.
Require Import chapters.
Require Import rules.
Require Import Stdlib.Lists.List.
Import ListNotations.


Theorem book05_well_formed: Forall WellFormedDecision (map (fun (pr: nat * decision) => let (_, d) := pr in d ) chapters).
Proof.
  repeat validate_book; try tauto.
Qed. 


