Require Import books_proofs.
Require Import book04.
Require Import chapters.
Require Import rules.
Require Import Stdlib.Lists.List.
Import ListNotations.

Theorem book04_well_formed: Forall WellFormedDecision (map (fun (pr: nat * decision) => let (_, d) := pr in d ) chapters).
Proof.
  repeat validate_book.
Qed. 

