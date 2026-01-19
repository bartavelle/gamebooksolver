Require Import books_proofs.
Require Import book01.
Require Import chapters.
Require Import rules.
Require Import Stdlib.Lists.List.
Require Import Stdlib.QArith.Qcanon.
Import ListNotations.

Theorem book01_well_formed: Forall WellFormedDecision (map (fun (pr: nat * decision) => let (_, d) := pr in d ) book01.chapters).
Proof.
  repeat validate_book.
Qed. 
