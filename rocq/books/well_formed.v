(* The transcribed books are well formed. These are stated but not proved yet: validate_book is the tactic meant for them. *)
Require Import chapters.
Require Import rules.
Require Import proba.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require book01 book02 book03 book04 book05.

Ltac solve_always_true :=
  tryif (
    match goal with
    | |- exists cnd co, In (cnd, co) ?lst /\ _ =>
        match lst with
        | context [(Always true, _)] => idtac
        end
    end
  )
  then (
    (* Always true found - solve it *)
    match goal with
    | |- exists cnd co, In (cnd, co) ?lst /\ check_cond ?stt cnd = true =>
        match lst with
        | context [(Always true, ?outcome)] =>
            exists (Always true); eexists;
            split;
            [ simpl; auto 10
            | simpl; reflexivity
            ]
        end
    end
  )
  else fail.

Ltac validate_book :=
  match goal with
  | |- Forall _ _  => constructor
  | |- WellFormedDecision _  => constructor
  | |- WellFormedCO _  => constructor
  | |- FullProba _  => vm_compute; apply Qceq_alt; vm_compute; reflexivity
  | |- forall _, _ => intro
  | |- exists _, _ => solve_always_true
  end.

Definition decisions_of (chapters: list (cid * decision)) : list decision := map snd chapters.

Theorem book01_well_formed: Forall WellFormedDecision (decisions_of book01.chapters).
Proof.
Admitted.

Theorem book02_well_formed: Forall WellFormedDecision (decisions_of book02.chapters).
Proof.
Admitted.

Theorem book03_well_formed: Forall WellFormedDecision (decisions_of book03.chapters).
Proof.
Admitted.

Theorem book04_well_formed: Forall WellFormedDecision (decisions_of book04.chapters).
Proof.
Admitted.

Theorem book05_well_formed: Forall WellFormedDecision (decisions_of book05.chapters).
Proof.
Admitted.
