Require Import chapters.
Require Import character.
Require Import rules.
Require Import proba.
Require Import Stdlib.QArith.Qcanon.

Require Import Stdlib.Lists.List.
Import ListNotations.

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
