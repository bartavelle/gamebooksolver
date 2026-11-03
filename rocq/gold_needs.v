(* The table of the gold each chapter needs, and what it is worth: a state can have its gold replaced by anything that
   reaches the need of its chapter. *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import lists.
Require Import proba.
Require Import combat.
Require Import rules.
Require Import value.
Require Import fight_facts.
Require Import simulation.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
Require Import Stdlib.FSets.FMapPositive.
Require Import Stdlib.PArith.PArith.
From Stdlib Require Import Lia.
Require Import Stdlib.QArith.Qcanon.
Open Scope nat_scope.
Require Import gold_items.
Require Import gold_relation.
Require Import gold_retrieve.

Module PM := FMapPositive.PositiveMap.

Definition need_of (t: PM.t nat) (c: cid) : nat :=
  match PM.find (Pos.of_succ_nat c) t with Some v => v | None => 0 end.

Definition gold_dec_need (t: PM.t nat) (wn: nat) (d: decision) : nat := dec_need gold_tracker (need_of t) wn d.

(* a table is fine if no chapter needs more than what it says *)
(* getting the equipment back when entering a chapter only needs what comes next *)
Definition gold_top_need (t: PM.t nat) (wn: nat) (d: decision) : nat :=
  match d with
  | retrieve_equipment n => Nat.max 1 (gold_dec_need t wn n)
  | _ => gold_dec_need t wn d
  end.

Definition needs_ok (chapters: list (cid * decision)) (wn: nat) (t: PM.t nat) : bool :=
  forallb (fun cd : cid * decision => gold_top_need t wn (snd cd) <=? need_of t (fst cd)) chapters.

Fixpoint needs_from (chapters: list (cid * decision)) (wn: nat) (fuel: nat) (t: PM.t nat) : PM.t nat :=
  match fuel with
  | O => t
  | S f =>
      let nt := fold_left (fun acc (cd: cid * decision) =>
        PM.add (Pos.of_succ_nat (fst cd)) (Nat.min ALL (gold_top_need acc wn (snd cd))) acc) chapters t in
      needs_from chapters wn f nt
  end.

(* The gold of a state can be replaced by anything that is at least what its chapter needs *)
Theorem gold_needs_sound: forall chapters wn score t,
  needs_ok chapters wn t = true ->
  (forall s x, GoldR wn s x -> score s = score x) ->
  forall fuel s x, GoldR (need_of t (chapter s)) s x -> value chapters score fuel s = value chapters score fuel x.
Proof.
  intros chapters wn score t OK HS fuel s x H.
  apply (value_same_top gold_tracker (need_of t) wn chapters score HS); auto.
  intros c d F. apply find_chapter_in in F. unfold needs_ok in OK. rewrite forallb_forall in OK.
  specialize (OK _ F). apply Nat.leb_le in OK. cbn [snd fst] in OK.
  destruct d; try (left; exact OK).
  right. apply retrieve_top; [eapply Nat.le_trans; [apply Nat.le_max_l|exact OK]|eapply Nat.le_trans; [apply Nat.le_max_r|exact OK]].
Qed.
