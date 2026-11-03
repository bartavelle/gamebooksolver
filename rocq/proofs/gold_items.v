(* The gold of two sets of items: they are the same apart from their gold, and their gold is the same or enough. *)
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
From Stdlib Require Import Lia.
Require Import Stdlib.QArith.Qcanon.
Open Scope nat_scope.

Definition gold_of (m: Items) : nat := match lookup Gold m with Some x => x | None => 0 end.

Definition SameM (a b: Items) : Prop :=
  ValidMap a /\ ValidMap b /\ forall i, i <> Gold -> lookup i a = lookup i b.

(* the amount of gold of two sets of items is the same, or both have enough *)
Definition GoldRel (n: nat) (a b: Items) : Prop :=
  lookup Gold a = lookup Gold b \/ (n <= gold_of a /\ n <= gold_of b /\ n < ALL).

Lemma add_form: forall i q, q <> 0 -> exists f v, forall m, add_item i q m = insert_with f i v m.
Proof.
  intros i q Q. unfold add_item. destruct (q =? 0) eqn: E; [apply Nat.eqb_eq in E; contradiction|].
  destruct i; repeat eexists; intros; reflexivity.
Qed.

Lemma same_add: forall i q a b, SameM a b -> SameM (add_item i q a) (add_item i q b).
Proof.
  intros i q a b [VA [VB E]]. destruct (Nat.eq_dec q 0) as [Q|Q].
  { subst. unfold add_item. simpl. split; auto. }
  destruct (add_form i q Q) as [f [v A]]. rewrite !A.
  split; [apply Insert.insert_with_valid; auto|]. split; [apply Insert.insert_with_valid; auto|].
  intros j J. destruct (eq_dec j i).
  - subst. rewrite !Lookup.lookup_insert_with; auto. rewrite E; auto.
  - rewrite !Lookup.lookup_insert_with_diff; auto.
Qed.

Lemma add_gold_other: forall i q a, i <> Gold -> ValidMap a -> lookup Gold (add_item i q a) = lookup Gold a.
Proof.
  intros i q a NG VA. destruct (Nat.eq_dec q 0) as [Q|Q].
  { subst. reflexivity. }
  destruct (add_form i q Q) as [f [v A]]. rewrite A.
  apply Lookup.lookup_insert_with_diff; auto.
Qed.

Lemma add_gold_lookup: forall q a, q <> 0 -> ValidMap a ->
  lookup Gold (add_item Gold q a) = Some (if gold_of a + q <? 51 then gold_of a + q else 50).
Proof.
  intros q a Q VA. unfold add_item. destruct (q =? 0) eqn: E; [apply Nat.eqb_eq in E; contradiction|].
  rewrite Lookup.lookup_insert_with; auto. unfold gold_of.
  destruct (lookup Gold a) as [x|]; auto. simpl. f_equal.
  destruct (q <? 51) eqn: L; [apply Nat.ltb_lt in L; lia|apply Nat.ltb_ge in L; lia].
Qed.

Lemma rel_add_gold: forall n q a b, ValidMap a -> ValidMap b -> GoldRel n a b -> GoldRel n (add_item Gold q a) (add_item Gold q b).
Proof.
  intros n q a b VA VB G. destruct (Nat.eq_dec q 0) as [Q|Q].
  { subst. exact G. }
  assert (forall m, ValidMap m -> gold_of (add_item Gold q m) = if gold_of m + q <? 51 then gold_of m + q else 50) as GO.
  { intros m VM. unfold gold_of at 1. rewrite add_gold_lookup; auto. }
  unfold GoldRel in *. rewrite (GO a VA), (GO b VB), !add_gold_lookup; auto.
  destruct G as [G|[A [B C]]].
  - left. unfold gold_of. rewrite G. reflexivity.
  - destruct (Nat.ltb_spec (gold_of a + q) 51), (Nat.ltb_spec (gold_of b + q) 51);
      first [right; lia | left; reflexivity | exfalso; lia].
Qed.

Lemma same_rm: forall i q a b, SameM a b -> SameM (rm_item i q a) (rm_item i q b).
Proof.
  intros i q a b [VA [VB E]]. unfold rm_item, bag_rm.
  split; [apply Update.valid; auto|]. split; [apply Update.valid; auto|].
  intros j J. destruct (eq_dec j i).
  - subst. rewrite !Lookup.update_eq_b; auto. rewrite E; auto.
  - rewrite !Lookup.update_diff; auto.
Qed.

Lemma rel_rm_other: forall n i q a b, i <> Gold -> ValidMap a -> ValidMap b -> GoldRel n a b -> GoldRel n (rm_item i q a) (rm_item i q b).
Proof.
  intros n i q a b NG VA VB G. unfold rm_item, bag_rm, GoldRel, gold_of in *.
  rewrite !Lookup.update_diff; auto.
Qed.

Lemma gold_rm: forall q a, ValidMap a -> gold_of (rm_item Gold q a) = gold_of a - q.
Proof.
  intros q a VA. unfold gold_of, rm_item, bag_rm. rewrite Lookup.update_eq_b; auto.
  destruct (lookup Gold a) as [x|]; auto. destruct (x <=? q) eqn: E; [apply Nat.leb_le in E; lia|reflexivity].
Qed.

Lemma lookup_rm_gold: forall q a, ValidMap a -> lookup Gold (rm_item Gold q a) =
  match lookup Gold a with None => None | Some cv => if cv <=? q then None else Some (cv - q) end.
Proof.
  intros q a VA. unfold rm_item, bag_rm. rewrite Lookup.update_eq_b; auto.
Qed.

Lemma same_delete: forall i a b, SameM a b -> SameM (delete i a) (delete i b).
Proof.
  intros i a b [VA [VB E]]. split; [apply Delete.valid; auto|]. split; [apply Delete.valid; auto|].
  intros j J. destruct (eq_dec j i).
  - subst. rewrite !Lookup.lookup_delete; auto.
  - rewrite !Lookup.lookup_delete_diff; auto.
Qed.

Lemma rel_delete: forall n i a b, ValidMap a -> ValidMap b -> GoldRel n a b -> GoldRel n (delete i a) (delete i b).
Proof.
  intros n i a b VA VB G. destruct (eq_dec i Gold).
  - subst. left. rewrite !Lookup.lookup_delete; auto.
  - unfold GoldRel, gold_of in *. rewrite !Lookup.lookup_delete_diff; auto.
Qed.

Lemma same_slot: forall sl a b, SameM a b -> SameM (lose_all_slot sl a) (lose_all_slot sl b).
Proof.
  intros sl a b H. unfold lose_all_slot. induction (slot_items sl); simpl; auto. apply same_delete. assumption.
Qed.

Lemma rel_slot: forall n sl a b, SameM a b -> GoldRel n a b -> GoldRel n (lose_all_slot sl a) (lose_all_slot sl b).
Proof.
  intros n sl a b S G. unfold lose_all_slot.
  induction (slot_items sl) as [|i q IH]; simpl; auto.
  assert (SameM (fold_right (fun i itms => delete i itms) a q) (fold_right (fun i itms => delete i itms) b q)) as S2.
  { clear -S. induction q; simpl; auto. apply same_delete. assumption. }
  destruct S2 as [V1 [V2 _]]. apply rel_delete; auto.
Qed.

Lemma same_slots: forall sls a b, SameM a b -> SameM (fold_right lose_all_slot a sls) (fold_right lose_all_slot b sls).
Proof.
  induction sls; intros; simpl; auto. apply same_slot. auto.
Qed.

Lemma rel_slots: forall n sls a b, SameM a b -> GoldRel n a b -> GoldRel n (fold_right lose_all_slot a sls) (fold_right lose_all_slot b sls).
Proof.
  induction sls; intros; simpl; auto. apply rel_slot; auto. apply same_slots. assumption.
Qed.

Lemma filter_same: forall (p: item * nat -> bool) a b, SameM a b -> (forall v, p (Gold, v) = false) -> filter p a = filter p b.
Proof.
  intros p a b [VA [VB E]] P. apply Lookup.map_ext_valid; try (apply Valid.filter; assumption).
  intro k. rewrite !Lookup.lookup_filter; auto. destruct (eq_dec k Gold).
  - subst. destruct (lookup Gold a), (lookup Gold b); rewrite ?P; reflexivity.
  - rewrite E; auto.
Qed.

(* Relating totals, when the gains are not the same: see gold_retrieve.v *)
Lemma rel_congr: forall n a b a2 b2, lookup Gold a = lookup Gold a2 -> lookup Gold b = lookup Gold b2 ->
  GoldRel n a b -> GoldRel n a2 b2.
Proof.
  intros n a b a2 b2 A B. unfold GoldRel, gold_of. rewrite <- A, <- B. tauto.
Qed.

Lemma add_congr: forall q a b, q <> 0 -> ValidMap a -> ValidMap b -> lookup Gold a = lookup Gold b ->
  lookup Gold (add_item Gold q a) = lookup Gold (add_item Gold q b).
Proof.
  intros q a b Q VA VB E. rewrite !add_gold_lookup; auto. unfold gold_of. rewrite E. reflexivity.
Qed.

(* the gold is the sum of what was stored and what is held *)
Lemma rel_total: forall a n p q x y, a <= n -> ValidMap p -> ValidMap q -> ValidMap x -> ValidMap y ->
  gold_of x <> 0 -> gold_of y <> 0 -> GoldRel n p q -> GoldRel n x y ->
  GoldRel a (add_item Gold (gold_of x) p) (add_item Gold (gold_of y) q).
Proof.
  intros a n p q x y L VP VQ VX VY GX GY RP RX.
  assert (forall k m, k <> 0 -> ValidMap m -> gold_of (add_item Gold k m) = if gold_of m + k <? 51 then gold_of m + k else 50) as GO.
  { intros k m K VM. unfold gold_of at 1. rewrite add_gold_lookup; auto. }
  unfold GoldRel. rewrite (GO _ p GX VP), (GO _ q GY VQ), !add_gold_lookup; auto.
  assert (gold_of p = gold_of q \/ (n <= gold_of p /\ n <= gold_of q /\ n < ALL)) as RP2.
  { destruct RP as [E|E]; [left; unfold gold_of; rewrite E; reflexivity|right; exact E]. }
  assert (gold_of x = gold_of y \/ (n <= gold_of x /\ n <= gold_of y /\ n < ALL)) as RX2.
  { destruct RX as [E|E]; [left; unfold gold_of; rewrite E; reflexivity|right; exact E]. }
  destruct (Nat.ltb_spec (gold_of p + gold_of x) 51), (Nat.ltb_spec (gold_of q + gold_of y) 51);
    destruct RP2 as [E1|[A1 [B1 C1]]], RX2 as [E2|[A2 [B2 C2]]];
    first [left; f_equal; lia | right; unfold ALL in *; lia | left; reflexivity | exfalso; lia].
Qed.

Lemma add_gold_keep: forall i q m, i <> Gold -> ValidMap m -> lookup i (add_item Gold q m) = lookup i m.
Proof.
  intros i q m NG VM. destruct (Nat.eq_dec q 0) as [Q|Q]; [subst; reflexivity|].
  destruct (add_form Gold q Q) as [g [v A]]. rewrite A. apply Lookup.lookup_insert_with_diff; auto.
Qed.

Lemma same_add_gold: forall q1 q2 a b, SameM a b -> SameM (add_item Gold q1 a) (add_item Gold q2 b).
Proof.
  intros q1 q2 a b [VA [VB E]].
  assert (forall q m, ValidMap m -> ValidMap (add_item Gold q m)) as AV.
  { intros q m VM. assert (SameM m m) as S by (split; [assumption|split; [assumption|reflexivity]]).
    apply (same_add Gold q) in S. apply S. }
  split; [auto|]. split; [auto|]. intros i I. rewrite !add_gold_keep; auto.
Qed.

Lemma not_gold_slot: forall sl i, sl <> SPouch -> In i (slot_items sl) -> i <> Gold.
Proof.
  intros sl i N I E. subst. apply character.Slot.in_slot_items_slot in I. simpl in I. subst. contradiction.
Qed.

Lemma slot_gold: forall a, ValidMap a -> lookup Gold (lose_all_slot SBackpack a) = lookup Gold a.
Proof.
  intros a VA. unfold lose_all_slot.
  assert (forall i, In i (slot_items SBackpack) -> i <> Gold) as NG by (intros; eapply not_gold_slot; eauto; discriminate).
  induction (slot_items SBackpack) as [|i q IH]; simpl; auto.
  assert (SameM (fold_right (fun i itms => delete i itms) a q) (fold_right (fun i itms => delete i itms) a q)) as S2.
  { clear -VA. induction q; simpl; [split; [assumption|split; [assumption|reflexivity]]|]. apply same_delete. assumption. }
  destruct S2 as [V1 _]. rewrite Lookup.lookup_delete_diff; auto.
  - apply IH. intros j J. apply NG. right. exact J.
  - intro E. subst. apply (NG Gold); [left; reflexivity|reflexivity].
Qed.
