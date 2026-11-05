(* The chances of a fight are never negative, as long as it has no poison, whose chance comes from the book. *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import lists.
Require Import proba.
Require Import combat.
Require Import rules.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.QArith.QArith.
Require Import Stdlib.Arith.Arith.
From Stdlib Require Import Lia.

Open Scope Qc_scope.

Definition PNN {K: Set} (p: Proba K) : Prop := forall kv, In kv p -> 0 <= snd kv.

Lemma qc_mul_nn: forall a b: Qc, 0 <= a -> 0 <= b -> 0 <= a * b.
Proof.
  intros a b A B. rewrite <- (Qcmult_0_l b). apply Qcmult_le_compat_r; assumption.
Qed.

Lemma qc_add_nn: forall a b: Qc, 0 <= a -> 0 <= b -> 0 <= a + b.
Proof.
  intros a b A B. rewrite <- (Qcplus_0_l 0). apply Qcplus_le_compat; assumption.
Qed.

Lemma tenth_nn: 0 <= tenth.
Proof. vm_compute. discriminate. Qed.

Lemma pnn_nil {K: Set}: @PNN K [].
Proof. intros kv []. Qed.

Lemma pnn_one {K: Set}: forall (k: K) p, 0 <= p -> PNN [(k, p)].
Proof. intros k p N kv [E|[]]. subst. exact N. Qed.

Lemma pnn_cons {K: Set}: forall (x: K * Qc) m, 0 <= snd x -> PNN m -> PNN (x :: m).
Proof. intros x m A B kv [E|I]; [subst; exact A|apply B; exact I]. Qed.

Lemma pnn_tail {K: Set}: forall (x: K * Qc) m, PNN (x :: m) -> PNN m.
Proof. intros x m A kv I. apply A. right. exact I. Qed.

Lemma pnn_merge_with {K: Set} `{OrdDec K}: forall g (m1 m2 out: Proba K),
  (forall a b, g a b = a + b) -> Merge.MergeWithL g m1 m2 out -> PNN m1 -> PNN m2 -> PNN out.
Proof.
  intros g m1 m2 out G M. induction M; intros A B; auto.
  - apply pnn_cons.
    + simpl. rewrite G. apply qc_add_nn; [exact (A (_, _) (or_introl Logic.eq_refl))|exact (B (_, _) (or_introl Logic.eq_refl))].
    + apply IHM; [exact G|eapply pnn_tail; eauto|eapply pnn_tail; eauto].
  - apply pnn_cons; [exact (A (_, _) (or_introl Logic.eq_refl))|]. apply IHM; [exact G|eapply pnn_tail; eauto|exact B].
  - apply pnn_cons; [exact (B (_, _) (or_introl Logic.eq_refl))|]. apply IHM; [exact G|exact A|eapply pnn_tail; eauto].
Qed.

Lemma pnn_add {K: Set} `{OrdDec K}: forall (m1 m2: Proba K), PNN m1 -> PNN m2 -> PNN (add_proba m1 m2).
Proof.
  intros m1 m2 A B. eapply pnn_merge_with; [|apply Merge.inductive_correct; reflexivity|exact A|exact B]. reflexivity.
Qed.

Lemma pnn_mul {K: Set}: forall (m: Proba K) p, 0 <= p -> PNN m -> PNN (mul_proba m p).
Proof.
  intros m p N A kv I. unfold mul_proba, map_proba in I. apply in_map_iff in I.
  destruct I as [[k q] [E J]]. subst. simpl. apply qc_mul_nn; [exact N|exact (A _ J)].
Qed.

Definition RNN (r: RoundEnd) : Prop := PNN (fst r) /\ PNN (snd r).

Lemma rnn_add: forall a b, RNN a -> RNN b -> RNN (add_round_end a b).
Proof. intros a b [A1 A2] [B1 B2]. split; apply pnn_add; assumption. Qed.

Lemma rnn_over: forall p te, 0 <= p -> RNN (over p te).
Proof. intros p te N. split; [apply pnn_one; exact N|apply pnn_nil]. Qed.

Lemma rnn_next: forall p lw op, 0 <= p -> RNN (next_round p lw op).
Proof. intros p lw op N. split; [apply pnn_nil|apply pnn_one; exact N]. Qed.

Lemma rnn_fold: forall f l, (forall x, In x l -> RNN (f x)) -> RNN (fold_rounds f l).
Proof.
  intros f l. induction l as [|x l IH]; intro A.
  - split; apply pnn_nil.
  - apply rnn_add; [apply A; left; reflexivity|]. apply IH. intros y I. apply A. right. exact I.
Qed.

Lemma rnn_common: forall late stt mds lw op p, 0 <= p -> RNN (common_round late stt mds lw op p).
Proof.
  intros. unfold common_round.
  destruct (lw =? 0)%nat; [apply rnn_over; assumption|].
  destruct (Modifiers.extractl gOnDamage (Modifiers.get_cur_mods mds)).
  - destruct (lw <? curendurance stt)%nat; [apply rnn_over; assumption|].
    destruct (op =? 0)%nat; [apply rnn_over; assumption|apply rnn_next; assumption].
  - destruct (op =? 0)%nat; [apply rnn_over; assumption|apply rnn_next; assumption].
Qed.

Lemma one_round_nn: forall stt osk opphp mds, RNN (one_round stt osk opphp mds).
Proof.
  intros. unfold one_round. apply rnn_fold. intros [lw op] _. apply rnn_common, tenth_nn.
Qed.

(* whether a modifier is poison, now or when its timer ends *)
Fixpoint fm_poison (m: fight_modifier) : bool :=
  match m with
  | Poisonous _ => true
  | Timed _ x => fm_poison x
  | _ => false
  end.

Definition no_poison (mds: list fight_modifier) : bool := forallb (fun m => negb (fm_poison m)) mds.

Lemma no_poison_cur: forall mds, no_poison mds = true ->
  Modifiers.extractl Modifiers.gPoisonous (Modifiers.get_cur_mods mds) = None.
Proof.
  induction mds as [|m q IH]; intro N; [reflexivity|].
  simpl in N. apply andb_prop in N. destruct N as [M R].
  destruct m; simpl in M; try discriminate; try (simpl; apply IH; exact R).
  destruct m; simpl in M; try discriminate; simpl; apply IH; exact R.
Qed.

Lemma no_poison_advance: forall mds, no_poison mds = true -> no_poison (Modifiers.advance_time mds) = true.
Proof.
  induction mds as [|m q IH]; intro N; [reflexivity|].
  simpl in N. apply andb_prop in N. destruct N as [M R]. specialize (IH R).
  destruct m as [| |n x| | | | | | | | | | | | | | | | | | | ]; simpl in M; try discriminate;
    try (simpl; rewrite IH; reflexivity).
  destruct n as [|[|n]]; simpl; try rewrite IH; try rewrite M; try reflexivity;
    destruct (Modifiers.survives_timer x); simpl; try rewrite M; try rewrite IH; reflexivity.
Qed.

Lemma fight_step_nn: forall stt osk opphp mds, no_poison mds = true -> RNN (fight_step stt osk opphp mds).
Proof.
  intros stt osk opphp mds NP. unfold fight_step. cbv zeta.
  destruct (Modifiers.extractl Modifiers.gStopFight _).
  { apply rnn_over. vm_compute. discriminate. }
  destruct (Modifiers.extractl gOnPickOne mds).
  { destruct (round_results stt osk opphp mds) as [|r others]; [split; apply pnn_nil|].
    apply rnn_add; [apply rnn_over, tenth_nn|]. apply rnn_fold. intros [lw op] _.
    destruct (lw =? 0)%nat; [apply rnn_over, tenth_nn|].
    destruct (op =? 0)%nat; [apply rnn_over, tenth_nn|apply rnn_next, tenth_nn]. }
  destruct (Modifiers.extractl Modifiers.gEvaded _).
  { apply rnn_fold. intros [lw op] _.
    destruct (lw =? 0)%nat; [apply rnn_over, tenth_nn|].
    destruct (Modifiers.extractl gOnDamage _); [destruct (lw <? _)%nat|]; apply rnn_over, tenth_nn. }
  rewrite (no_poison_cur mds NP).
  apply rnn_fold. intros [lw op] _. apply rnn_common, tenth_nn.
Qed.

Lemma fight_rounds_nn: forall fuel stt osk frontier mds acc, no_poison mds = true -> PNN frontier -> PNN acc ->
  PNN (fight_rounds fuel stt osk frontier mds acc).
Proof.
  induction fuel as [|f IH]; intros stt osk frontier mds acc NP F A; [exact A|].
  simpl. destruct frontier as [|x q]; [exact A|].
  match goal with |- PNN (fight_rounds _ _ _ (snd ?r) _ (add_proba _ (fst ?r))) => assert (RNN r) as RR end.
  { generalize (x :: q) F. clear IH A. intro l. induction l as [|[[lw op] p] l IHl]; intro F2; simpl.
    - split; apply pnn_nil.
    - pose proof (F2 ((lw, op), p) (or_introl Logic.eq_refl)) as N. simpl in N.
      destruct (fight_step_nn (update_endurance (fun _ => lw) stt) osk op mds NP) as [R1 R2].
      apply rnn_add.
      + split; apply pnn_mul; assumption.
      + apply IHl. eapply pnn_tail; eauto. }
  apply IH; [apply no_poison_advance; exact NP|apply RR|apply pnn_add; [exact A|apply RR]].
Qed.

Lemma fight_nn: forall stt osk opphp mds, no_poison mds = true -> PNN (fight stt osk opphp mds).
Proof.
  intros. unfold fight. apply fight_rounds_nn; auto; [|apply pnn_nil].
  apply pnn_one. vm_compute. discriminate.
Qed.

Lemma merge_nn {K: Set} `{OrdDec K}: forall (l: list (Qc * Proba K)),
  (forall x, In x l -> 0 <= fst x /\ PNN (snd x)) -> PNN (merge_probas l).
Proof.
  induction l as [|[p d] l IH]; intro A.
  - rewrite merge_probas_nil. apply pnn_nil.
  - rewrite merge_probas_cons. destruct (A (p, d)) as [N D]; [left; reflexivity|]. simpl in N, D.
    apply pnn_add; [apply pnn_mul; assumption|]. apply IH. intros x I. apply A. right. exact I.
Qed.

Lemma fight_round_nn: forall stt osk opphp mds, PNN (fight_round stt osk opphp mds).
Proof.
  intros. unfold fight_round. cbv zeta. unfold rebuild_proba. apply merge_nn.
  intros x I. apply in_map_iff in I. destruct I as [a [E I]]. subst x. split; [vm_compute; discriminate|].
  apply in_map_iff in I. destruct I as [dmg [E J]]. subst a. simpl. apply pnn_one. vm_compute. discriminate.
Qed.

(* a mean of what is compared item by item *)
Lemma ev_merge_le {A K: Set} `{OrdDec K}: forall (f: K -> Qc) (l: list A) (G H: A -> Qc * Proba K),
  (forall a, In a l -> fst (G a) = fst (H a) /\ 0 <= fst (G a) /\ ev f (snd (G a)) <= ev f (snd (H a))) ->
  ev f (merge_probas (map G l)) <= ev f (merge_probas (map H l)).
Proof.
  intros f l G1 G2 P. rewrite !ev_merge. induction l as [|a l IH]; simpl; [apply Qcle_refl|].
  destruct (P a) as [A1 [A2 A3]]; [left; reflexivity|]. rewrite <- A1.
  apply Qcplus_le_compat.
  - rewrite (Qcmult_comm (fst (G1 a))), (Qcmult_comm (fst (G1 a))). apply Qcmult_le_compat_r; assumption.
  - apply IH. intros. apply P. right. assumption.
Qed.

Lemma ev_map_le {A K: Set}: forall (f: K -> Qc) (l: list A) (G H: A -> K * Qc),
  (forall a, In a l -> snd (G a) = snd (H a) /\ 0 <= snd (G a) /\ f (fst (G a)) <= f (fst (H a))) ->
  ev f (map G l) <= ev f (map H l).
Proof.
  intros f l G1 G2 P. unfold ev. induction l as [|a l IH]; simpl; [apply Qcle_refl|].
  destruct (P a) as [A1 [A2 A3]]; [left; reflexivity|]. rewrite <- A1.
  apply Qcplus_le_compat.
  - rewrite (Qcmult_comm (snd (G1 a))), (Qcmult_comm (snd (G1 a))). apply Qcmult_le_compat_r; assumption.
  - apply IH. intros. apply P. right. assumption.
Qed.
