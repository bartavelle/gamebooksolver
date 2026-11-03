(* A fight never gives endurance: this is all that keeping a state valid needs from them. *)
Require Import character chapters mps order lists proba combat.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.QArith.Qcanon.
From Stdlib Require Import Lia.
Open Scope nat_scope.

Definition te_le (cur: nat) (te: TEscaped) : Prop :=
  match te with
  | Escaped _ h | Std h | LateWin _ h | Stopped _ h => h <= cur
  | TLost _ => True
  end.

Definition end_le (cur: nat) (r: RoundEnd) : Prop :=
  (forall te, In te (map fst (fst r)) -> te_le cur te) /\ (forall k, In k (map fst (snd r)) -> fst k <= cur).

Lemma end_le_over: forall cur p te, te_le cur te -> end_le cur (over p te).
Proof.
  intros cur p te H. split.
  - intros x I. simpl in I. destruct I as [I|[]]. subst. exact H.
  - intros k I. destruct I.
Qed.

Lemma end_le_next: forall cur p lw op, lw <= cur -> end_le cur (next_round p lw op).
Proof.
  intros cur p lw op H. split.
  - intros x I. destruct I.
  - intros k I. simpl in I. destruct I as [I|[]]. subst. exact H.
Qed.

Lemma end_le_add: forall cur a b, end_le cur a -> end_le cur b -> end_le cur (add_round_end a b).
Proof.
  intros cur a b [A1 A2] [B1 B2]. split.
  - intros x I. destruct (add_proba_keeps_keys_conv _ _ _ x Logic.eq_refl I); auto.
  - intros x I. destruct (add_proba_keeps_keys_conv _ _ _ x Logic.eq_refl I); auto.
Qed.

Lemma end_le_nil: forall cur, end_le cur ([], []).
Proof.
  intro. split; intros x I; destruct I.
Qed.

Lemma end_le_mono: forall a b r, a <= b -> end_le a r -> end_le b r.
Proof.
  intros a b r L [A B]. split.
  - intros x I. specialize (A x I). destruct x; simpl in *; auto; lia.
  - intros x I. specialize (B x I). lia.
Qed.

Lemma end_le_fold: forall cur g l, (forall x, In x l -> end_le cur (g x)) -> end_le cur (fold_rounds g l).
Proof.
  intros cur g l H. unfold fold_rounds. induction l as [|x l IH]; simpl.
  - apply end_le_nil.
  - apply end_le_add; [apply H; left; reflexivity|]. apply IH. intros. apply H. right. assumption.
Qed.

Lemma round_results_le: forall s osk op mds r, In r (round_results s osk op mds) -> fst r <= curendurance s.
Proof.
  intros s osk op mds r I. unfold round_results in I. apply in_map_iff in I. destruct I as [[a b] [E _]]. subst r.
  unfold moddmg. simpl. lia.
Qed.

Lemma lost_le: forall cur mds, te_le cur (lost_result mds).
Proof.
  intros cur mds. unfold lost_result. destruct (Modifiers.extractl _ _); simpl; lia || exact I.
Qed.

Lemma win_le: forall cur late mds lw, lw <= cur -> te_le cur (win_result late mds lw).
Proof.
  intros cur late mds lw L. unfold win_result. destruct (Modifiers.extractl _ _); [destruct late|]; exact L.
Qed.

Lemma common_le: forall cur late s mds lw op p, lw <= cur -> end_le cur (common_round late s mds lw op p).
Proof.
  intros cur late s mds lw op p L. unfold common_round.
  destruct (lw =? 0). { apply end_le_over, lost_le. }
  destruct (Modifiers.extractl gOnDamage _).
  - destruct (lw <? curendurance s). { apply end_le_over. exact L. }
    destruct (op =? 0); [apply end_le_over, win_le; exact L|apply end_le_next; exact L].
  - destruct (op =? 0); [apply end_le_over, win_le; exact L|apply end_le_next; exact L].
Qed.

Lemma poison_le: forall cur mds lw0 (total: Qc) (l: list (bool * nat * Qc)), lw0 <= cur ->
  end_le cur (fold_right (fun (c: bool * nat * Qc) acc =>
      let '(k, op, w) := c in
      let p := (w / total)%Qc in
      add_round_end
        (if k then over p (lost_result mds)
         else if (op =? 0)%nat then over p (Std lw0)
         else next_round p lw0 op) acc) ([], []) l).
Proof.
  intros cur mds lw0 total l L. induction l as [|[[k o] w] tl IH]; simpl.
  { apply end_le_nil. }
  apply end_le_add; auto.
  destruct k; [apply end_le_over, lost_le|].
  destruct (o =? 0); [apply end_le_over; exact L|apply end_le_next; exact L].
Qed.

Lemma fight_step_le: forall s osk op mds, end_le (curendurance s) (fight_step s osk op mds).
Proof.
  intros s osk op mds. unfold fight_step.
  destruct (Modifiers.extractl Modifiers.gStopFight _). { apply end_le_over. apply le_n. }
  destruct (Modifiers.extractl gOnPickOne _).
  { destruct (round_results s osk op mds) as [|r others] eqn: RS.
    - apply end_le_nil.
    - apply end_le_add. { apply end_le_over. apply le_n. }
      apply end_le_fold. intros [lw o] I.
      assert (lw <= curendurance s) as L.
      { apply (round_results_le s osk op mds (lw, o)). rewrite RS. right. exact I. }
      destruct (lw =? 0). { apply end_le_over, lost_le. }
      destruct (o =? 0); [apply end_le_over; exact L|apply end_le_next; exact L]. }
  destruct (Modifiers.extractl Modifiers.gEvaded _).
  { apply end_le_fold. intros [lw o] I.
    pose proof (round_results_le s osk op mds (lw, o) I) as L. simpl in L.
    destruct (lw =? 0). { apply end_le_over, lost_le. }
    destruct (Modifiers.extractl gOnDamage _); [destruct (lw <? curendurance s)|]; apply end_le_over; exact L. }
  destruct (Modifiers.extractl Modifiers.gPoisonous _).
  { unfold poisonous_round. apply poison_le. apply le_n. }
  apply end_le_fold. intros [lw o] I. apply common_le.
  exact (round_results_le s osk op mds (lw, o) I).
Qed.

Lemma fight_rounds_le: forall fuel s osk fr mds acc cur,
  (forall k, In k (map fst fr) -> fst k <= cur) ->
  (forall te, In te (map fst acc) -> te_le cur te) ->
  forall te, In te (map fst (fight_rounds fuel s osk fr mds acc)) -> te_le cur te.
Proof.
  induction fuel as [|fuel IH]; intros s osk fr mds acc cur Hf Ha te I; simpl in I; auto.
  destruct fr as [|x tl]; auto.
  remember (x :: tl) as frr.
  assert (end_le cur (fold_right (fun (st: (nat * nat) * Qc) cu =>
      let '((lw, op), p) := st in
      let r := fight_step (update_endurance (fun _ => lw) s) osk op mds in
      add_round_end (mul_proba (fst r) p, mul_proba (snd r) p) cu) ([], []) frr)) as ST.
  { clear -Hf. induction frr as [|[[lw op] p] l IHl]; simpl.
    - apply end_le_nil.
    - apply end_le_add.
      + pose proof (fight_step_le (update_endurance (fun _ => lw) s) osk op mds) as E.
        assert (lw <= cur) as L by (apply (Hf (lw, op)); left; reflexivity).
        apply (end_le_mono _ cur) in E; [|exact L].
        destruct E as [E1 E2]. split.
        * intros t It. simpl in It. unfold mul_proba in It. rewrite <- (map_proba_keeps_keys _ _ _ Logic.eq_refl) in It. auto.
        * intros t It. simpl in It. unfold mul_proba in It. rewrite <- (map_proba_keeps_keys _ _ _ Logic.eq_refl) in It. auto.
      + apply IHl. intros. apply Hf. right. assumption. }
  destruct ST as [S1 S2].
  eapply IH; [exact S2| |exact I].
  intros t It. destruct (add_proba_keeps_keys_conv _ _ _ t Logic.eq_refl It); auto.
Qed.

(* the number of rounds stays out of the proofs, as the kernel would otherwise try to compute with it *)
Lemma fight_le_fuel: forall fuel s osk op mds te,
  In te (map fst (fight_rounds fuel s osk [((curendurance s, op), 1%Qc)] mds [])) -> te_le (curendurance s) te.
Proof.
  intros fuel s osk op mds te I. eapply fight_rounds_le; [| |exact I].
  - intros k J. simpl in J. destruct J as [J|[]]. subst. apply le_n.
  - intros t J. destruct J.
Qed.

Lemma fight_le: forall s osk op mds te, In te (map fst (fight s osk op mds)) -> te_le (curendurance s) te.
Proof.
  intros s osk op mds te. apply fight_le_fuel.
Qed.

(* Fights played one round at a time *)
Lemma one_round_le: forall s osk op mds, end_le (curendurance s) (one_round s osk op mds).
Proof.
  intros s osk op mds. apply end_le_fold. intros [lw o] I. apply common_le.
  exact (round_results_le s osk op mds (lw, o) I).
Qed.

Definition next_pos (r: RoundEnd) : Prop := forall k, In k (map fst (snd r)) -> 0 < fst k.

Lemma common_pos: forall late s mds lw op p, next_pos (common_round late s mds lw op p).
Proof.
  intros late s mds lw op p k I. unfold common_round in I.
  destruct (lw =? 0) eqn: Z; [destruct I|]. apply Nat.eqb_neq in Z.
  destruct (Modifiers.extractl gOnDamage _).
  - destruct (lw <? curendurance s); [destruct I|].
    destruct (op =? 0); [destruct I|]. simpl in I. destruct I as [I|[]]. subst. simpl. lia.
  - destruct (op =? 0); [destruct I|]. simpl in I. destruct I as [I|[]]. subst. simpl. lia.
Qed.

Lemma one_round_pos: forall s osk op mds, next_pos (one_round s osk op mds).
Proof.
  intros s osk op mds. unfold one_round, fold_rounds.
  induction (round_results s osk op mds) as [|[lw o] l IH]; simpl.
  - intros k I. destruct I.
  - intros k I. destruct (add_proba_keeps_keys_conv _ _ _ k Logic.eq_refl I) as [A|A].
    + exact (common_pos _ _ _ _ _ _ _ A).
    + apply IH. exact A.
Qed.

Lemma fight_round_le: forall s osk op mds k, In k (map fst (fight_round s osk op mds)) -> fst k <= curendurance s.
Proof.
  intros s osk op mds k I. unfold fight_round in I.
  apply rebuild_proba_keeps_keys_conv in I. rewrite map_map in I. apply in_map_iff in I.
  destruct I as [[a b] [E _]]. subst k. unfold moddmg. simpl. lia.
Qed.
