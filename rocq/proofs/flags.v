(* A story flag that the rest of a book can't read can be ignored: this is the part of Reads, in the Rust solver, that is
   about flags. The rules read some flags on their own, those can't be ignored. *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import proba.
Require Import combat.
Require Import rules.
Require Import value.
Require Import fight_facts.
Require Import simulation.
Require Import gold_items.
Require Import gold_relation.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
From Stdlib Require Import Lia.
Open Scope nat_scope.

Definition rule_flag (f: flag) : bool :=
  match f with
  | Poisonned2 | HadCombat | LimbDeath | PermanentSkillReduction | PermanentSkillReduction2
  | StrengthPotionActive | PotentStrengthPotionActive => true
  | _ => false
  end.

Section FlagSection.
  Variable f: flag.
  Hypothesis story: rule_flag f = false.

  Lemma not_f: forall x, rule_flag x = true -> x <> f.
  Proof. intros x E N. subst. congruence. Qed.

  Record FlagR (n: nat) (s t: Stt) : Prop := {
    r_book: book s = book t;
    r_maxe: maxendurance s = maxendurance t;
    r_cur: curendurance s = curendurance t;
    r_sk: sk s = sk t;
    r_disc: disciplines s = disciplines t;
    r_chap: chapter s = chapter t;
    r_items: items s = items t;
    r_prev: previtems s = previtems t;
    r_vs: ValidMap (flags s);
    r_vt: ValidMap (flags t);
    r_other: forall x, x <> f -> has_flag x s = has_flag x t;
    r_read: ALL <= n -> has_flag f s = has_flag f t;
  }.

  Lemma fr_flag: forall n s t x, x <> f -> FlagR n s t -> has_flag x s = has_flag x t.
  Proof. intros. apply H0. assumption. Qed.

  Lemma fr_has: forall n s t i, FlagR n s t -> has_item i s = has_item i t.
  Proof. intros. unfold has_item. rewrite (r_items _ _ _ H). reflexivity. Qed.

  Lemma fr_sitems: forall n s t i, FlagR n s t -> sitems i s = sitems i t.
  Proof. intros. unfold sitems. rewrite (r_items _ _ _ H). reflexivity. Qed.

  Lemma fr_maxhp: forall n s t, FlagR n s t -> max_hp s = max_hp t.
  Proof. intros. unfold max_hp. rewrite (r_maxe _ _ _ H), (fr_has _ _ _ BodyArmor H), (fr_has _ _ _ Helmet H). reflexivity. Qed.

  Lemma fr_mono: forall m n s t, m <= n -> FlagR n s t -> FlagR m s t.
  Proof.
    intros m n s t L H. destruct H; constructor; auto. intro A. apply r_read0. lia.
  Qed.

  Lemma fr_top: forall n m s t, ALL <= n -> ALL <= m -> FlagR n s t -> FlagR m s t.
  Proof.
    intros n m s t A M H. destruct H; constructor; auto.
  Qed.

  Lemma fr_endur: forall n s t g h, g (curendurance s) = h (curendurance t) -> FlagR n s t ->
    FlagR n (update_endurance g s) (update_endurance h t).
  Proof.
    intros n s t g h E H. destruct H; constructor; unfold has_flag in *; simpl; auto.
  Qed.

  Lemma fr_goto: forall n s t c, FlagR n s t -> FlagR n (update_chapter c s) (update_chapter c t).
  Proof.
    intros n s t c H. destruct H; constructor; unfold has_flag in *; simpl; auto.
  Qed.

  Lemma fr_update_items: forall n s t (g: Items -> Items), FlagR n s t -> FlagR n (update_items g s) (update_items g t).
  Proof.
    intros n s t g H. destruct H; constructor; unfold has_flag in *; simpl; auto. rewrite r_items0. reflexivity.
  Qed.

  Lemma fr_clamp: forall n s t, FlagR n s t -> FlagR n (clamp_endurance s) (clamp_endurance t).
  Proof.
    intros n s t H. unfold clamp_endurance. apply fr_endur; auto. rewrite (r_cur _ _ _ H), (fr_maxhp _ _ _ H). reflexivity.
  Qed.

  Lemma fr_heal: forall n s t q, FlagR n s t -> FlagR n (heal s q) (heal t q).
  Proof.
    intros n s t q H. unfold heal. apply fr_endur; auto. rewrite (r_cur _ _ _ H), (fr_maxhp _ _ _ H). reflexivity.
  Qed.

  Lemma fr_set: forall n s t x, FlagR n s t -> FlagR n (update_flags (s_set x) s) (update_flags (s_set x) t).
  Proof.
    intros n s t x H. destruct H; constructor; unfold has_flag in *; simpl; auto;
      try (apply Insert.insert_with_valid; assumption).
    - intros y Y. rewrite !s_check_set; auto. destruct (order.eqb y x); auto.
    - intro A. rewrite !s_check_set; auto. destruct (order.eqb f x); auto.
  Qed.

  Lemma fr_unset: forall n s t x, FlagR n s t -> FlagR n (update_flags (s_unset x) s) (update_flags (s_unset x) t).
  Proof.
    intros n s t x H. destruct H; constructor; unfold has_flag in *; simpl; auto;
      try (apply Delete.valid; assumption).
    - intros y Y. rewrite !s_check_unset; auto. destruct (order.eqb y x); auto.
    - intro A. rewrite !s_check_unset; auto. destruct (order.eqb f x); auto.
  Qed.

  Lemma fr_simple: forall so n s t, FlagR n s t -> FlagR n (update_simple s so) (update_simple t so).
  Proof.
    intros so n s t H.
    destruct so as [dmg|v| | |i q|i q|slts|c| | |fl|fl]; cbn [update_simple].
    - apply fr_endur; auto. rewrite (r_cur _ _ _ H). reflexivity.
    - apply fr_heal. assumption.
    - apply fr_endur; auto. rewrite (fr_maxhp _ _ _ H). reflexivity.
    - apply fr_endur; auto. rewrite (fr_maxhp _ _ _ H), (r_cur _ _ _ H). reflexivity.
    - apply fr_update_items. assumption.
    - destruct (q =? 0); auto. apply fr_clamp.
      destruct i; simpl; try apply fr_update_items; auto; try (apply fr_update_items; assumption).
    - apply fr_clamp, fr_update_items. assumption.
    - assert (FlagR n (update_items (rm_item Laumspur 1) (heal s 3)) (update_items (rm_item Laumspur 1) (heal t 3))) as H2
        by (apply fr_update_items, fr_heal; assumption).
      assert (FlagR n (update_items (rm_item Meal 1) s) (update_items (rm_item Meal 1) t)) as H3 by (apply fr_update_items; assumption).
      assert (FlagR n (update_endurance (fun e => e - 3) s) (update_endurance (fun e => e - 3) t)) as H4
        by (apply fr_endur; auto; rewrite (r_cur _ _ _ H); reflexivity).
      rewrite (r_disc _ _ _ H), (r_book _ _ _ H), (fr_has _ _ _ Laumspur H), (fr_has _ _ _ Meal H), (fr_maxhp _ _ _ H), (r_cur _ _ _ H).
      destruct c, (s_check Hunting (disciplines t)); cbn iota zeta; try exact H;
      repeat match goal with |- FlagR _ (if ?b then _ else _) _ => destruct b end; assumption.
    - apply fr_clamp. destruct H; constructor; unfold has_flag in *; simpl; auto.
    - apply fr_clamp. destruct H; constructor; unfold has_flag in *; simpl; auto.
    - apply fr_set. assumption.
    - apply fr_unset. assumption.
  Qed.

  Fixpoint flag_cond_need (c: bool_cond) : nat :=
    match c with
    | HasFlag x => if flag_beq x f then ALL else 0
    | Not x => flag_cond_need x
    | COr a b | CAnd a b => Nat.max (flag_cond_need a) (flag_cond_need b)
    | _ => 0
    end.

  Lemma fr_cond: forall c n s t, flag_cond_need c <= n -> FlagR n s t -> check_cond s c = check_cond t c.
  Proof.
    induction c; intros n0 s t L H; cbn [flag_cond_need check_cond] in *.
    - rewrite (r_disc _ _ _ H). reflexivity.
    - rewrite (IHc n0 s t); auto.
    - rewrite (IHc1 n0 s t), (IHc2 n0 s t); auto; lia.
    - rewrite (IHc1 n0 s t), (IHc2 n0 s t); auto; lia.
    - rewrite (fr_sitems _ _ _ i H). reflexivity.
    - reflexivity.
    - rewrite (r_cur _ _ _ H). reflexivity.
    - destruct (flag_beq f0 f) eqn: E.
      + apply m_flag.eqb_correct in E. subst. exact (r_read _ _ _ H L).
      + apply (r_other _ _ _ H). intro C. subst. rewrite (proj1 (m_flag.eqb_correct f f) Logic.eq_refl) in E. discriminate.
    - unfold level. rewrite (r_disc _ _ _ H). reflexivity.
  Qed.

  Lemma fr_take: forall i n s t, FlagR n s t -> tcan_take i s = tcan_take i t.
  Proof.
    intros i n s t H. unfold tcan_take, held_items, has_item, sitems. rewrite (r_items _ _ _ H), (r_book _ _ _ H). reflexivity.
  Qed.

  Lemma fr_fight: forall n s t, FlagR n s t -> SameFight s t.
  Proof.
    intros n s t H. constructor; try (apply (fr_has n); exact H); try (apply (fr_flag n); [apply not_f; reflexivity|exact H]).
    - apply (r_cur _ _ _ H).
    - apply (r_sk _ _ _ H).
    - apply (r_disc _ _ _ H).
    - unfold all_character_items_slot_count, all_character_items_slot. rewrite (r_items _ _ _ H). reflexivity.
    - intro w. apply (fr_has n). exact H.
  Qed.

  Definition flag_tracker : Tracker.
  Proof.
    refine {|
      R := FlagR;
      need_cond := flag_cond_need;
      need_simple := fun _ a => a;
      need_held := fun _ => 0;
      need_take := fun _ a => a;
      need_buy := fun _ _ a => a;
      need_sell := fun _ _ a => a;
      need_cartwheel := 0;
      need_retrieve := 0;
    |}.
    - exact fr_mono.
    - exact fr_top.
    - exact r_cur.
    - exact r_book.
    - exact r_chap.
    - exact r_disc.
    - exact fr_maxhp.
    - exact fr_fight.
    - intros. apply (fr_flag n); [apply not_f; reflexivity|exact H].
    - intros. apply (fr_flag n); [apply not_f; reflexivity|exact H].
    - intros. apply (fr_has n). assumption.
    - intros. apply fr_endur; auto.
    - intros. apply fr_goto. assumption.
    - intros. apply fr_set. assumption.
    - intros. apply fr_unset. assumption.
    - intros n s t H. exact (fr_unset _ _ _ StrengthPotionActive (fr_unset _ _ _ PotentStrengthPotionActive H)).
    - intros so a n s t L H. eapply fr_mono; [exact L|]. apply fr_simple. assumption.
    - intros c n s t L H. exact (fr_cond c n s t L H).
    - intros sl n s t L H. unfold held_items, sitems. rewrite (r_items _ _ _ H). reflexivity.
    - intros n s t A H. unfold all_held_items, sitems. rewrite (r_items _ _ _ H). reflexivity.
    - intros i a n s t L H. apply (fr_take _ n). exact H.
    - intros i p a n s t L H. split; [rewrite (fr_sitems n s t Gold H); reflexivity|apply (fr_take _ n); exact H].
    - intros i p a n s t L H. rewrite (fr_sitems n s t i H). reflexivity.
    - intros n s t L H. apply (fr_sitems n). exact H.
    - intros. assumption.
    - intros. apply le_n.
    - intros. reflexivity.
    - intros. lia.
    - intros. reflexivity.
    - intros. lia.
    - intros. apply le_n.
    - intros. assumption.
    - intros. apply le_n.
    - intros. apply le_n.
    - intros. apply le_n.
    - intros. apply le_n.
    - intros. apply le_n.
    - intros. apply le_n.
    - intros. reflexivity.
    - intros. reflexivity.
  Defined.

  Lemma flag_value_same: forall (ch: cid -> nat) wn chapters score,
    (forall s t, FlagR wn s t -> score s = score t) ->
    (forall c d, find_chapter c chapters = Some d -> dec_need flag_tracker ch wn d <= ch c) ->
    forall fuel s t, FlagR (ch (chapter s)) s t -> value chapters score fuel s = value chapters score fuel t.
  Proof.
    intros ch wn chapters score HS HB fuel s t H. exact (value_same flag_tracker ch wn chapters score HS HB fuel s t H).
  Qed.
End FlagSection.

(* A flag that a whole book never reads *)
Definition flag_unread_in (chapters: list (cid * decision)) (f: flag) (H: rule_flag f = false) : bool :=
  forallb (fun cd : cid * decision => Nat.eqb (dec_need (flag_tracker f H) (fun _ => 0) 0 (snd cd)) 0) chapters.

Lemma flag_unread_same: forall chapters f H score,
  flag_unread_in chapters f H = true ->
  (forall s t, FlagR f 0 s t -> score s = score t) ->
  forall fuel s t, FlagR f 0 s t -> value chapters score fuel s = value chapters score fuel t.
Proof.
  intros chapters f H score U HS fuel s t HR.
  apply (flag_value_same f H (fun _ => 0) 0 chapters score HS); auto.
  intros c d Fd. apply find_chapter_in in Fd.
  unfold flag_unread_in in U. rewrite forallb_forall in U. specialize (U _ Fd). apply Nat.eqb_eq in U. simpl in U. rewrite U. apply Nat.le_0_l.
Qed.
