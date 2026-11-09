(* The gold that is more than what can be used from a chapter can be ignored, which is GoldNeeds in the Rust solver: two
   states that only differ by gold that is enough are told apart by nothing, as a Tracker. *)
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
Require Import gold_items.

(* Two states that are the same, apart from gold that is enough *)
Record GoldR (n: nat) (s t: Stt) : Prop := {
  g_book: book s = book t;
  g_maxe: maxendurance s = maxendurance t;
  g_cur: curendurance s = curendurance t;
  g_sk: sk s = sk t;
  g_disc: disciplines s = disciplines t;
  g_chap: chapter s = chapter t;
  g_flags: flags s = flags t;
  g_psame: SameM (previtems s) (previtems t);
  g_prel: GoldRel n (previtems s) (previtems t);
  g_same: SameM (items s) (items t);
  g_rel: GoldRel n (items s) (items t);
}.

Ltac gr_fields H := destruct H; constructor; unfold update_items, update_endurance, update_flags, update_chapter; simpl; auto.

Lemma gr_mono: forall m n s t, m <= n -> GoldR n s t -> GoldR m s t.
Proof.
  intros m n s t L H. gr_fields H.
  - destruct g_prel0 as [G|[A [B C]]]; [left; assumption|right; lia].
  - destruct g_rel0 as [G|[A [B C]]]; [left; assumption|right; lia].
Qed.

Lemma gr_full: forall n s t, ALL <= n -> GoldR n s t -> lookup Gold (items s) = lookup Gold (items t).
Proof.
  intros n s t A H. destruct (g_rel _ _ _ H) as [G|[_ [_ C]]]; [assumption|lia].
Qed.

Lemma gr_top: forall n m s t, ALL <= n -> ALL <= m -> GoldR n s t -> GoldR m s t.
Proof.
  intros n m s t A M H. pose proof (gr_full _ _ _ A H) as G.
  assert (lookup Gold (previtems s) = lookup Gold (previtems t)) as P.
  { destruct (g_prel _ _ _ H) as [X|[_ [_ C]]]; [assumption|lia]. }
  gr_fields H; left; assumption.
Qed.

Lemma gr_items_eq: forall n s t, ALL <= n -> GoldR n s t -> items s = items t.
Proof.
  intros n s t A H. pose proof (gr_full _ _ _ A H) as G. destruct (g_same _ _ _ H) as [VA [VB E]].
  apply Lookup.map_ext_valid; auto. intro k. destruct (eq_dec k Gold); [subst; assumption|apply E; assumption].
Qed.

Lemma gr_update_items: forall a n s t (f: Items -> Items), a <= n -> GoldR n s t ->
  SameM (f (items s)) (f (items t)) -> GoldRel a (f (items s)) (f (items t)) ->
  GoldR a (update_items f s) (update_items f t).
Proof.
  intros a n s t f L H S G. pose proof (gr_mono _ _ _ _ L H) as H1. gr_fields H1.
Qed.

Lemma has_same: forall n s t i, i <> Gold -> GoldR n s t -> has_item i s = has_item i t.
Proof.
  intros n s t i N H. unfold has_item, has. destruct (g_same _ _ _ H) as [_ [_ E]]. rewrite E; auto.
Qed.

Lemma sitems_same: forall n s t i, i <> Gold -> GoldR n s t -> sitems i s = sitems i t.
Proof.
  intros n s t i N H. unfold sitems. destruct (g_same _ _ _ H) as [_ [_ E]]. rewrite E; auto.
Qed.

Lemma gr_maxhp: forall n s t, GoldR n s t -> max_hp s = max_hp t.
Proof.
  intros n s t H. unfold max_hp. rewrite (g_maxe _ _ _ H), (has_same n s t BodyArmor), (has_same n s t Helmet); auto; discriminate.
Qed.

Lemma gr_endur: forall n s t f g, f (curendurance s) = g (curendurance t) -> GoldR n s t ->
  GoldR n (update_endurance f s) (update_endurance g t).
Proof.
  intros n s t f g E H. gr_fields H.
Qed.

Lemma gr_clamp: forall n s t, GoldR n s t -> GoldR n (clamp_endurance s) (clamp_endurance t).
Proof.
  intros n s t H. unfold clamp_endurance. apply gr_endur; auto.
  rewrite (g_cur _ _ _ H), (gr_maxhp _ _ _ H). reflexivity.
Qed.

Lemma gr_heal: forall n s t q, GoldR n s t -> GoldR n (heal s q) (heal t q).
Proof.
  intros n s t q H. unfold heal. apply gr_endur; auto.
  rewrite (g_cur _ _ _ H), (gr_maxhp _ _ _ H). reflexivity.
Qed.

Lemma gr_rm: forall n s t i q, i <> Gold -> GoldR n s t -> GoldR n (update_items (rm_item i q) s) (update_items (rm_item i q) t).
Proof.
  intros n s t i q N H. destruct (g_same _ _ _ H) as [VA [VB E]].
  eapply gr_update_items; [apply le_n|exact H|..].
  - apply same_rm, H.
  - apply rel_rm_other; auto. apply H.
Qed.

Definition gold_simple_need (so: simple_outcome) (a: nat) : nat :=
  match so with
  | LoseItem i q => if item_beq i Gold then Nat.max a (Nat.min ALL (a + q)) else a
  | _ => a
  end.

Lemma beq_gold: forall i, i <> Gold -> item_beq i Gold = false.
Proof.
  intros i N. destruct (item_beq i Gold) eqn: E; auto. apply m_item.eqb_correct in E. contradiction.
Qed.

Lemma gr_lose_gold: forall a n q s t, Nat.max a (Nat.min ALL (a + q)) <= n -> GoldR n s t ->
  GoldRel a (rm_item Gold q (items s)) (rm_item Gold q (items t)).
Proof.
  intros a n q s t L H. destruct (g_same _ _ _ H) as [VA [VB E]].
  destruct (g_rel _ _ _ H) as [G|[A [B C]]].
  - left. rewrite !lookup_rm_gold; auto. rewrite G. reflexivity.
  - right. rewrite !gold_rm; auto. lia.
Qed.

Lemma gr_simple: forall so a n s t, gold_simple_need so a <= n -> GoldR n s t -> GoldR a (update_simple s so) (update_simple t so).
Proof.
  intros so a n s t L H. pose proof H as H0.
  destruct so as [dmg|v| | |i q|i q|slts|c| | |fl|fl]; cbn [update_simple gold_simple_need] in *.
  - apply gr_endur; [rewrite (g_cur _ _ _ H); reflexivity|eapply gr_mono; eauto].
  - apply gr_heal. eapply gr_mono; eauto.
  - apply gr_endur; [rewrite (gr_maxhp _ _ _ H); reflexivity|eapply gr_mono; eauto].
  - apply gr_endur; [rewrite (gr_maxhp _ _ _ H), (g_cur _ _ _ H); reflexivity|eapply gr_mono; eauto].
  - (* GainItem *)
    eapply gr_update_items; [exact L|exact H|apply same_add, H|].
    destruct (eq_dec i Gold).
    + subst. destruct H as [_ _ _ _ _ _ _ _ _ S R]. destruct S as [VA [VB _]].
      apply rel_add_gold; auto. destruct R as [G|[A [B C]]]; [left; assumption|right; lia].
    + destruct H as [_ _ _ _ _ _ _ _ _ S R]. destruct S as [VA [VB _]]. unfold GoldRel, gold_of.
      rewrite !add_gold_other; auto. destruct R as [G|[A [B C]]]; [left; assumption|right; unfold gold_of in *; lia].
  - (* LoseItem *)
    destruct (q =? 0) eqn: Q0.
    + eapply gr_mono; [|exact H]. destruct (item_beq i Gold); lia.
    + apply gr_clamp. destruct (eq_dec i Gold) as [EG|NG].
      * subst i. simpl in L. simpl.
        eapply gr_update_items; [eapply Nat.le_trans; [apply Nat.le_max_l|exact L]|exact H|apply same_rm, H|]. eapply gr_lose_gold; eauto.
      * rewrite beq_gold in L by assumption.
        assert (GoldR a (update_items (rm_item i q) s) (update_items (rm_item i q) t)) as G1.
        { apply gr_rm; auto. eapply gr_mono; eauto. }
        destruct i; simpl; try exact G1.
        eapply gr_update_items; [apply le_n|exact G1|apply same_slot, G1|].
        apply rel_slot; apply G1.
  - (* LoseItemKind *)
    apply gr_clamp. eapply gr_update_items; [exact L|exact H|apply same_slots, H|].
    apply rel_slots; [apply H|]. eapply g_rel, gr_mono; eauto.
  - (* MustEat *)
    assert (GoldR a s t) as H1 by (eapply gr_mono; eauto).
    rewrite (g_disc _ _ _ H), (g_book _ _ _ H), (has_same n s t Laumspur), (has_same n s t Meal), (gr_maxhp _ _ _ H), (g_cur _ _ _ H);
      auto; try discriminate.
    assert (GoldR a (update_items (rm_item Laumspur 1) (heal s 3)) (update_items (rm_item Laumspur 1) (heal t 3))) as H2.
    { apply gr_rm; [discriminate|]. apply gr_heal. exact H1. }
    assert (GoldR a (update_items (rm_item Meal 1) s) (update_items (rm_item Meal 1) t)) as H3 by (apply gr_rm; [discriminate|exact H1]).
    assert (GoldR a (update_endurance (fun e => e - 3) s) (update_endurance (fun e => e - 3) t)) as H4
      by (apply gr_endur; [rewrite (g_cur _ _ _ H); reflexivity|exact H1]).
    destruct c, (s_check Hunting (disciplines t)); cbn iota zeta; try exact H1;
    repeat match goal with |- GoldR _ (if ?b then _ else _) _ => destruct b end; assumption.
  - (* StoreEquipment *)
    apply gr_clamp. destruct (g_same _ _ _ H) as [VA [VB E]]. pose proof (gr_mono _ _ _ _ L H) as H1.
    destruct H1; constructor; simpl; auto.
    + split; [constructor|]. split; [constructor|]. reflexivity.
    + left. reflexivity.
  - (* RetrieveEquipment *)
    apply gr_clamp. pose proof (gr_mono _ _ _ _ L H) as H1. destruct H1; constructor; simpl; auto.
    + split; [constructor|]. split; [constructor|]. reflexivity.
    + left. reflexivity.
  - (* SetFlag *)
    assert (GoldR a s t) as H1 by (eapply gr_mono; eauto). destruct H1; constructor; simpl; auto. rewrite g_flags0. reflexivity.
  - (* ClearFlag *)
    assert (GoldR a s t) as H1 by (eapply gr_mono; eauto). destruct H1; constructor; simpl; auto. rewrite g_flags0. reflexivity.
Qed.

Fixpoint gold_cond_need (c: bool_cond) : nat :=
  match c with
  | HasItem i q => if item_beq i Gold then q else 0
  | Not x => gold_cond_need x
  | COr a b | CAnd a b => Nat.max (gold_cond_need a) (gold_cond_need b)
  | _ => 0
  end.

Lemma gr_cond: forall c n s t, gold_cond_need c <= n -> GoldR n s t -> check_cond s c = check_cond t c.
Proof.
  induction c; intros n0 s t L H; cbn [gold_cond_need check_cond] in *.
  - rewrite (g_disc _ _ _ H). reflexivity.
  - rewrite (IHc n0 s t); auto.
  - rewrite (IHc1 n0 s t), (IHc2 n0 s t); auto; lia.
  - rewrite (IHc1 n0 s t), (IHc2 n0 s t); auto; lia.
  - destruct (eq_dec i Gold) as [EG|NG].
    + subst i. simpl in L. destruct (g_rel _ _ _ H) as [G|[A [B C]]].
      * unfold sitems. rewrite G. reflexivity.
      * unfold gold_of in *. unfold sitems. destruct (Nat.leb_spec n (match lookup Gold (items s) with Some x => x | None => 0 end)),
          (Nat.leb_spec n (match lookup Gold (items t) with Some x => x | None => 0 end)); try reflexivity; lia.
    + rewrite (sitems_same n0 s t i NG H). reflexivity.
  - reflexivity.
  - rewrite (g_cur _ _ _ H). reflexivity.
  - unfold has_flag. rewrite (g_flags _ _ _ H). reflexivity.
  - unfold level, discipline_count. rewrite (g_disc _ _ _ H). reflexivity.
Qed.

Definition gold_held_need (sl: slot) : nat := match sl with SPouch => ALL | _ => 0 end.

Lemma gr_sitems_full: forall n s t i, ALL <= n -> GoldR n s t -> sitems i s = sitems i t.
Proof.
  intros n s t i A H. destruct (eq_dec i Gold).
  - subst. unfold sitems. rewrite (gr_full _ _ _ A H). reflexivity.
  - apply (sitems_same n); auto.
Qed.

Lemma held_ext: forall sl s t, (forall i, In i (slot_items sl) -> sitems i s = sitems i t) -> held_items sl s = held_items sl t.
Proof.
  intros sl s t E. unfold held_items. f_equal. apply map_ext_in. intros i I. rewrite E; auto.
Qed.

Lemma gr_held_other: forall sl n s t, sl <> SPouch -> GoldR n s t -> held_items sl s = held_items sl t.
Proof.
  intros sl n s t N H. apply held_ext. intros i I. apply (sitems_same n); auto. eapply not_gold_slot; eauto.
Qed.

Lemma gr_take: forall i n s t, GoldR n s t -> tcan_take i s = tcan_take i t.
Proof.
  intros i n s t H. unfold tcan_take. destruct (item_slot i) eqn: SL; auto.
  - rewrite (gr_held_other SWeapon n s t) by (auto; discriminate). reflexivity.
  - rewrite (has_same n s t Backpack), (gr_held_other SBackpack n s t), (g_book _ _ _ H); auto; discriminate.
  - assert (i <> Gold) by (intro E; subst; discriminate).
    rewrite (has_same n s t i), (has_same n s t Helmet), (has_same n s t SilverHelmet); auto; discriminate.
Qed.

Lemma gr_fight: forall n s t, GoldR n s t -> SameFight s t.
Proof.
  intros n s t H. constructor.
  - apply (g_cur _ _ _ H).
  - apply (g_sk _ _ _ H).
  - apply (g_disc _ _ _ H).
  - unfold all_character_items_slot_count, all_character_items_slot.
    rewrite (filter_same _ (items s) (items t)); [reflexivity|apply H|reflexivity].
  - intros w _. apply (has_same n); [discriminate|exact H].
  - apply (has_same n); [discriminate|exact H].
  - apply (has_same n); [discriminate|exact H].
  - unfold has_flag. rewrite (g_flags _ _ _ H). reflexivity.
  - unfold has_flag. rewrite (g_flags _ _ _ H). reflexivity.
  - unfold has_flag. rewrite (g_flags _ _ _ H). reflexivity.
  - unfold has_flag. rewrite (g_flags _ _ _ H). reflexivity.
  - unfold has_flag. rewrite (g_flags _ _ _ H). reflexivity.
Qed.

Lemma gr_flags: forall n s t f, GoldR n s t -> GoldR n (update_flags f s) (update_flags f t).
Proof.
  intros n s t f H. destruct H; constructor; simpl; auto. rewrite g_flags0. reflexivity.
Qed.

Lemma gold_not_dropped: forall i s lst l, tcan_take i s = MustDrop lst -> In l lst -> l <> Gold.
Proof.
  intros i s lst l E I. unfold tcan_take in E.
  assert (forall sl, sl <> SPouch -> In l (map fst (held_items sl s)) -> l <> Gold) as HS.
  { intros sl N J. apply in_map_iff in J. destruct J as [x [X J]]. subst. eapply not_gold_slot; eauto. eapply held_in_slot; eauto. }
  destruct (item_slot i).
  - destruct (_ <? 2); inversion E; subst. eapply HS; eauto; discriminate.
  - destruct (negb _); try discriminate. destruct (_ <=? 8); inversion E; subst. eapply HS; eauto; discriminate.
  - destruct (has_item i s); try discriminate.
    destruct (_ && _)%bool; [inversion E; subst; destruct I as [I|[]]; subst; discriminate|].
    destruct (_ && _)%bool; [inversion E; subst; destruct I as [I|[]]; subst; discriminate|discriminate].
  - discriminate.
Qed.

Definition gold_tracker : Tracker.
Proof.
  refine {|
    R := GoldR;
    need_cond := gold_cond_need;
    need_simple := gold_simple_need;
    need_held := gold_held_need;
    need_take := fun _ a => a;
    need_buy := fun _ p a => Nat.max a (Nat.min ALL (a + p));
    need_sell := fun i _ a => if item_beq i Gold then Nat.max ALL a else a;
    need_cartwheel := ALL;
    need_retrieve := ALL;
  |}.
  - exact gr_mono.
  - exact gr_top.
  - exact g_cur.
  - exact g_book.
  - exact g_chap.
  - exact g_disc.
  - exact gr_maxhp.
  - exact gr_fight.
  - intros. unfold has_flag. rewrite (g_flags _ _ _ H). reflexivity.
  - intros. unfold has_flag. rewrite (g_flags _ _ _ H). reflexivity.
  - intros n s t i I H. apply (has_same n); auto. simpl in I. intro E. subst.
    repeat (destruct I as [I|I]; [discriminate|]). destruct I.
  - intros. apply gr_endur; auto.
  - intros. destruct H; constructor; simpl; auto.
  - intros. apply gr_flags. assumption.
  - intros. apply gr_flags. assumption.
  - intros. apply gr_flags. assumption.
  - exact gr_simple.
  - intros c n s t L H. exact (gr_cond c n s t L H).
  - intros sl n s t L H. destruct sl; try (apply (gr_held_other _ n); [discriminate|exact H]).
    apply held_ext. intros i _. apply (gr_sitems_full n); auto.
  - intros n s t A H. unfold all_held_items. f_equal. apply map_ext. intro i. rewrite (gr_sitems_full n s t i A H). reflexivity.
  - intros i a n s t L H. apply (gr_take _ n). exact H.
  - intros i p a n s t L H. split; [|apply (gr_take _ n); exact H].
    destruct (g_rel _ _ _ H) as [G|[A [B C]]].
    + unfold sitems. rewrite G. reflexivity.
    + unfold gold_of, sitems in *.
      destruct (Nat.leb_spec p (match lookup Gold (items s) with Some x => x | None => 0 end)),
        (Nat.leb_spec p (match lookup Gold (items t) with Some x => x | None => 0 end)); try reflexivity; lia.
  - intros i p a n s t L H. destruct (eq_dec i Gold) as [EG|NG].
    + subst. change (item_beq Gold Gold) with true in L. cbn iota in L.
      rewrite (gr_sitems_full n s t Gold); [reflexivity|lia|exact H].
    + rewrite (sitems_same n s t i NG H). reflexivity.
  - intros n s t L H. apply (gr_sitems_full n); auto.
  - intros so a b L. destruct so; cbn [gold_simple_need]; try lia. destruct (item_beq i Gold); lia.
  - intros so a. destruct so; cbn [gold_simple_need]; try lia. destruct (item_beq i Gold); lia.
  - intros so a RE. destruct so; cbn [gold_simple_need]; try reflexivity; try (simpl in RE; discriminate).
    destruct (eq_dec i Gold) as [EG|NG]; [|rewrite beq_gold; auto].
    subst. destruct n as [|[|n]]; simpl in RE; discriminate.
  - intros sl i a I. cbn [gold_simple_need]. destruct (eq_dec i Gold) as [EG|NG].
    + subst. apply character.Slot.in_slot_items_slot in I. simpl in I. subst.
      change (item_beq Gold Gold) with true. cbn iota. unfold gold_held_need. lia.
    + rewrite beq_gold by assumption. lia.
  - intros i s lst l a E I. cbn [gold_simple_need]. rewrite beq_gold; [reflexivity|]. eapply gold_not_dropped; eauto.
  - intros v a. cbn [gold_simple_need]. lia.
  - intros. apply le_n.
  - intros. assumption.
  - intros. apply le_n.
  - intros i q a. destruct i; simpl; lia.
  - intros. lia.
  - intros. reflexivity.
  - intros i p a. destruct (item_beq i Gold); lia.
  - intros i p a. destruct (eq_dec i Gold) as [EG|NG].
    + subst. cbn [gold_simple_need]. change (item_beq Gold Gold) with true. cbn iota. lia.
    + cbn [gold_simple_need]. rewrite !beq_gold by assumption. lia.
  - intros i a L. lia.
  - intros a L. lia.
Defined.
