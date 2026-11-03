(* The gold that is more than what can be used from a chapter can be ignored: this is GoldNeeds in the Rust solver. *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import lists.
Require Import proba.
Require Import combat.
Require Import rules.
Require Import value.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
From Stdlib Require Import Lia.
Require Import Stdlib.FSets.FMapPositive.
Require Import Stdlib.PArith.PArith.
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

Definition gold_sn (so: simple_outcome) (a: nat) : nat :=
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

Lemma gr_simple: forall so a n s t, gold_sn so a <= n -> GoldR n s t -> GoldR a (update_simple s so) (update_simple t so).
Proof.
  intros so a n s t L H. pose proof H as H0.
  destruct so as [dmg|v| | |i q|i q|slts|c| | |fl|fl]; cbn [update_simple gold_sn] in *.
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

Fixpoint gold_cn (c: bool_cond) : nat :=
  match c with
  | HasItem i q => if item_beq i Gold then q else 0
  | Not x => gold_cn x
  | COr a b | CAnd a b => Nat.max (gold_cn a) (gold_cn b)
  | _ => 0
  end.

Lemma gr_cond: forall c n s t, gold_cn c <= n -> GoldR n s t -> check_cond s c = check_cond t c.
Proof.
  induction c; intros n0 s t L H; cbn [gold_cn check_cond] in *.
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
  - unfold level. rewrite (g_disc _ _ _ H). reflexivity.
Qed.

Definition gold_ln (sl: slot) : nat := match sl with SPouch => ALL | _ => 0 end.

Lemma not_gold_slot: forall sl i, sl <> SPouch -> In i (slot_items sl) -> i <> Gold.
Proof.
  intros sl i N I E. subst. apply character.Slot.in_slot_items_slot in I. simpl in I. subst. contradiction.
Qed.

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
  - intro w. apply (has_same n); [discriminate|exact H].
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
    cn := gold_cn;
    sn := gold_sn;
    ln := gold_ln;
    tn := fun _ a => a;
    bn := fun _ p a => Nat.max a (Nat.min ALL (a + p));
    en := fun i _ a => if item_beq i Gold then Nat.max ALL a else a;
    cw := ALL;
    rn := ALL;
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
  - intros so a b L. destruct so; cbn [gold_sn]; try lia. destruct (item_beq i Gold); lia.
  - intros so a. destruct so; cbn [gold_sn]; try lia. destruct (item_beq i Gold); lia.
  - intros so a RE. destruct so; cbn [gold_sn]; try reflexivity; try (simpl in RE; discriminate).
    destruct (eq_dec i Gold) as [EG|NG]; [|rewrite beq_gold; auto].
    subst. destruct n as [|[|n]]; simpl in RE; discriminate.
  - intros sl i a I. cbn [gold_sn]. destruct (eq_dec i Gold) as [EG|NG].
    + subst. apply character.Slot.in_slot_items_slot in I. simpl in I. subst.
      change (item_beq Gold Gold) with true. cbn iota. unfold gold_ln. lia.
    + rewrite beq_gold by assumption. lia.
  - intros i s lst l a E I. cbn [gold_sn]. rewrite beq_gold; [reflexivity|]. eapply gold_not_dropped; eauto.
  - intros v a. cbn [gold_sn]. lia.
  - intros. apply le_n.
  - intros. assumption.
  - intros. apply le_n.
  - intros i q a. destruct i; simpl; lia.
  - intros. lia.
  - intros. reflexivity.
  - intros i p a. destruct (item_beq i Gold); lia.
  - intros i p a. destruct (eq_dec i Gold) as [EG|NG].
    + subst. cbn [gold_sn]. change (item_beq Gold Gold) with true. cbn iota. lia.
    + cbn [gold_sn]. rewrite !beq_gold by assumption. lia.
  - intros i a L. lia.
  - intros a L. lia.
Defined.

Module PM := FMapPositive.PositiveMap.

Definition need_of (t: PM.t nat) (c: cid) : nat :=
  match PM.find (Pos.of_succ_nat c) t with Some v => v | None => 0 end.

(* Getting the equipment back adds the gold that is held to the gold that was stored, so that two states can get the
   same total from different gains, and the outcomes are not the same. *)
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

(* what doesn't change the gold *)
Definition keeps (so: simple_outcome) : Prop :=
  match so with
  | GainItem i _ | LoseItem i _ => i <> Gold
  | ClearFlag _ => True
  | _ => False
  end.

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

Lemma keep_gold: forall so s, keeps so -> ValidMap (items s) ->
  lookup Gold (items (update_simple s so)) = lookup Gold (items s) /\ previtems (update_simple s so) = previtems s.
Proof.
  intros so s K VA. destruct so as [dmg|v| | |i q|i q|slts|c| | |fl|fl]; simpl in K; try contradiction.
  - simpl. split; [|reflexivity]. apply add_gold_other; auto.
  - cbn [update_simple]. destruct (q =? 0); [split; reflexivity|].
    assert (lookup Gold (rm_item i q (items s)) = lookup Gold (items s)) as RG.
    { unfold rm_item, bag_rm. rewrite Lookup.update_diff; auto. }
    assert (ValidMap (rm_item i q (items s))) as RV.
    { assert (SameM (items s) (items s)) as S by (split; [assumption|split; [assumption|reflexivity]]).
      apply (same_rm i q) in S. apply S. }
    destruct i; try contradiction; simpl; split; try reflexivity; try exact RG.
    rewrite slot_gold; auto.
  - split; reflexivity.
Qed.

Lemma keep_need: forall so a, keeps so -> gold_sn so a = a.
Proof.
  intros so a K. destruct so; simpl in K; try contradiction; try reflexivity.
  cbn [gold_sn]. rewrite beq_gold; auto.
Qed.

Section Retrieve.
  Variable ch: cid -> nat.
  Variable wn: nat.
  Variable f: next_step -> Qc.
  Hypothesis fN: forall x y, GoldR (ch (chapter x)) x y -> f (New x) = f (New y).
  Hypothesis fW: forall x y, GoldR wn x y -> f (Won x) = f (Won y).

  Definition dn (d: decision) : nat := dec_need gold_tracker ch wn d.

  Definition OK (P: Stt -> Stt -> Prop) (o1 o2: chapter_outcome) : Prop :=
    forall fuel p q, P p q -> ev f (s_update_d fuel p o1) = ev f (s_update_d fuel q o2).

  Lemma ok_wrap: forall (P P2: Stt -> Stt -> Prop) sos1 sos2 l1 l2,
    (forall p q, P p q -> curendurance p = curendurance q /\ P2 (apply_effects p sos1) (apply_effects q sos2)) ->
    Forall2 (OK P2) l1 l2 -> Forall2 (OK P) (map (Simple sos1) l1) (map (Simple sos2) l2).
  Proof.
    intros P P2 sos1 sos2 l1 l2 ST F. induction F; simpl; constructor; auto.
    intros fuel p q H1. destruct (ST _ _ H1) as [C P3]. destruct fuel as [|md]; cbn [s_update_d]; rewrite C; [reflexivity|].
    destruct (curendurance q =? 0); [reflexivity|]. apply H. exact P3.
  Qed.

  Lemma same_ok: forall a fl p q d, dn d <= a -> GoldR a p q -> Forall2 (OK (GoldR a)) (flatten_d fl p d) (flatten_d fl q d).
  Proof.
    intros a fl p q d L H. destruct (flatten_same gold_tracker ch wn fl a p q d L H) as [E M]. rewrite <- E.
    apply forall2_refl_in. intros o I fuel u v R2.
    destruct (M o I) as [C|A].
    - exact (ev_update gold_tracker ch wn f fN fW fuel u v o a C R2).
    - apply (ev_update gold_tracker ch wn f fN fW fuel u v o (Nat.max a (co_need gold_tracker ch wn o))); [lia|].
      eapply gr_top; [exact A| |exact R2]. lia.
  Qed.

  Variables (a gs gt: nat).
  Hypotheses (GS: gs <> 0) (GT: gt <> 0).
  Variable rest: decision.
  Hypothesis RN: dn rest <= a.

  (* the gold that is held is added later on *)
  Definition Pend (p q: Stt) : Prop :=
    GoldR 0 p q /\ GoldRel a (previtems p) (previtems q) /\ GoldRel a (add_item Gold gs (items p)) (add_item Gold gt (items q)).

  Lemma pend_step: forall so p q, keeps so -> Pend p q -> Pend (update_simple p so) (update_simple q so).
  Proof.
    intros so p q K [H0 [HP HA]].
    destruct (g_same _ _ _ H0) as [VP [VQ _]].
    destruct (keep_gold so p K VP) as [L1 P1]. destruct (keep_gold so q K VQ) as [L2 P2].
    assert (GoldR 0 (update_simple p so) (update_simple q so)) as H1.
    { apply (gr_simple so 0 0); [rewrite keep_need; auto|exact H0]. }
    destruct (g_same _ _ _ H1) as [VP1 [VQ1 _]].
    split; [exact H1|]. split; [rewrite P1, P2; exact HP|].
    eapply rel_congr; [| |exact HA]; apply add_congr; auto.
  Qed.

  Lemma pend_steps: forall sos p q, Forall keeps sos -> Pend p q -> Pend (apply_effects p sos) (apply_effects q sos).
  Proof.
    induction sos as [|so r IH]; intros p q K H; simpl; auto. inversion K; subst.
    apply IH; auto. apply pend_step; auto.
  Qed.

  Lemma pend_wrap: forall sos l1 l2, Forall keeps sos -> Forall2 (OK Pend) l1 l2 ->
    Forall2 (OK Pend) (map (Simple sos) l1) (map (Simple sos) l2).
  Proof.
    intros sos l1 l2 K F. eapply ok_wrap; [|exact F].
    intros p q H. split; [apply (g_cur 0); apply H|]. apply pend_steps; auto.
  Qed.

  Inductive PD : decision -> decision -> Prop :=
    | pd_gold: PD (can_take Gold gs rest) (can_take Gold gt rest)
    | pd_take: forall i q d1 d2, i <> Gold -> PD d1 d2 -> PD (can_take i q d1) (can_take i q d2).

  Lemma pd_flatten: forall fl p q d1 d2, PD d1 d2 -> Pend p q -> Forall2 (OK Pend) (flatten_d fl p d1) (flatten_d fl q d2).
  Proof.
    induction fl as [|fl IH]; intros p q d1 d2 D H; [constructor|].
    pose proof H as [H0 _].
    cbn [flatten_d]. unfold has_flag. rewrite (g_flags _ _ _ H0). fold (has_flag Poisonned2 q).
    rewrite (has_same 0 p q Laumspur) by (discriminate || exact H0).
    destruct (has_flag Poisonned2 q && has_item Laumspur q)%bool.
    { apply pend_wrap; [repeat constructor; discriminate|]. apply IH; auto. apply pend_steps; auto. repeat constructor; discriminate. }
    destruct D as [|i n d1 d2 NG D].
    - (* the gold is added *)
      eapply ok_wrap; [|apply (same_ok a)].
      + intros u v [U0 [UP UA]]. split; [apply (g_cur 0); exact U0|].
        destruct U0 as [B M C K DS CH FL PS PR SM RL]. destruct SM as [VU [VV E]].
        constructor; simpl; auto.
        apply same_add_gold. split; [exact VU|split; [exact VV|exact E]].
      + exact RN.
      + destruct H0 as [B M C K DS CH FL PS PR SM RL]. destruct H as [_ [UP UA]]. destruct SM as [VU [VV E]].
        constructor; simpl; auto.
        apply same_add_gold. split; [exact VU|split; [exact VV|exact E]].
    - assert (tcan_take i p = tcan_take i q) as TC0 by exact (R_take gold_tracker i 0 0 p q (le_n _) H0).
      destruct i; try congruence;
      (destruct n as [|[|n]];
      [ apply IH; auto
      | rewrite TC0, (g_book _ _ _ H0);
        match goal with |- context [tcan_take ?it q] => destruct (tcan_take it q) as [|lst|] eqn: TC end;
        [ apply pend_wrap; [repeat constructor; discriminate|]; apply IH; auto; apply pend_steps; auto; repeat constructor; discriminate
        | apply forall2_app; [apply IH; auto|]; apply forall2_flat; intros l Il;
          pose proof (gold_not_dropped _ _ _ _ TC Il) as LG;
          (destruct (_ <? backpack_slots _ _)%nat;
            [ apply pend_wrap; [repeat constructor; assumption|]; apply IH;
              [apply pd_take; [discriminate|exact D]|apply pend_steps; auto; repeat constructor; assumption]
            | destruct (order.eqb l _);
              [ constructor
              | apply pend_wrap; [repeat constructor; first [assumption|discriminate]|]; apply IH; auto;
                apply pend_steps; auto; repeat constructor; first [assumption|discriminate]]])
        | apply IH; auto ]
      | apply IH; [apply pd_take; [discriminate|apply pd_take; [discriminate|exact D]]|exact H] ]).
  Qed.
End Retrieve.

Definition RP (n hs ht: nat) (p q: Stt) : Prop := GoldR n p q /\ sitems Gold p = hs /\ sitems Gold q = ht.

Lemma rp_steps: forall sos n hs ht p q, Forall keeps sos -> RP n hs ht p q -> RP n hs ht (apply_effects p sos) (apply_effects q sos).
Proof.
  induction sos as [|so r IH]; intros n hs ht p q K H; simpl; auto. inversion K as [|x y K1 K2]; subst.
  apply IH; auto. destruct H as [G [EP EQ]]. destruct (g_same _ _ _ G) as [VP [VQ _]]. split; [|split].
  - apply (gr_simple so n n); [rewrite keep_need; auto|exact G].
  - unfold sitems in *. destruct (keep_gold so p K1 VP) as [L _]. rewrite L. exact EP.
  - unfold sitems in *. destruct (keep_gold so q K1 VQ) as [L _]. rewrite L. exact EQ.
Qed.

Definition takes (l: list (item * nat)) (acc: decision) : decision :=
  fold_right (fun iq acc => can_take (fst iq) (snd iq) acc) acc l.

Lemma takes_rev: forall l nxt,
  fold_left (fun acc (iq: item * nat) => can_take (fst iq) (snd iq) acc) (rev l) nxt = takes l nxt.
Proof.
  intros l nxt. unfold takes.
  pose proof (fold_left_rev_right (fun (iq: item * nat) acc => can_take (fst iq) (snd iq) acc) (rev l) nxt) as E.
  rewrite rev_involutive in E. symmetry. exact E.
Qed.

Lemma takes_split: forall l1 x l2 nxt, takes (l1 ++ x :: l2) nxt = takes l1 (can_take (fst x) (snd x) (takes l2 nxt)).
Proof.
  intros. unfold takes. rewrite fold_right_app. reflexivity.
Qed.

Lemma retr_items: forall p, items (apply_effects p [RetrieveEquipment]) = previtems p.
Proof. reflexivity. Qed.

Lemma pd_takes: forall gs gt rest l d1 d2, (forall x, In x l -> fst x <> Gold) -> PD gs gt rest d1 d2 ->
  PD gs gt rest (takes l d1) (takes l d2).
Proof.
  induction l as [|x l IH]; intros d1 d2 NG D; simpl; auto. apply pd_take.
  - apply NG. left. reflexivity.
  - apply IH; auto. intros y I. apply NG. right. exact I.
Qed.

Definition held_of (l: list item) (s: Stt) : list (item * nat) :=
  filter (fun iq : item * nat => Nat.ltb 0 (snd iq)) (map (fun i => (i, sitems i s)) l).

Definition gold_pre : list item := firstn 10 all_items.
Definition gold_post : list item := skipn 11 all_items.

Lemma items_split: all_items = gold_pre ++ Gold :: gold_post.
Proof. vm_compute. reflexivity. Qed.

Lemma pre_not_gold: forall i, In i gold_pre -> i <> Gold.
Proof.
  intros i I E. subst. assert (forallb (fun x => negb (item_beq x Gold)) gold_pre = true) as F by (vm_compute; reflexivity).
  rewrite forallb_forall in F. specialize (F _ I). vm_compute in F. discriminate.
Qed.

Lemma post_not_gold: forall i, In i gold_post -> i <> Gold.
Proof.
  intros i I E. subst. assert (forallb (fun x => negb (item_beq x Gold)) gold_post = true) as F by (vm_compute; reflexivity).
  rewrite forallb_forall in F. specialize (F _ I). vm_compute in F. discriminate.
Qed.

Lemma held_split: forall s, sitems Gold s <> 0 ->
  all_held_items s = held_of gold_pre s ++ (Gold, sitems Gold s) :: held_of gold_post s.
Proof.
  intros s G. unfold all_held_items, held_of. rewrite items_split, map_app, filter_app. f_equal.
  cbn [map filter fst snd]. destruct (0 <? sitems Gold s) eqn: E; [reflexivity|]. apply Nat.ltb_ge in E. lia.
Qed.

Lemma held_same: forall n l s t, (forall i, In i l -> i <> Gold) -> GoldR n s t -> held_of l s = held_of l t.
Proof.
  intros n l s t NG H. unfold held_of. f_equal. apply map_ext_in. intros i I. f_equal. apply (sitems_same n); auto.
Qed.

Lemma held_not_gold: forall l s x, (forall i, In i l -> i <> Gold) -> In x (held_of l s) -> fst x <> Gold.
Proof.
  intros l s x NG I. unfold held_of in I. apply filter_In in I. destruct I as [I _]. apply in_map_iff in I.
  destruct I as [i [E J]]. subst. apply NG. exact J.
Qed.

Lemma all_held_same: forall n s t, sitems Gold s = sitems Gold t -> GoldR n s t -> all_held_items s = all_held_items t.
Proof.
  intros n s t G H. unfold all_held_items. f_equal. apply map_ext. intro i. f_equal.
  destruct (eq_dec i Gold); [subst; exact G|apply (sitems_same n); auto].
Qed.

Section RetrieveTop.
  Variable ch: cid -> nat.
  Variable wn: nat.
  Variable f: next_step -> Qc.
  Hypothesis fN: forall x y, GoldR (ch (chapter x)) x y -> f (New x) = f (New y).
  Hypothesis fW: forall x y, GoldR wn x y -> f (Won x) = f (Won y).
  Variables (nxt: decision) (n: nat).
  Hypotheses (N1: 1 <= n) (NL: dn ch wn nxt <= n).

  Lemma dn_takes: forall l, dn ch wn (takes l nxt) = dn ch wn nxt.
  Proof.
    induction l as [|x l IH]; simpl; auto.
  Qed.

  Lemma retr_flat: forall fl s t, GoldR n s t ->
    Forall2 (OK f (RP n (sitems Gold s) (sitems Gold t))) (flatten_d fl s (retrieve_equipment nxt)) (flatten_d fl t (retrieve_equipment nxt)).
  Proof.
    induction fl as [|fl IH]; intros s t H; [constructor|].
    cbn [flatten_d]. unfold has_flag. rewrite (g_flags _ _ _ H). fold (has_flag Poisonned2 t).
    rewrite (has_same n s t Laumspur) by (discriminate || exact H).
    assert (RP n (sitems Gold s) (sitems Gold t) s t) as HR by (split; [exact H|split; reflexivity]).
    destruct (has_flag Poisonned2 t && has_item Laumspur t)%bool.
    { assert (Forall keeps [ClearFlag Poisonned2; LoseItem Laumspur 1]) as K by (repeat constructor; discriminate).
      destruct (rp_steps _ _ _ _ _ _ K HR) as [G [E1 E2]].
      eapply ok_wrap; [|specialize (IH _ _ G); rewrite E1, E2 in IH; exact IH].
      intros p q P. split; [apply (g_cur n); apply P|]. apply rp_steps; auto. }
    cbn zeta.
    destruct (Nat.eq_dec (sitems Gold s) (sitems Gold t)) as [EG|NE].
    - (* the same gold is held *)
      rewrite (all_held_same n s t EG H), !takes_rev.
      eapply ok_wrap; [|apply (same_ok ch wn f fN fW (dn ch wn nxt))].
      + intros p q [G _]. split; [apply (g_cur n); exact G|].
        apply (gr_simple RetrieveEquipment _ n); [exact NL|exact G].
      + rewrite dn_takes. apply le_n.
      + apply (gr_simple RetrieveEquipment _ n); [exact NL|exact H].
    - (* more than what is needed is held in both, as the amounts are different *)
      assert (n <= sitems Gold s /\ n <= sitems Gold t) as [GS GT].
      { destruct (g_rel _ _ _ H) as [E|[A [B _]]]; [exfalso; apply NE; unfold sitems; rewrite E; reflexivity|].
        split; assumption. }
      assert (sitems Gold s <> 0) as GS0 by lia. assert (sitems Gold t <> 0) as GT0 by lia.
      rewrite (held_split s GS0), (held_split t GT0), (held_same n gold_pre s t pre_not_gold H), (held_same n gold_post s t post_not_gold H), !takes_rev, !takes_split.
      cbn [fst snd]. set (rest := takes (held_of gold_post t) nxt).
      assert (dn ch wn rest <= dn ch wn nxt) as RN by (unfold rest; rewrite dn_takes; apply le_n).
      assert (forall p q, RP n (sitems Gold s) (sitems Gold t) p q ->
        Pend (dn ch wn nxt) (sitems Gold s) (sitems Gold t) (apply_effects p [RetrieveEquipment]) (apply_effects q [RetrieveEquipment])) as ST.
      { intros p q [G [EP EQ]]. destruct (g_same _ _ _ G) as [VP [VQ _]]. destruct (g_psame _ _ _ G) as [PP [PQ _]]. split; [|split].
        - apply (gr_simple RetrieveEquipment _ n); [simpl; lia|exact G].
        - left. reflexivity.
        - rewrite !retr_items, <- EP, <- EQ. apply (rel_total _ n); auto.
          + unfold sitems, gold_of in *. rewrite <- EP in GS0. exact GS0.
          + unfold sitems, gold_of in *. rewrite <- EQ in GT0. exact GT0.
          + apply G.
          + apply G. }
      eapply ok_wrap; [|apply (pd_flatten ch wn f fN fW (dn ch wn nxt) (sitems Gold s) (sitems Gold t) GS0 GT0 rest RN)].
      + intros p q P. split; [apply (g_cur n); apply P|]. apply ST. exact P.
      + apply pd_takes; [intros x I; exact (held_not_gold _ _ _ pre_not_gold I)|]. apply pd_gold.
      + apply ST. exact HR.
  Qed.
End RetrieveTop.

Lemma retrieve_top: forall ch wn nxt n, 1 <= n -> dn ch wn nxt <= n -> TopSim gold_tracker ch wn (retrieve_equipment nxt) n.
Proof.
  intros ch wn nxt n N1 NL f fN fW fl fuel s t H.
  eapply Forall2_impl; [|exact (retr_flat ch wn f fN fW nxt n N1 NL fl s t H)].
  intros o1 o2 K. apply K. split; [|split; reflexivity]. exact (R_unset_had gold_tracker n s t H).
Qed.

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
