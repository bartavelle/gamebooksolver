(* The gold that is more than what can be used from a chapter can be ignored: this is GoldNeeds in the Rust solver. *)
Require Import character.
Require Import chapters.
Require Import mps.
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
Open Scope nat_scope.

Section MapLemmas.
  Context {K V: Set} `{OrdDec K}.

  Lemma lt_head_none: forall (m: Mp K V) k k0, ValidMap m -> Forall (fun pr : K * V => let (ck, _) := pr in lt k0 ck) m ->
    cmp k k0 <> GT -> lookup k m = None.
  Proof.
    intros m k k0 VM F C. apply Lookup.lowkey_not_found; auto.
    eapply Forall_impl; [|exact F]. intros [ck cv] L. simpl in *.
    destruct (cmp k k0) eqn: E; [|apply cmp_eq in E; subst; assumption|contradiction].
    apply cmp_correct in E. eapply lt_trans; eauto.
  Qed.

  Lemma map_ext_valid: forall (a b: Mp K V), ValidMap a -> ValidMap b -> (forall k, lookup k a = lookup k b) -> a = b.
  Proof.
    induction a as [|[k1 v1] a IH]; intros b VA VB E.
    - destruct b as [|[k2 v2] b]; auto. specialize (E k2). simpl in E. rewrite cmp_refl in E. discriminate.
    - destruct b as [|[k2 v2] b].
      + specialize (E k1). simpl in E. rewrite cmp_refl in E. discriminate.
      + apply Valid.head in VA. apply Valid.head in VB. destruct VA as [VA FA], VB as [VB FB].
        destruct (cmp k1 k2) eqn: C.
        * specialize (E k1). simpl in E. rewrite cmp_refl, C in E. discriminate.
        * apply cmp_eq in C. subst k2.
          pose proof (E k1) as E1. simpl in E1. rewrite cmp_refl in E1. inversion E1; subst v2. f_equal.
          apply IH; auto. intro k. specialize (E k). simpl in E.
          destruct (cmp k k1) eqn: D; try assumption;
            (rewrite (lt_head_none a k k1), (lt_head_none b k k1); auto; rewrite D; discriminate).
        * apply cmp_opp in C. specialize (E k2). simpl in E. rewrite cmp_refl, C in E. discriminate.
  Qed.

  Lemma lookup_delete_diff: forall (m: Mp K V) k1 k2, ValidMap m -> k1 <> k2 -> lookup k1 (delete k2 m) = lookup k1 m.
  Proof.
    induction m as [|[ck cv] m IH]; intros k1 k2 VM D; simpl; auto.
    apply Valid.head in VM. destruct VM as [VM F].
    destruct (cmp k2 ck) eqn: C; simpl; auto.
    - apply cmp_eq in C. subst ck. destruct (cmp k1 k2) eqn: C1; auto.
      + apply lt_head_none with (k0 := k2); auto. rewrite C1. discriminate.
      + apply cmp_eq in C1. contradiction.
    - rewrite IH; auto.
  Qed.

  Lemma lookup_filter: forall (m: Mp K V) p k, ValidMap m ->
    lookup k (filter p m) = match lookup k m with Some v => if p (k, v) then Some v else None | None => None end.
  Proof.
    induction m as [|[ck cv] m IH]; intros p k VM; simpl; auto.
    apply Valid.head in VM. destruct VM as [VM F].
    assert (cmp k ck <> GT -> lookup k (filter p m) = None) as NONE.
    { intro C. apply lt_head_none with (k0 := ck); auto.
      - apply Valid.filter. assumption.
      - apply Forall_forall. intros x I. apply filter_In in I. destruct I as [I _].
        rewrite Forall_forall in F. apply F. assumption. }
    destruct (cmp k ck) eqn: C; destruct (p (ck, cv)) eqn: P; simpl; rewrite ?C.
    - reflexivity.
    - apply NONE. discriminate.
    - apply cmp_eq in C. subst. rewrite P. reflexivity.
    - apply cmp_eq in C. subst. rewrite P. apply NONE. discriminate.
    - apply IH. assumption.
    - apply IH. assumption.
  Qed.
End MapLemmas.

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
  - rewrite !lookup_delete_diff; auto.
Qed.

Lemma rel_delete: forall n i a b, ValidMap a -> ValidMap b -> GoldRel n a b -> GoldRel n (delete i a) (delete i b).
Proof.
  intros n i a b VA VB G. destruct (eq_dec i Gold).
  - subst. left. rewrite !Lookup.lookup_delete; auto.
  - unfold GoldRel, gold_of in *. rewrite !lookup_delete_diff; auto.
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
  intros p a b [VA [VB E]] P. apply map_ext_valid; try (apply Valid.filter; assumption).
  intro k. rewrite !lookup_filter; auto. destruct (eq_dec k Gold).
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
  g_prev: previtems s = previtems t;
  g_vprev: ValidMap (previtems s);
  g_same: SameM (items s) (items t);
  g_rel: GoldRel n (items s) (items t);
}.

Ltac gr_fields H := destruct H; constructor; unfold update_items, update_endurance, update_flags, update_chapter; simpl; auto.

Lemma gr_mono: forall m n s t, m <= n -> GoldR n s t -> GoldR m s t.
Proof.
  intros m n s t L H. gr_fields H. destruct g_rel0 as [G|[A [B C]]]; [left; assumption|right; lia].
Qed.

Lemma gr_full: forall n s t, ALL <= n -> GoldR n s t -> lookup Gold (items s) = lookup Gold (items t).
Proof.
  intros n s t A H. destruct (g_rel _ _ _ H) as [G|[_ [_ C]]]; [assumption|lia].
Qed.

Lemma gr_top: forall n m s t, ALL <= n -> ALL <= m -> GoldR n s t -> GoldR m s t.
Proof.
  intros n m s t A M H. pose proof (gr_full _ _ _ A H) as G. gr_fields H. left. assumption.
Qed.

Lemma gr_items_eq: forall n s t, ALL <= n -> GoldR n s t -> items s = items t.
Proof.
  intros n s t A H. pose proof (gr_full _ _ _ A H) as G. destruct (g_same _ _ _ H) as [VA [VB E]].
  apply map_ext_valid; auto. intro k. destruct (eq_dec k Gold); [subst; assumption|apply E; assumption].
Qed.

Lemma gr_update_items: forall a n s t (f: Items -> Items), GoldR n s t ->
  SameM (f (items s)) (f (items t)) -> GoldRel a (f (items s)) (f (items t)) ->
  GoldR a (update_items f s) (update_items f t).
Proof.
  intros a n s t f H S G. gr_fields H.
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
  eapply gr_update_items; eauto.
  - apply same_rm, H.
  - apply rel_rm_other; auto. apply H.
Qed.

Definition gold_sn (so: simple_outcome) (a: nat) : nat :=
  match so with
  | LoseItem i q => if item_beq i Gold then Nat.max a (Nat.min ALL (a + q)) else a
  | StoreEquipment | RetrieveEquipment => Nat.max ALL a
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
    eapply gr_update_items; [exact H|apply same_add, H|].
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
        eapply gr_update_items; [exact H|apply same_rm, H|]. eapply gr_lose_gold; eauto.
      * rewrite beq_gold in L by assumption.
        assert (GoldR a (update_items (rm_item i q) s) (update_items (rm_item i q) t)) as G1.
        { apply gr_rm; auto. eapply gr_mono; eauto. }
        destruct i; simpl; try exact G1.
        eapply gr_update_items; [exact G1|apply same_slot, G1|].
        apply rel_slot; apply G1.
  - (* LoseItemKind *)
    apply gr_clamp. eapply gr_update_items; [exact H|apply same_slots, H|].
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
    apply gr_clamp. assert (ALL <= n) as AN by lia.
    pose proof (gr_items_eq _ _ _ AN H) as IE. destruct (g_same _ _ _ H) as [VA [VB _]].
    destruct H; constructor; simpl; auto.
    + split; [constructor|]. split; [constructor|]. reflexivity.
    + left. reflexivity.
  - (* RetrieveEquipment *)
    apply gr_clamp. pose proof (g_vprev _ _ _ H) as VP. destruct H; constructor; simpl; auto.
    + constructor.
    + split; [assumption|]. split; [rewrite <- g_prev0; assumption|]. intros. rewrite g_prev0. reflexivity.
    + left. rewrite g_prev0. reflexivity.
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
Defined.

Module PM := FMapPositive.PositiveMap.

Definition need_of (t: PM.t nat) (c: cid) : nat :=
  match PM.find (Pos.of_succ_nat c) t with Some v => v | None => 0 end.

Definition gold_dec_need (t: PM.t nat) (wn: nat) (d: decision) : nat := dec_need gold_tracker (need_of t) wn d.

(* a table is fine if no chapter needs more than what it says *)
Definition needs_ok (chapters: list (cid * decision)) (wn: nat) (t: PM.t nat) : bool :=
  forallb (fun cd : cid * decision => gold_dec_need t wn (snd cd) <=? need_of t (fst cd)) chapters.

Fixpoint needs_from (chapters: list (cid * decision)) (wn: nat) (fuel: nat) (t: PM.t nat) : PM.t nat :=
  match fuel with
  | O => t
  | S f =>
      let nt := fold_left (fun acc (cd: cid * decision) =>
        PM.add (Pos.of_succ_nat (fst cd)) (Nat.min ALL (gold_dec_need acc wn (snd cd))) acc) chapters t in
      needs_from chapters wn f nt
  end.

Lemma find_chapter_in: forall chapters c d, find_chapter c chapters = Some d -> In (c, d) chapters.
Proof.
  induction chapters as [|[k x] q IH]; intros c d F; simpl in F; [discriminate|].
  destruct (k =? c) eqn: E.
  - apply Nat.eqb_eq in E. inversion F; subst. left. reflexivity.
  - right. apply IH. assumption.
Qed.

(* The gold of a state can be replaced by anything that is at least what its chapter needs *)
Theorem gold_needs_sound: forall chapters wn score t,
  needs_ok chapters wn t = true ->
  (forall s x, GoldR wn s x -> score s = score x) ->
  forall fuel s x, GoldR (need_of t (chapter s)) s x -> value chapters score fuel s = value chapters score fuel x.
Proof.
  intros chapters wn score t OK HS fuel s x H.
  apply (value_same gold_tracker (need_of t) wn chapters score HS); auto.
  intros c d F. apply find_chapter_in in F. unfold needs_ok in OK. rewrite forallb_forall in OK.
  specialize (OK _ F). apply Nat.leb_le in OK. exact OK.
Qed.
