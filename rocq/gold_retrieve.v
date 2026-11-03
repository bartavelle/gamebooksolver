(* Getting the equipment back when entering a chapter (c14 of Book 5) adds the gold that is held to the gold that was
   stored, so that two states can get the same total from different gains. Their outcomes are not the same, but they have
   the same value. *)
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
Require Import gold_relation.

(* what doesn't change the gold *)
Definition keeps (so: simple_outcome) : Prop :=
  match so with
  | GainItem i _ | LoseItem i _ => i <> Gold
  | ClearFlag _ => True
  | _ => False
  end.

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

Lemma keep_need: forall so a, keeps so -> gold_simple_need so a = a.
Proof.
  intros so a K. destruct so; simpl in K; try contradiction; try reflexivity.
  cbn [gold_simple_need]. rewrite beq_gold; auto.
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
