(* What the five books do with their gold. The tables of gold_tables.v are checked on the books: no chapter needs more
   gold than what they say. *)
Require Import character chapters mps order lists proba combat rules value simulation gold_items gold_relation gold_needs books gold_tables.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.PArith.PArith.
Require Import Stdlib.Bool.Bool.
From Stdlib Require Import Lia.
Open Scope nat_scope.

Definition table_of (l: list (nat * nat)) : PM.t nat :=
  fold_left (fun t x => PM.add (Pos.of_succ_nat (fst x)) (snd x) t) l (PM.empty nat).

Definition table1 := table_of needs1.
Definition table2 := table_of needs2.
Definition table3 := table_of needs3.
Definition table5 := table_of needs5.

(* the gold is part of the score at the end of book 1 only *)
Theorem table1_ok: needs_ok book1 ALL table1 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem table2_ok: needs_ok book2 0 table2 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem table3_ok: needs_ok book3 0 table3 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem table5_ok: needs_ok book5 0 table5 = true.
Proof. vm_compute. reflexivity. Qed.

(* Book 3 starts at chapter 1, and needs 10 crowns from there: all the starts with 10 crowns or more are the same. *)
Lemma book3_start_needs: need_of table3 1 = 10.
Proof. vm_compute. reflexivity. Qed.

Theorem book3_gold_above_ten: forall score,
  (forall s t, GoldR 0 s t -> score s = score t) ->
  forall fuel s t, chapter s = 1 -> GoldR 10 s t -> value book3 score fuel s = value book3 score fuel t.
Proof.
  intros score HS fuel s t C H.
  apply (gold_needs_sound book3 0 score table3 table3_ok HS fuel s t).
  rewrite C, book3_start_needs. exact H.
Qed.

(* Book 4 never looks at the gold: all its starts are the same as far as gold goes. *)
Theorem book4_gold_unread: gold_unread_in book4 = true.
Proof. vm_compute. reflexivity. Qed.

Corollary book4_gold_same: forall score,
  (forall s t, GoldR 0 s t -> score s = score t) ->
  forall fuel s t, GoldR 0 s t -> value book4 score fuel s = value book4 score fuel t.
Proof.
  intros score HS. exact (gold_unread_same book4 score book4_gold_unread HS).
Qed.

(* The check is not vacuous: book 3 reads the gold *)
(* The checks are not vacuous: book 3 reads the gold, and these flags *)
Example book3_gold_read: gold_unread_in book3 = false.
Proof. vm_compute. reflexivity. Qed.

(* Book 4 never lowers the gold: all that a play of the book can do is add to it, up to the limit of 50. With the pouch of
   Book 4 and the pouch of Book 5, this gives at least 20 crowns at the start of Book 5, whatever the route. *)
Definition so_keeps_gold (so: simple_outcome) : bool :=
  match so with
  | LoseItem i _ => negb (item_beq i Gold)
  | LoseItemKind _ | StoreEquipment | RetrieveEquipment => false
  | _ => true
  end.

Fixpoint co_keeps_gold (o: chapter_outcome) : bool :=
  match o with
  | Fight _ c => co_keeps_gold c
  | OneRound _ a b c => co_keeps_gold a && co_keeps_gold b && co_keeps_gold c
  | Randomly l => forallb (fun x => co_keeps_gold (fst x)) l
  | Conditionally l => forallb (fun x => co_keeps_gold (snd x)) l
  | Simple sos c => forallb so_keeps_gold sos && co_keeps_gold c
  | Goto _ | GameLost | GameWon | EnterFight _ _ => true
  | LoseItemFrom sl _ c => negb (slot_beq sl SPouch) && co_keeps_gold c
  | Round _ c _ => co_keeps_gold c
  end.

Fixpoint dec_keeps_gold (d: decision) : bool :=
  match d with
  | App a b => dec_keeps_gold a && dec_keeps_gold b
  | retrieve_equipment _ | can_buy _ _ _ | can_sell _ _ _ | special _ => false
  | can_take _ _ n | conditional _ n | after_combat n => dec_keeps_gold n
  | none o => co_keeps_gold o
  | evade_fight _ _ _ _ => true
  | remove_item_from sl _ n => negb (slot_beq sl SPouch) && dec_keeps_gold n
  end.

Definition gold_kept_in (chapters: list (cid * decision)) : bool :=
  forallb (fun cd : cid * decision => dec_keeps_gold (snd cd)) chapters.

Theorem book4_gold_kept: gold_kept_in book4 = true.
Proof. vm_compute. reflexivity. Qed.

Definition GoldFloor (n: nat) (s: Stt) : Prop := ValidMap (items s) /\ n <= sitems Gold s.

Lemma rm_floor: forall n i q s, i <> Gold -> GoldFloor n s -> GoldFloor n (update_items (rm_item i q) s).
Proof.
  intros n i q s N [V G]. split.
  - assert (SameM (items s) (items s)) as S by (split; [exact V|split; [exact V|reflexivity]]).
    apply (same_rm i q) in S. apply S.
  - unfold sitems in *. cbn [items update_items]. unfold rm_item, bag_rm. rewrite Lookup.update_diff; auto.
Qed.

Lemma lose_backpack_floor: forall n s, GoldFloor n s -> GoldFloor n (update_items (lose_all_slot SBackpack) s).
Proof.
  intros n s [V G]. split.
  - cbn [items update_items]. unfold lose_all_slot.
    assert (SameM (fold_right (fun i itms => delete i itms) (items s) (slot_items SBackpack))
                  (fold_right (fun i itms => delete i itms) (items s) (slot_items SBackpack))) as S2.
    { clear -V. induction (slot_items SBackpack); simpl; [split; [assumption|split; [assumption|reflexivity]]|].
      apply same_delete. assumption. }
    apply S2.
  - unfold sitems in *. cbn [items update_items]. rewrite slot_gold; auto.
Qed.

Lemma gain_floor: forall n i q s, n <= 50 -> GoldFloor n s -> GoldFloor n (update_items (add_item i q) s).
Proof.
  intros n i q s L [V G]. split; [apply add_item_valid; exact V|].
  unfold sitems in *. cbn [items update_items].
  destruct (eq_dec i Gold) as [E|E].
  - subst i. destruct (Nat.eq_dec q 0) as [Q|Q].
    + subst. exact G.
    + rewrite add_gold_lookup; auto. unfold gold_of.
      destruct (lookup Gold (items s)) as [x|]; [destruct (Nat.ltb_spec (x + q) 51)|destruct (Nat.ltb_spec (0 + q) 51)]; lia.
  - rewrite add_gold_other; auto.
Qed.

Lemma simple_floor: forall n so s, n <= 50 -> so_keeps_gold so = true -> GoldFloor n s -> GoldFloor n (update_simple s so).
Proof.
  intros n so s L K H. destruct so as [dmg|v| | |i q|i q|slts|c| | |fl|fl]; simpl in K; try discriminate; try exact H.
  - apply gain_floor; auto.
  - cbn [update_simple]. destruct (q =? 0); [exact H|].
    assert (i <> Gold) as N.
    { intro E. subst. simpl in K. discriminate. }
    destruct i; try (apply (rm_floor n _ q s N H)).
    change (GoldFloor n (update_items (lose_all_slot SBackpack) (update_items (rm_item Backpack q) s))).
    apply lose_backpack_floor. apply rm_floor; auto.
  - cbn [update_simple]. destruct c; destruct (s_check Hunting (disciplines s)); try exact H;
      repeat match goal with |- context [if ?b then _ else _] => destruct b end;
      try exact H; try (apply rm_floor; [discriminate|exact H]).
Qed.

Lemma apply_floor: forall n sos s, n <= 50 -> forallb so_keeps_gold sos = true -> GoldFloor n s -> GoldFloor n (apply_effects s sos).
Proof.
  intros n sos. induction sos as [|so q IH]; intros s L K H; [exact H|].
  simpl in K. apply andb_true_iff in K. destruct K as [K1 K2]. apply IH; auto. apply simple_floor; auto.
Qed.

Definition ns_floor (n: nat) (ns: next_step) : Prop :=
  match ns with
  | Lost => True
  | Won x | New x => GoldFloor n x
  end.

Definition res_floor (n: nat) (p: Proba next_step) : Prop := forall ns, In ns (map fst p) -> ns_floor n ns.

Lemma floor_single: forall n ns, ns_floor n ns -> res_floor n (single_outcome ns).
Proof. intros n ns H x I. simpl in I. destruct I as [I|[]]. subst. exact H. Qed.

Lemma floor_lost: forall n, res_floor n (single_outcome Lost).
Proof. intros n. apply floor_single. exact I. Qed.

Lemma floor_merge: forall n l, (forall x, In x l -> res_floor n (snd x)) -> res_floor n (merge_probas l).
Proof.
  intros n l H ns I. apply merge_probas_keeps_keys_conv in I.
  apply in_map_iff in I. destruct I as [[k p] [E J]]. simpl in E. subst k.
  apply in_concat in J. destruct J as [m [Jm Jk]]. apply in_map_iff in Jm. destruct Jm as [x [E Ix]]. subst m.
  apply (H x Ix). apply (in_map fst) in Jk. exact Jk.
Qed.

Lemma after_fight_floor: forall n (rec: Stt -> chapter_outcome -> Proba next_step) s mds co te,
  co_keeps_gold co = true -> GoldFloor n s ->
  (forall t o, co_keeps_gold o = true -> GoldFloor n t -> res_floor n (rec t o)) ->
  res_floor n (after_fight rec s mds co te).
Proof.
  intros n rec s mds co te C H R. unfold after_fight.
  destruct te; cbv zeta; destruct (has_raw_mod MultiFight mds); destruct (Modifiers.extractl Modifiers.gFakeFight mds);
    repeat match goal with |- context [if ?b then _ else _] => destruct b end;
    first [apply floor_lost | apply floor_single; exact H | apply R; solve [exact C | reflexivity | exact H]].
Qed.

Lemma co_floor: forall n, n <= 50 -> forall fuel s o, co_keeps_gold o = true -> GoldFloor n s -> res_floor n (s_update_d fuel s o).
Proof.
  intros n L. induction fuel as [|md IH]; intros s o C H.
  { simpl. destruct (curendurance s =? 0); apply floor_lost. }
  destruct o as [fd co|fd a b c|lst|lst|sos co|next|sl q co| | |fd c|fd co rounds].
  - destruct fd as [osk opphp mds]. cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|].
    apply floor_merge. intros x I. apply in_map_iff in I. destruct I as [[te p] [E I]]. subst x. simpl.
    change (res_floor n (after_fight (s_update_d md) s mds co te)). apply after_fight_floor; auto.
  - destruct fd as [osk opphp mds]. cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|].
    simpl in C. apply andb_true_iff in C. destruct C as [C Cc]. apply andb_true_iff in C. destruct C as [Ca Cb].
    apply floor_merge. intros x I. apply in_map_iff in I. destruct I as [[[lw op] p] [E I]]. subst x. simpl.
    destruct (NatLemma.nat_cmp (curendurance s - lw) (opphp - op)); apply IH; auto.
  - cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|].
    simpl in C. rewrite forallb_forall in C.
    apply floor_merge. intros x I. apply in_map_iff in I. destruct I as [[y p] [E I]]. subst x. simpl.
    apply IH; auto. apply (C (y, p) I).
  - cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|].
    simpl in C. rewrite forallb_forall in C.
    destruct (filter (fun kv => check_cond s (fst kv)) lst) as [|[c0 co] tl] eqn: F; [apply floor_lost|].
    assert (In (c0, co) (filter (fun kv => check_cond s (fst kv)) lst)) as I by (rewrite F; left; reflexivity).
    apply filter_In in I. destruct I as [I _]. apply IH; auto. apply (C _ I).
  - cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|].
    simpl in C. apply andb_true_iff in C. destruct C as [C1 C2]. apply IH; auto. apply apply_floor; auto.
  - cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|].
    repeat match goal with |- context [if ?b then _ else _] => destruct b end;
      first [apply floor_lost | apply floor_single; exact H].
  - cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|].
    simpl in C. apply andb_true_iff in C. destruct C as [C1 C2].
    destruct q as [|nq]; [apply IH; auto|].
    destruct (held_items sl s) as [|x l] eqn: HE; [apply IH; auto|].
    apply floor_merge. intros y Iy. apply in_map_iff in Iy. destruct Iy as [[a m] [E Iy]]. subst y. cbn [snd].
    apply IH; [simpl; rewrite C1, C2; reflexivity|]. apply simple_floor; auto. simpl.
    assert (a <> Gold) as N.
    { rewrite <- HE in Iy. pose proof (held_in_slot sl s (a, m) Iy) as IS. simpl in IS. eapply not_gold_slot; [|exact IS].
      intro X. subst sl. simpl in C1. discriminate. }
    rewrite beq_gold; auto.
  - cbn [s_update_d]. destruct (curendurance s =? 0); apply floor_lost.
  - cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|]. apply floor_single. exact H.
  - cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|]. apply floor_single. exact H.
  - destruct fd as [osk opphp mds]. cbn [s_update_d]. destruct (curendurance s =? 0); [apply floor_lost|].
    intros ns I. destruct (add_proba_keeps_keys_conv _ _ _ ns Logic.eq_refl I) as [I1|I1].
    + clear I. revert ns I1. apply floor_merge. intros x J. apply in_map_iff in J. destruct J as [[te p] [E J]]. subst x. simpl.
      change (res_floor n (after_fight (s_update_d md) s mds co te)). apply after_fight_floor; auto.
    + apply in_map_iff in I1. destruct I1 as [[k p] [E J]]. simpl in E. subst ns.
      apply in_map_iff in J. destruct J as [[[lw op] p2] [E J]]. inversion E. subst k p. exact H.
Qed.

Lemma drop_not_gold: forall i s lst l, tcan_take i s = MustDrop lst -> In l lst -> l <> Gold.
Proof.
  intros i s lst l T I. unfold tcan_take in T. destruct (item_slot i) eqn: SL.
  - destruct (Nat.ltb (length (held_items SWeapon s)) 2); inversion T; subst.
    apply in_map_iff in I. destruct I as [iq [E J]]. subst l.
    eapply not_gold_slot; [|exact (held_in_slot _ _ _ J)]. discriminate.
  - destruct (negb (has_item Backpack s)); [discriminate|].
    match type of T with (if ?b then _ else _) = _ => destruct b end; inversion T; subst.
    apply in_map_iff in I. destruct I as [iq [E J]]. subst l.
    eapply not_gold_slot; [|exact (held_in_slot _ _ _ J)]. discriminate.
  - repeat match type of T with (if ?b then _ else _) = _ => destruct b end; inversion T; subst;
      destruct I as [I|[]]; subst; discriminate.
  - discriminate.
Qed.

Lemma lose_keeps: forall l, l <> Gold -> so_keeps_gold (LoseItem l 1) = true.
Proof. intros l N. simpl. rewrite beq_gold; auto. Qed.

Lemma drinkable_in {A: Type}: forall (u: list A) p q x,
  In x (filter p u ++ match rev (filter q u) with [] => [] | y :: _ => [y] end) -> In x u.
Proof.
  intros u p q x I. apply in_app_iff in I. destruct I as [I|I]; [apply filter_In in I; apply I|].
  destruct (rev (filter q u)) as [|y t] eqn: E; [destruct I|]. destruct I as [I|[]]. subst y.
  assert (In x (rev (filter q u))) as J by (rewrite E; left; reflexivity).
  apply in_rev in J. apply filter_In in J. apply J.
Qed.

Lemma with_effect_keeps: forall sos l o, forallb so_keeps_gold sos = true -> (forall x, In x l -> co_keeps_gold x = true) ->
  In o (map (Simple sos) l) -> co_keeps_gold o = true.
Proof.
  intros sos l o S A I. apply in_map_iff in I. destruct I as [x [E J]]. subst o. simpl. rewrite S, (A x J). reflexivity.
Qed.

Lemma flatten_keeps_gold: forall fuel s d o, dec_keeps_gold d = true -> In o (flatten_d fuel s d) -> co_keeps_gold o = true.
Proof.
  induction fuel as [|f IH]; intros s d o D I; [destruct I|].
  cbn [flatten_d] in I. destruct (has_flag Poisonned2 s && has_item Laumspur s).
  { refine (with_effect_keeps _ _ _ _ _ I); [reflexivity|intros x J; exact (IH _ _ _ D J)]. }
  destruct d as [a b|n|i q n|i q n|i q n|c n|sp|co|x y z w|n|sl q n]; simpl in D; try discriminate; cbv beta iota zeta in I.
  - apply in_app_iff in I. apply andb_true_iff in D. destruct D as [D1 D2]. destruct I as [I|I]; [exact (IH _ _ _ D1 I)|exact (IH _ _ _ D2 I)].
  - destruct i; cbv beta iota in I;
      first
        [ refine (with_effect_keeps _ _ _ _ _ I); [reflexivity|intros x J; exact (IH _ _ _ D J)]
        | destruct q as [|[|q]]; cbv beta iota in I;
          [ exact (IH _ _ _ D I)
          | match type of I with context [tcan_take ?x s] => destruct (tcan_take x s) as [|lst|] eqn: T end;
            [ refine (with_effect_keeps _ _ _ _ _ I); [reflexivity|intros x J; exact (IH _ _ _ D J)]
            | apply in_app_iff in I; destruct I as [I|I]; [exact (IH _ _ _ D I)|];
              apply in_flat_map in I; destruct I as [l [Il I]]; pose proof (drop_not_gold _ _ _ _ T Il) as N; cbv beta in I;
              repeat match type of I with In _ (if ?b then _ else _) => destruct b end; try destruct I;
              refine (with_effect_keeps _ _ _ _ _ I);
              first [ simpl; rewrite ?beq_gold; auto; fail | intros x J; eapply IH; [|exact J]; exact D ]
            | exact (IH _ _ _ D I) ]
          | eapply IH; [|exact I]; exact D ] ].
  - destruct (check_cond s c); [eapply IH; eauto|destruct I].
  - destruct (has_combat co) as [fd|]; [|destruct I as [<-|[]]; exact D].
    destruct (fd_has NoPotion fd); [destruct I as [<-|[]]; exact D|].
    destruct I as [<-|I]; [exact D|]. apply in_app_iff in I.
    destruct I as [I|I]; repeat match type of I with context [if ?b then _ else _] => destruct b end;
      first [contradiction | destruct I as [<-|[]]; simpl; rewrite D; reflexivity | destruct I].
  - destruct I.
  - destruct (max_hp s - curendurance s =? 0); [eapply IH; eauto|].
    apply in_app_iff in I. destruct I as [I|I]; [eapply IH; eauto|].
    apply in_flat_map in I. destruct I as [[[u i] h] [J I]]. cbv beta iota in I.
    assert (i <> Gold) as N.
    { apply drinkable_in in J. apply filter_In in J. destruct J as [J _]. simpl in J.
      repeat (destruct J as [J|J]; [inversion J; subst; discriminate|]). destruct J. }
    refine (with_effect_keeps _ _ _ _ _ I); [|intros x K; eapply IH; [|exact K]; exact D].
    simpl. rewrite beq_gold; auto.
  - apply andb_true_iff in D. destruct D as [D1 D2].
    assert (sl <> SPouch) as NP by (intro X; subst; discriminate D1).
    destruct q as [|m]; [eapply IH; eauto|].
    destruct (held_items sl s) as [|x l] eqn: HE; [eapply IH; eauto|].
    apply in_flat_map in I. destruct I as [iq [Iq I]]. rewrite <- HE in Iq.
    assert (fst iq <> Gold) as N by (eapply not_gold_slot; [exact NP|exact (held_in_slot _ _ _ Iq)]).
    refine (with_effect_keeps _ _ _ _ _ I); [|intros y K; eapply IH; [|exact K]; simpl; rewrite D1, D2; reflexivity].
    simpl. rewrite beq_gold; auto.
Qed.

Lemma flatten_fuel: forall s d, flatten_decision s d = flatten_d 1000 s d.
Proof. reflexivity. Qed.

Lemma update_fuel: forall s o, s_update s o = s_update_d 1000 s o.
Proof. reflexivity. Qed.

(* Each step of a book that passes the check keeps at least the gold it had, for any amount up to the limit *)
Theorem play_keeps_gold: forall n chapters s, n <= 50 -> gold_kept_in chapters = true -> GoldFloor n s ->
  forall o, In o (step chapters s) -> res_floor n o.
Proof.
  intros n chapters s L K H o I. unfold step in I.
  destruct (find_chapter (chapter s) chapters) as [d|] eqn: F; [|destruct I].
  apply in_map_iff in I. destruct I as [co [E I]]. subst o.
  apply find_chapter_in in F. unfold gold_kept_in in K. rewrite forallb_forall in K.
  rewrite flatten_fuel in I. rewrite update_fuel.
  apply co_floor; [exact L|exact (flatten_keeps_gold _ s d co (K _ F) I)|exact H].
Qed.

Theorem book4_keeps_gold: forall n s, n <= 50 -> GoldFloor n s -> forall o, In o (step book4 s) -> res_floor n o.
Proof. intros n s L. apply play_keeps_gold; [exact L|exact book4_gold_kept]. Qed.

(* The pouch of Books 4 and 5: a number from 0 to 9 plus 10, added to what the character has *)
Definition open_pouch (r: nat) (m: Items) : Items := add_item Gold (10 + r) m.

Lemma pouch_at_least: forall a r m, a <= 40 -> ValidMap m -> a <= gold_of m -> a + 10 <= gold_of (open_pouch r m).
Proof.
  intros a r m L V G. unfold open_pouch, gold_of at 1. rewrite add_gold_lookup; [|lia|exact V].
  destruct (Nat.ltb_spec (gold_of m + (10 + r)) 51); lia.
Qed.

(* Book 4 starts with its pouch, and never lowers it: Book 5 starts with at least 20 crowns once it has its own pouch *)
Theorem book5_start_gold: forall m_start m_end r4 r5,
  ValidMap m_start -> ValidMap m_end -> gold_of (open_pouch r4 m_start) <= gold_of m_end -> 20 <= gold_of (open_pouch r5 m_end).
Proof.
  intros ms me r4 r5 VS VE G. pose proof (pouch_at_least 0 r4 ms ltac:(lia) VS ltac:(lia)) as P4.
  apply (pouch_at_least 10 r5 me); [lia|exact VE|lia].
Qed.
