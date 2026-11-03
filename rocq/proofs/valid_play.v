(* Playing a book keeps a state valid, as long as the book respects what valid_check.v computes. *)
Require Import character chapters mps order lists proba combat rules simulation.
Require Import valid_check valid_items valid_fight valid_state.
Require books.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
Require Import Stdlib.QArith.Qcanon.
From Stdlib Require Import Lia.
Open Scope nat_scope.

Definition ns_ok (ns: next_step) : Prop :=
  match ns with
  | Lost => True
  | Won x | New x => ValidState x
  end.

Definition res_ok (p: Proba next_step) : Prop := forall ns, In ns (map fst p) -> ns_ok ns.

Lemma res_single: forall ns, ns_ok ns -> res_ok (single_outcome ns).
Proof.
  intros ns H x I. simpl in I. destruct I as [I|[]]. subst. exact H.
Qed.

Lemma res_lost: res_ok (single_outcome Lost).
Proof.
  apply res_single. exact I.
Qed.

Lemma res_merge: forall l, (forall x, In x l -> res_ok (snd x)) -> res_ok (merge_probas l).
Proof.
  intros l H ns I. apply merge_probas_keeps_keys_conv in I.
  apply in_map_iff in I. destruct I as [[k p] [E J]]. simpl in E. subst k.
  apply in_concat in J. destruct J as [m [Jm Jk]]. apply in_map_iff in Jm. destruct Jm as [x [E Ix]]. subst m.
  apply (H x Ix). apply (in_map fst) in Jk. exact Jk.
Qed.

Lemma res_add: forall a b, res_ok a -> res_ok b -> res_ok (add_proba a b).
Proof.
  intros a b A B ns I. destruct (add_proba_keeps_keys_conv _ _ _ ns Logic.eq_refl I); auto.
Qed.

Lemma s_book_flags: forall s f, book (update_flags f s) = book s.
Proof. intros. destruct s; reflexivity. Qed.

Lemma s_book_endur: forall s f, book (update_endurance f s) = book s.
Proof. intros. destruct s; reflexivity. Qed.

Lemma s_cur_flags: forall s f, curendurance (update_flags f s) = curendurance s.
Proof. intros. destruct s; reflexivity. Qed.

Lemma maxhp_flags: forall s f, max_hp (update_flags f s) = max_hp s.
Proof. intros. destruct s; reflexivity. Qed.

Lemma maxhp_chapter: forall s c, max_hp (update_chapter c s) = max_hp s.
Proof. intros. destruct s; reflexivity. Qed.

Lemma goto_ok: forall fuel s c, Weak s -> res_ok (s_update_d fuel s (Goto c)).
Proof.
  intros fuel s c H. destruct fuel as [|md]; cbn [s_update_d].
  { destruct (curendurance s =? 0); apply res_lost. }
  destruct (curendurance s =? 0) eqn: Z; [apply res_lost|]. apply Nat.eqb_neq in Z.
  pose proof (weak_chapter s c H) as WC.
  destruct (is_story_chapter (book s) (chapter s) && has_flag Poisonned2 s).
  - destruct (Nat.eqb _ 0) eqn: E; [apply res_lost|]. apply res_single. apply Nat.eqb_neq in E. apply weak_valid; [|lia].
    apply weak_endur; auto. rewrite maxhp_chapter. pose proof (w_cur _ H). destruct s; simpl in *; lia.
  - destruct (_ && _).
    + apply res_single. apply weak_valid; [apply heal_weak; exact WC|].
      pose proof (maxhp_pos _ WC). unfold heal. destruct s; simpl in *. lia.
    + apply res_single. apply weak_valid; [exact WC|]. destruct s; simpl in *. lia.
Qed.

Lemma simple_step: forall md s sos n, (forall m, res_ok (s_update_d m (apply_effects s sos) n)) -> res_ok (s_update_d md s (Simple sos n)).
Proof.
  intros md s sos n H. destruct md as [|md]; cbn [s_update_d]; destruct (curendurance s =? 0); try apply res_lost. apply H.
Qed.

Lemma from_zero: forall md s sl n, (forall m, res_ok (s_update_d m s n)) -> res_ok (s_update_d md s (LoseItemFrom sl 0 n)).
Proof.
  intros md s sl n H. destruct md as [|md]; cbn [s_update_d]; destruct (curendurance s =? 0); try apply res_lost. apply H.
Qed.

Lemma from_one: forall md s sl n,
  (held_items sl s = [] -> forall m, res_ok (s_update_d m s n)) ->
  (forall a q, In (a, q) (held_items sl s) -> forall m, res_ok (s_update_d m (update_simple s (LoseItem a 1)) (LoseItemFrom sl 0 n))) ->
  res_ok (s_update_d md s (LoseItemFrom sl 1 n)).
Proof.
  intros md s sl n E M. destruct md as [|md]; cbn [s_update_d]; destruct (curendurance s =? 0); try apply res_lost.
  destruct (held_items sl s) as [|x l] eqn: HE; [apply E; reflexivity|].
  apply res_merge. intros y Iy. apply in_map_iff in Iy. destruct Iy as [[a n2] [Ey Iy]]. subst y. simpl.
  apply (M a n2). exact Iy.
Qed.

Lemma cond_has_qty: forall c i s, cond_has c i = true -> check_cond s c = true -> 0 < qty i (items s).
Proof.
  induction c; intros i0 s H C; simpl in H; try discriminate.
  - apply orb_true_iff in H. simpl in C. apply andb_true_iff in C. destruct C, H; [apply IHc1|apply IHc2]; auto.
  - apply andb_true_iff in H. destruct H as [E Q]. apply eqb_correct in E. subst. apply Nat.ltb_lt in Q.
    simpl in C. apply Nat.leb_le in C. change (qty i (items s)) with (sitems i s). lia.
Qed.

Lemma swap_shape: forall bk c o, swap_safe bk c o = true -> exists i cid,
  o = Simple [LoseItem i 1] (LoseItemFrom SBackpack 1 (Simple [GainItem i 1] (Goto cid))) /\
  cond_has c i = true /\ item_slot i = SBackpack /\ backpack_slots bk i = 1 /\ stackable i = false.
Proof.
  intros bk c o H. unfold swap_safe in H.
  repeat (match goal with H: match ?x with _ => _ end = true |- _ => destruct x; try discriminate end).
  apply andb_true_iff in H. destruct H as [H S5]. apply andb_true_iff in H. destruct H as [H S4].
  apply andb_true_iff in H. destruct H as [H S3]. apply andb_true_iff in H. destruct H as [E S2].
  apply eqb_correct in E. subst. apply eqb_correct in S3. apply Nat.eqb_eq in S4. apply negb_true_iff in S5.
  eexists; eexists; repeat split; eauto.
Qed.

(* The one case where a book gives something in the Backpack: it is taken out first, and something else is lost *)
Lemma swap_ok: forall bk c o md s, swap_safe bk c o = true -> check_cond s c = true -> book s = bk -> Weak s ->
  res_ok (s_update_d md s o).
Proof.
  intros bk c o md s SF C B H. destruct (swap_shape _ _ _ SF) as [i [cid [E [HC [SL [ON ST]]]]]]. subst o.
  pose proof (cond_has_qty _ _ _ HC C) as Qi.
  pose proof (w_items _ H) as K. pose proof (ok_map _ _ K) as V.
  assert (i <> Backpack) as NB by (intro X; subst; discriminate).
  assert (used_g bk i = 1) as G by (unfold used_g; rewrite SL; simpl; exact ON).
  assert (0 < qty Backpack (items s)) as HB.
  { destruct (ok_contents _ _ K) as [X|X]; [exact X|].
    pose proof (rm_strict (count_g SBackpack) (items s) i V Qi) as Y.
    assert (count_g SBackpack i = 1) as CG by (unfold count_g; rewrite SL; reflexivity). lia. }
  assert (forall m w, Weak w -> book w = bk -> esum (used_g bk) (items w) + 1 <= 8 -> 0 < qty Backpack (items w) ->
    res_ok (s_update_d m w (Simple [GainItem i 1] (Goto cid)))) as FIN.
  { intros m w Hw Bw U Pb. apply simple_step. intro m2. apply goto_ok.
    change (Weak (update_simple w (GainItem i 1))). apply weak_gain; auto. unfold CanGain. rewrite SL, Bw. split; [lia|exact Pb]. }
  apply simple_step. intro m.
  change (apply_effects s [LoseItem i 1]) with (update_simple s (LoseItem i 1)).
  set (s1 := update_simple s (LoseItem i 1)).
  assert (Weak s1) as W1 by apply (weak_lose s i 1 H).
  assert (book s1 = bk) as B1 by (unfold s1; rewrite book_simple; exact B).
  assert (items s1 = rm_item i 1 (items s)) as I1 by (apply items_lose; exact NB).
  assert (esum (used_g bk) (items s1) + 1 <= 8) as U1.
  { rewrite I1. pose proof (rm_strict (used_g bk) (items s) i V Qi) as Y. pose proof (ok_backpack _ _ K) as X. rewrite B in X. lia. }
  assert (0 < qty Backpack (items s1)) as P1 by (rewrite I1, qty_rm_diff; auto; discriminate).
  apply from_one.
  { intros _ m2. apply FIN; auto. }
  intros a n Iy m2. rewrite held_eq in Iy.
  destruct (held_in _ _ _ Iy) as [Sa [Qa Pa]]. cbn [fst snd] in Sa, Qa, Pa.
  assert (a <> Backpack) as NBa by (intro X; subst; discriminate).
  apply from_zero. intro m3. apply FIN.
  - apply weak_lose. exact W1.
  - rewrite book_simple. exact B1.
  - rewrite items_lose by exact NBa. pose proof (esum_mono (used_g bk) (rm_item a 1 (items s1)) (items s1) (fun j => qty_rm_le a 1 (items s1) j (ok_map _ _ (w_items _ W1)))). lia.
  - rewrite items_lose by exact NBa. rewrite qty_rm_diff; [exact P1|apply (ok_map _ _ (w_items _ W1))|intro X; apply NBa; symmetry; exact X].
Qed.

Lemma after_fight_ok: forall bk (rec: Stt -> chapter_outcome -> Proba next_step) s mds co te,
  (forall x o, co_safe bk o = true -> book x = bk -> Weak x -> res_ok (rec x o)) ->
  co_safe bk co = true -> book s = bk -> Weak s -> 0 < curendurance s -> te_le (curendurance s) te ->
  res_ok (after_fight rec s mds co te).
Proof.
  intros bk rec s mds co te R CS B H P L.
  assert (forall nco nhp, co_safe bk nco = true -> nhp <= max_hp s ->
    res_ok (let stt1 := update_flags (s_set HadCombat) s in
            let stt2 := if has_raw_mod MultiFight mds then stt1 else unset_potions stt1 in
            match Modifiers.extractl Modifiers.gFakeFight mds with
            | Some cid2 => if nhp =? 0 then single_outcome (New (update_chapter cid2 stt2)) else rec stt2 nco
            | None => if nhp =? 0 then single_outcome Lost
                      else rec (update_endurance (fun _ => nhp) stt2) nco
            end)) as CORE.
  { intros nco nhp NC NH. cbv zeta.
    set (st := if has_raw_mod MultiFight mds then _ else _).
    assert (Weak st /\ book st = bk /\ curendurance st = curendurance s /\ max_hp st = max_hp s) as [W2 [B2 [C2 M2]]].
    { unfold st. assert (Weak (update_flags (s_set HadCombat) s)) as W1 by (apply weak_flags; auto; intros; apply set_valid; assumption).
      destruct (has_raw_mod MultiFight mds).
      - split; [exact W1|split; [rewrite s_book_flags; exact B|split; [apply s_cur_flags|apply maxhp_flags]]].
      - unfold unset_potions. split; [apply weak_unset_potions; exact W1|].
        split; [rewrite !s_book_flags; exact B|].
        split; [rewrite !s_cur_flags; reflexivity|rewrite !maxhp_flags; reflexivity]. }
    destruct (Modifiers.extractl Modifiers.gFakeFight mds).
    - destruct (nhp =? 0).
      + apply res_single. apply weak_valid; [apply weak_chapter; exact W2|]. destruct st; simpl in *. lia.
      + apply R; auto.
    - destruct (nhp =? 0) eqn: E; [apply res_lost|]. apply R.
      + exact NC.
      + rewrite s_book_endur. exact B2.
      + apply weak_endur; auto. rewrite M2. exact NH. }
  pose proof (w_cur _ H) as CM. pose proof (maxhp_pos _ H) as MP.
  unfold after_fight. destruct te as [c h|h|c h|c|c h]; simpl in L; apply CORE; auto; try (simpl; reflexivity); try lia.
Qed.

Lemma co_ok: forall bk fuel s o, co_safe bk o = true -> book s = bk -> Weak s -> res_ok (s_update_d fuel s o).
Proof.
  intros bk. induction fuel as [|md IH]; intros s o CS B H.
  { simpl. destruct (curendurance s =? 0); apply res_lost. }
  destruct o as [fd co|fd a b c|lst|lst|sos co|next|sl q co| | |fd c|fd co rounds].
  - (* Fight *)
    destruct fd as [osk opphp mds]. cbn [s_update_d]. destruct (curendurance s =? 0) eqn: Z; [apply res_lost|].
    apply Nat.eqb_neq in Z. apply res_merge. intros x I. apply in_map_iff in I. destruct I as [[te p] [E I]]. subst x. simpl.
    change (res_ok (after_fight (s_update_d md) s mds co te)).
    apply (after_fight_ok bk); auto; [lia|]. apply (fight_le s osk opphp mds). apply (in_map fst) in I. exact I.
  - (* OneRound *)
    destruct fd as [osk opphp mds]. cbn [s_update_d]. destruct (curendurance s =? 0) eqn: Z; [apply res_lost|].
    simpl in CS. apply andb_true_iff in CS. destruct CS as [CS Cc]. apply andb_true_iff in CS. destruct CS as [Ca Cb].
    apply res_merge. intros x I. apply in_map_iff in I. destruct I as [[[lw op] p] [E I]]. subst x. simpl.
    assert (lw <= curendurance s) as L.
    { apply (fight_round_le s osk opphp mds (lw, op)). apply (in_map fst) in I. exact I. }
    assert (Weak (update_endurance (fun _ => lw) (update_flags (s_set HadCombat) (unset_potions s)))) as W.
    { apply weak_endur.
      - apply weak_flags; [apply weak_unset_potions; exact H|]. intros. apply set_valid. assumption.
      - rewrite maxhp_flags. unfold unset_potions. rewrite maxhp_flags. pose proof (w_cur _ H). lia. }
    assert (book (update_endurance (fun _ => lw) (update_flags (s_set HadCombat) (unset_potions s))) = bk) as BB.
    { rewrite s_book_endur, s_book_flags. unfold unset_potions. rewrite s_book_flags. exact B. }
    destruct (NatLemma.nat_cmp (curendurance s - lw) (opphp - op)); apply IH; auto.
  - (* Randomly *)
    cbn [s_update_d]. destruct (curendurance s =? 0); [apply res_lost|].
    simpl in CS. rewrite forallb_forall in CS.
    apply res_merge. intros x I. apply in_map_iff in I. destruct I as [[y p] [E I]]. subst x. simpl.
    apply IH; auto. apply (CS (y, p) I).
  - (* Conditionally *)
    cbn [s_update_d]. destruct (curendurance s =? 0); [apply res_lost|].
    simpl in CS. rewrite forallb_forall in CS.
    destruct (filter (fun kv => check_cond s (fst kv)) lst) as [|[c0 co] tl] eqn: F; [apply res_lost|].
    assert (In (c0, co) (filter (fun kv => check_cond s (fst kv)) lst)) as I by (rewrite F; left; reflexivity).
    apply filter_In in I. destruct I as [I Ck]. simpl in Ck.
    specialize (CS _ I). simpl in CS. apply orb_true_iff in CS. destruct CS as [CS|CS].
    + apply IH; auto.
    + apply (swap_ok bk c0); auto.
  - (* Simple *)
    cbn [s_update_d]. destruct (curendurance s =? 0); [apply res_lost|].
    simpl in CS. apply andb_true_iff in CS. destruct CS as [C1 C2].
    apply IH; auto; [rewrite book_apply; exact B|apply weak_apply; auto].
  - (* Goto *)
    apply goto_ok. exact H.
  - (* LoseItemFrom *)
    cbn [s_update_d]. destruct (curendurance s =? 0); [apply res_lost|].
    simpl in CS. destruct q as [|nq]; [apply IH; auto|].
    destruct (held_items sl s) as [|x l] eqn: HE; [apply IH; auto|].
    apply res_merge. intros y Iy. apply in_map_iff in Iy. destruct Iy as [[a n] [E Iy]]. subst y. cbn [snd].
    apply IH; [exact CS|rewrite book_simple; exact B|apply weak_lose; exact H].
  - cbn [s_update_d]. destruct (curendurance s =? 0); apply res_lost.
  - cbn [s_update_d]. destruct (curendurance s =? 0) eqn: Z; [apply res_lost|]. apply Nat.eqb_neq in Z.
    apply res_single. apply weak_valid; auto. lia.
  - (* EnterFight *)
    cbn [s_update_d]. destruct (curendurance s =? 0) eqn: Z; [apply res_lost|]. apply Nat.eqb_neq in Z.
    apply res_single. apply weak_valid.
    + apply weak_chapter, weak_flags; auto. intros. apply set_valid. assumption.
    + destruct s; simpl in *. lia.
  - (* Round *)
    destruct fd as [osk opphp mds]. cbn [s_update_d]. destruct (curendurance s =? 0) eqn: Z; [apply res_lost|]. apply Nat.eqb_neq in Z.
    pose proof (one_round_le s osk opphp mds) as [L1 L2].
    apply res_add.
    + apply res_merge. intros x I. apply in_map_iff in I. destruct I as [[te p] [E I]]. subst x. simpl.
      change (res_ok (after_fight (s_update_d md) s mds co te)).
      apply (after_fight_ok bk); auto; [lia|]. apply L1. apply (in_map fst) in I. exact I.
    + intros ns I. apply in_map_iff in I. destruct I as [[k p] [E I]]. simpl in E. subst ns.
      apply in_map_iff in I. destruct I as [[[lw op] p2] [E I]]. inversion E. subst k p. simpl. clear E.
      pose proof (L2 (lw, op) (in_map fst _ _ I)) as Le. simpl in Le.
      pose proof (one_round_pos s osk opphp mds (lw, op) (in_map fst _ _ I)) as Po. simpl in Po.
      apply weak_valid.
      * apply weak_chapter, weak_endur; [apply weak_flags; auto; intros; apply set_valid; assumption|].
        rewrite maxhp_flags. pose proof (w_cur _ H). lia.
      * destruct s; simpl in *. lia.
Qed.

(* The choices are computed on a state, and played on the same state without HadCombat *)
Record Agree (s t: Stt) : Prop := {
  a_book: book s = book t;
  a_maxe: maxendurance s = maxendurance t;
  a_cur: curendurance s = curendurance t;
  a_sk: sk s = sk t;
  a_disc: disciplines s = disciplines t;
  a_items: items s = items t;
  a_prev: previtems s = previtems t;
}.

Lemma agree_maxhp: forall s t, Agree s t -> max_hp s = max_hp t.
Proof.
  intros s t H. unfold max_hp, has_item. rewrite (a_maxe _ _ H), (a_items _ _ H). reflexivity.
Qed.

Lemma agree_weak: forall s t, Agree s t -> ValidMap (flags t) -> Weak s -> Weak t.
Proof.
  intros s t A F [B S M C D N W _ I R]. constructor.
  - rewrite <- (a_book _ _ A). exact B.
  - rewrite <- (a_sk _ _ A). exact S.
  - rewrite <- (a_maxe _ _ A). exact M.
  - rewrite <- (a_cur _ _ A), <- (agree_maxhp _ _ A). exact C.
  - rewrite <- (a_disc _ _ A). exact D.
  - unfold discipline_count in *. rewrite <- (a_disc _ _ A), <- (a_book _ _ A). exact N.
  - unfold OneWeaponSpecMax in *. rewrite <- (a_disc _ _ A). exact W.
  - exact F.
  - rewrite <- (a_book _ _ A), <- (a_items _ _ A). exact I.
  - rewrite <- (a_book _ _ A), <- (a_prev _ _ A). exact R.
Qed.

Lemma agree_refl_flags: forall s f, Agree s (update_flags f s).
Proof.
  intros s f. destruct s. constructor; reflexivity.
Qed.

Lemma agree_simple: forall s t so, Agree s t -> Agree (update_simple s so) (update_simple t so).
Proof.
  intros [b m c k d ch it f p] [b2 m2 c2 k2 d2 ch2 it2 f2 p2] so [A1 A2 A3 A4 A5 A6 A7]. simpl in *. subst.
  destruct so as [x|q| | |i q|i q|sls|hn| | |fl|fl]; simpl; try (constructor; reflexivity).
  - destruct (q =? 0); [constructor; reflexivity|]. destruct i; constructor; reflexivity.
  - unfold heal, max_hp, has_item. simpl.
    destruct hn; destruct (s_check Hunting d2); try (constructor; reflexivity);
    repeat match goal with |- context [if ?b then _ else _] => destruct b end; constructor; reflexivity.
Qed.

Lemma agree_apply: forall sos s t, Agree s t -> Agree (apply_effects s sos) (apply_effects t sos).
Proof.
  induction sos as [|so q IH]; intros s t H; [exact H|]. apply IH. apply agree_simple. exact H.
Qed.

Lemma flags_simple: forall t so, ValidMap (flags t) -> ValidMap (flags (update_simple t so)).
Proof.
  intros t so V. destruct t as [b m c k d ch it f p].
  destruct so as [x|q| | |i q|i q|sls|hn| | |fl|fl]; simpl in *; auto.
  - destruct (q =? 0); [exact V|]. destruct i; exact V.
  - destruct hn; destruct (s_check Hunting d); auto;
    repeat match goal with |- context [if ?b then _ else _] => destruct b end; exact V.
  - apply set_valid. exact V.
  - apply unset_valid. exact V.
Qed.

Lemma flags_apply: forall sos t, ValidMap (flags t) -> ValidMap (flags (apply_effects t sos)).
Proof.
  induction sos as [|so q IH]; intros t V; [exact V|]. apply IH. apply flags_simple. exact V.
Qed.

Lemma valid_apply: forall sos s, forallb so_safe sos = true -> forallb keeps_alive sos = true -> ValidState s -> ValidState (apply_effects s sos).
Proof.
  induction sos as [|so q IH]; intros s S A H; [exact H|].
  simpl in S, A. apply andb_true_iff in S. apply andb_true_iff in A. destruct S as [S1 S2]. destruct A as [A1 A2].
  apply IH; auto. pose proof (valid_weak _ H) as W. apply weak_valid; [apply weak_simple; auto|].
  apply alive_simple; auto. apply (v_cur_pos _ H).
Qed.

Lemma dec_take: forall bk l nxt, dec_safe bk nxt = true ->
  dec_safe bk (fold_left (fun acc (iq: item * nat) => can_take (fst iq) (snd iq) acc) l nxt) = true.
Proof.
  intros bk l. induction l as [|x l IH]; intros nxt H; [exact H|]. simpl. apply IH. exact H.
Qed.

Lemma cartwheel_safe: forall bk g tgt, co_safe bk (cartwheel_choice g tgt) = true.
Proof.
  intros. unfold cartwheel_choice. simpl. apply forallb_forall. intros x I. apply in_map_iff in I.
  destruct I as [[v p] [E _]]. subst x. reflexivity.
Qed.

Lemma flat_take: forall i q nxt f s, i <> Gold -> has_flag Poisonned2 s && has_item Laumspur s = false ->
  flatten_d (S f) s (can_take i q nxt) =
  match q with
  | 0 => flatten_d f s nxt
  | 1 => match tcan_take i s with
        | Nope => flatten_d f s nxt
        | SpaceAvailable => map (Simple [GainItem i 1]) (flatten_d f (apply_effects s [GainItem i 1]) nxt)
        | MustDrop lst =>
            flatten_d f s nxt ++
            flat_map (fun l =>
              if Nat.ltb 1 (backpack_slots (book s) i) then map (Simple [LoseItem l 1]) (flatten_d f (apply_effects s [LoseItem l 1]) (can_take i 1 nxt))
              else if order.eqb l i then []
              else map (Simple [LoseItem l 1; GainItem i 1]) (flatten_d f (apply_effects s [LoseItem l 1; GainItem i 1]) nxt)) lst
        end
  | S p => flatten_d f s (can_take i 1 (can_take i p nxt))
  end.
Proof.
  intros i q nxt f s G P. cbn [flatten_d]. rewrite P. destruct i; try congruence; destruct q as [|[|q]]; reflexivity.
Qed.

Section Flat.
  Variable bk: nat.

  Definition FlatOk (f: nat) (s: Stt) (d: decision) : Prop :=
    forall o, In o (flatten_d f s d) -> forall t, Agree s t -> ValidMap (flags t) -> forall md, res_ok (s_update_d md t o).

  (* Playing an effect, then what comes next *)
  Lemma effect_ok: forall f s sos nxt, ValidState (apply_effects s sos) -> (FlatOk f (apply_effects s sos) nxt) ->
    forall o, In o (map (Simple sos) (flatten_d f (apply_effects s sos) nxt)) ->
    forall t, Agree s t -> ValidMap (flags t) -> forall md, res_ok (s_update_d md t o).
  Proof.
    intros f s sos nxt V IH o I t A F md. apply in_map_iff in I. destruct I as [x [E I]]. subst o.
    apply simple_step. intro m. apply (IH x I); [apply agree_apply; exact A|apply flags_apply; exact F].
  Qed.

  Lemma gain_valid: forall s i, ValidState s -> CanGain (book s) (items s) i -> ValidState (apply_effects s [GainItem i 1]).
  Proof.
    intros s i H C. apply weak_valid; [apply weak_gain; auto; apply valid_weak; exact H|].
    pose proof (v_cur_pos _ H). destruct s; simpl in *; lia.
  Qed.

  Lemma drop_valid: forall s i l, ValidState s -> l <> Backpack -> CanGain (book s) (rm_item l 1 (items s)) i ->
    ValidState (apply_effects s [LoseItem l 1; GainItem i 1]).
  Proof.
    intros s i l H N C. pose proof (valid_weak _ H) as W. apply weak_valid; [apply weak_drop_gain; auto|].
    change (0 < curendurance (update_simple (update_simple s (LoseItem l 1)) (GainItem i 1))).
    apply alive_simple; auto; [apply weak_lose; exact W|]. apply alive_simple; auto. apply (v_cur_pos _ H).
  Qed.

  Lemma buy_valid: forall s i p, ValidState s -> CanGain (book s) (items s) i -> ValidState (apply_effects s [LoseItem Gold p; GainItem i 1]).
  Proof.
    intros s i p H C. pose proof (valid_weak _ H) as W.
    assert (Weak (update_simple s (LoseItem Gold p))) as W1 by (apply weak_lose; exact W).
    apply weak_valid.
    - apply weak_gain; [exact W1|]. rewrite book_simple. apply lose_gold_gain; auto.
    - change (0 < curendurance (update_simple (update_simple s (LoseItem Gold p)) (GainItem i 1))).
      apply alive_simple; auto. apply alive_simple; auto. apply (v_cur_pos _ H).
  Qed.

  Lemma flat_ok: forall f s d, dec_safe bk d = true -> book s = bk -> ValidState s -> FlatOk f s d.
  Proof.
    induction f as [|f IH]; intros s d DS B H; [intros o I; destruct I|].
    pose proof (valid_weak _ H) as W.
    assert (forall sos, forallb so_safe sos = true -> forallb keeps_alive sos = true -> forall nxt, dec_safe bk nxt = true ->
      forall o, In o (map (Simple sos) (flatten_d f (apply_effects s sos) nxt)) ->
      forall t, Agree s t -> ValidMap (flags t) -> forall md, res_ok (s_update_d md t o)) as EFF.
    { intros sos S1 A1 nxt DN. apply effect_ok; [apply valid_apply; auto|]. apply IH; auto; [rewrite book_apply; exact B|apply valid_apply; auto]. }
    assert (forall x, co_safe bk x = true -> forall t, Agree s t -> ValidMap (flags t) -> forall md, res_ok (s_update_d md t x)) as CO.
    { intros x C t A F md. apply (co_ok bk); auto; [rewrite <- (a_book _ _ A); exact B|]. apply (agree_weak s t); auto. }
    destruct (has_flag Poisonned2 s && has_item Laumspur s) eqn: PO.
    { intros o I. cbn [flatten_d] in I. rewrite PO in I. apply (EFF [ClearFlag Poisonned2; LoseItem Laumspur 1] Logic.eq_refl Logic.eq_refl d DS o I). }
    assert (forall nxt, dec_safe bk nxt = true -> FlatOk f s nxt) as REC by (intros; apply IH; auto).
    destruct d as [a b|n|i q n|i p n|i p n|c n|sp|co|x y z w|n|sl q n]; intros o I.
    - (* App *)
      cbn [flatten_d] in I. rewrite PO in I. simpl in DS. apply andb_true_iff in DS. destruct DS as [D1 D2].
      apply in_app_or in I. destruct I as [I|I]; [apply (REC a D1 o I)|apply (REC b D2 o I)].
    - (* retrieve_equipment *)
      cbn [flatten_d] in I. rewrite PO in I. simpl in DS.
      exact (EFF [RetrieveEquipment] Logic.eq_refl Logic.eq_refl _ (dec_take bk _ n DS) o I).
    - (* can_take *)
      simpl in DS. destruct (eq_dec i Gold) as [->|G].
      { cbn [flatten_d] in I. rewrite PO in I. exact (EFF [GainItem Gold q] Logic.eq_refl Logic.eq_refl n DS o I). }
      rewrite (flat_take i q n f s G PO) in I.
      destruct q as [|[|q]].
      + apply (REC n DS o I).
      + destruct (tcan_take i s) as [|lst|] eqn: T.
        * pose proof (take_space_can s i W T) as CG.
          refine (effect_ok f s [GainItem i 1] n (gain_valid s i H CG) _ o I).
          apply IH; [exact DS|rewrite book_apply; exact B|apply gain_valid; auto].
        * apply in_app_or in I. destruct I as [I|I]; [apply (REC n DS o I)|].
          apply in_flat_map in I. destruct I as [l [Il I]].
          destruct (take_drop_can s i lst l W T Il) as [Ql [NB CG]].
          destruct (Nat.ltb 1 (backpack_slots (book s) i)) eqn: SB.
          { exact (EFF [LoseItem l 1] Logic.eq_refl Logic.eq_refl (can_take i 1 n) DS o I). }
          destruct (order.eqb l i); [destruct I|].
          assert (backpack_slots (book s) i = 1) as ONE.
          { apply Nat.ltb_ge in SB. pose proof (slots_pos (book s) i). lia. }
          specialize (CG ONE).
          refine (effect_ok f s [LoseItem l 1; GainItem i 1] n (drop_valid s i l H NB CG) _ o I).
          apply IH; [exact DS|rewrite book_apply; exact B|apply drop_valid; auto].
        * apply (REC n DS o I).
      + exact (REC (can_take i 1 (can_take i (S q) n)) DS o I).
    - (* can_buy *)
      simpl in DS. apply andb_true_iff in DS. destruct DS as [D1 D2]. apply Nat.eqb_eq in D1.
      cbn [flatten_d] in I. rewrite PO in I. apply in_app_or in I. destruct I as [I|I]; [apply (REC n D2 o I)|].
      destruct (Nat.leb p (sitems Gold s)); [|destruct I].
      destruct (tcan_take i s) as [|lst|] eqn: T.
      + pose proof (take_space_can s i W T) as CG.
        refine (effect_ok f s [LoseItem Gold p; GainItem i 1] n (buy_valid s i p H CG) _ o I).
        apply IH; [exact D2|rewrite book_apply; exact B|apply buy_valid; auto].
      + apply in_flat_map in I. destruct I as [l [Il I]].
        destruct (take_drop_can s i lst l W T Il) as [Ql [NB CG]].
        rewrite B in CG. specialize (CG D1). rewrite <- B in CG.
        assert (ValidState (apply_effects s [LoseItem l 1; LoseItem Gold p; GainItem i 1])) as VA.
        { change (ValidState (apply_effects (update_simple s (LoseItem l 1)) [LoseItem Gold p; GainItem i 1])).
          apply buy_valid; [|rewrite book_simple, items_lose by exact NB; exact CG].
          apply weak_valid; [apply weak_lose; exact W|]. apply alive_simple; auto. apply (v_cur_pos _ H). }
        refine (effect_ok f s [LoseItem l 1; LoseItem Gold p; GainItem i 1] n VA _ o I).
        apply IH; [exact D2|rewrite book_apply; exact B|exact VA].
      + destruct I.
    - (* can_sell *)
      simpl in DS. cbn [flatten_d] in I. rewrite PO in I. apply in_app_or in I. destruct I as [I|I]; [apply (REC n DS o I)|].
      destruct (Nat.ltb 0 (sitems i s)); [|destruct I].
      exact (EFF [LoseItem i 1; GainItem Gold p] Logic.eq_refl Logic.eq_refl n DS o I).
    - (* conditional *)
      simpl in DS. cbn [flatten_d] in I. rewrite PO in I. destruct (check_cond s c); [apply (REC n DS o I)|destruct I].
    - (* special *)
      destruct sp; cbn [flatten_d] in I; rewrite PO in I.
      + (* Cartwheel *)
        apply in_app_or in I. destruct I as [I|I].
        * destruct (Nat.leb cartwheel_fare (sitems Gold s)); [|destruct I]. apply in_app_or in I. destruct I as [I|I].
          -- exact (REC (none (Goto 136)) Logic.eq_refl o I).
          -- exact (REC (none (cartwheel_choice _ _)) (cartwheel_safe bk _ _) o I).
        * apply in_flat_map in I. destruct I as [tgt [_ I]]. exact (REC (none (cartwheel_choice _ tgt)) (cartwheel_safe bk _ _) o I).
      + (* Portholes *)
        exact (REC (none (Goto 197)) Logic.eq_refl o I).
      + (* B05S127 *)
        destruct I as [I|[]]. subst o. apply CO. reflexivity.
    - (* none *)
      simpl in DS. cbn [flatten_d] in I. rewrite PO in I.
      destruct (has_combat co) as [fd|].
      + destruct (fd_has NoPotion fd).
        * destruct I as [I|[]]. subst o. apply CO. exact DS.
        * destruct I as [I|I]; [subst o; apply CO; exact DS|]. apply in_app_or in I. destruct I as [I|I].
          -- destruct (has_item StrengthPotion4 s); [|destruct I]. destruct I as [I|[]]. subst o. apply CO. simpl. exact DS.
          -- destruct (has_item StrengthPotion s); [|destruct I]. destruct I as [I|[]]. subst o. apply CO. simpl. exact DS.
      + destruct I as [I|[]]. subst o. apply CO. exact DS.
    - (* evade_fight *)
      cbn [flatten_d] in I. rewrite PO in I. destruct I.
    - (* after_combat *)
      simpl in DS. cbn [flatten_d] in I. rewrite PO in I.
      destruct (Nat.eqb (max_hp s - curendurance s) 0); [apply (REC n DS o I)|].
      apply in_app_or in I. destruct I as [I|I]; [apply (REC n DS o I)|].
      apply in_flat_map in I. destruct I as [[[u i] h] [_ I]].
      exact (EFF [HealPlayer h; LoseItem i 1] Logic.eq_refl Logic.eq_refl (after_combat n) DS o I).
    - (* remove_item_from *)
      simpl in DS. cbn [flatten_d] in I. rewrite PO in I.
      destruct q as [|m]; [apply (REC n DS o I)|].
      destruct (held_items sl s) as [|x l]; [apply (REC n DS o I)|].
      apply in_flat_map in I. destruct I as [iq [_ I]].
      exact (EFF [LoseItem (fst iq) 1] Logic.eq_refl Logic.eq_refl (remove_item_from sl m n) DS o I).
  Qed.
End Flat.

(* the depths stay out of the proofs, as the kernel would otherwise try to compute with them *)
Lemma step_fuel: forall bk fl md s d co, dec_safe bk d = true -> book s = bk -> ValidState s -> In co (flatten_d fl s d) ->
  res_ok (s_update_d md (update_flags (s_unset HadCombat) s) co).
Proof.
  intros bk fl md s d co DS B H I.
  apply (flat_ok bk fl s d DS B H co I).
  - apply agree_refl_flags.
  - pose proof (w_flags _ (valid_weak _ H)) as F. destruct s; simpl in *. apply unset_valid. exact F.
Qed.

Lemma flatten_decision_eq: forall s d, flatten_decision s d = flatten_d 1000 s d.
Proof. reflexivity. Qed.

Lemma s_update_eq: forall s o, s_update s o = s_update_d 1000 s o.
Proof. reflexivity. Qed.

Lemma step_ok: forall bk s d co, dec_safe bk d = true -> book s = bk -> ValidState s -> In co (flatten_decision s d) ->
  res_ok (s_update (update_flags (s_unset HadCombat) s) co).
Proof.
  intros bk s d co DS B H I. rewrite flatten_decision_eq in I. rewrite s_update_eq. exact (step_fuel bk _ _ s d co DS B H I).
Qed.

(* Playing a book that respects what valid_check.v computes keeps the state valid *)
Theorem play_keeps_valid: forall bk chapters s, book_safe bk chapters = true -> book s = bk -> ValidState s ->
  forall o, In o (step chapters s) -> res_ok o.
Proof.
  intros bk chapters s BS B H o I. unfold step in I.
  destruct (find_chapter (chapter s) chapters) as [d|] eqn: F; [|destruct I].
  apply in_map_iff in I. destruct I as [co [E I]]. subst o.
  apply find_chapter_in in F. unfold book_safe in BS. rewrite forallb_forall in BS.
  exact (step_ok bk s d co (BS _ F) B H I).
Qed.

Theorem book1_keeps_valid: forall s, book s = 1 -> ValidState s -> forall o, In o (step books.book1 s) -> res_ok o.
Proof. intros s. apply (play_keeps_valid 1 books.book1 s book1_safe). Qed.
Theorem book2_keeps_valid: forall s, book s = 2 -> ValidState s -> forall o, In o (step books.book2 s) -> res_ok o.
Proof. intros s. apply (play_keeps_valid 2 books.book2 s book2_safe). Qed.
Theorem book3_keeps_valid: forall s, book s = 3 -> ValidState s -> forall o, In o (step books.book3 s) -> res_ok o.
Proof. intros s. apply (play_keeps_valid 3 books.book3 s book3_safe). Qed.
Theorem book4_keeps_valid: forall s, book s = 4 -> ValidState s -> forall o, In o (step books.book4 s) -> res_ok o.
Proof. intros s. apply (play_keeps_valid 4 books.book4 s book4_safe). Qed.
Theorem book5_keeps_valid: forall s, book s = 5 -> ValidState s -> forall o, In o (step books.book5 s) -> res_ok o.
Proof. intros s. apply (play_keeps_valid 5 books.book5 s book5_safe). Qed.
