(* More gold is never worse in Book 5: gold is only looked at to see if there is enough of it. The gold that was stored at
   c69 is the only thing that can differ between c69 and c14, where it comes back, so that chapters have a phase. *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import lists.
Require Import proba.
Require Import combat.
Require Import rules.
Require Import value.
Require Import fight_facts.
Require Import Stdlib.FSets.FMapPositive.
Require Import simulation.
Require Import fight_nonneg.
Require Import gold_items.
Require Import gold_relation.
Require Import gold_retrieve.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
Require Import Stdlib.PArith.PArith.
From Stdlib Require Import Lia.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.QArith.QArith.
Open Scope nat_scope.

Module PM := FMapPositive.PositiveMap.

Inductive phase : Set := Before | After.

Record GoldLe (p: phase) (s t: Stt) : Prop := {
  gl_r: GoldR 0 s t;
  gl_held: gold_of (items s) <= gold_of (items t);
  gl_phase: match p with
            | Before => True
            | After => gold_of (items s) = gold_of (items t) /\ gold_of (previtems s) <= gold_of (previtems t)
            end;
}.

Lemma rel0: forall a b, GoldRel 0 a b.
Proof. intros. right. unfold ALL. lia. Qed.

Lemma gr0_simple: forall so s t, GoldR 0 s t -> GoldR 0 (update_simple s so) (update_simple t so).
Proof.
  intros so s t H. destruct so as [dmg|v| | |i q|i q|slts|c| | |fl|fl];
    try (apply (gr_simple _ 0 0); [cbn [gold_simple_need]; lia|exact H]).
  destruct (eq_dec i Gold) as [EG|NG].
  - subst i. cbn [update_simple]. destruct (q =? 0); [exact H|]. apply gr_clamp.
    eapply gr_update_items; [apply le_n|exact H|apply same_rm, H|apply rel0].
  - apply (gr_simple _ 0 0); [cbn [gold_simple_need]; rewrite beq_gold by assumption; lia|exact H].
Qed.

Lemma gold_slot_other: forall sl m, sl <> SPouch -> ValidMap m -> lookup Gold (lose_all_slot sl m) = lookup Gold m.
Proof.
  intros sl m N V. unfold lose_all_slot.
  assert (forall i, In i (slot_items sl) -> i <> Gold) as NG by (intros; eapply not_gold_slot; eauto).
  induction (slot_items sl) as [|i q IH]; simpl; auto.
  assert (ValidMap (fold_right (fun i itms => mps.delete i itms) m q)) as VF.
  { clear -V. induction q; simpl; auto. apply Delete.valid. assumption. }
  rewrite Lookup.lookup_delete_diff; auto.
  - apply IH. intros j J. apply NG. right. exact J.
  - intro E. subst. apply (NG Gold); [left; reflexivity|reflexivity].
Qed.

Lemma gold_slot_pouch: forall m, ValidMap m -> gold_of (lose_all_slot SPouch m) = 0.
Proof.
  intros m V. unfold gold_of, lose_all_slot. simpl. rewrite Lookup.lookup_delete; auto.
Qed.

Lemma slots_valid: forall sls m, ValidMap m -> ValidMap (fold_right lose_all_slot m sls).
Proof.
  intros sls m V. assert (SameM m m) as S by (split; [exact V|split; [exact V|reflexivity]]).
  apply (same_slots sls) in S. apply S.
Qed.

Lemma gold_slots: forall sls m, ValidMap m ->
  gold_of (fold_right lose_all_slot m sls) = if existsb (fun sl => match sl with SPouch => true | _ => false end) sls then 0 else gold_of m.
Proof.
  induction sls as [|sl q IH]; intros m V; simpl; auto.
  pose proof (slots_valid q m V) as VF. destruct sl.
  - unfold gold_of. rewrite gold_slot_other by (auto; discriminate). apply IH. exact V.
  - unfold gold_of. rewrite gold_slot_other by (auto; discriminate). apply IH. exact V.
  - unfold gold_of. rewrite gold_slot_other by (auto; discriminate). apply IH. exact V.
  - apply gold_slot_pouch. exact VF.
Qed.

(* what an effect does to the gold, apart from storing it and getting it back *)
Lemma gold_effect: forall so, so <> StoreEquipment -> so <> RetrieveEquipment ->
  exists g, (forall a b, a <= b -> g a <= g b) /\
    forall s, ValidMap (items s) -> gold_of (items (update_simple s so)) = g (gold_of (items s)) /\
                                    previtems (update_simple s so) = previtems s.
Proof.
  intros so NS NR. destruct so as [dmg|v| | |i q|i q|slts|c| | |fl|fl]; try congruence;
    try (exists (fun x => x); split; [auto|intros s V; split; reflexivity]).
  - (* GainItem *)
    destruct (eq_dec i Gold) as [EG|NG].
    + subst i. destruct (Nat.eq_dec q 0) as [Q|Q].
      { subst. exists (fun x => x). split; [auto|intros s V; split; reflexivity]. }
      exists (fun a => if a + q <? 51 then a + q else 50). split.
      * intros a b L. destruct (Nat.ltb_spec (a + q) 51), (Nat.ltb_spec (b + q) 51); lia.
      * intros s V. split; [|reflexivity]. simpl. unfold gold_of at 1. rewrite add_gold_lookup; auto.
    + exists (fun x => x). split; [auto|]. intros s V. simpl. split; [|reflexivity].
      unfold gold_of. rewrite add_gold_other; auto.
  - (* LoseItem *)
    destruct (q =? 0) eqn: Q.
    { exists (fun x => x). split; [auto|]. intros s V. cbn [update_simple]. rewrite Q. split; reflexivity. }
    destruct (eq_dec i Gold) as [EG|NG].
    + subst i. exists (fun a => a - q). split; [intros; lia|]. intros s V. cbn [update_simple]. rewrite Q.
      split; [apply gold_rm; exact V|reflexivity].
    + exists (fun x => x). split; [auto|]. intros s V.
      destruct (keeps_gold_lookup (LoseItem i q) s NG V) as [L P]. split; [unfold gold_of; rewrite L; reflexivity|exact P].
  - (* LoseItemKind *)
    exists (fun a => if existsb (fun sl => match sl with SPouch => true | _ => false end) slts then 0 else a).
    split.
    + intros a b L. destruct (existsb _ _); lia.
    + intros s V. split; [|reflexivity]. apply gold_slots. exact V.
  - (* MustEat *)
    exists (fun x => x). split; [auto|]. intros s V. cbn [update_simple].
    assert (forall i, i <> Gold -> gold_of (rm_item i 1 (items s)) = gold_of (items s)) as RM.
    { intros i N. unfold gold_of, rm_item, bag_rm. rewrite Lookup.update_diff; auto. }
    destruct c, (s_check Hunting (disciplines s)); cbn iota zeta; try (split; reflexivity);
    repeat match goal with |- context [if ?b then _ else _] => destruct b end;
    try (split; reflexivity); split; try reflexivity; simpl; apply RM; discriminate.
Qed.

Definition so_next (p: phase) (so: simple_outcome) : option phase :=
  match so, p with
  | StoreEquipment, Before => Some After
  | StoreEquipment, After => None
  | RetrieveEquipment, After => Some Before
  | RetrieveEquipment, Before => None
  | _, _ => Some p
  end.

Lemma gl_simple: forall so p p2 s t, so_next p so = Some p2 -> GoldLe p s t -> GoldLe p2 (update_simple s so) (update_simple t so).
Proof.
  intros so p p2 s t N [R H P]. pose proof (gr0_simple so s t R) as R2.
  destruct (g_same _ _ _ R) as [VA [VB _]].
  assert (so = StoreEquipment \/ so = RetrieveEquipment \/ (so <> StoreEquipment /\ so <> RetrieveEquipment /\ p2 = p)) as C.
  { destruct so; auto; right; right; split; try discriminate; split; try discriminate; simpl in N; destruct p; congruence. }
  destruct C as [C|[C|[NS [NR E]]]].
  - subst so. destruct p; simpl in N; inversion N; subst. constructor; [exact R2|simpl; lia|split; [reflexivity|exact H]].
  - subst so. destruct p; simpl in N; inversion N; subst. destruct P as [_ PL]. constructor; [exact R2|exact PL|exact I].
  - subst p2. destruct (gold_effect so NS NR) as [g [M G]]. destruct (G s VA) as [G1 P1]. destruct (G t VB) as [G2 P2].
    constructor; [exact R2|rewrite G1, G2; apply M; exact H|].
    destruct p; [exact I|]. destruct P as [PE PL]. rewrite G1, G2, P1, P2, PE. split; [reflexivity|exact PL].
Qed.

Fixpoint effs_next (p: phase) (sos: list simple_outcome) : option phase :=
  match sos with
  | [] => Some p
  | so :: q => match so_next p so with Some p2 => effs_next p2 q | None => None end
  end.

Lemma gl_apply: forall sos p p2 s t, effs_next p sos = Some p2 -> GoldLe p s t -> GoldLe p2 (apply_effects s sos) (apply_effects t sos).
Proof.
  unfold apply_effects. induction sos as [|so q IH]; intros p p2 s t N H; simpl in *.
  - inversion N; subst. exact H.
  - destruct (so_next p so) as [p1|] eqn: E; [|discriminate]. eapply IH; [exact N|]. eapply gl_simple; eauto.
Qed.

Lemma gl_endur: forall p s t f g, f (curendurance s) = g (curendurance t) -> GoldLe p s t ->
  GoldLe p (update_endurance f s) (update_endurance g t).
Proof.
  intros p s t f g E [R H P]. constructor; [apply gr_endur; auto|exact H|exact P].
Qed.

Lemma gl_flags: forall p s t f, GoldLe p s t -> GoldLe p (update_flags f s) (update_flags f t).
Proof.
  intros p s t f [R H P]. constructor; [apply gr_flags; auto|exact H|exact P].
Qed.

Lemma gl_goto: forall p s t c, GoldLe p s t -> GoldLe p (update_chapter c s) (update_chapter c t).
Proof.
  intros p s t c [R H P]. constructor; [|exact H|exact P]. destruct R; constructor; simpl; auto.
Qed.

Lemma gl_heal: forall p s t q, GoldLe p s t -> GoldLe p (heal s q) (heal t q).
Proof.
  intros p s t q [R H P]. constructor; [apply gr_heal; auto|exact H|exact P].
Qed.

Definition not_pouch (sl: slot) : bool := match sl with SPouch => false | _ => true end.

Definition qc_nn (q: Qc) : bool := Qle_bool 0 (this q).

Lemma qc_nn_le: forall q, qc_nn q = true -> (0 <= q)%Qc.
Proof. intros q N. apply Qle_bool_imp_le. exact N. Qed.

Lemma q2qc_nn: forall q, (0 <= q)%Q -> (0 <= Q2Qc q)%Qc.
Proof.
  intros q N. unfold Qcle. simpl. rewrite Qred_correct. exact N.
Qed.

Section Book.
  (* the chapters that can be in each phase *)
  Variable tbl: phase -> cid -> bool.

  Fixpoint co_ok (p: phase) (o: chapter_outcome) : bool :=
    match o with
    | Fight fd n => no_poison (fd_mods fd) && forallb (tbl p) (mods_dests (fd_mods fd)) && co_ok p n
    | OneRound _ a b c => co_ok p a && co_ok p b && co_ok p c
    | Randomly l => forallb (fun x => qc_nn (snd x) && co_ok p (fst x)) l
    | Conditionally l => forallb (fun x => (gold_cond_need (fst x) =? 0) && co_ok p (snd x)) l
    | Simple l n => match effs_next p l with Some p2 => co_ok p2 n | None => false end
    | Goto c => tbl p c
    | LoseItemFrom sl _ n => not_pouch sl && co_ok p n
    | GameLost | GameWon => true
    | EnterFight _ c => tbl p c
    | Round fd n l => forallb (tbl p) (mods_dests (fd_mods fd)) && co_ok p n && tbl p 0 && forallb (tbl p) l
    end.

  Variable f: next_step -> Qc.
  Hypothesis fN: forall p x y, tbl p (chapter x) = true -> GoldLe p x y -> (f (New x) <= f (New y))%Qc.
  Hypothesis fW: forall p x y, GoldLe p x y -> (f (Won x) <= f (Won y))%Qc.

  Lemma le_refl_ev: forall d, (ev f d <= ev f d)%Qc.
  Proof. intro. apply Qcle_refl. Qed.

  Lemma after_fight_le: forall rec1 rec2 p s t mds co te,
    GoldLe p s t -> te_dest_in mds te -> forallb (tbl p) (mods_dests mds) = true -> co_ok p co = true ->
    (forall x y o, co_ok p o = true -> GoldLe p x y -> (ev f (rec1 x o) <= ev f (rec2 y o))%Qc) ->
    (ev f (after_fight rec1 s mds co te) <= ev f (after_fight rec2 t mds co te))%Qc.
  Proof.
    intros rec1 rec2 p s t mds co te HR HI HL HC REC.
    assert (forall c, In c (mods_dests mds) -> co_ok p (Goto c) = true) as HG.
    { intros c I. simpl. rewrite forallb_forall in HL. apply HL. exact I. }
    assert (exists nco nhp, co_ok p nco = true /\
      (after_fight rec1 s mds co te, after_fight rec2 t mds co te) =
      ((let stt1 := update_flags (s_set HadCombat) s in
        let stt2 := if has_raw_mod MultiFight mds then stt1 else unset_potions stt1 in
        match Modifiers.extractl Modifiers.gFakeFight mds with
        | Some cid2 => if nhp =? 0 then single_outcome (New (update_chapter cid2 stt2)) else rec1 stt2 nco
        | None => if nhp =? 0 then single_outcome Lost else rec1 (update_endurance (fun _ => nhp) stt2) nco
        end),
       (let stt1 := update_flags (s_set HadCombat) t in
        let stt2 := if has_raw_mod MultiFight mds then stt1 else unset_potions stt1 in
        match Modifiers.extractl Modifiers.gFakeFight mds with
        | Some cid2 => if nhp =? 0 then single_outcome (New (update_chapter cid2 stt2)) else rec2 stt2 nco
        | None => if nhp =? 0 then single_outcome Lost else rec2 (update_endurance (fun _ => nhp) stt2) nco
        end))) as [nco [nhp [NC E]]].
    { destruct te; simpl in HI; do 2 eexists; (split; [first [apply HG; exact HI|exact HC|reflexivity]|reflexivity]). }
    injection E as E1 E2. rewrite E1, E2. cbv zeta.
    assert (GoldLe p (if has_raw_mod MultiFight mds then update_flags (s_set HadCombat) s else unset_potions (update_flags (s_set HadCombat) s))
                     (if has_raw_mod MultiFight mds then update_flags (s_set HadCombat) t else unset_potions (update_flags (s_set HadCombat) t))) as R2.
    { destruct (has_raw_mod MultiFight mds); unfold unset_potions; repeat apply gl_flags; exact HR. }
    destruct (Modifiers.extractl Modifiers.gFakeFight mds) as [c|] eqn: EX.
    - destruct (nhp =? 0).
      + unfold single_outcome. rewrite !ev_single. eapply fN; [simpl|apply gl_goto; exact R2].
        specialize (HG c). simpl in HG. apply HG. eapply extractl_dest; eauto. mod_cases.
      + eapply REC; eauto.
    - destruct (nhp =? 0); [apply Qcle_refl|].
      eapply REC; [exact NC|]. apply gl_endur; [reflexivity|exact R2].
  Qed.

  Lemma ev_le: forall fuel p s t o, co_ok p o = true -> GoldLe p s t ->
    (ev f (s_update_d fuel s o) <= ev f (s_update_d fuel t o))%Qc.
  Proof.
    induction fuel as [|md IH]; intros p s t o HN HR.
    { simpl. rewrite (g_cur _ _ _ (gl_r _ _ _ HR)). destruct (curendurance t =? 0); apply Qcle_refl. }
    pose proof (gl_r _ _ _ HR) as GR. pose proof (gr_fight _ _ _ GR) as SF.
    cbn [s_update_d]. rewrite (g_cur _ _ _ GR). destruct (curendurance t =? 0) eqn: C0; [apply Qcle_refl|].
    destruct o as [fd co|fd a b c|lst|lst|sos co|next|sl q co| | |fd c|fd co rounds].
    - (* Fight *)
      destruct fd as [osk opphp mds]. simpl in HN. apply andb_prop in HN. destruct HN as [HN HC]. apply andb_prop in HN. destruct HN as [NP HD].
      change (ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) s mds co te)) (fight s osk opphp mds)))
            <= ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) t mds co te)) (fight t osk opphp mds))))%Qc.
      rewrite <- (fight_same _ _ osk opphp mds SF).
      apply ev_merge_le. intros [te q] I. simpl. split; [reflexivity|]. split.
      + exact (fight_nn s osk opphp mds NP _ I).
      + apply (after_fight_le _ _ p s t mds co te HR); auto.
        * apply (fight_dests s osk opphp). apply (in_map fst) in I. exact I.
        * intros x y o X Y. eapply IH; eauto.
    - (* OneRound *)
      destruct fd as [osk opphp mds]. simpl in HN. apply andb_prop in HN. destruct HN as [HN HC]. apply andb_prop in HN. destruct HN as [HA HB].
      rewrite <- (fight_round_same _ _ SF osk opphp mds).
      apply ev_merge_le. intros [[lw op] q] I. simpl. split; [reflexivity|]. split.
      + exact (fight_round_nn s osk opphp mds _ I).
      + destruct (NatLemma.nat_cmp _ _); eapply IH; eauto;
          (apply gl_endur; [reflexivity|apply gl_flags; unfold unset_potions; apply gl_flags; exact HR]).
    - (* Randomly *)
      cbn [co_ok] in HN. rewrite forallb_forall in HN.
      apply ev_merge_le. intros [co q] I. simpl. destruct (andb_prop _ _ (HN _ I)) as [NQ NC]. simpl in NQ, NC.
      split; [reflexivity|]. split; [apply qc_nn_le; exact NQ|]. eapply IH; eauto.
    - (* Conditionally *)
      cbn [co_ok] in HN. rewrite forallb_forall in HN.
      assert (filter (fun kv => check_cond s (fst kv)) lst = filter (fun kv => check_cond t (fst kv)) lst) as FE.
      { apply filter_ext_in. intros x I. destruct (andb_prop _ _ (HN _ I)) as [NQ _]. apply Nat.eqb_eq in NQ.
        apply (gr_cond _ 0); [lia|exact GR]. }
      assert (forall c co q, filter (fun kv => check_cond t (fst kv)) lst = (c, co) :: q -> In (c, co) lst) as IL.
      { intros c co q E. assert (In (c, co) (filter (fun kv => check_cond t (fst kv)) lst)) as J by (rewrite E; left; reflexivity).
        apply filter_In in J. tauto. }
      destruct (filter (fun kv => check_cond s (fst kv)) lst) as [|[c co] q];
      destruct (filter (fun kv => check_cond t (fst kv)) lst) as [|[c2 co2] q2] eqn: F2;
        try discriminate; [apply Qcle_refl|].
      inversion FE; subst.
      destruct (andb_prop _ _ (HN _ (IL _ _ _ Logic.eq_refl))) as [_ NC]. simpl in NC. eapply IH; eauto.
    - (* Simple *)
      cbn [co_ok] in HN. destruct (effs_next p sos) as [p2|] eqn: E; [|discriminate].
      eapply IH; [exact HN|]. eapply gl_apply; eauto.
    - (* Goto *)
      cbn [co_ok] in HN.
      assert (GoldLe p (update_chapter next s) (update_chapter next t)) as RG by (apply gl_goto; exact HR).
      unfold has_flag. rewrite (g_book _ _ _ GR), (g_chap _ _ _ GR), (g_flags _ _ _ GR), (g_disc _ _ _ GR). fold (has_flag Poisonned2 t). fold (has_flag HadCombat t).
      destruct (_ && has_flag Poisonned2 t)%bool.
      + simpl. rewrite (g_cur _ _ _ GR). destruct (curendurance t - 2 =? 0).
        * apply Qcle_refl.
        * unfold single_outcome. rewrite !ev_single. apply (fN p); [exact HN|].
          apply gl_endur; [simpl; rewrite (g_cur _ _ _ GR); reflexivity|exact RG].
      + destruct (_ && _ && _)%bool; unfold single_outcome; rewrite !ev_single; apply (fN p); try exact HN.
        * apply gl_heal. exact RG.
        * exact RG.
    - (* LoseItemFrom *)
      cbn [co_ok] in HN. apply andb_prop in HN. destruct HN as [NP HC].
      assert (sl <> SPouch) as NS by (intro E; subst; discriminate).
      assert (held_items sl s = held_items sl t) as HH by (apply (gr_held_other _ 0); auto).
      rewrite HH. destruct q as [|nq].
      + eapply IH; eauto.
      + destruct (held_items sl t) as [|x tl] eqn: E.
        * eapply IH; eauto.
        * apply ev_merge_le. intros [i amnt] I. cbn [fst snd]. split; [reflexivity|]. split.
          { apply q2qc_nn. apply Qmult_le_0_compat.
            - unfold Qle. simpl. lia.
            - apply Qinv_le_0_compat. unfold Qle. simpl. lia. }
          eapply IH; [|eapply gl_simple; [|exact HR]].
          -- cbn [co_ok]. rewrite NP, HC. reflexivity.
          -- reflexivity.
    - (* GameLost *)
      apply Qcle_refl.
    - (* GameWon *)
      unfold single_outcome. rewrite !ev_single. eapply fW. exact HR.
    - (* EnterFight *)
      cbn [co_ok] in HN. unfold single_outcome. rewrite !ev_single. apply (fN p); [exact HN|].
      apply gl_goto, gl_flags. exact HR.
    - (* Round *)
      destruct fd as [osk opphp mds]. cbn [co_ok fd_mods] in HN.
      apply andb_prop in HN. destruct HN as [HN HL]. apply andb_prop in HN. destruct HN as [HN H0]. apply andb_prop in HN. destruct HN as [HD HC].
      rewrite <- (one_round_same _ _ SF osk opphp mds).
      rewrite !ev_add_proba. apply Qcplus_le_compat.
      + change (ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) s mds co te)) (fst (one_round s osk opphp mds))))
              <= ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) t mds co te)) (fst (one_round s osk opphp mds)))))%Qc.
        apply ev_merge_le. intros [te q] I. simpl. split; [reflexivity|]. split.
        * exact (proj1 (one_round_nn s osk opphp mds) _ I).
        * apply (after_fight_le _ _ p s t mds co te HR); auto.
          -- apply (one_round_ok s osk opphp). apply (in_map fst) in I. exact I.
          -- intros x y o X Y. eapply IH; eauto.
      + apply ev_map_le. intros [[lw op] q] I. simpl. split; [reflexivity|]. split.
        * exact (proj2 (one_round_nn s osk opphp mds) _ I).
        * apply (fN p).
          -- simpl. destruct (nth_in_or_default op rounds 0) as [J|J].
             ++ rewrite forallb_forall in HL. apply HL. exact J.
             ++ rewrite J. exact H0.
          -- apply gl_goto, gl_endur; [reflexivity|]. apply gl_flags. exact HR.
  Qed.

  Fixpoint cond_pos (c: bool_cond) : bool :=
    match c with
    | Not x => gold_cond_need x =? 0
    | COr a b | CAnd a b => cond_pos a && cond_pos b
    | _ => true
    end.

  Lemma cond_le: forall c p s t, cond_pos c = true -> GoldLe p s t -> check_cond s c = true -> check_cond t c = true.
  Proof.
    induction c as [d|x IH|a IHa b IHb|a IHa b IHb|i q|b|n|fl|lv]; intros p s t N H C; pose proof (gl_r _ _ _ H) as GR;
      cbn [cond_pos check_cond] in *.
    - rewrite <- (g_disc _ _ _ GR). exact C.
    - apply Nat.eqb_eq in N. rewrite <- (gr_cond x 0 s t) by (auto; lia). exact C.
    - apply andb_prop in N. destruct N as [Na Nb]. apply orb_prop in C. apply orb_true_intro.
      destruct C as [C|C]; [left; eapply IHa|right; eapply IHb]; eauto.
    - apply andb_prop in N. destruct N as [Na Nb]. apply andb_prop in C. apply andb_true_intro.
      destruct C as [Ca Cb]. split; [eapply IHa|eapply IHb]; eauto.
    - destruct (eq_dec i Gold) as [EG|NG].
      + subst. apply Nat.leb_le in C. apply Nat.leb_le. pose proof (gl_held _ _ _ H) as L. unfold sitems, gold_of in *. lia.
      + rewrite <- (sitems_same 0 s t i NG GR). exact C.
    - exact C.
    - rewrite <- (g_cur _ _ _ GR). exact C.
    - unfold has_flag in *. rewrite <- (g_flags _ _ _ GR). exact C.
    - unfold level, discipline_count in *. rewrite <- (g_disc _ _ _ GR). exact C.
  Qed.

  Fixpoint dec_ok (p: phase) (d: decision) : bool :=
    match d with
    | App a b => dec_ok p a && dec_ok p b
    | retrieve_equipment n => match p with After => dec_ok Before n | Before => false end
    | can_take _ _ n => dec_ok p n
    | can_buy _ _ n => dec_ok p n
    | can_sell _ _ _ => false
    | conditional c n => cond_pos c && dec_ok p n
    | special B05S127 => tbl p 159 && tbl p 93
    | special _ => false
    | none o => co_ok p o
    | evade_fight _ _ _ _ => true
    | after_combat n => dec_ok p n
    | remove_item_from sl _ n => not_pouch sl && dec_ok p n
    end.

  Lemma borne_ok: forall x, qc_nn (Q2Qc ((inject_Z (Z.of_nat (Nat.min x 10)) / inject_Z 10)%Q)) = true.
  Proof.
    intro x. unfold qc_nn. apply Qle_bool_iff. apply q2qc_nn. apply Qmult_le_0_compat.
    - unfold Qle. simpl. lia.
    - vm_compute. discriminate.
  Qed.

  Definition Covers (p: phase) (a b: list chapter_outcome) : Prop :=
    forall o, In o a -> In o b /\ co_ok p o = true.

  Lemma covers_app: forall p a b c d, Covers p a c -> Covers p b d -> Covers p (a ++ b) (c ++ d).
  Proof.
    intros p a b c d A B o I. apply in_app_iff in I. destruct I as [I|I].
    - destruct (A o I) as [X Y]. split; [apply in_app_iff; left; exact X|exact Y].
    - destruct (B o I) as [X Y]. split; [apply in_app_iff; right; exact X|exact Y].
  Qed.

  Lemma covers_nil: forall p b, Covers p [] b.
  Proof. intros p b o []. Qed.

  Lemma covers_flat {A: Type}: forall p (l: list A) g h, (forall a, In a l -> Covers p (g a) (h a)) -> Covers p (flat_map g l) (flat_map h l).
  Proof.
    induction l as [|x l IH]; intros g h C; simpl; [apply covers_nil|].
    apply covers_app; [apply C; left; reflexivity|]. apply IH. intros a I. apply C. right. exact I.
  Qed.

  Lemma take_ok: forall p l nxt, dec_ok p (fold_left (fun acc (iq: item * nat) => can_take (fst iq) (snd iq) acc) l nxt) = dec_ok p nxt.
  Proof.
    induction l as [|x l IH]; intro nxt; simpl; auto. rewrite IH. reflexivity.
  Qed.

  Lemma flatten_covers: forall fl p s t d, dec_ok p d = true -> GoldLe p s t -> Covers p (flatten_d fl s d) (flatten_d fl t d).
  Proof.
    induction fl as [|fl IH]; intros p s t d HN HR; [apply covers_nil|].
    pose proof (gl_r _ _ _ HR) as GR.
    assert (forall sos nxt p2, effs_next p sos = Some p2 -> dec_ok p2 nxt = true ->
      Covers p (map (Simple sos) (flatten_d fl (apply_effects s sos) nxt)) (map (Simple sos) (flatten_d fl (apply_effects t sos) nxt))) as WE.
    { intros sos nxt p2 E D o I. apply in_map_iff in I. destruct I as [x [X J]]. subst o.
      destruct (IH p2 _ _ nxt D (gl_apply _ _ _ _ _ E HR) x J) as [A B]. split; [apply in_map; exact A|].
      simpl. rewrite E. exact B. }
    assert (has_flag Poisonned2 s = has_flag Poisonned2 t) as EP by (unfold has_flag; rewrite (g_flags _ _ _ GR); reflexivity).
    cbn [flatten_d]. rewrite EP, (has_same 0 s t Laumspur) by (discriminate || exact GR).
    destruct (has_flag Poisonned2 t && has_item Laumspur t)%bool.
    { eapply WE; [reflexivity|exact HN]. }
    destruct d as [d1 d2|nxt|i q nxt|i pr nxt|i pr nxt|c nxt|sp|o|a b c' o|nxt|sl k nxt]; cbn [dec_ok] in HN.
    - (* App *)
      apply andb_prop in HN. destruct HN as [H1 H2]. apply covers_app; apply IH; auto.
    - (* retrieve_equipment *)
      destruct p; [discriminate|]. destruct (gl_phase _ _ _ HR) as [HE _].
      assert (sitems Gold s = sitems Gold t) as SG by exact HE.
      rewrite (all_held_same 0 s t SG GR). eapply WE; [reflexivity|]. rewrite take_ok. exact HN.
    - (* can_take *)
      destruct (eq_dec i Gold) as [EG|NG].
      { subst i. eapply WE; [reflexivity|exact HN]. }
      destruct i; try congruence;
      (destruct q as [|[|q]];
      [ apply IH; auto
      | rewrite (gr_take _ 0 s t GR), (g_book _ _ _ GR);
        match goal with |- context [tcan_take ?it t] => destruct (tcan_take it t) as [|lst|] eqn: TC end;
        [ eapply WE; [reflexivity|exact HN]
        | apply covers_app; [apply IH; auto|]; apply covers_flat; intros l Il;
          (destruct (_ <? backpack_slots _ _)%nat;
            [ eapply WE; [reflexivity|exact HN]
            | destruct (order.eqb l _);
              [ apply covers_nil
              | eapply WE; [reflexivity|exact HN]]])
        | apply IH; auto ]
      | apply IH; [exact HN|exact HR] ]).
    - (* can_buy *)
      rewrite (gr_take i 0 s t GR). apply covers_app; [apply IH; auto|].
      destruct (pr <=? sitems Gold s)%nat eqn: LS; [|apply covers_nil].
      assert ((pr <=? sitems Gold t)%nat = true) as LT.
      { apply Nat.leb_le in LS. apply Nat.leb_le. pose proof (gl_held _ _ _ HR) as L. unfold sitems, gold_of in *. lia. }
      rewrite LT. destruct (tcan_take i t) as [|lst|].
      + eapply WE; [reflexivity|exact HN].
      + apply covers_flat. intros l _. eapply WE; [reflexivity|exact HN].
      + apply covers_nil.
    - (* can_sell *)
      discriminate.
    - (* conditional *)
      apply andb_prop in HN. destruct HN as [HC H2].
      destruct (check_cond s c) eqn: CS; [|apply covers_nil].
      rewrite (cond_le c p s t HC HR CS). apply IH; auto.
    - (* special *)
      destruct sp; try discriminate. cbn [dec_ok] in HN. apply andb_prop in HN. destruct HN as [H1 H2].
      rewrite (g_sk _ _ _ GR). intros x [E|[]]. subst x. split; [left; reflexivity|].
      cbn [co_ok forallb fst snd effs_next so_next andb]. rewrite H1, H2, !borne_ok. reflexivity.
    - (* none *)
      destruct (has_combat o) as [fd|]; [|intros x [E|[]]; subst x; split; [left; reflexivity|exact HN]].
      rewrite (has_same 0 s t StrengthPotion4), (has_same 0 s t StrengthPotion) by (discriminate || exact GR).
      destruct (fd_has NoPotion fd); [intros x [E|[]]; subst x; split; [left; reflexivity|exact HN]|].
      intros x I. split; [exact I|].
      destruct I as [E|I]; [subst x; exact HN|]. apply in_app_iff in I.
      destruct I as [I|I]; [destruct (has_item StrengthPotion4 t)|destruct (has_item StrengthPotion t)];
        try destruct I as [E|[]]; try destruct I; subst x; exact HN.
    - (* evade_fight *)
      apply covers_nil.
    - (* after_combat *)
      rewrite (gr_maxhp _ _ _ GR), (g_cur _ _ _ GR), (g_book _ _ _ GR).
      destruct ((max_hp t - curendurance t =? 0)%nat); [apply IH; auto|].
      assert (forall x, In x (potion_list (book t)) -> In (snd (fst x)) potions) as PL.
      { intros x J. simpl in J. repeat (destruct J as [J|J]; [subst; simpl; tauto|]). destruct J. }
      assert (filter (fun ih : bool * item * nat => let '(u, i, _) := ih in u && has_item i s) (potion_list (book t))
            = filter (fun ih : bool * item * nat => let '(u, i, _) := ih in u && has_item i t) (potion_list (book t))) as PF.
      { apply filter_ext_in. intros [[u i] h] J. specialize (PL _ J). simpl in PL.
        rewrite (R_potion gold_tracker 0 s t i PL GR). reflexivity. }
      unfold potion_list in PF. rewrite PF.
      apply covers_app; [apply IH; auto|]. apply covers_flat.
      intros [[u i] h] _. cbn beta iota. eapply WE; [reflexivity|exact HN].
    - (* remove_item_from *)
      apply andb_prop in HN. destruct HN as [NP H2].
      assert (sl <> SPouch) as NS by (intro E; subst; discriminate).
      assert (held_items sl s = held_items sl t) as HH by (apply (gr_held_other _ 0); auto).
      rewrite HH. destruct k as [|m]; [apply IH; auto|].
      destruct (held_items sl t) as [|x tl]; [apply IH; auto|].
      apply covers_flat. intros iq _. eapply WE; [reflexivity|]. cbn [dec_ok]. rewrite NP, H2. reflexivity.
  Qed.
End Book.

Lemma best_nn: forall l, (0 <= best l)%Qc.
Proof.
  induction l as [|a l IH]; simpl; [apply Qcle_refl|]. unfold qmax.
  destruct (Qclt_le_dec a (best l)); [exact IH|]. eapply Qcle_trans; [exact IH|assumption].
Qed.

Lemma best_ge: forall y l, In y l -> (y <= best l)%Qc.
Proof.
  intros y l. induction l as [|a l IH]; intro I; [destruct I|]. simpl. unfold qmax.
  destruct I as [E|I].
  - subst. destruct (Qclt_le_dec y (best l)); [apply Qclt_le_weak; assumption|apply Qcle_refl].
  - destruct (Qclt_le_dec a (best l)); [apply IH; exact I|]. eapply Qcle_trans; [apply IH; exact I|assumption].
Qed.

Lemma best_le: forall l1 l2, (forall x, In x l1 -> exists y, In y l2 /\ (x <= y)%Qc) -> (best l1 <= best l2)%Qc.
Proof.
  induction l1 as [|a l IH]; intros l2 A; simpl; [apply best_nn|]. unfold qmax.
  destruct (Qclt_le_dec a (best l)).
  - apply IH. intros x I. apply A. right. exact I.
  - destruct (A a) as [y [Y L]]; [left; reflexivity|]. eapply Qcle_trans; [exact L|apply best_ge; exact Y].
Qed.

Definition chapters_ok (tbl: phase -> cid -> bool) (chapters: list (cid * decision)) : bool :=
  forallb (fun cd : cid * decision =>
    (negb (tbl Before (fst cd)) || dec_ok tbl Before (snd cd)) && (negb (tbl After (fst cd)) || dec_ok tbl After (snd cd))) chapters.

(* A state with more gold is worth at least as much, if what follows is, from a chapter of the right phase *)
Theorem value_more_gold: forall tbl chapters score,
  chapters_ok tbl chapters = true ->
  (forall p x y, GoldLe p x y -> (score x <= score y)%Qc) ->
  forall fuel p s t, tbl p (chapter s) = true -> GoldLe p s t -> (value chapters score fuel s <= value chapters score fuel t)%Qc.
Proof.
  intros tbl chapters score OK HS. induction fuel as [|fuel IH]; intros p s t TB HR; [apply Qcle_refl|].
  pose proof (gl_r _ _ _ HR) as GR.
  cbn [value]. unfold step. rewrite <- (g_chap _ _ _ GR).
  destruct (find_chapter (chapter s) chapters) as [d|] eqn: F; [|apply Qcle_refl].
  assert (dec_ok tbl p d = true) as DK.
  { unfold chapters_ok in OK. rewrite forallb_forall in OK.
    destruct (andb_prop _ _ (OK _ (find_chapter_in _ _ _ F))) as [A B]. simpl in A, B.
    destruct p; rewrite TB in *; simpl in *; assumption. }
  set (g := fun ns => match ns with Lost => 0%Qc | Won x => score x | New x => value chapters score fuel x end).
  unfold flatten_decision, s_update. apply best_le. intros x I. rewrite map_map in I. apply in_map_iff in I. destruct I as [o [E I]]. subst x.
  destruct (flatten_covers tbl 1000 p s t d DK HR o I) as [J C].
  exists (ev g (s_update_d 1000 (update_flags (s_unset HadCombat) t) o)). split.
  - rewrite map_map. apply in_map_iff. exists o. split; [reflexivity|exact J].
  - apply (ev_le tbl g (fun p2 a b T2 R2 => IH p2 a b T2 R2) (fun p2 a b R2 => HS p2 a b R2) 1000 p _ _ o C).
    apply gl_flags. exact HR.
Qed.

(* What the chapters of a book lead to, to compute which chapters are in each phase *)
Fixpoint co_dests (p: phase) (o: chapter_outcome) : list (phase * cid) :=
  match o with
  | Fight fd n => map (pair p) (mods_dests (fd_mods fd)) ++ co_dests p n
  | OneRound _ a b c => co_dests p a ++ co_dests p b ++ co_dests p c
  | Randomly l => flat_map (fun x => co_dests p (fst x)) l
  | Conditionally l => flat_map (fun x => co_dests p (snd x)) l
  | Simple l n => match effs_next p l with Some p2 => co_dests p2 n | None => [] end
  | Goto c => [(p, c)]
  | LoseItemFrom _ _ n => co_dests p n
  | GameLost | GameWon => []
  | EnterFight _ c => [(p, c)]
  | Round fd n l => map (pair p) (mods_dests (fd_mods fd)) ++ co_dests p n ++ (p, 0%nat) :: map (pair p) l
  end.

Fixpoint dec_dests (p: phase) (d: decision) : list (phase * cid) :=
  match d with
  | App a b => dec_dests p a ++ dec_dests p b
  | retrieve_equipment n => dec_dests Before n
  | can_take _ _ n | can_buy _ _ n | can_sell _ _ n | conditional _ n | after_combat n | remove_item_from _ _ n => dec_dests p n
  | special _ => [(p, 159%nat); (p, 93%nat)]
  | none o => co_dests p o
  | evade_fight _ _ _ _ => []
  end.

Definition chapter_set := (PM.t bool * PM.t bool)%type.

Definition in_set (t: chapter_set) (p: phase) (c: cid) : bool :=
  match PM.find (Pos.of_succ_nat c) (match p with Before => fst t | After => snd t end) with Some b => b | None => false end.

Definition add_set (t: chapter_set) (x: phase * cid) : chapter_set :=
  match fst x with
  | Before => (PM.add (Pos.of_succ_nat (snd x)) true (fst t), snd t)
  | After => (fst t, PM.add (Pos.of_succ_nat (snd x)) true (snd t))
  end.

Definition grow (chapters: list (cid * decision)) (t: chapter_set) : chapter_set :=
  fold_left (fun acc (cd: cid * decision) =>
    let acc1 := if in_set acc Before (fst cd) then fold_left add_set (dec_dests Before (snd cd)) acc else acc in
    if in_set acc1 After (fst cd) then fold_left add_set (dec_dests After (snd cd)) acc1 else acc1) chapters t.

Fixpoint grow_n (n: nat) (chapters: list (cid * decision)) (t: chapter_set) : chapter_set :=
  match n with O => t | S k => grow_n k chapters (grow chapters t) end.

Definition set_of (before after: list cid) : chapter_set :=
  (fold_left (fun t c => PM.add (Pos.of_succ_nat c) true t) before (PM.empty bool),
   fold_left (fun t c => PM.add (Pos.of_succ_nat c) true t) after (PM.empty bool)).
