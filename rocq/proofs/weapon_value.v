(* The values of two states that only differ by the exchange of two plain weapons, see weapon_swap.v *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import order.
Require Import lists.
Require Import proba.
Require Import combat.
Require Import rules.
Require Import value.
Require Import fight_facts.
Require Import simulation.
Require Import weapon_analysis.
Require Import weapon_swap.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Sorting.Permutation.
Require Import Stdlib.Bool.Bool.
Require Import Stdlib.Arith.Arith.
From Stdlib Require Import Lia.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.QArith.QArith.
Require Import Field.

Open Scope nat_scope.

(* Expected values with two functions, over lists that are the same up to the order *)
Section Ev2.
  Open Scope Qc_scope.

  Definition qsum {A: Type} (h: A -> Qc) (l: list A) : Qc := fold_right (fun x acc => h x + acc) 0 l.

  Lemma qsum_perm {A: Type}: forall (h: A -> Qc) l1 l2, Permutation l1 l2 -> qsum h l1 = qsum h l2.
  Proof.
    intros h l1 l2 P. induction P; unfold qsum in *; simpl.
    - reflexivity.
    - rewrite IHP. reflexivity.
    - ring.
    - rewrite IHP1, IHP2. reflexivity.
  Qed.

  Lemma qsum_map {A B: Type}: forall (h: B -> Qc) (g: A -> B) l, qsum h (map g l) = qsum (fun x => h (g x)) l.
  Proof. intros. induction l; unfold qsum in *; simpl; [reflexivity|]. rewrite IHl. reflexivity. Qed.

  Lemma qsum_ext {A: Type}: forall (h k: A -> Qc) l, (forall x, In x l -> h x = k x) -> qsum h l = qsum k l.
  Proof.
    intros h k l E. induction l as [|x l IH]; unfold qsum in *; simpl; [reflexivity|].
    rewrite E by (left; reflexivity). rewrite IH; [reflexivity|]. intros. apply E. right. assumption.
  Qed.

  Lemma ev_merge_qsum {A K: Set} `{OrdDec K}: forall (f: K -> Qc) (l: list A) (G: A -> Qc * Proba K),
    ev f (merge_probas (map G l)) = qsum (fun x => fst (G x) * ev f (snd (G x))) l.
  Proof.
    intros. rewrite ev_merge. unfold qsum. induction l; simpl; [reflexivity|]. rewrite IHl. reflexivity.
  Qed.

  Lemma ev2_merge_perm {A B K: Set} `{OrdDec K}: forall (f1 f2: K -> Qc) (l1: list A) (l2: list B) (h: A -> B)
      (G: A -> Qc * Proba K) (H: B -> Qc * Proba K),
    Permutation l2 (map h l1) ->
    (forall x, In x l1 -> fst (G x) = fst (H (h x)) /\ ev f1 (snd (G x)) = ev f2 (snd (H (h x)))) ->
    ev f1 (merge_probas (map G l1)) = ev f2 (merge_probas (map H l2)).
  Proof.
    intros f1 f2 l1 l2 h G H' P E. rewrite !ev_merge_qsum. rewrite (qsum_perm _ _ _ P), qsum_map.
    apply qsum_ext. intros x I. destruct (E x I) as [E1 E2]. rewrite E1, E2. reflexivity.
  Qed.

  Lemma ev2_merge {A K: Set} `{OrdDec K}: forall (f1 f2: K -> Qc) (l: list A) (G H: A -> Qc * Proba K),
    (forall x, In x l -> fst (G x) = fst (H x) /\ ev f1 (snd (G x)) = ev f2 (snd (H x))) ->
    ev f1 (merge_probas (map G l)) = ev f2 (merge_probas (map H l)).
  Proof.
    intros. apply (ev2_merge_perm f1 f2 l l (fun x => x)); [rewrite map_id; reflexivity|assumption].
  Qed.

  Lemma ev2_map {A K: Set}: forall (f1 f2: K -> Qc) (l: list A) (G H: A -> K * Qc),
    (forall x, In x l -> snd (G x) = snd (H x) /\ f1 (fst (G x)) = f2 (fst (H x))) ->
    ev f1 (map G l) = ev f2 (map H l).
  Proof.
    intros f1 f2 l G H E. unfold ev. induction l as [|x l IH]; simpl; [reflexivity|].
    destruct (E x) as [E1 E2]; [left; reflexivity|]. rewrite E1, E2, IH; [reflexivity|].
    intros. apply E. right. assumption.
  Qed.
End Ev2.

Section Outcomes.
  Variables a b: weapon.
  Variable d: DisciplineSet.
  Hypothesis a_som: a <> Sommerswerd.
  Hypothesis a_mag: a <> MagicSpear.
  Hypothesis a_skill: s_check (WeaponSkill a) d = false.
  Hypothesis b_som: b <> Sommerswerd.
  Hypothesis b_mag: b <> MagicSpear.
  Hypothesis b_skill: s_check (WeaponSkill b) d = false.

  (* The chapters where the two weapons can be exchanged *)
  Variable OkC: cid -> Prop.
  Hypothesis ok0: OkC 0.

  Definition Rel := SwRel a b.
  Definition NoW := NoAB a b.

  Definition GoodCo (o: chapter_outcome) : Prop :=
    NoW (wset_co (spec_co d o)) /\ forall c, In c (co_dests (spec_co d o)) -> OkC c.

  Variables g1 g2: next_step -> Qc.
  Hypothesis gN: forall x y, Rel x y -> disciplines x = d -> OkC (chapter x) -> g1 (New x) = g2 (New y).
  Hypothesis gW: forall x y, Rel x y -> g1 (Won x) = g2 (Won y).
  Hypothesis gL: g1 Lost = g2 Lost.

  Lemma noab_nil: NoW [].
  Proof. split; intros []. Qed.

  Lemma goodco_goto: forall c, OkC c -> GoodCo (Goto c).
  Proof. intros c O. split; [apply noab_nil|]. intros x [X|[]]. subst. exact O. Qed.

  (* The first condition that holds is the same in both states *)
  Lemma cond_pick: forall l s t, Rel s t -> disciplines s = d ->
    (forall x, In x (spec_cl d l) -> NoW (wset_cond (fst x))) ->
    (filter (fun kv => check_cond s (fst kv)) l = [] /\ filter (fun kv => check_cond t (fst kv)) l = [])
    \/ exists c x q q2, filter (fun kv => check_cond s (fst kv)) l = (c, x) :: q
                   /\ filter (fun kv => check_cond t (fst kv)) l = (c, x) :: q2
                   /\ exists c2, In (c2, spec_co d x) (spec_cl d l).
  Proof.
    intros l s t H D N. assert (disciplines t = d) as DT by (rewrite <- (r_disc _ _ _ _ H); exact D).
    induction l as [|[c x] q IH]; [left; split; reflexivity|].
    cbn [filter fst spec_cl] in N |- *.
    rewrite <- (spec_cond_correct s d c D), <- (spec_cond_correct t d c DT).
    destruct (spec_cond d c) as [| | | | |[]| | |] eqn: S; cbn [spec_cl] in N |- *.
    all: try (right; exists c, x; do 2 eexists; split; [reflexivity|]; split; [reflexivity|];
              exists (Always true); left; reflexivity).
    all: try (apply IH; intros y I; apply N; exact I).
    all: match goal with HH: Rel ?u ?v |- context [if check_cond ?u ?k then _ else _] =>
      rewrite (check_rel a b k u v HH (N _ (or_introl Logic.eq_refl))); destruct (check_cond u k) eqn: K end.
    all: try (right; do 4 eexists; split; [reflexivity|]; split; [reflexivity|]; eexists; left; reflexivity).
    all: destruct IH as [IH|[c3 [x3 [q3 [q4 [E1 [E2 [c4 I4]]]]]]]];
      [ intros y I; apply N; right; exact I
      | left; exact IH
      | right; exists c3, x3, q3, q4; split; [exact E1|]; split; [exact E2|]; exists c4; right; exact I4 ].
  Qed.

  Lemma rel_disc: forall s t, Rel s t -> disciplines t = disciplines s.
  Proof. intros s t H. symmetry. apply (r_disc _ _ _ _ H). Qed.

  (* What follows a fight, as after_fight_core in simulation.v *)
  Lemma after_fight_rel: forall (rec1 rec2: Stt -> chapter_outcome -> Proba next_step) s t mds nco nhp,
    Rel s t -> disciplines s = d -> (forall c, In c (mods_dests mds) -> OkC c) -> GoodCo nco ->
    (forall x y o, Rel x y -> disciplines x = d -> GoodCo o -> ev g1 (rec1 x o) = ev g2 (rec2 y o)) ->
    let af (rec: Stt -> chapter_outcome -> Proba next_step) (stt: Stt) :=
      let stt1 := update_flags (s_set HadCombat) stt in
      let stt2 := if has_raw_mod MultiFight mds then stt1 else unset_potions stt1 in
      match Modifiers.extractl Modifiers.gFakeFight mds with
      | Some cid2 => if nhp =? 0 then single_outcome (New (update_chapter cid2 stt2)) else rec stt2 nco
      | None => if nhp =? 0 then single_outcome Lost
                else rec (update_endurance (fun _ => nhp) stt2) nco
      end in
    ev g1 (af rec1 s) = ev g2 (af rec2 t).
  Proof.
    intros rec1 rec2 s t mds nco nhp H D M C REC af. unfold af. cbv zeta.
    assert (Rel (if has_raw_mod MultiFight mds then update_flags (s_set HadCombat) s else unset_potions (update_flags (s_set HadCombat) s))
                (if has_raw_mod MultiFight mds then update_flags (s_set HadCombat) t else unset_potions (update_flags (s_set HadCombat) t))) as R2.
    { destruct (has_raw_mod MultiFight mds); unfold unset_potions; repeat apply swrel_flags; exact H. }
    assert (forall z, disciplines (if has_raw_mod MultiFight mds then update_flags (s_set HadCombat) z else unset_potions (update_flags (s_set HadCombat) z)) = disciplines z) as DZ.
    { intro z. destruct (has_raw_mod MultiFight mds); reflexivity. }
    destruct (Modifiers.extractl Modifiers.gFakeFight mds) as [c|] eqn: E.
    - destruct (nhp =? 0).
      + unfold single_outcome. rewrite !ev_single. apply gN.
        * apply swrel_chapter. exact R2.
        * simpl. rewrite DZ. exact D.
        * simpl. apply M. eapply extractl_dest; eauto. mod_cases.
      + apply REC; auto. rewrite DZ. exact D.
    - destruct (nhp =? 0); [unfold single_outcome; rewrite !ev_single; exact gL|].
      apply REC; auto.
      + apply swrel_endur; [exact R2|reflexivity].
      + simpl. rewrite DZ. exact D.
  Qed.

  Lemma after_fight_rel2: forall rec1 rec2 s t mds co te,
    Rel s t -> disciplines s = d -> te_dest_in mds te -> (forall c, In c (mods_dests mds) -> OkC c) -> GoodCo co ->
    (forall x y o, Rel x y -> disciplines x = d -> GoodCo o -> ev g1 (rec1 x o) = ev g2 (rec2 y o)) ->
    ev g1 (after_fight rec1 s mds co te) = ev g2 (after_fight rec2 t mds co te).
  Proof.
    intros rec1 rec2 s t mds co te H D I M C REC.
    unfold after_fight. destruct te; simpl in I; eapply after_fight_rel; eauto; apply goodco_goto; apply M; exact I.
  Qed.


  Lemma su_same: forall F s t o, Rel s t -> disciplines s = d -> GoodCo o ->
    ev g1 (s_update_d F s o) = ev g2 (s_update_d F t o).
  Proof.
    induction F as [|md IH]; intros s t o H D [HW HD].
    { simpl. rewrite (r_cur _ _ _ _ H). destruct (curendurance t =? 0); unfold single_outcome; rewrite !ev_single; exact gL. }
    cbn [s_update_d]. rewrite (r_cur _ _ _ _ H). destruct (curendurance t =? 0) eqn: C0; [unfold single_outcome; rewrite !ev_single; exact gL|].
    destruct o as [fd co|fd x y z|lst|lst|sos co|next|sl q co| | |fd c|fd co rounds].
    - (* Fight *)
      destruct fd as [osk opphp mds]. simpl in HW, HD.
      change (ev g1 (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) s mds co te)) (fight s osk opphp mds)))
            = ev g2 (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) t mds co te)) (fight t osk opphp mds)))).
      rewrite <- (fight_same _ _ osk opphp mds (fight_rel a b d a_som a_mag a_skill b_som b_mag b_skill s t H D)).
      apply ev2_merge. intros [te p] I. split; [reflexivity|]. cbn [snd].
      apply after_fight_rel2; auto.
      + apply (fight_dests s osk opphp). apply (in_map fst) in I. exact I.
      + intros c I2. apply HD. apply in_app_iff. right. exact I2.
      + split; [exact HW|]. intros c I2. apply HD. apply in_app_iff. left. exact I2.
    - (* OneRound *)
      destruct fd as [osk opphp mds]. simpl in HW, HD.
      rewrite <- (fight_round_same _ _ (fight_rel a b d a_som a_mag a_skill b_som b_mag b_skill s t H D) osk opphp mds).
      apply ev2_merge. intros [[lw op] p] I. split; [reflexivity|]. cbn [snd].
      apply noab_app in HW. destruct HW as [W1 W2]. apply noab_app in W2. destruct W2 as [W2 W3].
      assert (Rel (update_endurance (fun _ => lw) (update_flags (s_set HadCombat) (unset_potions s)))
                  (update_endurance (fun _ => lw) (update_flags (s_set HadCombat) (unset_potions t)))) as R1
        by (apply swrel_endur; [apply swrel_flags; unfold unset_potions; apply swrel_flags; exact H|reflexivity]).
      destruct (cmp _ _); apply IH; try exact R1; try exact D; split; auto; intros c I2; apply HD; rewrite !in_app_iff; tauto.
    - (* Randomly *)
      simpl in HW, HD.
      apply ev2_merge. intros [co p] I. split; [reflexivity|]. cbn [snd].
      assert (In (spec_co d co, p) (map (fun op => (spec_co d (fst op), snd op)) lst)) as J by (apply in_map_iff; exists (co, p); auto).
      apply IH; auto. split.
      + split; intro A; [apply (proj1 HW)|apply (proj2 HW)]; apply in_flat_map; exists (spec_co d co, p); split; assumption.
      + intros c I2. apply HD. apply in_flat_map. exists (spec_co d co, p). split; assumption.
    - (* Conditionally *)
      rewrite spec_co_cond in HW, HD. simpl in HW, HD.
      assert (forall z, In z (spec_cl d lst) -> NoW (wset_cond (fst z)) /\ NoW (wset_co (snd z))) as NZ.
      { intros z I. split; split; intro A; [apply (proj1 HW)|apply (proj2 HW)|apply (proj1 HW)|apply (proj2 HW)];
          apply in_flat_map; exists z; split; auto; apply in_app_iff; auto. }
      destruct (cond_pick lst s t H D) as [[E1 E2]|[c [x [q [q2 [E1 [E2 [c2 I2]]]]]]]]; [intros z I; apply NZ; exact I| |].
      + rewrite E1, E2. unfold single_outcome. rewrite !ev_single. exact gL.
      + rewrite E1, E2. apply IH; auto. split; [apply (NZ _ I2)|].
        intros c3 I3. apply HD. apply in_flat_map. exists (c2, spec_co d x). split; assumption.
    - (* Simple *)
      simpl in HW, HD. apply noab_app in HW. destruct HW as [W1 W2].
      assert (map (sw_so a b) sos = sos) as SW.
      { clear -W1. induction sos as [|so q IHq]; [reflexivity|]. simpl in *. apply noab_app in W1. destruct W1 as [W1 W3].
        rewrite IHq by exact W3. rewrite sw_so_other; [reflexivity|apply W1|apply W1]. }
      apply IH.
      + pose proof (apply_effects_rel a b sos s t H) as R. rewrite SW in R. exact R.
      + rewrite disc_effects. exact D.
      + split; assumption.
    - (* Goto *) simpl in HD.
      assert (OkC next) as ON by (apply HD; left; reflexivity).
      assert (Rel (update_chapter next s) (update_chapter next t)) as RG by (apply swrel_chapter; exact H).
    rewrite (r_book _ _ _ _ H), (r_chapter _ _ _ _ H), <- (flag_rel a b s t Poisonned2 H), <- (flag_rel a b s t HadCombat H), (r_disc _ _ _ _ H).
    destruct (is_story_chapter (book t) (chapter t) && has_flag Poisonned2 t)%bool.
      + simpl. rewrite (r_cur _ _ _ _ H). destruct (curendurance t - 2 =? 0); unfold single_outcome; rewrite !ev_single; [exact gL|].
      apply gN; [apply swrel_endur; [exact RG|simpl; rewrite (r_cur _ _ _ _ H); reflexivity]|exact D|exact ON].
      + destruct (_ && _ && _)%bool; unfold single_outcome; rewrite !ev_single; apply gN; try exact D; try exact ON; try exact RG.
      unfold heal. apply swrel_endur; [exact RG|]. rewrite (maxhp_rel a b _ _ RG). simpl. rewrite (r_cur _ _ _ _ H). reflexivity.
- (* LoseItemFrom *) simpl in HW, HD.
assert (GoodCo co) as GC by (split; assumption).
destruct q as [|nq]; [apply IH; auto|].
pose proof (held_perm a b s t sl H) as P.
destruct (held_items sl s) as [|x xs] eqn: ES.
{ simpl in P. apply Permutation_sym, Permutation_nil in P. rewrite P. apply IH; auto. }
destruct (held_items sl t) as [|y ys] eqn: ET.
{ apply Permutation_length in P. discriminate. }
assert (list_sum (map snd (y :: ys)) = list_sum (map snd (x :: xs)))%nat as TOT.
{ rewrite (perm_sum _ _ (Permutation_map snd P)), map_map. reflexivity. }
rewrite TOT.
apply (ev2_merge_perm g1 g2 (x :: xs) (y :: ys) (swkv a b)); [exact P|].
intros [i amnt] I. cbn [fst snd swkv]. split; [reflexivity|].
apply IH.
+ apply (update_simple_rel a b s t (LoseItem i 1) H).
+ rewrite disc_simple. exact D.
+ split; simpl; assumption.
- (* GameLost *) unfold single_outcome. rewrite !ev_single. exact gL.
- (* GameWon *) unfold single_outcome. rewrite !ev_single. apply gW. exact H.
- (* EnterFight *) unfold single_outcome. rewrite !ev_single. simpl in HD. apply gN; [apply swrel_chapter, swrel_flags; exact H|exact D|apply HD; left; reflexivity].
- (* Round *) destruct fd as [osk opphp mds]. simpl in HW, HD.
rewrite <- (one_round_same _ _ (fight_rel a b d a_som a_mag a_skill b_som b_mag b_skill s t H D) osk opphp mds).
rewrite !ev_add_proba. f_equal.
+ change (ev g1 (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) s mds co te)) (fst (one_round s osk opphp mds)))) = ev g2 (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) t mds co te)) (fst (one_round s osk opphp mds))))).
  apply ev2_merge. intros [te p] I. split; [reflexivity|]. cbn [snd].
  apply after_fight_rel2; auto.
  * apply (one_round_ok s osk opphp). apply (in_map fst) in I. exact I.
  * intros c I2. apply HD. rewrite !in_app_iff. tauto.
  * split; [exact HW|]. intros c I2. apply HD. rewrite !in_app_iff. tauto.
+ apply ev2_map. intros [[lw op] p] I. cbn [fst snd]. split; [reflexivity|]. apply gN.
  * apply swrel_chapter, swrel_endur; [apply swrel_flags; exact H|reflexivity].
  * simpl. exact D.
  * simpl. pose proof (one_round_snd s osk opphp mds (lw, op) (in_map fst _ _ I)) as OP. simpl in OP.
    destruct (nth_round rounds op OP) as [N|N]; [rewrite N; exact ok0|]. apply HD. rewrite !in_app_iff. tauto.
Qed.

  (* Two outcomes with the same value in any pair of exchanged states *)
  Definition CoRel (o o2: chapter_outcome) : Prop :=
    forall F x y, Rel x y -> disciplines x = d -> ev g1 (s_update_d F x o) = ev g2 (s_update_d F y o2).

  Lemma corel_same: forall o, GoodCo o -> CoRel o o.
  Proof. intros o G F x y H D. apply su_same; assumption. Qed.

  Lemma corel_simple: forall sos o o2, CoRel o o2 -> CoRel (Simple sos o) (Simple (map (sw_so a b) sos) o2).
  Proof.
    intros sos o o2 C F x y H D. destruct F as [|F].
    - simpl. rewrite (r_cur _ _ _ _ H). destruct (curendurance y =? 0); unfold single_outcome; rewrite !ev_single; exact gL.
    - cbn [s_update_d]. rewrite (r_cur _ _ _ _ H). destruct (curendurance y =? 0); [unfold single_outcome; rewrite !ev_single; exact gL|].
      apply C; [apply apply_effects_rel; exact H|rewrite disc_effects; exact D].
  Qed.

  Definition GoodDec (dc: decision) : Prop :=
    NoW (wset_live (spec_dec d dc)) /\ forall c, In c (dec_dests (spec_dec d dc)) -> OkC c.

  Definition IHf (f: nat) : Prop := forall s t dc o, Rel s t -> disciplines s = d -> GoodDec dc ->
    In o (flatten_d f s dc) -> exists o2, In o2 (flatten_d f t dc) /\ CoRel o o2.

  Lemma we_rel: forall f, IHf f -> forall s t sos sos2 nxt o, Rel s t -> disciplines s = d -> map (sw_so a b) sos = sos2 ->
    GoodDec nxt -> In o (map (Simple sos) (flatten_d f (apply_effects s sos) nxt)) ->
    exists o2, In o2 (map (Simple sos2) (flatten_d f (apply_effects t sos2) nxt)) /\ CoRel o o2.
  Proof.
    intros f IH s t sos sos2 nxt o H D E G I. subst sos2. apply in_map_iff in I. destruct I as [x [E X]]. subst o.
    destruct (IH _ _ nxt x (apply_effects_rel a b sos s t H) ltac:(rewrite disc_effects; exact D) G X) as [x2 [X2 C]].
    exists (Simple (map (sw_so a b) sos) x2). split; [apply in_map; exact X2|]. apply corel_simple. exact C.
  Qed.

  Lemma gooddec_take: forall i q nxt, GoodDec (can_take i q nxt) -> NoW (item_w i) /\ GoodDec nxt.
  Proof.
    intros i q nxt [W G]. simpl in W, G. apply noab_app in W. destruct W as [W1 W2]. split; [exact W1|]. split; assumption.
  Qed.

  Lemma gooddec_take1: forall i q nxt, GoodDec (can_take i q nxt) -> GoodDec (can_take i 1 (can_take i q nxt)).
  Proof.
    intros i q nxt [W G]. simpl in W, G |- *. split; [|exact G].
    apply noab_app. split; [apply noab_app in W; apply W|]. apply noab_app. apply noab_app in W. tauto.
  Qed.

  (* The choices of can_take for an item that is not gold *)
  Definition take_body (f: nat) (x: Stt) (i: item) (q: nat) (nxt: decision) : list chapter_outcome :=
    match q with
    | 0 => flatten_d f x nxt
    | 1 =>
      match tcan_take i x with
      | Nope => flatten_d f x nxt
      | SpaceAvailable => map (Simple [GainItem i 1]) (flatten_d f (apply_effects x [GainItem i 1]) nxt)
      | MustDrop lst =>
          flatten_d f x nxt ++
          flat_map (fun l =>
            if Nat.ltb 1 (backpack_slots (book x) i)
              then map (Simple [LoseItem l 1]) (flatten_d f (apply_effects x [LoseItem l 1]) (can_take i q nxt))
            else if order.eqb l i then []
            else map (Simple [LoseItem l 1; GainItem i 1]) (flatten_d f (apply_effects x [LoseItem l 1; GainItem i 1]) nxt)) lst
      end
    | S q => flatten_d f x (can_take i 1 (can_take i q nxt))
    end.

  Lemma take_case: forall f, IHf f -> forall s t i q nxt o, Rel s t -> disciplines s = d -> GoodDec (can_take i q nxt) ->
    In o (take_body f s i q nxt) -> exists o2, In o2 (take_body f t i q nxt) /\ CoRel o o2.
  Proof.
    intros f IH s t i q nxt o H D G I. pose proof IH as IH2. unfold IHf in IH2.
    destruct (gooddec_take i q nxt G) as [WI GN].
    assert (swi a b i = i) as SWI by (apply swi_other; [apply WI|apply WI]).
    destruct q as [|[|q]]; unfold take_body in I |- *.
    - exact (IH2 s t nxt o H D GN I).
    - pose proof (take_rel a b s t i H) as TR. rewrite SWI in TR.
      destruct (tcan_take i s) as [|l1|] eqn: TS; destruct (tcan_take i t) as [|l2|] eqn: TT; inversion TR; subst.
      + eapply we_rel; eauto. simpl. rewrite SWI. reflexivity.
      + apply in_app_iff in I. destruct I as [I|I].
        * destruct (IH2 s t nxt o H D GN I) as [o2 [I2 C]]. exists o2. split; [apply in_app_iff; left; exact I2|exact C].
        * apply in_flat_map in I. destruct I as [l [L I]].
          assert (In (swi a b l) l2) as L2 by (eapply Permutation_in; [symmetry; eassumption|apply in_map; exact L]).
          rewrite (r_book _ _ _ _ H) in I.
          assert (order.eqb (swi a b l) i = order.eqb l i) as EQ.
          { rewrite <- SWI at 1. apply eq_true_iff_eq. rewrite <- !eqb_correct. split; intro E; [apply swi_inj in E|]; congruence. }
          destruct (1 <? backpack_slots (book t) i) eqn: BS.
          -- destruct (we_rel f IH s t [LoseItem l 1] [LoseItem (swi a b l) 1] (can_take i 1 nxt) o H D Logic.eq_refl G I) as [o2 [I2 C]].
             exists o2. split; [|exact C]. apply in_app_iff. right. apply in_flat_map. exists (swi a b l). split; [exact L2|]. exact I2.
          -- destruct (order.eqb l i) eqn: E; [destruct I|].
             destruct (we_rel f IH s t [LoseItem l 1; GainItem i 1] [LoseItem (swi a b l) 1; GainItem i 1] nxt o H D ltac:(simpl; rewrite SWI; reflexivity) GN I) as [o2 [I2 C]].
             exists o2. split; [|exact C]. apply in_app_iff. right. apply in_flat_map. exists (swi a b l). split; [exact L2|]. rewrite EQ. exact I2.
      + exact (IH2 s t nxt o H D GN I).
    - exact (IH2 s t _ o H D (gooddec_take1 i (S q) nxt G) I).
    Qed.

  (* Each choice in one state has a choice of the same value in the other *)
  Lemma flat_rel: forall fl s t dc o, Rel s t -> disciplines s = d -> GoodDec dc -> In o (flatten_d fl s dc) ->
    exists o2, In o2 (flatten_d fl t dc) /\ CoRel o o2.
  Proof.
    induction fl as [|f IH]; intros s t dc o H D G I; [destruct I|].
    assert (disciplines t = d) as DT by (rewrite (rel_disc s t H); exact D).
    cbn [flatten_d] in I |- *.
    rewrite (flag_rel a b s t Poisonned2 H), (has_same a b s t Laumspur H) by discriminate.
    destruct (has_flag Poisonned2 s && has_item Laumspur s)%bool.
    { exact (we_rel f IH s t [ClearFlag Poisonned2; LoseItem Laumspur 1] _ dc o H D Logic.eq_refl G I). }
    destruct dc as [d1 d2|nxt|i q nxt|i p nxt|i p nxt|c nxt|sp|co|r c fd co|nxt|sl k nxt].
    - (* App *) destruct G as [GW GD]. simpl in GW, GD. apply noab_app in GW. destruct GW as [W1 W2].
      assert (GoodDec d1) as G1 by (split; [exact W1|intros c J; apply GD; apply in_app_iff; left; exact J]).
      assert (GoodDec d2) as G2 by (split; [exact W2|intros c J; apply GD; apply in_app_iff; right; exact J]).
      apply in_app_iff in I. destruct I as [I|I].
      + destruct (IH s t d1 o H D G1 I) as [o2 [I2 C]]. exists o2. split; [apply in_app_iff; left; exact I2|exact C].
      + destruct (IH s t d2 o H D G2 I) as [o2 [I2 C]]. exists o2. split; [apply in_app_iff; right; exact I2|exact C].
    - (* retrieve_equipment *) exfalso. destruct G as [GW _]. apply (proj1 GW). cbn [spec_dec wset_live]. apply in_app_iff. left. apply m_weapon.complete.
    - (* can_take *) destruct (eq_dec i Gold) as [EG|NG].
      + subst i. exact (we_rel f IH s t [GainItem Gold q] [GainItem Gold q] nxt o H D Logic.eq_refl (proj2 (gooddec_take Gold q nxt G)) I).
      + destruct i; try congruence; exact (take_case f IH s t _ q nxt o H D G I).
    - (* can_buy *) destruct G as [GW GD]. simpl in GW, GD. apply noab_app in GW. destruct GW as [WI WN].
      assert (GoodDec nxt) as GN by (split; assumption).
      assert (swi a b i = i) as SWI by (apply swi_other; [apply WI|apply WI]).
      rewrite (sitems_rel a b s t Gold H). cbn [swi].
      apply in_app_iff in I. destruct I as [I|I].
      + destruct (IH s t nxt o H D GN I) as [o2 [I2 C]]. exists o2. split; [apply in_app_iff; left; exact I2|exact C].
      + destruct (p <=? sitems Gold s) eqn: PG; [|destruct I].
        pose proof (take_rel a b s t i H) as TR. rewrite SWI in TR.
        destruct (tcan_take i s) as [|l1|] eqn: TS; destruct (tcan_take i t) as [|l2|] eqn: TT; inversion TR; subst; [| |destruct I].
        * destruct (we_rel f IH s t [LoseItem Gold p; GainItem i 1] [LoseItem Gold p; GainItem i 1] nxt o H D ltac:(simpl; rewrite SWI; reflexivity) GN I) as [o2 [I2 C]].
          exists o2. split; [apply in_app_iff; right; exact I2|exact C].
        * apply in_flat_map in I. destruct I as [l [L I]].
          assert (In (swi a b l) l2) as L2 by (eapply Permutation_in; [symmetry; eassumption|apply in_map; exact L]).
          destruct (we_rel f IH s t [LoseItem l 1; LoseItem Gold p; GainItem i 1] [LoseItem (swi a b l) 1; LoseItem Gold p; GainItem i 1] nxt o H D ltac:(simpl; rewrite SWI; reflexivity) GN I) as [o2 [I2 C]].
          exists o2. split; [|exact C]. apply in_app_iff. right. apply in_flat_map. exists (swi a b l). split; [exact L2|exact I2].
    - (* can_sell *) destruct G as [GW GD]. simpl in GW, GD. apply noab_app in GW. destruct GW as [WI WN].
      assert (GoodDec nxt) as GN by (split; assumption).
      assert (swi a b i = i) as SWI by (apply swi_other; [apply WI|apply WI]).
      rewrite (sitems_rel a b s t i H), SWI.
      apply in_app_iff in I. destruct I as [I|I].
      + destruct (IH s t nxt o H D GN I) as [o2 [I2 C]]. exists o2. split; [apply in_app_iff; left; exact I2|exact C].
      + destruct (0 <? sitems i s); [|destruct I].
        destruct (we_rel f IH s t [LoseItem i 1; GainItem Gold p] [LoseItem i 1; GainItem Gold p] nxt o H D ltac:(simpl; rewrite SWI; reflexivity) GN I) as [o2 [I2 C]].
        exists o2. split; [apply in_app_iff; right; exact I2|exact C].
    - (* conditional *) destruct (spec_conditional d c nxt) as [[SF SD]|[SN SD]].
      + rewrite <- (spec_cond_correct s d c D), SF in I. destruct I.
      + destruct G as [GW GD]. rewrite SD in GW, GD. simpl in GW, GD. apply noab_app in GW. destruct GW as [WC WN].
        rewrite <- (spec_cond_correct s d c D) in I. rewrite <- (spec_cond_correct t d c DT), (check_rel a b _ s t H WC).
        destruct (check_cond s (spec_cond d c)); [|destruct I].
        exact (IH s t nxt o H D (conj WN GD) I).
    - (* special *) destruct G as [GW GD]. destruct sp; simpl in GD.
      + (* Cartwheel *)
        rewrite (sitems_rel a b s t Gold H). cbn [swi].
        assert (forall tgt, GoodDec (none (cartwheel_choice (sitems Gold s) tgt))) as GC.
        { intro tgt. unfold GoodDec. cbn [spec_dec wset_live dec_dests]. rewrite cartwheel_spec, cartwheel_wset.
          split; [apply noab_nil|]. intros c I2. apply GD. destruct (cartwheel_dests _ _ _ I2) as [E|E]; subst; simpl; tauto. }
        assert (GoodDec (none (Goto 136))) as G136 by (split; [apply noab_nil|intros c [E|[]]; apply GD; subst; simpl; tauto]).
        apply in_app_iff in I. destruct I as [I|I].
        * destruct (cartwheel_fare <=? sitems Gold s); [|destruct I].
          apply in_app_iff in I. destruct I as [I|I].
          -- destruct (IH s t _ o H D G136 I) as [o2 [I2 C]]. exists o2. split; [|exact C]. apply in_app_iff. left. apply in_app_iff. left. exact I2.
          -- destruct (IH s t _ o H D (GC _) I) as [o2 [I2 C]]. exists o2. split; [|exact C]. apply in_app_iff. left. apply in_app_iff. right. exact I2.
        * apply in_flat_map in I. destruct I as [tgt [T I]].
          destruct (IH s t _ o H D (GC tgt) I) as [o2 [I2 C]]. exists o2. split; [|exact C]. apply in_app_iff. right. apply in_flat_map. exists tgt. split; [exact T|exact I2].
      + (* Portholes *)
        apply (IH s t _ o H D); [|exact I]. split; [apply noab_nil|intros c [E|[]]; apply GD; subst; simpl; tauto].
      + (* B05S127 *)
        rewrite <- (r_sk _ _ _ _ H). exists o. split; [exact I|]. destruct I as [I|[]]. subst o. apply corel_same.
        split; [apply noab_nil|]. intros c I2. apply GD. simpl in I2 |- *. tauto.
    - (* none *)
      assert (GoodCo co) as GC by (destruct G as [GW GD]; split; assumption).
      rewrite (has_same a b s t StrengthPotion4 H), (has_same a b s t StrengthPotion H) by discriminate.
      exists o. split; [exact I|].
      destruct (has_combat co) as [fd|].
      + destruct (fd_has NoPotion fd).
        * destruct I as [I|[]]. subst. apply corel_same. exact GC.
        * destruct I as [I|I]; [subst; apply corel_same; exact GC|].
          apply in_app_iff in I. destruct I as [I|I]; [destruct (has_item StrengthPotion4 s)|destruct (has_item StrengthPotion s)]; try destruct I as [I|[]]; try destruct I; subst; match goal with |- CoRel (Simple ?l _) _ => exact (corel_simple l co co (corel_same co GC)) end.
      + destruct I as [I|[]]. subst. apply corel_same. exact GC.
    - (* evade_fight *) destruct I.
    - (* after_combat *)
      assert (GoodDec nxt) as GN by (destruct G as [GW GD]; split; assumption).
      rewrite (maxhp_rel a b s t H), <- (r_cur _ _ _ _ H), <- (r_book _ _ _ _ H).
      destruct (max_hp s - curendurance s =? 0); [exact (IH s t nxt o H D GN I)|].
      erewrite (filter_ext_in (fun ih : bool * item * nat => let '(u, i, _) := ih in u && has_item i t) (fun ih : bool * item * nat => let '(u, i, _) := ih in u && has_item i s)).
      2: { intros [[u i] h] J. simpl in J. repeat (destruct J as [J|J]; [inversion J; subst; rewrite (has_same a b s t _ H) by discriminate; reflexivity|]). destruct J. }
      apply in_app_iff in I. destruct I as [I|I].
      + destruct (IH s t nxt o H D GN I) as [o2 [I2 C]]. exists o2. split; [apply in_app_iff; left; exact I2|exact C].
      + apply in_flat_map in I. destruct I as [[[u i] h] [J I]].
        assert (swi a b i = i) as SWI.
        { apply drink_sub in J. apply filter_In in J. destruct J as [J _]. simpl in J. repeat (destruct J as [J|J]; [inversion J; subst; reflexivity|]). destruct J. }
        destruct (we_rel f IH s t [HealPlayer h; LoseItem i 1] [HealPlayer h; LoseItem i 1] (after_combat nxt) o H D ltac:(simpl; rewrite SWI; reflexivity) G I) as [o2 [I2 C]].
        exists o2. split; [|exact C]. apply in_app_iff. right. apply in_flat_map. exists (u, i, h). split; [exact J|exact I2].
    - (* remove_item_from *)
      assert (GoodDec nxt) as GN by (destruct G as [GW GD]; split; assumption).
      pose proof (held_perm a b s t sl H) as P.
      destruct k as [|m]; [exact (IH s t nxt o H D GN I)|].
      destruct (held_items sl s) as [|x xs] eqn: ES.
      { simpl in P. apply Permutation_sym, Permutation_nil in P. rewrite P. exact (IH s t nxt o H D GN I). }
      destruct (held_items sl t) as [|y ys] eqn: ET; [apply Permutation_length in P; discriminate|].
      apply in_flat_map in I. destruct I as [iq [J I]].
      assert (In (swkv a b iq) (y :: ys)) as J2 by (eapply Permutation_in; [symmetry; exact P|apply in_map; exact J]).
      destruct (we_rel f IH s t [LoseItem (fst iq) 1] [LoseItem (swi a b (fst iq)) 1] (remove_item_from sl m nxt) o H D Logic.eq_refl G I) as [o2 [I2 C]].
      exists o2. split; [|exact C]. apply in_flat_map. exists (swkv a b iq). split; [exact J2|exact I2].
    Qed.

  (* useful_drops: in a state that holds a and b alike, in the current and in the stored equipment, each choice that drops a
     has a choice that drops b instead with the same value, in any pair of exchanged states *)
  Lemma drop_choices_same: forall f s sos sos2 nxt o, Rel s s -> disciplines s = d -> map (sw_so a b) sos = sos2 ->
    GoodDec nxt -> In o (map (Simple sos) (flatten_d f (apply_effects s sos) nxt)) ->
    exists o2, In o2 (map (Simple sos2) (flatten_d f (apply_effects s sos2) nxt)) /\ CoRel o o2.
  Proof.
    intros f s sos sos2 nxt o H D E G I. apply (we_rel f (flat_rel f) s s sos sos2 nxt o H D E G I).
  Qed.
End Outcomes.

(* As in gold_more.v, which can't be imported with this file as both name their own co_dests *)
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

(* The table of the weapons that can still matter, as WeaponUse::new computes it, except that a chapter that can get the
   equipment back counts as naming all the weapons, see wset_live *)
Definition UsedLive (chapters: list (cid * decision)) (d: DisciplineSet) (used: cid -> list weapon) : Prop :=
  forall c dc, find_chapter c chapters = Some dc ->
    incl (wset_live (spec_dec d dc)) (used c) /\
    forall c2, In c2 (dec_dests (spec_dec d dc)) ->
      (find_chapter c2 chapters = None -> forall w, In w (used c)) /\ incl (used c2) (used c).

(* Rewriting with these is cheap, while unfolding the definitions makes the kernel compare the two sides the slow way *)
Lemma fd_eq: forall s dc, flatten_decision s dc = flatten_d 1000 s dc.
Proof. intros. reflexivity. Qed.

Lemma su_eq: forall x o, s_update x o = s_update_d 1000 x o.
Proof. intros. reflexivity. Qed.

Lemma value_absent: forall chapters score fuel x, find_chapter (chapter x) chapters = None ->
  value chapters score fuel x = 0%Qc.
Proof. intros chapters score fuel x F. destruct fuel; [reflexivity|]. simpl. unfold step. rewrite F. reflexivity. Qed.

(* Each choice in one state is matched by a choice of the same value in the other *)
Lemma choices_match: forall (a b: weapon) (d: DisciplineSet),
  a <> Sommerswerd -> a <> MagicSpear -> s_check (WeaponSkill a) d = false ->
  b <> Sommerswerd -> b <> MagicSpear -> s_check (WeaponSkill b) d = false ->
  forall (OkC: cid -> Prop), OkC 0 -> forall (G: next_step -> Qc),
  (forall x y, SwRel a b x y -> disciplines x = d -> OkC (chapter x) -> G (New x) = G (New y)) ->
  (forall x y, SwRel a b x y -> G (Won x) = G (Won y)) ->
  forall dc s t, SwRel a b s t -> disciplines s = d -> GoodDec a b d OkC dc ->
    forall x, In x (map (ev G) (map (s_update (update_flags (s_unset HadCombat) s)) (flatten_decision s dc))) ->
    exists y, In y (map (ev G) (map (s_update (update_flags (s_unset HadCombat) t)) (flatten_decision t dc))) /\ (x <= y)%Qc.
Proof.
  intros a b d A1 A2 A3 B1 B2 B3 OkC O0 G GN GW dc s t R D GD x I.
  rewrite map_map in I. apply in_map_iff in I. destruct I as [o [E I]]. subst x.
  rewrite fd_eq in I.
  destruct (flat_rel a b d A1 A2 A3 B1 B2 B3 OkC O0 G G GN GW Logic.eq_refl 1000 s t dc o R D GD I) as [o2 [I2 C]].
  exists (ev G (s_update (update_flags (s_unset HadCombat) t) o2)). split.
  - rewrite map_map. apply in_map_iff. exists o2. split; [reflexivity|]. rewrite fd_eq. exact I2.
  - pose proof (C 1000 _ _ (swrel_flags a b s t (s_unset HadCombat) R) D) as C2.
    (* stated with s_update, as a rewrite that has to find it is very slow *)
    assert (ev G (s_update (update_flags (s_unset HadCombat) s) o) = ev G (s_update (update_flags (s_unset HadCombat) t) o2))
      as C3 by exact C2.
    rewrite C3. apply Qcle_refl.
Qed.

(* O1 and O3: two states that only differ by the exchange of two plain weapons have the same value. The weapons are plain
   for the disciplines d of the character, and not in the table at the chapter of the states. The score of the end of the
   book must not tell them apart either, which holds when it only reads what get_boundary lists. *)
Theorem weapon_swap_value: forall (chapters: list (cid * decision)) (score: Stt -> Qc) (d: DisciplineSet)
    (used: cid -> list weapon) (a b: weapon),
  a <> Sommerswerd -> a <> MagicSpear -> s_check (WeaponSkill a) d = false ->
  b <> Sommerswerd -> b <> MagicSpear -> s_check (WeaponSkill b) d = false ->
  UsedLive chapters d used -> find_chapter 0 chapters = None ->
  (forall x y, SwRel a b x y -> score x = score y) ->
  forall fuel s t, SwRel a b s t -> disciplines s = d -> NoAB a b (used (chapter s)) ->
    value chapters score fuel s = value chapters score fuel t.
Proof.
  intros chapters score d used a b A1 A2 A3 B1 B2 B3 U Z SC.
  set (OkC := fun c => NoAB a b (used c) \/ find_chapter c chapters = None).
  induction fuel as [|fuel IH]; intros s t H D N; [reflexivity|].
  cbn [value]. unfold step. rewrite <- (r_chapter _ _ _ _ H).
  destruct (find_chapter (chapter s) chapters) as [dc|] eqn: F; [|reflexivity].
  set (G := fun ns => match ns with Lost => 0%Qc | Won x => score x | New x => value chapters score fuel x end).
  assert (forall x y, SwRel a b x y -> disciplines x = d -> OkC (chapter x) -> G (New x) = G (New y)) as GN.
  { intros x y R DX [O|O]; simpl; [apply IH; assumption|].
    rewrite !value_absent; [reflexivity| |exact O]. rewrite <- (r_chapter _ _ _ _ R). exact O. }
  assert (forall x y, SwRel a b x y -> G (Won x) = G (Won y)) as GW by (intros x y R; simpl; apply SC; exact R).
  assert (OkC 0) as O0 by (right; exact Z).
  destruct (U _ _ F) as [UW UD].
  assert (GoodDec a b d OkC dc) as GD.
  { split.
    - split; intro I; [apply (proj1 N)|apply (proj2 N)]; apply UW; exact I.
    - intros c2 I. destruct (UD c2 I) as [UN UI]. unfold OkC. destruct (find_chapter c2 chapters) eqn: F2; [left|right; reflexivity].
      split; intro J; [apply (proj1 N)|apply (proj2 N)]; apply UI; exact J. }
  apply Qcle_antisym; apply best_le.
  - exact (choices_match a b d A1 A2 A3 B1 B2 B3 OkC O0 G GN GW dc s t H D GD).
  - apply (choices_match a b d A1 A2 A3 B1 B2 B3 OkC O0 G GN GW dc t s).
    + apply swrel_sym. exact H.
    + rewrite <- (r_disc _ _ _ _ H). exact D.
    + exact GD.
Qed.

(* canonical renames the plain weapons of a state, in the current equipment and in the stored one, keeping those held in
   both the same in both: this applies a permutation of the plain weapons, which is a sequence of exchanges of two of them *)
Inductive SwChain (d: DisciplineSet) (used: list weapon) : Stt -> Stt -> Prop :=
  | sc_refl: forall s, SwChain d used s s
  | sc_step: forall a b s u t, Plain d used a -> Plain d used b -> SwRel a b s u -> SwChain d used u t ->
      SwChain d used s t.

Theorem weapon_rename_value: forall (chapters: list (cid * decision)) (score: Stt -> Qc) (d: DisciplineSet)
    (used: cid -> list weapon),
  UsedLive chapters d used -> find_chapter 0 chapters = None ->
  (forall a b x y, Plain d [] a -> Plain d [] b -> SwRel a b x y -> score x = score y) ->
  forall fuel s t, SwChain d (used (chapter s)) s t -> disciplines s = d ->
    value chapters score fuel s = value chapters score fuel t.
Proof.
  intros chapters score d used U Z SC fuel s t C.
  assert (forall w, Plain d (used (chapter s)) w -> Plain d [] w) as PE.
  { intros w [W1 [W2 [W3 _]]]. split; [exact W1|split; [exact W2|split; [exact W3|intros []]]]. }
  remember (used (chapter s)) as us eqn: E.
  induction C as [s|a b s u t PA PB R C IH]; intro D; [reflexivity|].
  pose proof (PE a PA) as PA0. pose proof (PE b PB) as PB0.
  destruct PA as [A1 [A2 [A3 A4]]]. destruct PB as [B1 [B2 [B3 B4]]].
  rewrite (weapon_swap_value chapters score d used a b A1 A2 A3 B1 B2 B3 U Z (fun x y R2 => SC a b x y PA0 PB0 R2) fuel s u R D)
    by (rewrite <- E; split; assumption).
  apply IH.
  - rewrite <- (r_chapter _ _ _ _ R). exact E.
  - rewrite <- (r_disc _ _ _ _ R). exact D.
Qed.
