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
End Outcomes.
