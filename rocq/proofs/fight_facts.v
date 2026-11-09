(* What a fight depends on, and where it can lead: the facts about fights that the comparison of values relies on. *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import lists.
Require Import proba.
Require Import combat.
Require Import rules.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.QArith.QArith.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
From Stdlib Require Import Lia.
Require Import Field.

Open Scope Qc_scope.

(* What a fight can look at *)
Record SameFight (s t: Stt) : Prop := {
  sf_cur: curendurance s = curendurance t;
  sf_sk: sk s = sk t;
  sf_disc: disciplines s = disciplines t;
  sf_count: all_character_items_slot_count s SWeapon = all_character_items_slot_count t SWeapon;
  (* the weapons that a fight looks at: the two magical ones, and that of the Weapon Skill *)
  sf_weapon: forall w, (w = Sommerswerd \/ w = MagicSpear \/ s_check (WeaponSkill w) (disciplines s) = true) ->
    has_item (Weapon w) s = has_item (Weapon w) t;
  sf_shield: has_item Shield s = has_item Shield t;
  sf_helm: has_item (GenSpecial S10) s = has_item (GenSpecial S10) t;
  sf_limb: has_flag LimbDeath s = has_flag LimbDeath t;
  sf_psr: has_flag PermanentSkillReduction s = has_flag PermanentSkillReduction t;
  sf_psr2: has_flag PermanentSkillReduction2 s = has_flag PermanentSkillReduction2 t;
  sf_str: has_flag StrengthPotionActive s = has_flag StrengthPotionActive t;
  sf_pot: has_flag PotentStrengthPotionActive s = has_flag PotentStrengthPotionActive t;
}.

Lemma same_fight_endurance: forall s t lw, SameFight s t ->
  SameFight (update_endurance (fun _ => lw) s) (update_endurance (fun _ => lw) t).
Proof.
  intros s t lw H. destruct H; constructor; unfold all_character_items_slot_count, all_character_items_slot,
    has_item, has_flag in *; simpl in *; auto.
Qed.

Lemma wspec_skill: forall d l w, Modifiers.wspec d l = Some w -> s_check (WeaponSkill w) d = true.
Proof.
  intros d l w. induction l as [|x l IH]; simpl; [discriminate|].
  destruct (s_check (WeaponSkill x) d) eqn: E; [|exact IH]. intro F. inversion F. subst. exact E.
Qed.

Section SameFightLemmas.
  Variables (s t: Stt).
  Hypothesis H: SameFight s t.

  Lemma ratio_same: forall osk mds, get_ratio s osk mds = get_ratio t osk mds.
  Proof.
    intros. unfold get_ratio.
    rewrite (sf_sk _ _ H), (sf_disc _ _ H), (sf_count _ _ H), (sf_shield _ _ H), (sf_helm _ _ H), (sf_limb _ _ H),
      (sf_psr _ _ H), (sf_psr2 _ _ H), (sf_str _ _ H), (sf_pot _ _ H), (sf_weapon _ _ H Sommerswerd ltac:(tauto)), (sf_weapon _ _ H MagicSpear ltac:(tauto)).
    destruct (Modifiers.wspec _ _) as [w|] eqn: W; [|reflexivity].
    rewrite (sf_weapon _ _ H w); [reflexivity|]. right. right. rewrite (sf_disc _ _ H). eapply wspec_skill. exact W.
  Qed.

  Lemma moddmg_same: forall op mds d, moddmg s op mds d = moddmg t op mds d.
  Proof.
    intros. unfold moddmg. destruct d.
    rewrite (sf_cur _ _ H), (sf_disc _ _ H), (sf_weapon _ _ H Sommerswerd ltac:(tauto)). reflexivity.
  Qed.

  Lemma round_results_same: forall osk op mds, round_results s osk op mds = round_results t osk op mds.
  Proof.
    intros. unfold round_results. rewrite ratio_same. apply map_ext. intros. apply moddmg_same.
  Qed.

  Lemma fight_round_same: forall osk op mds, fight_round s osk op mds = fight_round t osk op mds.
  Proof.
    intros. unfold fight_round. rewrite ratio_same. f_equal. apply map_ext. intros. rewrite moddmg_same. reflexivity.
  Qed.

  Lemma common_round_same: forall late mds lw op p, common_round late s mds lw op p = common_round late t mds lw op p.
  Proof. intros. unfold common_round. rewrite (sf_cur _ _ H). reflexivity. Qed.

  Lemma fold_rounds_ext: forall f g l, (forall x, f x = g x) -> fold_rounds f l = fold_rounds g l.
  Proof.
    intros. unfold fold_rounds. induction l; simpl; auto. rewrite IHl, H0. reflexivity.
  Qed.

  Lemma one_round_same: forall osk op mds, one_round s osk op mds = one_round t osk op mds.
  Proof.
    intros. unfold one_round. rewrite round_results_same. apply fold_rounds_ext.
    intros [lw o]. apply common_round_same.
  Qed.

  Lemma fight_step_same: forall osk op mds, fight_step s osk op mds = fight_step t osk op mds.
  Proof.
    intros. unfold fight_step. rewrite round_results_same, (sf_cur _ _ H).
    destruct (Modifiers.extractl Modifiers.gStopFight _); auto.
    destruct (Modifiers.extractl gOnPickOne _); auto.
    destruct (Modifiers.extractl Modifiers.gEvaded _); auto.
    destruct (Modifiers.extractl Modifiers.gPoisonous _); auto.
    - unfold poisonous_round. rewrite (sf_cur _ _ H). reflexivity.
    - apply fold_rounds_ext. intros [lw o]. apply common_round_same.
  Qed.
End SameFightLemmas.

Lemma fight_rounds_same: forall fuel s t osk fr mds acc, SameFight s t ->
  fight_rounds fuel s osk fr mds acc = fight_rounds fuel t osk fr mds acc.
Proof.
  induction fuel; intros; simpl; auto.
  destruct fr as [|x fr]; auto.
  remember (x :: fr) as frr.
  replace (fold_right _ _ frr) with (fold_right (fun (st: (nat * nat) * Qc) cur =>
      let '((lw, op), p) := st in
      let r := fight_step (update_endurance (fun _ => lw) t) osk op mds in
      add_round_end (mul_proba (fst r) p, mul_proba (snd r) p) cur) ([], []) frr).
  - apply IHfuel. assumption.
  - clear Heqfrr. induction frr as [|[[lw op] p] q IH]; auto.
    cbn [fold_right]. rewrite (fight_step_same _ _ (same_fight_endurance _ _ lw H)). f_equal. exact IH.
Qed.

Lemma fight_same: forall s t osk op mds, SameFight s t -> fight s osk op mds = fight t osk op mds.
Proof.
  intros. unfold fight. rewrite (sf_cur _ _ H). apply fight_rounds_same. assumption.
Qed.

(* Where a fight can lead to, other than the outcome that follows it *)
Fixpoint fm_dests (m: fight_modifier) : list cid :=
  match m with
  | Evaded c | FakeFight c | OnDamage c | OnLose c | OnNotYetWon c | StopFight c | OnPickOne c => [c]
  | Timed _ x => fm_dests x
  | _ => []
  end.

Definition mods_dests (mds: list fight_modifier) : list cid := flat_map fm_dests mds.

Definition te_dest_in (mds: list fight_modifier) (te: TEscaped) : Prop :=
  match te with
  | Escaped c _ | LateWin c _ | Stopped c _ | TLost c => In c (mods_dests mds)
  | Std _ => True
  end.

Lemma cur_mods_incl: forall mds c, In c (mods_dests (Modifiers.get_cur_mods mds)) -> In c (mods_dests mds).
Proof.
  unfold mods_dests. induction mds as [|m q IH]; intros c H; simpl in *; auto.
  apply in_app_iff.
  destruct m; simpl in H; try rewrite in_app_iff in H; try destruct m; simpl in H; try rewrite in_app_iff in H; firstorder.
Qed.

Lemma advance_incl: forall mds c, In c (mods_dests (Modifiers.advance_time mds)) -> In c (mods_dests mds).
Proof.
  unfold mods_dests. induction mds as [|m q IH]; intros c H; simpl in *; auto.
  apply in_app_iff.
  destruct m; simpl in H; try rewrite in_app_iff in H; try solve [firstorder].
  destruct n as [|[|n]]; simpl in H; try (destruct (Modifiers.survives_timer m)); simpl in H;
    try rewrite in_app_iff in H; firstorder.
Qed.

Lemma extractl_dest: forall (g: fight_modifier -> option cid) mds c,
  (forall m x, g m = Some x -> In x (fm_dests m)) ->
  Modifiers.extractl g mds = Some c -> In c (mods_dests mds).
Proof.
  induction mds as [|m q IH]; intros c G H; simpl in *; try discriminate.
  apply in_app_iff. destruct (g m) eqn: E.
  - inversion H; subst. left. eapply G; eauto.
  - right. apply IH; auto.
Qed.

Ltac mod_cases := let m := fresh in let x := fresh in let E := fresh in
  intros m x E; destruct m; simpl in E; try discriminate; inversion E; subst; simpl; auto.

Lemma lost_result_in: forall mds, te_dest_in mds (lost_result mds).
Proof.
  intros. unfold lost_result. destruct (Modifiers.extractl _ _) eqn: E; simpl; auto.
  apply cur_mods_incl. eapply extractl_dest; eauto. mod_cases.
Qed.

Lemma win_result_in: forall late mds lw, te_dest_in mds (win_result late mds lw).
Proof.
  intros. unfold win_result. destruct (Modifiers.extractl _ _) eqn: E; simpl; auto.
  destruct late; simpl; auto.
  apply cur_mods_incl. eapply extractl_dest; eauto. mod_cases.
Qed.

Definition end_ok (mds: list fight_modifier) (r: RoundEnd) : Prop :=
  forall te, In te (map fst (fst r)) -> te_dest_in mds te.

Lemma end_ok_over: forall mds p te, te_dest_in mds te -> end_ok mds (over p te).
Proof. intros. intros x I. simpl in I. destruct I as [I|[]]. subst. assumption. Qed.

Lemma end_ok_next: forall mds p lw op, end_ok mds (next_round p lw op).
Proof. intros. intros x I. simpl in I. destruct I. Qed.

Lemma end_ok_add: forall mds a b, end_ok mds a -> end_ok mds b -> end_ok mds (add_round_end a b).
Proof.
  intros. intros x I. simpl in I.
  destruct (add_proba_keeps_keys_conv _ _ _ x Logic.eq_refl I); [apply H|apply H0]; assumption.
Qed.

Lemma end_ok_fold: forall mds g l, (forall x, end_ok mds (g x)) -> end_ok mds (fold_rounds g l).
Proof.
  intros. unfold fold_rounds. induction l; simpl.
  - intros x I. destruct I.
  - apply end_ok_add; auto.
Qed.

Lemma common_round_ok: forall late s mds lw op p, end_ok mds (common_round late s mds lw op p).
Proof.
  intros. unfold common_round. destruct (lw =? 0)%nat. { apply end_ok_over, lost_result_in. }
  destruct (Modifiers.extractl gOnDamage _) eqn: E.
  - destruct (lw <? curendurance s)%nat.
    + apply end_ok_over. simpl. apply cur_mods_incl. eapply extractl_dest; eauto. mod_cases.
    + destruct (op =? 0)%nat; [apply end_ok_over, win_result_in | apply end_ok_next].
  - destruct (op =? 0)%nat; [apply end_ok_over, win_result_in | apply end_ok_next].
Qed.

Lemma one_round_ok: forall s osk op mds, end_ok mds (one_round s osk op mds).
Proof.
  intros. apply end_ok_fold. intros [lw o]. apply common_round_ok.
Qed.

Lemma poison_fold_ok: forall mds (lw0: nat) (total: Qc) (l: list (bool * nat * Qc)),
  end_ok mds (fold_right (fun (c: bool * nat * Qc) acc =>
      let '(k, op, w) := c in
      let p := (w / total)%Qc in
      add_round_end
        (if k then over p (lost_result mds)
         else if (op =? 0)%nat then over p (Std lw0)
         else next_round p lw0 op) acc) ([], []) l).
Proof.
  induction l as [|[[k o] w] tl IH]; simpl.
  { intros x I. destruct I. }
  apply end_ok_add; auto.
  destruct k.
  - apply end_ok_over, lost_result_in.
  - destruct (o =? 0)%nat; [apply end_ok_over; simpl; auto | apply end_ok_next].
Qed.

Lemma fight_step_ok: forall s osk op mds, end_ok mds (fight_step s osk op mds).
Proof.
  intros. unfold fight_step.
  destruct (Modifiers.extractl Modifiers.gStopFight _) eqn: E1.
  { apply end_ok_over. simpl. apply cur_mods_incl. eapply extractl_dest; eauto. mod_cases. }
  destruct (Modifiers.extractl gOnPickOne _) eqn: E2.
  { destruct (round_results _ _ _ _) as [|r others].
    - intros x I. destruct I.
    - apply end_ok_add.
      + apply end_ok_over. simpl. eapply extractl_dest; eauto. mod_cases.
      + apply end_ok_fold. intros [lw o]. destruct (lw =? 0)%nat.
        * apply end_ok_over, lost_result_in.
        * destruct (o =? 0)%nat; [apply end_ok_over; simpl; auto | apply end_ok_next]. }
  destruct (Modifiers.extractl Modifiers.gEvaded _) eqn: E3.
  { apply end_ok_fold. intros [lw o]. destruct (lw =? 0)%nat.
    - apply end_ok_over, lost_result_in.
    - destruct (Modifiers.extractl gOnDamage _) eqn: E4.
      + destruct (lw <? curendurance s)%nat; apply end_ok_over; simpl; apply cur_mods_incl; eapply extractl_dest; eauto; mod_cases.
      + apply end_ok_over; simpl; apply cur_mods_incl; eapply extractl_dest; eauto; mod_cases. }
  destruct (Modifiers.extractl Modifiers.gPoisonous _) eqn: E5.
  2: { apply end_ok_fold. intros [lw o]. apply common_round_ok. }
  apply poison_fold_ok.
Qed.

Lemma fight_rounds_ok: forall fuel s osk fr mds acc all,
  (forall c, In c (mods_dests mds) -> In c (mods_dests all)) ->
  (forall te, In te (map fst acc) -> te_dest_in all te) ->
  forall te, In te (map fst (fight_rounds fuel s osk fr mds acc)) -> te_dest_in all te.
Proof.
  induction fuel; intros s osk fr mds acc all Hm Ha te I; simpl in I; auto.
  destruct fr as [|x tl]; auto.
  remember (x :: tl) as frr.
  eapply IHfuel in I; eauto.
  - intros c Ic. apply Hm, advance_incl. assumption.
  - intros t It. destruct (add_proba_keeps_keys_conv _ _ _ t Logic.eq_refl It) as [A|B]; auto.
    clear - B Hm. revert t B. induction frr as [|[[lw op] p] l IH]; intros t B; simpl in B.
    + destruct B.
    + destruct (add_proba_keeps_keys_conv _ _ _ t Logic.eq_refl B) as [A|C]; auto.
      unfold mul_proba in A. rewrite <- (map_proba_keeps_keys _ _ _ Logic.eq_refl) in A.
      pose proof (fight_step_ok (update_endurance (fun _ => lw) s) osk op mds t A) as G.
      destruct t; simpl in *; auto.
Qed.

(* the number of rounds stays out of the proofs, as the kernel would otherwise try to compute with it *)
Lemma fight_dests_fuel: forall fuel s osk fr mds te,
  In te (map fst (fight_rounds fuel s osk fr mds [])) -> te_dest_in mds te.
Proof.
  intros. eapply fight_rounds_ok in H; eauto. intros t I. destruct I.
Qed.

Lemma fight_dests: forall s osk op mds te, In te (map fst (fight s osk op mds)) -> te_dest_in mds te.
Proof.
  intros s osk op mds te. exact (fight_dests_fuel _ s osk _ mds te).
Qed.
