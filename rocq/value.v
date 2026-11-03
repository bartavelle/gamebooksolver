(* The value of a state: the best chance of winning, given the value of finishing the book. Each theorem about an
   optimization compares values, so that it says something about the results and not about the way they are computed. *)
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
  sf_weapon: forall w, has_item (Weapon w) s = has_item (Weapon w) t;
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

Section SameFightLemmas.
  Variables (s t: Stt).
  Hypothesis H: SameFight s t.

  Lemma ratio_same: forall osk mds, get_ratio s osk mds = get_ratio t osk mds.
  Proof.
    intros. unfold get_ratio.
    rewrite (sf_sk _ _ H), (sf_disc _ _ H), (sf_count _ _ H), (sf_shield _ _ H), (sf_helm _ _ H), (sf_limb _ _ H),
      (sf_psr _ _ H), (sf_psr2 _ _ H), (sf_str _ _ H), (sf_pot _ _ H), (sf_weapon _ _ H Sommerswerd), (sf_weapon _ _ H MagicSpear).
    destruct (Modifiers.wspec _ _) as [w|]; [rewrite (sf_weapon _ _ H w)|]; reflexivity.
  Qed.

  Lemma moddmg_same: forall op mds d, moddmg s op mds d = moddmg t op mds d.
  Proof.
    intros. unfold moddmg. destruct d.
    rewrite (sf_cur _ _ H), (sf_disc _ _ H), (sf_weapon _ _ H Sommerswerd). reflexivity.
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

(* The value of a state, in a book where finishing with a state is worth score. No choice is worth nothing. The fuel is
   the depth of the search: the books have no cycle, so that enough of it gives the true value, and the theorems below
   hold for any amount. *)
Definition qmax (a b: Qc) : Qc := if Qclt_le_dec a b then b else a.

Definition best (l: list Qc) : Qc := fold_right qmax 0 l.

Fixpoint value (chapters: list (cid * decision)) (score: Stt -> Qc) (fuel: nat) (stt: Stt) : Qc :=
  match fuel with
  | O => 0
  | S f => best (map (ev (fun ns =>
      match ns with
      | Lost => 0
      | Won s => score s
      | New s => value chapters score f s
      end)) (step chapters stt))
  end.

Open Scope nat_scope.

Definition ALL : nat := 255.

(* What follows a fight, as in s_update_d *)
Definition after_fight (rec: Stt -> chapter_outcome -> Proba next_step) (stt: Stt) (mds: list fight_modifier)
    (co: chapter_outcome) (te: TEscaped) : Proba next_step :=
  let (nco, nhp) := match te with
    | Escaped c hp | LateWin c hp | Stopped c hp => (Goto c, hp)
    | TLost c => (Goto c, 1%nat)
    | Std hp => (co, hp)
    end in
  let stt1 := update_flags (s_set HadCombat) stt in
  let stt2 := if has_raw_mod MultiFight mds then stt1 else unset_potions stt1 in
  match Modifiers.extractl Modifiers.gFakeFight mds with
  | Some cid2 => if nhp =? 0 then single_outcome (New (update_chapter cid2 stt2)) else rec stt2 nco
  | None => if nhp =? 0 then single_outcome Lost
            else rec (update_endurance (fun _ => nhp) stt2) nco
  end.

(* The items that the rules use on their own, without being asked by the book *)
Definition potions : list item :=
  [Laumspur; StrengthPotion4; StrengthPotion; GenBackpack S2; Potion6Hp; Potion5Hp; Potion4Hp; Potion2Hp].

(* What the rules do on their own: drinking a potion, and being cured of poison *)
Definition rule_effect (so: simple_outcome) : bool :=
  match so with
  | HealPlayer _ | DamagePlayer _ | ClearFlag Poisonned2 | SetFlag StrengthPotionActive | SetFlag PotentStrengthPotionActive => true
  | LoseItem i 1 => existsb (order.eqb i) potions
  | _ => false
  end.

(* A way of telling that two states can't be told apart by what is left to play. R n s t reads: s and t are the same as
   far as a part of the game that needs n can tell. ALL is a need that only the same states can fulfil. The other fields
   say what each part of a book needs. *)
Record Tracker := {
  R : nat -> Stt -> Stt -> Prop;
  cn : bool_cond -> nat;
  sn : simple_outcome -> nat -> nat;
  ln : slot -> nat;
  tn : item -> nat -> nat;
  bn : item -> nat -> nat -> nat;
  en : item -> nat -> nat -> nat;
  cw : nat;
  rn : nat;

  R_mono : forall m n s t, m <= n -> R n s t -> R m s t;
  R_top : forall n m s t, ALL <= n -> ALL <= m -> R n s t -> R m s t;
  R_cur : forall n s t, R n s t -> curendurance s = curendurance t;
  R_book : forall n s t, R n s t -> book s = book t;
  R_chapter : forall n s t, R n s t -> chapter s = chapter t;
  R_disc : forall n s t, R n s t -> disciplines s = disciplines t;
  R_maxhp : forall n s t, R n s t -> max_hp s = max_hp t;
  R_fight : forall n s t, R n s t -> SameFight s t;
  R_poison : forall n s t, R n s t -> has_flag Poisonned2 s = has_flag Poisonned2 t;
  R_had : forall n s t, R n s t -> has_flag HadCombat s = has_flag HadCombat t;
  R_potion : forall n s t i, In i potions -> R n s t -> has_item i s = has_item i t;
  R_endur : forall n s t f g, f (curendurance s) = g (curendurance t) -> R n s t ->
      R n (update_endurance f s) (update_endurance g t);
  R_goto : forall n s t c, R n s t -> R n (update_chapter c s) (update_chapter c t);
  R_set_had : forall n s t, R n s t -> R n (update_flags (s_set HadCombat) s) (update_flags (s_set HadCombat) t);
  R_unset_had : forall n s t, R n s t -> R n (update_flags (s_unset HadCombat) s) (update_flags (s_unset HadCombat) t);
  R_unset_pot : forall n s t, R n s t -> R n (unset_potions s) (unset_potions t);
  R_simple : forall so a n s t, sn so a <= n -> R n s t -> R a (update_simple s so) (update_simple t so);
  R_cond : forall c n s t, cn c <= n -> R n s t -> check_cond s c = check_cond t c;
  R_held : forall sl n s t, ln sl <= n -> R n s t -> held_items sl s = held_items sl t;
  R_all_held : forall n s t, rn <= n -> R n s t -> all_held_items s = all_held_items t;
  R_take : forall i a n s t, tn i a <= n -> R n s t -> tcan_take i s = tcan_take i t;
  R_buy : forall i p a n s t, bn i p a <= n -> R n s t ->
      Nat.leb p (sitems Gold s) = Nat.leb p (sitems Gold t) /\ tcan_take i s = tcan_take i t;
  R_sell : forall i p a n s t, en i p a <= n -> R n s t -> Nat.ltb 0 (sitems i s) = Nat.ltb 0 (sitems i t);
  R_cw : forall n s t, cw <= n -> R n s t -> sitems Gold s = sitems Gold t;

  sn_mono : forall so a b, a <= b -> sn so a <= sn so b;
  sn_ext : forall so a, a <= sn so a;
  sn_rule : forall so a, rule_effect so = true -> sn so a = a;
  sn_slot : forall sl i a, In i (slot_items sl) -> sn (LoseItem i 1) a <= Nat.max (ln sl) a;
  sn_drop : forall i s lst l a, tcan_take i s = MustDrop lst -> In l lst -> sn (LoseItem l 1) a = a;
  sn_cw : forall v a, sn (LoseItemKind [SPouch]) (sn (GainItem Gold v) a) <= Nat.max cw a;

  tn_ext : forall i a, a <= tn i a;
  tn_mono : forall i a b, a <= b -> tn i a <= tn i b;
  tn_idem : forall i a, tn i (tn i a) <= tn i a;
  tn_gain : forall i q a, sn (GainItem i q) a <= tn i a;
  bn_ext : forall i p a, a <= bn i p a;
  bn_gain : forall i p a, sn (LoseItem Gold p) (sn (GainItem i 1) a) <= bn i p a;
  en_ext : forall i p a, a <= en i p a;
  en_gain : forall i p a, sn (LoseItem i 1) (sn (GainItem Gold p) a) <= en i p a;
  rn_tn : forall i a, rn < ALL -> tn i a = a;
  rn_sn : forall a, rn < ALL -> sn RetrieveEquipment a = a;
}.

Section Sim.
  Variable T: Tracker.
  Variable ch: cid -> nat.
  Variable wn: nat.

  Fixpoint co_need (o: chapter_outcome) : nat :=
    match o with
    | Fight fd n => Nat.max (lmax ch (mods_dests (fd_mods fd))) (co_need n)
    | OneRound _ a b c => Nat.max (co_need a) (Nat.max (co_need b) (co_need c))
    | Randomly l => fold_right (fun x acc => Nat.max (co_need (fst x)) acc) 0 l
    | Conditionally l => fold_right (fun x acc => Nat.max (Nat.max (cn T (fst x)) (co_need (snd x))) acc) 0 l
    | Simple l n => fold_right (sn T) (co_need n) l
    | Goto c => ch c
    | LoseItemFrom sl _ n => Nat.max (ln T sl) (co_need n)
    | GameLost => 0
    | GameWon => wn
    | EnterFight _ c => ch c
    | Round fd n l => Nat.max (lmax ch (mods_dests (fd_mods fd))) (Nat.max (co_need n) (Nat.max (ch 0) (lmax ch l)))
    end.

  Fixpoint dec_need (d: decision) : nat :=
    match d with
    | App a b => Nat.max (dec_need a) (dec_need b)
    | retrieve_equipment n => Nat.max (rn T) (dec_need n)
    | can_take i _ n => tn T i (dec_need n)
    | can_buy i p n => bn T i p (dec_need n)
    | can_sell i p n => en T i p (dec_need n)
    | conditional c n => Nat.max (cn T c) (dec_need n)
    | special Cartwheel => Nat.max (cw T) (Nat.max (ch 169) (ch 136))
    | special Portholes => ch 197
    | special B05S127 => Nat.max (ch 159) (ch 93)
    | none o => co_need o
    | evade_fight _ _ _ _ => 0
    | after_combat n => dec_need n
    | remove_item_from sl _ n => Nat.max (ln T sl) (dec_need n)
    end.

  Lemma apply_same: forall sos a n s t, fold_right (sn T) a sos <= n -> R T n s t ->
    R T a (apply_effects s sos) (apply_effects t sos).
  Proof.
    unfold apply_effects. induction sos as [|so q IH]; intros a n s t L H; simpl in *.
    - eapply R_mono; eauto.
    - eapply IH; [apply le_n|]. eapply R_simple; eauto.
  Qed.

  Lemma fold_mono: forall sos a b, a <= b -> fold_right (sn T) a sos <= fold_right (sn T) b sos.
  Proof.
    induction sos; intros; simpl; auto. apply sn_mono. auto.
  Qed.

  Lemma fold_ext: forall sos a, a <= fold_right (sn T) a sos.
  Proof.
    induction sos; intros; simpl; auto. eapply Nat.le_trans; [apply IHsos|apply sn_ext].
  Qed.

  Definition le' (x n: nat) := x <= n \/ ALL <= n.

  Variable f: next_step -> Qc.
  Hypothesis fN: forall x y, R T (ch (chapter x)) x y -> f (New x) = f (New y).
  Hypothesis fW: forall x y, R T wn x y -> f (Won x) = f (Won y).

  Lemma af_core: forall (rec1 rec2: Stt -> chapter_outcome -> Proba next_step) s t n mds nco nhp,
    R T n s t -> lmax ch (mods_dests mds) <= n -> co_need nco <= n ->
    (forall a x y o, co_need o <= a -> R T a x y -> ev f (rec1 x o) = ev f (rec2 y o)) ->
    let af (rec: Stt -> chapter_outcome -> Proba next_step) (stt: Stt) :=
      let stt1 := update_flags (s_set HadCombat) stt in
      let stt2 := if has_raw_mod MultiFight mds then stt1 else unset_potions stt1 in
      match Modifiers.extractl Modifiers.gFakeFight mds with
      | Some cid2 => if nhp =? 0 then single_outcome (New (update_chapter cid2 stt2)) else rec stt2 nco
      | None => if nhp =? 0 then single_outcome Lost
                else rec (update_endurance (fun _ => nhp) stt2) nco
      end in
    ev f (af rec1 s) = ev f (af rec2 t).
  Proof.
    intros rec1 rec2 s t n mds nco nhp HR HL HC REC af. unfold af. cbv zeta.
    assert (R T n (if has_raw_mod MultiFight mds then update_flags (s_set HadCombat) s else unset_potions (update_flags (s_set HadCombat) s))
                  (if has_raw_mod MultiFight mds then update_flags (s_set HadCombat) t else unset_potions (update_flags (s_set HadCombat) t))) as R2.
    { destruct (has_raw_mod MultiFight mds); [|apply R_unset_pot]; apply R_set_had; assumption. }
    destruct (Modifiers.extractl Modifiers.gFakeFight mds) as [c|] eqn: E.
    - destruct (nhp =? 0).
      + unfold single_outcome. rewrite !ev_single. apply fN. simpl.
        eapply R_goto, R_mono; [|exact R2].
        eapply Nat.le_trans; [|exact HL]. apply lmax_le. eapply extractl_dest; eauto. mod_cases.
      + eapply REC; eauto.
    - destruct (nhp =? 0); [reflexivity|].
      eapply REC; [exact HC|]. apply R_endur; [reflexivity|assumption].
  Qed.

  Lemma af_same: forall rec1 rec2 s t n mds co te,
    R T n s t -> te_dest_in mds te -> lmax ch (mods_dests mds) <= n -> co_need co <= n ->
    (forall a x y o, co_need o <= a -> R T a x y -> ev f (rec1 x o) = ev f (rec2 y o)) ->
    ev f (after_fight rec1 s mds co te) = ev f (after_fight rec2 t mds co te).
  Proof.
    intros rec1 rec2 s t n mds co te HR HI HL HC REC.
    assert (forall c, In c (mods_dests mds) -> co_need (Goto c) <= n) as HG.
    { intros c I. simpl. eapply Nat.le_trans; [|exact HL]. apply lmax_le. assumption. }
    unfold after_fight. destruct te; simpl in HI; eapply af_core; eauto.
  Qed.

  Lemma ev_update: forall fuel s t o n, co_need o <= n -> R T n s t ->
    ev f (s_update_d fuel s o) = ev f (s_update_d fuel t o).
  Proof.
    induction fuel as [|md IH]; intros s t o n HN HR.
    { simpl. rewrite (R_cur _ _ _ _ HR). reflexivity. }
    cbn [s_update_d]. rewrite (R_cur _ _ _ _ HR). destruct (curendurance t =? 0) eqn: C0; [reflexivity|].
    destruct o as [fd co|fd a b c|lst|lst|sos co|next|sl q co| | |fd c|fd co rounds].
    - (* Fight *)
      destruct fd as [osk opphp mds]. simpl in HN.
      change (ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) s mds co te)) (fight s osk opphp mds)))
            = ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) t mds co te)) (fight t osk opphp mds)))).
      rewrite <- (fight_same _ _ osk opphp mds (R_fight _ _ _ _ HR)).
      apply ev_merge_map2. intros [te p] I. simpl. split; [reflexivity|].
      eapply af_same; eauto; try lia.
      apply (fight_dests s osk opphp). apply (in_map fst) in I. exact I.
    - (* OneRound *)
      destruct fd as [osk opphp mds]. simpl in HN.
      rewrite <- (fight_round_same _ _ (R_fight _ _ _ _ HR) osk opphp mds).
      apply ev_merge_map2. intros [[lw op] p] I. simpl. split; [reflexivity|].
      destruct (NatLemma.nat_cmp _ _); apply (IH _ _ _ n); try lia;
        (apply R_endur; [reflexivity | apply R_set_had, R_unset_pot; exact HR]).
    - (* Randomly *)
      apply ev_merge_map2. intros [co p] I. simpl. split; [reflexivity|].
      eapply IH; [|exact HR].
      eapply Nat.le_trans; [|exact HN]. exact (lmax_le (fun x => co_need (fst x)) lst (co, p) I).
    - (* Conditionally *)
      assert (forall x, In x lst -> Nat.max (cn T (fst x)) (co_need (snd x)) <= n) as HL.
      { intros x I. eapply Nat.le_trans; [|exact HN].
        exact (lmax_le (fun x => Nat.max (cn T (fst x)) (co_need (snd x))) lst x I). }
      assert (filter (fun kv => check_cond s (fst kv)) lst = filter (fun kv => check_cond t (fst kv)) lst) as FE.
      { apply filter_ext_in. intros x I. eapply R_cond; [|exact HR]. specialize (HL x I). lia. }
      assert (forall c co q, filter (fun kv => check_cond t (fst kv)) lst = (c, co) :: q -> In (c, co) lst) as IL.
      { intros c co q E. assert (In (c, co) (filter (fun kv => check_cond t (fst kv)) lst)) as J by (rewrite E; left; reflexivity).
        apply filter_In in J. tauto. }
      destruct (filter (fun kv => check_cond s (fst kv)) lst) as [|[c co] q];
      destruct (filter (fun kv => check_cond t (fst kv)) lst) as [|[c2 co2] q2] eqn: F2;
        try discriminate; [reflexivity|].
      inversion FE; subst.
      eapply IH; [|exact HR].
      specialize (HL _ (IL _ _ _ Logic.eq_refl)). simpl in HL. lia.
    - (* Simple *)
      eapply IH; [apply le_n|]. eapply apply_same; eauto.
    - (* Goto *)
      simpl in HN.
      assert (R T (ch next) (update_chapter next s) (update_chapter next t)) as RG.
      { apply R_goto. eapply R_mono; eauto. }
      rewrite (R_book _ _ _ _ HR), (R_chapter _ _ _ _ HR), (R_poison _ _ _ _ HR), (R_had _ _ _ _ HR), (R_disc _ _ _ _ HR).
      destruct (_ && has_flag Poisonned2 t)%bool.
      + simpl. rewrite (R_cur _ _ _ _ HR). destruct (curendurance t - 2 =? 0).
        * reflexivity.
        * unfold single_outcome. rewrite !ev_single. apply fN. simpl.
          apply R_endur; [simpl; rewrite (R_cur _ _ _ _ HR); reflexivity|assumption].
      + destruct (_ && _ && _)%bool; unfold single_outcome; rewrite !ev_single; apply fN; simpl.
        * unfold heal. apply R_endur; [|assumption].
          rewrite (R_maxhp _ _ _ _ RG). simpl. rewrite (R_cur _ _ _ _ HR). reflexivity.
        * assumption.
    - (* LoseItemFrom *)
      simpl in HN.
      assert (held_items sl s = held_items sl t) as HH by (eapply R_held; [|exact HR]; lia).
      rewrite HH. destruct q as [|nq].
      + eapply IH; [|exact HR]. lia.
      + destruct (held_items sl t) as [|x tl] eqn: E.
        * eapply IH; [|exact HR]. lia.
        * apply ev_merge_map2. intros [i amnt] I. cbn [fst snd]. split; [reflexivity|].
          eapply IH; [apply le_n|].
          eapply R_simple; [|exact HR].
          assert (In (i, amnt) (held_items sl s)) as I2 by (rewrite HH; exact I).
          pose proof (held_in_slot _ _ _ I2) as IS. simpl in IS.
          pose proof (sn_slot T sl i (co_need (LoseItemFrom sl nq co)) IS) as SL.
          change (co_need (LoseItemFrom sl nq co)) with (Nat.max (ln T sl) (co_need co)) in *. lia.
    - (* GameLost *)
      reflexivity.
    - (* GameWon *)
      unfold single_outcome. rewrite !ev_single. apply fW. eapply R_mono; eauto.
    - (* EnterFight *)
      unfold single_outcome. rewrite !ev_single. apply fN. simpl.
      apply R_goto, R_set_had. eapply R_mono; eauto.
    - (* Round *)
      destruct fd as [osk opphp mds]. simpl in HN.
      rewrite <- (one_round_same _ _ (R_fight _ _ _ _ HR) osk opphp mds).
      rewrite !ev_add_proba. f_equal.
      + change (ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) s mds co te)) (fst (one_round s osk opphp mds))))
              = ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) t mds co te)) (fst (one_round s osk opphp mds))))).
        apply ev_merge_map2. intros [te p] I. simpl. split; [reflexivity|].
        eapply af_same; eauto; try lia.
        apply (one_round_ok s osk opphp). apply (in_map fst) in I. exact I.
      + apply ev_map_same. intros [[lw op] p] I. simpl. split; [reflexivity|].
        apply fN. simpl. apply R_goto.
        apply R_endur; [reflexivity|]. apply R_set_had.
        eapply R_mono; [|exact HR].
        assert (ch (nth op rounds 0) <= Nat.max (ch 0) (lmax ch rounds)).
        { destruct (nth_in_or_default op rounds 0) as [J|J].
          - pose proof (lmax_le ch rounds _ J). lia.
          - rewrite J. lia. }
        lia.
  Qed.

  Lemma le'_up: forall sos x a n, le' x a -> fold_right (sn T) a sos <= n -> le' (fold_right (sn T) x sos) n.
  Proof.
    intros sos x a n [H|H] L.
    - left. eapply Nat.le_trans; [apply fold_mono; exact H|exact L].
    - right. eapply Nat.le_trans; [exact H|]. eapply Nat.le_trans; [apply fold_ext|exact L].
  Qed.

  Definition FlatOk (fl: nat) (n: nat) (s t: Stt) (d: decision) : Prop :=
    flatten_d fl s d = flatten_d fl t d /\ forall o, In o (flatten_d fl s d) -> le' (co_need o) n.

  Lemma with_effect_ok: forall fl sos nxt n s t,
    (forall m x y d, dec_need d <= m -> R T m x y -> FlatOk fl m x y d) ->
    fold_right (sn T) (dec_need nxt) sos <= n -> R T n s t ->
    map (Simple sos) (flatten_d fl (apply_effects s sos) nxt) = map (Simple sos) (flatten_d fl (apply_effects t sos) nxt)
    /\ forall o, In o (map (Simple sos) (flatten_d fl (apply_effects s sos) nxt)) -> le' (co_need o) n.
  Proof.
    intros fl sos nxt n s t IH L HR.
    destruct (IH (dec_need nxt) _ _ nxt (le_n _) (apply_same _ _ _ _ _ L HR)) as [E M].
    split.
    - rewrite E. reflexivity.
    - intros o I. apply in_map_iff in I. destruct I as [x [X J]]. subst. simpl.
      eapply le'_up; [apply M; exact J|exact L].
  Qed.

  Lemma app_ok: forall (a b c d: list chapter_outcome) n,
    a = b -> c = d -> (forall o, In o a -> le' (co_need o) n) -> (forall o, In o c -> le' (co_need o) n) ->
    a ++ c = b ++ d /\ forall o, In o (a ++ c) -> le' (co_need o) n.
  Proof.
    intros. subst. split; auto. intros o I. apply in_app_iff in I. destruct I; auto.
  Qed.

  Lemma app_ok2: forall (a b c d: list chapter_outcome) n,
    a = b -> (forall o, In o a -> le' (co_need o) n) ->
    (c = d /\ forall o, In o c -> le' (co_need o) n) ->
    a ++ c = b ++ d /\ forall o, In o (a ++ c) -> le' (co_need o) n.
  Proof.
    intros a b c d n E M [E2 M2]. apply app_ok; auto.
  Qed.

  Lemma flat_map_ok {A: Type}: forall (l: list A) (g h: A -> list chapter_outcome) n,
    (forall a, In a l -> g a = h a /\ forall o, In o (g a) -> le' (co_need o) n) ->
    flat_map g l = flat_map h l /\ forall o, In o (flat_map g l) -> le' (co_need o) n.
  Proof.
    induction l as [|a l IH]; intros g h n P; simpl.
    - split; auto. intros o [].
    - destruct (P a) as [E M]; [left; reflexivity|].
      destruct (IH g h n) as [E2 M2]; [intros; apply P; right; assumption|].
      apply app_ok; auto.
  Qed.

  Lemma cartwheel_choice_need: forall g tgt, co_need (cartwheel_choice g tgt) <= Nat.max (cw T) (Nat.max (ch 169) (ch 136)).
  Proof.
    intros. unfold cartwheel_choice. generalize (cartwheel_with_token g tgt). intro l.
    induction l as [|[v p] l IH]; simpl; [lia|].
    apply Nat.max_lub; [|exact IH].
    eapply Nat.le_trans; [apply sn_cw|].
    destruct (v <? cartwheel_fare); lia.
  Qed.

  Definition potion_list (bk: nat) : list (bool * item * nat) :=
    [ (Nat.eqb bk 5, GenBackpack S2, 10%nat)
    ; (true, Potion6Hp, 6%nat)
    ; (true, Potion5Hp, 5%nat)
    ; (true, Potion4Hp, 4%nat)
    ; (negb (Nat.eqb bk 1), Laumspur, 4%nat)
    ; (true, Potion2Hp, 2%nat)
    ].

  Lemma drink_sub: forall (U: list (bool * item * nat)) m x,
    In x (filter (fun ih : bool * item * nat => let '(_, _, h) := ih in Nat.leb h m) U
          ++ match rev (filter (fun ih : bool * item * nat => let '(_, _, h) := ih in Nat.ltb m h) U) with
             | [] => [] | y :: _ => [y] end) -> In x U.
  Proof.
    intros U m x I. apply in_app_iff in I. destruct I as [I|I].
    - apply filter_In in I. tauto.
    - destruct (rev _) as [|y q] eqn: E; [destruct I|].
      destruct I as [I|[]]. subst.
      assert (In x (rev (filter (fun ih : bool * item * nat => let '(_, _, h) := ih in Nat.ltb m h) U))) as J by (rewrite E; left; reflexivity).
      apply in_rev in J. apply filter_In in J. tauto.
  Qed.

  Lemma fold_take_need: forall (l: list (item * nat)) nxt, rn T < ALL ->
    dec_need (fold_left (fun acc (iq: item * nat) => can_take (fst iq) (snd iq) acc) l nxt) = dec_need nxt.
  Proof.
    induction l as [|x l IH]; intros nxt S; simpl; auto. rewrite IH by assumption. simpl. apply rn_tn. assumption.
  Qed.

  Lemma flatten_same: forall fl n s t d, dec_need d <= n -> R T n s t -> FlatOk fl n s t d.
  Proof.
    induction fl as [|fl IH]; intros n s t d HN HR.
    { split; [reflexivity|intros o []]. }
    assert (forall sos nxt, fold_right (sn T) (dec_need nxt) sos <= n ->
      map (Simple sos) (flatten_d fl (apply_effects s sos) nxt) = map (Simple sos) (flatten_d fl (apply_effects t sos) nxt)
      /\ forall o, In o (map (Simple sos) (flatten_d fl (apply_effects s sos) nxt)) -> le' (co_need o) n) as WE.
    { intros. eapply with_effect_ok; eauto. }
    assert (forall so x, rule_effect so = true -> sn T so x = x) as RU by (intros; apply sn_rule; assumption).
    assert (forall d2, dec_need d2 <= n -> FlatOk fl n s t d2) as NX by (intros; apply IH; auto).
    unfold FlatOk. cbn [flatten_d].
    rewrite (R_poison _ _ _ _ HR), (R_potion _ n s t Laumspur ltac:(simpl; tauto) HR).
    destruct (has_flag Poisonned2 t && has_item Laumspur t)%bool.
    { apply WE. simpl. rewrite RU by reflexivity. rewrite RU by reflexivity. exact HN. }
    destruct d as [d1 d2|nxt|i q nxt|i p nxt|i p nxt|c nxt|sp|o|a b c' o|nxt|sl k nxt]; cbn [dec_need] in HN.
    - (* App *)
      destruct (NX d1) as [E1 M1]; [lia|]. destruct (NX d2) as [E2 M2]; [lia|].
      apply app_ok; auto.
    - (* retrieve_equipment *)
      rewrite (R_all_held T n s t ltac:(lia) HR).
      match goal with |- context [map (Simple [RetrieveEquipment]) (flatten_d fl (apply_effects t [RetrieveEquipment]) ?nd)] =>
        set (ndec := nd) end.
      destruct (Nat.le_gt_cases ALL (rn T)) as [BIG|SM].
      2: { apply WE. simpl. unfold ndec. rewrite rn_sn by assumption. rewrite fold_take_need by assumption. lia. }
      assert (ALL <= n) as AN by lia.
      set (n2 := sn T RetrieveEquipment (Nat.max n (dec_need ndec))).
      assert (Nat.max n (dec_need ndec) <= n2) as N2 by apply sn_ext.
      assert (R T n2 s t) as R2 by (eapply R_top; [exact AN| |exact HR]; lia).
      destruct (with_effect_ok fl [RetrieveEquipment] ndec n2 s t) as [E M].
      + intros. apply IH; auto.
      + simpl. apply sn_mono. lia.
      + exact R2.
      + split; [exact E|]. intros x I. right. exact AN.
    - (* can_take *)
      destruct (eq_dec i Gold) as [EG|NG].
      { subst i. apply WE. simpl. eapply Nat.le_trans; [apply tn_gain|exact HN]. }
      assert (dec_need nxt <= n) as HA by (eapply Nat.le_trans; [apply tn_ext|exact HN]).
      destruct i; try congruence;
      (destruct q as [|[|q]];
      [ apply NX; exact HA
      | rewrite (R_take T _ (dec_need nxt) n s t HN HR), (R_book _ _ _ _ HR);
        match goal with |- context [tcan_take ?it t] => destruct (tcan_take it t) as [|lst|] eqn: TC end;
        [ apply WE; simpl; eapply Nat.le_trans; [apply tn_gain|exact HN]
        | destruct (NX nxt HA) as [E1 M1]; apply app_ok2; auto; apply flat_map_ok;
          intros l Il; (destruct (_ <? backpack_slots _ _)%nat;
            [ apply WE; simpl; rewrite (sn_drop T _ t lst l _ TC Il); exact HN
            | destruct (order.eqb l _);
              [ split; [reflexivity|intros x []]
              | apply WE; simpl; rewrite (sn_drop T _ t lst l _ TC Il); eapply Nat.le_trans; [apply tn_gain|exact HN]]])
        | apply NX; exact HA ]
      | apply NX; simpl; eapply Nat.le_trans; [apply tn_idem|exact HN] ]).
    - (* can_buy *)
      assert (dec_need nxt <= n) as HA by (eapply Nat.le_trans; [apply bn_ext|exact HN]).
      destruct (R_buy T i p _ n s t HN HR) as [BG BT].
      rewrite BG, BT. destruct (NX nxt HA) as [E1 M1].
      destruct (p <=? sitems Gold t)%nat.
      + destruct (tcan_take i t) as [|lst|] eqn: TC.
        * edestruct (WE [LoseItem Gold p; GainItem i 1] nxt) as [E2 M2]; [simpl; eapply Nat.le_trans; [apply bn_gain|exact HN]|].
          apply app_ok; auto.
        * apply app_ok2; auto. apply flat_map_ok. intros l Il. apply WE. simpl. rewrite (sn_drop T _ t lst l _ TC Il).
          eapply Nat.le_trans; [apply bn_gain|exact HN].
        * apply app_ok; auto. intros x [].
      + apply app_ok; auto. intros x [].
    - (* can_sell *)
      assert (dec_need nxt <= n) as HA by (eapply Nat.le_trans; [apply en_ext|exact HN]).
      rewrite (R_sell T i p _ n s t HN HR). destruct (NX nxt HA) as [E1 M1].
      destruct (0 <? sitems i t)%nat.
      + edestruct (WE [LoseItem i 1; GainItem Gold p] nxt) as [E2 M2]; [simpl; eapply Nat.le_trans; [apply en_gain|exact HN]|].
        apply app_ok; auto.
      + apply app_ok; auto. intros x [].
    - (* conditional *)
      rewrite (R_cond T c n s t ltac:(lia) HR). destruct (check_cond t c).
      + apply NX. lia.
      + split; [reflexivity|intros x []].
    - (* special *)
      destruct sp; cbn [dec_need] in HN.
      + (* Cartwheel *)
        rewrite (R_cw T n s t ltac:(lia) HR).
        assert (forall tgt, dec_need (none (cartwheel_choice (sitems Gold t) tgt)) <= n) as HC.
        { intro tgt. simpl. eapply Nat.le_trans; [apply cartwheel_choice_need|exact HN]. }
        destruct (NX (none (Goto 136))) as [E1 M1]; [simpl; lia|].
        apply app_ok2; [|  |apply flat_map_ok; intros tgt _; exact (NX _ (HC tgt))].
        * destruct (cartwheel_fare <=? sitems Gold t)%nat; [|reflexivity].
          destruct (NX _ (HC (sitems Gold t))) as [E2 _]. rewrite E1, E2. reflexivity.
        * destruct (cartwheel_fare <=? sitems Gold t)%nat; [|intros x []].
          destruct (NX _ (HC (sitems Gold t))) as [_ M2]. intros x I.
          apply in_app_iff in I. destruct I; auto.
      + (* Portholes *)
        apply NX. simpl. exact HN.
      + (* B05S127 *)
        rewrite (sf_sk _ _ (R_fight _ _ _ _ HR)). split; [reflexivity|]. intros x [I|[]]. subst. left. simpl.
        rewrite RU by reflexivity. lia.
    - (* none *)
      rewrite (R_potion _ n s t StrengthPotion4 ltac:(simpl; tauto) HR), (R_potion _ n s t StrengthPotion ltac:(simpl; tauto) HR).
      split; [reflexivity|]. intros x I.
      destruct (has_combat o) as [fd|].
      + destruct (fd_has NoPotion fd).
        * destruct I as [I|[]]. subst. left. exact HN.
        * destruct I as [I|I]; [subst; left; exact HN|].
          apply in_app_iff in I.
          destruct I as [I|I]; [destruct (has_item StrengthPotion4 t)|destruct (has_item StrengthPotion t)];
            try destruct I as [I|[]]; try destruct I; subst; left; simpl; rewrite !RU by reflexivity; exact HN.
      + destruct I as [I|[]]. subst. left. exact HN.
    - (* evade_fight *)
      split; [reflexivity|intros x []].
    - (* after_combat *)
      destruct (NX nxt HN) as [E1 M1].
      rewrite (R_maxhp _ _ _ _ HR), (R_cur _ _ _ _ HR), (R_book _ _ _ _ HR).
      destruct ((max_hp t - curendurance t =? 0)%nat); [split; assumption|].
      assert (forall x, In x (potion_list (book t)) -> In (snd (fst x)) potions) as PL.
      { intros x J. simpl in J. repeat (destruct J as [J|J]; [subst; simpl; tauto|]). destruct J. }
      assert (filter (fun ih : bool * item * nat => let '(u, i, _) := ih in u && has_item i s) (potion_list (book t))
            = filter (fun ih : bool * item * nat => let '(u, i, _) := ih in u && has_item i t) (potion_list (book t))) as PF.
      { apply filter_ext_in. intros [[u i] h] J. specialize (PL _ J). simpl in PL. rewrite (R_potion _ n s t i PL HR). reflexivity. }
      unfold potion_list in PF. rewrite PF.
      apply app_ok2; [exact E1|exact M1|]. apply flat_map_ok.
      intros [[u i] h] J. cbn beta iota. apply WE.
      apply drink_sub in J. apply filter_In in J. destruct J as [J _].
      specialize (PL _ J). simpl in PL. simpl.
      rewrite RU by reflexivity. rewrite sn_rule; [exact HN|].
      repeat (destruct PL as [PL|PL]; [subst; reflexivity|]). destruct PL.
    - (* remove_item_from *)
      assert (dec_need nxt <= n) as HA by lia.
      assert (held_items sl s = held_items sl t) as HH by (eapply R_held; [|exact HR]; lia).
      rewrite HH. destruct k as [|m]; [apply NX; exact HA|].
      destruct (held_items sl t) as [|x tl] eqn: E; [apply NX; exact HA|].
      apply flat_map_ok. intros iq J. apply WE. simpl.
      assert (In iq (held_items sl s)) as J2 by (rewrite HH; exact J).
      pose proof (sn_slot T sl (fst iq) (Nat.max (ln T sl) (dec_need nxt)) (held_in_slot _ _ _ J2)). lia.
  Qed.
End Sim.

(* Two decisions that give different outcomes can still have the same value: a decision is fine if its outcomes in two
   states have the same value, one by one, for everything that can't tell apart what comes next. *)
Definition TopSim (T: Tracker) (ch: cid -> nat) (wn: nat) (d: decision) (n: nat) : Prop :=
  forall f: next_step -> Qc,
    (forall x y, R T (ch (chapter x)) x y -> f (New x) = f (New y)) ->
    (forall x y, R T wn x y -> f (Won x) = f (Won y)) ->
    forall fl fuel s t, R T n s t ->
      Forall2 (fun o1 o2 =>
        ev f (s_update_d fuel (update_flags (s_unset HadCombat) s) o1) = ev f (s_update_d fuel (update_flags (s_unset HadCombat) t) o2))
        (flatten_d fl s d) (flatten_d fl t d).

(* States that R can't tell apart at the need of their chapter have the same value, if each chapter needs no more than
   what is written for it, or has outcomes that have the same value. *)
Theorem value_same_top: forall (T: Tracker) (ch: cid -> nat) (wn: nat) (chapters: list (cid * decision)) (score: Stt -> Qc),
  (forall s t, R T wn s t -> score s = score t) ->
  (forall c d, find_chapter c chapters = Some d -> dec_need T ch wn d <= ch c \/ TopSim T ch wn d (ch c)) ->
  forall fuel s t, R T (ch (chapter s)) s t -> value chapters score fuel s = value chapters score fuel t.
Proof.
  intros T ch wn chapters score HS HB.
  induction fuel as [|fuel IH]; intros s t HR; [reflexivity|].
  cbn [value]. unfold step.
  rewrite <- (R_chapter _ _ _ _ HR).
  destruct (find_chapter (chapter s) chapters) as [d|] eqn: F; [|reflexivity].
  set (g := fun ns => match ns with Lost => 0%Qc | Won x => score x | New x => value chapters score fuel x end).
  destruct (HB _ _ F) as [HN|TS].
  - destruct (flatten_same T ch wn 1000 (ch (chapter s)) s t d HN HR) as [E M].
    unfold flatten_decision. rewrite <- E. rewrite !map_map. f_equal. apply map_ext_in. intros o I.
    assert (R T (ch (chapter s)) (update_flags (s_unset HadCombat) s) (update_flags (s_unset HadCombat) t)) as RN
      by (apply R_unset_had; assumption).
    unfold s_update.
    assert (forall n, co_need T ch wn o <= n -> R T n (update_flags (s_unset HadCombat) s) (update_flags (s_unset HadCombat) t) ->
      ev g (s_update_d 1000 (update_flags (s_unset HadCombat) s) o) = ev g (s_update_d 1000 (update_flags (s_unset HadCombat) t) o)) as G.
    { intros n L R2.
      apply (ev_update T ch wn g (fun x y H => IH x y H) (fun x y H => HS x y H) 1000 _ _ o n L R2). }
    destruct (M o I) as [L|A].
    + apply (G _ L RN).
    + apply (G (Nat.max (ch (chapter s)) (co_need T ch wn o))); [lia|].
      eapply R_top; [exact A| |exact RN]. lia.
  - unfold flatten_decision, s_update. rewrite !map_map. f_equal.
    apply forall2_map. apply TS.
    + intros x y H. apply IH. exact H.
    + intros x y H. apply HS. exact H.
    + exact HR.
Qed.

Theorem value_same: forall (T: Tracker) (ch: cid -> nat) (wn: nat) (chapters: list (cid * decision)) (score: Stt -> Qc),
  (forall s t, R T wn s t -> score s = score t) ->
  (forall c d, find_chapter c chapters = Some d -> dec_need T ch wn d <= ch c) ->
  forall fuel s t, R T (ch (chapter s)) s t -> value chapters score fuel s = value chapters score fuel t.
Proof.
  intros T ch wn chapters score HS HB. apply (value_same_top T ch wn chapters score HS).
  intros c d F. left. eapply HB. exact F.
Qed.
