(* Plain weapons are interchangeable (O1), with the analysis of the book specialised for the character (O3). Two states
   that only differ by exchanging two weapons a and b have the same value, when a and b are plain at their chapter: not
   magical, not that of the Weapon Skill, and not named in the part of the book, as specialised for the disciplines of the
   character, that can still be reached. This is what WeaponUse::canonical relies on, as each of its renamings is a
   sequence of such exchanges. *)
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
Require Import valid_items.
Require Import weapon_analysis.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Sorting.Permutation.
Require Import Stdlib.Bool.Bool.
Require Import Stdlib.Arith.Arith.
From Stdlib Require Import Lia.
Require Import Stdlib.QArith.Qcanon.

Open Scope nat_scope.

Section Swap.
  Variables a b: weapon.

  Definition sww (w: weapon) : weapon :=
    if weapon_beq w a then b else if weapon_beq w b then a else w.

  Lemma weq: forall x y, weapon_beq x y = true <-> x = y.
  Proof. intros. symmetry. apply m_weapon.eqb_correct. Qed.

  Lemma weqf: forall x y, weapon_beq x y = false <-> x <> y.
  Proof. intros. rewrite <- not_true_iff_false. rewrite weq. tauto. Qed.

  Lemma sww_inv: forall w, sww (sww w) = w.
  Proof.
    intro w. unfold sww.
    assert (forall x, weapon_beq x x = true) as RF by (intro x; apply weq; reflexivity).
    destruct (weapon_beq w a) eqn: E1; [apply weq in E1; subst w; rewrite RF|].
    - destruct (weapon_beq b a) eqn: E2; [apply weq in E2; congruence|reflexivity].
    - destruct (weapon_beq w b) eqn: E2.
      + apply weq in E2; subst w. destruct (weapon_beq a a) eqn: E4; [|rewrite RF in E4; discriminate]. reflexivity.
      + rewrite E1, E2. reflexivity.
  Qed.

  Lemma sww_other: forall w, w <> a -> w <> b -> sww w = w.
  Proof.
    intros w A B. unfold sww.
    destruct (weapon_beq w a) eqn: E1; [apply weq in E1; congruence|].
    destruct (weapon_beq w b) eqn: E2; [apply weq in E2; congruence|reflexivity].
  Qed.

  Lemma sww_inj: forall x y, sww x = sww y -> x = y.
  Proof. intros x y E. rewrite <- (sww_inv x), <- (sww_inv y), E. reflexivity. Qed.

  Definition swi (i: item) : item :=
    match i with
    | Weapon w => Weapon (sww w)
    | x => x
    end.

  Lemma swi_inv: forall i, swi (swi i) = i.
  Proof. intros [w| | | | | | | | | |s|s| | | | ]; simpl; try rewrite sww_inv; reflexivity. Qed.

  Lemma swi_inj: forall x y, swi x = swi y -> x = y.
  Proof. intros x y E. rewrite <- (swi_inv x), <- (swi_inv y), E. reflexivity. Qed.

  Lemma swi_slot: forall i, item_slot (swi i) = item_slot i.
  Proof. intros [w| | | | | | | | | |s|s| | | | ]; reflexivity. Qed.

  Lemma swi_other: forall i, ~ In a (item_w i) -> ~ In b (item_w i) -> swi i = i.
  Proof.
    intros [w| | | | | | | | | |s|s| | | | ] A B; simpl in *; try reflexivity.
    rewrite sww_other; auto.
  Qed.

  Lemma swi_not_weapon: forall i, item_slot i <> SWeapon -> swi i = i.
  Proof. intros [w| | | | | | | | | |s|s| | | | ] S; simpl in *; congruence. Qed.

  Definition sw_so (so: simple_outcome) : simple_outcome :=
    match so with
    | GainItem i q => GainItem (swi i) q
    | LoseItem i q => LoseItem (swi i) q
    | x => x
    end.

  Lemma sw_so_other: forall so, ~ In a (wset_so so) -> ~ In b (wset_so so) -> sw_so so = so.
  Proof. intros [] A B; simpl in *; try reflexivity; rewrite swi_other; auto. Qed.

  (* Two maps of items that only differ by the exchange *)
  Definition MapRel (m n: Items) : Prop := forall i, lookup i n = lookup (swi i) m.

  Lemma maprel_qty: forall m n i, MapRel m n -> qty i n = qty (swi i) m.
  Proof. intros m n i H. unfold qty. rewrite H. reflexivity. Qed.

  Lemma maprel_sym: forall m n, MapRel m n -> MapRel n m.
  Proof. intros m n H i. rewrite H, swi_inv. reflexivity. Qed.

  Lemma maprel_empty: MapRel empty empty.
  Proof. intro i. reflexivity. Qed.

  Lemma maprel_insert_with: forall m n f j v, ValidMap m -> ValidMap n -> MapRel m n ->
    MapRel (insert_with f j v m) (insert_with f (swi j) v n).
  Proof.
    intros m n f j v VM VN H i. destruct (eq_dec i (swi j)) as [->|D].
    - rewrite swi_inv, !Lookup.lookup_insert_with by assumption. rewrite H, swi_inv. reflexivity.
    - rewrite Lookup.lookup_insert_with_diff by assumption.
      rewrite Lookup.lookup_insert_with_diff; [apply H|assumption|].
      intro E. apply D. rewrite <- E, swi_inv. reflexivity.
  Qed.

  Lemma maprel_update: forall m n f j, f None = None -> ValidMap m -> ValidMap n -> MapRel m n ->
    MapRel (update f j m) (update f (swi j) n).
  Proof.
    intros m n f j F VM VN H i. destruct (eq_dec i (swi j)) as [->|D].
    - rewrite swi_inv, !Lookup.update_eq_b by assumption. rewrite H, swi_inv. reflexivity.
    - rewrite Lookup.update_diff by assumption.
      rewrite Lookup.update_diff; [apply H|assumption|].
      intro E. apply D. rewrite <- E, swi_inv. reflexivity.
  Qed.

  Lemma maprel_delete: forall m n j, ValidMap m -> ValidMap n -> MapRel m n ->
    MapRel (delete j m) (delete (swi j) n).
  Proof.
    intros m n j VM VN H i. destruct (eq_dec i (swi j)) as [->|D].
    - rewrite swi_inv, !Lookup.lookup_delete by assumption. reflexivity.
    - rewrite Lookup.lookup_delete_diff by assumption.
      rewrite Lookup.lookup_delete_diff; [apply H|assumption|].
      intro E. apply D. rewrite <- E, swi_inv. reflexivity.
  Qed.

  Lemma maprel_add: forall m n i q, ValidMap m -> ValidMap n -> MapRel m n ->
    MapRel (add_item i q m) (add_item (swi i) q n).
  Proof.
    intros m n i q VM VN H. unfold add_item. destruct (q =? 0); [exact H|].
    destruct i; simpl; unfold bag_add, insert; apply maprel_insert_with; assumption.
  Qed.

  Lemma maprel_rm: forall m n i q, ValidMap m -> ValidMap n -> MapRel m n ->
    MapRel (rm_item i q m) (rm_item (swi i) q n).
  Proof.
    intros m n i q VM VN H. unfold rm_item, bag_rm. apply maprel_update; auto.
  Qed.

  Lemma maprel_lose_all: forall m n sl, ValidMap m -> ValidMap n -> MapRel m n ->
    MapRel (lose_all_slot sl m) (lose_all_slot sl n).
  Proof.
    intros m n sl VM VN H.
    assert (forall i x, ValidMap x -> lookup i (lose_all_slot sl x) = if order.eqb (item_slot i) sl then None else lookup i x) as L.
    { intros i x V. pose proof (qty_lose_all sl x i V) as Q. unfold qty in Q.
      unfold lose_all_slot in *.
      assert (ValidMap (fold_right (fun i itms => delete i itms) x (slot_items sl))) as VF.
      { clear Q. induction (slot_items sl) as [|y l IH]; simpl; [exact V|]. apply Delete.valid. exact IH. }
      destruct (order.eqb (item_slot i) sl) eqn: E.
      - destruct (lookup i (fold_right _ x _)) as [v|] eqn: F; [|reflexivity].
        exfalso. apply eqb_correct in E. subst sl.
        clear Q. revert F VF. pose proof (item_in_slot_items i) as I. revert I.
        induction (slot_items (item_slot i)) as [|y l IH]; intros I F VF; [destruct I|]. simpl in *.
        assert (ValidMap (fold_right (fun i itms => delete i itms) x l)) as VL.
        { clear -V. induction l as [|z l IH]; simpl; [exact V|]. apply Delete.valid. exact IH. }
        destruct (eq_dec i y) as [->|D].
        + rewrite Lookup.lookup_delete in F; [discriminate|assumption].
        + rewrite Lookup.lookup_delete_diff in F by assumption. destruct I as [I|I]; [congruence|].
          eapply IH; eauto.
      - assert (forall y, In y (slot_items sl) -> i <> y) as NE.
        { intros y I C. subst. apply Slot.in_slot_items_slot in I. subst. rewrite order.eqb_refl in E. discriminate. }
        clear Q. revert NE VF. induction (slot_items sl) as [|y l IH]; intros NE VF; [reflexivity|]. simpl in *.
        assert (ValidMap (fold_right (fun i itms => delete i itms) x l)) as VL.
        { clear -V. induction l as [|z l IH]; simpl; [exact V|]. apply Delete.valid. exact IH. }
        rewrite Lookup.lookup_delete_diff by auto. apply IH; auto. }
    intro i. rewrite (L i n VN), (L (swi i) m VM), swi_slot, H. reflexivity.
  Qed.

  Lemma lose_all_map: forall sl m, ValidMap m -> ValidMap (lose_all_slot sl m).
  Proof.
    intros sl m V. unfold lose_all_slot. induction (slot_items sl) as [|y l IH]; simpl; [exact V|]. apply Delete.valid. exact IH.
  Qed.

  Lemma maprel_lose_kinds: forall sls m n, ValidMap m -> ValidMap n -> MapRel m n ->
    MapRel (fold_right lose_all_slot m sls) (fold_right lose_all_slot n sls).
  Proof.
    induction sls as [|sl q IH]; intros m n VM VN H; simpl; [exact H|].
    assert (forall x, ValidMap x -> ValidMap (fold_right lose_all_slot x q)) as VQ.
    { clear. intros x V. induction q; simpl; auto. apply lose_all_map. assumption. }
    apply maprel_lose_all; auto.
  Qed.

  (* States that only differ by the exchange *)
  Record SwRel (s t: Stt) : Prop := {
    r_book: book s = book t;
    r_maxe: maxendurance s = maxendurance t;
    r_cur: curendurance s = curendurance t;
    r_sk: sk s = sk t;
    r_disc: disciplines s = disciplines t;
    r_chapter: chapter s = chapter t;
    r_flags: flags s = flags t;
    r_items: MapRel (items s) (items t);
    r_prev: MapRel (previtems s) (previtems t);
    r_vs: ValidBag (items s);
    r_vt: ValidBag (items t);
    r_ps: ValidBag (previtems s);
    r_pt: ValidBag (previtems t);
  }.

  Lemma swrel_sym: forall s t, SwRel s t -> SwRel t s.
  Proof.
    intros s t H. destruct H. constructor; auto using maprel_sym.
  Qed.

  Lemma sitems_rel: forall s t i, SwRel s t -> sitems i t = sitems (swi i) s.
  Proof. intros s t i H. unfold sitems. rewrite (r_items _ _ H). reflexivity. Qed.

  Lemma has_rel: forall s t i, SwRel s t -> has_item i t = has_item (swi i) s.
  Proof. intros s t i H. unfold has_item, has. rewrite (r_items _ _ H). reflexivity. Qed.

  Lemma has_same: forall s t i, SwRel s t -> item_slot i <> SWeapon -> has_item i t = has_item i s.
  Proof. intros s t i H S. rewrite (has_rel _ _ _ H), swi_not_weapon by assumption. reflexivity. Qed.

  Lemma flag_rel: forall s t f, SwRel s t -> has_flag f t = has_flag f s.
  Proof. intros s t f H. unfold has_flag. rewrite (r_flags _ _ H). reflexivity. Qed.

  Lemma maxhp_rel: forall s t, SwRel s t -> max_hp t = max_hp s.
  Proof.
    intros s t H. unfold max_hp. rewrite (r_maxe _ _ H), (has_same _ _ BodyArmor H), (has_same _ _ Helmet H)
      by discriminate. reflexivity.
  Qed.

  Lemma swrel_endur: forall s t f g, SwRel s t -> f (curendurance s) = g (curendurance t) ->
    SwRel (update_endurance f s) (update_endurance g t).
  Proof. intros s t f g H E. destruct H. constructor; simpl; auto. Qed.

  Lemma swrel_chapter: forall s t c, SwRel s t -> SwRel (update_chapter c s) (update_chapter c t).
  Proof. intros s t c H. destruct H. constructor; simpl; auto. Qed.

  Lemma swrel_flags: forall s t f, SwRel s t -> SwRel (update_flags f s) (update_flags f t).
  Proof. intros s t f H. destruct H. constructor; simpl; auto. rewrite r_flags0. reflexivity. Qed.

  Lemma swrel_items: forall s t f g, SwRel s t -> MapRel (f (items s)) (g (items t)) ->
    ValidBag (f (items s)) -> ValidBag (g (items t)) -> SwRel (update_items f s) (update_items g t).
  Proof. intros s t f g H M V1 V2. destruct H. constructor; simpl; auto. Qed.

  Lemma swrel_clamp: forall s t, SwRel s t -> SwRel (clamp_endurance s) (clamp_endurance t).
  Proof.
    intros s t H. unfold clamp_endurance. apply swrel_endur; auto. rewrite (maxhp_rel _ _ H), (r_cur _ _ H). reflexivity.
  Qed.

  Lemma add_bag: forall i q m, ValidBag m -> ValidBag (add_item i q m).
  Proof.
    intros i q m V. destruct q as [|q]; [exact V|]. apply add_item_valid_bag; [lia|exact V].
  Qed.

  Lemma lose_kinds_bag: forall sls m, ValidBag m -> ValidBag (fold_right lose_all_slot m sls).
  Proof. exact lose_kinds_valid. Qed.

  Lemma update_simple_rel: forall s t so, SwRel s t ->
    SwRel (update_simple s so) (update_simple t (sw_so so)).
  Proof.
    intros s t so H.
    pose proof (r_vs _ _ H) as VS. pose proof (r_vt _ _ H) as VT.
    destruct so as [d|v| | |i q|i q|sls|ch| | |f|f]; cbn [update_simple sw_so].
    - apply swrel_endur; auto. rewrite (r_cur _ _ H). reflexivity.
    - unfold heal. apply swrel_endur; auto. rewrite (maxhp_rel _ _ H), (r_cur _ _ H). reflexivity.
    - apply swrel_endur; auto. rewrite (maxhp_rel _ _ H). reflexivity.
    - apply swrel_endur; auto. rewrite (maxhp_rel _ _ H), (r_cur _ _ H). reflexivity.
    - apply swrel_items; auto using add_bag. apply maprel_add; try apply VS; try apply VT. apply H.
    - destruct (q =? 0); [exact H|].
      apply swrel_clamp.
      assert (SwRel (update_items (rm_item i q) s) (update_items (rm_item (swi i) q) t)) as R.
      { apply swrel_items; auto using rm_valid. apply maprel_rm; try apply VS; try apply VT. apply H. }
      destruct i; simpl; auto.
      unfold rm_backpack_content. apply swrel_items; auto.
      + apply maprel_lose_all; [apply (r_vs _ _ R)|apply (r_vt _ _ R)|apply R].
      + apply lose_all_valid. apply (r_vs _ _ R).
      + apply lose_all_valid. apply (r_vt _ _ R).
    - apply swrel_clamp. apply swrel_items; auto using lose_kinds_bag.
      apply maprel_lose_kinds; [apply VS|apply VT|apply H].
    - rewrite (r_disc _ _ H).
      rewrite (has_same _ _ Laumspur H), (has_same _ _ Meal H), (r_book _ _ H), (maxhp_rel _ _ H), (r_cur _ _ H) by discriminate.
      assert (SwRel (heal s 3) (heal t 3)) as HE.
      { unfold heal. apply swrel_endur; auto. rewrite (maxhp_rel _ _ H), (r_cur _ _ H). reflexivity. }
      destruct ch, (s_check Hunting (disciplines t)); try exact H;
      repeat match goal with |- context [if ?c then _ else _] => destruct c end;
      try (apply swrel_endur; [exact H|rewrite (r_cur _ _ H); reflexivity]);
      first [ apply (swrel_items _ _ (rm_item Laumspur 1) (rm_item (swi Laumspur) 1) HE)
            | apply (swrel_items _ _ (rm_item Meal 1) (rm_item (swi Meal) 1) H) ];
      first [ apply maprel_rm; [apply HE|apply HE|apply HE] | apply maprel_rm; [apply H|apply H|apply H]
            | apply rm_valid; first [apply HE|apply H] ].
    - apply swrel_clamp. destruct H. constructor; simpl; auto using maprel_empty.
      + split; [constructor|constructor].
      + split; [constructor|constructor].
    - apply swrel_clamp. destruct H. constructor; simpl; auto using maprel_empty.
      + split; [constructor|constructor].
      + split; [constructor|constructor].
    - apply swrel_flags. exact H.
    - apply swrel_flags. exact H.
  Qed.

  Lemma apply_effects_rel: forall sos s t, SwRel s t ->
    SwRel (apply_effects s sos) (apply_effects t (map sw_so sos)).
  Proof.
    unfold apply_effects. induction sos as [|so q IH]; intros s t H; simpl; auto.
    apply IH. apply update_simple_rel. exact H.
  Qed.

  Lemma disc_simple: forall s so, disciplines (update_simple s so) = disciplines s.
  Proof.
    intros s so. destruct so as [| | | | |i q| |ch| | | |]; simpl; try reflexivity.
    - destruct (q =? 0); [reflexivity|]. destruct i; reflexivity.
    - destruct ch, (s_check Hunting (disciplines s)); repeat match goal with |- context [if ?c then _ else _] => destruct c end;
      reflexivity.
  Qed.

  Lemma disc_effects: forall sos s, disciplines (apply_effects s sos) = disciplines s.
  Proof.
    unfold apply_effects. induction sos as [|so q IH]; intro s; simpl; [reflexivity|]. rewrite IH. apply disc_simple.
  Qed.

  (* The exchange only reorders the lists that go over all the items, or over those of a slot *)
  Lemma perm_swi: forall l, NoDup l -> (forall x, In x l -> In (swi x) l) -> Permutation (map swi l) l.
  Proof.
    intros l N C. apply NoDup_Permutation.
    - apply FinFun.Injective_map_NoDup; [intros x y; apply swi_inj|exact N].
    - exact N.
    - intro x. rewrite in_map_iff. split.
      + intros [y [E I]]. subst. apply C. exact I.
      + intro I. exists (swi x). rewrite swi_inv. split; [reflexivity|]. apply C. exact I.
  Qed.

  Lemma perm_all_items: Permutation (map swi all_items) all_items.
  Proof. apply perm_swi; [exact nodup_all_items|intros; apply all_items_complete]. Qed.

  Lemma perm_slot: forall sl, Permutation (map swi (slot_items sl)) (slot_items sl).
  Proof.
    intro sl. apply perm_swi; [apply nodup_slot|].
    intros x I. apply Slot.in_slot_items_slot in I. subst sl. rewrite <- swi_slot. apply item_in_slot_items.
  Qed.

  Lemma perm_filter {A: Type}: forall (p: A -> bool) l1 l2, Permutation l1 l2 -> Permutation (filter p l1) (filter p l2).
  Proof.
    intros p l1 l2 P. induction P; simpl.
    - constructor.
    - destruct (p x); [constructor|]; assumption.
    - destruct (p x), (p y); try constructor; try reflexivity.
    - eapply Permutation_trans; eassumption.
  Qed.

  Lemma perm_sum: forall l1 l2, Permutation l1 l2 -> list_sum l1 = list_sum l2.
  Proof.
    intros l1 l2 P. induction P; simpl; lia.
  Qed.

  Definition swkv (iq: item * nat) : item * nat := (swi (fst iq), snd iq).

  Lemma held_perm: forall s t sl, SwRel s t -> Permutation (held_items sl t) (map swkv (held_items sl s)).
  Proof.
    intros s t sl H. unfold held_items.
    assert (map swkv (filter (fun iq : item * nat => 0 <? snd iq) (map (fun i => (i, sitems i s)) (slot_items sl)))
          = filter (fun iq : item * nat => 0 <? snd iq) (map (fun i => (i, sitems i t)) (map swi (slot_items sl)))) as E.
    { induction (slot_items sl) as [|x l IH]; [reflexivity|]. simpl.
      rewrite (sitems_rel s t (swi x) H), swi_inv. destruct (0 <? sitems x s); simpl; rewrite IH; reflexivity. }
    rewrite E. apply perm_filter. apply Permutation_map. symmetry. apply perm_slot.
  Qed.

  Lemma held_same: forall s t sl, SwRel s t -> sl <> SWeapon -> held_items sl t = held_items sl s.
  Proof.
    intros s t sl H S. unfold held_items. f_equal. apply map_ext_in. intros i I.
    rewrite (sitems_rel s t i H), swi_not_weapon; [reflexivity|].
    apply Slot.in_slot_items_slot in I. congruence.
  Qed.

  Lemma all_held_perm: forall s t, SwRel s t -> Permutation (all_held_items t) (map swkv (all_held_items s)).
  Proof.
    intros s t H. unfold all_held_items.
    assert (map swkv (filter (fun iq : item * nat => 0 <? snd iq) (map (fun i => (i, sitems i s)) all_items))
          = filter (fun iq : item * nat => 0 <? snd iq) (map (fun i => (i, sitems i t)) (map swi all_items))) as E.
    { induction all_items as [|x l IH]; [reflexivity|]. simpl.
      rewrite (sitems_rel s t (swi x) H), swi_inv. destruct (0 <? sitems x s); simpl; rewrite IH; reflexivity. }
    rewrite E. apply perm_filter. apply Permutation_map. symmetry. apply perm_all_items.
  Qed.

  Lemma esum_rel: forall g m n, MapRel m n -> (forall i, g (swi i) = g i) -> esum g n = esum g m.
  Proof.
    intros g m n H G. unfold esum, lsum.
    rewrite <- (perm_sum _ _ (Permutation_map (fun i => g i * qty i m) perm_all_items)).
    rewrite map_map. f_equal. apply map_ext. intro i. rewrite (maprel_qty m n i H), G. reflexivity.
  Qed.

  Lemma count_rel: forall s t sl, SwRel s t ->
    all_character_items_slot_count t sl = all_character_items_slot_count s sl.
  Proof.
    intros s t sl H.
    change (items_slot_count (items t) sl = items_slot_count (items s) sl).
    rewrite !count_esum by (first [apply (r_vt _ _ H) | apply (r_vs _ _ H)]).
    apply esum_rel; [apply (r_items _ _ H)|]. intro i. unfold count_g. rewrite swi_slot. reflexivity.
  Qed.

  (* A condition that only asks for any weapon *)
  Lemma any_of_check: forall c l x, any_of c = Some l ->
    check_cond x c = existsb (fun w => Nat.leb 1 (sitems (Weapon w) x)) l.
  Proof.
    induction c; intros l x E; simpl in E; try discriminate.
    - destruct (any_of c1) as [l1|] eqn: E1; [|discriminate]. destruct (any_of c2) as [l2|] eqn: E2; [|discriminate].
      inversion E. subst. simpl. rewrite existsb_app, (IHc1 l1), (IHc2 l2) by reflexivity. reflexivity.
    - destruct i; try discriminate. destruct n as [|[|n]]; try discriminate. inversion E. subst. simpl.
      rewrite orb_false_r. reflexivity.
  Qed.

  Lemma covers_exists: forall l (p: weapon -> bool), covers_all l = true ->
    existsb p l = existsb p all_weapons.
  Proof.
    intros l p C. apply eq_true_iff_eq. rewrite !existsb_exists. split.
    - intros [w [_ P]]. exists w. split; [apply m_weapon.complete|exact P].
    - intros [w [_ P]]. exists w. split; [|exact P].
      unfold covers_all in C. rewrite forallb_forall in C. specialize (C w (m_weapon.complete w)).
      apply existsb_exists in C. destruct C as [x [I E]]. apply weq in E. subst. exact I.
  Qed.

  Lemma any_weapon_rel: forall s t c, SwRel s t -> any_weapon c = true -> check_cond t c = check_cond s c.
  Proof.
    intros s t c H A. unfold any_weapon in A. destruct (any_of c) as [l|] eqn: E; [|discriminate].
    rewrite !(any_of_check c l _ E), !(covers_exists l _ A).
    apply eq_true_iff_eq. rewrite !existsb_exists. split.
    - intros [w [_ P]]. exists (sww w). split; [apply m_weapon.complete|]. rewrite (sitems_rel s t _ H) in P. exact P.
    - intros [w [_ P]]. exists (sww w). split; [apply m_weapon.complete|]. rewrite (sitems_rel s t _ H). simpl.
      rewrite sww_inv. exact P.
  Qed.

  Definition NoAB (l: list weapon) : Prop := ~ In a l /\ ~ In b l.

  Lemma noab_app: forall l1 l2, NoAB (l1 ++ l2) <-> NoAB l1 /\ NoAB l2.
  Proof. intros. unfold NoAB. rewrite !in_app_iff. tauto. Qed.

  Lemma check_rel: forall c s t, SwRel s t -> NoAB (wset_cond c) -> check_cond t c = check_cond s c.
  Proof.
    induction c as [x|c IH|c1 IH1 c2 IH2|c1 IH1 c2 IH2|i q|bb|n|f|lvl]; intros s t H N;
      (match goal with |- check_cond _ ?c = _ => destruct (any_weapon c) eqn: A end;
       [apply (any_weapon_rel s t _ H A)|]);
      simpl in N; try rewrite A in N; simpl.
    - rewrite (r_disc _ _ H). reflexivity.
    - rewrite (IH s t H N). reflexivity.
    - apply noab_app in N. rewrite (IH1 s t H), (IH2 s t H); tauto.
    - apply noab_app in N. rewrite (IH1 s t H), (IH2 s t H); tauto.
    - rewrite (sitems_rel s t i H), swi_other; [reflexivity|apply N|apply N].
    - reflexivity.
    - rewrite (r_cur _ _ H). reflexivity.
    - apply flag_rel. exact H.
    - unfold level, discipline_count. rewrite (r_disc _ _ H). reflexivity.
  Qed.

  (* Taking an item: the choices of what to drop are exchanged *)
  Inductive TakeRel : can_take_result -> can_take_result -> Prop :=
    | tr_space: TakeRel SpaceAvailable SpaceAvailable
    | tr_nope: TakeRel Nope Nope
    | tr_drop: forall l1 l2, Permutation l2 (map swi l1) -> TakeRel (MustDrop l1) (MustDrop l2).

  Lemma take_rel: forall s t i, SwRel s t -> TakeRel (tcan_take i s) (tcan_take (swi i) t).
  Proof.
    intros s t i H. unfold tcan_take. rewrite swi_slot.
    destruct (item_slot i) eqn: S.
    - assert (length (held_items SWeapon t) = length (held_items SWeapon s)) as L.
      { rewrite (Permutation_length (held_perm s t SWeapon H)). apply length_map. }
      rewrite L. destruct (length (held_items SWeapon s) <? 2); [constructor|]. constructor.
      rewrite (Permutation_map fst (held_perm s t SWeapon H)). rewrite !map_map. apply Permutation_refl.
    - rewrite swi_not_weapon by congruence.
      rewrite (has_same s t Backpack H), (held_same s t SBackpack H), (r_book _ _ H) by discriminate.
      destruct (negb _); [constructor|]. destruct (_ <=? 8); [constructor|]. constructor.
      replace (map swi (map fst (held_items SBackpack s))) with (map fst (held_items SBackpack s));
        [apply Permutation_refl|].
      rewrite map_map. apply map_ext_in. intros [j q] I. apply held_in_slot in I. cbn [fst] in *. apply Slot.in_slot_items_slot in I. rewrite swi_not_weapon; [reflexivity|congruence].
    - rewrite swi_not_weapon by congruence.
      rewrite (has_same s t i H), (has_same s t Helmet H), (has_same s t SilverHelmet H) by (try discriminate; congruence).
      destruct (has_item i s); [constructor|].
      destruct (order.eqb i SilverHelmet && has_item Helmet s); [constructor; reflexivity|].
      destruct (order.eqb i Helmet && has_item SilverHelmet s); [constructor; reflexivity|constructor].
    - constructor.
  Qed.

  (* The plain weapons are not looked at by a fight *)
  Variable d: DisciplineSet.
  Hypothesis a_som: a <> Sommerswerd.
  Hypothesis a_mag: a <> MagicSpear.
  Hypothesis a_skill: s_check (WeaponSkill a) d = false.
  Hypothesis b_som: b <> Sommerswerd.
  Hypothesis b_mag: b <> MagicSpear.
  Hypothesis b_skill: s_check (WeaponSkill b) d = false.

  Lemma fight_rel: forall s t, SwRel s t -> disciplines s = d -> SameFight s t.
  Proof.
    intros s t H D. constructor.
    - apply (r_cur _ _ H).
    - apply (r_sk _ _ H).
    - apply (r_disc _ _ H).
    - symmetry. apply count_rel. exact H.
    - intros w W. rewrite (has_rel s t _ H). simpl. rewrite sww_other; [reflexivity| |];
      intro E; subst; destruct W as [W|[W|W]]; subst; try congruence; rewrite D in W; congruence.
    - symmetry. apply has_same; [exact H|discriminate].
    - symmetry. apply has_same; [exact H|discriminate].
    - symmetry. apply flag_rel. exact H.
    - symmetry. apply flag_rel. exact H.
    - symmetry. apply flag_rel. exact H.
    - symmetry. apply flag_rel. exact H.
    - symmetry. apply flag_rel. exact H.
  Qed.
End Swap.
