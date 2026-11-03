(* What keeps a state valid, one effect at a time. An effect can bring the endurance to 0, which is only looked at when
   the next outcome starts, so that the effects have to keep all of ValidState but this. *)
Require Import character chapters mps order lists rules valid_check valid_items.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
Require Import ZArith.
From Stdlib Require Import Lia.
Open Scope nat_scope.

Record Weak (s: Stt) : Prop := {
  w_book : 1 <= book s <= 5;
  w_skill : (10 <= sk s <= 19)%Z;
  w_maxe : 20 <= maxendurance s <= 29;
  w_cur : curendurance s <= max_hp s;
  w_disc : ValidMap (disciplines s);
  w_count : discipline_count s = book s + 4;
  w_ws : OneWeaponSpecMax s;
  w_flags : ValidMap (flags s);
  w_items : ItemsOk (book s) (items s);
  w_prev : ItemsOk (book s) (previtems s);
}.

Lemma valid_weak: forall s, ValidState s -> Weak s.
Proof.
  intros s [B S M P C D N W F I R]. constructor; auto; apply items_ok_iff; assumption.
Qed.

Lemma weak_valid: forall s, Weak s -> 0 < curendurance s -> ValidState s.
Proof.
  intros s [B S M C D N W F I R] P. constructor; auto; apply items_ok_iff; assumption.
Qed.

Lemma maxhp_pos: forall s, Weak s -> 20 <= max_hp s.
Proof.
  intros s H. pose proof (w_maxe _ H). unfold max_hp. lia.
Qed.

Lemma has_item_qty: forall i s, has_item i s = Nat.ltb 0 (qty i (items s)).
Proof.
  intros. apply has_qty.
Qed.

Lemma maxhp_ext: forall s t, maxendurance s = maxendurance t ->
  has_item BodyArmor s = has_item BodyArmor t -> has_item Helmet s = has_item Helmet t -> max_hp s = max_hp t.
Proof.
  intros s t M A B. unfold max_hp. rewrite M, A, B. reflexivity.
Qed.

(* Changing the endurance *)
Lemma weak_endur: forall s f, Weak s -> f (curendurance s) <= max_hp s -> Weak (update_endurance f s).
Proof.
  intros s f [B S M C D N W F I R] L. destruct s. constructor; simpl in *; auto.
Qed.

Lemma weak_chapter: forall s c, Weak s -> Weak (update_chapter c s).
Proof.
  intros s c [B S M C D N W F I R]. destruct s. constructor; simpl in *; auto.
Qed.

Lemma weak_flags: forall s f, Weak s -> (forall x, ValidMap x -> ValidMap (f x)) -> Weak (update_flags f s).
Proof.
  intros s f [B S M C D N W F I R] G. destruct s. constructor; simpl in *; auto.
Qed.

Lemma set_valid: forall (m: FlagSet) f, ValidMap m -> ValidMap (s_set f m).
Proof.
  intros. unfold s_set, insert. apply Insert.insert_with_valid. assumption.
Qed.

Lemma unset_valid: forall (m: FlagSet) f, ValidMap m -> ValidMap (s_unset f m).
Proof.
  intros. unfold s_unset. apply Delete.valid. assumption.
Qed.

Lemma weak_unset_potions: forall s, Weak s -> Weak (unset_potions s).
Proof.
  intros s H. unfold unset_potions. apply weak_flags; auto. intros. apply unset_valid, unset_valid. assumption.
Qed.

(* Changing the items: the endurance is cut to what the items allow *)
Lemma weak_clamp_items: forall s f, Weak s -> ItemsOk (book s) (f (items s)) -> Weak (clamp_endurance (update_items f s)).
Proof.
  intros s f [B S M C D N W F I R] K. destruct s. unfold clamp_endurance. constructor; simpl in *; auto.
  apply Nat.le_min_r.
Qed.

Lemma weak_items_same: forall s f, Weak s -> ItemsOk (book s) (f (items s)) ->
  curendurance s <= max_hp (update_items f s) -> Weak (update_items f s).
Proof.
  intros s f [B S M C D N W F I R] K L. destruct s. constructor; simpl in *; auto.
Qed.

Lemma qty_add_keeps: forall i q m j, ValidMap m -> 0 < qty j m -> 0 < qty j (add_item i q m).
Proof.
  intros i q m j V P. destruct q as [|q]; [exact P|].
  destruct (add_item_form i (S q)) as [f [v [VP [FP E]]]]; [lia|].
  unfold qty in *. rewrite E. destruct (eq_dec j i) as [->|D].
  - rewrite Lookup.lookup_insert_with by exact V. destruct (lookup i m); [apply FP|lia].
  - rewrite Lookup.lookup_insert_with_diff by assumption. exact P.
Qed.

Lemma ltb_mono: forall (x y a: nat), (0 < x -> 0 < y) -> (if Nat.ltb 0 x then a else 0) <= (if Nat.ltb 0 y then a else 0).
Proof.
  intros x y a H. destruct (Nat.ltb 0 x) eqn: E; [|lia]. apply Nat.ltb_lt in E. specialize (H E). apply Nat.ltb_lt in H. rewrite H. lia.
Qed.

Lemma maxhp_add: forall s i q, ValidMap (items s) -> max_hp s <= max_hp (update_items (add_item i q) s).
Proof.
  intros s i q V. unfold max_hp. rewrite !has_item_qty.
  replace (maxendurance (update_items (add_item i q) s)) with (maxendurance s) by (destruct s; reflexivity).
  replace (items (update_items (add_item i q) s)) with (add_item i q (items s)) by (destruct s; reflexivity).
  pose proof (ltb_mono _ _ 4 (qty_add_keeps i q (items s) BodyArmor V)).
  pose proof (ltb_mono _ _ 2 (qty_add_keeps i q (items s) Helmet V)).
  lia.
Qed.

Lemma weak_add: forall s i q, Weak s -> ItemsOk (book s) (add_item i q (items s)) -> Weak (update_items (add_item i q) s).
Proof.
  intros s i q H K. apply weak_items_same; auto. eapply Nat.le_trans; [apply (w_cur _ H)|]. apply maxhp_add, (ok_map _ _ (w_items _ H)).
Qed.

Definition CanGain (bk: nat) (m: Items) (i: item) : Prop :=
  match item_slot i with
  | SPouch => True
  | SSpecial => (i = Helmet -> qty SilverHelmet m = 0) /\ (i = SilverHelmet -> qty Helmet m = 0)
  | SWeapon => esum (count_g SWeapon) m + 1 <= 2
  | SBackpack => esum (used_g bk) m + backpack_slots bk i <= 8 /\ 0 < qty Backpack m
  end.

Lemma pouch_gold: forall i, item_slot i = SPouch -> i = Gold.
Proof.
  intros i E. destruct i; try reflexivity; discriminate.
Qed.

Lemma ok_can_gain: forall bk m i, ItemsOk bk m -> CanGain bk m i -> ItemsOk bk (add_item i 1 m).
Proof.
  intros bk m i H C. unfold CanGain in C. destruct (item_slot i) eqn: SL.
  - apply ok_gain_weapon; auto.
  - destruct C. apply ok_gain_backpack; auto.
  - destruct C. apply ok_gain_special; auto.
  - rewrite (pouch_gold i SL). apply ok_gain_gold. exact H.
Qed.

Lemma free_can_gain: forall i q bk m, free_gain i = true -> ItemsOk bk m -> ItemsOk bk (add_item i q m).
Proof.
  intros i q bk m F H. destruct (eq_dec i Gold) as [->|G]; [apply ok_gain_gold; exact H|].
  destruct q as [|q]; [exact H|].
  assert (item_slot i = SSpecial /\ i <> Helmet /\ i <> SilverHelmet) as [SL [N1 N2]].
  { unfold free_gain in F. destruct i; try discriminate; try congruence.
    - split; [reflexivity|split; discriminate].
    - split; [reflexivity|split; discriminate].
    - destruct s; try discriminate; (split; [reflexivity|split; discriminate]). }
  apply ok_gain_special; auto; try lia; intro; contradiction.
Qed.

Lemma keeps_contents_or: forall sls, keeps_contents sls = true ->
  existsb (fun s => match s with SSpecial => true | _ => false end) sls = false \/ In SBackpack sls.
Proof.
  intros sls K. unfold keeps_contents in K. apply orb_true_iff in K. destruct K as [K|K].
  - left. apply negb_true_iff. exact K.
  - right. apply existsb_exists in K. destruct K as [x [I E]]. destruct x; try discriminate. exact I.
Qed.

Lemma rm_backpack_other: forall i s, i <> Backpack -> rm_backpack_content i s = s.
Proof.
  intros i s N. destruct i; try reflexivity. contradiction.
Qed.

Lemma weak_lose: forall s i q, Weak s -> Weak (update_simple s (LoseItem i q)).
Proof.
  intros s i q H. cbn [update_simple]. destruct (q =? 0) eqn: Q; [exact H|].
  destruct (eq_dec i Backpack) as [->|N].
  - cbn [rm_backpack_content].
    change (update_items (lose_all_slot SBackpack) (update_items (rm_item Backpack q) s))
      with (update_items (fun m => lose_all_slot SBackpack (rm_item Backpack q m)) (update_items (fun m => m) s)).
    replace (update_items (fun m : Items => m) s) with s by (destruct s; reflexivity).
    apply weak_clamp_items; auto. apply ok_rm_backpack, (w_items _ H).
  - rewrite rm_backpack_other by exact N. apply weak_clamp_items; auto. apply ok_rm; auto. apply (w_items _ H).
Qed.

Lemma weak_rm_food: forall s i q, Weak s -> i = Meal \/ i = Laumspur -> Weak (update_items (rm_item i q) s).
Proof.
  intros s i q H F.
  assert (i <> Backpack) as NB by (destruct F; subst; discriminate).
  assert (i <> BodyArmor) as NA by (destruct F; subst; discriminate).
  assert (i <> Helmet) as NH by (destruct F; subst; discriminate).
  pose proof (ok_map _ _ (w_items _ H)) as V.
  apply weak_items_same; auto.
  - apply ok_rm; auto. apply (w_items _ H).
  - replace (max_hp (update_items (rm_item i q) s)) with (max_hp s); [apply (w_cur _ H)|].
    apply maxhp_ext; [destruct s; reflexivity| |]; rewrite !has_item_qty; destruct s; simpl in *;
    rewrite qty_rm_diff; auto.
Qed.

Lemma heal_weak: forall s q, Weak s -> Weak (heal s q).
Proof.
  intros s q H. unfold heal. apply weak_endur; auto. lia.
Qed.

Lemma weak_simple: forall s so, so_safe so = true -> Weak s -> Weak (update_simple s so).
Proof.
  intros s so SF H. destruct so as [d|q| | |i q|i q|sls|c| | |f|f]; cbn [update_simple].
  - apply weak_endur; auto. pose proof (w_cur _ H). lia.
  - apply heal_weak. exact H.
  - apply weak_endur; auto.
  - apply weak_endur; auto. pose proof (w_cur _ H). apply Nat.Div0.div_le_upper_bound. lia.
  - apply weak_add; auto. apply free_can_gain; auto. apply (w_items _ H).
  - exact (weak_lose s i q H).
  - apply weak_clamp_items; auto. apply ok_lose_kinds; [apply keeps_contents_or; exact SF|apply (w_items _ H)].
  - destruct c, (s_check Hunting (disciplines s)); try exact H;
    repeat match goal with |- context [if ?b then _ else _] => destruct b end;
    first [apply weak_endur; auto; pose proof (w_cur _ H); lia
          |apply weak_rm_food; auto; apply heal_weak; exact H
          |apply weak_rm_food; auto].
  - destruct H as [B S M C D N W F I R]. destruct s. unfold clamp_endurance. constructor; simpl in *; auto.
    + apply Nat.le_min_r.
    + apply ok_empty.
  - destruct H as [B S M C D N W F I R]. destruct s. unfold clamp_endurance. constructor; simpl in *; auto.
    + apply Nat.le_min_r.
    + apply ok_empty.
  - apply weak_flags; auto. intros. apply set_valid. assumption.
  - apply weak_flags; auto. intros. apply unset_valid. assumption.
Qed.

Lemma weak_apply: forall sos s, forallb so_safe sos = true -> Weak s -> Weak (apply_effects s sos).
Proof.
  induction sos as [|so q IH]; intros s F H; [exact H|]. simpl in F. apply andb_true_iff in F. destruct F as [F1 F2].
  apply IH; auto. apply weak_simple; auto.
Qed.

(* These leave the character alive *)
Definition keeps_alive (so: simple_outcome) : bool :=
  match so with
  | DamagePlayer _ | MustEat _ => false
  | _ => true
  end.

Lemma alive_simple: forall s so, keeps_alive so = true -> Weak s -> 0 < curendurance s -> 0 < curendurance (update_simple s so).
Proof.
  intros s so A H P. pose proof (maxhp_pos _ H) as M. pose proof (w_maxe _ H) as ME.
  destruct so as [d|q| | |i q|i q|sls|c| | |f|f]; try discriminate; cbn [update_simple].
  - unfold heal. simpl. lia.
  - simpl. lia.
  - change (0 < (max_hp s + curendurance s) / 2). apply Nat.div_str_pos. lia.
  - destruct s; exact P.
  - destruct (q =? 0); [exact P|].
    unfold clamp_endurance. simpl. destruct i; simpl; apply Nat.min_glb_lt; auto;
    unfold max_hp; destruct s; simpl in *; lia.
  - unfold clamp_endurance. simpl. apply Nat.min_glb_lt; auto. unfold max_hp. destruct s; simpl in *; lia.
  - unfold clamp_endurance. simpl. apply Nat.min_glb_lt; auto. unfold max_hp. simpl. lia.
  - unfold clamp_endurance. simpl. apply Nat.min_glb_lt; auto. unfold max_hp. simpl. lia.
  - destruct s; exact P.
  - destruct s; exact P.
Qed.

Lemma book_simple: forall s so, book (update_simple s so) = book s.
Proof.
  intros s so. destruct so as [d|q| | |i q|i q|sls|c| | |f|f]; cbn [update_simple]; try reflexivity.
  - destruct (q =? 0); [reflexivity|]. destruct i; reflexivity.
  - destruct c, (s_check Hunting (disciplines s)); try reflexivity;
    repeat match goal with |- context [if ?b then _ else _] => destruct b end; reflexivity.
Qed.

Lemma book_apply: forall sos s, book (apply_effects s sos) = book s.
Proof.
  induction sos as [|so q IH]; intros s; [reflexivity|]. simpl. rewrite IH. apply book_simple.
Qed.

(* Getting an item that the rules allow *)
Lemma weak_gain: forall s i, Weak s -> CanGain (book s) (items s) i -> Weak (update_simple s (GainItem i 1)).
Proof.
  intros s i H C. cbn [update_simple]. apply weak_add; auto. apply ok_can_gain; auto. apply (w_items _ H).
Qed.

Lemma items_lose: forall s i, i <> Backpack -> items (update_simple s (LoseItem i 1)) = rm_item i 1 (items s).
Proof.
  intros s i N. cbn [update_simple]. simpl (1 =? 0). rewrite rm_backpack_other by exact N. destruct s; reflexivity.
Qed.

Lemma slots_pos: forall bk i, 1 <= backpack_slots bk i.
Proof.
  intros bk i. unfold backpack_slots. destruct i; try lia. destruct s; try lia; destruct (bk =? 4), (bk =? 3); lia.
Qed.

Lemma held_eq: forall sl s, held_items sl s = held_of sl (items s).
Proof.
  reflexivity.
Qed.

Lemma held_one: forall s, Weak s -> forall iq, In iq (held_of SWeapon (items s)) -> snd iq = 1.
Proof.
  intros s H iq I. destruct (held_in _ _ _ I) as [SL [Q P]].
  destruct (ok_single _ _ (w_items _ H) (fst iq)) as [A|A].
  - destruct (fst iq); try discriminate.
  - lia.
Qed.

Lemma used_sum: forall s, ValidMap (items s) ->
  list_sum (map (fun iq : item * nat => backpack_slots (book s) (fst iq) * snd iq) (held_items SBackpack s))
    = esum (used_g (book s)) (items s).
Proof.
  intros s V. rewrite held_eq. apply (held_sum (backpack_slots (book s)) SBackpack (items s) V).
Qed.

(* Dropping something from a slot makes room in it *)
Lemma can_gain_rm: forall bk m l i q, ItemsOk bk m -> ValidMap m -> item_slot i = SBackpack -> l <> Backpack ->
  esum (used_g bk) (rm_item l q m) + backpack_slots bk i <= 8 -> 0 < qty Backpack m ->
  CanGain bk (rm_item l q m) i.
Proof.
  intros bk m l q i H V SL N R B. unfold CanGain. rewrite SL. split; auto.
  rewrite qty_rm_diff; auto.
Qed.

Lemma helmet_gone: forall bk m l, ItemsOk bk m -> stackable l = false -> qty l (rm_item l 1 m) = 0.
Proof.
  intros bk m l H S. rewrite qty_rm_same by exact (ok_map _ _ H).
  destruct (ok_single _ _ H l) as [A|A]; [rewrite S in A; discriminate|lia].
Qed.

Lemma weapon_count: forall s, Weak s -> length (held_items SWeapon s) = esum (count_g SWeapon) (items s).
Proof.
  intros s H. rewrite held_eq. apply held_length; [apply (ok_map _ _ (w_items _ H))|apply held_one; exact H].
Qed.

Lemma rm_strict: forall g m l, ValidMap m -> 0 < qty l m -> esum g (rm_item l 1 m) + g l <= esum g m.
Proof.
  intros g m l V P. apply esum_strict.
  - intro j. apply qty_rm_le. exact V.
  - rewrite qty_rm_same by exact V. lia.
Qed.

Lemma take_space_can: forall s i, Weak s -> tcan_take i s = SpaceAvailable -> CanGain (book s) (items s) i.
Proof.
  intros s i H T. pose proof (ok_map _ _ (w_items _ H)) as V. unfold CanGain, tcan_take in *.
  destruct (item_slot i) eqn: SL.
  - destruct (length (held_items SWeapon s) <? 2) eqn: L; [|discriminate].
    apply Nat.ltb_lt in L. rewrite weapon_count in L by exact H. lia.
  - destruct (negb (has_item Backpack s)) eqn: B; [discriminate|].
    rewrite used_sum in T by exact V.
    destruct (Nat.leb _ 8) eqn: L; [|discriminate]. apply Nat.leb_le in L. split; [exact L|].
    rewrite has_item_qty in B. destruct (Nat.ltb 0 (qty Backpack (items s))) eqn: E; [apply Nat.ltb_lt; exact E|discriminate].
  - destruct (has_item i s); [discriminate|].
    split; intro E; subst i.
    + destruct (has_item SilverHelmet s) eqn: X; [discriminate|].
      rewrite has_item_qty in X. destruct (qty SilverHelmet (items s)); [reflexivity|discriminate].
    + destruct (has_item Helmet s) eqn: X; [discriminate|].
      rewrite has_item_qty in X. destruct (qty Helmet (items s)); [reflexivity|discriminate].
  - exact I.
Qed.

(* What can be dropped to make room *)
Lemma take_drop_can: forall s i lst l, Weak s -> tcan_take i s = MustDrop lst -> In l lst ->
  0 < qty l (items s) /\ l <> Backpack /\
  (backpack_slots (book s) i = 1 -> CanGain (book s) (rm_item l 1 (items s)) i).
Proof.
  intros s i lst l H T I. pose proof (w_items _ H) as K. pose proof (ok_map _ _ K) as V. unfold CanGain, tcan_take in *.
  destruct (item_slot i) eqn: SL.
  - destruct (length (held_items SWeapon s) <? 2); [discriminate|]. inversion T; subst lst. clear T.
    rewrite held_eq in I. apply in_map_iff in I. destruct I as [iq [E J]]. subst l.
    destruct (held_in _ _ _ J) as [S2 [Q P]]. split; [lia|]. split; [intro C; rewrite C in S2; discriminate|].
    intros _. pose proof (rm_strict (count_g SWeapon) (items s) (fst iq) V ltac:(lia)) as X.
    assert (count_g SWeapon (fst iq) = 1) as G by (unfold count_g; rewrite S2; reflexivity).
    pose proof (ok_weapons _ _ K). lia.
  - destruct (negb (has_item Backpack s)) eqn: B; [discriminate|].
    destruct (Nat.leb _ 8); [discriminate|]. inversion T; subst lst. clear T.
    rewrite held_eq in I. apply in_map_iff in I. destruct I as [iq [E J]]. subst l.
    destruct (held_in _ _ _ J) as [S2 [Q P]].
    assert (fst iq <> Backpack) as NB by (intro C; rewrite C in S2; discriminate).
    split; [lia|]. split; [exact NB|].
    intros One. pose proof (rm_strict (used_g (book s)) (items s) (fst iq) V ltac:(lia)) as X.
    assert (used_g (book s) (fst iq) = backpack_slots (book s) (fst iq)) as G by (unfold used_g; rewrite S2; reflexivity).
    pose proof (slots_pos (book s) (fst iq)). pose proof (ok_backpack _ _ K). split; [lia|].
    rewrite qty_rm_diff by (auto; discriminate).
    rewrite has_item_qty in B. destruct (Nat.ltb 0 (qty Backpack (items s))) eqn: E; [apply Nat.ltb_lt; exact E|discriminate].
  - destruct (has_item i s); [discriminate|].
    destruct (order.eqb i SilverHelmet && has_item Helmet s) eqn: A.
    { inversion T; subst lst. clear T. apply andb_true_iff in A. destruct A as [A1 A2]. apply eqb_correct in A1. subst i.
      destruct I as [I|[]]. subst l. rewrite has_item_qty in A2. apply Nat.ltb_lt in A2. split; [exact A2|]. split; [discriminate|].
      intros _. split; [discriminate|]. intros _. apply (helmet_gone (book s)); auto. }
    destruct (order.eqb i Helmet && has_item SilverHelmet s) eqn: A3; [|discriminate].
    inversion T; subst lst. clear T. apply andb_true_iff in A3. destruct A3 as [A1 A2]. apply eqb_correct in A1. subst i.
    destruct I as [I|[]]. subst l. rewrite has_item_qty in A2. apply Nat.ltb_lt in A2. split; [exact A2|]. split; [discriminate|].
    intros _. split; [|discriminate]. intros _. apply (helmet_gone (book s)); auto.
  - discriminate.
Qed.

(* The state after dropping and getting an item *)
Lemma weak_drop_gain: forall s i l, Weak s -> l <> Backpack -> CanGain (book s) (rm_item l 1 (items s)) i ->
  Weak (update_simple (update_simple s (LoseItem l 1)) (GainItem i 1)).
Proof.
  intros s i l H N C. apply weak_gain; [apply weak_lose; exact H|]. rewrite book_simple, items_lose by exact N. exact C.
Qed.

Lemma rm_gold_gain: forall bk m i q, ItemsOk bk m -> CanGain bk m i -> CanGain bk (rm_item Gold q m) i.
Proof.
  intros bk m i q K C. pose proof (ok_map _ _ K) as V. unfold CanGain in *.
  assert (forall g, esum g (rm_item Gold q m) <= esum g m) as E by (intro g; apply esum_mono; intro j; apply qty_rm_le; exact V).
  destruct (item_slot i).
  - pose proof (E (count_g SWeapon)). lia.
  - destruct C as [C1 C2]. pose proof (E (used_g bk)). split; [lia|]. rewrite qty_rm_diff by (auto; discriminate). exact C2.
  - destruct C as [C1 C2]. split; intro X; [specialize (C1 X)|specialize (C2 X)]; rewrite qty_rm_diff by (auto; discriminate); assumption.
  - exact I.
Qed.

Lemma lose_gold_gain: forall s i p, Weak s -> CanGain (book s) (items s) i ->
  CanGain (book s) (items (update_simple s (LoseItem Gold p))) i.
Proof.
  intros s i p H C. cbn [update_simple]. destruct (p =? 0); [exact C|].
  replace (items (clamp_endurance (rm_backpack_content Gold (update_items (rm_item Gold p) s)))) with (rm_item Gold p (items s)) by (destruct s; reflexivity).
  apply rm_gold_gain; auto. apply (w_items _ H).
Qed.
