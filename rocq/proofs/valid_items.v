(* What keeps the items valid. All the limits are sums over the items, and comparing sums is easier over the list of all the
   items, as it doesn't depend on what is held. Everything here then comes down to the quantity of each item. *)
Require Import character chapters mps order lists.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
From Stdlib Require Import Lia.
Open Scope nat_scope.

Definition qty (i: item) (m: Items) : nat := match lookup i m with Some x => x | None => 0 end.

Lemma has_qty: forall i m, has i m = Nat.ltb 0 (qty i m).
Proof.
  intros i m. unfold has, qty. destruct (lookup i m) as [[|x]|]; reflexivity.
Qed.

Definition wsum (g: item -> nat) (m: Items) : nat := list_sum (map (fun kv => g (fst kv) * snd kv) m).

Definition lsum (g: item -> nat) (m: Items) (l: list item) : nat := list_sum (map (fun i => g i * qty i m) l).

Lemma lsum_app: forall g m a b, lsum g m (a ++ b) = lsum g m a + lsum g m b.
Proof.
  intros. unfold lsum. rewrite map_app, list_sum_app. reflexivity.
Qed.

Lemma lsum_ext: forall g m n l, (forall i, In i l -> qty i m = qty i n) -> lsum g m l = lsum g n l.
Proof.
  intros g m n l H. induction l as [|x l IH]; [reflexivity|]. unfold lsum in *. simpl.
  rewrite (H x) by (left; reflexivity). rewrite IH; auto. intros. apply H. right. assumption.
Qed.

Lemma qty_cons_same: forall k v t, qty k ((k, v) :: t) = v.
Proof.
  intros. unfold qty. cbn [lookup]. rewrite cmp_refl. reflexivity.
Qed.

Lemma qty_cons_diff: forall i k v t, ValidMap ((k, v) :: t) -> i <> k -> qty i ((k, v) :: t) = qty i t.
Proof.
  intros i k v t V D. unfold qty. cbn [lookup].
  destruct (cmp i k) eqn: C; auto.
  - apply Valid.head in V. destruct V as [VT F].
    rewrite Lookup.lowkey_not_found; auto.
    eapply Forall_impl; [|exact F]. intros [ck cv] L. simpl in *.
    exact (@lt_trans item item_EqDec item_OrdDec i k ck C L).
  - apply cmp_eq in C. contradiction.
Qed.

(* A sum over the map is a sum over any list that has its keys once *)
Lemma wsum_lsum: forall g m l, ValidMap m -> NoDup l -> (forall kv, In kv m -> In (fst kv) l) -> wsum g m = lsum g m l.
Proof.
  intros g m. induction m as [|[k v] t IH]; intros l V ND K.
  - unfold lsum, wsum, qty. simpl. induction l as [|x l IHl]; [reflexivity|].
    simpl. rewrite Nat.mul_0_r. simpl. apply IHl.
    + inversion ND; assumption.
    + intros kv I. destruct I.
  - destruct (in_split k l) as [a [b E]]; [apply (K (k, v)); left; reflexivity|]. subst l.
    pose proof (NoDup_remove_1 _ _ _ ND) as ND2. pose proof (NoDup_remove_2 _ _ _ ND) as NI.
    apply Valid.head in V as VH. destruct VH as [VT F].
    assert (forall i, In i (a ++ b) -> qty i ((k, v) :: t) = qty i t) as Q.
    { intros i I. apply qty_cons_diff; auto. intro C. subst. contradiction. }
    assert (wsum g t = lsum g t (a ++ b)) as IH2.
    { apply IH; auto. intros [x y] I. simpl.
      assert (x <> k) as XK.
      { intro C. subst. rewrite Forall_forall in F. specialize (F _ I). simpl in F. exact (@lt_not item item_EqDec item_OrdDec k F). }
      specialize (K (x, y) (or_intror I)). simpl in K. apply in_app_or in K. apply in_or_app.
      destruct K as [K|[K|K]]; auto. subst. contradiction. }
    assert (lsum g ((k, v) :: t) (a ++ k :: b) = g k * qty k ((k, v) :: t) + lsum g ((k, v) :: t) a + lsum g ((k, v) :: t) b) as E.
    { rewrite lsum_app. unfold lsum. simpl. lia. }
    rewrite E, qty_cons_same, (lsum_ext g _ t a), (lsum_ext g _ t b).
    + rewrite lsum_app in IH2. unfold wsum in *. simpl. lia.
    + intros. apply Q. apply in_or_app. auto.
    + intros. apply Q. apply in_or_app. auto.
Qed.

Lemma nodup_all_items: NoDup all_items.
Proof.
  unfold all_items. simpl. repeat constructor; intro C; repeat destruct C as [C|C]; try discriminate; inversion C.
Qed.

Definition esum (g: item -> nat) (m: Items) : nat := lsum g m all_items.

Lemma wsum_esum: forall g m, ValidMap m -> wsum g m = esum g m.
Proof.
  intros. apply wsum_lsum; auto using nodup_all_items. intros. apply all_items_complete.
Qed.

Lemma lsum_mono: forall g m n l, (forall i, In i l -> qty i m <= qty i n) -> lsum g m l <= lsum g n l.
Proof.
  intros g m n l H. induction l as [|x l IH]; [apply le_n|]. unfold lsum in *. simpl.
  specialize (H x (or_introl Logic.eq_refl)) as Hx.
  assert (g x * qty x m <= g x * qty x n) by (apply Nat.mul_le_mono_l; exact Hx).
  assert (list_sum (map (fun i => g i * qty i m) l) <= list_sum (map (fun i => g i * qty i n) l)).
  { apply IH. intros. apply H. right. assumption. }
  lia.
Qed.

Lemma esum_mono: forall g m n, (forall i, qty i m <= qty i n) -> esum g m <= esum g n.
Proof.
  intros. apply lsum_mono. auto.
Qed.

Lemma lsum_split: forall (g: item -> nat) (l: list item) (k: item), NoDup l -> In k l ->
  exists rest, ~ In k rest /\ (forall i, In i l -> i = k \/ In i rest) /\ (forall i, In i rest -> In i l)
    /\ forall n, lsum g n l = g k * qty k n + lsum g n rest.
Proof.
  intros g l k ND I. destruct (in_split k l) as [a [b E]]; [exact I|]. subst l.
  exists (a ++ b). pose proof (NoDup_remove_2 _ _ _ ND) as NI. split; [exact NI|]. split; [|split].
  - intros i J. apply in_app_or in J. destruct J as [J|[J|J]]; auto using in_or_app.
  - intros i J. apply in_app_or in J. apply in_or_app. destruct J; auto. right. right. assumption.
  - intro n. rewrite !lsum_app. unfold lsum. simpl. lia.
Qed.

(* One item can grow, by at most d *)
Lemma esum_add: forall g m n k d, (forall i, i <> k -> qty i n <= qty i m) -> qty k n <= qty k m + d ->
  esum g n <= esum g m + g k * d.
Proof.
  intros g m n k d H Hk. unfold esum.
  destruct (lsum_split g all_items k nodup_all_items (all_items_complete k)) as [r [NI [_ [_ E]]]].
  rewrite !E.
  assert (lsum g n r <= lsum g m r) by (apply lsum_mono; intros i I; apply H; intro C; subst; contradiction).
  assert (g k * qty k n <= g k * qty k m + g k * d) by (rewrite <- Nat.mul_add_distr_l; apply Nat.mul_le_mono_l; exact Hk).
  lia.
Qed.

(* One item goes down *)
Lemma esum_strict: forall g m n k, (forall i, qty i n <= qty i m) -> qty k n + 1 <= qty k m ->
  esum g n + g k <= esum g m.
Proof.
  intros g m n k H Hk. unfold esum.
  destruct (lsum_split g all_items k nodup_all_items (all_items_complete k)) as [r [NI [_ [_ E]]]].
  rewrite !E.
  assert (lsum g n r <= lsum g m r) by (apply lsum_mono; intros i I; apply H).
  assert (g k * (qty k n + 1) <= g k * qty k m) by (apply Nat.mul_le_mono_l; exact Hk).
  rewrite Nat.mul_add_distr_l in *. lia.
Qed.

(* The limits as sums *)
Definition used_g (bk: nat) (i: item) : nat :=
  if order.eqb (item_slot i) SBackpack then backpack_slots bk i else 0.

Definition count_g (sl: slot) (i: item) : nat := if order.eqb (item_slot i) sl then 1 else 0.

Lemma sum_filter_slot: forall (h: item -> nat) (sl: slot) m,
  list_sum (map (fun kv: item * nat => h (fst kv) * snd kv) (filter (fun kv: item * nat => order.eqb (item_slot (fst kv)) sl) m))
    = wsum (fun i => if order.eqb (item_slot i) sl then h i else 0) m.
Proof.
  intros h sl m. induction m as [|[k v] t IH]; [reflexivity|]. unfold wsum in *. simpl.
  destruct (order.eqb (item_slot k) sl); simpl; rewrite IH; reflexivity.
Qed.

Lemma used_esum: forall bk m, ValidMap m -> backpack_used bk m = esum (used_g bk) m.
Proof.
  intros bk m V. rewrite <- wsum_esum by exact V. unfold backpack_used. rewrite sum_filter_slot. reflexivity.
Qed.

Lemma count_esum: forall sl m, ValidMap m -> items_slot_count m sl = esum (count_g sl) m.
Proof.
  intros sl m V. rewrite <- wsum_esum by exact V. unfold items_slot_count, wsum, count_g.
  induction m as [|[k v] t IH]; [reflexivity|]. simpl.
  apply Valid.head in V. destruct V as [VT _].
  destruct (order.eqb (item_slot k) sl); simpl; rewrite IH; auto; lia.
Qed.

Lemma single_qty: forall m, ValidMap m ->
  (forallb (fun kv: item * nat => stackable (fst kv) || Nat.leb (snd kv) 1) m = true <->
   forall k, stackable k = true \/ qty k m <= 1).
Proof.
  intros m V. rewrite forallb_forall. split.
  - intros H k. unfold qty. destruct (lookup k m) eqn: L; [|right; lia].
    apply vmap_in in L; auto. specialize (H _ L). simpl in H. apply orb_true_iff in H. destruct H; [left; auto|right].
    apply Nat.leb_le. assumption.
  - intros H [k v] I. simpl. apply vmap_in in I; auto. destruct (H k) as [S|Q]; [rewrite S; reflexivity|].
    unfold qty in Q. rewrite I in Q. apply Nat.leb_le in Q. rewrite Q. apply orb_true_r.
Qed.

(* The limits, as what is used by the proofs *)
Record ItemsOk (bk: nat) (m: Items) : Prop := {
  ok_bag: ValidBag m;
  ok_gold: qty Gold m <= 50;
  ok_backpack: esum (used_g bk) m <= 8;
  ok_weapons: esum (count_g SWeapon) m <= 2;
  ok_single: forall k, stackable k = true \/ qty k m <= 1;
  ok_helmets: qty Helmet m = 0 \/ qty SilverHelmet m = 0;
  ok_contents: 0 < qty Backpack m \/ esum (count_g SBackpack) m = 0;
}.

Lemma items_ok_iff: forall bk m, ValidItems bk m <-> ItemsOk bk m.
Proof.
  intros bk m. split.
  - intros [B G U W S H C].
    assert (ValidMap m) as V by (destruct B; assumption).
    constructor; auto.
    + rewrite <- used_esum; auto.
    + rewrite <- count_esum; auto.
    + apply single_qty; auto.
    + rewrite !has_qty in H. destruct (qty Helmet m) as [|a]; [left; reflexivity|].
      destruct (qty SilverHelmet m) as [|b]; [right; reflexivity|]. discriminate.
    + rewrite has_qty in C. rewrite count_esum in C by exact V. apply orb_true_iff in C. destruct C as [C|C].
      * left. apply Nat.ltb_lt. exact C.
      * right. apply Nat.eqb_eq. exact C.
  - intros [B G U W S H C].
    assert (ValidMap m) as V by (destruct B; assumption).
    constructor; auto.
    + rewrite used_esum; auto.
    + rewrite count_esum; auto.
    + apply single_qty; auto.
    + rewrite !has_qty. destruct H as [H|H]; rewrite H; [reflexivity|]. rewrite andb_false_r. reflexivity.
    + rewrite has_qty, count_esum by exact V. apply orb_true_iff. destruct C as [C|C].
      * left. apply Nat.ltb_lt. exact C.
      * right. apply Nat.eqb_eq. exact C.
Qed.

Lemma ok_map: forall bk m, ItemsOk bk m -> ValidMap m.
Proof.
  intros bk m H. destruct (ok_bag _ _ H). assumption.
Qed.

Lemma esum_zero: forall g m, (forall i, g i = 0 \/ qty i m = 0) -> esum g m = 0.
Proof.
  intros g m H. unfold esum, lsum. induction all_items as [|x l IH]; [reflexivity|]. simpl.
  rewrite IH. destruct (H x) as [Z|Z]; rewrite Z; lia.
Qed.

Lemma ok_empty: forall bk, ItemsOk bk empty.
Proof.
  intro bk. constructor.
  - split; constructor.
  - apply le_0_n.
  - rewrite esum_zero; [lia|right; reflexivity].
  - rewrite esum_zero; [lia|right; reflexivity].
  - right. apply le_0_n.
  - left; reflexivity.
  - right. apply esum_zero. right; reflexivity.
Qed.

(* Quantities after each operation *)
Lemma qty_add_diff: forall i q m j, ValidMap m -> j <> i -> qty j (add_item i q m) = qty j m.
Proof.
  intros i q m j V D. destruct q; [reflexivity|].
  destruct (add_item_form i (S q)) as [f [v [_ [_ E]]]]; [lia|].
  unfold qty. rewrite E, Lookup.lookup_insert_with_diff; auto.
Qed.

Lemma qty_add_one: forall i m, ValidMap m -> qty i (add_item i 1 m) <= qty i m + 1.
Proof.
  intros i m V.
  destruct (eq_dec i Gold) as [->|G].
  { rewrite add_item_gold by lia. unfold qty. rewrite Lookup.lookup_insert_with; auto.
    destruct (lookup Gold m) as [x|]; [|simpl; lia]. destruct (x + 1 <? 51) eqn: E; [lia|]. apply Nat.ltb_ge in E. lia. }
  destruct (eq_dec i Meal) as [->|M].
  { rewrite add_item_stack by (auto; lia). unfold qty, bag_add. rewrite Lookup.lookup_insert_with; auto.
    destruct (lookup Meal m); lia. }
  destruct (eq_dec i Laumspur) as [->|L].
  { rewrite add_item_stack by (auto; lia). unfold qty, bag_add. rewrite Lookup.lookup_insert_with; auto.
    destruct (lookup Laumspur m); lia. }
  rewrite add_item_single by (auto; lia). unfold qty, insert. rewrite Lookup.lookup_insert_with; auto.
  destruct (lookup i m); lia.
Qed.

Lemma qty_add_single: forall i q m, ValidMap m -> stackable i = false -> 0 < q -> qty i (add_item i q m) = 1.
Proof.
  intros i q m V S Q. rewrite add_item_single; try lia; try (intro C; subst; discriminate).
  unfold qty, insert. rewrite Lookup.lookup_insert_with; auto. destruct (lookup i m); reflexivity.
Qed.

Lemma qty_add_gold: forall q m, ValidMap m -> qty Gold m <= 50 -> qty Gold (add_item Gold q m) <= 50.
Proof.
  intros q m V H. destruct q as [|q]; [exact H|].
  rewrite add_item_gold by lia. unfold qty in *. rewrite Lookup.lookup_insert_with; auto.
  destruct (lookup Gold m) as [x|]; [destruct (x + S q <? 51) eqn: E; [apply Nat.ltb_lt in E; lia|lia]|lia].
Qed.

Lemma qty_rm_diff: forall i q m j, ValidMap m -> j <> i -> qty j (rm_item i q m) = qty j m.
Proof.
  intros i q m j V D. unfold qty, rm_item, bag_rm. rewrite Lookup.update_diff; auto.
Qed.

Lemma qty_rm_same: forall i q m, ValidMap m -> qty i (rm_item i q m) = qty i m - q.
Proof.
  intros i q m V. unfold qty, rm_item, bag_rm. rewrite Lookup.update_eq_b; auto.
  destruct (lookup i m) as [x|]; [|reflexivity]. destruct (x <=? q) eqn: E; [apply Nat.leb_le in E; lia|reflexivity].
Qed.

Lemma qty_rm_le: forall i q m j, ValidMap m -> qty j (rm_item i q m) <= qty j m.
Proof.
  intros i q m j V. destruct (eq_dec j i) as [->|D].
  - rewrite qty_rm_same by exact V. lia.
  - rewrite qty_rm_diff by assumption. lia.
Qed.

Lemma rm_map: forall i q m, ValidMap m -> ValidMap (rm_item i q m).
Proof.
  intros. unfold rm_item, bag_rm. apply Update.valid. assumption.
Qed.

Lemma lose_all_valid: forall sl m, ValidBag m -> ValidBag (lose_all_slot sl m).
Proof.
  intros sl m V. unfold lose_all_slot. induction (slot_items sl) as [|x l IH]; simpl; [exact V|].
  destruct IH as [V1 F]. split; [apply Delete.valid; assumption|].
  rewrite Forall_forall in *. intros [k v] I. apply (vmap_in _ k v) in I; [|apply Delete.valid; assumption].
  assert (lookup k (fold_right (fun i itms => delete i itms) m l) = Some v) as L.
  { destruct (eq_dec k x) as [->|D].
    - rewrite Lookup.lookup_delete in I; [discriminate|assumption].
    - rewrite Lookup.lookup_delete_diff in I; auto. }
  apply vmap_in in L; auto.
Qed.

Lemma qty_lose_all: forall sl m j, ValidMap m ->
  qty j (lose_all_slot sl m) = if order.eqb (item_slot j) sl then 0 else qty j m.
Proof.
  intros sl m j V. unfold lose_all_slot.
  assert (ValidMap (fold_right (fun i itms => delete i itms) m (slot_items sl))) as VF.
  { induction (slot_items sl) as [|x l IH]; simpl; [exact V|]. apply Delete.valid. exact IH. }
  destruct (order.eqb (item_slot j) sl) eqn: E.
  - apply eqb_correct in E. subst sl.
    pose proof (item_in_slot_items j) as I. revert I VF.
    induction (slot_items (item_slot j)) as [|x l IH]; intros I VF; [destruct I|]. simpl in *.
    assert (ValidMap (fold_right (fun i itms => delete i itms) m l)) as VL.
    { clear -V. induction l as [|y l IH]; simpl; [exact V|]. apply Delete.valid. exact IH. }
    unfold qty. destruct (eq_dec j x) as [->|D].
    + rewrite Lookup.lookup_delete; auto.
    + rewrite Lookup.lookup_delete_diff; auto. destruct I as [I|I]; [subst; contradiction|]. apply IH; auto.
  - assert (forall x, In x (slot_items sl) -> j <> x) as NE.
    { intros x I C. subst. apply Slot.in_slot_items_slot in I. subst.  rewrite order.eqb_refl in E. discriminate. }
    revert NE VF. induction (slot_items sl) as [|x l IH]; intros NE VF; [reflexivity|]. simpl in *.
    assert (ValidMap (fold_right (fun i itms => delete i itms) m l)) as VL.
    { clear -V. induction l as [|y l IH]; simpl; [exact V|]. apply Delete.valid. exact IH. }
    unfold qty in *. rewrite Lookup.lookup_delete_diff; auto.
Qed.

Lemma lose_all_le: forall sl m j, ValidMap m -> qty j (lose_all_slot sl m) <= qty j m.
Proof.
  intros. rewrite qty_lose_all by assumption. destruct (order.eqb _ _); lia.
Qed.

(* Anything that has less of each item, apart from the Backpack that keeps its content *)
Lemma ok_shrink: forall bk m n, ItemsOk bk m -> ValidBag n -> (forall i, qty i n <= qty i m) ->
  (0 < qty Backpack n \/ esum (count_g SBackpack) n = 0) -> ItemsOk bk n.
Proof.
  intros bk m n [B G U W S H C] VB L K. constructor; auto.
  - specialize (L Gold). lia.
  - eapply Nat.le_trans; [apply esum_mono|exact U]. exact L.
  - eapply Nat.le_trans; [apply esum_mono|exact W]. exact L.
  - intro k. destruct (S k) as [A|A]; auto. right. specialize (L k). lia.
  - destruct H as [H|H]; [left|right]; [specialize (L Helmet)|specialize (L SilverHelmet)]; lia.
Qed.

Lemma ok_rm: forall bk m i q, i <> Backpack -> ItemsOk bk m -> ItemsOk bk (rm_item i q m).
Proof.
  intros bk m i q NB H. pose proof (ok_map _ _ H) as V.
  apply (ok_shrink bk m); auto.
  - apply rm_valid, (ok_bag _ _ H).
  - intro j. apply qty_rm_le. exact V.
  - destruct (ok_contents _ _ H) as [C|C].
    + left. rewrite qty_rm_diff; auto.
    + right. pose proof (esum_mono (count_g SBackpack) (rm_item i q m) m (fun j => qty_rm_le i q m j V)) as E. lia.
Qed.

Lemma ok_rm_backpack: forall bk m q, ItemsOk bk m -> ItemsOk bk (lose_all_slot SBackpack (rm_item Backpack q m)).
Proof.
  intros bk m q H. pose proof (ok_map _ _ H) as V. pose proof (rm_map Backpack q m V) as VR.
  apply (ok_shrink bk m); auto.
  - apply lose_all_valid, rm_valid, (ok_bag _ _ H).
  - intro j. eapply Nat.le_trans; [apply lose_all_le; exact VR|apply qty_rm_le; exact V].
  - right. apply esum_zero. intro i. unfold count_g. destruct (order.eqb (item_slot i) SBackpack) eqn: E; [right|left; reflexivity].
    rewrite qty_lose_all, E; auto.
Qed.

Lemma lose_kinds_valid: forall sls m, ValidBag m -> ValidBag (fold_right lose_all_slot m sls).
Proof.
  intros sls m V. induction sls as [|s l IH]; simpl; [exact V|]. apply lose_all_valid. exact IH.
Qed.

Lemma lose_kinds_le: forall sls m j, ValidBag m -> qty j (fold_right lose_all_slot m sls) <= qty j m.
Proof.
  intros sls m j V. induction sls as [|s l IH]; simpl; [lia|].
  eapply Nat.le_trans; [apply lose_all_le|exact IH]. destruct (lose_kinds_valid l m V). assumption.
Qed.

Lemma lose_kinds_qty: forall sls m j, ValidBag m -> order.eqb (item_slot j) SSpecial = true ->
  existsb (fun s => match s with SSpecial => true | _ => false end) sls = false ->
  qty j (fold_right lose_all_slot m sls) = qty j m.
Proof.
  intros sls m j V J. induction sls as [|s l IH]; simpl; [reflexivity|]. intro E.
  destruct s; simpl in E; try discriminate;
    (rewrite qty_lose_all by (destruct (lose_kinds_valid l m V); assumption);
     apply eqb_correct in J; rewrite J; simpl; apply IH; auto).
Qed.

Lemma lose_kinds_empties: forall sls m j, ValidBag m -> In SBackpack sls -> item_slot j = SBackpack ->
  qty j (fold_right lose_all_slot m sls) = 0.
Proof.
  intros sls m j V. induction sls as [|s l IH]; intros I J; [destruct I|]. simpl.
  rewrite qty_lose_all by (destruct (lose_kinds_valid l m V); assumption).
  destruct I as [I|I].
  - subst. rewrite J. rewrite order.eqb_refl. reflexivity.
  - destruct (order.eqb (item_slot j) s); auto.
Qed.

Lemma ok_lose_kinds: forall bk m sls,
  existsb (fun s => match s with SSpecial => true | _ => false end) sls = false \/ In SBackpack sls ->
  ItemsOk bk m -> ItemsOk bk (fold_right lose_all_slot m sls).
Proof.
  intros bk m sls K H. pose proof (ok_bag _ _ H) as B.
  apply (ok_shrink bk m); auto.
  - apply lose_kinds_valid. exact B.
  - intro j. apply lose_kinds_le. exact B.
  - destruct K as [K|K].
    + destruct (ok_contents _ _ H) as [C|C].
      * left. rewrite lose_kinds_qty; auto.
      * right. pose proof (esum_mono (count_g SBackpack) (fold_right lose_all_slot m sls) m (fun j => lose_kinds_le sls m j B)). lia.
    + right. apply esum_zero. intro i. unfold count_g. destruct (order.eqb (item_slot i) SBackpack) eqn: E; [right|left; reflexivity].
      apply eqb_correct in E. apply lose_kinds_empties; auto.
Qed.

Lemma qty_add_gold_le: forall q m, ValidMap m -> qty Gold (add_item Gold q m) <= qty Gold m + q.
Proof.
  intros q m V. destruct q as [|q]; [change (add_item Gold 0 m) with m; lia|].
  rewrite add_item_gold by lia. unfold qty. rewrite Lookup.lookup_insert_with; auto.
  destruct (lookup Gold m) as [x|]; [|lia]. destruct (x + S q <? 51) eqn: E; [lia|]. apply Nat.ltb_ge in E. lia.
Qed.

Lemma ok_gain_gold: forall bk m q, ItemsOk bk m -> ItemsOk bk (add_item Gold q m).
Proof.
  intros bk m q H. destruct q as [|q]; [exact H|]. pose proof (ok_map _ _ H) as V.
  assert (forall g, g Gold = 0 -> esum g (add_item Gold (S q) m) <= esum g m) as E.
  { intros g G. pose proof (esum_add g m (add_item Gold (S q) m) Gold (S q)) as X.
    rewrite G, Nat.mul_0_l, Nat.add_0_r in X. apply X.
    - intros i D. rewrite qty_add_diff; auto.
    - apply qty_add_gold_le. exact V. }
  destruct H as [B G U W S Hm C]. constructor.
  - apply add_item_valid_bag; [lia|exact B].
  - apply qty_add_gold; auto.
  - eapply Nat.le_trans; [apply E; reflexivity|exact U].
  - eapply Nat.le_trans; [apply E; reflexivity|exact W].
  - intro k. destruct (eq_dec k Gold) as [->|D]; [left; reflexivity|]. rewrite qty_add_diff; auto.
  - rewrite !qty_add_diff; auto; discriminate.
  - destruct C as [C|C]; [left|right].
    + rewrite qty_add_diff; auto; discriminate.
    + pose proof (E (count_g SBackpack) Logic.eq_refl). lia.
Qed.

(* Adding an item that is not gold, when all it needs is there *)
Lemma ok_add: forall bk m i q d, ItemsOk bk m -> 0 < q -> i <> Gold ->
  qty i (add_item i q m) <= qty i m + d ->
  esum (used_g bk) m + used_g bk i * d <= 8 ->
  esum (count_g SWeapon) m + count_g SWeapon i * d <= 2 ->
  (i = Helmet -> qty SilverHelmet m = 0) -> (i = SilverHelmet -> qty Helmet m = 0) ->
  (0 < qty Backpack m \/ count_g SBackpack i = 0) ->
  ItemsOk bk (add_item i q m).
Proof.
  intros bk m i q d H Q NG Qi U1 W1 H1 H2 C1. pose proof (ok_map _ _ H) as V.
  assert (forall g, esum g (add_item i q m) <= esum g m + g i * d) as E.
  { intro g. apply esum_add; auto. intros j D. rewrite qty_add_diff; auto. }
  destruct H as [B G U W S Hm C]. constructor.
  - apply add_item_valid_bag; [lia|exact B].
  - rewrite qty_add_diff; auto.
  - eapply Nat.le_trans; [apply E|exact U1].
  - eapply Nat.le_trans; [apply E|exact W1].
  - intro k. destruct (eq_dec k i) as [->|D].
    + destruct (stackable i) eqn: ST; [left; reflexivity|right]. rewrite qty_add_single; auto.
    + rewrite qty_add_diff; auto.
  - destruct (eq_dec i Helmet) as [->|D1].
    + right. rewrite qty_add_diff; auto; try discriminate.
    + destruct (eq_dec i SilverHelmet) as [->|D2].
      * left. rewrite qty_add_diff; auto; try discriminate.
      * rewrite !qty_add_diff; auto.
  - destruct (eq_dec i Backpack) as [->|D].
    + left. rewrite qty_add_single; auto.
    + destruct C1 as [C1|C1].
      * left. rewrite qty_add_diff; auto.
      * destruct C as [C|C]; [left; rewrite qty_add_diff; auto|right].
        pose proof (E (count_g SBackpack)) as X. rewrite C1, C in X. simpl in X. lia.
Qed.

Lemma ok_gain_special: forall bk m i q, ItemsOk bk m -> 0 < q -> item_slot i = SSpecial ->
  (i = Helmet -> qty SilverHelmet m = 0) -> (i = SilverHelmet -> qty Helmet m = 0) ->
  ItemsOk bk (add_item i q m).
Proof.
  intros bk m i q H Q SL H1 H2. pose proof (ok_map _ _ H) as V.
  assert (i <> Gold) by (intro C; subst; discriminate).
  assert (stackable i = false) as ST by (destruct i; auto; discriminate).
  assert (used_g bk i = 0) as G1 by (unfold used_g; rewrite SL; reflexivity).
  assert (count_g SWeapon i = 0) as G2 by (unfold count_g; rewrite SL; reflexivity).
  assert (count_g SBackpack i = 0) as G3 by (unfold count_g; rewrite SL; reflexivity).
  apply (ok_add bk m i q 1); auto.
  - rewrite qty_add_single; auto. lia.
  - rewrite G1. pose proof (ok_backpack _ _ H). lia.
  - rewrite G2. pose proof (ok_weapons _ _ H). lia.
Qed.

Lemma ok_gain_weapon: forall bk m i, ItemsOk bk m -> item_slot i = SWeapon ->
  esum (count_g SWeapon) m + 1 <= 2 -> ItemsOk bk (add_item i 1 m).
Proof.
  intros bk m i H SL R. pose proof (ok_map _ _ H) as V.
  assert (i <> Gold) by (intro C; subst; discriminate).
  assert (used_g bk i = 0) as G1 by (unfold used_g; rewrite SL; reflexivity).
  assert (count_g SWeapon i = 1) as G2 by (unfold count_g; rewrite SL; reflexivity).
  assert (count_g SBackpack i = 0) as G3 by (unfold count_g; rewrite SL; reflexivity).
  apply (ok_add bk m i 1 1); auto.
  - apply qty_add_one. exact V.
  - rewrite G1. pose proof (ok_backpack _ _ H). lia.
  - rewrite G2. lia.
  - intro C. subst. discriminate.
  - intro C. subst. discriminate.
Qed.

Lemma ok_gain_backpack: forall bk m i, ItemsOk bk m -> item_slot i = SBackpack ->
  esum (used_g bk) m + backpack_slots bk i <= 8 -> 0 < qty Backpack m -> ItemsOk bk (add_item i 1 m).
Proof.
  intros bk m i H SL R HB. pose proof (ok_map _ _ H) as V.
  assert (i <> Gold) by (intro C; subst; discriminate).
  assert (used_g bk i = backpack_slots bk i) as G1 by (unfold used_g; rewrite SL; reflexivity).
  assert (count_g SWeapon i = 0) as G2 by (unfold count_g; rewrite SL; reflexivity).
  apply (ok_add bk m i 1 1); auto.
  - apply qty_add_one. exact V.
  - rewrite G1. lia.
  - rewrite G2. pose proof (ok_weapons _ _ H). lia.
  - intro C. subst. discriminate.
  - intro C. subst. discriminate.
Qed.

(* What the rules look at is the same as the sums *)
Definition held_of (sl: slot) (m: Items) : list (item * nat) :=
  filter (fun iq : item * nat => Nat.ltb 0 (snd iq)) (map (fun i => (i, qty i m)) (slot_items sl)).

Lemma nodup_slot: forall sl, NoDup (slot_items sl).
Proof. exact Slot.nodup_slot_items. Qed.

Lemma held_sum: forall (h: item -> nat) sl m, ValidMap m ->
  list_sum (map (fun iq : item * nat => h (fst iq) * snd iq) (held_of sl m))
    = esum (fun i => if order.eqb (item_slot i) sl then h i else 0) m.
Proof.
  intros h sl m V. rewrite <- wsum_esum by exact V. rewrite <- sum_filter_slot.
  set (fm := filter (fun kv: item * nat => order.eqb (item_slot (fst kv)) sl) m).
  assert (ValidMap fm) as VF by (apply Valid.filter; exact V).
  change (list_sum (map (fun kv: item * nat => h (fst kv) * snd kv) fm)) with (wsum h fm).
  rewrite (wsum_lsum h fm (slot_items sl) VF (nodup_slot sl)).
  2: { intros [k v] I. unfold fm in I. apply filter_In in I. destruct I as [_ I]. apply eqb_correct in I. simpl in *. subst. apply item_in_slot_items. }
  rewrite (lsum_ext h fm m).
  2: { intros i I. unfold qty, fm. rewrite Lookup.lookup_filter by exact V.
       apply Slot.in_slot_items_slot in I. destruct (lookup i m); [|reflexivity]. simpl. rewrite I, order.eqb_refl. reflexivity. }
  unfold held_of, lsum. induction (slot_items sl) as [|x l IH]; [reflexivity|]. simpl.
  destruct (qty x m) as [|y] eqn: Q; simpl; rewrite ?Nat.mul_0_r; simpl; rewrite IH; reflexivity.
Qed.

Lemma held_in: forall sl m iq, In iq (held_of sl m) -> item_slot (fst iq) = sl /\ snd iq = qty (fst iq) m /\ 0 < snd iq.
Proof.
  intros sl m iq I. unfold held_of in I. apply filter_In in I. destruct I as [I P]. apply in_map_iff in I.
  destruct I as [x [E J]]. subst iq. simpl in *. apply Nat.ltb_lt in P. split; [|split; auto].
  apply Slot.in_slot_items_slot. exact J.
Qed.

Lemma held_length: forall sl m, ValidMap m -> (forall iq, In iq (held_of sl m) -> snd iq = 1) ->
  length (held_of sl m) = esum (count_g sl) m.
Proof.
  intros sl m V H. unfold count_g. rewrite <- (held_sum (fun _ => 1) sl m V).
  induction (held_of sl m) as [|x l IH]; [reflexivity|]. cbn [length map list_sum]. rewrite IH.
  - cbv beta. rewrite (H x) by (left; reflexivity). simpl. lia.
  - intros. apply H. right. assumption.
Qed.
