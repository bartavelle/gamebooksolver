Require Import Stdlib.FSets.FMapList.
Require Import Stdlib.Structures.OrderedType.
From Equations Require Import Equations.
Require Import Stdlib.QArith.Qcanon.
Require Import Field.

Require Import Stdlib.Logic.FunctionalExtensionality.
Require Import Stdlib.Lists.List.
Import ListNotations.

Require Import mps.

Definition Proba K := Mp K Qc.

Definition certain {K: Set} (k: K) : Proba K := (k, 1) :: nil.

Definition SumProba {K: Set} (m: Proba K) := foldMap (fun (x y : Qc) => x + y) (0: Qc) m.

Definition FullProba {K: Set} (m: Proba K) : Prop := SumProba m = 1.

Lemma certain_full {K: Set}: forall (k: K), FullProba (certain k).
Proof.
intro k.
unfold FullProba, certain, SumProba.
unfold foldMap.
simpl.
reflexivity.
Qed.

Lemma sumproba_empty {K}: @SumProba K [] = 0.
Proof.
  unfold SumProba.
  unfold foldMap.
  simpl.
  reflexivity.
Qed.

Lemma sumproba_cons {K: Set}: forall p (k: K) m, SumProba ((k, p) :: m) = p + SumProba m.
Proof.
  intros.
  unfold SumProba.
  apply FoldMap.cons.
Qed.

Definition add_event {K: Set} `{OrdDec K} (m: Proba K) (ep: K * Qc) : Proba K :=
  let (e, p) := ep in
  insert_with (fun a => a + p) e p m.

Lemma add_event_adds_proba {K: Set} `{OrdDec K}: forall (m: Proba K) (p: Qc) (e: K),
  SumProba (add_event m (e, p)) = p + SumProba m.
Proof.
  unfold add_event.
  induction m; intros.
  * unfold SumProba. 
    unfold foldMap. 
    simpl.
    reflexivity.
  * unfold SumProba.
    unfold foldMap. 
    destruct a as [e2 p2]. simpl.
    destruct (cmp e e2) eqn: Heq; simpl; auto. field.
    { unfold SumProba in IHm. specialize (IHm p e).  unfold foldMap in IHm.
      rewrite IHm. field.
    }
Qed.

Module AddEvent.

  Lemma swap_l {K: Set} `{OrdDec K}: forall a b m,
      add_event (add_event m a) b = add_event (add_event m b) a.
  Proof.
    unfold add_event.
    intros. destruct a as [k1 v1]. destruct b as [k2 v2]. simpl in *.
    revert k1.
    revert k2.
    revert v1.
    revert v2.
    induction m; intros.
    * simpl. destruct (cmp k2 k1) eqn: CC;
      try (apply cmp_opp in CC; rewrite CC; reflexivity);
      try (apply cmp_eq in CC; subst; rewrite cmp_refl; f_equal; f_equal)
      .
      field.
    * destruct a as [ka va].
      simpl in *.
      Ltac gogo_swap_l := match goal with
      | |- (?k2, ?v1 + ?v2) :: ?xs = (?k2, ?v2 + ?v1) :: ?xz => f_equal
      | |- (?k2, ?v1 + ?v2) = (?k2, ?v2 + ?v1) => f_equal
      | |- ?v1 + ?v2 = ?v2 + ?v1 => field
      | |- _ => inequalities
      end.
      destruct (cmp k1 ka) eqn: D1; destruct (cmp k2 ka) eqn: D2; simpl; try rewrite D1; try rewrite D2;
        destruct (cmp k1 k2) eqn: KK; try (apply cmp_opp in KK; rewrite KK; auto);
          try (apply cmp_eq in KK; subst; rewrite cmp_refl); repeat gogo_swap_l; auto. 
      + f_equal. f_equal. field.
      + f_equal. apply IHm.
      + f_equal. apply IHm.
      + f_equal. apply IHm.
  Qed.

End AddEvent.

Definition add_proba {K: Set} `{OrdDec K} (m1: Proba K) (m2: Proba K) : Proba K :=
  merge_with (fun a b => a + b) m1 m2.

Lemma add_proba_adds_proba {K: Set} `{OrdDec K}: forall (m1 m2 : Proba K),
   SumProba (add_proba m1 m2) = SumProba m1 + SumProba m2.
Proof.
  intros.
  unfold SumProba.
  symmetry.
  apply FoldMap.merge; intros; field.
Qed.

Lemma add_proba_nil_l {K: Set} `{OrdDec K}: forall (m : Proba K),
  add_proba [] m = m.
Proof.
  intros. unfold add_proba. apply Merge.nil_left.
Qed.

Lemma add_proba_nil_r {K: Set} `{OrdDec K}: forall (m : Proba K),
  add_proba m [] = m.
Proof.
  intros. unfold add_proba. apply Merge.nil_right.
Qed.

Lemma add_proba_nil {K: Set} `{OrdDec K}: forall (m1 m2 : Proba K),
  add_proba m1 m2 = [] <-> m1 = [] /\ m2 = [].
Proof.
  intros. unfold add_proba. 
  apply Merge.nil_both.
Qed.

Lemma add_proba_comm {K: Set} `{OrdDec K}: forall (m1 m2: Proba K),
  ValidMap m1 -> ValidMap m2 -> add_proba m1 m2 = add_proba m2 m1.
Proof.
  intros.
  unfold add_proba.
  apply Merge.comm; auto.
  intros.
  field.
Qed.

Lemma add_proba_keys_comm {K: Set} `{OrdDec K}: forall (m1 m2: Proba K),
  ListDef.map fst (add_proba m1 m2) = ListDef.map fst (add_proba m2 m1).
Proof.
  unfold add_proba.
  intros.
  apply Merge.keys_comm.
Qed.

Lemma add_proba_keeps_keys {K: Set} `{OrdDec K}: forall (ttl m1 m2: Proba K) (k: K),
  add_proba m1 m2 = ttl -> In k (ListDef.map fst m1) -> In k (ListDef.map fst ttl).
Proof.
  intros ttl m1 m2 k P1 IK.
  eapply Merge.conserve_lk; eauto.
Qed.

Lemma add_proba_keeps_keys_conv {K: Set} `{OrdDec K}: forall (ttl m1 m2: Proba K) (k: K),
  add_proba m1 m2 = ttl -> In k (ListDef.map fst ttl) -> In k (ListDef.map fst m1) \/ In k (ListDef.map fst m2).
Proof.
  intros ttl m1 m2 k P1 IK.
  eapply Merge.conserve_lk_conv; eauto.
Qed.

Lemma add_proba_valid {K: Set} `{OrdDec K}: forall m1 m2,
  ValidMap m1 -> ValidMap m2 -> ValidMap (add_proba m1 m2).
Proof.
  intros.
  unfold add_proba.
  apply Merge.correct; auto.
Qed.

Module AddProba.

Lemma add_proba_event {K: Set} `{OrdDec K}:
  forall m e, add_event m e = add_proba m [e].
Proof.
  induction m; intros; destruct e as [k p].
  rewrite add_proba_nil_l. reflexivity.

  destruct a as [ka pa]. unfold add_proba. simpl. simp merge_with. unfold merge_with_unfold_clause_3.
  destruct (cmp k ka) eqn: CK; try (apply cmp_opp in CK; rewrite CK); try (apply cmp_eq in CK; subst; rewrite cmp_refl); f_equal.
  rewrite Merge.nil_right. reflexivity.
  unfold add_event, add_proba in IHm.
  specialize (IHm (k, p)). simpl in IHm. assumption.
Qed.

Lemma add_proba_cons {K: Set} `{OrdDec K}:
  forall m1 m2 e,
    ValidMap (e::m1) -> ValidMap m2 ->
    add_proba (e::m1) m2 = add_event (add_proba m1 m2) e.
Proof.
  intros m1 m2.
  revert m1.
  induction m2; intros.
  * rewrite add_proba_nil_r.
    rewrite add_proba_nil_r.
    destruct e as [k v].
    apply Valid.head in H1.
    destruct H1.
    destruct m1; auto.
    destruct p as [kp vp].
    simpl. 
    apply Forall_cons_iff in H3.
    destruct H3.
    apply cmp_correct in H3. rewrite H3.
    reflexivity.
  * destruct e as [ke ve], a as [ka va].
    simpl.
    destruct m1 as [|[kc vb] m1].
    + rewrite add_proba_nil_l.
      unfold add_proba. simpl.
      simp merge_with. unfold merge_with_unfold_clause_3.
      destruct (cmp ke ka) eqn: KK; try rewrite Merge.nil_left; auto.
      - f_equal. f_equal. field.
      - f_equal. apply IHm2; auto. apply Valid.head in H2. tauto.
    + unfold add_proba.
      simp merge_with. unfold merge_with_unfold_clause_3.
      apply Valid.head in H1. destruct H1.
      apply Valid.head in H1. destruct H1.
      apply Forall_cons_iff in H3. destruct H3.
      apply Valid.head in H2. destruct H2.
      apply cmp_correct in H3.

      destruct (cmp ke ka) eqn: K1, (cmp kc ka) eqn: K2; repeat inequalities; simpl; repeat inequalities; f_equal;
        simp merge_with; unfold merge_with_unfold_clause_3; repeat inequalities; auto.
      - f_equal. field.
      - apply IHm2; auto.
        apply Valid.head. split.
        apply Valid.head. split.
        assumption.
        assumption.
        apply Forall_cons.
        apply cmp_correct.
        assumption.
        assumption.
Qed.

Lemma add_proba_event_swap {K: Set} `{OrdDec K}:
  forall m1 m2 e,
    ValidMap m1 -> ValidMap m2 ->
    add_event (add_proba m1 m2) e = add_proba m1 (add_event m2 e).
Proof.
  intros.
  rewrite add_proba_event.
  rewrite add_proba_event.
  symmetry.
  unfold add_proba.
  apply Merge.assoc; auto.
  intros. field.
  constructor.
Qed.

Lemma assoc {K: Set} `{OrdDec K}: forall m1 m2 m3,
  ValidMap m1 -> ValidMap m2 -> ValidMap m3 ->
  add_proba m1 (add_proba m2 m3) = add_proba (add_proba m1 m2) m3.
Proof.
  unfold add_proba.
  intros.
  apply Merge.assoc; auto.
  intros.
  field.
Qed.

Lemma swap {K: Set} `{OrdDec K}: forall m1 m2 m3,
  ValidMap m1 -> ValidMap m2 -> ValidMap m3 ->
  add_proba m1 (add_proba m2 m3) = add_proba m2 (add_proba m1 m3).
Proof.
  intros.
  rewrite add_proba_comm; try apply add_proba_valid; auto.
  rewrite <- assoc; auto.
  f_equal.
  apply add_proba_comm; auto.
Qed.

End AddProba.

Definition map_proba {K: Set} (p: Proba K) (f: Qc -> Qc): Proba K :=
    ListDef.map (fun (kv: K * Qc) => let (k, p) := kv in (k, f p)) p.

Lemma map_proba_keeps_keys {K: Set}:
  forall (p: Proba K) out f, map_proba p f = out -> ListDef.map fst p = ListDef.map fst out.
Proof.
  unfold map_proba.
  intros.
  rewrite <- H.
  rewrite map_map.
  f_equal.
  apply functional_extensionality.
  intro x.
  destruct x.
  simpl.
  reflexivity.
Qed.

Lemma map_proba_keeps_keys_conv {K: Set}:
  forall (p: Proba K) out f, map_proba p f = out -> ListDef.map fst out = ListDef.map fst p.
Proof.
  unfold map_proba.
  intros.
  subst.
  rewrite map_map.
  f_equal.
  apply functional_extensionality.
  intros.
  destruct x.
  reflexivity.
Qed.

Lemma map_proba_valid {K: Set} `{OrdDec K}:
  forall (p: Proba K) (f: Qc -> Qc),
    ValidMap p -> ValidMap (map_proba p f).
Proof.
  intros.
  unfold map_proba.
  induction H1.
  * constructor.
  * constructor.
  * constructor; auto.
Qed.

Definition mul_proba {K: Set} (p: Proba K) (n: Qc): Proba K :=
  map_proba p (fun x => n * x).

Lemma mul_proba_sum {K: Set}: forall (p: Proba K) (n: Qc), 
    SumProba (mul_proba p n) = n * SumProba p.
Proof.
  induction p; simpl; intros.
  * rewrite sumproba_empty. field.
  * destruct a as [p2 k2]. repeat (rewrite sumproba_cons).
    rewrite (IHp n).
    field.
Qed.

Lemma mul_proba_1 {K: Set}: forall (p: Proba K), mul_proba p 1 = p.
Proof.
intros.
induction p; simpl; auto.
destruct a.
rewrite IHp.
f_equal.
f_equal.
field.
Qed.

Definition merge_probas {K: Set} `{OrdDec K} (l: list (Qc * Proba K)): Proba K :=
  fold_right add_proba ([]: Proba K) (map (fun (pr: Qc * Proba K) => let (p, d) := pr in mul_proba d p) l).

Lemma merge_probas_nil {K: Set} `{OrdDec K}: merge_probas ([] : list (Qc * Proba K)) = [].
Proof.
  intros.
  auto.
Qed.

Lemma merge_probas_cons {K: Set} `{OrdDec K}: forall lst p a,
      merge_probas ((p, a) :: lst) = add_proba (mul_proba a p) (merge_probas lst).
Proof.
  unfold merge_probas.
  simpl. auto.
Qed.

Lemma merge_probas_sum {K: Set} `{OrdDec K}: forall(lst: list (Qc * Proba K)),
    SumProba (merge_probas lst) = fold_right ((fun (i : Qc * Proba K) cur => cur + let (p, s) := i in p * SumProba s)) 0 lst.
Proof.
induction lst; intros.
* rewrite merge_probas_nil.
  simpl.
  rewrite sumproba_empty.
  reflexivity.
* destruct a.
  rewrite merge_probas_cons.
  rewrite add_proba_adds_proba.
  simpl.
  rewrite  IHlst.
  rewrite mul_proba_sum.
  field.
Qed.

Lemma merge_probas_correct {K: Set} `{OrdDec K}:
    forall ls, Forall ValidMap (ListDef.map snd ls) -> ValidMap (merge_probas ls).
Proof.
  induction ls; unfold merge_probas; intros; simpl.
  constructor.
  unfold add_proba.
  simpl in H1. apply Forall_cons_iff in H1.
  apply Merge.correct.
  destruct a. 
  apply map_proba_valid.
  tauto.
  apply IHls.
  tauto.
Qed.

Lemma merge_probas_keeps_keys_cons {K: Set} `{OrdDec K}:
  forall out p l ls k, ValidMap l -> Forall ValidMap (ListDef.map snd ls) ->
    merge_probas ((p, l) :: ls) = out ->
    In k (ListDef.map fst l) -> In k (ListDef.map fst out).
Proof.
  intros out p l ls k VML VMLS MPO.
  unfold merge_probas in MPO.
  unfold mul_proba in MPO. simpl in MPO.
  remember (fold_right add_proba nil (ListDef.map (fun pr : prod Qc (Proba K) => let (p, d) := pr in map_proba d (fun n => n * p)) ls)) as X.
  assert (ValidMap X) as VX. {
    subst.
    clear VML k l p.
    induction ls. constructor.
    destruct a.
    inversion VMLS; subst.
    simpl.
    specialize (IHls H4).
    apply add_proba_valid.
    apply map_proba_valid.
    assumption.
    assumption.
  }
  intro INL.
  apply (add_proba_keeps_keys out (map_proba l (fun n => n * p)) X); auto.
  * subst. f_equal.
    f_equal.
    apply functional_extensionality. intros. field.
    f_equal.
    f_equal.
    apply functional_extensionality. intros. destruct x; simpl.
    f_equal.
    apply functional_extensionality. intros. field.
  * pose proof (map_proba_keeps_keys l (map_proba l (fun n => n * p))).
    subst.
    erewrite <- H1; try reflexivity.
    assumption.
Qed.

Lemma merge_probas_eq_nil {K: Set} `{OrdDec K}:
  forall (ls: list (Qc * Proba K)),
     merge_probas ls = [] <-> Forall (fun pp => snd pp = []) ls.
Proof.
  induction ls; split; intros; auto.
  unfold merge_probas in H1.
  simpl in H1.
  apply add_proba_nil in H1.
  destruct H1.
  destruct a.
  apply Forall_cons.
  simpl.
  unfold mul_proba in H1. unfold map_proba in H1.
  apply map_eq_nil in H1. assumption.
  apply IHls; auto.
  inversion H1; subst. destruct a. simpl in H4. subst.
  unfold merge_probas.
  simpl.
  apply add_proba_nil. tauto.
Qed.

Lemma fold_right_all_prop (A: Set) (f: A -> A -> A) (P: A -> Prop):
  (forall a b, P a -> P b -> P (f a b)) -> forall (ini: A) (l: list A),
    P ini ->
    Forall P l ->
        P (fold_right f ini l).
Proof.
  intros.
  induction l.
  simpl. assumption.
  apply Forall_cons_iff in H1.
  simpl. apply H; tauto.
Qed.

Lemma fold_right_one_prop (A: Set) (f: A -> A -> A) (P: A -> Prop):
  forall (l: list A),
    (forall a b, P a \/ P b -> P (f a b)) ->
  forall (ini: A),
    (exists e, In e l /\ P e) ->
    P (fold_right f ini l).
Proof.
  induction l; intros.
  {
    destruct H0.
    destruct H0. inversion H0.
  }
  {
    destruct H0. destruct H0. simpl.
    destruct H0.
    {
      subst. apply H. tauto.
    }
    apply H.
    right.
    apply IHl; intros. apply H. auto.
    exists x. tauto.
  }
Qed.

Lemma map_f_is_id (A : Set): forall (f: A -> A) (lst: list A),
    f = id -> ListDef.map f lst = lst.
Proof.
  intros.
  subst.
  apply map_id.
Qed.

Lemma merge_probas_keeps_keys {K: Set} `{OrdDec K}:
    forall (out: Proba K) (ls: list (Qc * Proba K)) k,
        merge_probas ls = out ->
        In k (ListDef.map fst (concat (ListDef.map snd ls))) ->
        In k (ListDef.map fst out).
Proof.
  intros. subst.
  unfold merge_probas.
  apply fold_right_one_prop; intros.
  {
    destruct H1. eapply add_proba_keeps_keys; eauto.
    rewrite add_proba_keys_comm. eapply add_proba_keeps_keys; eauto.
  }
  induction ls. inversion H2.

  destruct a as [p mp].
  simpl in H2.
  rewrite map_app in H2.
  rewrite in_app_iff in H2. destruct H2. {
    exists (mul_proba mp p ). split; auto.
    simpl. tauto.
    unfold mul_proba.

    assert (ListDef.map fst (map_proba mp (fun n => p * n)) = ListDef.map fst mp). {
      symmetry.
      eapply map_proba_keeps_keys. reflexivity.
      }
      rewrite H2. assumption.
    }

    {
        specialize (IHls H1).
        destruct IHls.
        destruct H2.
        exists x.
        split; auto.
        right.
        assumption.
    }
Qed.

Lemma merge_probas_keeps_keys_conv {K: Set} `{OrdDec K}:
  forall (ls: list (Qc * Proba K)) k,
    In k (ListDef.map fst (merge_probas ls)) -> 
        In k (ListDef.map fst (concat (ListDef.map snd ls))).
Proof.
  induction ls; intros.
  inversion H1.
  destruct a.
  simpl.
  rewrite map_app.
  rewrite in_app_iff.
  destruct (add_proba_keeps_keys_conv
      (add_proba (mul_proba p q) (merge_probas ls))
      (mul_proba p q)
      (merge_probas ls)
      k
      ); auto.
  left.
  unfold mul_proba in H2.
  erewrite <- map_proba_keeps_keys in H2. apply H2. reflexivity.
Qed.

Definition rebuild_proba {K: Set} `{OrdDec K} (lst: Proba K): Proba K :=
  merge_probas (ListDef.map (fun a => (1, [a])) lst).

Lemma rebuild_proba_keeps_sumproba {K: Set} `{OrdDec K}:
    forall (lst: Proba K), SumProba (rebuild_proba lst) = SumProba lst.
Proof.
  unfold rebuild_proba.
  intros.
  rewrite merge_probas_sum.
  unfold SumProba.
  unfold foldMap.
  induction lst; simpl; auto.
  rewrite IHlst.
  field.
Qed.

Lemma rebuild_proba_keeps_keys {K: Set} `{OrdDec K}:
  forall k (lst: Proba K),
    In k (ListDef.map fst lst) -> In k (ListDef.map fst (rebuild_proba lst)).
Proof.
  intros.
  unfold rebuild_proba.

  eapply merge_probas_keeps_keys.
  reflexivity. 
  rewrite map_map.
  rewrite concat_map.
  rewrite map_map.
  apply in_concat. simpl.
  exists [k]. split; try (constructor; reflexivity).
  eapply (in_map (fun x => [x])) in H1.
  rewrite map_map in H1.
  assumption.
Qed.

Lemma concat_pure_is_id (K: Type):
    forall l : list K,
      concat (map (fun x => [x]) l) = l.
Proof.
  induction l; auto.
  simpl. f_equal. auto.
Qed.

Lemma rebuild_proba_keeps_keys_conv {K: Set} `{OrdDec K}:
  forall k (lst: Proba K),
    In k (ListDef.map fst (rebuild_proba lst)) -> 
    In k (ListDef.map fst lst) .
Proof.
  intros.
  unfold rebuild_proba in H1.
  pose proof (merge_probas_keeps_keys_conv _ _ H1).
  rewrite map_map in H2.
  rewrite (map_ext _ (fun x : K * Qc => [x])) in H2. 2: {
      intros. destruct a. simpl. reflexivity.
  }
  rewrite concat_pure_is_id in H2.
  assumption.
Qed.

Lemma rebuild_proba_valid {K: Set} `{OrdDec K}:
  forall (lst: Proba K),
    ValidMap (rebuild_proba lst).
Proof.
  intros.
  unfold rebuild_proba.
  unfold merge_probas.
  induction lst; simpl.
  * constructor.
  * apply add_proba_valid; auto.
    constructor.
Qed.

Lemma SumProba_map_keys {K1 K2: Set}:
    forall (f: K1 * Qc -> K2 * Qc) (l: Proba K1),
        (forall x, snd (f x) = snd x) -> SumProba (ListDef.map f l) = SumProba l.
Proof.
  unfold SumProba; unfold foldMap; induction l; intros; simpl; auto.
  rewrite H.
  rewrite <- IHl; auto.
Qed.
