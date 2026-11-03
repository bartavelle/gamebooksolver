(* Maps and sets as sorted association lists, and bags as maps of positive quantities. *)
Require Import Stdlib.Logic.FunctionalExtensionality.
Require Import Stdlib.Lists.List.
Require Import Stdlib.Arith.Compare_dec.
Require Import Stdlib.QArith.Qcanon.
From Equations Require Import Equations.
From Stdlib Require Lia.
Require Import ZArith.
Import ListNotations.
Require Export order.

Definition Mp (K: Set) (V: Set) := list (K * V).

Definition empty {K: Set}: Mp K nat := [].

Inductive ValidMap {K V: Set} `{OrdDec K} : Mp K V -> Prop :=
   | valid_empty: ValidMap []
   | valid_single: forall kv, ValidMap [kv]
   | valid_cons: forall k1 k2 v1 v2 xs, lt k1 k2 -> ValidMap ((k2, v2)::xs) -> ValidMap ((k1, v1)::(k2, v2)::xs)
   .

Module Valid.
  Lemma value_switch {K V: Set} `{OrdDec K}:
    forall (k: K) (v1 v2: V) (m: Mp K V),
      ValidMap ((k, v1)::m) -> ValidMap ((k, v2)::m).
  Proof.
    intros.
    inversion H1; subst.
    constructor. constructor; auto.
  Qed.

  Lemma head {K V: Set} `{OrdDec K}:
    forall (k: K) (v: V) (m: Mp K V),
      ValidMap ((k, v)::m) <-> (ValidMap m /\ Forall (fun (pr: K * V) => let (ck, _) := pr in lt k ck) m).
  Proof.
    intros k v m.
    generalize dependent v.
    generalize dependent k.
    induction m; repeat split; intros.
    * constructor.
    * constructor.
    * constructor.
    * inversion H1; subst; auto.
    * inversion H1; subst.
      constructor; auto.
      inversion H7; subst. constructor.
      constructor. eapply lt_trans; eauto.
      destruct (IHm k v). destruct H2. constructor. eapply lt_trans;eauto. auto.
      inversion H6; subst. auto.
    * destruct H1.
      destruct a as [k2 v2].
      inversion H2; subst.
      constructor; auto.
  Qed.

  Lemma same_keys {K V: Set} `{OrdDec K}:
    forall (m1 m2: Mp K V),
      map fst m1 = map fst m2 ->
      ValidMap m1 ->
      ValidMap m2.
  Proof.
    intros.
    generalize dependent m2.
    induction H2; intros.
    * simpl in H1.
      symmetry in H1.
      apply map_eq_nil in H1.
      subst.
      constructor.
    * destruct kv.
      simpl in H1.
      destruct m2. inversion H1.
      destruct p.
      simpl in H1.
      inversion H1; subst.
      symmetry in H4.
      apply map_eq_nil in H4.
      subst.
      constructor.
    * destruct m2 as [|[km1 kv1] [|[km2 kv2]]]; simpl in H3; inversion H3; subst; clear H3.
      constructor; auto.
      apply IHValidMap.
      simpl.
      f_equal.
      assumption.
  Qed.

  Lemma filter {K V: Set} `{OrdDec K}:
    forall (m: Mp K V) pred,
      ValidMap m ->
      ValidMap (filter pred m).
  Proof.
    intros.
    induction H1.
    * constructor.
    * simpl. destruct (pred kv); constructor.
    * simpl in *.
      destruct (pred (k1, v1)); destruct (pred (k2, v2)).
      + constructor; auto.
      + apply Valid.head in H2.
        destruct H2 as [VMM LT].
        apply Valid.head.
        split; auto.
        apply Forall_forall.
        intros.
        apply filter_In in H2.
        destruct H2, x.
        eapply Forall_forall in LT; eauto.
        simpl in LT.
        eapply lt_trans;eauto.
      + assumption.
      + assumption.
  Qed.

End Valid.

Fixpoint insert_with {K: Set} {V: Set} `{OrdDec K} (f: V -> V) (k: K) (v: V) (mp: Mp K V) :=
  match mp with
  | [] => [(k, v)]
  | ((ck, cv)::xs) =>
      match cmp k ck with
      | EQ => ((k, f cv)::xs)
      | LT => ((k, v)::(ck, cv)::xs)
      | GT => (ck,cv)::insert_with f k v xs
      end
  end.

Fixpoint update {K: Set} {V: Set} `{OrdDec K} (f: option V -> option V) (k: K) (mp: Mp K V) :=
  match mp with
  | [] => match f None with
          | None => []
          | Some v => [(k, v)]
          end
  | ((ck, cv)::xs) =>
      match cmp k ck with
      | EQ => match f (Some cv) with
              | None => xs
              | Some nv => (k, nv)::xs
              end
      | LT => match f None with
              | None => (ck, cv)::xs
              | Some nv => (k, nv)::(ck,cv)::xs
              end
      | GT => (ck,cv)::update f k xs
      end
  end.

Module Update.
  Lemma update_preserve_keys {K: Set} {V: Set} `{OrdDec K}:
    forall (m: Mp K V) f k,
      ValidMap m ->
      f None = None ->
      (forall x, exists v, f (Some x) = Some v) ->
      map fst m = map fst (update f k m).
  Proof.
    intros.
    generalize dependent k.
    induction H1; intros; simpl.
    * rewrite H2. reflexivity.
    * destruct kv.
      rewrite H2.
      destruct (cmp k k0) eqn:D; auto.
      destruct (H3 v).
      rewrite H1.
      simpl.
      rewrite cmp_eq in D.
      subst.
      reflexivity.
    * rewrite H2.
      destruct (H3 v1).
      destruct (H3 v2).
      rewrite H5.
      rewrite H6.
      destruct (cmp k k1) eqn: KK1; simpl; repeat inequalities; auto.
      f_equal.
      simpl in IHValidMap.
      rewrite H2 in IHValidMap.
      rewrite H6 in IHValidMap.
      apply IHValidMap.
  Qed.

  Lemma valid {K: Set} {V: Set} `{OrdDec K}:
    forall (m: Mp K V) f k,
    ValidMap m ->
    ValidMap (update f k m).
  Proof.
    intros.
    induction H1; simpl.
    * destruct (f None); constructor.
    * destruct kv.
      destruct (cmp k k0) eqn: KK0.
      + destruct (f None); constructor.
        apply cmp_correct in KK0. assumption.
        constructor.
      + destruct (f (Some v)); constructor.
      + destruct (f None); repeat constructor.
        apply cmp_opp in KK0.
        apply cmp_correct.
        assumption.
    * simpl in *.
      destruct (cmp k k1) eqn: KK1, (cmp k k2) eqn: KK2; repeat inequalities.
      + destruct (f None).
        inversion IHValidMap; subst.
        constructor. apply cmp_correct. assumption.
        constructor; auto.
        constructor; auto.
      + destruct (f None).
        constructor.
        apply cmp_correct; auto.
        constructor; assumption.
        constructor; assumption.
      + destruct (f (Some v1)); auto.
        constructor; auto.
      + destruct (f (Some v1)); auto.
        constructor; auto.
      + destruct (f None); constructor; auto.
        apply cmp_opp in KK1.
        apply cmp_correct.
        assumption.
      + destruct (f (Some v2)).
        constructor; auto.
        apply Valid.head in H2.
        destruct H2.
        apply Valid.head.
        split ;auto .
        apply Forall_forall.
        intros.
        destruct x.
        eapply Forall_forall in H3; eauto.
        simpl in H3.
        eapply lt_trans; eauto.
      + constructor; auto.
  Qed.

End Update.

Definition insert {K: Set} {V: Set} `{OrdDec K} (k: K) (v: V) (mp: Mp K V) :=
  insert_with (fun _ => v) k v mp.

Module Insert.

  Lemma insert_with_low {K V: Set} `{OrdDec K}:
    forall (m: Mp K V) f k v, ValidMap ((k, v)::m) -> insert_with f k v m = (k, v) :: m.
  Proof.
    induction m; intros; auto.
    destruct a as [nk nv].
    apply Valid.head in H1. destruct H1.
    rewrite Forall_cons_iff in H2.
    destruct H2.
    apply cmp_correct in H2.
    simpl. rewrite H2. auto.
  Qed.

  Lemma insert_with_valid {K V: Set} `{OrdDec K}: forall mp k v f, ValidMap mp -> ValidMap (insert_with f k v mp: Mp K V).
  Proof.
    intros mp k v F VM.
    induction VM; simpl.
    * constructor.
    * destruct kv as [k2 v2].
      destruct (cmp k k2) eqn:E; try constructor.
      - apply cmp_correct. auto.
      - constructor.
      - apply cmp_correct. apply cmp_opp. auto.
      - constructor.
    * destruct (cmp k k1) eqn:E; try constructor; auto.
      - apply cmp_correct. auto.
      - constructor; auto.
      - apply cmp_eq in E. subst. auto.
      - simpl in IHVM.
      destruct (cmp k k2) eqn:E2; constructor; auto.
        + apply cmp_correct. apply cmp_opp. auto.
        + apply cmp_correct. apply cmp_opp. auto.
  Qed.

  Lemma insert_with_insert_with {K: Set} {V: Set} `{OrdDec K}:
    forall (k: K) (f1 f2: V -> V) (v1 v2: V) (m: Mp K V),
      insert_with f1 k v1 (insert_with f2 k v2 m) = insert_with (fun v => f1 (f2 v)) k (f1 v2) m.
  Proof.
    induction m; simpl. 
    * rewrite cmp_refl. reflexivity.
    * rewrite <- IHm. 
      destruct a as [k3 v3].
      destruct (cmp k k3) eqn: CKCK; simpl.
      + rewrite cmp_refl; auto.
      + rewrite cmp_refl; auto.
      + rewrite CKCK; auto.
  Qed.

  Lemma insert_insert {K: Set} {V: Set} `{OrdDec K}:
    forall (k: K) (v1 v2: V) (m: Mp K V),
      insert k v1 (insert k v2 m) = insert k v1 m.
  Proof.
    intros.
    unfold insert.
    apply insert_with_insert_with.
  Qed.

  Lemma reorder_insert_with {K: Set} {V: Set} `{OrdDec K} :
      forall (k1 k2: K) (v1 v2: V) (f1 f2: V -> V) (mp: Mp K V),
        k1 <> k2 -> insert_with f1 k1 v1 (insert_with f2 k2 v2 mp) = insert_with f2 k2 v2 (insert_with f1 k1 v1 mp).
  Proof.
    induction mp; intros; simpl.
    * destruct (cmp k1 k2) eqn: HC.
      + replace (cmp k2 k1) with GT; auto.
        apply cmp_opp in HC. auto.
      + apply cmp_eq in HC. contradiction.
      + replace (cmp k2 k1) with LT; auto.
        apply cmp_opp in HC. auto.
    * specialize (IHmp H1).
      destruct a as [k3 v3].
      destruct (cmp k1 k2) eqn: HC3.
      + {
        destruct (cmp k2 k3) eqn:HC1; simpl.
        + rewrite HC3.
          replace (cmp k1 k3) with LT; simpl.
          replace (cmp k2 k1) with GT. rewrite HC1. reflexivity.
          apply cmp_opp in HC3. auto.
          symmetry. eapply cmp_trans; eauto.
        + rewrite HC3. apply cmp_eq in HC1. subst. rewrite HC3. simpl.
          replace (cmp k3 k1) with GT. replace (cmp k3 k3) with EQ. reflexivity.
          symmetry. apply cmp_eq. reflexivity.
          apply cmp_opp in HC3. auto.
        + assert (cmp k2 k1 = GT) by (apply cmp_opp; auto). destruct (cmp k1 k3) eqn: HC2; simpl; try rewrite H2; auto.
          - rewrite HC1. reflexivity.
          - rewrite HC1. f_equal. auto.
      }
      + apply cmp_eq in HC3. subst. contradiction.
      + {
        destruct (cmp k2 k3) eqn:HC1; simpl.
        * rewrite HC3. destruct (cmp k1 k3) eqn: HC2; simpl; try rewrite HC1; auto; apply cmp_opp in HC3; rewrite HC3; auto.
        * { apply cmp_eq in HC1. subst.
          destruct (cmp k1 k3); simpl; try discriminate.
          replace (cmp k3 k3) with EQ; auto. symmetry. apply cmp_refl.
        }
        *  pose proof (cmp_trans _ _ _ _ HC3 HC1). rewrite H2; simpl. rewrite HC1. f_equal;auto.
      }
  Qed.

  Lemma reorder_insert {K: Set} {V: Set} `{OrdDec K} :
      forall (k1: K) (k2: K) (v1: V) (v2: V) (mp: Mp K V),
        k1 <> k2 -> insert k1 v1 (insert k2 v2 mp) = insert k2 v2 (insert k1 v1 mp).
  Proof.
    intros k1 k2 v1 v2 mp Hneq.
    unfold insert. apply reorder_insert_with. auto.
  Qed.

  Lemma empty {K: Set} {V: Set} `{OrdDec K}:
    forall (k: K) (v: V) (mp: Mp K V) (f: V -> V),
      insert_with f k v mp = [] -> False.
  Proof.

    induction mp; intros.
    simpl in H1. inversion H1.
    simpl in H1. destruct a. destruct (cmp k k0); inversion H1.
  Qed.

End Insert.

Equations merge_with {K V: Set} `{OrdDec K} (f: V -> V -> V) (m1 m2: Mp K V) : Mp K V 
  by wf (length m1 + length m2)%nat lt :=
  merge_with f [] m2 := m2;
  merge_with f m1 [] := m1;
  merge_with f ((k1, v1)::m1s) ((k2, v2)::m2s) with cmp k1 k2 := {
    | LT := (k1, v1)::merge_with f m1s ((k2, v2)::m2s);
    | EQ := (k1, f v1 v2)::merge_with f m1s m2s;
    | GT := (k2, v2)::merge_with f ((k1, v1)::m1s) m2s
  }.
Next Obligation. simpl. Lia.lia. Qed.
Next Obligation. simpl. Lia.lia. Qed.

Module Merge.
  Inductive MergeWithL {K V: Set} `{OrdDec K} : (V -> V -> V) -> Mp K V -> Mp K V -> Mp K V -> Prop :=
      | MW_nil_l: forall f m, MergeWithL f [] m m
      | MW_nil_r: forall f m, MergeWithL f m [] m
      | MW_eq: forall f k1 v1 k2 v2 m1s m2s nxt, cmp k1 k2 = EQ ->
            MergeWithL f m1s m2s nxt ->
            MergeWithL f ((k1, v1)::m1s) ((k2, v2)::m2s) ((k1, f v1 v2)::nxt)
      | MW_lt: forall f k1 v1 k2 v2 m1s m2s nxt, cmp k1 k2 = LT ->
            MergeWithL f m1s ((k2, v2)::m2s) nxt ->
            MergeWithL f ((k1, v1)::m1s) ((k2, v2)::m2s) ((k1, v1)::nxt)
      | MW_gt: forall f k1 v1 k2 v2 m1s m2s nxt, cmp k1 k2 = GT ->
            MergeWithL f ((k1, v1)::m1s) m2s nxt ->
            MergeWithL f ((k1, v1)::m1s) ((k2, v2)::m2s) ((k2, v2)::nxt)
      .

  Lemma empty {K V: Set} `{OrdDec K}:
    forall (f: V -> V -> V) (m1 m2: Mp K V),
      merge_with f m1 m2 = [] <-> m1 = [] /\ m2 = [].
  Proof.
    split; intros.
    { 
      destruct m1, m2; auto.
      destruct p, p0.
      simp merge_with in H1.
      destruct (cmp k k0); simp merge_with in H1; discriminate.
    }
    { 
      destruct H1. subst. simp merge_with. reflexivity.
    }
  Qed.

  Lemma inductive_correct {K V: Set} `{OrdDec K}:
      forall (f: V -> V -> V) (m1 m2 out: Mp K V),
        merge_with f m1 m2 = out <-> MergeWithL f m1 m2 out.
  Proof.
    split.
    { generalize dependent m1.
      generalize dependent m2.
      induction out; intros. 
      apply empty in H1.
      destruct H1. subst. constructor.

      destruct m1 as [|[k1 v1] m1s]; destruct m2 as [|[k2 v2] m2s]; simp merge_with in H1.
      { discriminate.  }
      { inversion H1; subst. constructor.  }
      { inversion H1; subst. constructor.  }
      { destruct (cmp k1 k2) eqn: H12; simp merge_with in H1; inversion H1; subst; constructor; auto. }
    }
    { 
      intros.
      induction H1; simp merge_with; auto.
      * destruct m; auto.
      * rewrite H1. simp merge_with. f_equal. auto. 
      * rewrite H1. simp merge_with. f_equal. auto. 
      * rewrite H1. simp merge_with. f_equal. auto. 
    }
  Qed.

  Lemma to_inductive {K V: Set} `{OrdDec K}:
      forall (f: V -> V -> V) (m1 m2 out: Mp K V),
        merge_with f m1 m2 = out -> MergeWithL f m1 m2 out.
  Proof.
    intros.
    apply inductive_correct.
    assumption.
  Qed.

  Lemma from_inductive {K V: Set} `{OrdDec K}:
      forall (f: V -> V -> V) (m1 m2 out: Mp K V),
        MergeWithL f m1 m2 out -> merge_with f m1 m2 = out.
  Proof.
    intros.
    apply inductive_correct.
    assumption.
  Qed.

  Lemma nil_left {K V: Set} `{OrdDec K}:  forall (m: Mp K V) (f: V -> V -> V), merge_with f [] m = m.
  Proof.
  intros.
  destruct m; auto.
  Qed.

  Lemma nil_right {K V: Set} `{OrdDec K}:  forall (m: Mp K V) (f: V -> V -> V), merge_with f m [] = m.
  Proof.
  intros.
  destruct m; auto.
  Qed.

  Lemma nil_both {K V: Set} `{OrdDec K}:  forall (m1 m2: Mp K V) (f: V -> V -> V),
    merge_with f m1 m2 = [] <-> m1 = [] /\ m2 = [].
  Proof.
  intros.
  destruct m1, m2; simp merge_with; split; intros; try tauto.
  destruct p, p0. simp merge_with in H1. unfold merge_with_unfold_clause_3 in H1.
  destruct (cmp k k0); inversion H1.
  destruct H1. inversion H1.
  Qed.

  Lemma use_forall {A} (P : A -> Prop) (l : list A) (x : A) :
    Forall P l -> In x l -> P x.
  Proof.
    intros.
    pose proof (Forall_forall P l).
    destruct H1.
    apply H1; auto.
  Qed.

  Lemma conserve_elements {K V: Set} `{OrdDec K}:
    forall (m1 m2 out: Mp K V) f,
        ValidMap m1 -> ValidMap m2 ->
        merge_with f m1 m2 = out ->
          forall k v, In (k, v) out ->
              (In (k, v) m1 /\ ~In (k, v) m2) \/
              (In (k, v) m2 /\ ~In (k, v) m1) \/
              (exists v1 v2, In (k, v1) m1 /\ In (k, v2) m2 /\ v = f v1 v2).
  Proof.
    intros.
    apply inductive_correct in H3.
    generalize dependent k.
    generalize dependent v.
    induction H3; intros; try tauto.
    {
      apply cmp_eq in H3. subst.
      apply Valid.head in H1, H2.
      inversion H5; subst.
      { inversion H3; subst.
        right. right. exists v1, v2. repeat split; auto; constructor; reflexivity.
      }
      { destruct H1, H2.
        specialize (IHMergeWithL H1 H2 v k H3). destruct IHMergeWithL as [[Xa Xb]|[X|X]].
        {
          left. split. simpl. tauto.
          intro contra. simpl in contra.
          destruct contra as [contra|contra].
          {
            inversion contra; subst. clear contra.
            pose proof (use_forall _ m1s _ H6 Xa). simpl in H8. apply lt_not in H8. auto.
          }
          contradiction. 
        }
        {
          destruct X as [X1 X2] .
          right. left. split; simpl. tauto. intro contra. destruct contra as [contra|contra].
          {
            inversion contra; subst. clear contra.
            pose proof (use_forall _ m2s _ H7 X1). simpl in H8. apply lt_not in H8. auto.
          }
          contradiction.
        }
        {
          right. right.
          destruct X as [vx [vy [P1 [P2 P3]]]].
          exists vx, vy.
          repeat split; try auto; right; auto.
        }
      }
    }
    {
      inversion H5; subst.
      {
        inversion H6; subst.
        left. split; simpl; try tauto.
        intro contra.
        apply Valid.head in H2. destruct H2.
        destruct contra as [contra|contra].
        {
          inversion contra; subst. rewrite cmp_refl in H3. discriminate.
        }
        {
          pose proof (use_forall _ m2s _ H7 contra).
          simpl in H8.
          apply cmp_correct in H8.
          repeat inequalities.
        }
      }
      {
        apply Valid.head in H1. destruct H1.
        specialize (IHMergeWithL H1 H2 v k H6).
        destruct IHMergeWithL as [[Xa Xb]|[[X1 X2]|X]].
        {
          left. split; auto. right; auto.
        }
        {
          right. left. split; auto. intro contra. destruct contra.
          inversion H8; subst.
          apply Valid.head in H2. destruct H2.
          destruct X1.
          inversion H10; subst. rewrite cmp_refl in H3. discriminate.
          pose proof (use_forall _ m2s _ H9 H10). simpl in H11.
          apply cmp_correct in H11.
          repeat inequalities.
          contradiction.
        }
        {
        right. right. 
          destruct X as [vx [vy [P1 [P2 P3]]]].
          exists vx, vy.
          repeat split; try auto; right; auto.
        }
      }
    }
    {
      inversion H5; subst.
      {
        inversion H6; subst.
        right. left. split. left. reflexivity.
        intro contra.
        destruct contra as [contra|contra].
        inversion contra; subst. rewrite cmp_refl in H3. discriminate.
        apply Valid.head in H1.
        destruct H1.
        pose proof (use_forall _ m1s _ H7 contra). simpl in H8.
        repeat inequalities.
      }
      {
        apply Valid.head in H2. destruct H2.
        specialize (IHMergeWithL H1 H2 v k H6).
        destruct IHMergeWithL as [[Xa Xb]|[[X1 X2]|X]].
        {
          left. split.
          * inversion Xa. inversion H8; subst.
            left. auto. right. auto.
          * intro contra. destruct contra as [contra|contra].
            inversion contra; subst. inversion Xa; subst. inversion H8; subst. rewrite cmp_refl in H3. discriminate.
            apply Valid.head in H1. destruct H1.
            pose proof (use_forall _ m1s _ H9 H8). simpl in H10.
            repeat inequalities.
            contradiction.
        }
        {
          right. left. split. right; auto. auto.
        }
        {
          right. right.
          destruct X as [vx [vy [P1 [P2 P3]]]].
          exists vx, vy.
          repeat split; try auto; right; auto.
        }
      }
    }
  Qed.

  Lemma conserve_lk {K V: Set} `{OrdDec K}: forall (out m1 m2: Mp K V) (f: V -> V -> V) k,
    merge_with f m1 m2 = out ->
    In k (map fst m1) -> In k (map fst out).
  Proof.
    intros.
    apply inductive_correct in H1.
    generalize dependent k.
    induction H1; intros.
    * inversion H2.
    * assumption.
    * simpl in *. destruct H3; auto.
    * simpl in *. destruct H3; auto.
    * simpl in *. destruct H3; auto.
  Qed.

  Lemma conserve_lk_r {K V: Set} `{OrdDec K}: forall (out m1 m2: Mp K V) (f: V -> V -> V) k,
    merge_with f m1 m2 = out ->
    In k (map fst m2) -> In k (map fst out).
  Proof.
    intros.
    apply inductive_correct in H1.
    generalize dependent k.
    induction H1; intros.
    * assumption.
    * inversion H2.
    * simpl in *. destruct H3; auto.
      subst. inequalities. tauto.
    * simpl in *. destruct H3; auto.
    * simpl in *. destruct H3; auto.
  Qed.

  Lemma conserve_lk_conv {K V: Set} `{OrdDec K}: forall (out m1 m2: Mp K V) (f: V -> V -> V) k,
    merge_with f m1 m2 = out ->
    In k (map fst out) -> In k (map fst m1) \/ In k (map fst m2).
  Proof.
    intros.
    apply inductive_correct in H1.
    generalize dependent k.
    induction H1; intros; try tauto; simpl in *; destruct H3; try tauto;
      specialize (IHMergeWithL k H3); tauto.
  Qed.

  Lemma correct {K V: Set} `{OrdDec K}: forall (m1 m2: Mp K V) (f: V -> V -> V),
    ValidMap m1 -> ValidMap m2 -> ValidMap (merge_with f m1 m2).
  Proof.
    intros.
    remember (merge_with f m1 m2). symmetry in Heqm. apply inductive_correct in Heqm.
    generalize dependent m2.
    generalize dependent m1.
    induction m; intros.
    * constructor.
    * inversion Heqm; subst; auto. 
    {
      apply Valid.head in H1, H2. destruct H1, H2.
      specialize (IHm _ H1 _ H2 H9).
      apply cmp_eq in H8. subst.
      apply Valid.head. split; auto.
      apply inductive_correct in H9.
      apply Forall_forall.
      intros.
      destruct x as [kx vx].
      pose proof (conserve_elements m1s m2s m f H1 H2 H9 _ _ H5).
      destruct H6 as [[Xa Xb]|[[Xa Xb]|[v P]]].
      {
        pose proof (use_forall _ m1s _ H3 Xa). simpl in H6. auto.
      }
      {
        pose proof (use_forall _ m2s _ H4 Xa). simpl in H6. auto.
      }
      {
        destruct P as [vn [P1 [P2 P3]]].
        pose proof (use_forall _ m1s _ H3 P1). simpl in H6. auto.
      }
    }
    {
      apply Valid.head in H1. destruct H1.
      specialize (IHm _ H1 _ H2 H9).
      apply Valid.head. split; auto.
      apply inductive_correct in H9.

      apply Forall_forall.
      intros [kx vx] HI.
      pose proof (conserve_elements m1s _ m f H1 H2 H9 _ _ HI).
      destruct H4 as [[Xa Xb]|[[Xa Xb]|X]].
      {
        pose proof (use_forall _ _ _ H3 Xa). simpl in H4. auto.
      }
      {
        inversion Xa;subst.
        inversion H4; subst.
        apply cmp_correct. auto.
        apply Valid.head in H2. destruct H2.
        pose proof (use_forall _ _ _ H5 H4). simpl in H6.
        apply cmp_correct in H8.
        eapply lt_trans; eauto.
      }
      {
        destruct X as [va1 [vb2 [P]]].
        pose proof (use_forall _ _ _ H3 P). simpl in H5.
        auto.
      }
    }
    {
      apply Valid.head in H2. destruct H2.
      specialize (IHm _ H1 _ H2 H9).
      apply Valid.head. split; auto.
      apply inductive_correct in H9.

      apply Forall_forall.
      intros [kx vx] HI.
      pose proof (conserve_elements _ _ m f H1 H2 H9 _ _ HI).
      destruct H4 as [[Xa Xb]|[[Xa Xb]|X]].
      {
        inversion Xa;subst.
        inversion H4; subst.
        apply cmp_correct. apply cmp_opp. auto.
        apply Valid.head in H1. destruct H1.
        pose proof (use_forall _ _ _ H5 H4). simpl in H6.
        apply cmp_opp in H8.
        apply cmp_correct in H8.
        eapply lt_trans; eauto.
      }
      {
        pose proof (use_forall _ _ _ H3 Xa). simpl in H4. auto.
      }
      {
        destruct X as [va1 [vb2 [P [Z ZZ]]]].
        pose proof (use_forall _ _ _ H3 Z). simpl in H4. 
        auto.
      }
    }
  Qed.

  Lemma comm {K V: Set} `{OrdDec K}: forall (m1 m2: Mp K V) (f: V -> V -> V),
    (forall a b, f a b = f b a) ->
    ValidMap m1 ->
    ValidMap m2 ->
    merge_with f m1 m2 = merge_with f m2 m1.
  Proof.
    intros.
    remember (merge_with f m1 m2) as out.
    generalize dependent m2.
    generalize dependent m1.
    induction out; intros.
    { symmetry in Heqout.
      apply empty in Heqout.
      destruct Heqout. subst. simp merge_with. auto.
    }
    destruct m2 as [|[k2 p2]].
    { simp merge_with in *.
      rewrite Merge.nil_right in Heqout. auto. }
    destruct m1 as [|[k1 p1]].
    {
      rewrite Merge.nil_right.
      rewrite Merge.nil_left in Heqout. auto.
    }
    simp merge_with in *.
    { destruct (cmp k1 k2) eqn:KK; try (apply cmp_opp in KK; rewrite KK; simp merge_with in *).
      {
        inversion Heqout; subst.
        f_equal.
        apply IHout; auto.
        rewrite Valid.head in H2. tauto.
      }
      {
        apply cmp_eq in KK. subst. rewrite cmp_refl.
        simp merge_with in *.
        inversion Heqout; subst.
        f_equal.
        f_equal; auto.
        rewrite Valid.head in H2, H3.
        apply IHout; auto; tauto.
      }
      {
        inversion Heqout; subst.
        f_equal.
        apply IHout; auto.
        rewrite Valid.head in H3. tauto.
      }
    }
  Qed.

  Lemma keys_comm {K V: Set} `{OrdDec K}: forall (m1 m2: Mp K V) (f: V -> V -> V),
    ListDef.map fst (merge_with f m1 m2) = ListDef.map fst (merge_with f m2 m1).
  Proof.
    intros.
    remember (merge_with f m1 m2) as out.
    symmetry in Heqout.
    apply inductive_correct in Heqout.
    induction Heqout.
    { rewrite nil_right. reflexivity. }
    { rewrite nil_left. reflexivity. }
    {
      apply cmp_eq in H1. subst.
       simpl. simp merge_with. unfold merge_with_unfold_clause_3.
       rewrite cmp_refl. simpl. f_equal. assumption.
    }
    {
      simp merge_with. unfold merge_with_unfold_clause_3.
      apply cmp_opp in H1. rewrite H1.
      simpl.
      f_equal.
      assumption.
    }
    {
      simp merge_with. unfold merge_with_unfold_clause_3.
      apply cmp_opp in H1. rewrite H1.
      simpl.
      f_equal.
      assumption.
    }
  Qed.

  Lemma valid_give_r {K V: Set} `{OrdDec K} : forall (m1 m2 m3: Mp K V) f,
    ValidMap m1 -> ValidMap m3 ->
    merge_with f m1 m2 = m3 ->
    ValidMap m2.
  Proof.
    intros.
    apply inductive_correct in H3.
    induction H3; auto; try constructor; apply Valid.head; apply Valid.head in H1, H2; split; repeat inequalities.
    * apply IHMergeWithL; tauto.
    * apply Forall_forall.
      intros.
      destruct x.
      destruct H1, H2.
      apply inductive_correct in H4.
      pose proof (@conserve_lk_r K V _ _ _ _ _ f k H4).
      lapply H7; clear H7; intros.
      2: {
        replace k with (fst (k, v)) by auto.
        apply in_map.
        assumption.
      }
      assert (Forall (lt k2) (map fst nxt)). {
        apply Forall_map.
        replace (fun x : K * V => lt k2 (fst x)) with (fun pr : K * V => let (ck, _) := pr in lt k2 ck); auto.
        apply functional_extensionality.
        intros.
        destruct x.
        reflexivity.
      }
      eapply use_forall; eauto.
    * destruct H1, H2.
      specialize (IHMergeWithL H1 H2).
      apply Valid.head in IHMergeWithL. tauto.
    * destruct H1, H2.
      specialize (IHMergeWithL H1 H2).
      apply Valid.head in IHMergeWithL.
      tauto.
    * apply IHMergeWithL; try tauto.
      apply Valid.head; tauto.
    * destruct H1, H2.
      apply Forall_forall.
      intros.
      destruct x.
      apply inductive_correct in H4.
      pose proof (@conserve_lk_r K V _ _ _ _ _ f k H4).
      lapply H8; clear H8; intros.
      2: {
        replace k with (fst (k, v)) by auto.
        apply in_map.
        assumption.
      }
      assert (Forall (lt k2) (map fst nxt)). {
        apply Forall_map.
        replace (fun x : K * V => lt k2 (fst x)) with (fun pr : K * V => let (ck, _) := pr in lt k2 ck); auto.
        apply functional_extensionality.
        intros.
        destruct x.
        reflexivity.
      }
      eapply use_forall; eauto.
  Qed.

  Lemma cons_l {K V: Set} `{OrdDec K}: forall (m1 m2: Mp K V) (f: V -> V -> V) e,
    ValidMap (e::m1) -> ValidMap (e::m2) ->
      merge_with f (e::m1) m2 = e :: merge_with f m1 m2.
  Proof.
    intros.
    generalize dependent m2.
    generalize dependent e.
    induction m1; intros.
    * destruct m2. simp merge_with. reflexivity.
      destruct e, p. 
      simp merge_with.
      unfold merge_with_unfold_clause_3.
      apply Valid.head in H2. destruct H2.
      apply Valid.head in H2. destruct H2.
      apply Forall_cons_iff in H3.
      destruct H3.
      apply cmp_correct in H3.
      repeat inequalities.
      rewrite nil_left.
      reflexivity.
    * destruct e, a, m2.
      simp merge_with. auto.
      destruct p.
      simp merge_with.
      unfold merge_with_unfold_clause_3.
      apply Valid.head in H1, H2.
      destruct H1, H2.
      apply Valid.head in H1, H2.
      destruct H1, H2.
      apply Forall_cons_iff in H3, H4.
      destruct H3, H4.
      apply cmp_correct in H3, H4.
      repeat inequalities.
      f_equal.
      destruct (cmp k0 k1) eqn: K0K1; repeat inequalities.
      + apply IHm1.
        apply Valid.head. tauto.
        apply Valid.head; split.
        apply Valid.head. tauto.
        apply Forall_cons. apply cmp_correct. assumption.
        apply Forall_forall.
        intros.
        destruct x.
        eapply use_forall.
        eapply (Forall_map fst). 2: {
          apply in_map_iff.
          exists (k2, v2).
          split; eauto.
        }
        assert (Forall (fun x : K * V => lt k1 (fst x)) m2 -> Forall (fun x : K * V => lt k0 (fst x)) m2). {
          intros.
          apply Forall_forall.
          intros.
          destruct x. simpl in *.
          eapply use_forall in H11; eauto.
          simpl in H11.
          apply cmp_correct in H11.
          apply cmp_correct.
          inequalities.
        }
        apply H10. clear H10.
        replace (fun x : K * V => lt k1 (fst x)) with (fun pr : K * V => let (ck, _) := pr in lt k1 ck); auto.
        apply functional_extensionality.
        intros.
        destruct x.
        reflexivity.
      + simp merge_with.
        unfold merge_with_unfold_clause_3.
        rewrite cmp_refl.
        reflexivity.
      + simp merge_with.
        unfold merge_with_unfold_clause_3.
        rewrite K0K1.
        reflexivity.
  Qed.

  Lemma cons_r {K V: Set} `{OrdDec K}: forall (m1 m2: Mp K V) (f: V -> V -> V) e,
    ValidMap (e::m1) -> ValidMap (e::m2) ->
      merge_with f m1 (e::m2) = e :: merge_with f m1 m2.
  Proof.
    intros.
    generalize dependent m1.
    generalize dependent e.
    induction m2; intros.
    * destruct m1. simp merge_with. reflexivity.
      destruct e, p. 
      simp merge_with.
      unfold merge_with_unfold_clause_3.
      apply Valid.head in H1. destruct H1.
      apply Valid.head in H1. destruct H1.
      apply Forall_cons_iff in H3.
      destruct H3.
      apply cmp_correct in H3.
      repeat inequalities.
      rewrite nil_right.
      reflexivity.
    * destruct e, a, m1.
      simp merge_with. auto.
      destruct p.
      simp merge_with.
      unfold merge_with_unfold_clause_3.
      apply Valid.head in H1, H2.
      destruct H1, H2.
      apply Valid.head in H1, H2.
      destruct H1, H2.
      apply Forall_cons_iff in H3, H4.
      destruct H3, H4.
      apply cmp_correct in H3, H4.
      repeat inequalities.
      f_equal.
      destruct (cmp k1 k0) eqn: K1K0; repeat inequalities.
      + simp merge_with.
        unfold merge_with_unfold_clause_3.
        rewrite K1K0.
        reflexivity.
      + simp merge_with.
        unfold merge_with_unfold_clause_3.
        rewrite cmp_refl.
        reflexivity.
      + apply IHm2.
        apply Valid.head. tauto.
        apply Valid.head; split.
        apply Valid.head. tauto.
        apply Forall_cons. apply cmp_correct. repeat inequalities. reflexivity.
        apply Forall_forall.
        intros.
        destruct x.
        eapply use_forall.
        eapply (Forall_map fst). 2: {
          apply in_map_iff.
          exists (k2, v2).
          split; eauto.
        }
        assert (Forall (fun x : K * V => lt k1 (fst x)) m1 -> Forall (fun x : K * V => lt k0 (fst x)) m1). {
          intros.
          apply Forall_forall.
          intros.
          destruct x. simpl in *.
          eapply use_forall in H11; eauto.
          simpl in H11.
          apply cmp_correct in H11.
          apply cmp_correct.
          apply cmp_opp in K1K0.
          inequalities.
        }
        apply H10. clear H10.
        replace (fun x : K * V => lt k1 (fst x)) with (fun pr : K * V => let (ck, _) := pr in lt k1 ck); auto.
        apply functional_extensionality.
        intros.
        destruct x.
        reflexivity.
  Qed.

  Lemma assoc {K V: Set} `{OrdDec K}: forall (m1 m2 m3: Mp K V) (f: V -> V -> V),
      (forall x y z, f x (f y z) = f (f x y) z) ->
      ValidMap m1 -> ValidMap m2 -> ValidMap m3 ->
      merge_with f m1 (merge_with f m2 m3) = merge_with f (merge_with f m1 m2) m3.
  Proof.
    intros m1 m2 m3 f fassoc V1 V2 V3.
    generalize dependent m2.
    generalize dependent m3.

    (* remember (merge_with f m1 m2) as M12.
    symmetry in HeqM12.
    apply inductive_correct in HeqM12.
    generalize dependent m3. *)

    Ltac gogo_assoc := match goal with
    | H: MergeWithL _ [] _ _ |- _ => inversion H; subst; clear H
    | H: MergeWithL _ _ [] _ |- _ => inversion H; subst; clear H
    | H: MergeWithL _ _ _ [] |- _ => inversion H; subst; clear H
    | H: MergeWithL _ ((_, _)::_) ((_, _)::_) _ |- _ => inversion H; subst; clear H
    | H1: MergeWithL ?f ?m1 ?m2 ?ma,
      H2: MergeWithL ?f ?m1 ?m2 ?mb |- _ => 
      first [ constr_eq ma mb; fail 1  (* if equal, fail this branch *)
            | pose proof (from_inductive _ _ _ _ H1) as K1;
              pose proof (from_inductive _ _ _ _ H2) as K2;
              rewrite K1 in K2;
              subst]
    | |- context [merge_with _ [] _] => rewrite nil_left
    | |- context [merge_with _ _ []] => rewrite nil_right
    | |- context [merge_with _ ((_,_)::_) ((_,_)::_)] => simp merge_with; unfold merge_with_unfold_clause_3
    | |- ?a :: _ = ?a :: _ => f_equal
    | H: MergeWithL ?f ?m1s ?m2s ?n
        |- ?n = merge_with ?f ?m1s ?m2s 
        => symmetry
    | H: MergeWithL ?f ?m1s ?m2s ?n
        |- merge_with ?f ?m1s ?m2s = ?n
        => apply inductive_correct
    | |- ?a = ?a => reflexivity
    | H: ValidMap ((_, _) :: _) |- _ => apply Valid.head in H
    | H: ValidMap (?x :: _) |- _ => destruct x
    | H: _ /\ _ |- _ => destruct H
    | |- ValidMap ((_, _) :: _) => apply Valid.head
    | |- _ => inequalities
    end.

    induction m1; intros; repeat gogo_assoc.
    specialize (IHm1 H1).
    generalize dependent m3.
    induction m2; intros; repeat gogo_assoc.
    specialize (IHm2 H3).
    destruct (cmp k k0) eqn: KK0; repeat gogo_assoc.
    * induction m3; repeat gogo_assoc.
      destruct (cmp k0 k1) eqn: K0K1, (cmp k k1) eqn: KK1; repeat gogo_assoc.
      + rewrite <- IHm1; repeat gogo_assoc; split; auto.
      + rewrite <- IHm1; repeat gogo_assoc; split; auto.
      + rewrite <- IHm1; repeat gogo_assoc; split; auto.
      + rewrite <- IHm1; repeat gogo_assoc; auto.
      + rewrite <- IHm3; auto.
    * induction m3; repeat gogo_assoc.
      destruct (cmp k0 k) eqn: K0K; repeat gogo_assoc.
      + rewrite <- IHm1; repeat gogo_assoc; auto.
      + rewrite <- IHm1; repeat gogo_assoc; auto.
        f_equal. f_equal. apply fassoc.
      + rewrite <- IHm3; auto.
    * induction m3; repeat gogo_assoc.
      destruct (cmp k0 k1) eqn: K0K1; repeat gogo_assoc.
      + rewrite <- IHm2; auto. apply Valid.head.
        split; auto.
      + rewrite <- IHm2; auto. 
      + destruct (cmp k k1) eqn: KK1; repeat gogo_assoc.
        - pose proof (cmp_trans _ _ _ _ KK0 K0K1). repeat gogo_assoc.
        - rewrite <- IHm3; auto.
  Qed.
End Merge.

Fixpoint delete {K: Set} {V: Set} `{OrdDec K} (k: K) (mp: Mp K V) :=
  match mp with
  | [] => []
  | ((ck, cv)::xs) =>
      match cmp k ck with
      | LT => (ck, cv)::xs
      | EQ => xs
      | GT => (ck, cv)::delete k xs
      end
  end.

Module Delete.

  Ltac dtac := match goal with
  | H: cmp ?a ?b = LT |- ValidMap ((?a, _)::(?b, _)::_) => constructor
  | H: cmp ?a ?b = LT |- lt ?a ?b => apply cmp_correct
  | |- ?a = ?a => reflexivity
  | H: lt ?a ?a |- _ => apply lt_not in H
  | H: False |- _ => contradiction
  | |- _ => inequalities
  end.

  Lemma valid {K: Set} {V: Set} `{OrdDec K}: forall (m: Mp K V) (k: K),
      ValidMap m -> ValidMap (delete k m).
  Proof.
    intros.
    generalize dependent k.
    induction H1; intros; simpl.
    * constructor.
    * destruct kv . destruct (cmp k k0); simpl; constructor.
    * destruct (cmp k k1) eqn: CMP1, (cmp k k2) eqn: CMP2; auto;
      destruct (cmp k1 k2) eqn: CMP3; repeat dtac; auto.
      + apply Valid.head in H2.
        apply Valid.head.
        split; try tauto.
        apply Forall_forall.
        destruct H2.
        intros.

        destruct (Forall_forall (fun pr : K * V => let (ck, _) := pr in lt k2 ck) xs ) as [A B].
        destruct x.
        eapply A in H3. 2: { apply H4. }
        simpl in H3.
        apply cmp_correct.
        apply cmp_correct in H3.
        inequalities.
      + simpl in IHValidMap. specialize (IHValidMap k). rewrite CMP2 in IHValidMap. assumption.
  Qed.
    
  Lemma delete_insert {K: Set} {V: Set} `{OrdDec K}: forall (m: Mp K V) (k: K) (v: V) (f: V -> V),
      ValidMap m ->
      delete k (insert_with f k v m) = delete k m.
  Proof.
    induction m; intros; simpl.
    * rewrite cmp_refl. auto.
    * destruct a as [nk nv].
      destruct (cmp k nk) eqn: Hk; simpl; try rewrite cmp_refl; auto.
      rewrite Hk.
      f_equal.
      apply IHm.
      apply Valid.head in H1.
      tauto.
  Qed.

  Lemma insert_delete {K: Set} {V: Set} `{OrdDec K}: forall (m: Mp K V) (k: K) (v: V),
      ValidMap m ->
      insert k v (delete k m) = insert k v m.
  Proof.
    induction m; intros; auto.
    destruct a as [nk nv].
    destruct (cmp k nk) eqn: Hk; simpl; try rewrite Hk; auto.
    * unfold insert. simpl. rewrite Hk.
      apply Insert.insert_with_low.
      apply cmp_eq in Hk. subst.
      eapply Valid.value_switch.
      apply H1.
    * unfold insert. simpl. rewrite Hk.
      f_equal.
      apply IHm.
      apply Valid.head in H1.
      tauto.
  Qed.

  Lemma reorder_insert_delete {K: Set} {V: Set} `{OrdDec K}:
    forall (m: Mp K V), ValidMap m ->
        forall k1 k2 v f,
          k1 <> k2 ->
            insert_with f k1 v (delete k2 m) =
              delete k2 (insert_with f k1 v m).
  Proof.
    induction m; intros; simpl.
    * destruct (cmp k2 k1) eqn: KK; auto.
      apply cmp_eq in KK. subst. contradiction.
    * destruct a as [ck cv].
      destruct (cmp k2 ck) eqn:K1; simpl; destruct (cmp k1 ck) eqn:K2; simpl; try rewrite K1; auto;
        try (
          destruct (cmp k2 k1) eqn: KK; auto; repeat inequalities; try contradiction
        ).
      + apply Insert.insert_with_low.
        apply Valid.head.
        apply Valid.head in H1.
        split; try tauto.
        apply Forall_forall.
        intro x.
        destruct x as [kx vx]. intros.
        destruct H1.
        epose proof (Forall_forall _ m).
        destruct H5.
        eapply H5 in H4; eauto.
        simpl in H4.
        apply cmp_correct. apply cmp_correct in H4. inequalities.

      + f_equal. apply IHm; auto. apply Valid.head in H1. tauto.
      + f_equal. apply IHm; auto. apply Valid.head in H1. tauto.
  Qed.
End Delete.

Fixpoint lookup {K: Set} {V: Set} `{OrdDec K} (k: K) (m: Mp K V): option V :=
  match m with
  | [] => None
  | ((ck, v)::xs) =>
      match cmp k ck with
      | EQ => Some v
      | GT => lookup k xs
      | LT => None
      end
  end.

Module Lookup.
  Lemma lookup_insert_with {K: Set} {V: Set} `{OrdDec K}: forall (mp: Mp K V), ValidMap mp -> forall f k v,
    lookup k (insert_with f k v mp) = match lookup k mp with | Some x => Some (f x) | None => Some v end.
  Proof.
    intro m.
    induction m; intros; simpl.
    * rewrite cmp_refl. reflexivity.
    * destruct a as [ck cv]. inversion H1; subst; simpl.
      { destruct (cmp k ck) eqn: KCK; simpl; try rewrite cmp_refl; auto.
        rewrite KCK. reflexivity. }
      { specialize (IHm H6 f k v).
        simpl in IHm.
        destruct (cmp k ck) eqn: KCK; simpl in *; try rewrite cmp_refl; auto.
        rewrite KCK.
        destruct (cmp k k2) eqn: K2K2; simpl in *; try rewrite cmp_refl; auto.
      }
  Qed.

  Lemma lookup_insert_with_diff {K: Set} {V: Set} `{OrdDec K}: forall (mp: Mp K V), ValidMap mp -> forall f k1 k2 v,
    k1 <> k2 ->
    lookup k1 (insert_with f k2 v mp) = lookup k1 mp.
  Proof.
    induction mp; intros; simpl.
    * destruct (cmp k1 k2) eqn: CC; auto.
      apply cmp_eq in CC. contradiction.
    * destruct a as [ck cv].
      inversion H1; subst; simpl.
      { destruct (cmp k2 ck) eqn: K2CK; destruct (cmp k1 ck) eqn: K1CK; simpl; destruct (cmp k1 k2) eqn: K1K2; auto; 
        repeat inequalities; try contradiction; auto.
      }
      { specialize (IHmp H7 f k1 k2 v H2).
        destruct (cmp k2 ck) eqn: K2CK; destruct (cmp k2 k3) eqn: K2K3; simpl; destruct (cmp k1 ck) eqn: K1CK; auto; repeat inequalities;
          auto; apply cmp_eq_not in H2.
        * destruct (cmp k1 k2) eqn: K1K2; auto; try contradiction.
        * destruct (cmp k1 k2) eqn: K1K2; auto; try contradiction. inequalities.
        * destruct (cmp k1 k2) eqn: K1K2; auto; try contradiction.
        * destruct (cmp k1 k2) eqn: K1K2; auto; try contradiction; repeat inequalities.
        * rewrite cmp_refl in H2. contradiction.
        * rewrite cmp_refl in H2. contradiction.
        * destruct (cmp k1 k2) eqn: K1K2; auto; try contradiction. replace (cmp k1 k3) with LT; auto. symmetry. inequalities.
        * destruct (cmp k1 k3) eqn: K1K3; auto; try contradiction.
        * destruct (cmp k1 k3) eqn: K1K3; auto; try contradiction.
          simpl in IHmp.
          rewrite K2K3 in IHmp.
          rewrite K1K3 in IHmp.
          simpl in IHmp.
          rewrite K1K3 in IHmp.
          assumption.
      }
  Qed.

  Lemma lookup_insert {K: Set} {V: Set} `{OrdDec K}: forall (mp: Mp K V), ValidMap mp -> forall k v,
      lookup k (insert k v mp) = Some v.
  Proof.
    intros.
    pose proof (lookup_insert_with mp H1 (fun _ : V => v) k v).
    unfold insert.
    rewrite H2.
    destruct (lookup k mp); reflexivity.
  Qed.

  Lemma lookup_delete {K: Set} {V: Set} `{OrdDec K}:
        forall (mp: Mp K V), ValidMap mp -> forall k,
          lookup k (delete k mp) = None.
  Proof.
    induction mp; intros; auto.
    destruct a as [ck v].
    simpl.
    inversion H1; subst.
    destruct (cmp k ck) eqn: CK; auto; simpl; rewrite CK; reflexivity.
    specialize (IHmp H6).
    apply cmp_correct in H4.
    destruct (cmp k ck) eqn: CK; auto; simpl; try (rewrite CK);
    destruct (cmp k k2) eqn: K2; auto; repeat inequalities.
    {
      specialize (IHmp k).
      simpl in IHmp. rewrite K2 in IHmp. auto.
    }
    {
      specialize (IHmp k2). simpl in IHmp. rewrite cmp_refl in IHmp. auto.
    }
    {
      specialize (IHmp k).
      simpl in IHmp. rewrite K2 in IHmp. auto.
    }
  Qed.

  Lemma lowkey_not_found {K: Set} {V: Set} `{OrdDec K}: forall (mp: Mp K V) k,
    ValidMap mp ->
     Forall (fun pr : K * V => let (ck, _) := pr in lt k ck) mp ->
     lookup k mp = None.
  Proof.
    destruct mp; intros; auto.
    simpl.
    destruct p.
    rewrite Valid.head in H1.
    rewrite Forall_cons_iff in H2.
    destruct H2.
    apply cmp_correct in H2.
    rewrite H2.
    reflexivity.
  Qed.

  Lemma update_eq {K: Set} {V: Set} `{OrdDec K}: forall k f (mp: Mp K V) res,  ValidMap mp -> lookup k (update f k mp) = res ->
      (lookup k mp = None \/ res = f None) \/ (exists r, lookup k mp = Some r /\ lookup k (update f k mp) = f (Some r)).
  Proof.
    induction mp; intros; auto.
    destruct a.
    simpl in H2.
    simpl.
    apply Valid.head in H1.
    destruct (cmp k k0) eqn: KK0; repeat inequalities; simpl in *; repeat inequalities.
    + subst. tauto.
    + right. exists v. split; auto.
      destruct (f (Some v)) eqn: FV; simpl.
      - rewrite cmp_refl. reflexivity.
      - apply lowkey_not_found;tauto.
    + apply IHmp; tauto.
  Qed.

  Lemma update_diff {K V: Set} `{OrdDec K}: forall (mp: Mp K V) f k1 k2,
    ValidMap mp ->
    k1 <> k2 -> lookup k1 (update f k2 mp) = lookup k1 mp.
Proof.
  induction mp; intros; simpl; auto. {
    destruct (f None); auto.
    simpl.
    destruct (cmp k1 k2) eqn: E; auto.
    apply cmp_eq in E. contradiction.
  }
  destruct a as [ck cv].
  destruct (cmp k2 ck) eqn:K2; simpl; repeat inequalities.
  * destruct (f None); simpl; destruct (cmp k1 ck) eqn: K1CK; auto; repeat inequalities; auto;
      destruct (cmp k1 k2) eqn: K1K2; repeat inequalities; auto; contradiction.
  * destruct (f (Some cv)); simpl; destruct (cmp k1 ck) eqn: K1CK; auto; repeat inequalities;
      try contradiction.
    apply Valid.head in H1.
    apply lowkey_not_found; try tauto.
    apply Forall_forall.
    intros.
    destruct x.
    destruct H1.

    destruct (Forall_forall (fun pr : K * V => let (ck0, _) := pr in lt ck ck0) mp) as [FR _].
    specialize (FR H4 (k, v) H3). simpl in FR. apply cmp_correct in K1CK. eapply lt_trans; eauto.
  * destruct (cmp k1 ck) eqn: K1CK; auto.
    apply Valid.head in H1.
    apply IHmp; tauto.
Qed.

Lemma update_eq_b {K V: Set} `{OrdDec K}: forall (mp: Mp K V) f k,
    f None = None ->
    ValidMap mp -> lookup k (update f k mp) = f (lookup k mp).
Proof.
  induction mp; intros; simpl; auto. {
      destruct (f None); auto.
      simpl.
      rewrite cmp_refl.
      reflexivity.
  }
  destruct a as [ck cv]. apply Valid.head in H2. destruct H2.
  destruct (cmp k ck) eqn:KCK; simpl; repeat inequalities.
  * rewrite H1. simpl. rewrite KCK. reflexivity.
  * destruct (f (Some cv)) eqn:FCV; simpl.
    + rewrite cmp_refl. reflexivity.
    + apply lowkey_not_found; tauto.
  * apply IHmp; auto.
Qed.

  Section MapLemmas.
    Context {K V: Set} `{OrdDec K}.

    Lemma lt_head_none: forall (m: Mp K V) k k0, ValidMap m -> Forall (fun pr : K * V => let (ck, _) := pr in lt k0 ck) m ->
      cmp k k0 <> GT -> lookup k m = None.
    Proof.
      intros m k k0 VM F C. apply lowkey_not_found; auto.
      eapply Forall_impl; [|exact F]. intros [ck cv] L. simpl in *.
      destruct (cmp k k0) eqn: E; [|apply cmp_eq in E; subst; assumption|contradiction].
      apply cmp_correct in E. eapply lt_trans; eauto.
    Qed.

    Lemma map_ext_valid: forall (a b: Mp K V), ValidMap a -> ValidMap b -> (forall k, lookup k a = lookup k b) -> a = b.
    Proof.
      induction a as [|[k1 v1] a IH]; intros b VA VB E.
      - destruct b as [|[k2 v2] b]; auto. specialize (E k2). simpl in E. rewrite cmp_refl in E. discriminate.
      - destruct b as [|[k2 v2] b].
        + specialize (E k1). simpl in E. rewrite cmp_refl in E. discriminate.
        + apply Valid.head in VA. apply Valid.head in VB. destruct VA as [VA FA], VB as [VB FB].
          destruct (cmp k1 k2) eqn: C.
          * specialize (E k1). simpl in E. rewrite cmp_refl, C in E. discriminate.
          * apply cmp_eq in C. subst k2.
            pose proof (E k1) as E1. simpl in E1. rewrite cmp_refl in E1. inversion E1; subst v2. f_equal.
            apply IH; auto. intro k. specialize (E k). simpl in E.
            destruct (cmp k k1) eqn: D; try assumption;
              (rewrite (lt_head_none a k k1), (lt_head_none b k k1); auto; rewrite D; discriminate).
          * apply cmp_opp in C. specialize (E k2). simpl in E. rewrite cmp_refl, C in E. discriminate.
    Qed.

    Lemma lookup_delete_diff: forall (m: Mp K V) k1 k2, ValidMap m -> k1 <> k2 -> lookup k1 (delete k2 m) = lookup k1 m.
    Proof.
      induction m as [|[ck cv] m IH]; intros k1 k2 VM D; simpl; auto.
      apply Valid.head in VM. destruct VM as [VM F].
      destruct (cmp k2 ck) eqn: C; simpl; auto.
      - apply cmp_eq in C. subst ck. destruct (cmp k1 k2) eqn: C1; auto.
        + apply lt_head_none with (k0 := k2); auto. rewrite C1. discriminate.
        + apply cmp_eq in C1. contradiction.
      - rewrite IH; auto.
    Qed.

    Lemma lookup_filter: forall (m: Mp K V) p k, ValidMap m ->
      lookup k (filter p m) = match lookup k m with Some v => if p (k, v) then Some v else None | None => None end.
    Proof.
      induction m as [|[ck cv] m IH]; intros p k VM; simpl; auto.
      apply Valid.head in VM. destruct VM as [VM F].
      assert (cmp k ck <> GT -> lookup k (filter p m) = None) as NONE.
      { intro C. apply lt_head_none with (k0 := ck); auto.
        - apply Valid.filter. assumption.
        - apply Forall_forall. intros x I. apply filter_In in I. destruct I as [I _].
          rewrite Forall_forall in F. apply F. assumption. }
      destruct (cmp k ck) eqn: C; destruct (p (ck, cv)) eqn: P; simpl; rewrite ?C.
      - reflexivity.
      - apply NONE. discriminate.
      - apply cmp_eq in C. subst. rewrite P. reflexivity.
      - apply cmp_eq in C. subst. rewrite P. apply NONE. discriminate.
      - apply IH. assumption.
      - apply IH. assumption.
    Qed.
  End MapLemmas.
End Lookup.

Definition St (K: Set) := Mp K unit.

Definition s_set {K: Set} `{OrdDec K} (k: K) (s: St K) := insert k tt s.
Definition s_unset {K: Set} `{OrdDec K} (k: K) (s: St K) := delete k s.
Definition s_check {K: Set} `{OrdDec K} (k: K) (s: St K): bool :=
  match lookup k s with
  | None => false
  | Some tt => true
  end.

Lemma s_set_set {K: Set} `{OrdDec K}:
  forall (k: K) (s: St K), s_set k (s_set k s) = s_set k s.
Proof.
  intros k s.
  apply Insert.insert_insert.
Qed.

Lemma s_set_unset {K: Set} `{OrdDec K}:
  forall (k: K) (s: St K), ValidMap s -> s_set k (s_unset k s) = s_set k s.
Proof.
  intros k s VM.
  apply Delete.insert_delete.
  auto.
Qed.

Lemma s_unset_set {K: Set} `{OrdDec K}:
  forall (k: K) (s: St K), ValidMap s -> s_unset k (s_set k s) = s_unset k s.
Proof.
  intros k s.
  apply Delete.delete_insert.
Qed.

Lemma s_set_set_reorder {K: Set} `{OrdDec K}:
  forall (k1: K) (k2: K) (s: St K), k1 <> k2 -> s_set k1 (s_set k2 s) = s_set k2 (s_set k1 s).
Proof.
  intros k1 k2 s.
  apply Insert.reorder_insert.
Qed.

Lemma s_set_unset_reorder {K: Set} `{OrdDec K}:
  forall (k1: K) (k2: K) (s: St K), ValidMap s -> k1 <> k2 -> s_set k1 (s_unset k2 s) = s_unset k2 (s_set k1 s).
Proof.
  intros vm k1 k2 s.
  apply Delete.reorder_insert_delete; auto.
Qed.

Lemma s_unset_set_reorder {K: Set} `{OrdDec K}:
  forall (k1: K) (k2: K) (s: St K), ValidMap s -> k1 <> k2 -> s_unset k1 (s_set k2 s) = s_set k2 (s_unset k1 s).
Proof.
  intros k1 k2 s vm df.
  unfold s_unset, s_set, insert.
  rewrite Delete.reorder_insert_delete; auto.
Qed.

Lemma s_check_set {K: Set} `{OrdDec K}: forall (m: St K) x y, ValidMap m ->
  s_check y (s_set x m) = if eqb y x then true else s_check y m.
Proof.
  intros m x y V. unfold s_check, s_set, insert. destruct (eqb y x) eqn: E.
  - apply eqb_correct in E. subst. rewrite Lookup.lookup_insert_with; auto. destruct (lookup x m); reflexivity.
  - rewrite Lookup.lookup_insert_with_diff; auto. intro C. subst. rewrite eqb_refl in E. discriminate.
Qed.

Lemma s_check_unset {K: Set} `{OrdDec K}: forall (m: St K) x y, ValidMap m ->
  s_check y (s_unset x m) = if eqb y x then false else s_check y m.
Proof.
  intros m x y V. unfold s_check, s_unset. destruct (eqb y x) eqn: E.
  - apply eqb_correct in E. subst. rewrite Lookup.lookup_delete; auto.
  - rewrite Lookup.lookup_delete_diff; auto. intro C. subst. rewrite eqb_refl in E. discriminate.
Qed.

Definition foldMap {K V: Set} (f: V -> V -> V) (e: V) (m: Mp K V) := fold_right f e (map snd m).

Module FoldMap.

  Lemma empty {K V: Set}: forall f e, foldMap f e ([]: Mp K V) = e.
  Proof.
    intros.
    unfold foldMap.
    auto.
  Qed.

  Lemma cons {K V: Set}: forall f e k v (m: Mp K V),
        foldMap f e ((k,v)::m) = f v (foldMap f e m).
  Proof.
    unfold foldMap.
    intros.
    simpl.
    auto.
  Qed.

  Lemma merge {K V: Set} `{OrdDec K}:
      forall f e (m1 m2: Mp K V),
          (forall x y z, f x (f y z) = f (f x y) z) ->
          (forall n, f e n = n) ->
          (forall a b, f a b = f b a) ->
          f (foldMap f e m1) (foldMap f e m2) = foldMap f e (merge_with f m1 m2).
  Proof.
    intros f e m1 m2 fassoc femptyl fcomm.
    assert (forall n, f n e = n) as femptyr.
    { intros. rewrite fcomm. apply femptyl. }
    remember (merge_with f m1 m2) as out.
    generalize dependent m2.
    generalize dependent m1.
    induction out; intros; symmetry in Heqout.
    { apply Merge.empty in Heqout. destruct Heqout. subst. unfold foldMap.
      simpl. apply femptyr.
    }
    destruct m1 as [|[k1 v1] m1] .
    { rewrite Merge.nil_left in Heqout.
      subst.
      rewrite empty.
      rewrite femptyl.
      reflexivity.
    }
    destruct m2 as [|[k2 v2] m2] .
    { rewrite Merge.nil_right in Heqout.
      inversion Heqout; subst. clear Heqout.
      rewrite empty.
      repeat (rewrite cons).
      rewrite femptyr.
      reflexivity.
    }
    apply Merge.inductive_correct in Heqout.
    inversion Heqout; subst.
    {
      repeat (rewrite cons).

      rewrite <- fassoc.
      rewrite (fassoc _ v2 _).
      rewrite (fcomm _ v2).
      rewrite fassoc.
      rewrite fassoc.
      rewrite <- fassoc.
      f_equal.
      apply IHout.
      symmetry.
      apply Merge.inductive_correct; auto.
    }
    {
      rewrite (cons _ _ _ _ m1).
      rewrite (cons _ _ _ _ out).
      rewrite <- fassoc.
      f_equal.
      apply IHout.
      apply Merge.inductive_correct in H11. auto.
    }
    {
      rewrite (cons _ _ _ _ m2).
      rewrite (cons _ _ _ _ out).
      rewrite fcomm.
      rewrite <- fassoc.
      f_equal.
      rewrite fcomm.
      apply IHout.
      apply Merge.inductive_correct in H11. auto.
    }
  Qed.

End FoldMap.

Definition MpMap {K V: Set} (f: V -> V) (m: Mp K V) := map (fun (kv: K*V) => 
      let (k, v) := kv in (k, f v)) m.



Definition from_list {K: Set} `{OrdDec K} {V}
  (l: list (K * V)): Mp K V :=
  fold_right (fun (kv: K * V) (curmap : Mp K V) => let (k, v) := kv in insert k v curmap) [] l.

Lemma from_list_correct {K: Set} `{OrdDec K} {V}:
  forall l (mp: Mp K V), mp = from_list l -> ValidMap mp.
Proof.
  unfold from_list.
  induction l; intros; simpl in *.
  * subst. constructor.
  * subst. destruct a. simpl in *.
    unfold insert.
    apply Insert.insert_with_valid.
    apply IHl.
    reflexivity.
Qed.
  
Definition set_from_list {K: Set} `{OrdDec K} (l: list K): St K :=
  fold_right (fun (k: K) (curset : St K) => s_set k curset) [] l.

Lemma vmap_in {K V: Set} `{OrdDec K}: forall (m: Mp K V) k q,
    ValidMap m ->
    In (k, q) m <-> lookup k m = Some q.
Proof.
  intros m k q VMM.
  induction VMM; split; intros.
  * inversion H1.
  * inversion H1.
  * inversion H1; subst; clear H1; simpl.
    rewrite cmp_refl. reflexivity.
    inversion H2.
  * simpl in H1.
    destruct kv as [ck v].
    destruct (cmp k ck) eqn: KCK; try discriminate.
    inversion H1; subst.
    apply cmp_eq in KCK.
    subst.
    left.
    reflexivity.
  * simpl in *.
    destruct H2 as [EQ1|[EQ1|EQ2]]; try (inversion EQ1; subst; clear EQ1); try rewrite cmp_refl; auto.
    + apply cmp_correct in H1.
      inequalities.
      inequalities.
      reflexivity.
    + apply cmp_correct in H1.
      destruct (cmp k k1) eqn: KK1, (cmp k k2) eqn: KK2; auto; repeat inequalities; try apply IHVMM; try tauto.
      apply Valid.head in VMM.
      destruct VMM.
      eapply Forall_forall in H3; eauto. simpl in H3. apply cmp_correct in H3.
      repeat inequalities.
  *  simpl in *.
      destruct (cmp k k1) eqn: KK1, (cmp k k2) eqn: KK2; auto; repeat inequalities; try apply IHVMM; try tauto; try discriminate.
      + inversion H2; subst; clear H2.
        tauto.
      + inversion H2; subst; clear H2.
        tauto.
Qed.

Lemma vmap_out {K V: Set} `{OrdDec K}: forall (m: Mp K V) k,
    ValidMap m ->
    (forall q, ~ In (k, q) m) <-> lookup k m = None.
Proof.
  intros m k VMM.
  induction VMM; split; intros; auto.
  * destruct kv.
    simpl.
    destruct (cmp k k0) eqn: KK0; auto.
    apply cmp_eq in KK0.
    subst.
    exfalso.
    apply (H1 v).
    constructor.
    reflexivity.
  * intro contra.
    inversion contra; subst; simpl in H1.
    rewrite cmp_refl in H1. discriminate.
    inversion H2.
  * simpl in *.
    destruct (cmp k k1) eqn: KK1, (cmp k k2) eqn: KK2; auto; repeat inequalities.
    + exfalso. apply (H2 v1). tauto.
    + exfalso. apply (H2 v1). tauto.
    + exfalso. apply (H2 v2). tauto.
    + apply IHVMM.
      intros.
      intro contra.
      destruct contra.
      - inversion H3; subst.
        apply (H2 q).
        tauto.
      - apply (H2 q).
        tauto.
  * intro contra.
    simpl in H2.
    inversion contra; subst; clear contra.
    - inversion H3; subst; clear H3. rewrite cmp_refl in H2. discriminate.
    - inversion H3; subst; clear H3.
     -- inversion H4; subst; clear H4.
        rewrite cmp_refl in H2.
        apply cmp_correct in H1.
        apply cmp_opp in H1.
        rewrite H1 in H2.
        discriminate.
     -- destruct (cmp k k1) eqn: KK1, (cmp k k2) eqn: KK2; try discriminate; simpl in *; rewrite KK2 in IHVMM; destruct IHVMM; repeat inequalities.
       ++ lapply H5; auto. intros. specialize (H6 q). tauto.
       ++ apply cmp_correct in H1. inequalities.
       ++ lapply H5; auto. intros. specialize (H6 q). tauto.
       ++ lapply H5; auto. intros. specialize (H6 q). tauto.
  Qed.

Section Bag.
  Context {K: Set} `{EqDec K} `{OrdDec K}.


  Definition ValidBag (m: Mp K nat) := ValidMap m /\ Forall (fun kv => snd kv =? 0 = false)%nat m.

  Definition bag_add (k: K) (q: nat) (m: Mp K nat) :=
    insert_with (fun prev => prev + q)%nat k q m.
  
  Definition bag_rm (k: K) (q: nat) (m: Mp K nat) :=
    update (fun mv => match mv with | None => None | Some cv => if cv <=? q then None else Some (cv - q)%nat end) k m.

  Lemma add_valid: forall (m: Mp K nat) k q,
    ValidBag m -> (q > 0)%nat -> ValidBag (bag_add k q m).
  Proof.
    intros.
    unfold ValidBag in *.
    split.
    * apply Insert.insert_with_valid.
      tauto.
    * induction m; simpl.
      + constructor. 
        simpl.
        apply Nat.eqb_neq.
        Lia.lia.
        tauto.
      + unfold bag_add in *.
        simpl.
        destruct a as [ck cv].
        destruct H2 as [VMM APOS].
        inversion APOS; subst; clear APOS.
        destruct (cmp k ck) eqn: KCK.
        - apply cmp_correct in KCK.
          constructor; simpl.
          apply Nat.eqb_neq.
          Lia.lia.
          constructor; simpl; auto.
        - constructor; auto.
          simpl.
          apply Nat.eqb_neq.
          Lia.lia.
        - constructor; auto.
          apply IHm.
          apply Valid.head in VMM.
          tauto.
  Qed.

  Lemma add_valid_no_zero: forall (m: Mp K nat) k q,
    ValidBag m -> (q > 0)%nat -> Forall (fun kv => snd kv =? 0 = false)%nat (bag_add k q m).
  Proof.
    intros.
    destruct (add_valid m k q H2 H3).
    tauto.
  Qed.

  Lemma rm_valid: forall (m: Mp K nat) k q,
    ValidBag m -> ValidBag (bag_rm k q m).
  Proof.
    unfold ValidBag.
    split; intros.
    * apply Update.valid. tauto.
    * destruct H2 as [VMM APOS].
      induction m; simpl; unfold bag_rm; simpl.
      + constructor.
      + destruct a as [ck cv].
        inversion APOS; subst; clear APOS.
        apply Valid.head in VMM.
        simpl in *.
        destruct (cmp k ck) eqn: KCK.
        - constructor; auto.
        - apply cmp_eq in KCK. subst.
          destruct (cv <=? q) eqn: LT; auto.
          constructor; auto.
          simpl.
          apply Nat.eqb_neq.
          apply Nat.leb_gt in LT.
          Lia.lia.
        - constructor; auto.
          apply IHm; auto.
          tauto.
  Qed.

  Lemma lookup_cant_be_zero: forall (m: Mp K nat) k,
    ValidBag m -> lookup k m = Some O -> False.
  Proof.
    intros.
    destruct H2.
    apply vmap_in in H3; auto.
    eapply Forall_forall in H4; eauto.
    simpl in H4.
    discriminate.
  Qed.

  Definition has (k: K) (m: Mp K nat): bool := match lookup k m with | Some O | None => false | Some _ => true end.

  Lemma has_rm_preserve: forall (m: Mp K nat) k rm q, ValidBag m -> k <> rm -> has k m = has k (bag_rm rm q m).
  Proof.
    induction m; intros; unfold has; simpl; auto.
    unfold bag_rm.
    destruct a as [ck v].
    destruct H2.
    apply Valid.head in H2.
    destruct H2.
    inversion H4; subst; clear H4. simpl in *.
    destruct (cmp k ck) eqn: KCK; simpl; destruct (cmp rm ck) eqn: RMCK; simpl; repeat inequalities; auto; try contradiction.
    * destruct (v <=? q); simpl; repeat inequalities; auto.
      replace (lookup k m) with (None : option nat). reflexivity.
      symmetry.
      apply Lookup.lowkey_not_found; auto.
      apply Forall_forall; intros.
      destruct x.
      eapply Forall_forall in H5; eauto. simpl in H5. apply cmp_correct in KCK. eapply lt_trans; eauto.
    * destruct (v <=? q); simpl; repeat inequalities; auto.
    * assert (ValidBag m). {
        unfold ValidBag.
        tauto.
      }
      specialize (IHm k rm q H4 H3).
      unfold has in IHm.
      rewrite IHm.
      reflexivity.
  Qed.
End Bag.