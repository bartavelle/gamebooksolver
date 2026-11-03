(* Decidable equality and total orders, for the types used as keys of the maps. *)
Require Import Stdlib.Logic.FunctionalExtensionality.
Require Import Stdlib.Lists.List.
Require Import Stdlib.Arith.Compare_dec.
Require Import Stdlib.QArith.Qcanon.
From Equations Require Import Equations.
From Stdlib Require Lia.
Require Import ZArith.
Import ListNotations.

Class EqDec (T: Type) := {
  eqb : T -> T -> bool;
  eqb_correct : forall (x y: T), (x = y) <-> eqb x y = true
}.

Lemma eqb_refl {T: Type} `{EqDec T}: forall x, eqb x x = true.
Proof.
  intros.
  apply eqb_correct.
  reflexivity.
Qed.

Lemma eqdec_dec {T: Type} `{EqDec T} : forall (a b: T), {a = b} + {a <> b}.
Proof.
  intros.
  destruct (eqb a b) eqn:H2.
  - left. apply eqb_correct. assumption.
  - right. intro Heq. apply eqb_correct in Heq. congruence.
Qed.

Inductive Order := LT | EQ | GT.

Class OrdDec (T: Type) `(EqDec T) := {
  lt : T -> T -> Prop;
  cmp: T -> T -> Order;
  cmp_correct: forall a b, cmp a b = LT <-> lt a b;
  cmp_opp: forall a b, cmp a b = LT <-> cmp b a = GT;
  cmp_trans: forall r a b c, cmp a b = r -> cmp b c = r -> cmp a c = r;
  cmp_eq: forall a b, cmp a b = EQ <-> a = b;
}.

Lemma eq_refl {T: Type} `{EqDec T}: forall x, eqb x x = true.
Proof.
  intros.
  apply eqb_correct. reflexivity.
Qed.

Lemma eq_dec {T: Type} `{EqDec T}:
    forall (x y : T), {x = y} + {x <> y}.
Proof.
  intros.
  destruct (eqb x y) eqn: E.
  left. apply eqb_correct. assumption.
  right.
  intro contra.
  apply eqb_correct in contra.
  rewrite E in contra.
  discriminate.
Qed.

Lemma cmp_refl {T: Type} `{OrdDec T}: forall x, cmp x x = EQ.
Proof.
  intros.
  apply cmp_eq. reflexivity.
Qed.

Lemma eqb_not {T: Type} `{EqDec T}: forall (x y: T), x <> y <-> eqb x y = false.
Proof.
  split; intros.
  * destruct (eqb x y) eqn:l; auto.
    apply eqb_correct in l.
    contradiction.
  * intro Neg.
    subst.
    rewrite (eq_refl y) in H0.
    inversion H0.
Qed.

Lemma lt_not {T: Type} `{OrdDec T}: forall (x: T), lt x x -> False.
Proof.
  intros.
  apply cmp_correct in H1.
  pose proof (cmp_opp x x).
  destruct H2. specialize (H2 H1). rewrite H1 in H2. discriminate.
Qed.

Lemma lt_trans {T: Type} `{OrdDec T}: forall a b c, lt a b -> lt b c -> lt a c.
Proof.
  intros.
  apply cmp_correct in H1, H2. apply cmp_correct.
  eapply cmp_trans; eauto.
Qed.

Lemma cmp_antitrans {T: Type} `{OrdDec T}: forall r a b c, 
  cmp a b = r -> cmp b c = r -> cmp a c <> r -> False.
Proof.
  intros.
  pose proof (cmp_trans r a b c H1 H2).
  contradiction.
Qed.

Lemma gt_ne {T: Type} `{OrdDec T}: forall a b,
  cmp a b = GT -> a <> b.
Proof.
  intros.
  intro contra.
  subst.
  rewrite cmp_refl in H1.
  discriminate.
Qed.

Lemma lt_ne {T: Type} `{OrdDec T}: forall a b,
  cmp a b = LT -> a <> b.
Proof.
  intros.
  intro contra.
  subst.
  rewrite cmp_refl in H1.
  discriminate.
Qed.

Lemma gt_not_lt: forall x, x = GT -> x <> LT.
Proof.
  intros.
  intro contra. subst. discriminate.
Qed.

Lemma cmp_eq_not {T: Type} `{OrdDec T}: forall (a b : T), a <> b -> cmp a b <> EQ.
Proof.
  intros.
  intro contra.
  apply cmp_eq in contra.
  contradiction.
Qed.

Lemma eqb_cmp_lt {T: Type} `{OrdDec T}: forall (a b: T), cmp a b = LT -> eqb a b = false.
Proof.
  intros.
  apply eqb_not.
  intro contra.
  subst. rewrite cmp_refl in H1. discriminate.
Qed.

Lemma eqb_cmp_gt {T: Type} `{OrdDec T}: forall (a b: T), cmp a b = GT -> eqb a b = false.
Proof.
  intros.
  apply eqb_not.
  intro contra.
  subst. rewrite cmp_refl in H1. discriminate.
Qed.

Lemma eqb_comm {T: Type} `{OrdDec T}: forall a b, eqb a b = eqb b a.
Proof.
  intros.
  destruct (eq_dec a b).
  * subst. reflexivity.
  * destruct (eqb_not a b) as [A _].
    destruct (eqb_not b a) as [B _].

    specialize (A n).
    apply not_eq_sym in n.
    specialize (B n).
    rewrite A.
    rewrite B.
    reflexivity.
Qed.

Ltac inequalities :=
  match goal with
  | H1: cmp ?x ?y = ?r1, H2: cmp ?x ?y = ?r2 |- _ =>
      first [ constr_eq r1 r2; fail 1  (* if equal, fail this branch *)
            | rewrite H1 in H2 ]
  | H1: cmp ?x ?y = LT, H2: lt ?y ?x |- _ =>
      apply cmp_correct in H2
  | H1: cmp ?x ?y = GT, H2: lt ?x ?y |- _ =>
      apply cmp_correct in H2
  | H1: cmp ?x ?y = LT, H2: cmp ?y ?x = LT |- _ =>
      apply cmp_opp in H1; rewrite H1 in H2
  | H1: cmp ?x ?y = GT, H2: cmp ?y ?x = GT |- _ =>
      apply cmp_opp in H1; rewrite H1 in H2
  | H4 : cmp ?ck ?k2 = LT,
    CK : cmp ?k ?ck = LT,
    K2 : cmp ?k ?k2 = GT |- _ =>
      pose proof (cmp_antitrans LT k ck k2 CK H4 (gt_not_lt _ K2)); contradiction
  | H: cmp ?k ?k = ?x |- _ => rewrite cmp_refl in H
  | H: cmp _ _ = EQ |- _ => apply cmp_eq in H; subst
  | H: EQ = LT |- _ => discriminate
  | H: EQ = GT |- _ => discriminate
  | H: LT = EQ |- _ => discriminate
  | H: LT = GT |- _ => discriminate
  | H: GT = LT |- _ => discriminate
  | H: GT = EQ |- _ => discriminate
  | H1: cmp ?a ?b = ?r, H2: cmp ?b ?c = ?r |- cmp ?a ?c = ?r => eapply cmp_trans;eauto
  | H: cmp ?a ?b = LT |- context [cmp ?a ?b] => rewrite H
  | H: cmp ?a ?b = LT |- context [cmp ?b ?a] => apply cmp_opp in H
  | H: cmp ?a ?b = GT |- context [cmp ?a ?b] => rewrite H
  | H: cmp ?a ?b = GT |- context [cmp ?b ?a] => apply cmp_opp in H
  | |- context [cmp ?a ?a] => rewrite cmp_refl
  | H: cmp ?a ?b = LT |- context [eqb ?a ?b] => rewrite (eqb_cmp_lt a b H)
  | H: cmp ?a ?b = GT |- context [eqb ?a ?b] => rewrite (eqb_cmp_gt a b H)
  | H: cmp ?a ?b = LT |- context [eqb ?b ?a] => rewrite (eqb_comm b a)
  | H: cmp ?a ?b = GT |- context [eqb ?b ?a] => rewrite (eqb_comm b a)
  | H: cmp ?a ?b = LT, H2: context [eqb ?a ?b] |- _ => rewrite (eqb_cmp_lt a b H) in H2
  | H: cmp ?a ?b = GT, H2: context [eqb ?a ?b] |- _ => rewrite (eqb_cmp_gt a b H) in H2
  | H: cmp ?a ?b = LT, H2: context [eqb ?b ?a] |- _ => rewrite (eqb_comm b a) in H2
  | H: cmp ?a ?b = GT, H2: context [eqb ?b ?a] |- _ => rewrite (eqb_comm b a) in H2
  | H: cmp ?a ?b = _, H2: context [cmp ?a ?b] |- _ => rewrite H in H2
  | |- context [eqb ?a ?a] => rewrite eq_refl
  end. 

Module PairLemma.

  Definition pair_eqb {X Y: Type} `{EqDec X} `{EqDec Y} (a b: X * Y): bool :=
    let (ka, va) := a in
    let (kb, vb) := b in
    eqb ka kb && eqb va vb.

  Lemma pair_eqb_correct {X Y: Type} `{EqDec X} `{EqDec Y}: forall (a b: X * Y), a = b <-> pair_eqb a b = true.
  Proof.
    intros.
    destruct a as [ka va].
    destruct b as [kb vb].
    simpl. destruct (eqb ka kb) eqn: HK; destruct (eqb va vb) eqn: HB
        ;try (apply eqb_correct in HK)
        ;try (apply eqb_correct in HB)
        ;subst
        ;try contradiction
        ;split
        ;intros
        ;simpl in *
        ;auto
        ;try discriminate
        .
    * inversion H1; subst. rewrite eq_refl in HB. discriminate.
    * inversion H1; subst. rewrite eq_refl in HK. discriminate.
    * inversion H1; subst. rewrite eq_refl in HB. discriminate.
  Qed.

  Definition pair_lt {X Y: Type} `{OrdDec X} `{OrdDec Y} (a b : X * Y) :=
    let (ka, va) := a in
    let (kb, vb) := b in
    lt ka kb \/ (ka = kb /\ lt va vb).

  Definition pair_cmp {X Y: Type} `{OrdDec X} `{OrdDec Y} (a b : X * Y) :=
    let (ka, va) := a in
    let (kb, vb) := b in
    match cmp ka kb with
    | EQ => cmp va vb
    | x => x
    end.

  Lemma pair_cmp_correct {X Y: Type} `{OrdDec X} `{OrdDec Y} : forall (a b: X * Y), pair_cmp a b = LT <-> pair_lt a b.
  Proof.
    intros.
    destruct a as [ka va].
    destruct b as [kb vb].
    unfold pair_cmp, pair_lt.
    destruct (cmp ka kb) eqn:HP; split; intros; auto; try discriminate.
    * left. apply cmp_correct; auto.
    * apply cmp_eq in HP. subst. right. split; auto.
      apply cmp_correct; auto.
    * apply cmp_eq in HP. subst. destruct H3. 
      apply lt_not in H3. contradiction.
      destruct H3. apply cmp_correct;auto.
    * destruct H3.
      apply cmp_correct in H3. rewrite HP in H3. discriminate.
      destruct H3.
      subst. pose proof (cmp_refl kb). rewrite H3 in HP. discriminate.
  Qed.

  Lemma pair_cmp_opp {X Y: Type} `{OrdDec X} `{OrdDec Y} : forall (a b: X * Y), pair_cmp a b = LT <-> pair_cmp b a = GT.
  Proof.
    intros.
    unfold pair_cmp.
    destruct a as [ka va].
    destruct b as [kb vb].
    destruct (cmp ka kb) eqn:HA; try (apply cmp_opp in HA; rewrite HA); split; auto.
    * apply cmp_eq in HA. subst. rewrite cmp_refl. intros. apply cmp_opp. auto.
    * apply cmp_eq in HA. subst. rewrite cmp_refl. intros. apply cmp_opp. auto.
  Qed.

  Lemma pair_cmp_trans {X Y: Type} `{OrdDec X} `{OrdDec Y}:  forall r (a b c: X * Y), pair_cmp a b = r -> pair_cmp b c = r -> pair_cmp a c = r.
  Proof.
    intros.
    unfold pair_cmp in *.
    destruct a as [ka va].
    destruct b as [kb vb].
    destruct c as [kc vc].
    destruct (cmp ka kb) eqn: Ha; subst; destruct (cmp kb kc) eqn: Hb; subst; try discriminate.
    * replace (cmp ka kc) with LT;auto. symmetry. eapply cmp_trans; eauto.
    * apply cmp_eq in Hb. subst. rewrite Ha. auto.
    * apply cmp_eq in Ha. subst. rewrite Hb. auto.
    * apply cmp_eq in Ha, Hb. subst. rewrite cmp_refl.
      { destruct (cmp va vc) eqn: Ka; destruct (cmp vb vc) eqn: Kb; auto; symmetry in H4.
      - apply cmp_eq in Kb, H4. subst. apply cmp_correct in Ka. apply lt_not in Ka. contradiction.
      - apply cmp_opp in Kb, H4. pose proof (cmp_trans _ _ _ _ Ka Kb).
        apply cmp_opp in H3. rewrite H4 in H3. discriminate.
      - pose proof (cmp_trans _ _ _ _ H4 Kb). rewrite Ka in H3. discriminate. 
      - apply cmp_eq in Ka. subst. apply cmp_opp in H4. rewrite Kb in H4. discriminate.
      - pose proof (cmp_trans _ _ _ _ H4 Kb). rewrite Ka in H3. discriminate.
      - rewrite cmp_eq in Kb, H4. subst. rewrite cmp_refl in Ka. discriminate.
      }
    * apply cmp_eq in Ha. subst. rewrite Hb. auto.
    * apply cmp_eq in Hb. subst. rewrite Ha. auto.
    * replace (cmp ka kc) with GT; auto. symmetry. eapply cmp_trans; eauto.
  Qed.

  Lemma pair_cmp_eq {X Y: Type} `{OrdDec X} `{OrdDec Y} : forall (a b: X * Y), pair_cmp a b = EQ <-> a = b.
  Proof.
    intros.
    destruct a as [ka va].
    destruct b as [kb vb].
    unfold pair_cmp.
    destruct (cmp ka kb) eqn:Ha; split; intros; try discriminate.
    * inversion H3; subst.
      rewrite cmp_refl in Ha. discriminate.
    * apply cmp_eq in Ha, H3. subst. reflexivity.
    * inversion H3; subst. apply cmp_refl.
    * inversion H3; subst. rewrite cmp_refl in Ha. discriminate.
  Qed.
End PairLemma.

Instance pair_EqDec {X Y: Type} `{EqDec X} `{EqDec Y}: EqDec (X * Y) := {
  eqb := PairLemma.pair_eqb;
  eqb_correct := PairLemma.pair_eqb_correct;
}.

Instance pair_OrdDec {X Y: Type} `{OrdDec X} `{OrdDec Y}: OrdDec (X * Y) pair_EqDec := {
  lt := PairLemma.pair_lt;
  cmp := PairLemma.pair_cmp;
  cmp_correct := PairLemma.pair_cmp_correct;
  cmp_opp := PairLemma.pair_cmp_opp;
  cmp_trans := PairLemma.pair_cmp_trans;
  cmp_eq := PairLemma.pair_cmp_eq;
}.

Module ListLemma.

  Fixpoint list_eqb {X: Type} `{EqDec X} (l1 l2: list X) :=
      match (l1, l2) with
      | ([], []) => true
      | (x::xs, y::ys) => (eqb x y && list_eqb xs ys)%bool
      | _ => false
      end.

  Lemma list_eqb_correct {X: Type} `{EqDec X} : forall (l1 l2: list X), l1 = l2 <-> list_eqb l1 l2 = true.
  Proof.
    induction l1; intros; split; intros; subst; auto.
    * destruct l2; auto. inversion H0.
    * simpl. rewrite eq_refl. simpl. apply IHl1. auto.
    * simpl in H0. destruct l2. discriminate.
      apply andb_prop in H0.
      destruct H0.
      apply eqb_correct in H0. subst. f_equal. apply IHl1. auto.
  Qed.

  Fixpoint list_cmp {X: Type} `{OrdDec X} (l1 l2: list X) :=
      match (l1, l2) with
      | ([], []) => EQ
      | ([], _) => LT
      | (_, []) => GT
      | (x::xs, y::ys) =>
          match cmp x y with
          | EQ => list_cmp xs ys
          | z => z
          end
      end.

  Definition list_lt {X: Type} `{OrdDec X} (l1 l2: list X) := list_cmp l1 l2 = LT.

  Lemma list_cmp_correct {X: Type} `{OrdDec X} : forall (l1 l2: list X), list_cmp l1 l2 = LT <-> list_lt l1 l2.
  Proof.
    unfold list_lt. intros.
    split;auto.
  Qed.

  Lemma list_cmp_opp {X: Type} `{OrdDec X}: forall (l1 l2: list X), list_cmp l1 l2 = LT <-> list_cmp l2 l1 = GT.
  Proof.
    induction l1; intros; simpl; destruct l2; split; intros; try discriminate;auto.
    { destruct (cmp a x) eqn: Hc; try discriminate; simpl.
      apply cmp_opp in Hc. rewrite Hc; auto.
      apply cmp_eq in Hc. subst. rewrite cmp_refl. apply IHl1. auto.
    }
    { simpl in H1. destruct (cmp x a) eqn: Hx; try discriminate.
      * apply cmp_eq in Hx. subst. rewrite cmp_refl. apply IHl1. auto.
      * apply cmp_opp in Hx. rewrite Hx. auto.
    }
  Qed.

  Lemma list_cmp_trans {X: Type} `{OrdDec X}: forall r (a b c: list X), list_cmp a b = r -> list_cmp b c = r -> list_cmp a c = r.
  Proof.
    intros r a.
    generalize dependent r.
    induction a as [| x xs IHxs]; intros r b c Hab Hbc.
    - (* a = [] *)
      simpl in Hab.
      destruct b as [| y ys].
      + (* b = [] *)
        simpl in Hbc.
        destruct c as [| z zs].
        * (* c = [] *)
          simpl. assumption.
        * (* c = z :: zs *)
          simpl in *. assumption.
      + (* b = y :: ys *)
        simpl in Hab.
        destruct r; try discriminate.
        (* Hab says LT = LT, which is fine *)
        simpl in Hbc.
        destruct c as [| z zs].
        * (* c = [] *)
          simpl in Hbc. discriminate.
        * (* c = z :: zs *)
          simpl. reflexivity.
          
    - (* a = x :: xs *)
      destruct b as [| y ys].
      + (* b = [] *)
        simpl in Hab. subst.
        destruct c; simpl in Hbc; discriminate.
        
      + (* b = y :: ys *)
        destruct c as [| z zs].
        { (* c = [] *)
          simpl in Hbc. subst. simpl in *.
          destruct (cmp x y) eqn: Hxy; try discriminate; auto.
        }
        {
          simpl in *.
          destruct (cmp x y) eqn: Hxy; destruct (cmp y z) eqn: Hyz; subst; simpl in *;
              try discriminate; auto; try apply cmp_eq in Hxy; try apply cmp_eq in Hyz;
              subst; try discriminate; try rewrite Hxy; try rewrite Hyz; auto.
          { replace (cmp x z) with LT; auto. symmetry. eapply cmp_trans; eauto. }
          { rewrite cmp_refl.
            eapply IHxs. reflexivity. auto.
          }
          { replace (cmp x z) with GT; auto. symmetry. eapply cmp_trans; eauto. }
        }
  Qed.

  Lemma list_cmp_eq {X: Type} `{OrdDec X} : forall (a b: list X), list_cmp a b = EQ <-> a = b.
  Proof.
    induction a; split; intros; subst; auto.
    { destruct b; auto. simpl in H1. discriminate.  }
    { simpl in H1. destruct b. discriminate.
      destruct (cmp a x) eqn: Hax; try discriminate.
      apply cmp_eq in Hax. subst. f_equal.
      apply IHa. auto.
    }
    { simpl. rewrite cmp_refl. apply IHa. reflexivity. }
  Qed.

End ListLemma.

Instance list_EqDec {X: Type} `{EqDec X} : EqDec (list X) := {
  eqb := ListLemma.list_eqb;
  eqb_correct := ListLemma.list_eqb_correct;
}.

Instance list_OrdDec {X: Type} `{OrdDec X} : OrdDec (list X) list_EqDec := {
  lt := ListLemma.list_lt;
  cmp := ListLemma.list_cmp;
  cmp_correct := ListLemma.list_cmp_correct;
  cmp_opp := ListLemma.list_cmp_opp;
  cmp_trans := ListLemma.list_cmp_trans;
  cmp_eq := ListLemma.list_cmp_eq;
}.

Module NatLemma.

  Lemma nat_eqb_correct: forall (x y: nat), (x = y) <-> Nat.eqb x y = true.
  Proof.
    intros.
    split; apply PeanoNat.Nat.eqb_eq.
  Qed.

  Definition nat_cmp (a b : nat) : Order :=
    match Nat.compare a b with
    | Datatypes.Lt => LT
    | Datatypes.Eq => EQ
    | Datatypes.Gt => GT
    end.

  Lemma cmp_correct: forall (a b: nat), nat_cmp a b = LT <-> PeanoNat.Nat.lt a b.
  Proof.
    intros; unfold nat_cmp; split; intros.
    * destruct (Nat.compare a b) eqn: CM; simpl in *; try discriminate.
      apply nat_compare_Lt_lt. auto.
    * apply PeanoNat.Nat.compare_lt_iff in H.
      rewrite H. reflexivity.
  Qed.

  Lemma cmp_opp: forall (a b : nat), nat_cmp a b = LT <-> nat_cmp b a = GT.
  Proof.
    intros. unfold nat_cmp.
    destruct (Nat.compare a b) eqn: Ha.
    * apply PeanoNat.Nat.compare_eq_iff in Ha. subst. rewrite PeanoNat.Nat.compare_refl.
      split; intros; discriminate.
    * split; intros; auto.
      apply PeanoNat.Nat.compare_lt_iff in Ha.
      assert (Nat.compare b a = Gt). apply PeanoNat.Nat.compare_gt_iff. auto.
      rewrite H0. reflexivity.
    * apply PeanoNat.Nat.compare_gt_iff in Ha.
      apply PeanoNat.Nat.compare_lt_iff in Ha.
      rewrite Ha. split; discriminate.
  Qed.

  Lemma cmp_trans: forall r (a b c : nat), nat_cmp a b = r -> nat_cmp b c = r -> nat_cmp a c = r.
  Proof.
      intros.
      unfold nat_cmp in *.
      destruct (Nat.compare a b) eqn: HA.
      * { apply PeanoNat.Nat.compare_eq_iff in HA. subst. auto.  }
      * { subst. 
        destruct (Nat.compare b c) eqn: HB.
        + apply PeanoNat.Nat.compare_eq_iff in HB. subst. rewrite HA. reflexivity.
        + apply PeanoNat.Nat.compare_lt_iff in HA, HB.
          pose proof (PeanoNat.Nat.lt_trans a b c HA HB).
          apply PeanoNat.Nat.compare_lt_iff in H.
          rewrite H. reflexivity.
        + discriminate.
        }
      * { subst.
        destruct (Nat.compare b c) eqn: HB.
        + apply PeanoNat.Nat.compare_eq_iff in HB. subst. rewrite HA. reflexivity.
        + discriminate.
        + apply PeanoNat.Nat.compare_gt_iff in HA, HB.
          pose proof (PeanoNat.Nat.lt_trans _ _ _ HB HA).
          apply PeanoNat.Nat.compare_gt_iff in H.
          rewrite H. reflexivity.
      }
  Qed.

  Lemma cmp_eq: forall a b, nat_cmp a b = EQ <-> a = b.
  Proof.
    intros.
    unfold nat_cmp.
    destruct (Nat.compare a b) eqn:X; split; intros; try discriminate; auto.
    * apply PeanoNat.Nat.compare_eq_iff in X. auto.
    * subst. 
      apply PeanoNat.Nat.compare_lt_iff  in X. Lia.lia.
    * subst.
      apply PeanoNat.Nat.compare_gt_iff  in X. Lia.lia.
  Qed.

End NatLemma.

Instance nat_EqDec : EqDec nat := {
  eqb := Nat.eqb;
  eqb_correct := NatLemma.nat_eqb_correct;
}.

Instance nat_OrdDec : OrdDec nat nat_EqDec := {
  lt := PeanoNat.Nat.lt;
  cmp := NatLemma.nat_cmp;
  cmp_correct := NatLemma.cmp_correct;
  cmp_opp := NatLemma.cmp_opp;
  cmp_trans := NatLemma.cmp_trans;
  cmp_eq := NatLemma.cmp_eq;
}.

Module IntLemma.
  Open Scope Z_scope.

  Lemma int_eqb_correct: forall (x y: Z), x = y <-> x =? y = true.
  Proof.
    split; intros; subst.
    * Lia.lia. 
    * apply Z.eqb_eq. auto.
  Qed.

  Definition int_cmp (a b : Z) : Order :=
    match Z.compare a b with
    | Datatypes.Lt => LT
    | Datatypes.Eq => EQ
    | Datatypes.Gt => GT
    end.

  Definition int_lt (a b: Z) := a < b.

  Lemma cmp_correct: forall (a b : Z), int_cmp a b = LT <-> a < b.
  Proof.
    unfold int_cmp.
    intros.
    destruct (Z.compare a b) eqn: ZA; split; intros; auto; try discriminate.
    * apply Z.compare_eq in ZA.  Lia.lia.
    * apply Z.compare_gt_iff in ZA.  Lia.lia.
  Qed.


  Lemma cmp_opp: forall a b: Z, int_cmp a b = LT <-> int_cmp b a = GT.
  Proof.
    unfold int_cmp.
    intros.
    destruct (a ?= b) eqn: LA.
    * apply Z.compare_eq in LA. subst. rewrite Z.compare_refl. split; discriminate.
    * split; intros; auto.

      rewrite Z.compare_lt_iff in LA.
      assert (b ?= a = Gt). apply Z.compare_gt_iff. auto.
      rewrite H0. auto.
    * assert (b ?= a = Lt). apply Z.compare_lt_iff. rewrite Z.compare_gt_iff in LA. auto. rewrite H.
      split; intros; discriminate.
  Qed.

  Lemma cmp_trans: forall r a b c, int_cmp a b = r -> int_cmp b c = r -> int_cmp a c = r.
  Proof.
    intros.
    unfold int_cmp in *.
    destruct (a ?= b) eqn: RA; subst.
    * apply Z.compare_eq in RA. subst. auto.
    * destruct (b ?= c) eqn: RB; try discriminate.
      rewrite (Z.compare_lt_iff) in RA.
      rewrite (Z.compare_lt_iff) in RB.
      assert (a ?= c = Lt). apply Z.compare_lt_iff. Lia.lia.
      rewrite H. auto.
    * destruct (b ?= c) eqn: RB; try discriminate.
      rewrite (Z.compare_gt_iff) in RA.
      rewrite (Z.compare_gt_iff) in RB.
      assert (a ?= c = Gt). apply Z.compare_gt_iff. Lia.lia.
      rewrite H. auto.
  Qed.

  Lemma cmp_eq: forall a b, int_cmp a b = EQ <-> a = b.
  Proof.
    unfold int_cmp.
    split; intros.
    * destruct (a ?= b) eqn: A; try discriminate.
      apply Z.compare_eq in A. auto.
    * subst. rewrite Z.compare_refl. auto.
  Qed.

End IntLemma.

Instance int_EqDec: EqDec Z := {
  eqb := Z.eqb;
  eqb_correct := IntLemma.int_eqb_correct;
}.

Instance int_OrdDec: OrdDec Z int_EqDec := {
  lt := IntLemma.int_lt;
  cmp := IntLemma.int_cmp;
  cmp_correct := IntLemma.cmp_correct;
  cmp_opp := IntLemma.cmp_opp;
  cmp_trans := IntLemma.cmp_trans;
  cmp_eq := IntLemma.cmp_eq;
}.

Module BoolLemma.

  Lemma bool_eqb_correct: forall (x y: bool), (x = y) <-> Bool.eqb x y = true.
  Proof.
    intros. split; apply Stdlib.Bool.Bool.eqb_true_iff.
  Qed.

End BoolLemma.

Instance bool_EqDec : EqDec bool := {
  eqb := Bool.eqb;
  eqb_correct := BoolLemma.bool_eqb_correct;
}.

Module Helpers.

  Fixpoint to_nat_with_list {T: Set} `{EqDec T} (lst: list T) (i: T): nat :=
    match lst with
    | [] => 0
    | x::xs => if eqb x i then 0 else S (to_nat_with_list xs i)
    end.

  Definition compare_with_list {T: Set} `{EqDec T} (lst: list T) (a b: T): Order :=
    cmp (to_nat_with_list lst a) (to_nat_with_list lst b).

  Lemma compare_with_list_refl {T: Set} `{EqDec T}: forall (lst: list T) a, compare_with_list lst a a = EQ.
  Proof.
    intros.
    unfold compare_with_list.
    apply cmp_refl.
  Qed.

  Lemma to_nat_with_list_unique {T: Set} `{EqDec T}:
    forall lst, forall a b, In a lst -> In b lst -> to_nat_with_list lst a = to_nat_with_list lst b -> a = b.
  Proof.
    intros lst a b IA IB Heq.
    induction lst; intros. inversion IA.
    simpl in Heq.
    inversion IA; subst; inversion IB; subst; auto; try (rewrite eqb_refl in Heq).
    {
      destruct (eqb a b) eqn: X.
      apply eqb_correct. auto. inversion Heq.
    }
    {
      destruct (eqb b a) eqn: X.
      apply eqb_correct in X. auto.
      inversion Heq.
    }
    {
      destruct (eqb a0 a) eqn: Xa;
      destruct (eqb a0 b) eqn: Xb;
      try apply eqb_correct in Xa;
      try apply eqb_correct in Xb;
      subst;auto.
      inversion Heq.
      inversion Heq.
    }
  Qed.
End Helpers.

Definition enumOrdDec (T: Set) `(eqd: EqDec T) (lst: list T) (complete: forall x, In x lst) : OrdDec T eqd. 
refine {|
  lt := fun (a b : T) => Helpers.compare_with_list lst a b = LT;
  cmp := Helpers.compare_with_list lst;
  |}.
Proof.
  { split; auto.  }
  { intros. unfold Helpers.compare_with_list.
    rewrite cmp_opp.
    split; auto.
  }
  { unfold Helpers.compare_with_list.
    intros.
    eapply cmp_trans; eauto.
  }
  {
    split; intros.
    2: {
      subst. unfold Helpers.compare_with_list. apply cmp_eq. reflexivity.
    }
    unfold Helpers.compare_with_list in H.
    apply cmp_eq in H.
    eapply Helpers.to_nat_with_list_unique; eauto.
  }
Defined.

Module UnitL.
  Definition eqb (x y: unit) := true.

  Lemma eqb_correct: forall (x y: unit), (x = y) <-> eqb x y = true.
  Proof. destruct x, y. split; auto.
  Qed.

  Definition lt (a b: unit) := False.
  Definition cmp (a b: unit) := EQ.

  Lemma cmp_correct: forall (a b: unit), cmp a b = LT <-> lt a b.
  Proof.
    destruct a, b. split; intros; auto.
    unfold cmp in H. discriminate.
    unfold lt in H. contradiction.
  Qed.

  Lemma cmp_opp: forall (a b: unit), cmp a b = LT <-> cmp b a = GT.
  Proof.
    destruct a, b. unfold cmp. split; intros; discriminate.
  Qed.

  Lemma cmp_trans: forall r (a b c: unit) , cmp a b = r -> cmp b c = r -> cmp a c = r.
  Proof.
    destruct a, b, c.
    unfold cmp.
    intros. auto.
  Qed.

  Lemma cmp_eq: forall (a b: unit), cmp a b = EQ <-> a = b.
  Proof.
    destruct a, b. unfold cmp.
    split; auto.
  Qed.

End UnitL.

Instance unit_EqDec : EqDec unit := {
  eqb := UnitL.eqb;
  eqb_correct := UnitL.eqb_correct;
}.

Instance unit_OrdDec : OrdDec unit unit_EqDec := {
  lt := UnitL.lt;
  cmp := UnitL.cmp;
  cmp_correct := UnitL.cmp_correct;
  cmp_opp := UnitL.cmp_opp;
  cmp_trans := UnitL.cmp_trans;
  cmp_eq := UnitL.cmp_eq;
}.

Module RatioL.

  Lemma rat_eqb_correct: forall (x y: Qc), (x = y) <-> Qc_eq_bool x y = true.
  Proof.
    split; intros.
    subst.
    unfold Qc_eq_bool.
    destruct (Qc_eq_dec y y); auto.
    apply Qc_eq_bool_correct.
    assumption.
  Qed.

  Instance rat_EqDec : EqDec Qc := {
    eqb := Qc_eq_bool;
    eqb_correct := rat_eqb_correct;
  }.

End RatioL.
