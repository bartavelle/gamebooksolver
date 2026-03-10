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
  Lemma lookup_insert {K: Set} {V: Set} `{OrdDec K}: forall (mp: Mp K V), ValidMap mp -> forall k v,
      lookup k (insert k v mp) = Some v.
  Proof.
    intro m.
    induction m; intros.
    * {
      unfold insert. simpl. rewrite cmp_refl. reflexivity.
    }
    * unfold insert.
      destruct a as [ck cv]. inversion H1; subst; simpl.
      { destruct (cmp k ck) eqn: KCK; simpl; try rewrite cmp_refl; auto.
        rewrite KCK. auto. }
      { specialize (IHm H6 k v).
        unfold insert in IHm. simpl in IHm.
      destruct (cmp k ck) eqn: KCK; simpl in *; try rewrite cmp_refl; auto.
      rewrite KCK.
      destruct (cmp k k2) eqn: K2K2; simpl in *; try rewrite cmp_refl; auto.
      }
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