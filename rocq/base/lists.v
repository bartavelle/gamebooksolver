(* Facts about lists that have nothing to do with the rest of the development. *)
Require Import order.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
From Stdlib Require Import Lia.

Local Open Scope nat_scope.

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

Lemma concat_pure_is_id (K: Type):
    forall l : list K,
      concat (map (fun x => [x]) l) = l.
Proof.
  induction l; auto.
  simpl. f_equal. auto.
Qed.

Lemma forall2_map {A B C: Type}: forall (g: A -> C) (h: B -> C) l1 l2,
  Forall2 (fun a b => g a = h b) l1 l2 -> map g l1 = map h l2.
Proof.
  intros g h l1 l2 F. induction F; simpl; auto. f_equal; assumption.
Qed.

Lemma forall2_refl_in {A: Type}: forall (r: A -> A -> Prop) l, (forall x, In x l -> r x x) -> Forall2 r l l.
Proof.
  induction l; intros H; constructor; auto. apply H. left. reflexivity. apply IHl. intros. apply H. right. assumption.
Qed.

Lemma forall2_app {A: Type}: forall (r: A -> A -> Prop) a b c d, Forall2 r a b -> Forall2 r c d -> Forall2 r (a ++ c) (b ++ d).
Proof.
  intros r a b c d F G. induction F; simpl; auto.
Qed.

Lemma forall2_flat {A B: Type}: forall (r: B -> B -> Prop) (g h: A -> list B) l,
  (forall x, In x l -> Forall2 r (g x) (h x)) -> Forall2 r (flat_map g l) (flat_map h l).
Proof.
  induction l; intros H; simpl; [constructor|]. apply forall2_app.
  - apply H. left. reflexivity.
  - apply IHl. intros. apply H. right. assumption.
Qed.

Definition lmax {A: Type} (g: A -> nat) (l: list A) : nat := fold_right (fun x acc => Nat.max (g x) acc) 0 l.

Lemma lmax_le {A: Type}: forall (g: A -> nat) l x, In x l -> g x <= lmax g l.
Proof.
  induction l; intros x I; simpl in *; [destruct I|]. destruct I; subst; [lia|]. specialize (IHl x H). lia.
Qed.

Lemma list_sum_add {A: Type} `{EqDec A}: forall lst (i: A) (f: A -> nat),
  In i lst -> NoDup lst ->
  list_sum (map (fun x => if order.eqb x i then f x + 1 else f x) lst) = list_sum (map f lst) + 1.
Proof.
  induction lst; intros; simpl.
  * contradiction.
  * inversion H0; subst; clear H0.
    + rewrite order.eqb_refl.
      inversion H1; subst; clear H1.
      replace (list_sum (map (fun x : A => if order.eqb x i then f x + 1 else f x) lst)) with (list_sum (map f lst)).
      Lia.lia.
      f_equal.
      apply map_ext_in.
      intros.
      destruct (order.eqb a i) eqn: EAI; auto.
      apply eqb_correct in EAI. subst. contradiction.
    + inversion H1; subst; clear H1.
      destruct (eq_dec a i).
      - subst. contradiction.
      - replace (order.eqb a i) with false. 2: {
          symmetry.
          apply eqb_not.
          assumption.
        }
        rewrite IHlst; auto. Lia.lia.
Qed.

Lemma list_sum_sub {A: Type} `{EqDec A}: forall lst (drp: A) (f: A -> nat),
  In drp lst -> NoDup lst -> f drp > 0 ->
  list_sum (map (fun x => if order.eqb x drp then f x - 1 else f x) lst) + 1 = list_sum (map f lst).
Proof.
  induction lst; intros; simpl.
  * contradiction.
  * inversion H0; subst; clear H0.
    + rewrite order.eqb_refl.
      inversion H1; subst; clear H1.
      replace (list_sum (map (fun x : A => if order.eqb x drp then f x - 1 else f x) lst)) with (list_sum (map f lst)).
      Lia.lia.
      f_equal.
      apply map_ext_in.
      intros.
      destruct (order.eqb a drp) eqn: EAI; auto.
      apply eqb_correct in EAI. subst. contradiction.
    + inversion H1; subst; clear H1.
      destruct (eq_dec a drp).
      - subst. contradiction.
      - replace (order.eqb a drp) with false. 2: {
          symmetry.
          apply eqb_not.
          assumption.
        }
        rewrite <- Nat.add_assoc.
        rewrite IHlst; auto.
Qed.
