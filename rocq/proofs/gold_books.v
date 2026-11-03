(* What the five books do with their gold. The tables of gold_tables.v are checked on the books: no chapter needs more
   gold than what they say. *)
Require Import character chapters mps rules value simulation gold_relation gold_needs books gold_tables.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.PArith.PArith.
Open Scope nat_scope.

Definition table_of (l: list (nat * nat)) : PM.t nat :=
  fold_left (fun t x => PM.add (Pos.of_succ_nat (fst x)) (snd x) t) l (PM.empty nat).

Definition table1 := table_of needs1.
Definition table2 := table_of needs2.
Definition table3 := table_of needs3.
Definition table5 := table_of needs5.

(* the gold is part of the score at the end of book 1 only *)
Theorem table1_ok: needs_ok book1 ALL table1 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem table2_ok: needs_ok book2 0 table2 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem table3_ok: needs_ok book3 0 table3 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem table5_ok: needs_ok book5 0 table5 = true.
Proof. vm_compute. reflexivity. Qed.

(* Book 3 starts at chapter 1, and needs 10 crowns from there: all the starts with 10 crowns or more are the same. *)
Lemma book3_start_needs: need_of table3 1 = 10.
Proof. vm_compute. reflexivity. Qed.

Theorem book3_gold_above_ten: forall score,
  (forall s t, GoldR 0 s t -> score s = score t) ->
  forall fuel s t, chapter s = 1 -> GoldR 10 s t -> value book3 score fuel s = value book3 score fuel t.
Proof.
  intros score HS fuel s t C H.
  apply (gold_needs_sound book3 0 score table3 table3_ok HS fuel s t).
  rewrite C, book3_start_needs. exact H.
Qed.

(* Book 4 never looks at the gold: all its starts are the same as far as gold goes. *)
Theorem book4_gold_unread: gold_unread_in book4 = true.
Proof. vm_compute. reflexivity. Qed.

Corollary book4_gold_same: forall score,
  (forall s t, GoldR 0 s t -> score s = score t) ->
  forall fuel s t, GoldR 0 s t -> value book4 score fuel s = value book4 score fuel t.
Proof.
  intros score HS. exact (gold_unread_same book4 score book4_gold_unread HS).
Qed.

(* The check is not vacuous: book 3 reads the gold *)
(* The checks are not vacuous: book 3 reads the gold, and these flags *)
Example book3_gold_read: gold_unread_in book3 = false.
Proof. vm_compute. reflexivity. Qed.
