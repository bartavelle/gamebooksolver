(* The flags that the five books can't read, checked on the books as they are played. Each theorem holds for any way of
   scoring the end of the book that doesn't look at the flag, so that it holds with the values of the following books as
   well. *)
Require Import character chapters mps rules value simulation flags books.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.Arith.Arith.

Open Scope nat_scope.

(* The flags that each book never reads, so that it doesn't matter whether they cross into it *)
Definition unread_flags1 := [FoughtElix; ReceivedCrystalStarPendant; Knowledge01; Knowledge02; Knowledge03; Knowledge04; Special01; Special02; Special03; Special04].
Definition unread_flags2 := [FoughtElix; ReceivedCrystalStarPendant; Knowledge02; Knowledge03; Knowledge04; Special01; Special02; Special03; Special04].
Definition unread_flags3 := [FoughtElix; ReceivedCrystalStarPendant; Knowledge02; Knowledge03; Knowledge04].
Definition unread_flags4 := [FoughtElix; ReceivedCrystalStarPendant; Knowledge01; Knowledge02; Knowledge03; Knowledge04; Special03; Special04].
Definition unread_flags5 := [Knowledge02; Knowledge03; Knowledge04; Special02; Special03; Special04].

Ltac unread_tac := intros f H I; simpl in I; repeat (destruct I as [I|I]; [subst f; vm_compute; reflexivity|]); destruct I.

Theorem book1_flags_unread: forall f H, In f unread_flags1 -> flag_unread_in book1 f H = true.
Proof. unread_tac. Qed.
Theorem book2_flags_unread: forall f H, In f unread_flags2 -> flag_unread_in book2 f H = true.
Proof. unread_tac. Qed.
Theorem book3_flags_unread: forall f H, In f unread_flags3 -> flag_unread_in book3 f H = true.
Proof. unread_tac. Qed.
Theorem book4_flags_unread: forall f H, In f unread_flags4 -> flag_unread_in book4 f H = true.
Proof. unread_tac. Qed.
Theorem book5_flags_unread: forall f H, In f unread_flags5 -> flag_unread_in book5 f H = true.
Proof. unread_tac. Qed.

(* The checks are not vacuous: these flags are read *)
Example book3_knowledge_read: forall H, flag_unread_in book3 Knowledge01 H = false.
Proof. intro H. vm_compute. reflexivity. Qed.

Example book5_elix_read: forall H, flag_unread_in book5 FoughtElix H = false.
Proof. intro H. vm_compute. reflexivity. Qed.
