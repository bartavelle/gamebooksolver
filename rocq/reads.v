(* What the five books can't read, checked on the books as they are played, with the rounds of the fights added. Each
   theorem holds for any way of scoring the end of the book that doesn't look at what is ignored, so that it holds with
   the values of the following books as well. *)
Require Import character chapters mps rules value gold flags.
Require book01 book02 book03 book04 book05.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.Arith.Arith.

Definition book1 := expand_evasions 1 book01.chapters.
Definition book2 := expand_evasions 2 book02.chapters.
Definition book3 := expand_evasions 3 book03.chapters.
Definition book4 := expand_evasions 4 book04.chapters.
Definition book5 := expand_evasions 5 book05.chapters.

Open Scope nat_scope.

Definition flag_unread_in (chapters: list (cid * decision)) (f: flag) (H: rule_flag f = false) : bool :=
  forallb (fun cd : cid * decision => Nat.eqb (dec_need (flag_tracker f H) (fun _ => 0) 0 (snd cd)) 0) chapters.

Lemma flag_unread_same: forall chapters f H score,
  flag_unread_in chapters f H = true ->
  (forall s t, FlagR f 0 s t -> score s = score t) ->
  forall fuel s t, FlagR f 0 s t -> value chapters score fuel s = value chapters score fuel t.
Proof.
  intros chapters f H score U HS fuel s t HR.
  apply (flag_value_same f H (fun _ => 0) 0 chapters score HS); auto.
  intros c d Fd. apply find_chapter_in in Fd.
  unfold flag_unread_in in U. rewrite forallb_forall in U. specialize (U _ Fd). apply Nat.eqb_eq in U. simpl in U. rewrite U. apply Nat.le_0_l.
Qed.

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

(* Book 4 never looks at the gold: all its starts are the same as far as gold goes. *)
Definition gold_unread_in (chapters: list (cid * decision)) : bool :=
  forallb (fun cd : cid * decision => Nat.eqb (dec_need gold_tracker (fun _ => 0) 0 (snd cd)) 0) chapters.

Theorem book4_gold_unread: gold_unread_in book4 = true.
Proof. vm_compute. reflexivity. Qed.

Lemma gold_unread_same: forall chapters score,
  gold_unread_in chapters = true ->
  (forall s t, GoldR 0 s t -> score s = score t) ->
  forall fuel s t, GoldR 0 s t -> value chapters score fuel s = value chapters score fuel t.
Proof.
  intros chapters score U HS fuel s t H.
  apply (value_same gold_tracker (fun _ => 0) 0 chapters score HS); auto.
  intros c d Fd. apply find_chapter_in in Fd.
  unfold gold_unread_in in U. rewrite forallb_forall in U. specialize (U _ Fd). apply Nat.eqb_eq in U. simpl in U. rewrite U. apply Nat.le_0_l.
Qed.

Corollary book4_gold_same: forall score,
  (forall s t, GoldR 0 s t -> score s = score t) ->
  forall fuel s t, GoldR 0 s t -> value book4 score fuel s = value book4 score fuel t.
Proof.
  intros score HS. exact (gold_unread_same book4 score book4_gold_unread HS).
Qed.

(* The checks are not vacuous: book 3 reads the gold, and these flags *)
Example book3_gold_read: gold_unread_in book3 = false.
Proof. vm_compute. reflexivity. Qed.

Example book3_knowledge_read: forall H, flag_unread_in book3 Knowledge01 H = false.
Proof. intro H. vm_compute. reflexivity. Qed.

Example book5_elix_read: forall H, flag_unread_in book5 FoughtElix H = false.
Proof. intro H. vm_compute. reflexivity. Qed.
