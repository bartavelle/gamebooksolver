(* Books 1 to 4 never give the equipment back, so that the table of the Rust is enough for weapon_swap_value there *)
Require Import chapters rules books weapon_analysis weapon_value.
Require Import Stdlib.Lists.List.
Require Import Stdlib.Bool.Bool.

Definition no_retrieve (chapters: list (cid * decision)) : bool :=
  forallb (fun cd => negb (has_retrieve (snd cd))) chapters.

Lemma spec_retrieve: forall d dc, has_retrieve (spec_dec d dc) = true -> has_retrieve dc = true.
Proof.
  intros d dc. induction dc; simpl; intro H; try discriminate; auto.
  - apply orb_true_iff in H. apply orb_true_iff. tauto.
  - destruct (spec_cond d b) as [| | | | |[]| | |]; simpl in H; auto; discriminate.
Qed.

Lemma closed_live: forall chapters d used, no_retrieve chapters = true -> UsedClosed chapters d used -> UsedLive chapters d used.
Proof.
  intros chapters d used N U c dc F. destruct (U c dc F) as [W D]. split; [|exact D].
  rewrite wset_live_dec; [exact W|].
  destruct (has_retrieve (spec_dec d dc)) eqn: E; [|reflexivity].
  apply spec_retrieve in E. apply find_chapter_in in F. unfold no_retrieve in N. rewrite forallb_forall in N.
  specialize (N _ F). simpl in N. rewrite E in N. discriminate.
Qed.

Example books_no_retrieve: no_retrieve book1 = true /\ no_retrieve book2 = true /\ no_retrieve book3 = true /\
  no_retrieve book4 = true.
Proof. vm_compute. repeat split. Qed.

Example book5_retrieve: no_retrieve book5 = false.
Proof. vm_compute. reflexivity. Qed.

Example books_no_chapter_0: find_chapter 0 book1 = None /\ find_chapter 0 book2 = None /\ find_chapter 0 book3 = None /\
  find_chapter 0 book4 = None /\ find_chapter 0 book5 = None.
Proof. vm_compute. repeat split. Qed.
