(* The chances written in the books are chances: in every random outcome of the five books, none is negative and they add
   up to 1. Fights, the Cartwheel and the special chapters compute their own chances, and are not covered here. *)
Require Import chapters rules books.
Require Import Stdlib.Lists.List.
Require Import Stdlib.QArith.QArith.
Require Import Stdlib.QArith.Qcanon.
Import ListNotations.

Definition chances_ok (l: list (chapter_outcome * Qc)) : bool :=
  forallb (fun x => Qle_bool 0 (this (snd x))) l
  && (if Qc_eq_dec (fold_right Qcplus 0 (map snd l)) 1 then true else false).

Fixpoint co_chances_ok (o: chapter_outcome) : bool :=
  match o with
  | Randomly l => chances_ok l && forallb (fun x => co_chances_ok (fst x)) l
  | Conditionally l => forallb (fun x => co_chances_ok (snd x)) l
  | Simple _ n | Fight _ n | LoseItemFrom _ _ n | Round _ n _ => co_chances_ok n
  | OneRound _ a b c => co_chances_ok a && co_chances_ok b && co_chances_ok c
  | Goto _ | GameLost | GameWon | EnterFight _ _ => true
  end.

Fixpoint dec_chances_ok (d: decision) : bool :=
  match d with
  | App a b => dec_chances_ok a && dec_chances_ok b
  | retrieve_equipment n | can_take _ _ n | can_buy _ _ n | can_sell _ _ n | conditional _ n | after_combat n
  | remove_item_from _ _ n => dec_chances_ok n
  | none o | evade_fight _ _ _ o => co_chances_ok o
  | special _ => true
  end.

Definition book_chances_ok (chapters: list (cid * decision)) : bool := forallb (fun cd => dec_chances_ok (snd cd)) chapters.

Theorem book1_chances: book_chances_ok book1 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem book2_chances: book_chances_ok book2 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem book3_chances: book_chances_ok book3 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem book4_chances: book_chances_ok book4 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem book5_chances: book_chances_ok book5 = true.
Proof. vm_compute. reflexivity. Qed.
