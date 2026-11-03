(* What a book has to respect for playing it to keep a state valid. Most of it comes from the rules, but the book can give
   items and take them directly, without going through what the rules check. This is computed on each book. *)
Require Import chapters character combat rules books.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
Open Scope nat_scope.

(* Items that come with no decision: they can't need any room, and don't get in the way of another one *)
Definition free_gain (i: item) : bool :=
  match i with
  | Gold => true
  | Backpack | Helmet => false
  | _ => match item_slot i with SSpecial => negb (order.eqb i SilverHelmet) | _ => false end
  end.

(* Losing the Backpack loses what it holds *)
Definition keeps_contents (sls: list slot) : bool :=
  negb (existsb (fun s => match s with SSpecial => true | _ => false end) sls)
  || existsb (fun s => match s with SBackpack => true | _ => false end) sls.

Definition so_safe (so: simple_outcome) : bool :=
  match so with
  | GainItem i _ => free_gain i
  | LoseItemKind sls => keeps_contents sls
  | _ => true
  end.

Fixpoint cond_has (c: bool_cond) (i: item) : bool :=
  match c with
  | HasItem j q => order.eqb i j && Nat.ltb 0 q
  | CAnd a b => cond_has a i || cond_has b i
  | _ => false
  end.

(* Putting an item back after losing another one from the Backpack, when it is held for sure *)
Definition swap_safe (bk: nat) (c: bool_cond) (o: chapter_outcome) : bool :=
  match o with
  | Simple [LoseItem i 1] (LoseItemFrom SBackpack 1 (Simple [GainItem j 1] (Goto _))) =>
      order.eqb i j && cond_has c i && order.eqb (item_slot i) SBackpack
      && Nat.eqb (backpack_slots bk i) 1 && negb (stackable i)
  | _ => false
  end.

Fixpoint co_safe (bk: nat) (o: chapter_outcome) : bool :=
  match o with
  | Goto _ | GameLost | GameWon | EnterFight _ _ => true
  | Simple sos n => forallb so_safe sos && co_safe bk n
  | Conditionally l => forallb (fun x => co_safe bk (snd x) || swap_safe bk (fst x) (snd x)) l
  | Randomly l => forallb (fun x => co_safe bk (fst x)) l
  | LoseItemFrom _ _ n | Fight _ n | Round _ n _ => co_safe bk n
  | OneRound _ a b c => co_safe bk a && co_safe bk b && co_safe bk c
  end.

Fixpoint dec_safe (bk: nat) (d: decision) : bool :=
  match d with
  | App a b => dec_safe bk a && dec_safe bk b
  | retrieve_equipment n | can_take _ _ n | can_sell _ _ n | conditional _ n | after_combat n | remove_item_from _ _ n => dec_safe bk n
  (* buying has no way of dropping several items *)
  | can_buy i _ n => Nat.eqb (backpack_slots bk i) 1 && dec_safe bk n
  | special _ | evade_fight _ _ _ _ => true
  | none o => co_safe bk o
  end.

Definition book_safe (bk: nat) (chapters: list (cid * decision)) : bool :=
  forallb (fun cd => dec_safe bk (snd cd)) chapters.

Theorem book1_safe: book_safe 1 book1 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem book2_safe: book_safe 2 book2 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem book3_safe: book_safe 3 book3 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem book4_safe: book_safe 4 book4 = true.
Proof. vm_compute. reflexivity. Qed.
Theorem book5_safe: book_safe 5 book5 = true.
Proof. vm_compute. reflexivity. Qed.
