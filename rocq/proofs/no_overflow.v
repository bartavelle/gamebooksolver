(* Endurance::damage, in the Rust solver, takes a damage that must fit in an i8. A killing blow is worth 100, and doubling
   it would wrap around. Rounds computed with the shortcut of combat.rs halve the endurance of the opponent instead, so
   that only the fights played round by round are at risk. This checks on the books that they are never doubled:

   - DoubleDamage never applies to such a fight, in any book;
   - Undead only doubles the damage with the Sommerswerd. It can't be held before it is given in book 2, and these
     fights can't be reached from there. Books 3 to 5 have no such fights. *)
Require Import chapters.
Require book01 book02 book03 book04 book05.
Require Import Stdlib.Lists.List Stdlib.Arith.Arith Stdlib.Bool.Bool.
Import ListNotations.

Fixpoint has_mod (f: fight_modifier -> bool) (mds: list fight_modifier) : bool :=
  match mds with
  | [] => false
  | Timed _ x :: q => f x || has_mod f q
  | x :: q => f x || has_mod f q
  end.

Definition is_undead (m: fight_modifier) := match m with Undead => true | _ => false end.
Definition is_double (m: fight_modifier) := match m with DoubleDamage => true | _ => false end.

(* the fights that do not use the shortcut *)
Definition by_rounds (mds: list fight_modifier) : bool :=
  existsb (fun m => match m with
    | Timed _ _ | OnDamage _ | Dpr _ | Evaded _ | Poisonous _ | OnPickOne _ | StopFight _ => true
    | _ => false end) mds.

Definition mods_of (fd: fight_details) := match fd with Details _ _ m => m end.

Fixpoint co_rounds_with (f: fight_modifier -> bool) (o: chapter_outcome) : bool :=
  match o with
  | Fight fd n => (has_mod f (mods_of fd) && by_rounds (mods_of fd)) || co_rounds_with f n
  | OneRound fd a b c => has_mod f (mods_of fd) || co_rounds_with f a || co_rounds_with f b || co_rounds_with f c
  | Randomly l => existsb (fun x => co_rounds_with f (fst x)) l
  | Conditionally l => existsb (fun x => co_rounds_with f (snd x)) l
  | Simple _ n | LoseItemFrom _ _ n => co_rounds_with f n
  | EnterFight fd _ => has_mod f (mods_of fd)
  | Round fd n _ => has_mod f (mods_of fd) || co_rounds_with f n
  | Goto _ | GameLost | GameWon => false
  end.

Fixpoint rounds_with (f: fight_modifier -> bool) (d: decision) : bool :=
  match d with
  | App a b => rounds_with f a || rounds_with f b
  | retrieve_equipment n | can_take _ _ n | can_buy _ _ n | can_sell _ _ n | conditional _ n | after_combat n
  | remove_item_from _ _ n => rounds_with f n
  | special _ => false
  | none o => co_rounds_with f o
  | evade_fight _ _ fd o => has_mod f (mods_of fd) || co_rounds_with f o
  end.

Definition is_sommerswerd (i: item) := match i with Weapon Sommerswerd => true | _ => false end.

Definition so_gives (so: simple_outcome) : bool :=
  match so with GainItem i _ => is_sommerswerd i | _ => false end.

Fixpoint co_gives (o: chapter_outcome) : bool :=
  match o with
  | Fight _ n | Round _ n _ | LoseItemFrom _ _ n => co_gives n
  | Simple l n => existsb so_gives l || co_gives n
  | OneRound _ a b c => co_gives a || co_gives b || co_gives c
  | Randomly l => existsb (fun x => co_gives (fst x)) l
  | Conditionally l => existsb (fun x => co_gives (snd x)) l
  | EnterFight _ _ | Goto _ | GameLost | GameWon => false
  end.

Fixpoint gives (d: decision) : bool :=
  match d with
  | App a b => gives a || gives b
  | can_take i _ n | can_buy i _ n => is_sommerswerd i || gives n
  | retrieve_equipment n | can_sell _ _ n | conditional _ n | after_combat n | remove_item_from _ _ n => gives n
  | special _ => false
  | none o => co_gives o
  | evade_fight _ _ _ o => co_gives o
  end.

Definition fm_dest (m: fight_modifier) : list cid :=
  match m with
  | Evaded c | FakeFight c | OnDamage c | OnLose c | OnNotYetWon c | StopFight c | OnPickOne c => [c]
  | Timed _ (Evaded c) | Timed _ (FakeFight c) | Timed _ (OnDamage c) | Timed _ (OnLose c)
  | Timed _ (OnNotYetWon c) | Timed _ (StopFight c) | Timed _ (OnPickOne c) => [c]
  | _ => []
  end.

Definition fd_dests (fd: fight_details) := flat_map fm_dest (mods_of fd).

Fixpoint co_dests (o: chapter_outcome) : list cid :=
  match o with
  | Fight fd n => fd_dests fd ++ co_dests n
  | OneRound fd a b c => fd_dests fd ++ co_dests a ++ co_dests b ++ co_dests c
  | Randomly l => flat_map (fun x => co_dests (fst x)) l
  | Conditionally l => flat_map (fun x => co_dests (snd x)) l
  | Simple _ n | LoseItemFrom _ _ n => co_dests n
  | Goto c => [c]
  | EnterFight fd c => fd_dests fd ++ [c]
  | Round fd n l => fd_dests fd ++ co_dests n ++ l
  | GameLost | GameWon => []
  end.

Definition special_dests (s: special_chapter) : list cid :=
  match s with
  | Cartwheel => [169; 136]
  | Portholes => [197]
  | B05S127 => [159; 93]
  end.

Fixpoint dests (d: decision) : list cid :=
  match d with
  | App a b => dests a ++ dests b
  | retrieve_equipment n | can_take _ _ n | can_buy _ _ n | can_sell _ _ n | conditional _ n | after_combat n
  | remove_item_from _ _ n => dests n
  | special s => special_dests s
  | none o => co_dests o
  | evade_fight _ c fd o => c :: fd_dests fd ++ co_dests o
  end.

Fixpoint lookup_chapter (c: cid) (b: list (cid * decision)) : option decision :=
  match b with
  | [] => None
  | (k, d) :: q => if Nat.eqb k c then Some d else lookup_chapter c q
  end.

Definition mem (c: cid) (l: list cid) := existsb (Nat.eqb c) l.

(* the chapters that can be reached from these ones *)
Fixpoint reach (b: list (cid * decision)) (fuel: nat) (todo seen: list cid) : list cid :=
  match fuel, todo with
  | O, _ | _, [] => seen
  | S f, c :: q =>
      if mem c seen then reach b f q seen
      else reach b f (match lookup_chapter c b with Some d => dests d ++ q | None => q end) (c :: seen)
  end.

Definition givers (b: list (cid * decision)) : list cid :=
  map fst (filter (fun cd => gives (snd cd)) b).

Definition no_rounds_with (f: fight_modifier -> bool) (b: list (cid * decision)) : bool :=
  forallb (fun cd => negb (rounds_with f (snd cd))) b.

Definition after_sword_safe (b: list (cid * decision)) : bool :=
  forallb (fun c => match lookup_chapter c b with Some d => negb (rounds_with is_undead d) | None => true end)
    (reach b 100000 (givers b) []).

Theorem double_damage_never_by_rounds:
  no_rounds_with is_double book01.chapters = true /\ no_rounds_with is_double book02.chapters = true
  /\ no_rounds_with is_double book03.chapters = true /\ no_rounds_with is_double book04.chapters = true
  /\ no_rounds_with is_double book05.chapters = true.
Proof. vm_compute. repeat split. Qed.

(* The sword can't be held before book 2 *)
Theorem no_sword_in_book01: givers book01.chapters = [].
Proof. vm_compute. reflexivity. Qed.

Theorem sword_is_given_in_book02: givers book02.chapters = [79; 123].
Proof. vm_compute. reflexivity. Qed.

Theorem no_undead_by_rounds_after_sword: after_sword_safe book02.chapters = true.
Proof. vm_compute. reflexivity. Qed.

Theorem no_undead_by_rounds_in_later_books:
  no_rounds_with is_undead book03.chapters = true /\ no_rounds_with is_undead book04.chapters = true
  /\ no_rounds_with is_undead book05.chapters = true.
Proof. vm_compute. repeat split. Qed.

(* The check is not vacuous: without the sword, there is such a fight in book 2 *)
Example undead_by_rounds_exists_in_book02: no_rounds_with is_undead book02.chapters = false.
Proof. vm_compute. reflexivity. Qed.
