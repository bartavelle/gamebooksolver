(* The analysis of a book that tells which weapons can still matter, as WeaponUse::for_character in data.rs. The book is
   first specialised for the disciplines of the character (specialize_decision), then the weapons named in each chapter
   (WeaponSet) are merged along the destinations of the chapters (get_destinations). The functions mirror the Rust, and
   weapon_swap.v proves what they are used for. *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import lists.
Require Import proba.
Require Import combat.
Require Import rules.
Require Import fight_facts.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Bool.Bool.
Require Import Stdlib.Arith.Arith.
From Stdlib Require Import Lia.
Require Import Stdlib.QArith.Qcanon.

Open Scope nat_scope.

(* specialize_cond: what the disciplines tell is computed. HasLevel follows the model, check_cond, which gives the same
   answer as the Rust for a character with at least one discipline. *)
Definition disc_count (d: DisciplineSet) : nat := length (filter (fun x => s_check x d) all_disciplines).

Fixpoint spec_cond (d: DisciplineSet) (c: bool_cond) : bool_cond :=
  match c with
  | HasDiscipline x => Always (s_check x d)
  | HasLevel lvl => Always (Nat.leb (level_to_nat lvl) (level_to_nat (nat_to_level (disc_count d))))
  | Not c1 => match spec_cond d c1 with
              | Always b => Always (negb b)
              | c2 => Not c2
              end
  | COr c1 c2 => match spec_cond d c1 with
                 | Always true => Always true
                 | Always false => spec_cond d c2
                 | x => match spec_cond d c2 with
                        | Always true => Always true
                        | Always false => x
                        | y => COr x y
                        end
                 end
  | CAnd c1 c2 => match spec_cond d c1 with
                  | Always false => Always false
                  | Always true => spec_cond d c2
                  | x => match spec_cond d c2 with
                         | Always false => Always false
                         | Always true => x
                         | y => CAnd x y
                         end
                  end
  | _ => c
  end.

(* specialize_outcome: in a list of conditions, the first that holds is used, so that what follows a condition that
   always holds is dropped, as well as what never holds *)
Fixpoint spec_co (d: DisciplineSet) (o: chapter_outcome) : chapter_outcome :=
  match o with
  | Fight fd n => Fight fd (spec_co d n)
  | OneRound fd x y z => OneRound fd (spec_co d x) (spec_co d y) (spec_co d z)
  | Randomly l => Randomly (map (fun op => (spec_co d (fst op), snd op)) l)
  | Conditionally l =>
      let fix go (l: list (bool_cond * chapter_outcome)) :=
        match l with
        | [] => []
        | (c, x) :: q =>
            match spec_cond d c with
            | Always false => go q
            | Always true => [(Always true, spec_co d x)]
            | c2 => (c2, spec_co d x) :: go q
            end
        end in
      Conditionally (go l)
  | Simple sos n => Simple sos (spec_co d n)
  | LoseItemFrom sl q n => LoseItemFrom sl q (spec_co d n)
  | Round fd n l => Round fd (spec_co d n) l
  | Goto _ | EnterFight _ _ | GameLost | GameWon => o
  end.

(* The empty decision that the Rust puts in place of what can't be chosen. It is only used by the analysis. *)
Definition no_choice : decision := conditional (Always false) (none GameLost).

(* specialize_decision *)
Fixpoint spec_dec (d: DisciplineSet) (dec: decision) : decision :=
  match dec with
  | App x y => App (spec_dec d x) (spec_dec d y)
  | retrieve_equipment n => retrieve_equipment (spec_dec d n)
  | can_take i q n => can_take i q (spec_dec d n)
  | can_buy i p n => can_buy i p (spec_dec d n)
  | can_sell i p n => can_sell i p (spec_dec d n)
  | conditional c n => match spec_cond d c with
                       | Always false => no_choice
                       | c2 => conditional c2 (spec_dec d n)
                       end
  | special s => special s
  | none o => none (spec_co d o)
  | evade_fight r c fd o => evade_fight r c fd (spec_co d o)
  | after_combat n => after_combat (spec_dec d n)
  | remove_item_from sl q n => remove_item_from sl q (spec_dec d n)
  end.

(* WeaponSet *)
Definition item_w (i: item) : list weapon :=
  match i with
  | Weapon w => [w]
  | _ => []
  end.

(* any_of: the weapons a condition accepts, when it only asks for one of them *)
Fixpoint any_of (c: bool_cond) : option (list weapon) :=
  match c with
  | HasItem (Weapon w) 1 => Some [w]
  | COr x y => match any_of x, any_of y with
               | Some a, Some b => Some (a ++ b)
               | _, _ => None
               end
  | _ => None
  end.

Definition covers_all (l: list weapon) : bool := forallb (fun w => existsb (weapon_beq w) l) all_weapons.

Definition any_weapon (c: bool_cond) : bool :=
  match any_of c with
  | Some l => covers_all l
  | None => false
  end.

Fixpoint wset_cond (c: bool_cond) : list weapon :=
  if any_weapon c then [] else
  match c with
  | HasItem i _ => item_w i
  | Not x => wset_cond x
  | COr x y | CAnd x y => wset_cond x ++ wset_cond y
  | _ => []
  end.

Definition wset_so (so: simple_outcome) : list weapon :=
  match so with
  | GainItem i _ | LoseItem i _ => item_w i
  | _ => []
  end.

Fixpoint wset_co (o: chapter_outcome) : list weapon :=
  match o with
  | Fight _ n | Round _ n _ | LoseItemFrom _ _ n => wset_co n
  | OneRound _ x y z => wset_co x ++ wset_co y ++ wset_co z
  | Randomly l => flat_map (fun op => wset_co (fst op)) l
  | Conditionally l => flat_map (fun cx => wset_cond (fst cx) ++ wset_co (snd cx)) l
  | Simple sos n => flat_map wset_so sos ++ wset_co n
  | Goto _ | EnterFight _ _ | GameLost | GameWon => []
  end.

Fixpoint wset_dec (dec: decision) : list weapon :=
  match dec with
  | App x y => wset_dec x ++ wset_dec y
  | retrieve_equipment n | after_combat n | remove_item_from _ _ n => wset_dec n
  | can_take i _ n | can_buy i _ n | can_sell i _ n => item_w i ++ wset_dec n
  | conditional c n => wset_cond c ++ wset_dec n
  | special _ => []
  | none o | evade_fight _ _ _ o => wset_co o
  end.

(* What the proof can handle: as wset_dec, but getting the equipment back counts as naming all the weapons. The equipment
   is offered back in the order of the items, which an exchange of weapons changes, and that the order does not matter is
   not proved yet. *)
Fixpoint wset_live (dec: decision) : list weapon :=
  match dec with
  | App x y => wset_live x ++ wset_live y
  | retrieve_equipment n => all_weapons ++ wset_live n
  | after_combat n | remove_item_from _ _ n => wset_live n
  | can_take i _ n | can_buy i _ n | can_sell i _ n => item_w i ++ wset_live n
  | conditional c n => wset_cond c ++ wset_live n
  | special _ => []
  | none o | evade_fight _ _ _ o => wset_co o
  end.

Fixpoint has_retrieve (dec: decision) : bool :=
  match dec with
  | App x y => has_retrieve x || has_retrieve y
  | retrieve_equipment _ => true
  | after_combat n | remove_item_from _ _ n | can_take _ _ n | can_buy _ _ n | can_sell _ _ n | conditional _ n =>
      has_retrieve n
  | special _ | none _ | evade_fight _ _ _ _ => false
  end.

Lemma wset_live_dec: forall dec, has_retrieve dec = false -> wset_live dec = wset_dec dec.
Proof.
  induction dec; simpl; intro H; try reflexivity; try discriminate; try (rewrite IHdec; [reflexivity|exact H]).
  apply orb_false_iff in H. rewrite IHdec1, IHdec2; tauto.
Qed.

(* get_destinations. The first chapter of the rounds is for an opponent without endurance, and is never reached. *)
Fixpoint co_dests (o: chapter_outcome) : list cid :=
  match o with
  | Conditionally l => flat_map (fun cx => co_dests (snd cx)) l
  | Fight fd n => co_dests n ++ mods_dests (fd_mods fd)
  | EnterFight _ c => [c]
  | Round fd n l => co_dests n ++ mods_dests (fd_mods fd) ++ tl l
  | GameLost | GameWon => []
  | Goto c => [c]
  | OneRound fd x y z => mods_dests (fd_mods fd) ++ co_dests x ++ co_dests y ++ co_dests z
  | Randomly l => flat_map (fun op => co_dests (fst op)) l
  | Simple _ n | LoseItemFrom _ _ n => co_dests n
  end.

Fixpoint dec_dests (dec: decision) : list cid :=
  match dec with
  | App x y => dec_dests x ++ dec_dests y
  | retrieve_equipment n | after_combat n | remove_item_from _ _ n
  | can_take _ _ n | can_buy _ _ n | can_sell _ _ n | conditional _ n => dec_dests n
  | evade_fight _ c _ o => c :: co_dests o
  | none o => co_dests o
  | special B05S127 => [159; 93]
  | special Cartwheel => [169; 136]
  | special Portholes => [197]
  end.

(* The table of WeaponUse: the loop of WeaponUse::new only stops once each chapter holds the weapons named in it and
   those of its destinations, and a destination that is not in the book counts as all the weapons. Any table with
   this property will do. *)
Definition UsedClosed (chapters: list (cid * decision)) (disc: DisciplineSet) (used: cid -> list weapon) : Prop :=
  forall c d, find_chapter c chapters = Some d ->
    incl (wset_dec (spec_dec disc d)) (used c) /\
    forall c2, In c2 (dec_dests (spec_dec disc d)) ->
      (find_chapter c2 chapters = None -> forall w, In w (used c)) /\ incl (used c2) (used c).

(* The weapons that canonical can rename at a chapter: not magical, not that of the Weapon Skill, not used *)
Definition Plain (disc: DisciplineSet) (used: list weapon) (w: weapon) : Prop :=
  w <> Sommerswerd /\ w <> MagicSpear /\ s_check (WeaponSkill w) disc = false /\ ~ In w used.

(* A specialised condition gives the same answer to a character with these disciplines *)
Lemma spec_cond_correct: forall s d c, disciplines s = d -> check_cond s (spec_cond d c) = check_cond s c.
Proof.
  intros s d c E. subst d.
  induction c as [x|c IH|c1 IH1 c2 IH2|c1 IH1 c2 IH2|i q|bb|n|f|lvl]; simpl; try reflexivity.
  all: try (destruct (spec_cond _ c) as [| | | | |[]| | |] eqn: S; simpl in *; rewrite <- IH; reflexivity).
  all: destruct (spec_cond _ c1) as [| | | | |[]| | |] eqn: S1; destruct (spec_cond _ c2) as [| | | | |[]| | |] eqn: S2;
    simpl in *; rewrite <- IH1, <- IH2; simpl; try reflexivity;
    rewrite ?orb_true_r, ?orb_false_r, ?andb_true_r, ?andb_false_r; reflexivity.
Qed.

(* The list of conditions of a specialised Conditionally *)
Fixpoint spec_cl (d: DisciplineSet) (l: list (bool_cond * chapter_outcome)) : list (bool_cond * chapter_outcome) :=
  match l with
  | [] => []
  | (c, x) :: q =>
      match spec_cond d c with
      | Always false => spec_cl d q
      | Always true => [(Always true, spec_co d x)]
      | c2 => (c2, spec_co d x) :: spec_cl d q
      end
  end.

Lemma spec_co_cond: forall d l, spec_co d (Conditionally l) = Conditionally (spec_cl d l).
Proof.
  intros d l. simpl. f_equal. induction l as [|[c x] q IH]; [reflexivity|]. simpl.
  destruct (spec_cond d c) as [| | | | |[]| | |]; try rewrite IH; reflexivity.
Qed.

(* A condition that is kept is not always false *)
Lemma spec_conditional: forall d c n,
  (spec_cond d c = Always false /\ spec_dec d (conditional c n) = no_choice) \/
  (spec_cond d c <> Always false /\ spec_dec d (conditional c n) = conditional (spec_cond d c) (spec_dec d n)).
Proof.
  intros d c n. simpl. destruct (spec_cond d c) as [| | | | |[]| | |]; [right..|left|right|right|right];
    split; (reflexivity || discriminate).
Qed.

(* A round only goes on when the opponent has some endurance left *)
Definition snd_ok (r: RoundEnd) : Prop := forall k, In k (map fst (snd r)) -> snd k <> 0.

Lemma snd_ok_add: forall x y, snd_ok x -> snd_ok y -> snd_ok (add_round_end x y).
Proof.
  intros x y X Y k I. simpl in I.
  destruct (add_proba_keeps_keys_conv _ _ _ k Logic.eq_refl I); [apply X|apply Y]; assumption.
Qed.

Lemma one_round_snd: forall s osk op mds, snd_ok (one_round s osk op mds).
Proof.
  intros. unfold one_round, fold_rounds. induction (round_results s osk op mds) as [|[lw o] l IH]; simpl.
  - intros k [].
  - apply snd_ok_add; [|exact IH]. unfold common_round.
    destruct (lw =? 0); [intros k []|].
    destruct (Modifiers.extractl gOnDamage _).
    + destruct (lw <? curendurance s); [intros k []|]. destruct (o =? 0) eqn: E; [intros k []|].
      intros k [K|[]]. subst. simpl. apply Nat.eqb_neq. exact E.
    + destruct (o =? 0) eqn: E; [intros k []|]. intros k [K|[]]. subst. simpl. apply Nat.eqb_neq. exact E.
Qed.

Lemma nth_round: forall (l: list cid) op, op <> 0 -> nth op l 0 = 0 \/ In (nth op l 0) (tl l).
Proof.
  intros l op O. destruct l as [|x q]; [left; destruct op; reflexivity|].
  destruct op as [|op]; [contradiction|]. simpl.
  destruct (nth_in_or_default op q 0); [right|left]; assumption.
Qed.

(* The game of Book 2 needs nothing but the gold *)
Lemma cartwheel_spec: forall d g tgt, spec_co d (cartwheel_choice g tgt) = cartwheel_choice g tgt.
Proof.
  intros. unfold cartwheel_choice. simpl. f_equal. rewrite map_map. apply map_ext. intros [v p]. reflexivity.
Qed.

Lemma cartwheel_wset: forall g tgt, wset_co (cartwheel_choice g tgt) = [].
Proof.
  intros. unfold cartwheel_choice. simpl. induction (cartwheel_with_token g tgt) as [|[v p] l IH]; [reflexivity|].
  simpl. exact IH.
Qed.

Lemma cartwheel_dests: forall g tgt c, In c (co_dests (cartwheel_choice g tgt)) -> c = 169 \/ c = 136.
Proof.
  intros g tgt c. unfold cartwheel_choice. simpl. induction (cartwheel_with_token g tgt) as [|[v p] l IH]; [intros []|].
  simpl. intros [I|I]; [|apply IH; exact I].
  destruct (v <? cartwheel_fare); subst; [left|right]; reflexivity.
Qed.
