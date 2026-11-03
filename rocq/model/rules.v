Require Import character.
Require Import chapters.
Require Import mps.
Require Import proba.
Require Import combat.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Bool.Bool.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.QArith.QArith.
Require Import Field.

Open Scope nat_scope.

Definition rm_backpack_content (i: item) (stt: Stt): Stt :=
  match i with
  | Backpack => update_items (lose_all_slot SBackpack) stt
  | _ => stt
  end.

Definition update_simple (stt: Stt) (so: simple_outcome) : Stt :=
  match so with
    | DamagePlayer dmg => update_endurance (fun e => e - dmg)%nat stt
    | HealPlayer v => heal stt v
    | FullHeal => update_endurance (fun e => max_hp stt) stt
    | HalfHeal =>
        update_endurance (fun e => (max_hp stt + e) / 2)%nat stt
    | GainItem i q => update_items (add_item i q) stt
    | LoseItem i q =>
        if q =? 0 then stt
        else clamp_endurance (rm_backpack_content i (update_items (rm_item i q) stt))
    | LoseItemKind slts => clamp_endurance (update_items (fun items => fold_right lose_all_slot items slts) stt)
    | MustEat ch =>
        match (ch, s_check Hunting (disciplines stt)) with
        | (Hunt, true) => stt
        | _ =>
            let b01ls := (Nat.eqb (book stt) 1 || Nat.eqb (book stt) 2) && has_item Laumspur stt
             in if b01ls && Nat.leb 3 (max_hp stt - curendurance stt)
              then update_items (rm_item Laumspur 1) (heal stt 3)
              else if has_item Meal stt
                    then update_items (rm_item Meal 1) stt
                    else if b01ls
                          then update_items (rm_item Laumspur 1) (heal stt 3)
                          else update_endurance (fun e => e - 3)%nat stt
        end
    | StoreEquipment => clamp_endurance
       {| book := book stt
       ; maxendurance := maxendurance stt
       ; curendurance := curendurance stt
       ; sk := sk stt
       ; disciplines := disciplines stt
       ; chapter := chapter stt
       ; items := empty
       ; flags := flags stt
       ; previtems := items stt
       |}
    | RetrieveEquipment => clamp_endurance
       {| book := book stt
       ; maxendurance := maxendurance stt
       ; curendurance := curendurance stt
       ; sk := sk stt
       ; disciplines := disciplines stt
       ; chapter := chapter stt
       ; items := previtems stt
       ; flags := flags stt
       ; previtems := empty
       |}
    | SetFlag f => update_flags (s_set f) stt
    | ClearFlag f => update_flags (s_unset f) stt
  end.

Inductive next_step : Set :=
 | Lost
 | Won: Stt -> next_step
 | New: Stt -> next_step
 .

Module NS.
  Definition next_step_beq (ns1: next_step) (ns2: next_step): bool :=
    match (ns1, ns2) with
    | (Lost, Lost) => true
    | (Won s1, Won s2) => Stt.stt_beq s1 s2
    | (New s1, New s2) => Stt.stt_beq s1 s2
    | _ => false
    end.

  Definition next_step_eqb_correct: forall (ns1: next_step) (ns2: next_step), ns1 = ns2 <-> next_step_beq ns1 ns2 = true.
  Proof.
    intros ns1 ns2. split; intro H.
    * subst. destruct ns2; unfold next_step_beq; auto; apply Stt.stt_beq_correct; reflexivity.
    * unfold next_step_beq in H. 
      destruct ns1, ns2; simpl in *; auto; try discriminate; apply Stt.stt_beq_correct in H; subst; reflexivity.
  Qed.

  Definition ns_cmp (ns1 ns2: next_step): Order :=
    match (ns1, ns2) with
    | (Lost, Lost) => EQ
    | (Won s1, Won s2) => Stt.stt_cmp s1 s2
    | (New s1, New s2) => Stt.stt_cmp s1 s2
    | (Lost, _) => LT
    | (_, Lost) => GT
    | (Won _, _) => LT
    | (_ , Won _) => GT
    end.

  Definition ns_lt (ns1 ns2: next_step) := ns_cmp ns1 ns2 = LT.

  Lemma cmp_correct: forall (a b: next_step), ns_cmp a b = LT <-> ns_lt a b.
  Proof.
    unfold ns_lt.
    split; auto.
  Qed.

  Lemma cmp_opp: forall a b, ns_cmp a b = LT <-> ns_cmp b a = GT.
  Proof.
    unfold ns_cmp.
    destruct a, b; simpl; split; intros; auto; try discriminate;
    apply Stt.cmp_opp; auto.
  Qed.

  Lemma cmp_trans: forall r a b c, ns_cmp a b = r -> ns_cmp b c = r -> ns_cmp a c = r.
  Proof.
    unfold ns_cmp; destruct r; destruct a, b, c; intros; auto; simpl in *; try discriminate; eapply Stt.cmp_trans; eauto.
  Qed.

  Lemma cmp_eq: forall a b, ns_cmp a b = EQ <-> a = b.
  Proof.
    unfold ns_cmp. destruct a, b; split; intros; simpl in *; subst; auto; try discriminate;
      try (apply Stt.cmp_eq);
      try (apply Stt.cmp_eq in H; subst);
      auto;
      inversion H; auto.
  Qed.
End NS.

Instance next_step_EqDec: EqDec next_step := {
  eqb := NS.next_step_beq;
  eqb_correct := NS.next_step_eqb_correct;
}.

Instance next_step_OrdDec: OrdDec next_step next_step_EqDec :=
{
  cmp := NS.ns_cmp;
  lt := NS.ns_lt;
  cmp_correct := NS.cmp_correct;
  cmp_opp := NS.cmp_opp;
  cmp_trans := NS.cmp_trans;
  cmp_eq := NS.cmp_eq;
}.
  

Fixpoint check_cond (stt: Stt) (cond: bool_cond) : bool :=
  match cond with
  | HasDiscipline d => s_check d (disciplines stt)
  | Not c => negb (check_cond stt c)
  | COr c1 c2 => check_cond stt c1 || check_cond stt c2
  | CAnd c1 c2 => check_cond stt c1 && check_cond stt c2
  | HasItem i q => Nat.leb q (sitems i stt)
  | Always b => b
  | HasEndurance n => Nat.leb n (curendurance stt)
  | HasFlag f => has_flag f stt
  | HasLevel lvl => Nat.leb (level_to_nat lvl) (level_to_nat (level stt))
  end.

Definition single_outcome (ns: next_step): Proba next_step :=
  certain ns.

Definition first_story_round: nat := 2000.
Definition first_other_round: nat := 30000.

(* Whether leaving this chapter has the effects of the passing of time. It is not the case for the epilogue and for the
   chapters that were added to the books. The chapters holding the rounds of a fight behave like the chapter holding the
   fight, see expand_evasions. *)
Definition is_story_chapter (bk c: nat) : bool :=
  let max_chapter: nat := if Nat.eqb bk 5 then 400%nat else 350%nat in
  Nat.ltb c max_chapter || (Nat.leb first_story_round c && Nat.ltb c first_other_round).

Definition apply_effects (stt: Stt) (effects: list simple_outcome) : Stt :=
  fold_left update_simple effects stt.

Definition held_items (s: slot) (stt: Stt): list (item * nat) :=
  filter (fun iq : item * nat => Nat.ltb 0 (snd iq)) (map (fun i => (i, sitems i stt)) (slot_items s)).

Lemma held_in_slot: forall sl s iq, In iq (held_items sl s) -> In (fst iq) (slot_items sl).
Proof.
  unfold held_items. intros. apply filter_In in H. destruct H as [H _].
  apply in_map_iff in H. destruct H as [i [E I]]. subst. assumption.
Qed.

Definition has_raw_mod (m: fight_modifier) (mds: list fight_modifier) := existsb (FM.fm_eqb m) mds.

Definition unset_potions (stt: Stt) : Stt :=
  update_flags (fun flgs => s_unset StrengthPotionActive (s_unset PotentStrengthPotionActive flgs)) stt.

Fixpoint s_update_d (maxdepth: nat) (stt: Stt) (outcome: chapter_outcome): Proba next_step :=
  if curendurance stt =? 0
    then single_outcome Lost
  else match maxdepth with
  | 0%nat => single_outcome Lost
  | S md =>
    let after_fight (mds: list fight_modifier) (co: chapter_outcome) (te: TEscaped) : Proba next_step :=
        let (nco, nhp) := match te with
          | Escaped c hp | LateWin c hp | Stopped c hp => (Goto c, hp)
          | TLost c => (Goto c, 1%nat)
          | Std hp => (co, hp)
          end in
        let stt1 := update_flags (s_set HadCombat) stt in
        let stt2 := if has_raw_mod MultiFight mds then stt1 else unset_potions stt1 in
        match Modifiers.extractl Modifiers.gFakeFight mds with
        | Some cid2 => if nhp =? 0 then single_outcome (New (update_chapter cid2 stt2)) else s_update_d md stt2 nco
        | None => if nhp =? 0 then single_outcome Lost
                  else s_update_d md (update_endurance (fun _ => nhp) stt2) nco
        end in
    match outcome with
      | Goto next =>
        let story := is_story_chapter (book stt) (chapter stt) in
        let ustt := update_chapter next stt in
          if story && has_flag Poisonned2 stt
              then
                let nstt := update_endurance (fun e => e - 2)%nat ustt
                  in if Nat.eqb (curendurance nstt) 0
                        then single_outcome Lost
                        else single_outcome (New nstt)
          else if story && s_check Healing (disciplines stt) && negb (has_flag HadCombat stt)
              then single_outcome (New (heal ustt 1))
              else single_outcome (New ustt)
      | GameLost => single_outcome Lost
      | GameWon => single_outcome (Won stt)
      | Simple effects co => s_update_d md (apply_effects stt effects) co
      | Conditionally lst => match filter (fun kv => check_cond stt (fst kv)) lst with
              | (_, co)::_ => s_update_d md stt co
              | [] => single_outcome Lost
              end
      | Randomly lst => merge_probas (map (fun (cndo: chapter_outcome * Qc) =>
          let (curo, pb) := cndo in (pb, s_update_d md stt curo)) lst)
      | LoseItemFrom s q next =>
          let lst := held_items s stt in
          match q, lst with
          | 0%nat, _ | _, [] => s_update_d md stt next
          | S nq, _ =>
              let total := list_sum (map snd lst) in
              merge_probas (map (fun iq: item * nat =>
                  let (i, amnt) := iq in
                  (Q2Qc ((inject_Z (Z.of_nat amnt) / inject_Z (Z.of_nat total))%Q),
                   s_update_d md (update_simple stt (LoseItem i 1)) (LoseItemFrom s nq next))) lst)
          end
      | Fight (Details osk opphp mds) co =>
          merge_probas (map (fun tep : TEscaped * Qc =>
            let (te, p) := tep in (p, after_fight mds co te)) (fight stt osk opphp mds))
      | EnterFight _ first_round =>
          single_outcome (New (update_chapter first_round (update_flags (s_set HadCombat) stt)))
      | Round (Details osk opphp mds) co next_rounds =>
          let res := one_round stt osk opphp mds in
          let stt1 := update_flags (s_set HadCombat) stt in
          add_proba
            (merge_probas (map (fun tep : TEscaped * Qc =>
              let (te, p) := tep in (p, after_fight mds co te)) (fst res)))
            (map (fun st : (nat * nat) * Qc =>
              let '((lw, op), p) := st in
              (New (update_chapter (nth op next_rounds 0%nat) (update_endurance (fun _ => lw) stt1)), p)) (snd res))
      | OneRound (Details osk opphp mds) olose oeq owin => merge_probas (map (fun frr: (nat * nat) * Qc =>
            let '((lw, op), p) := frr in
            let lwloss := (curendurance stt - lw)%nat in
            let oploss := (opphp - op)%nat in
            let nstt := update_endurance (fun _ => lw) (update_flags (s_set HadCombat) (unset_potions stt)) in
            let nxt := match cmp lwloss oploss with
                          | GT => olose
                          | EQ => oeq
                          | LT => owin
                          end in
            (p, s_update_d md nstt nxt)
            ) (fight_round stt osk opphp mds))
    end
  end.

Definition s_update (stt: Stt) (outcome: chapter_outcome): Proba next_step :=
    s_update_d 1000 stt outcome.

Inductive can_take_result :=
  | SpaceAvailable : can_take_result
  | MustDrop : list item -> can_take_result
  | Nope : can_take_result
  .

Definition tcan_take (i: item) (s: Stt): can_take_result :=
  match item_slot i with
  | SPouch => SpaceAvailable
  | SSpecial =>
      if has_item i s then Nope
      else if order.eqb i SilverHelmet && has_item Helmet s then MustDrop [Helmet]
      else if order.eqb i Helmet && has_item SilverHelmet s then MustDrop [SilverHelmet]
      else SpaceAvailable
  | SWeapon =>
      let weapons := held_items SWeapon s in
      if Nat.ltb (length weapons) 2 then SpaceAvailable else MustDrop (map fst weapons)
  | SBackpack =>
      if negb (has_item Backpack s) then Nope
      else
        let bpis := held_items SBackpack s in
        let used := list_sum (map (fun iq : item * nat => backpack_slots (book s) (fst iq) * snd iq) bpis) in
        if Nat.leb (used + backpack_slots (book s) i) 8 then SpaceAvailable else MustDrop (map fst bpis)
  end.

Fixpoint has_combat (o: chapter_outcome): option fight_details :=
  match o with
  | Fight fd _ | OneRound fd _ _ _ | EnterFight fd _ => Some fd
  | Round _ _ _ | Goto _ | GameLost | GameWon => None
  | Randomly lst => fold_right (fun (cp: chapter_outcome * Qc) acc =>
        match has_combat (fst cp) with Some x => Some x | None => acc end) None lst
  | Conditionally lst => fold_right (fun (cp: bool_cond * chapter_outcome) acc =>
        match has_combat (snd cp) with Some x => Some x | None => acc end) None lst
  | Simple _ n | LoseItemFrom _ _ n => has_combat n
  end.

Definition fd_has (m: fight_modifier) (fd: fight_details) := has_raw_mod m (fd_mods fd).

(* Stands for the probabilities of each final amount of gold when playing Cartwheel with the given money, staking one
   crown, until the money is lost or until it reaches the target, as computed in cartwheel.rs. With m below the target,
   writing d m for this distribution: d m = 7/10 d (m-1) + 2/10 d (m+5) + 1/10 d (m+8). *)
Parameter cartwheel: nat -> nat -> list (nat * Qc).

Definition max_cartwheel: nat := 30.
Definition cartwheel_fare: nat := 20.

(* The final gold when arriving with this gold and the free token, which is played first. A win pays like any other
   stake, as cartwheel_with_token in cartwheel.rs. *)
Definition cartwheel_with_token (gold target: nat): list (nat * Qc) :=
  flat_map (fun gp : nat * Q =>
    let (gain, p) := gp in
    let money := (gold + gain)%nat in
    if Nat.leb target money
      then [(money, Q2Qc p)]
      else map (fun vq : nat * Qc => (fst vq, (snd vq * Q2Qc p)%Qc)) (cartwheel money target))
  [(0%nat, (7 # 10)%Q); (6%nat, (2 # 10)%Q); (9%nat, (1 # 10)%Q)].

Definition cartwheel_choice (gold target: nat): chapter_outcome :=
  Randomly (map (fun vp : nat * Qc =>
    let (v, p) := vp in
    (Simple [LoseItemKind [SPouch]; GainItem Gold v]
       (Goto (if Nat.ltb v cartwheel_fare then 169%nat else 136%nat)), p)) (cartwheel_with_token gold target)).

Fixpoint flatten_d (fuel: nat) (stt: Stt) (d: decision) : list chapter_outcome :=
  match fuel with
  | O => []
  | S f =>
    let with_effect (sos: list simple_outcome) (nxt: decision) : list chapter_outcome :=
      map (Simple sos) (flatten_d f (apply_effects stt sos) nxt) in
    if has_flag Poisonned2 stt && has_item Laumspur stt
    then with_effect [ClearFlag Poisonned2; LoseItem Laumspur 1] d
    else
    match d with
    | none o =>
        match has_combat o with
        | None => [o]
        | Some fd =>
            if fd_has NoPotion fd then [o]
            else
              o ::
              (if has_item StrengthPotion4 stt
                then [Simple [SetFlag PotentStrengthPotionActive; LoseItem StrengthPotion4 1] o] else []) ++
              (if has_item StrengthPotion stt
                then [Simple [SetFlag StrengthPotionActive; LoseItem StrengthPotion 1] o] else [])
        end
    | retrieve_equipment nxt =>
        let ndec := fold_left (fun acc (iq: item * nat) => can_take (fst iq) (snd iq) acc)
                      (rev (all_held_items stt)) nxt in
        with_effect [RetrieveEquipment] ndec
    | after_combat nxt =>
        let missing := (max_hp stt - curendurance stt)%nat in
        let out := flatten_d f stt nxt in
        if Nat.eqb missing 0 then out
        else
          let usable := filter (fun ih : bool * item * nat =>
              let '(u, i, _) := ih in u && has_item i stt)
            [ (Nat.eqb (book stt) 5, GenBackpack S2, 10%nat)
            ; (true, Potion6Hp, 6%nat)
            ; (true, Potion5Hp, 5%nat)
            ; (true, Potion4Hp, 4%nat)
            ; (negb (Nat.eqb (book stt) 1 || Nat.eqb (book stt) 2), Laumspur, 4%nat)
            ; (Nat.eqb (book stt) 2, Laumspur, 3%nat)
            ; (Nat.eqb (book stt) 2, GenBackpack S2, 3%nat)
            ; (true, Potion2Hp, 2%nat)
            ] in
          let not_wasted := filter (fun ih : bool * item * nat => let '(_, _, h) := ih in Nat.leb h missing) usable in
          let wasted := filter (fun ih : bool * item * nat => let '(_, _, h) := ih in Nat.ltb missing h) usable in
          (* only the smallest of the potions that heal too much is useful *)
          let drinkable := not_wasted ++ (match rev wasted with [] => [] | x :: _ => [x] end) in
          out ++ flat_map (fun ih : bool * item * nat =>
            let '(_, i, h) := ih in with_effect [HealPlayer h; LoseItem i 1] d) drinkable
    | conditional c nxt => if check_cond stt c then flatten_d f stt nxt else []
    | App d1 d2 => flatten_d f stt d1 ++ flatten_d f stt d2
    | can_take Gold q nxt => with_effect [GainItem Gold q] nxt
    | can_take _ 0 nxt => flatten_d f stt nxt
    | can_take i 1 nxt =>
        match tcan_take i stt with
        | Nope => flatten_d f stt nxt
        | SpaceAvailable => with_effect [GainItem i 1] nxt
        | MustDrop lst =>
            flatten_d f stt nxt ++
            flat_map (fun l =>
              if Nat.ltb 1 (backpack_slots (book stt) i) then with_effect [LoseItem l 1] d
              else if order.eqb l i then []
              else with_effect [LoseItem l 1; GainItem i 1] nxt) lst
        end
    | can_take i (S q) nxt => flatten_d f stt (can_take i 1 (can_take i q nxt))
    | remove_item_from sl n nxt =>
        let lst := held_items sl stt in
        match n, lst with
        | 0%nat, _ | _, [] => flatten_d f stt nxt
        | S m, _ => flat_map (fun iq : item * nat =>
              with_effect [LoseItem (fst iq) 1] (remove_item_from sl m nxt)) lst
        end
    | can_sell i price nxt =>
        flatten_d f stt nxt ++
        (if Nat.ltb 0 (sitems i stt)
          then with_effect [LoseItem i 1; GainItem Gold price] nxt
          else [])
    | can_buy i price nxt =>
        flatten_d f stt nxt ++
        (if Nat.leb price (sitems Gold stt)
          then match tcan_take i stt with
               | Nope => []
               | SpaceAvailable => with_effect [LoseItem Gold price; GainItem i 1] nxt
               | MustDrop lst => flat_map (fun l =>
                    with_effect [LoseItem l 1; LoseItem Gold price; GainItem i 1] nxt) lst
               end
          else [])
    | special B05S127 =>
        let cs := Z.to_nat (sk stt) in
        let borne (x: nat) := Q2Qc ((inject_Z (Z.of_nat (Nat.min x 10)) / inject_Z 10)%Q) in
        [Randomly [ (Goto 159%nat, borne (cs - 9)%nat)
                  ; (Simple [DamagePlayer 1] (Goto 93%nat), borne (19 - cs)%nat)]]
    | special Cartwheel =>
        let gold := sitems Gold stt in
        let first := Nat.max cartwheel_fare (gold + 1) in
        (if Nat.leb cartwheel_fare gold
           then flatten_d f stt (none (Goto 136%nat)) ++ flatten_d f stt (none (cartwheel_choice gold gold))
           else [])
        ++ flat_map (fun target => flatten_d f stt (none (cartwheel_choice gold target)))
             (seq first (max_cartwheel + 1 - first))
    | special Portholes => flatten_d f stt (none (Goto 197%nat))
    (* see expand_evasions *)
    | evade_fight _ _ _ _ => []
    end
  end.

Definition flatten_decision (stt: Stt) (d: decision) : list chapter_outcome :=
  flatten_d 1000 stt d.

(* Fights that can be evaded are played one round at a time, in chapters that are added to the book, see evasion.rs *)
Record NewChapters := mkNC
  { next_story: nat
  ; next_other: nat
  ; added: list (cid * decision)
  }.

Fixpoint fm_timer (mds: list fight_modifier): nat :=
  match mds with
  | [] => 0
  | Timed n _ :: q => Nat.max n (fm_timer q)
  | _ :: q => fm_timer q
  end.

Definition fight_chapters (first min_rounds last_round opponent: nat) (osk: skill) (mds0: list fight_modifier)
    (evaded: cid) (after: chapter_outcome) : list (cid * decision) :=
  let id (round ep: nat) := (first + round * (opponent + 1) + ep)%nat in
  let fix rounds (round fuel: nat) (mds: list fight_modifier) :=
    match fuel with
    | O => []
    | S f =>
        let next_rounds := map (id (Nat.min (round + 1) last_round)) (seq 0 (opponent + 1)) in
        map (fun ep =>
          let fight_on := none (Round (Details osk ep mds) after next_rounds) in
          (id round ep,
           if Nat.leb min_rounds round
             then App fight_on (none (Fight (Details osk ep (filter (fun m => negb (FM.fm_eqb MultiFight m)) mds ++ [Evaded evaded; NoPotion])) after))
             else fight_on)) (seq 1 opponent)
        ++ rounds (S round) f (Modifiers.advance_time mds)
    end in
  rounds 0 (S last_round) mds0.

Definition expand_fight (nc: NewChapters) (story: bool) (min_rounds: nat) (evaded: cid) (fd: fight_details)
    (after: chapter_outcome) : NewChapters * cid :=
  match fd with
  | Details osk opponent mds =>
    let last_round := Nat.max (fm_timer mds) min_rounds in
    let count := ((last_round + 1) * (opponent + 1))%nat in
    let first := if story then next_story nc else next_other nc in
    let chapters := fight_chapters first min_rounds last_round opponent osk mds evaded after in
    (mkNC (if story then (first + count)%nat else next_story nc)
          (if story then next_other nc else (first + count)%nat)
          (added nc ++ chapters),
     (first + opponent)%nat)
  end.

Fixpoint expand_dec (nc: NewChapters) (story: bool) (d: decision) : NewChapters * decision :=
  match d with
  | evade_fight rounds evaded fd after =>
      let (nc2, first) := expand_fight nc story rounds evaded fd after in
      (nc2, none (EnterFight fd first))
  | App a b =>
      let (nc1, na) := expand_dec nc story a in
      let (nc2, nb) := expand_dec nc1 story b in
      (nc2, App na nb)
  | retrieve_equipment n => let (nc1, n2) := expand_dec nc story n in (nc1, retrieve_equipment n2)
  | can_take i q n => let (nc1, n2) := expand_dec nc story n in (nc1, can_take i q n2)
  | can_buy i q n => let (nc1, n2) := expand_dec nc story n in (nc1, can_buy i q n2)
  | can_sell i q n => let (nc1, n2) := expand_dec nc story n in (nc1, can_sell i q n2)
  | conditional c n => let (nc1, n2) := expand_dec nc story n in (nc1, conditional c n2)
  | after_combat n => let (nc1, n2) := expand_dec nc story n in (nc1, after_combat n2)
  | remove_item_from s q n => let (nc1, n2) := expand_dec nc story n in (nc1, remove_item_from s q n2)
  | special _ | none _ => (nc, d)
  end.

Fixpoint insert_chapter (cd: cid * decision) (l: list (cid * decision)) : list (cid * decision) :=
  match l with
  | [] => [cd]
  | x :: q => if Nat.leb (fst cd) (fst x) then cd :: l else x :: insert_chapter cd q
  end.

(* The chapters are handled in order, so that a chapter keeps the same number from one run to the other *)
Definition expand_evasions (bk: nat) (chapters: list (cid * decision)) : list (cid * decision) :=
  let '(nc, out) := fold_left (fun (acc: NewChapters * list (cid * decision)) (cd: cid * decision) =>
      let (cur, done) := acc in
      let (c, d) := cd in
      let (nc2, nd) := expand_dec cur (is_story_chapter bk c) d in
      (nc2, (c, nd) :: done)) (fold_right insert_chapter [] chapters) (mkNC first_story_round first_other_round [], []) in
  rev out ++ added nc.

Fixpoint find_chapter (c: cid) (chapters: list (cid * decision)) : option decision :=
  match chapters with
  | [] => None
  | (k, d) :: q => if Nat.eqb k c then Some d else find_chapter c q
  end.

(* The choices at a chapter of an expanded book, each with its results *)
Lemma find_chapter_in: forall chapters c d, find_chapter c chapters = Some d -> In (c, d) chapters.
Proof.
  induction chapters as [|[k x] q IH]; intros c d F; simpl in F; [discriminate|].
  destruct (k =? c) eqn: E.
  - apply Nat.eqb_eq in E. inversion F; subst. left. reflexivity.
  - right. apply IH. assumption.
Qed.

Definition step (chapters: list (cid * decision)) (stt: Stt) : list (Proba next_step) :=
  match find_chapter (chapter stt) chapters with
  | None => []
  | Some d =>
      let nv := update_flags (s_unset HadCombat) stt in
      map (s_update nv) (flatten_decision stt d)
  end.

Inductive WellFormedCO: chapter_outcome -> Prop :=
  | check_conditionally: forall lst,
      (forall cnd so, In (cnd, so) lst -> WellFormedCO so) ->
      (forall stt, ValidState stt -> exists cnd co rmn, filter (fun kv => check_cond stt (fst kv)) lst = (cnd, co)::rmn) ->
      WellFormedCO (Conditionally lst)
  | check_randomly: forall lst,
      FullProba lst ->
      (forall so pb, In (so, pb) lst -> WellFormedCO so) ->
      WellFormedCO (Randomly lst)
  | check_fight: forall dt o, WellFormedCO o -> Modifiers.ValidMods (fd_mods dt) -> WellFormedCO (Fight dt o)
  | check_oneround: forall dt o1 o2 o3,
      Modifiers.ValidMods (fd_mods dt) ->
      WellFormedCO o1 ->
      WellFormedCO o2 ->
      WellFormedCO o3 ->
      WellFormedCO (OneRound dt o1 o2 o3)
  | check_simple: forall lst o,
      WellFormedCO o ->
      WellFormedCO (Simple lst o)
  | check_goto: forall cid, WellFormedCO (Goto cid)
  | check_gamewon: WellFormedCO GameWon
  | check_gamelost: WellFormedCO GameLost
  | check_lose_item_from: forall s q o,
      WellFormedCO o ->
      (q > 0)%nat ->
      WellFormedCO (LoseItemFrom s q o)
  | check_enter_fight: forall dt c, WellFormedCO (EnterFight dt c)
  | check_round: forall dt o l, WellFormedCO o -> WellFormedCO (Round dt o l)
  .

Inductive WellFormedDecision: decision -> Prop :=
  | check_decisions: forall d1 d2, WellFormedDecision d1 -> WellFormedDecision d2 -> WellFormedDecision (App d1 d2)
  | check_retrieve_equipment: forall d, WellFormedDecision d -> WellFormedDecision (retrieve_equipment d)
  | check_can_take: forall i q d, (q > 0)%nat -> WellFormedDecision d -> WellFormedDecision (can_take i q d)
  | check_can_buy: forall i q d, (q > 0)%nat -> WellFormedDecision d -> WellFormedDecision (can_buy i q d)
  | check_can_sell: forall i q d, (q > 0)%nat -> WellFormedDecision d -> WellFormedDecision (can_sell i q d)
  | check_conditional: forall cnd d, WellFormedDecision d -> WellFormedDecision (conditional cnd d)
  | check_special : forall s, WellFormedDecision (special s)
  | check_none: forall co, WellFormedCO co -> WellFormedDecision (none co)
  | check_evade_fight: forall a b c co, WellFormedCO co -> WellFormedDecision (evade_fight a b c co)
  | check_after_combat: forall d, WellFormedDecision d -> WellFormedDecision (after_combat d)
  | check_remove_item_from: forall i s d, WellFormedDecision d -> WellFormedDecision (remove_item_from i s d)
  .

Inductive CheckNS: next_step -> Prop :=
  | check_lost: CheckNS Lost
  | check_won: forall stt, ValidState stt -> CheckNS (Won stt)
  | check_new: forall stt, ValidState stt -> CheckNS (New stt)
  .

(* Every character must be offered at least one option *)
Definition Exhaustive (d: decision) : Prop :=
  forall stt, ValidState stt -> flatten_decision stt d <> [].

Theorem flatten_dec_full: forall stt d,
  ValidState stt -> WellFormedDecision d -> Exhaustive d
  -> Forall (fun o => FullProba (s_update stt o)) (flatten_decision stt d) /\ exists x, In x (flatten_decision stt d).
Proof.
Admitted.
