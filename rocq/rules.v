From Equations Require Import Equations.
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

Definition update_simple (stt: Stt) (so: simple_outcome) : Stt :=
  match so with
    | DamagePlayer dmg => update_endurance (fun e => e - dmg)%nat stt
    | HealPlayer v => heal stt v
    | FullHeal => update_endurance (fun e => max_hp stt) stt
    | HalfHeal =>
        let me := max_hp stt in
        let ce := curendurance stt in
            update_endurance (fun e => Nat.min me (ce + me / 2))%nat stt
    | GainItem i q => update_items (add_item i q) stt
    | LoseItem i q => update_items (rm_item i q) stt
    | LoseItemKind slts => update_items (fun items => fold_right lose_all_slot items slts) stt
    | MustEat ch =>
        match (ch, s_check Hunting (disciplines stt)) with
        | (Hunt, true) => stt
        | _ =>
            let b01ls := Nat.eqb (book stt) 1
             in if b01ls && (Nat.ltb 3 (max_hp stt - curendurance stt)) && has_item Laumspur stt
              then update_items (rm_item Laumspur 1) (heal stt 3)
              else if has_item Meal stt
                    then update_items (rm_item Meal 1) stt
                    else if b01ls && has_item Laumspur stt
                          then update_items (rm_item Laumspur 1) (heal stt 3)
                          else update_endurance (fun e => e - 3)%nat stt
        end
    | StoreEquipment => 
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

Fixpoint select_cond (stt: Stt) (conds: list (bool_cond * chapter_outcome)) : chapter_outcome :=
  match conds with
  | [] => GameLost
  | (cnd, co) :: rst => if check_cond stt cnd then co else select_cond stt rst
  end
.

Definition single_outcome (ns: next_step): Proba next_step :=
  certain ns.


Definition has_mod (m: fight_modifier) (mds: list fight_modifier) :=  existsb (FM.fm_eqb m) (Modifiers.get_cur_mods mds).

Fixpoint apply_effects (stt: Stt) (effects: list simple_outcome) :=
  match effects with
  | [] => Some stt
  | e :: es => 
    let nstt := update_simple stt e in
    if curendurance nstt =? 0 then None else apply_effects nstt es
  end.

Fixpoint lose_items (s: slot) (q: nat) (stt: Stt): Proba Stt :=
  match q with
  | 0%nat => certain stt
  | S nq =>
        let losable := flat_map (fun i => repeat i (sitems i stt)) (slot_items s) in
        let amnt := length losable in
        match losable with
        | [] => certain stt
        | _ => let withoutitem := fun i: item => 
                let nstt := update_items (rm_item i 1) stt in
                    (Q2Qc (1 / inject_Z (Z.of_nat amnt)), lose_items s nq nstt)
                in
          merge_probas (map withoutitem losable)
        end
  end.

Fixpoint s_update_d (maxdepth: nat) (stt: Stt) (outcome: chapter_outcome): Proba next_step :=
  let certain (s: Stt) := single_outcome (New s)
  in
  if curendurance stt =? 0
    then single_outcome Lost
  else match maxdepth with
  | 0%nat => single_outcome Lost
  | S md =>
    match outcome with
      | Goto next => 
        let max_chapter: nat := if Nat.eqb (book stt) 5 then 400%nat else 350%nat in
        let ustt := update_chapter next stt
          in
          if Nat.ltb next max_chapter && has_flag Poisonned2 ustt
              then
                let nstt := update_endurance (fun e => e - 2)%nat ustt
                  in if Nat.eqb (curendurance nstt) 0
                        then [(Lost, 1%Qc)]
                        else certain nstt

          else if Nat.ltb next max_chapter && s_check Healing (disciplines stt) && negb (has_flag HadCombat stt)
              then certain (heal ustt 1)
              else certain ustt
      | GameLost => single_outcome Lost
      | GameWon => single_outcome (Won stt)
      | Simple effects co => match apply_effects stt effects with
            | None => single_outcome Lost
            | Some nstt => s_update_d md nstt co
            end
      | Conditionally lst => match filter (fun kv => check_cond stt (fst kv)) lst with
              | (_, co)::_ => s_update_d md stt co
              | [] => single_outcome Lost
              end
      | Randomly lst => merge_probas (map (fun (cndo: chapter_outcome * Qc) =>
          let (curo, pb) := cndo in (pb, s_update_d md stt curo)) lst)
      | LoseItemFrom s q next =>
            match q with
            | 0%nat => [] (* should not happen *)
            | S nq =>
                  let result := lose_items s q stt in
                  merge_probas (map (fun sq: Stt * Qc => let (nstt, nq) := sq in (nq, s_update_d md nstt next)) result)
            end
      | Fight (Details sk opphp mds) co =>
          let fightres := fight stt sk opphp mds in
          let apply_fight := fun te =>
            let (nco, nhp) := match te with
              | Escaped c hp | LateWin c hp | Stopped c hp => (Goto c, hp)
              | TLost c => (Goto c, 1%nat)
              | Std hp => (co, hp)
              end in
            let stt1 := update_flags (s_set HadCombat) stt in
            let stt2 := if has_mod MultiFight mds then stt1 else update_flags (fun flgs => s_unset StrengthPotionActive (s_unset PotentStrengthPotionActive flgs)) stt1 in
            match Modifiers.extractl Modifiers.gFakeFight mds with
                  | Some cid2 => if nhp =? 0 then certain (update_chapter cid2 stt2) else s_update_d md stt2 nco
                  | None => if nhp =? 0 then single_outcome Lost else 
                      s_update_d md (update_endurance (fun _ => nhp) stt2) nco
                  end in
          merge_probas (map (fun tep : TEscaped * Qc => 
            let (te, p) := tep in
              (p, apply_fight te)
              ) fightres)
      | OneRound (Details sk opphp mds) olose oeq owin => merge_probas (map (fun frr: (nat * nat) * Qc => 
            let (lwp, p) := frr in
            let adjust := fun lwp: nat * nat =>
              let (lw, op) := lwp in
              if (lw =? 0)%nat then single_outcome Lost else
                let lwloss := (curendurance stt - lw)%nat in
                let oploss := (opphp - op)%nat in
                let nstt := update_endurance (fun _ => lw) (update_flags (fun flgs => s_set HadCombat (s_unset StrengthPotionActive (s_unset PotentStrengthPotionActive flgs))) stt) in
                let nxt := match cmp lwloss oploss with
                              | GT => olose
                              | EQ => oeq
                              | LT => owin
                              end in
              s_update_d md nstt nxt in
            (p, adjust lwp)
            ) (fight_round stt sk opphp mds))
    end
  end .

Definition s_update (stt: Stt) (outcome: chapter_outcome): Proba next_step :=
    s_update_d 1000 stt outcome.

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
  .

Inductive WellFormedDecision: decision -> Prop :=
  | check_decisions: forall decs, Forall WellFormedDecision decs -> (exists d, In d decs) -> WellFormedDecision (Decisions decs)
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

Module RHelpers.

  Lemma in_map_fst {A B: Type}: forall x (lst : list (A * B)), In x (map fst lst) <-> exists y, In (x,y) lst.
  Proof.
    induction lst; simpl; split; intros; try contradiction.
    * destruct H. contradiction.
    * destruct a, H; simpl in *; subst.
      exists b. tauto.
      apply IHlst in H.
      destruct H.
      exists x0. tauto.
    * destruct H, a. simpl in *.
      destruct H.
      +  left. inversion H; subst. reflexivity.
      + right. apply IHlst. exists x0. assumption.
  Qed.

  Ltac rap := match goal with
    | |- _ /\ _ => split
    | |- context [ curendurance ?stt =? 0 ] =>
          destruct (curendurance stt =? 0) eqn: DEAD
    | |- FullProba (single_outcome _) => reflexivity
    | |- context [map fst (single_outcome _)] => unfold single_outcome
    | |- context [map fst (certain _)] => unfold certain
    | |- context [map fst [(_, _)]] => simpl
    | |- Forall _ (_::_) => constructor
    | |- Forall _ [] => constructor
    | |- CheckNS _ => constructor
    | |- context [let (_, _) := ?p in _] => destruct p eqn:?
    | |- FullProba (if check_cond ?stt ?b then _ else _) => destruct (check_cond stt b) eqn:?
    | |- ValidState (if has_mod ?m ?l then _ else _) => destruct (has_mod m l) eqn:?
    | |- FullProba match ?lst with | [] => _ | _ :: _ => _ end => destruct lst eqn:?
    | H: Forall _ (map _ (_ :: _)) |- _ => simpl in H
    | H: Forall _ (_ :: _) |- _ => apply Forall_cons_iff in H
    | H: _ /\ _ |- _ => destruct H
    | |- FullProba (merge_probas _) => unfold FullProba
    | |- SumProba (merge_probas _) = _ => rewrite merge_proba_rebuild_full
    | |- FullProba (s_update_d ?fuel _) => destruct fuel
    | |- forall _, _ => intro
    | |- FullProba (match Modifiers.extractl ?f ?l with _ => _ end) => destruct (Modifiers.extractl f l) eqn:?
    | |- FullProba (if ?n =? 0 then _ else _) => destruct n; simpl
    | H1: WellFormedCO ?x, H2: WellFormedCO ?x -> _ |- _ => specialize (H2 H1)
    | |- ValidState (update_flags _ _) => apply VS.update_flags
    | H: ?x |- ?x => assumption
  end.

  Lemma fold_right_map {A B C: Type}:
      forall (f: A -> B -> B) (i: B) (f2: C -> A) (lst: list C),
        fold_right f i (map f2 lst) =
          fold_right (fun a b => f (f2 a) b) i lst.
  Proof.
    intros.
    induction lst; simpl; auto.
    rewrite IHlst. reflexivity.
  Qed.

  Lemma goto_fullmap: forall stt cid fuel, FullProba (s_update_d fuel stt (Goto cid)).
  Proof.
    intros.
      destruct fuel; simpl; repeat rap.
      destruct (cid <? (if book stt =? 5 then 400 else 350)); simpl; repeat rap.
      destruct (has_flag Poisonned2 (update_chapter cid stt)); simpl; repeat rap.
      reflexivity.
      destruct (s_check Healing (disciplines stt) && negb (has_flag HadCombat stt)); simpl; repeat rap.
  Qed.

  Lemma max_hp_invar: forall book1 book2 maxendurance cure1 cure2 sk1 sk2 d1 d2 ch1 ch2 i f1 f2 p1 p2,
    max_hp {| book := book1; maxendurance := maxendurance; curendurance := cure1; sk := sk1; disciplines := d1; chapter := ch1; items := i; flags := f1; previtems := p1 |} =
    max_hp {| book := book2; maxendurance := maxendurance; curendurance := cure2; sk := sk2; disciplines := d2; chapter := ch2; items := i; flags := f2; previtems := p2 |}.
  Proof.
    intros.
    unfold max_hp. simpl. unfold has_item. simpl. reflexivity.
  Qed.

  Lemma apply_effects_cons: forall stt a lst r,
     apply_effects stt (a :: lst) = Some r ->
     apply_effects stt (a :: lst) = apply_effects (update_simple stt a) lst.
  Proof.
    intros.
    simpl in *.
    destruct (curendurance (update_simple stt a) =? 0); try discriminate; reflexivity.
  Qed.
    
  Lemma q2qc_plus: forall a b, (Q2Qc a + Q2Qc b)%Qc = Q2Qc (a + b)%Q.
  Proof.
    intros.
    unfold Qcplus.
    apply Q2Qc_eq_iff.
    simpl.
    rewrite !Qred_correct.
    reflexivity.
  Qed.

  Lemma ln_nn: forall ln, (ln > 0)%nat -> ~ inject_Z (Z.of_nat ln) == 0.
  Proof.
    intros.
    intro H0.
    destruct ln; try Lia.lia.
    unfold inject_Z in H0.
    unfold Qeq in H0.
    simpl in H0. Lia.lia.
  Qed.

  Lemma lose_items_full_helper: forall lst (ln :nat) (f: item -> Proba Stt), 
      (ln > 0)%nat ->
      (forall i, FullProba (f i)) -> 
      SumProba (merge_probas (map (fun i: item => (Q2Qc (1/ inject_Z (Z.of_nat ln)), f i)) lst)) = Q2Qc (inject_Z (Z.of_nat (length lst)) / inject_Z (Z.of_nat ln)).
  Proof.
    induction lst; intros; simpl. rewrite merge_probas_nil.
    { rewrite sumproba_empty. 
      apply Q2Qc_eq_iff.
      unfold Qdiv, inject_Z; simpl. reflexivity.
    }

    rewrite merge_probas_cons.
    rewrite add_proba_adds_proba.
    rewrite IHlst; auto.
    rewrite mul_proba_sum.
    rewrite (H0 a).
    rewrite Qcmult_1_r.

    rewrite q2qc_plus.
    apply Q2Qc_eq_iff.
    field_simplify; try (apply ln_nn; assumption).
    unfold Qeq.
    f_equal.
    f_equal.
    unfold inject_Z.
    f_equal.
    clear H0 f IHlst H ln a. 
    unfold Qplus.
    simpl.
    f_equal.
    Lia.lia.
  Qed.

  Lemma lose_items_full: forall s q stt, FullProba (lose_items s q stt).
  Proof.
    induction q; intros; simpl; try reflexivity.

    remember (flat_map (fun i : item => repeat i (sitems i stt)) (slot_items s)) as tolose.

    clear Heqtolose.
    induction tolose; try reflexivity.
    unfold FullProba.
    assert (length (a :: tolose) > 0)%nat. { simpl. Lia.lia. }
    rewrite lose_items_full_helper;auto.
      apply Q2Qc_eq_iff.
      unfold Qdiv.
      apply Qmult_inv_r.
      apply ln_nn.
      simpl.
      assumption.
  Qed.

  Fixpoint dec_amount (nitems_cost: nat) (d: decision): nat := match d with
  | Decisions lst => 1 + list_sum (map (dec_amount nitems_cost) lst)
  | retrieve_equipment nxt => 
    1 + nitems_cost + dec_amount 0 nxt
  | can_take _ q nxt => 1 + q + dec_amount nitems_cost nxt
  | can_buy _ _ nxt | can_sell _ _ nxt
  | conditional _ nxt
  | after_combat nxt
  | remove_item_from _ _ nxt => 1 + dec_amount nitems_cost nxt
  | special _ => 1
  | none _ => 1
  | evade_fight _ _ _ _ => 1
  end.

  Lemma dec_amount_positive: forall n d, (dec_amount n d > 0)%nat.
  Proof.
    intros.
    induction d; simpl; try Lia.lia.
  Qed.

  Inductive can_take_result :=
  | SpaceAvailable : can_take_result
  | MustDrop : list item -> can_take_result
  .

  Definition tcan_take (i: item) (q: nat) (s: Stt): can_take_result :=
    match item_slot i with
    | SSpecial | SPouch => SpaceAvailable
    | SWeapon =>
        let weapons := all_character_items_slot s SWeapon in
        let amount := list_sum (map snd weapons) in
        if amount <? 2 then SpaceAvailable else MustDrop (map fst weapons)
    | SBackpack =>
        let bpis := all_character_items_slot s SBackpack in
        let amount := list_sum (map snd bpis) in
        if amount <? 8 then SpaceAvailable else MustDrop (map fst bpis)
    end.

  Definition simplify_retrieve_equipment (stt: Stt) (nxt: decision) :=
      let takable := previtems stt in
      fold_right (fun (ic : item * nat) curdec =>
            let (i, q) := ic in 
            if (0 <? q)%nat
              then can_take i q curdec
              else curdec
            ) nxt takable.
  
  Definition item_cost (stt: Stt)  : nat := list_sum (map snd (previtems stt)) + length (previtems stt).

  Lemma update_items_preserve_cost:
      forall stt f, item_cost (update_items f stt) = item_cost stt.
  Proof.
    intros.
    destruct stt. unfold item_cost. simpl. reflexivity.
  Qed.

  Program Definition mk_updated_item (stt : Stt) (f: Items -> Items)
    : {nstt | RHelpers.item_cost (update_items f stt) = RHelpers.item_cost stt} :=
  exist _ (update_items f stt) (update_items_preserve_cost stt f).
End RHelpers.

Section FlattenDec.

Ltac fd_solver := match goal with
  | |- RHelpers.item_cost (update_items _ ?stt) = RHelpers.item_cost ?stt => 
        apply RHelpers.update_items_preserve_cost
  | nstt : {_ | RHelpers.item_cost _ = RHelpers.item_cost _} |- _ => destruct nstt; simpl in *
  | |- (forall _ : list decision, Decisions _ <> ?w) /\ _ => unfold w; repeat (split; intros; try (intro contra; inversion contra))
  | |- _ => Lia.lia
  end.

Local Obligation Tactic := intros; subst; simpl; repeat fd_solver.

Program Fixpoint flatten_decision (stt: Stt) (d: decision) {measure (RHelpers.dec_amount (RHelpers.item_cost stt) d)}
  : list (Proba next_step) :=
  match d with
    | can_take i q nxt =>
      let notake := flatten_decision stt nxt in
      let wtakestts : list {nstt | RHelpers.item_cost nstt = RHelpers.item_cost stt } := match RHelpers.tcan_take i q stt with
                    | RHelpers.SpaceAvailable => [RHelpers.mk_updated_item stt (add_item i 1)]
                    | RHelpers.MustDrop lst => map (fun drp => RHelpers.mk_updated_item stt
                            (fun ns => rm_item drp 1 (add_item i 1 ns)) :
                             {nstt | RHelpers.item_cost nstt = RHelpers.item_cost stt}) lst
                    end in
      match q with
      | 0%nat => [] (* can't happen *)
      | 1%nat => notake ++ flat_map (fun nstt => flatten_decision (proj1_sig nstt) nxt) wtakestts
      | S q => notake ++ flat_map (fun nstt => flatten_decision (proj1_sig nstt) (can_take i q nxt)) wtakestts
      end
    | Decisions lst =>
      match lst with
      | [] => []
      | [d] => flatten_decision stt d
      | d::ds => flatten_decision stt d ++ flatten_decision stt (Decisions ds)
      end
    | retrieve_equipment nxt =>
    let nstt := {| book := book stt
                  ; maxendurance := maxendurance stt
                  ; curendurance := curendurance stt
                  ; sk := sk stt
                  ; disciplines := disciplines stt
                  ; chapter := chapter stt
                  ; items := items stt
                  ; flags := flags stt
                  ; previtems := empty
                  |} in
    let ndecision := RHelpers.simplify_retrieve_equipment stt nxt in
        flatten_decision nstt ndecision
    | _ => []
  end.
Next Obligation.
rewrite e. Lia.lia.
Defined.
Next Obligation.
rewrite e. Lia.lia.
Defined.
Next Obligation.
  remember (list_sum (map (RHelpers.dec_amount (RHelpers.item_cost stt)) ds)) as x.
  pose proof (RHelpers.dec_amount_positive (RHelpers.item_cost stt) d).
  Lia.lia.
Defined.
Next Obligation.
intro contra. 
unfold ds in *. inversion contra.
Defined.
Next Obligation.
unfold ndecision.
unfold RHelpers.item_cost.
destruct stt. simpl in *.
unfold RHelpers.simplify_retrieve_equipment in *.
simpl in *.
clear nstt flatten_decision ndecision flags items chapter disciplines sk maxendurance curendurance book.
induction previtems.
* simpl.  Lia.lia.
* destruct a. simpl. destruct (0 <? n); simpl; Lia.lia.
Defined.
Next Obligation.
apply measure_wf.
apply lt_wf.
Defined.
End FlattenDec.

Module FDH.
  Lemma flatten_decision_cons: forall d ds stt, flatten_decision stt (Decisions (d::ds)) =
        flatten_decision stt d ++ flatten_decision stt (Decisions ds).
  Proof.
    intros.
  Admitted.
End FDH.

Theorem flatten_dec_full: forall stt d sol, ValidState stt -> WellFormedDecision d -> sol = flatten_decision stt d
    -> Forall FullProba sol /\ exists x, In x sol.
Proof.
Admitted.


(*
Theorem rules_are_preserving_fullproba: forall stt co fuel sol, ValidState stt -> WellFormedCO co ->
         sol = s_update_d fuel stt co ->
          FullProba sol.
Proof.
  intros stt co fuel sol VS WCO EQC.
  subst.
  generalize dependent stt.
  generalize dependent fuel.
  induction WCO; intros; destruct fuel; simpl; repeat RHelpers.rap; auto.
  * apply (H0 b c); auto.
    pose proof (incl_filter (fun kv: bool_cond * chapter_outcome => check_cond stt (fst kv)) lst) as Hincl.
    rewrite Heql in Hincl.
    apply incl_cons_inv in Hincl. tauto.

  * pose proof (RHelpers.in_map_fst x lst). apply H3 in H2. destruct H2.
    eapply H1; eauto.

  * unfold fight.
    destruct (Modifiers.ValidMods_dec l) eqn: VM; try contradiction.
    remember (f_fight stt s e l v).
    symmetry in Heqp.
    apply fight_inductive in Heqp.
    eapply ifight_correct; eauto.
  
  * destruct x; inversion Heqp; subst; clear Heqp; destruct (has_mod MultiFight l) eqn:MF; try apply RHelpers.goto_fullmap.
    + apply IHWCO.
      repeat RHelpers.rap.
      apply Insert.insert_with_valid; auto.
    + apply IHWCO.
      repeat RHelpers.rap.
      apply Delete.valid.
      apply Delete.valid.
      assumption.
      apply Insert.insert_with_valid; auto.
  
  * remember (fight stt s e l) as res.
    pose proof (fight_res_hp stt s e l res) as FRH.
    lapply FRH; auto; intro FRH2; clear FRH.
    simpl in H.
    symmetry in Heqres.
    specialize (FRH2 H Heqres).
    subst.
    eapply Forall_forall in H0; try apply FRH2.
    unfold valid_fight_result in H0.
  
    destruct x; inversion Heqp; subst; clear Heqp; destruct (has_mod MultiFight l) eqn:MF; try apply RHelpers.goto_fullmap.
    + eapply IHWCO.
      apply VS.update_endurance.
      { 
        intros. simpl. split; try Lia.lia.
        rewrite MH.update_flags.
        assumption.
      }
      apply VS.update_flags; auto.
      intros.
      apply Insert.insert_with_valid; auto.
    + eapply IHWCO.
      apply VS.update_endurance.
      { intros. simpl. rewrite MH.update_flags. rewrite MH.update_flags. Lia.lia. }
      apply VS.update_flags; auto.
      intros.
      apply Delete.valid.
      apply Delete.valid.
      assumption.
      apply VS.update_flags; auto.
      intros.
      apply Insert.insert_with_valid; auto.
  
  * apply fight_round_full.
  * unfold FullProba.
    subst.
    remember (fight_round stt s e l) as fr.
    symmetry in Heqfr.
    pose proof (FRH.fight_round_vr stt s e l fr Heqfr).
    pose proof (Merge.use_forall _ _ _ H1 H0). simpl in H2.
    remember (NatLemma.nat_cmp (curendurance stt - S n) (e - n0)) as xc.
    unfold ValidState in VS. simpl in VS.

    destruct xc; try apply IHWCO1; try apply IHWCO2; try apply IHWCO3;
      apply VS.update_endurance; intros; simpl; try Lia.lia;
      try apply VS.update_flags; try assumption; intros;
      try apply Insert.insert_with_valid;
      try apply Delete.valid;
      try apply Delete.valid;
      try assumption;
      rewrite MH.update_flags;
      split;
      try Lia.lia
      .

  * admit.
  * admit.
  * destruct q; try Lia.lia.
    unfold FullProba.
    rewrite merge_proba_rebuild_full.
    apply RHelpers.lose_items_full.
    intros.
    apply IHWCO.
    Search (merge_probas).
*)