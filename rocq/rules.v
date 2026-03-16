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
    | LoseItem i q => c_rm_items i q stt
    | LoseItemKind slts => update_items (fun items => fold_right lose_all_slot items slts) stt
    | MustEat ch =>
        match (ch, s_check Hunting (disciplines stt)) with
        | (Hunt, true) => stt
        | _ =>
            let b01ls := Nat.eqb (book stt) 1
             in if b01ls && (Nat.ltb 3 (max_hp stt - curendurance stt)) && has_item Laumspur stt
              then c_rm_items Laumspur 1 (heal stt 3)
              else if has_item Meal stt
                    then c_rm_items Meal 1 stt
                    else if b01ls && has_item Laumspur stt
                          then c_rm_items Laumspur 1 (heal stt 3)
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
                let nstt := c_rm_items i 1 stt in
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
  | App d1 d2 => 1 + dec_amount nitems_cost d1 + dec_amount nitems_cost d2
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

  Definition tcan_take (i: item) (s: Stt): can_take_result :=
    match item_slot i with
    | SSpecial | SPouch => SpaceAvailable
    | SWeapon =>
        let weapons := all_character_items_slot s SWeapon in
        let amount := list_sum (map snd weapons) in
        if amount <? 2 then SpaceAvailable else MustDrop (map fst (filter (fun iq : item * nat => (0 <? snd iq)%nat && (negb (mps.eqb i (fst iq)))) weapons))
    | SBackpack =>
        let bpis := all_character_items_slot s SBackpack in
        let amount := list_sum (map snd bpis) in
        if amount <? 8 then SpaceAvailable else MustDrop (map fst (filter (fun iq : item * nat => (0 <? snd iq)%nat && (negb (mps.eqb i (fst iq))))  bpis))
    end.

  Lemma tcan_take_notin: forall i s lst, tcan_take i s = MustDrop lst ->
    ~ In i lst.
  Proof.
    intros. intro contra.
    unfold tcan_take in H.
    destruct (item_slot i) eqn: SLOT; try discriminate.
    * destruct (list_sum (map snd (all_character_items_slot s SWeapon)) <? 2); try discriminate.
      remember (map fst (filter (fun iq : item * nat => (0 <? snd iq) && negb (mps.eqb i (fst iq))) (all_character_items_slot s SWeapon))).
      inversion H; subst; clear H.
      apply in_map_fst in contra.
      destruct contra.
      apply filter_In in H.
      destruct H.
      apply andb_true_iff in H0.
      destruct H0.
      apply negb_true_iff in H1.
      apply eqb_not in H1.
      simpl in H1.
      contradiction.
    * destruct (list_sum (map snd (all_character_items_slot s SBackpack)) <? 8); try discriminate.
      remember (map fst (filter (fun iq : item * nat => (0 <? snd iq) && negb (mps.eqb i (fst iq))) (all_character_items_slot s SBackpack))).
      inversion H; subst; clear H.
      apply in_map_fst in contra.
      destruct contra.
      apply filter_In in H.
      destruct H.
      apply andb_true_iff in H0.
      destruct H0.
      apply negb_true_iff in H1.
      apply eqb_not in H1.
      simpl in H1.
      contradiction.
  Qed.

  Lemma tcan_take_slot: forall i s lst, tcan_take i s = MustDrop lst ->
    Forall (fun e => item_slot e = item_slot i) lst.
  Proof.
    intros.
    unfold tcan_take in H.
    destruct (item_slot i) eqn: SLOT; try discriminate.
    * destruct (list_sum (map snd (all_character_items_slot s SWeapon)) <? 2); try discriminate.
      remember (map fst (filter (fun iq : item * nat => (0 <? snd iq) && negb (mps.eqb i (fst iq))) (all_character_items_slot s SWeapon))).
      inversion H; subst; clear H.
      apply Forall_forall.
      intros.
      apply in_map_fst in H.
      destruct H.
      apply filter_In in H.
      destruct H. unfold all_character_items_slot in H.
      apply filter_In in H.
      destruct H.
      simpl in *.
      apply mps.eqb_correct in H1.
      exact H1.
    * destruct (list_sum (map snd (all_character_items_slot s SBackpack)) <? 8); try discriminate.
      remember (map fst (filter (fun iq : item * nat => (0 <? snd iq) && negb (mps.eqb i (fst iq))) (all_character_items_slot s SBackpack))).
      inversion H; subst; clear H.
      eapply Forall_forall.
      intros.
      apply in_map_fst in H.
      destruct H.
      apply filter_In in H.
      destruct H. unfold all_character_items_slot in H.
      apply filter_In in H.
      destruct H.
      simpl in *.
      apply mps.eqb_correct in H1.
      exact H1.
  Qed.

  Lemma lookup_helper_found: forall i sl s q,
    ValidMap (items s) ->
    item_slot i = sl ->
    lookup i (all_character_items_slot s sl) = Some q <-> lookup i (items s) = Some q.
  Proof.
    intros i sl s q VMM IS.
    unfold all_character_items_slot.
    split; intro MH.
    * apply vmap_in in MH. 2: {apply Slot.valid. auto. }
      apply filter_In in MH.
      destruct MH as [P1 P2].
      apply vmap_in in P1; auto.
    * apply vmap_in.
      apply Slot.valid. auto.
      apply filter_In.
      simpl.
      split.
      + apply vmap_in; auto.
      + apply mps.eqb_correct in IS.
        exact IS.
  Qed.

  Lemma tcan_take_in: forall i s lst, ValidMap (items s) -> tcan_take i s = MustDrop lst ->
    forall drp,
      In drp lst ->
      i <> drp /\ (sitems drp s > 0)%nat.
  Proof.
    intros i s lst VSS.
    intros.
    split. {
      intro contra.
      subst.
      pose proof (tcan_take_notin _ _ _ H).
      contradiction.
    }
    unfold tcan_take in H.
    destruct (item_slot i) eqn: SLOT; try discriminate.
    * destruct (list_sum (map snd (all_character_items_slot s SWeapon)) <? 2); try discriminate.
      inversion H;subst;clear H.
      unfold all_character_items_slot in H0.
      apply in_map_iff in H0.
      destruct H0 as [x [A B]].
      destruct x as [ni nq].
      simpl in *.
      subst.
      apply filter_In in B.
      destruct B as [B C].
      simpl in C.
      apply filter_In in B.
      destruct B as [B D].
      simpl in *.
      unfold sitems.
      apply vmap_in in B; auto.
      rewrite B.
      apply andb_true_iff in C.
      destruct C.
      apply Nat.ltb_lt in H.
      Lia.lia.
    * destruct (list_sum (map snd (all_character_items_slot s SBackpack)) <? 8); try discriminate.
      inversion H;subst;clear H.
      unfold all_character_items_slot in H0.
      apply in_map_iff in H0.
      destruct H0 as [x [A B]].
      destruct x as [ni nq].
      simpl in *.
      subst.
      apply filter_In in B.
      destruct B as [B C].
      simpl in C.
      apply filter_In in B.
      destruct B as [B D].
      simpl in *.
      unfold sitems.
      apply vmap_in in B; auto.
      rewrite B.
      apply andb_true_iff in C.
      destruct C.
      apply Nat.ltb_lt in H.
      Lia.lia.
  Qed.

  Definition simplify_retrieve_equipment (stt: Stt) (nxt: decision) :=
      let takable := previtems stt in
      fold_right (fun (ic : item * nat) curdec =>
            let (i, q) := ic in 
            if (0 <? q)%nat
              then can_take i q curdec
              else curdec
            ) nxt takable.
  
  Definition item_cost (stt: Stt)  : nat := list_sum (map snd (previtems stt)) + length (previtems stt).

  Lemma update_stt_preserve_cost:
      forall stt f, (previtems stt = previtems (f stt)) -> item_cost (f stt) = item_cost stt.
  Proof.
    intros.
    unfold item_cost.
    rewrite !H.
    reflexivity.
  Qed.

  Definition add_rm (stt: Stt) (add rm: item) :=
    update_items (add_item add 1) (c_rm_items rm 1 stt).

  Lemma add_rm_preserve_cost:
      forall stt add rm, item_cost (add_rm stt add rm) = item_cost stt.
  Proof.
    intros.
    destruct stt. unfold item_cost. simpl. reflexivity.
  Qed.

  Lemma add_rm_preserve_count:
      forall stt add rm sl,
       ValidBag (items stt) ->
       item_slot add = item_slot rm ->
       add <> rm ->
       (sitems rm stt > 0)%nat ->
       all_character_items_slot_count (add_rm stt add rm) sl = all_character_items_slot_count stt sl.
  Proof.
    intros stt add rm sl VMM Hslot Hdiff Hhasrm.
    unfold add_rm.
    pose proof (Slot.add_rm stt add rm VMM Hdiff Hslot Hhasrm sl).
    unfold all_character_items_slot_count, update_items, update_endurance in *; simpl in *.
    unfold sitems, add_item in *. simpl in *.
    assumption.
  Qed.

  Lemma add_rm_preserve: forall stt add rm i,
    ValidMap (items stt) ->
    has_item i stt = true -> rm <> i -> has_item i (add_rm stt add rm) = true.
  Proof.
    intros.
    unfold has_item, add_rm, c_rm_items, has, rm_item, bag_rm in *.
    simpl.
    destruct (eq_dec add Gold).
    * subst. rewrite add_item_gold.
      destruct (eq_dec i Gold).
      + subst.
        rewrite Lookup.lookup_insert_with.
        rewrite Lookup.update_diff; auto.
        destruct (lookup Gold (items stt)); auto.
        destruct (n + 1 <? 51) eqn: X;auto.
        destruct n; simpl; auto.
        apply Update.valid; auto.
      + rewrite Lookup.lookup_insert_with_diff; auto.
        rewrite Lookup.update_diff; auto.
        apply Update.valid; auto.
      + Lia.lia.
    * rewrite add_item_nogold; auto.
      destruct (eq_dec i add).
      + subst.
        rewrite Lookup.lookup_insert_with.
        rewrite Lookup.update_diff; auto.
        destruct (lookup add (items stt)); auto.
        destruct n0; simpl; auto.
        apply Update.valid; auto.
      + rewrite Lookup.lookup_insert_with_diff; auto.
        rewrite Lookup.update_diff; auto.
        apply Update.valid; auto.
  Qed.

  Lemma update_items_preserve_cost:
      forall stt f, item_cost (update_items f stt) = item_cost stt.
  Proof.
    intros.
    destruct stt. unfold item_cost. simpl. reflexivity.
  Qed.

  Program Definition mk_updated_add_rm (stt : Stt) (add rm: item)
    : {nstt | RHelpers.item_cost (add_rm stt add rm) = RHelpers.item_cost stt} :=
  exist _ (add_rm stt add rm) (add_rm_preserve_cost stt add rm).

  Program Definition mk_updated_item (stt : Stt) (f: Items -> Items)
    : {nstt | RHelpers.item_cost (update_items f stt) = RHelpers.item_cost stt} :=
  exist _ (update_items f stt) (update_items_preserve_cost stt f).

  Program Definition mk_updated_stt (stt : Stt) (f: Stt -> Stt) (H: forall stt, previtems stt = previtems (f stt))
    : {nstt | RHelpers.item_cost (f stt) = RHelpers.item_cost stt} :=
  exist _ (f stt) (update_stt_preserve_cost stt f (H stt)).

  Program Definition get_wtakestts (stt: Stt) (i: item) : 
  list {nstt | RHelpers.item_cost nstt = RHelpers.item_cost stt } :=
  match RHelpers.tcan_take i stt with
  | RHelpers.SpaceAvailable => [mk_updated_item stt (add_item i 1)]
  | RHelpers.MustDrop lst => 
      map (fun drp => RHelpers.mk_updated_add_rm stt i drp : {nstt | RHelpers.item_cost nstt = RHelpers.item_cost stt}) lst
  end.

  Lemma get_wtakestts_valid: forall stt i out nstt, ValidState stt ->
     out = get_wtakestts stt i -> In nstt out ->
     ValidState (proj1_sig nstt).
  Proof.
    intros stt i out nstt VSS Eqout IO.
    destruct (tcan_take i stt) eqn: tct.
    { (* space available *)
      unfold get_wtakestts in Eqout. simpl in *. subst.
      rewrite tct in IO.
      destruct nstt; simpl in *. destruct IO; try contradiction.
      inversion H; subst; clear H.
      unfold ValidState, ValidBag in *.
      simpl in *.
      {
      repeat split; try tauto.
        * destruct stt; simpl in *. unfold update_items in *; simpl.
          unfold sitems. simpl.
          destruct (eq_dec Gold i).
          + subst. rewrite add_item_gold; auto.
            rewrite Lookup.lookup_insert_with; try tauto.
            destruct (lookup Gold items) eqn: G.
            - destruct (n + 1 <? 51) eqn:LT; auto.
              apply Nat.ltb_lt in LT. Lia.lia.
            - Lia.lia.
          + rewrite add_item_nogold; auto.
            rewrite Lookup.lookup_insert_with_diff; try tauto.
        * unfold tcan_take in tct.
          destruct (item_slot i) eqn: SLOT.
          + rewrite Slot.no_increase; try tauto. unfold ValidBag. tauto.
            intro contra.
            rewrite SLOT in contra.
            discriminate.
          + rewrite Slot.increase; auto; try tauto. 
            destruct (list_sum (map snd (all_character_items_slot stt SBackpack)) <? 8) eqn: LT.
            apply Nat.ltb_lt in LT. unfold all_character_items_slot_count.
            simpl. simpl in LT. Lia.lia.
            discriminate.
            intro contra.
            subst.
            discriminate.
          + rewrite Slot.no_increase; try (unfold ValidBag; tauto). rewrite SLOT. intro contra. discriminate.
          + rewrite Slot.no_increase; try (unfold ValidBag; tauto). rewrite SLOT. intro contra. discriminate.
        * unfold tcan_take in tct.
          destruct (item_slot i) eqn: SLOT.
          + rewrite Slot.increase; auto; try tauto. 
            destruct (list_sum (map snd (all_character_items_slot stt SWeapon)) <? 2) eqn: LT.
            apply Nat.ltb_lt in LT. unfold all_character_items_slot_count.
            simpl. simpl in LT. Lia.lia.
            discriminate.
            intro contra.
            subst.
            discriminate.
          + rewrite Slot.no_increase; try (unfold ValidBag; tauto). rewrite SLOT. intro contra. discriminate.
          + rewrite Slot.no_increase; try (unfold ValidBag; tauto). rewrite SLOT. intro contra. discriminate.
          + rewrite Slot.no_increase; try (unfold ValidBag; tauto). rewrite SLOT. intro contra. discriminate.
        * unfold add_item. destruct i; apply Insert.insert_with_valid; tauto.
        * destruct (eq_dec Gold i).
          + unfold add_item. subst. 
            apply Forall_forall.
            intros.
            destruct x.
            simpl in *.
            apply Nat.eqb_neq.
            intro contra.
            subst.
            apply vmap_in in H. 2: {
              apply Insert.insert_with_valid.
              tauto.
            }
            destruct (eq_dec i Gold).
            - subst.
              rewrite Lookup.lookup_insert_with in H.
              destruct (lookup Gold (items stt)) eqn: GLD.
              ** destruct (n + 1 <? 51); simpl in H. inversion H.
                 Lia.lia.
                inversion H.
              ** inversion H.
              ** tauto.
            - rewrite Lookup.lookup_insert_with_diff in H; auto; try tauto.
              apply vmap_in in H; try tauto.
              destruct VSS as [A [B [C [D [E [F [G [[I1 I2] [J [K L]]]]]]]]]].
              eapply Forall_forall in I2; eauto.
              simpl in I2.
              discriminate.
          + rewrite add_item_nogold; auto.
            apply add_valid_no_zero; auto.
            unfold ValidBag.
            tauto.
        * assert (max_hp stt <= max_hp (update_items (add_item i 1) stt))%nat as MHP. {
            apply (VS.max_hp_add_items stt i 1); try tauto.
            Lia.lia.
          }
          Lia.lia.
      }
    }
    { (* must drop *)
    unfold get_wtakestts in Eqout. simpl in *. subst.
    pose proof (tcan_take_notin i stt l tct) as NI.
    rewrite tct in IO.
    apply in_map_iff in IO.
    destruct IO as [drp [Heq Hin]].
    inversion Heq; subst. simpl. clear H.
    unfold ValidState in *.
    simpl in *.
    repeat (split; try tauto).
    + unfold max_hp.
      apply Nat.min_glb_lt; try Lia.lia.
      assert (0 < maxendurance (update_items (rm_item drp 1) stt))%nat. {
        unfold update_items.
        simpl.
        Lia.lia.
      }
      Lia.lia.
    + unfold sitems, add_rm, update_items; simpl.
      destruct (eq_dec i Gold).
      - subst.
        rewrite add_item_gold; auto.
        rewrite Lookup.lookup_insert_with.
        destruct (eq_dec drp Gold).
        subst. contradiction.
        unfold rm_item, bag_rm.
        rewrite Lookup.update_diff; auto.
        -- destruct (lookup Gold (items stt)).
           destruct (n0 + 1 <? 51) eqn: L51.
           apply Nat.ltb_lt in L51.
           Lia.lia.
           Lia.lia.
           Lia.lia.
        -- unfold ValidBag in VSS. tauto.
        -- pose proof (rm_valid (items stt) drp 1).
           lapply H. intros. destruct H0. tauto. tauto.
      - rewrite add_item_nogold; auto.
        rewrite Lookup.lookup_insert_with_diff; auto.
        destruct (eq_dec Gold drp).
        --  subst. unfold rm_item, bag_rm.
            rewrite Lookup.update_eq_b; auto; try tauto.
            destruct (lookup Gold (items stt)) eqn: GLD; try Lia.lia.
            destruct n0; simpl; auto. Lia.lia.
            destruct n0; simpl; auto. Lia.lia.
            unfold sitems in VSS. rewrite GLD in VSS. Lia.lia.
            unfold ValidBag in VSS.
            tauto.
        -- unfold rm_item, bag_rm.
           unfold ValidBag, sitems in VSS.
           rewrite Lookup.update_diff; auto; tauto.
        -- unfold rm_item.
           apply Update.valid.
           unfold ValidBag in VSS.
           tauto.
    + rewrite add_rm_preserve_count; try tauto.
      - apply tcan_take_slot in tct.
        eapply Forall_forall in tct; eauto.
      - intro contra. subst. contradiction.
      - epose proof (tcan_take_in _ _ _ _ tct _ Hin). tauto.
        Unshelve. unfold ValidBag in VSS. tauto.
    + rewrite add_rm_preserve_count; try tauto.
      - apply tcan_take_slot in tct.
        eapply Forall_forall in tct; eauto.
      - intro contra. subst. contradiction.
      - epose proof (tcan_take_in _ _ _ _ tct _ Hin). tauto.
        Unshelve. unfold ValidBag in VSS. tauto.
    + destruct (eq_dec i Gold).
      - subst. simpl.
        apply Insert.insert_with_valid.
        unfold rm_item.
        unfold bag_rm.
        apply Update.valid.
        unfold ValidBag in VSS.
        tauto.
      - rewrite add_item_nogold;auto.
        apply Insert.insert_with_valid.
        unfold rm_item.
        unfold bag_rm.
        apply Update.valid.
        unfold ValidBag in VSS.
        tauto.
    + apply Forall_forall.
      intros.
      destruct x as [itm q].
      simpl.
      apply Nat.eqb_neq.
      intro contra.
      subst.
      apply vmap_in in H. 2: {
        apply add_item_valid_bag. Lia.lia.
        apply rm_valid.
        tauto.
      }
      apply lookup_cant_be_zero in H; auto.
      apply add_item_valid_bag. Lia.lia.
      apply rm_valid.
      tauto.
    + assert (ValidMap (items (update_items (rm_item drp 1) stt))) as CND. {
        unfold update_items, rm_item, bag_rm.
        simpl.
        apply Update.valid. unfold ValidBag in VSS.
        tauto.
      }
      pose proof (VS.has_item_kept (update_items (rm_item drp 1) stt) BodyArmor i 1 CND) as BA.
      pose proof (VS.has_item_kept (update_items (rm_item drp 1) stt) Helmet i 1 CND) as HE.
      unfold add_rm, c_rm_items, update_items, max_hp, has_item in *.
      simpl in *.
      remember (rm_item drp 1 (items stt)) as rstt.
      assert ((if has Helmet (add_item i 1 rstt) then 2 else 0) <= 2)%nat. {
        destruct (has Helmet (add_item i 1 rstt)); Lia.lia.
      }
      assert ((if has BodyArmor (add_item i 1 rstt) then 4 else 0) <= 4)%nat. {
        destruct (has BodyArmor (add_item i 1 rstt)); Lia.lia.
      }
      { destruct (has BodyArmor rstt) eqn: PBA.
      * lapply BA; auto.
        clear BA; intro BA.
        rewrite BA.
        destruct (has Helmet rstt) eqn: PHE.
        + lapply HE; auto.
          clear HE; intro HE.
          rewrite HE.
          pose proof (Nat.le_min_r (curendurance stt) (maxendurance stt + 4 + 2)).
          Lia.lia.
        + Lia.lia.
      * destruct (has Helmet rstt) eqn: PHE; try Lia.lia.
        lapply HE; auto.
        clear HE; intro HE.
        rewrite HE.
        Lia.lia.
      }
    }
  Qed.

End RHelpers.

Section FlattenDec.

Ltac fd_solver := match goal with
  | |- RHelpers.item_cost (update_items _ ?stt) = RHelpers.item_cost ?stt => 
        apply RHelpers.update_items_preserve_cost
  | nstt : {_ | RHelpers.item_cost _ = RHelpers.item_cost _} |- _ => destruct nstt; simpl in *
  | |- _ => Lia.lia
  end.

Local Obligation Tactic := intros; subst; simpl; repeat fd_solver.

Equations flatten_decision (stt: Stt) (d: decision) : list (Proba next_step) 
  by wf (RHelpers.dec_amount (RHelpers.item_cost stt) d) lt :=
flatten_decision stt (can_take _ 0 _) := []; (* can't happen *)
flatten_decision stt (can_take i 1 nxt) :=
  let notake := flatten_decision stt nxt in
  let wtakestts := RHelpers.get_wtakestts stt i in
     notake ++ flat_map (fun nstt => flatten_decision (proj1_sig nstt) nxt) wtakestts;
flatten_decision stt (can_take i (S (S q'')) nxt) :=
  let q' := S q'' in
  let notake := flatten_decision stt nxt in
  let wtakestts := RHelpers.get_wtakestts stt i in
    notake ++ flat_map (fun nstt => flatten_decision (proj1_sig nstt) (can_take i q' nxt)) wtakestts;
flatten_decision stt (App d1 d2) := flatten_decision stt d1 ++ flatten_decision stt d2;
flatten_decision stt (retrieve_equipment nxt) :=
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
        flatten_decision nstt ndecision;
flatten_decision stt (can_buy i cost nxt) :=
  let notake := flatten_decision stt nxt in
  let pstt: Stt := c_rm_items Gold cost stt in
  let wtakestts := RHelpers.get_wtakestts pstt i in
    if cost <? sitems Gold stt
      then notake ++ flat_map (fun nstt => flatten_decision (proj1_sig nstt) nxt) wtakestts
      else notake;
flatten_decision stt _ := []
.
Next Obligation.
unfold ndecision.
unfold RHelpers.simplify_retrieve_equipment.
unfold nstt.
clear ndecision nstt.
destruct stt; simpl in *.
unfold RHelpers.item_cost. simpl.
clear flatten_decision flags items chapter disciplines sk maxendurance curendurance book.
induction previtems; simpl; try Lia.lia.
destruct a.
destruct (0 <? n); simpl; Lia.lia.
Defined.
Next Obligation.
rewrite e.
Lia.lia.
Defined.
Next Obligation.
rewrite e.
Lia.lia.
Defined.
Next Obligation.
rewrite e.
subst pstt.
assert (RHelpers.item_cost (c_rm_items Gold cost stt) = RHelpers.item_cost stt). {
  apply RHelpers.update_stt_preserve_cost.
  destruct stt.
  reflexivity.
}
rewrite H.
Lia.lia.
Defined.

Lemma simplify_retrieve_wf : forall lst d,
  WellFormedDecision d ->
  WellFormedDecision (fold_right (fun (ic: item * nat) curdec => let (i, q) := ic in if 0 <? q then can_take i q curdec else curdec) d lst).
Proof.
  induction lst as [| [i q] lst IH]; intros d WFD.
  - simpl. assumption.
  - simpl. destruct (0 <? q) eqn:Hq.
    + apply check_can_take.
      * apply Nat.ltb_lt in Hq. Lia.lia.
      * apply IH; auto.
    + apply IH; auto.
Qed.

Lemma simplify_retrieve_cost : forall lst d,
  (RHelpers.dec_amount 0
    (fold_right (fun (ic: item * nat) curdec => let (i, q) := ic in if 0 <? q then can_take i q curdec else curdec) d lst)
  <= list_sum (map snd lst) + length lst + RHelpers.dec_amount 0 d)%nat.
Proof.
  induction lst as [| [i q] lst IH]; intros d.
  - simpl. Lia.lia.
  - simpl. destruct (0 <? q) eqn:Hq; simpl; pose proof (IH d); Lia.lia.
Qed.

Theorem flatten_dec_full: forall stt d sol, 
  ValidState stt -> WellFormedDecision d -> sol = flatten_decision stt d
  -> Forall FullProba sol /\ exists x, In x sol.
Proof.
  intros stt d sol VS WFD Heq.
  
  (* Generalize our measure to use strong induction *)
  remember (RHelpers.dec_amount (RHelpers.item_cost stt) d) as n.
  generalize dependent stt. 
  generalize dependent d. 
  generalize dependent sol.
  
  (* Strong induction on the measure n *)
  induction n as [n IHn] using lt_wf_ind.
  intros sol d WFD stt VS Heq Hn.

  destruct d;simp flatten_decision in Heq.

  - (* App *) subst sol.
    inversion WFD; subst.
    assert (Forall FullProba (flatten_decision stt d1) /\ 
            exists x, In x (flatten_decision stt d1)) as IH1.
    {
      (* We tell IHn what our smaller measure 'm' is *)
      eapply (IHn (RHelpers.dec_amount (RHelpers.item_cost stt) d1)); try reflexivity; auto.
      simpl. Lia.lia. 
    }
    assert (Forall FullProba (flatten_decision stt d2) /\ 
            exists x, In x (flatten_decision stt d2)) as IH2.
    {
      eapply (IHn (RHelpers.dec_amount (RHelpers.item_cost stt) d2)); try reflexivity; auto.
      simpl. Lia.lia.
    }
    destruct IH1 as [Hforall1 [x1 Hexists1]].
    destruct IH2 as [Hforall2 [x2 Hexists2]].
    split.
    + apply Forall_app.
      split; assumption.
      
    + exists x1.
      apply in_app_iff.
      left. assumption.
  - (* retrieve equipment *) 
    set (nstt := {| book := book stt; maxendurance := maxendurance stt; 
                    curendurance := curendurance stt; sk := sk stt; 
                    disciplines := disciplines stt; chapter := chapter stt; 
                    items := items stt; flags := flags stt; previtems := empty |}) in *.
    set (ndecision := RHelpers.simplify_retrieve_equipment stt d) in *.
    eapply (IHn (RHelpers.dec_amount (RHelpers.item_cost nstt) ndecision)). 4: {
      apply Heq.
    }
    + subst n.
      unfold nstt. unfold RHelpers.item_cost at 1. simpl.
      pose proof (simplify_retrieve_cost (previtems stt) d) as Hcost.
      unfold RHelpers.item_cost.
      unfold ndecision.
      unfold RHelpers.simplify_retrieve_equipment.
      Lia.lia.
    + apply (simplify_retrieve_wf (previtems stt) d).
      inversion WFD; subst. assumption.
    + unfold nstt in *. subst. clear nstt.
      destruct stt. unfold ValidState in *. simpl in *.
      unfold ValidBag in VS.
      repeat split; try tauto. constructor. constructor.
    + reflexivity.
  - (* can_take A *) 
    subst sol.
    inversion WFD; subst; clear WFD.
    destruct n0; try Lia.lia.

    assert (Forall FullProba (flatten_decision stt d) /\ 
            exists x, In x (flatten_decision stt d)) as IH_notake.
    {
      eapply (IHn (RHelpers.dec_amount (RHelpers.item_cost stt) d)); try reflexivity; auto.
      simpl. Lia.lia.
    }
    destruct IH_notake as [Hforall_notake [x_notake Hexists_notake]].
    simpl in *.
    destruct n0 as [| q']; simp flatten_decision; simpl.
    + split.
      * apply Forall_app.
        split.
        -- assumption.
        -- apply Forall_forall.
          intros x Hx.
          apply in_flat_map in Hx.
          destruct Hx as [nstt [Hnstt_in Hx_in]].
           
           (* Apply IHn to the inner call using the new state nstt *)
          assert (Forall FullProba (flatten_decision (proj1_sig nstt) d) /\ 
                   exists x, In x (flatten_decision (proj1_sig nstt) d)) as IH_nstt.
          {
            eapply (IHn (RHelpers.dec_amount (RHelpers.item_cost (proj1_sig nstt)) d)); try reflexivity; auto.
            - destruct nstt as [stt' Heq_cost]. simpl in *. 
              (* The item_cost is mathematically identical, so the measure decreases! *)
              rewrite Heq_cost. Lia.lia.
            - eapply RHelpers.get_wtakestts_valid; eauto.
          }
          destruct IH_nstt.
          eapply Forall_forall in H; eauto.
      * exists x_notake.
        apply in_app_iff.
        left.
        exact Hexists_notake.
    + split.
      * apply Forall_app.
        split; auto.
        apply Forall_forall.
        intros x Hx.
        apply in_flat_map in Hx.
        destruct Hx as [nstt [Hnstt_in Hx_in]].
          assert (Forall FullProba (flatten_decision (proj1_sig nstt) (can_take i (S q') d)) /\ 
                   exists x, In x (flatten_decision (proj1_sig nstt) (can_take i (S q') d))) as IH_nstt.
          {
            eapply (IHn (RHelpers.dec_amount (RHelpers.item_cost (proj1_sig nstt)) (can_take i (S q') d))); try reflexivity; auto.
            - destruct nstt as [stt' Heq_cost]. simpl in *. 
              (* The item_cost is mathematically identical, so the measure decreases! *)
              rewrite Heq_cost. Lia.lia.
            - constructor; auto. Lia.lia.
            - eapply RHelpers.get_wtakestts_valid; eauto.
          }
          destruct IH_nstt.
          eapply Forall_forall in H; eauto.
      * exists x_notake.
        apply in_app_iff.
        left.
        exact Hexists_notake.
  - (* can_buy *)
    inversion WFD; subst; clear WFD.
    assert (Forall FullProba (flatten_decision stt d) /\ 
            exists x, In x (flatten_decision stt d)) as IH_notake.
    {
      eapply (IHn (RHelpers.dec_amount (RHelpers.item_cost stt) d)); try reflexivity; auto.
    }
    destruct IH_notake as [Hforall_notake [x_notake Hexists_notake]].
    simpl.
    destruct (n0 <? sitems Gold stt).
    + split.
      * apply Forall_app.
        split; auto.
        apply Forall_forall.
        intros x Hx.
        apply in_flat_map in Hx.
        destruct Hx as [nstt [Hnstt_in Hx_in]].
        assert (Forall FullProba (flatten_decision (proj1_sig nstt) d) /\ 
                  exists x, In x (flatten_decision (proj1_sig nstt) d)) as IH_nstt.
        {
          eapply (IHn (RHelpers.dec_amount (RHelpers.item_cost (proj1_sig nstt)) d)); try reflexivity; auto.
          - destruct nstt as [stt' Heq_cost]. simpl in *. 
            rewrite Heq_cost.
            assert (RHelpers.item_cost (c_rm_items Gold n0 stt) = RHelpers.item_cost stt). {
              apply RHelpers.update_stt_preserve_cost.
              unfold c_rm_items, update_endurance; simpl.
              reflexivity.
            }
            rewrite H.
            Lia.lia.
          - eapply RHelpers.get_wtakestts_valid; eauto.
          - eapply RHelpers.get_wtakestts_valid; eauto.
        }
        
    split.
  - (* can_sell *)
    inversion WFD; subst; clear WFD.
    admit.
  - (* conditional *)
    inversion WFD; subst; clear WFD.
    admit.
  - (* special *)
    inversion WFD; subst; clear WFD.
    admit.
  - (* none *)
    inversion WFD; subst; clear WFD.
    admit.
  - (* evade_fight *)
    inversion WFD; subst; clear WFD.
    admit.
  - (* after_combat *)
    inversion WFD; subst; clear WFD.
    admit.
  - (* remove_item_from *)
    inversion WFD; subst; clear WFD.
    admit.
Admitted.

End FlattenDec.
