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

Fixpoint s_update_d (maxdepth: nat) (stt: Stt) (outcome: chapter_outcome): Proba next_step :=
  let certain (s: Stt) := single_outcome (New s)
  in
  if curendurance stt =? 0
    then single_outcome Lost
  else match maxdepth with
  | 0 => single_outcome Lost
  | S md =>
    match outcome with
      | Goto next => 
        let max_chapter: nat := if Nat.eqb (book stt) 5 then 400 else 350 in
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
      | Simple effects co => s_update_d md (fold_left update_simple effects stt) co
      | Conditionally [] => single_outcome Lost
      | Conditionally ((cnd, co)::rst) =>
          if check_cond stt cnd
              then s_update_d md stt co
              else s_update_d md stt (Conditionally rst)
      | Randomly lst => fold_right (fun (cndo: chapter_outcome * Qc) out =>
          let (curo, pb) := cndo in
          let curprob := s_update_d md stt curo in
              add_proba (mul_proba curprob pb) out
          ) nil lst
      | LoseItemFrom s q next =>
            match q with
            | 0 => s_update_d md stt next
            | S nq =>
                  let losable := map (fun i => (i, sitems i stt)) (slot_items s) in
                  let amnt := list_sum (map snd losable) in
                   []
            end
      | Fight de co => []
      | OneRound de a b c => []
    end
  end .

Definition s_update (stt: Stt) (outcome: chapter_outcome): Proba next_step :=
    s_update_d 1000 stt outcome.

Inductive WellFormedCO: chapter_outcome -> Prop :=
  | check_conditionally: forall lst,
      (forall stt, ValidState stt -> exists cnd co, In (cnd, co) lst /\ check_cond stt cnd = true) ->
        Forall WellFormedCO (map snd lst) ->
        WellFormedCO (Conditionally lst)
  | check_randomly: forall lst,
        FullProba lst ->
        Forall WellFormedCO (map fst lst) ->
        WellFormedCO (Randomly lst)
  | check_fight: forall dt o, WellFormedCO o -> WellFormedCO (Fight dt o)
  | check_oneround: forall dt o1 o2 o3,
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
      WellFormedCO (LoseItemFrom s q o)
  .

Inductive WellFormedDecision: decision -> Prop :=
  | check_decisions: forall decs, Forall WellFormedDecision decs -> WellFormedDecision (Decisions decs)
  | check_retrieve_equipment: forall d, WellFormedDecision d -> WellFormedDecision (retrieve_equipment d)
  | check_can_take: forall i q d, WellFormedDecision d -> WellFormedDecision (can_take i q d)
  | check_can_buy: forall i q d, WellFormedDecision d -> WellFormedDecision (can_buy i q d)
  | check_can_sell: forall i q d, WellFormedDecision d -> WellFormedDecision (can_sell i q d)
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

Theorem rules_are_preserving: forall stt co, ValidState stt -> WellFormedCO co ->
        let sol := s_update stt co
         in FullProba sol /\ Forall CheckNS (map fst sol).
Proof.
  intros stt co VS WCO.
  unfold FullProba.
  induction co; inversion WCO; subst; split; simpl.
Admitted.