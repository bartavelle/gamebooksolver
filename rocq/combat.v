Require Import Stdlib.Lists.List.
Import ListNotations.

Require Import hits.
Require Import proba.
Require Import mps.
Require Import character.
Require Import chapters.
From Equations Require Import Equations.
From Stdlib Require Import ZArith.
Require Import Stdlib.QArith.QArith.
Require Import Stdlib.QArith.Qcanon.

Lemma hits_has_13_elements: length HITSCHART = 13%nat.
Proof.
  reflexivity.
Qed.

Lemma all_hits_10: forall x, In x HITSCHART -> length x = 10%nat.
Proof.
  apply Forall_forall.
  repeat (constructor; auto).
Qed.

Record FightModifiers: Set := mkFM
  { stop_fight: option cid
  ; evaded: option cid
  ; poisonous: option Qc
  ; onlose: option cid
  ; double_damage: bool
  ; undead: bool
  ; force_emindblast: bool
  ; emindblast: bool
  ; notyetwon: option cid
  }.

Inductive TEscaped : Set :=
  | Escaped: cid -> nat -> TEscaped
  | Std: nat -> TEscaped
  | LateWin: cid -> nat -> TEscaped
  | TLost: cid -> TEscaped
  | Stopped: cid -> nat -> TEscaped
  .

Module TE.
  Definition te_eqb (a b: TEscaped): bool :=
  match (a, b) with
  | (Escaped c1 a1, Escaped c2 a2) => order.eqb c1 c2 && order.eqb a1 a2
  | (LateWin c1 a1, LateWin c2 a2) => order.eqb c1 c2 && order.eqb a1 a2
  | (Stopped c1 a1, Stopped c2 a2) => order.eqb c1 c2 && order.eqb a1 a2
  | (TLost a1, TLost a2) => order.eqb a1 a2
  | (Std a1, Std a2) => order.eqb a1 a2
  | _ => false
  end.


  Lemma eqb_correct: forall (x y: TEscaped), (x = y) <-> te_eqb x y = true.
  Proof.
    split; intros.
    { subst.
      destruct y; unfold te_eqb; repeat (rewrite order.eqb_refl; auto).
    }
    { unfold te_eqb in H. destruct x, y; try (apply Bool.andb_true_iff in H); auto; try discriminate.
      destruct H; apply eqb_correct in H, H0; subst; auto.
      apply eqb_correct in H; subst; auto.
      destruct H; apply eqb_correct in H, H0; subst; auto.
      apply eqb_correct in H; subst; auto.
      destruct H; apply eqb_correct in H, H0; subst; auto.
    }
  Qed.

  Definition te_cmp (a b: TEscaped): Order :=
  match (a, b) with
  | (Escaped c1 a1, Escaped c2 a2) => SH.chain (cmp c1 c2) (cmp a1 a2)
  | (Escaped _ _, _) => GT
  | (_, Escaped _ _) => LT
  | (LateWin c1 a1, LateWin c2 a2) => SH.chain (cmp c1 c2) (cmp a1 a2)
  | (LateWin _ _, _) => GT
  | (_, LateWin _ _) => LT
  | (Stopped c1 a1, Stopped c2 a2) => SH.chain (cmp c1 c2) (cmp a1 a2)
  | (Stopped _ _, _) => GT
  | (_, Stopped _ _) => LT
  | (TLost a1, TLost a2) => cmp a1 a2
  | (TLost _, _) => GT
  | (_, TLost _) => LT
  | (Std a1, Std a2) => cmp a1 a2
  end.

  Ltac run :=
    match goal with
    | |- SH.chain LT _ = _ => simpl
    | |- SH.chain EQ _ = _ => simpl
    | |- SH.chain GT _ = _ => simpl
    | H: SH.chain (NatLemma.nat_cmp _ _) _ = _ |- _ => rewrite SH.rw in H
    | |- SH.chain (NatLemma.nat_cmp _ _) _ = _ => rewrite SH.rw
    | |- SH.chain (cmp ?a ?a) _ = _ => rewrite cmp_refl
    | |- cmp ?a ?a = EQ => apply cmp_refl
    | |- NatLemma.nat_cmp ?a ?a = EQ => apply NatLemma.cmp_eq
    | H: SH.chain (cmp _ _) _ = LT |- _ => apply SH.chain_lt in H
    | H: SH.chain (cmp _ _) _ = GT |- _ => apply SH.chain_gt in H
    | H: cmp ?a ?b = EQ |- _ => apply cmp_eq in H; subst
    | H: cmp ?a ?b = _ |- SH.chain (cmp ?b ?a) _ = _ => apply cmp_opp in H
    | H: cmp ?a ?b = _ |- SH.chain (cmp ?a ?b) _ = _ => rewrite H
    | H1: cmp ?a ?b = ?r, H2: cmp ?b ?c = ?r |- SH.chain (cmp ?a ?c) _ = ?r =>
            pose proof (cmp_trans _ _ _ _ H1 H2)
    | H1: cmp ?a ?b = ?r, H2: cmp ?b ?c = ?r |- cmp ?a ?c = ?r =>
            apply (cmp_trans _ _ _ _ H1 H2)
    | H1: cmp ?a ?b = ?r, H2: cmp ?b ?c = ?r |- NatLemma.nat_cmp ?a ?c = ?r =>
            apply (NatLemma.cmp_trans _ _ _ _ H1 H2)
    | H: _ \/ _ |- _ => destruct H
    | H: _ /\ _ |- _ => destruct H
    | H: SH.chain _ _ = EQ |- _ => apply SH.chain_eq in H
    | H: Escaped _ _ _ = Escaped _ _ _ |- _ => inversion H; subst
    | H: LateWin _ _ _ = LateWin _ _ _ |- _ => inversion H; subst
    | H: Stopped _ _ _ = Stopped _ _ _ |- _ => inversion H; subst
    | H: TLost _ _ = TLost _ _ |- _ => inversion H; subst
    | H: Std _ _ = Std _ _ |- _ => inversion H; subst
    end.

  Definition te_lt (a b: TEscaped) := te_cmp a b = LT.

  Lemma cmp_correct: forall a b, te_cmp a b = LT <-> te_lt a b.
  Proof.
    unfold te_lt. split; intros; auto.
  Qed.

  Lemma cmp_opp: forall a b, te_cmp a b = LT <-> te_cmp b a = GT.
  Proof.
    intros.
    destruct a, b; unfold te_cmp; split; intros; try discriminate; auto; repeat run; auto; unfold cmp in *; simpl in *; 
     apply NatLemma.cmp_opp; auto.
  Qed.


  Lemma cmp_trans: forall r a b c, te_cmp a b = r -> te_cmp b c = r -> te_cmp a c = r.
  Proof.
    unfold te_cmp.
    intros.
    destruct a, b, c; try discriminate; auto; auto;
    destruct r; subst; repeat run; auto; try discriminate.
  Qed.

  Lemma cmp_eq {A: Set} `{OrdDec A} : forall a b, te_cmp a b = EQ <-> a = b.
  Proof.
    unfold te_cmp.
    destruct a, b; split; intros; repeat run; auto; try discriminate.
    * inversion H1; subst.
      rewrite cmp_refl.
      rewrite cmp_refl. auto.
    * inversion H1; subst.
      apply cmp_refl.
    * inversion H1; subst.
      rewrite cmp_refl.
      rewrite cmp_refl. auto.
    * inversion H1; subst.
      apply cmp_refl.
    * inversion H1; subst.
      rewrite cmp_refl.
      rewrite cmp_refl. auto.
  Qed.
End TE.


Instance te_EqDec: order.EqDec TEscaped := {
  eqb := TE.te_eqb;
  eqb_correct := TE.eqb_correct;
}.

Instance te_OrdDec: OrdDec TEscaped (te_EqDec) :=
{
  cmp := TE.te_cmp;
  lt := TE.te_lt;
  cmp_correct := TE.cmp_correct;
  cmp_opp := TE.cmp_opp;
  cmp_trans := TE.cmp_trans;
  cmp_eq := TE.cmp_eq;
}.

Module Modifiers.

  Definition gStopFight (m: fight_modifier): option cid :=
    match m with
    | StopFight x => Some x
    | _ => None
    end.

  Definition gFakeFight (m: fight_modifier): option cid :=
    match m with
    | FakeFight x => Some x
    | _ => None
    end.

  Definition gOnlose (m: fight_modifier): option cid :=
    match m with
    | OnLose x => Some x
    | _ => None
    end.

  Definition gEvaded (m: fight_modifier): option cid :=
    match m with
    | Evaded x => Some x
    | _ => None
    end.

  Definition gPoisonous (m: fight_modifier): option Qc :=
    match m with
    | Poisonous x => Some x
    | _ => None
    end.

  Definition gOnNotYetWon (m: fight_modifier): option cid :=
    match m with
    | OnNotYetWon x => Some x
    | _ => None
    end.

  Fixpoint extractl {A FM} (matcher: FM -> option A) (mds: list FM): option A :=
    match mds with
    | [] => None
    | x::xs => match matcher x with
              | Some v => Some v
              | None => extractl matcher xs
              end
    end.

  (* those stay active once their timer has expired *)
  Definition survives_timer (m: fight_modifier): bool :=
    match m with
    | Evaded _ | OnNotYetWon _ | ForceEMindblast | StopFight _ => true
    | _ => false
    end.

  Fixpoint advance_time (mds: list fight_modifier): list fight_modifier :=
  match mds with
  | [] => []
  | Timed (S (S n)) x::xs => Timed (S n) x :: advance_time xs
  | Timed _ x::xs => if survives_timer x then x :: advance_time xs else advance_time xs
  | x::xs => x :: advance_time xs
  end.

Fixpoint wspec (d: DisciplineSet) (wpns: list weapon): option weapon :=
  match wpns with
  | [] => None
  | w::wq => if s_check (WeaponSkill w) d then Some w else wspec d wq
  end.

  Fixpoint get_cur_mods (mds: list fight_modifier): list fight_modifier :=
    match mds with
    | [] => []
    | x::xs => match x with
               (* those only start once the timer has expired *)
               | Timed _ (Evaded _) | Timed _ (StopFight _) | Timed _ ForceEMindblast => get_cur_mods xs
               | Timed _ k => k :: get_cur_mods xs
               | k => k :: get_cur_mods xs
               end
    end.

  Lemma get_cur_mods_cons: forall mds md,
       (exists n ev, md = Timed n (Evaded ev) /\ get_cur_mods (md::mds) = get_cur_mods mds)
    \/ (exists n sd, ~(exists ev, sd = Evaded ev) /\ md = Timed n sd /\ get_cur_mods (md::mds) = sd::get_cur_mods mds)
    \/ ((~exists n df, md = Timed n df) /\ get_cur_mods (md::mds) = md :: get_cur_mods mds)
    .
Proof.
Admitted.

  Definition ValidMods (mds: list fight_modifier): Prop :=
    let cur := get_cur_mods mds in
    let valid_mods := fun md => match md with | PlayerInvulnerable | EnemyInvulnerable | Timed _ (Timed _ _) => False | _ => True end in
    Forall valid_mods mds
    /\ (~In PlayerInvulnerable cur)
    /\ (~In EnemyInvulnerable cur)
    /\ (In PlayerInvulnerable cur -> ~In EnemyInvulnerable cur)
    /\ (In EnemyInvulnerable cur -> ~In PlayerInvulnerable cur)
    .

    Lemma valid_mod_dec: forall md,
    {match md with
     | PlayerInvulnerable | EnemyInvulnerable | Timed _ (Timed _ _) => False
     | _ => True
     end}
  + {~match md with
     | PlayerInvulnerable | EnemyInvulnerable | Timed _ (Timed _ _) => False
     | _ => True
     end}.
    Proof.
      destruct md; simpl; auto.
      destruct md; simpl; auto.
    Qed.

    Lemma ValidMods_dec: forall mds, {Modifiers.ValidMods mds} + {~Modifiers.ValidMods mds}.
    Proof.
      intro mds.
      unfold Modifiers.ValidMods.
      set (cur := Modifiers.get_cur_mods mds).
      set (valid_mods := fun md => match md with
        | PlayerInvulnerable | EnemyInvulnerable | Timed _ (Timed _ _) => False
        | _ => True end).
      destruct (Forall_dec valid_mods valid_mod_dec mds) as [HF|HF];
      destruct (In_dec eqdec_dec PlayerInvulnerable cur) as [HPI|HPI];
      destruct (In_dec eqdec_dec EnemyInvulnerable cur) as [HEI|HEI];
      (* now just tauto for all 8 combinations *)
      (left; tauto) || (right; tauto).
    Qed.

  Lemma in_get_cur_mods_cons: forall x md xs,
    In md (get_cur_mods xs) ->
    In md (get_cur_mods (x::xs)).
  Proof.
    intros.
    simpl.
    destruct x; simpl; try tauto.
    destruct x; simpl; try tauto.
  Qed.

  Lemma advance_time_not_timed: forall m ms, (~exists n s, m = Timed n s) <-> advance_time (m :: ms) = m :: advance_time ms.
Proof.
Admitted.

  Fixpoint fm_depth (a: fight_modifier): nat := match a with
      | Timed _ s => S (fm_depth s)
      | _ => 0
      end.

  Lemma get_cur_mods_swap_queue: forall a mds1 mds2,
      get_cur_mods (a :: mds1) = a :: get_cur_mods mds1 ->
      get_cur_mods (a :: mds2) = a :: get_cur_mods mds2.
Proof.
Admitted.



  Lemma advance_time_correct: forall mds, ValidMods mds -> ValidMods (advance_time mds).
Proof.
Admitted.

End Modifiers.

Definition addcombatbonus (fm: fight_modifier) (cur: Z)  :=
match fm with
| CombatBonus x => (cur + x)%Z
| _ => cur
end.

Fixpoint gdpr (lst: list fight_modifier): nat  :=
match lst with
| [] => 0
| x::xs => match x with
           | Dpr n => n + gdpr xs
           | _ => gdpr xs
           end
end.

Definition get_ratio (stt: Stt) (osk: Z) (mds: list fight_modifier): Z :=
  let hasd d := s_check d (disciplines stt) in
  let curmods := Modifiers.get_cur_mods mds in
  let hasmod m := existsb (FM.fm_eqb m) curmods in
  let skdiff := (sk stt - osk)%Z in
  let combat_bonus := fold_right addcombatbonus 0%Z curmods in
  let mspec := Modifiers.wspec (disciplines stt) all_weapons in
  let wpnbonus :=
      if ((all_character_items_slot_count stt SWeapon =? 0) || hasmod BareHanded)%bool
        then (-4)%Z
        else if has_item (Weapon Sommerswerd) stt
                then 
                  match mspec with
                  | Some Sword | Some ShortSword | Some BroadSword => 10%Z
                  | _ => 8%Z
                  end
        else match mspec with
              | Some w => if ((order.eqb w Spear && has_item (Weapon MagicSpear) stt) || has_item (Weapon w) stt)%bool
                    then 2%Z
                    else 0%Z
              | None => 0%Z
              end in
  let mb := if (hasd MindBlast && negb (hasmod MindblastImmune))%bool then 2%Z else 0%Z in
  let shield := if (has_item Shield stt && negb (has_flag LimbDeath stt))%bool then 2%Z else 0%Z in
  let silverhelm := if has_item (GenSpecial S10) stt then 2%Z else 0%Z in
  let skm1 := if has_flag PermanentSkillReduction stt then (-1)%Z else 0%Z in
  let skm2 := if has_flag PermanentSkillReduction2 stt then (-2)%Z else 0%Z in
  let limbd := if has_flag LimbDeath stt then (-3)%Z else 0%Z in
  let str2 := if has_flag StrengthPotionActive stt then 2%Z else 0%Z in
  let str4 := if has_flag PotentStrengthPotionActive stt then 4%Z else 0%Z in
  let raw_ratio: Z := (skdiff + combat_bonus + wpnbonus + mb + shield + silverhelm + skm1 + skm2 + limbd + str2 + str4)%Z in
    raw_ratio.

Definition moddmg (stt: Stt) (opp_hp: nat) (mds: list fight_modifier) (dmgs: nat * nat): (nat * nat) :=
  let (raw_op, raw_lw) := dmgs in
  let odmg_opponent := (raw_op + gdpr (Modifiers.get_cur_mods mds))%nat in
  let has_mod := fun m => existsb (FM.fm_eqb m) (Modifiers.get_cur_mods mds) in
  let dmg_lw := if has_mod PlayerInvulnerable then 0:nat else
        if (has_mod ForceEMindblast || (has_mod EnemyMindblast && negb (s_check MindShield (disciplines stt))))%bool
          then (raw_lw + 2:nat)%nat else raw_lw in
  let dmg_opp := if has_mod EnemyInvulnerable then 0:nat else
        if (has_mod DoubleDamage || (has_mod Undead && has_item (Weapon Sommerswerd) stt))%bool
            then (odmg_opponent * 2)%nat
            else odmg_opponent in
  (curendurance stt - dmg_lw, opp_hp - dmg_opp)%nat.

Definition fight_round (stt: Stt) (osk: Z) (opphp: nat) (mds: list fight_modifier) : Proba (nat * nat) :=
  let ratio := get_ratio stt osk mds in
  let hits := hits_from_ratio ratio in
  rebuild_proba (map (fun dmg => (moddmg stt opphp mds dmg, (Q2Qc (1%Q/10%Q)))) hits).

Definition fd_sk (d: fight_details): skill := match d with
  | Details s _ _ => s
  end.

Definition fd_opp_hp (d: fight_details): endurance := match d with
  | Details _ e _ => e
  end.

Definition fd_mods (d: fight_details): list fight_modifier := match d with
  | Details _ _ l => l
  end.

(* the outcome of a round, for each number that can be picked, from 1 to 9 and then 0 *)
Definition round_results (stt: Stt) (osk: Z) (opphp: nat) (mds: list fight_modifier) : list (nat * nat) :=
  map (moddmg stt opphp mds) (hits_from_ratio (get_ratio stt osk mds)).

Definition tenth: Qc := Q2Qc (1%Q/10%Q).

Definition lost_result (mds: list fight_modifier): TEscaped :=
  match Modifiers.extractl Modifiers.gOnlose (Modifiers.get_cur_mods mds) with
  | Some c => TLost c
  | None => Std 0
  end.

Definition gOnDamage (m: fight_modifier): option cid :=
  match m with
  | OnDamage x => Some x
  | _ => None
  end.

Definition gOnPickOne (m: fight_modifier): option cid :=
  match m with
  | OnPickOne x => Some x
  | _ => None
  end.

(* what is left to do after a round *)
Definition RoundEnd : Set := Proba TEscaped * Proba (nat * nat).

Definition add_round_end (a b: RoundEnd): RoundEnd :=
  (add_proba (fst a) (fst b), add_proba (snd a) (snd b)).

Definition over (p: Qc) (te: TEscaped): RoundEnd := ([(te, p)], []).
Definition next_round (p: Qc) (lw op: nat): RoundEnd := ([], [((lw, op), p)]).

Definition has_timer_branch (mds: list fight_modifier): bool :=
  existsb (fun m => match m with Dpr _ | Timed _ _ | OnDamage _ => true | _ => false end) mds.

Definition has_timer (mds: list fight_modifier): bool :=
  existsb (fun m => match m with Timed _ _ => true | _ => false end) mds.

(* a win is late only when using the shortcut in the solver, that is when nothing is timed or depending on damage *)
Definition win_result (late: bool) (mds: list fight_modifier) (lw: nat): TEscaped :=
  match Modifiers.extractl Modifiers.gOnNotYetWon (Modifiers.get_cur_mods mds) with
  | Some ny => if late then LateWin ny lw else Std lw
  | None => Std lw
  end.

Definition common_round (late: bool) (stt: Stt) (mds: list fight_modifier) (lw op: nat) (p: Qc) : RoundEnd :=
  let curmods := Modifiers.get_cur_mods mds in
  if lw =? 0 then over p (lost_result mds)
  else match Modifiers.extractl gOnDamage curmods with
  | Some dcid => if lw <? curendurance stt then over p (Stopped dcid lw)
                 else if op =? 0 then over p (win_result late mds lw)
                 else next_round p lw op
  | None => if op =? 0 then over p (win_result late mds lw) else next_round p lw op
  end.

Definition fold_rounds (f: (nat * nat) -> RoundEnd) (l: list (nat * nat)) : RoundEnd :=
  fold_right (fun x acc => add_round_end (f x) acc) ([], []) l.

(* the numerators of the distribution of the rounds of a poisonous opponent *)
Definition poisonous_round (stt: Stt) (opphp: nat) (instakill: Qc) (mds: list fight_modifier) (rs: list (nat * nat)) : RoundEnd :=
  let lw0 := curendurance stt in
  (* each of the cases where something changes, with its weight *)
  let cases := flat_map (fun r : nat * nat =>
      let (lw, op) := r in
      if lw <? lw0
        then (true, 0%nat, tenth * instakill)%Qc :: (false, op, tenth * (1 - instakill))%Qc :: nil
        else (false, op, tenth) :: nil) rs in
  let useful := filter (fun c: bool * nat * Qc =>
      let '(k, op, _) := c in k || negb (op =? opphp)) cases in
  let total := fold_right (fun (c: bool * nat * Qc) acc => let '(_, _, w) := c in (w + acc)%Qc) 0%Qc useful in
  fold_right (fun (c: bool * nat * Qc) acc =>
      let '(k, op, w) := c in
      let p := (w / total)%Qc in
      add_round_end
        (if k then over p (lost_result mds)
         else if op =? 0 then over p (Std lw0)
         else next_round p lw0 op) acc) ([], []) useful.

Definition fight_step (stt: Stt) (osk: Z) (opphp: nat) (mds: list fight_modifier) : RoundEnd :=
  let curmods := Modifiers.get_cur_mods mds in
  let lw0 := curendurance stt in
  let rs := round_results stt osk opphp mds in
  match Modifiers.extractl Modifiers.gStopFight curmods with
  | Some c => over 1%Qc (Stopped c lw0)
  | None =>
  match Modifiers.extractl gOnPickOne mds with
  | Some c =>
      match rs with
      | [] => ([], [])
      | _ :: others =>
          add_round_end (over tenth (Stopped c lw0))
            (fold_rounds (fun r: nat * nat =>
                let (lw, op) := r in
                if lw =? 0 then over tenth (lost_result mds)
                else if op =? 0 then over tenth (Std lw)
                else next_round tenth lw op) others)
      end
  | None =>
  match Modifiers.extractl Modifiers.gEvaded curmods with
  | Some c =>
      fold_rounds (fun r: nat * nat =>
          let (lw, _) := r in
          if lw =? 0 then over tenth (lost_result mds)
          else match Modifiers.extractl gOnDamage curmods with
               | Some d => if lw <? lw0 then over tenth (Stopped d lw) else over tenth (Escaped c lw)
               | None => over tenth (Escaped c lw)
               end) rs
  | None =>
  match Modifiers.extractl Modifiers.gPoisonous curmods with
  | Some instakill => poisonous_round stt opphp instakill mds rs
  | None =>
      fold_rounds (fun r: nat * nat =>
          let (lw, op) := r in
          common_round (negb (has_timer_branch mds)) stt mds lw op tenth) rs
  end end end end.

(* A single round, for the fights where the player can decide after each round *)
Definition one_round (stt: Stt) (osk: Z) (opphp: nat) (mds: list fight_modifier) : RoundEnd :=
  fold_rounds (fun r: nat * nat =>
      let (lw, op) := r in
      common_round (negb (has_timer mds)) stt mds lw op tenth) (round_results stt osk opphp mds).

(* All the fighters in the same round have the same modifiers, and each state of a round is only computed once *)
Fixpoint fight_rounds (fuel: nat) (stt: Stt) (osk: Z) (frontier: Proba (nat * nat)) (mds: list fight_modifier) (acc: Proba TEscaped) : Proba TEscaped :=
  match fuel, frontier with
  | O, _ | _, [] => acc
  | S f, _ =>
      let res := fold_right (fun (st: (nat * nat) * Qc) cur =>
          let '((lw, op), p) := st in
          let r := fight_step (update_endurance (fun _ => lw) stt) osk op mds in
          add_round_end (mul_proba (fst r) p, mul_proba (snd r) p) cur) ([], []) frontier in
      fight_rounds f stt osk (snd res) (Modifiers.advance_time mds) (add_proba acc (fst res))
  end.

Definition fight (stt: Stt) (osk: Z) (opphp: nat) (mds: list fight_modifier): Proba TEscaped :=
  fight_rounds 1000 stt osk [((curendurance stt, opphp), 1%Qc)] mds [].
