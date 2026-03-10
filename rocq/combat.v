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
  | (Escaped c1 a1, Escaped c2 a2) => mps.eqb c1 c2 && mps.eqb a1 a2
  | (LateWin c1 a1, LateWin c2 a2) => mps.eqb c1 c2 && mps.eqb a1 a2
  | (Stopped c1 a1, Stopped c2 a2) => mps.eqb c1 c2 && mps.eqb a1 a2
  | (TLost a1, TLost a2) => mps.eqb a1 a2
  | (Std a1, Std a2) => mps.eqb a1 a2
  | _ => false
  end.


  Lemma eqb_correct: forall (x y: TEscaped), (x = y) <-> te_eqb x y = true.
  Proof.
    split; intros.
    { subst.
      destruct y; unfold te_eqb; repeat (rewrite mps.eqb_refl; auto).
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


Instance te_EqDec: mps.EqDec TEscaped := {
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

  Fixpoint advance_time (mds: list fight_modifier): list fight_modifier :=
  match mds with
  | [] => []
  | Timed 0 x::xs => advance_time xs
  | Timed (S n) x::xs => Timed n x :: advance_time xs
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
               | Timed _ (Evaded _) => get_cur_mods xs
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
    Ltac irun :=
      match goal with
      | H: _ = Timed _ _ |- _ => fail 1
      | |- _ /\ _ => split
      | |- Forall _ (_ :: _) => apply Forall_cons
      | H: ?a |- ?a => assumption
      | |- True => trivial
      | |- ~ _ => intro
      | |- _ -> _ => intro
      | H1: ?a, H2: ~?a |- _ => contradiction
      | H : exists (_ : nat) (_ : fight_modifier), _ = Timed _ _ |- _ =>
            destruct H as [n [df PX]]; discriminate
      | |- get_cur_mods [?a] = ?a :: get_cur_mods [] => reflexivity
      | |- _ \/ _ => right
      end.

    destruct md eqn:MD; repeat irun; auto.
      destruct f eqn:MF; try (right; left; exists n, f; subst; split; try (intro contra; destruct contra; discriminate); tauto).
      subst.
      left. exists n, c. tauto.
  Qed.

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
    split; intros.
    {
      destruct m; auto. exfalso. apply H. exists n, m. reflexivity.
    }
    intro contra.
    destruct contra.
    destruct H0.
    subst.
    simpl in H.
    destruct x. apply f_equal with (f := @length _) in H. simpl in H. Lia.lia.
    inversion H; subst. Lia.lia.
  Qed.

  Fixpoint fm_depth (a: fight_modifier): nat := match a with
      | Timed _ s => S (fm_depth s)
      | _ => 0
      end.

  Lemma get_cur_mods_swap_queue: forall a mds1 mds2,
      get_cur_mods (a :: mds1) = a :: get_cur_mods mds1 ->
      get_cur_mods (a :: mds2) = a :: get_cur_mods mds2.
  Proof.
    induction a; intros; simpl; auto.
    destruct a; intros; simpl in *; auto; inversion H; subst.
    apply f_equal with (f := fm_depth) in H2.
    simpl in H2. Lia.lia.
    apply f_equal with (f := @length _) in H1.
    simpl in H1. Lia.lia.
  Qed.



  Lemma advance_time_correct: forall mds, ValidMods mds -> ValidMods (advance_time mds).
  induction mds; intros.
  simpl; auto.
  pose proof (get_cur_mods_cons mds a) as GCM.

  destruct H as [Pa [Pb [Pc [Pd Pe]]]].
  apply Forall_cons_iff in Pa.
  destruct Pa as [MA FR].

  Ltac mrun :=
    match goal with
    | H2: In ?t (get_cur_mods (?xa :: advance_time ?xmds))
      |- False => destruct ?xa
    | |- _ /\ _ => split
    | |- Forall _ (_ :: _) => apply Forall_cons
    | H: ?a |- ?a => assumption
    | |- True => trivial
    | |- ~ _ => intro
    | |- _ -> _ => intro
    | H1: ?a, H2: ~?a |- _ => contradiction
    | H: ?a = ?a \/ In _ (get_cur_mods (advance_time _)) |- _ => fail 1
    | H: ?a = ?b \/ In _ (get_cur_mods (advance_time _)) |- _ =>
          destruct H as [Heq | Hin];
        [ discriminate
        | idtac (* handle Hin *) ]
    end.


  destruct GCM as [GCA|[GCB|GCC]].
  {
      destruct GCA as [n [ev [PA P2]]]. 
      rewrite P2 in Pb, Pc, Pd, Pe. subst. clear P2.
      simpl. destruct n. apply IHmds. split; auto.
      assert (ValidMods mds). {
        unfold ValidMods.
        split; auto.
      }
      specialize (IHmds H).
      unfold ValidMods in IHmds.
      unfold ValidMods. simpl. split; try tauto.
      apply Forall_cons; auto.  tauto.
  }
  {
      destruct GCB as [n [sd [PA [P1 P2]]]]. 
      rewrite P2 in Pb, Pc, Pd, Pe. subst. clear P2.
      simpl in Pb, Pc.
      apply Decidable.not_or in Pb, Pc.
      destruct Pb as [Pba Pbb].
      destruct Pc as [Pca Pcb].
      assert (ValidMods mds). {
        unfold ValidMods.
        split; tauto.
      }
      specialize (IHmds H).
      destruct IHmds as [I1 [I2 [I3 [I4 I5]]]].
      destruct n; simpl.
      { split; simpl; tauto. }
      unfold ValidMods. simpl. destruct sd; try tauto;auto; simpl;
      repeat mrun.
  }
  {
      destruct GCC as [PA PB].
      rewrite PB in Pb, Pc, Pd, Pe.
      apply Decidable.not_or in Pb, Pc.
      destruct Pb as [Pba Pbb].
      destruct Pc as [Pca Pcb].
      assert (ValidMods mds). {
        unfold ValidMods.
        split; simpl; tauto.
      }
      specialize (IHmds H).
      destruct IHmds as [I1 [I2 [I3 [I4 I5]]]].
      pose proof (advance_time_not_timed a mds).
      apply H0 in PA.
      rewrite PA.

      unfold ValidMods.
      split. apply Forall_cons. tauto. apply I1.

      eapply get_cur_mods_swap_queue in PB.
      rewrite PB.
      simpl. tauto.
  }
Qed.

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
  let hasmod m := existsb (FM.fm_eqb m) mds in
  let skdiff := (sk stt - osk)%Z in
  let combat_bonus := fold_right addcombatbonus 0%Z mds in
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
              | Some w => if ((mps.eqb w Spear && has_item (Weapon MagicSpear) stt) || has_item (Weapon w) stt)%bool
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
  let str4 := if has_flag PotentStrengthPotionActive stt then 2%Z else 0%Z in
  let raw_ratio: Z := (skdiff + combat_bonus + wpnbonus + mb + shield + silverhelm + skm1 + skm2 + limbd + str2 + str4)%Z in
    raw_ratio.

Definition moddmg (stt: Stt) (opp_hp: nat) (mds: list fight_modifier) (dmgs: nat * nat): (nat * nat) :=
  let (raw_op, raw_lw) := dmgs in
  let odmg_opponent := (raw_op + gdpr mds)%nat in
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

Definition fight_shortcut (stt: Stt) (d: fight_details): option (Proba TEscaped) :=
    let (brr, mds) := match d with Details sk en m => ((sk, en), m) end in
    let (sk, en) := brr in
    let extract {A}:  (fight_modifier -> option A) -> option A := fun x => Modifiers.extractl x mds in
    let lose := match extract Modifiers.gOnlose with
                | None => Std 0
                | Some cid => TLost cid
                end in
    match extract Modifiers.gStopFight with
    | Some cid => Some [(Stopped cid (curendurance stt), 1%Qc)]
    | None =>
      match extract Modifiers.gEvaded with
      | Some cid => Some (rebuild_proba (map (fun (rp: ((nat * nat) * Qc)) =>
            let (res, p) := rp in
            let (lw, opp) := res in
            if Nat.eqb lw 0
              then (lose, p)
              else (Escaped cid lw, p)
          ) (fight_round stt sk en mds)))
      | None => None
      end
    end.

Fixpoint all_timings (mds: list fight_modifier): nat :=
  match mds with
  | [] => 0
  | (Timed n _::xs) => n + all_timings xs
  | _::xs => all_timings xs
  end.

Definition has_damage (p: ((nat * nat) * Qc)) (stt: Stt) (opphp: nat):=
    let (lw, op) := fst p in
    let lwp := curendurance stt in
        (lw <= lwp /\ op <= opphp /\ lw + op < lwp + opphp)%nat.

Definition ValidFight (d: fight_details): Prop :=
  let mds := fd_mods d in
  (* invulnerabilities are timed, no timed timed *)
  (fd_opp_hp d > 0 /\ Modifiers.ValidMods mds)%nat
    .

Lemma valid_advanced: forall fd (opp : nat),
    (opp > 0)%nat -> ValidFight fd -> ValidFight (Details (fd_sk fd) opp (Modifiers.advance_time (fd_mods fd))).
Proof.
  intros.
  unfold ValidFight in *.
  destruct fd. simpl in *.
  split; auto.
  apply Modifiers.advance_time_correct.
  tauto.
Qed.

Lemma valid_advanced_g: forall sk prevo opp mds,
    (opp > 0)%nat -> ValidFight (Details sk prevo mds) -> ValidFight (Details sk opp (Modifiers.advance_time mds)).
Proof.
  intros.
  unfold ValidFight in *. simpl in *.
  split; auto.
  apply Modifiers.advance_time_correct.
  tauto.
Qed.

Lemma fight_round_correct: forall (stt: Stt) (osk: Z) (opphp: nat) (mds: list fight_modifier) lst,
  (curendurance stt > 0)%nat ->
  ValidFight (Details osk opphp mds) -> lst = fight_round stt osk opphp mds -> Forall (fun p => has_damage p stt opphp) lst.
Proof.
  (* intros *)
  intros stt osk pphp mds lst CE0 VF H.
  unfold has_damage.
  apply Forall_forall.
  intros x HIn.
  destruct x as [[nlw nop] p].
  simpl.
  unfold fight_round in H.
  subst.
  (* simpligication de HIn *)
  apply (in_map fst) in HIn.
  simpl in HIn.
  apply rebuild_proba_keeps_keys_conv in HIn.
  rewrite map_map in HIn. simpl in HIn.
  remember (hits_from_ratio (get_ratio stt osk mds)) as hr.
  apply hits_from_ratio_content in Heqhr.

  pose proof (hits_damage).
  eapply Forall_forall in H; eauto.

  apply in_map_iff in HIn.
  destruct HIn.
  destruct x. destruct H0.
  pose proof (Merge.use_forall _ _ _ H H1). simpl in H2.
  clear H H1.

  (* case analysis *)
  unfold moddmg in H0.
  destruct VF as [OPp [FR [NP [NE [IP IE]]]]].
  remember (Modifiers.get_cur_mods mds) as curmods.
  simpl in Heqcurmods, FR. 
  Ltac gogo := match goal with
  | H: existsb (FM.fm_eqb _) _ = true |- _ => apply existsb_exists in H
  | H: exists _ : fight_modifier, In _ _ /\ FM.fm_eqb _ _ = _ |- _ =>
      destruct H
  | H: _ /\ _ |- _ => destruct H
  | H: FM.fm_eqb _ _ = true |- _ => apply FM.eqb_correct in H;subst
  | H1: ?x, H2: ~ ?x |- _ => contradiction
  | H: (_, _) = (_, _) |- _ => inversion H; subst; clear H
  | |- _ /\ _ => split
  end.
  simpl in OPp.
  destruct (existsb (FM.fm_eqb PlayerInvulnerable) curmods) eqn: HPI,
           (existsb (FM.fm_eqb EnemyInvulnerable) curmods) eqn: EPI,
           (existsb (FM.fm_eqb Undead) curmods) eqn: UD,
           (existsb (FM.fm_eqb ForceEMindblast) curmods) eqn: FEMB,
           (existsb (FM.fm_eqb DoubleDamage) curmods) eqn: DD,
           (existsb (FM.fm_eqb EnemyMindblast) curmods) eqn: EMB,
           (has_item (Weapon Sommerswerd) stt) eqn: SW;
           repeat gogo; simpl;
           try Lia.lia;
  destruct (s_check MindShield (disciplines stt)) eqn:MS;
           repeat gogo; simpl;
           try Lia.lia
          .
Qed.

Lemma fight_round_full: forall stt osk opphp mds, FullProba (fight_round stt osk opphp mds).
Proof.
  intros.
  unfold FullProba.
  unfold fight_round.
  rewrite rebuild_proba_keeps_sumproba.
  remember (hits_from_ratio (get_ratio stt osk mds)).

  pose proof (hits_from_ratio_content _ _ Heql). clear Heql.
  unfold SumProba.
  unfold foldMap.
  rewrite map_map.
  replace (fun x : nat * nat => snd (moddmg stt opphp mds x, (1 / 10)%Q)) with
          (fun x : nat * nat => (1 / 10)%Q).
  2: {
    apply functional_extensionality. 
    intros.
    reflexivity.
  }

  unfold HITSCHART in H.
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  destruct H; subst; try (apply Qceq_alt; compute; reflexivity).
  inversion H.
Qed.

Lemma some_notequal (T: Type): forall (a b :T), Some a <> Some b <-> a <> b.
Proof.
  split; intros.
  intro contra. subst. contradiction.
  intro contra. inversion contra; subst. contradiction.
Qed.

Lemma pair_notequal (A B: Set) `{eqa: mps.EqDec A} `{eqb: mps.EqDec B}: forall (a1 a2: A) (b1 b2: B),
    (a1, b1) <> (a2, b2) <-> a1 <> a2 \/ b1 <> b2.
Proof.
  split; intros.
  {
    destruct (mps.eqb a1 a2) eqn: EA, (mps.eqb b1 b2) eqn: EB.
    apply mps.eqb_correct in EA, EB. subst. contradiction.
    apply mps.eqb_not in EB. tauto.
    apply mps.eqb_not in EA. tauto.
    apply mps.eqb_not in EB. tauto.
  }
  {
    intro contra. inversion contra; subst. tauto.
  }
Qed.

Lemma all_timings_decrease: forall mds, Modifiers.ValidMods mds ->
  (all_timings (Modifiers.advance_time mds) <= all_timings mds)%nat.
Proof.
  intros.
  destruct H as [FR [NP [NE [IP IE]]]]. clear NP NE IP IE.
  induction mds. simpl. Lia.lia.
  simpl.
  inversion FR; subst. clear FR.
  specialize (IHmds H2). clear H2.
  destruct a; simpl; auto.
  destruct n; simpl; auto.
  Lia.lia.
Qed.

Lemma fight_shortcut_correct: forall stt fd res,
    fight_shortcut stt fd = Some res -> FullProba res.
Proof.
  intros.
  destruct stt.
  destruct fd.
  unfold fight_shortcut in H.
  simpl in H.
  destruct (Modifiers.extractl Modifiers.gStopFight l) eqn: GS.
  * inversion H; subst. compute. reflexivity.
  * destruct (Modifiers.extractl Modifiers.gEvaded l) eqn: GE.
  - inversion H; subst; clear H.
    unfold FullProba.
    rewrite rebuild_proba_keeps_sumproba.
    rewrite SumProba_map_keys.
    apply fight_round_full.
    intros.
    destruct x.
    destruct p.
    simpl.
    destruct  (n =? 0); auto.
  - inversion H.
Qed. 

Lemma fight_shortcut_valid: forall stt fd res,
  fight_shortcut stt fd = Some res -> ValidMap res.
Proof.
  intros.
  destruct stt.
  destruct fd.
  unfold fight_shortcut in H.
  simpl in H.
  destruct (Modifiers.extractl Modifiers.gStopFight l) eqn: GS.
  * inversion H; subst. constructor.
  * destruct (Modifiers.extractl Modifiers.gEvaded l) eqn: GE.
    - inversion H; subst; clear H.
      apply rebuild_proba_valid.
    - discriminate.
Qed. 

Module IF.
  Definition ustt (stt: Stt) (opphp: nat) (mds: list fight_modifier) (dmgs: nat * nat): (Stt * nat) :=
    let (lwd, opd) := moddmg stt opphp mds dmgs in
    (update_endurance (fun _ => lwd) stt, opd).
End IF.

Inductive ifight : Stt -> skill -> nat -> list fight_modifier -> Proba TEscaped -> Prop :=
  | lost_onlose:
      forall curstt sk opphp mds cid,
        curendurance curstt = 0%nat ->
        Modifiers.extractl Modifiers.gOnlose mds = Some cid ->
        ifight curstt sk opphp mds [(TLost cid, 1%Qc)]
  | lost_std:
      forall curstt sk opphp mds,
        curendurance curstt = 0%nat ->
        Modifiers.extractl Modifiers.gOnlose mds = None ->
        ifight curstt sk opphp mds [(Std 0%nat, 1%Qc)]
  | win:
      forall curstt sk mds,
        curendurance curstt <> 0%nat ->
        ifight curstt sk 0 mds [(Std (curendurance curstt), 1%Qc)]
  | shortcut:
      forall curstt sk opphp mds subout,
        curendurance curstt <> 0%nat ->
        opphp <> 0%nat ->
        fight_shortcut curstt (Details sk opphp mds) = Some subout ->
        ifight curstt sk opphp mds subout
  | applydmg:
      forall curstt sk opphp mds ratio nmds result
        h0 h1 h2 h3 h4 h5 h6 h7 h8 h9
        r0 r1 r2 r3 r4 r5 r6 r7 r8 r9
        s0 s1 s2 s3 s4 s5 s6 s7 s8 s9
        o0 o1 o2 o3 o4 o5 o6 o7 o8 o9,
        fight_shortcut curstt (Details sk opphp mds) = None ->
        curendurance curstt <> 0%nat ->
        opphp <> 0%nat ->
        ratio = get_ratio curstt sk mds ->
        i_hits ratio h0 h1 h2 h3 h4 h5 h6 h7 h8 h9 ->
        nmds = Modifiers.advance_time mds ->
        (s0, o0) = IF.ustt curstt opphp mds h0 ->
        (s1, o1) = IF.ustt curstt opphp mds h1 ->
        (s2, o2) = IF.ustt curstt opphp mds h2 ->
        (s3, o3) = IF.ustt curstt opphp mds h3 ->
        (s4, o4) = IF.ustt curstt opphp mds h4 ->
        (s5, o5) = IF.ustt curstt opphp mds h5 ->
        (s6, o6) = IF.ustt curstt opphp mds h6 ->
        (s7, o7) = IF.ustt curstt opphp mds h7 ->
        (s8, o8) = IF.ustt curstt opphp mds h8 ->
        (s9, o9) = IF.ustt curstt opphp mds h9 ->
        ifight s0 sk o0 nmds r0 ->
        ifight s1 sk o1 nmds r1 ->
        ifight s2 sk o2 nmds r2 ->
        ifight s3 sk o3 nmds r3 ->
        ifight s4 sk o4 nmds r4 ->
        ifight s5 sk o5 nmds r5 ->
        ifight s6 sk o6 nmds r6 ->
        ifight s7 sk o7 nmds r7 ->
        ifight s8 sk o8 nmds r8 ->
        ifight s9 sk o9 nmds r9 ->
        result = merge_probas 
            [ (Q2Qc (1%Q/10%Q), r0)
            ; (Q2Qc (1%Q/10%Q), r1)
            ; (Q2Qc (1%Q/10%Q), r2)
            ; (Q2Qc (1%Q/10%Q), r3)
            ; (Q2Qc (1%Q/10%Q), r4)
            ; (Q2Qc (1%Q/10%Q), r5)
            ; (Q2Qc (1%Q/10%Q), r6)
            ; (Q2Qc (1%Q/10%Q), r7)
            ; (Q2Qc (1%Q/10%Q), r8)
            ; (Q2Qc (1%Q/10%Q), r9)
            ] ->
        ifight curstt sk opphp mds result
  .

Lemma ifight_correct: forall stt sk opphp mds res,
    ifight stt sk opphp mds res ->
    FullProba res.
Proof.
  intros.
  unfold FullProba.
  induction H; auto.
  eapply fight_shortcut_correct; eauto.
  subst.
  rewrite merge_probas_sum.
  simpl.
  rewrite IHifight1.
  rewrite IHifight2.
  rewrite IHifight3.
  rewrite IHifight4.
  rewrite IHifight5.
  rewrite IHifight6.
  rewrite IHifight7.
  rewrite IHifight8.
  rewrite IHifight9.
  rewrite IHifight10.

  vm_compute.
  apply Qceq_alt.
  vm_compute.
  reflexivity.
Qed.

Lemma ifight_result_unique: forall stt sk opphp mds res1 res2,
      ifight stt sk opphp mds res1 ->
      ifight stt sk opphp mds res2 ->
      res1 = res2
      .
Proof.
  intros.
  generalize dependent res2.
  induction H; intros.
  * inversion H1; subst; clear H1; try contradiction.
    + rewrite H0 in H3. inversion H3; auto.
    + rewrite H0 in H3. discriminate.
  * inversion H1; subst; clear H1; try contradiction.
    + rewrite H0 in H3. discriminate.
    + rewrite H0 in H3. inversion H3; auto.
  * inversion H0; subst; clear H0; try contradiction. reflexivity.
  * inversion H2; subst; clear H2; try contradiction.
    + rewrite H1 in H5. inversion H5; subst; reflexivity.
    + rewrite H1 in H3. discriminate.
  * inversion H26; subst; clear H26; try contradiction.
    + rewrite H in H29. discriminate.
    + pose proof (i_hits_eq _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ H3 H31) as EQS.
      destruct EQS as [X0 [X1 [X2 [X3 [X4 [X5 [X6 [X7 [X8 X9]]]]]]]]].
      subst.
      clear H3 H31.

      Ltac ifight_result_unique_hlp := match goal with
        | H1: (?s1, ?o1) = IF.ustt ?curstt ?opphp ?mds _
        , H2: (?s2, ?o2) = IF.ustt ?curstt ?opphp ?mds _
          |- _ => rewrite <- H1 in H2
        | H: (_, _) = (_, _) |- _ => inversion H; subst; clear H
        | |- merge_probas (_::_) = merge_probas (_::_) => f_equal
        | |- (_::_) = (_::_) => f_equal
        | |- (Q2Qc _, _) = (Q2Qc _, _) => f_equal
        | Hi: forall _ : Proba TEscaped, ifight _ _ _ (Modifiers.advance_time _) _ -> ?r0 = _
          |- ?r0 = _ => apply Hi
      end.

      repeat ifight_result_unique_hlp; assumption.
Qed.

Lemma ifight_validmap: forall stt sk opphp mds res,
  ifight stt sk opphp mds res -> ValidMap res.
Proof.
  intros.
  induction H; subst; clear H.
  * constructor.
  * constructor.
  * constructor.
  * eapply fight_shortcut_valid; eauto.
  * apply merge_probas_correct.
    simpl.
    constructor; try assumption.
    constructor; try assumption.
    constructor; try assumption.
    constructor; try assumption.
    constructor; try assumption.
    constructor; try assumption.
    constructor; try assumption.
    constructor; try assumption.
    constructor; try assumption.
    constructor; try assumption.
    constructor.
Qed.

Lemma fight_shortcut_endurance_unchanged:
    forall lw1 lw2 stt d,
    fight_shortcut (update_endurance (fun _ : endurance => lw1) stt) d = None ->
    fight_shortcut (update_endurance (fun _ : endurance => lw2) stt) d = None.
Proof.
  intros.
  destruct stt, d. unfold fight_shortcut in *. simpl in *.
  destruct (Modifiers.extractl Modifiers.gStopFight l); try discriminate.
  destruct (Modifiers.extractl Modifiers.gEvaded l); try discriminate.
  reflexivity.
Qed.

Definition counter (stt: Stt) (opphp: nat) (mds: list fight_modifier): nat :=
    curendurance stt + opphp.

Program Fixpoint f_fight (stt: Stt) (sk: skill) (opphp: nat) (mds: list fight_modifier) (vld: Modifiers.ValidMods mds) {measure (counter stt opphp mds)}: Proba TEscaped :=
  match Nat.eq_dec (curendurance stt) 0 with
  | left _ =>
         match Modifiers.extractl Modifiers.gOnlose mds with
        | Some cid => [(TLost cid, 1%Qc)]
        | None => [(Std 0%nat, 1%Qc)]
        end
  | right hce =>
    match Nat.eq_dec opphp 0 with
    | left _ => [(Std (curendurance stt), 1%Qc)]
    | right hne =>
      match fight_shortcut stt (Details sk opphp mds) with
          | Some subout => subout
          | None => 
        let hitres := fight_round stt sk opphp mds in
        let fix go (lst : Proba (nat * nat)) (Hc: (curendurance stt <> 0)%nat) (Ho : (opphp <> 0)%nat) (Hsub : forall x, In x lst -> In x hitres) :=
          match lst return (forall x, In x lst -> In x hitres) -> _ with
          | [] => fun _ => []
          | ((nlw, nopp), p) :: rest => fun Hsub' =>
              (p, f_fight (update_endurance (fun _ => nlw) stt) sk nopp (Modifiers.advance_time mds) _)
              :: go rest Hc Ho (fun x Hx => Hsub' x (in_cons _ x _ Hx))
          end Hsub
        in
        merge_probas (go hitres hce hne (fun x Hx => Hx))
    end
  end
end.
Next Obligation.
apply Modifiers.advance_time_correct. assumption.
Defined.
Next Obligation.
  unfold counter.
  simpl.
  remember (fight_round stt sk opphp mds) as fr.
  assert (ValidFight (Details sk opphp mds)) as VFF. {
    unfold ValidFight. split; simpl; auto.
    Lia.lia.
  }
  assert (curendurance stt > 0)%nat as Hc2 by Lia.lia.
  pose proof (fight_round_correct _ _ _ _ _ Hc2 VFF Heqfr).
  subst.
  specialize (Hsub' ((nlw, nopp), p)).
  lapply Hsub'.
  2: {
    left. reflexivity.
  }
  intro HsubX. clear Hsub'.
  destruct (Forall_forall (fun p : nat * nat * Qc => has_damage p stt opphp) (fight_round stt sk opphp mds)) as [A B].
  specialize (A H ((nlw, nopp), p) HsubX).
  unfold has_damage in A. simpl in A. tauto.
Defined.

Lemma fight_inductive: forall stt sk opphp mds vld res,
    f_fight stt sk opphp mds vld = res <-> ifight stt sk opphp mds res.
Proof.
Admitted.



Definition fight (stt: Stt) (sk: skill) (opphp: nat) (mds: list fight_modifier): Proba TEscaped :=
  match Modifiers.ValidMods_dec mds with
  | left pr => f_fight stt sk opphp mds pr
  | right _ => []
  end.

Lemma fight_full: forall stt sk opphp mds, Modifiers.ValidMods mds -> FullProba (fight stt sk opphp mds).
Proof.
  intros.
  unfold FullProba.
  unfold fight.
  destruct (Modifiers.ValidMods_dec mds); try contradiction.
  remember (f_fight stt sk opphp mds v).
  symmetry in Heqp.
  apply fight_inductive in Heqp.
  eapply ifight_correct; eauto.
Qed.

Definition valid_fight_result (stt: Stt) (te: TEscaped) :=
  match te with
  | Escaped _ nhp | LateWin _ nhp | Stopped _ nhp | Std nhp => (nhp <= max_hp stt)%nat
  | TLost _ => True
  end.

Module FRH.

  Lemma ustt_valid: forall stt opphp mds dmgs nstt no,
    ValidState stt -> IF.ustt stt opphp mds dmgs = (nstt, no) ->
    ValidState nstt \/ curendurance nstt = 0%nat.
  Proof.
    unfold IF.ustt, moddmg.
    intros.
    destruct dmgs.
    unfold ValidState in H.

    destruct (existsb (FM.fm_eqb PlayerInvulnerable) (Modifiers.get_cur_mods mds)) eqn:PI;
    destruct (existsb (FM.fm_eqb EnemyInvulnerable) (Modifiers.get_cur_mods mds)) eqn:EI;
    destruct (existsb (FM.fm_eqb DoubleDamage) (Modifiers.get_cur_mods mds) || existsb (FM.fm_eqb Undead) (Modifiers.get_cur_mods mds) && has_item (Weapon Sommerswerd) stt) eqn:SS;
    destruct (existsb (FM.fm_eqb ForceEMindblast) (Modifiers.get_cur_mods mds) || existsb (FM.fm_eqb EnemyMindblast) (Modifiers.get_cur_mods mds) && negb (s_check MindShield (disciplines stt))) eqn: MB;
    destruct (curendurance nstt) eqn: NE; try tauto;left;
      inversion H0; subst; clear H0; apply VS.update_endurance; auto; intros; simpl in *; try Lia.lia.
  Qed.

  Lemma fight_round_vr: forall stt sk opphp mds res,
      fight_round stt sk opphp mds = res ->
      Forall (fun lo: nat * nat => let (l, o) := lo in l <= curendurance stt /\ o <= opphp)%nat (map fst res).
  Proof.
    intros.
    unfold fight_round in H.
    apply Forall_forall.
    intros.
    subst.
    apply rebuild_proba_keeps_keys_conv in H0.
    rewrite map_map in H0.
    simpl in H0.
    unfold moddmg in H0.
    destruct (existsb (FM.fm_eqb PlayerInvulnerable) (Modifiers.get_cur_mods mds)) eqn: PI.
    destruct (existsb (FM.fm_eqb EnemyInvulnerable) (Modifiers.get_cur_mods mds)) eqn: EI.
    * apply in_map_iff in H0.
      destruct H0, x0. destruct x, H. inversion H; subst. Lia.lia.
    * apply in_map_iff in H0.
      destruct H0, x0. destruct x, H. inversion H; subst. Lia.lia.
    * apply in_map_iff in H0.
      destruct H0, x0. destruct x, H. inversion H; subst. Lia.lia.
  Qed.

  Lemma fight_shortcut_res_hp: forall stt sk opphp mds subout,
    ValidState stt ->
    Modifiers.ValidMods mds ->
    fight_shortcut stt (Details sk opphp mds) = Some subout ->
    Forall (valid_fight_result stt) (map fst subout).
  Proof.
    intros stt sk opphp mds subout VSS VMDS SHC.
    unfold fight_shortcut in SHC.
    unfold ValidState in VSS.
    simpl in VSS.
    simpl in VMDS.
    destruct (Modifiers.extractl Modifiers.gStopFight mds) eqn: SF.
    { inversion SHC; subst; clear SHC.
      repeat constructor.
      simpl. tauto.
    }
    destruct (Modifiers.extractl Modifiers.gEvaded mds) eqn:EV; try discriminate.
    inversion SHC; subst; clear SHC.
    remember (fight_round stt sk opphp mds) as result.
    pose proof (fight_round_vr stt sk opphp mds result (symmetry Heqresult)) as FRC.
      
    apply Forall_forall.
    intros.
    pose proof (rebuild_proba_keeps_keys_conv _ _ H).
    rewrite map_map in H0.
    clear H.
    apply in_map_iff in H0.
    destruct H0.
    destruct x0 as [[lwr opr] pr].
    unfold valid_fight_result.
    destruct (lwr =? 0) eqn:LWD.
    * destruct (Modifiers.extractl Modifiers.gOnlose mds) eqn:onlose; destruct H; simpl in *; subst; auto. Lia.lia.
    * destruct H; simpl in *; subst.
      apply Forall_map in FRC.
      epose proof (Merge.use_forall _ (fight_round stt sk opphp mds) ((lwr, opr), pr) FRC H0).
      simpl in H. Lia.lia.
  Qed.

  Lemma valid_fight_result_change_stt: forall h curstt s x o opphp mds,
      ValidState curstt ->
      HasDamage h ->
      (s, o) = IF.ustt curstt opphp mds h ->
      valid_fight_result s x ->
      valid_fight_result curstt x.
  Proof.
    intros.
  Admitted.

End FRH.


Lemma fight_res_hp: forall stt sk opphp mds res, ValidState stt \/ curendurance stt = 0%nat ->
  Modifiers.ValidMods mds -> fight stt sk opphp mds = res ->
  Forall (valid_fight_result stt) (map fst res).
Proof.
  intros.
  unfold fight in H1.
  destruct (Modifiers.ValidMods_dec mds); try contradiction.
  apply fight_inductive in H1.
  clear v.
  induction H1 ; simpl; repeat constructor.
  * unfold valid_fight_result. unfold ValidState in H. Lia.lia.
  * unfold valid_fight_result. unfold ValidState in H. Lia.lia.
  * destruct H; try contradiction. eapply FRH.fight_shortcut_res_hp; eauto.
  * destruct H; try contradiction. apply Forall_forall.
    intros.
    subst.
    apply merge_probas_keeps_keys_conv in H18.
    simpl in H18.
    repeat (rewrite map_app in H18).
    apply ihits_damages in H5.

    Ltac fight_res_hp_finish := match goal with
    | H: _ /\ _ |- _ => destruct H
    | H: In _ (_ ++ _) |- _ => apply in_app_or in H
    | H: In _ _ \/ _ |- _ => destruct H
    | H: (?s, ?o) = IF.ustt ?cur ?opphp ?mds ?h,
      Hi: In ?x (map fst ?r),
      Hf: ifight ?s _ ?o _ ?r,
      Vc: ValidState ?cur
      |- valid_fight_result ?cur ?x => symmetry in H; destruct (FRH.ustt_valid cur opphp mds h s o Vc H)
    | VS: ValidState ?cur,
      H: IF.ustt ?cur ?opphp ?mds ?h = (?s, ?o),
      Hi: In ?x (map fst ?r),
      Hd: HasDamage ?h,
      Hf: ifight ?s _ ?o _ ?r
      |- valid_fight_result ?cur ?x => 
        apply (FRH.valid_fight_result_change_stt h cur s x o opphp mds VS Hd (symmetry H)); eapply Forall_forall
    | Ih: ValidState ?s \/ curendurance ?s = 0%nat -> Modifiers.ValidMods (Modifiers.advance_time ?mds) -> Forall (valid_fight_result ?s) (map fst ?r)
      |- Forall (valid_fight_result ?s) _ => apply Ih; auto
    | |- Modifiers.ValidMods (Modifiers.advance_time ?mds) => apply Modifiers.advance_time_correct
    | H: ?x |- ?x => assumption
    end.

    repeat fight_res_hp_finish.
    simpl in H18. contradiction.
Qed.