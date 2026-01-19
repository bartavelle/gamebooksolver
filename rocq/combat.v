Require Import Stdlib.Lists.List.
Import ListNotations.

Require Import hits.
Require Import proba.
Require Import mps.
Require Import character.
Require Import chapters.
From Equations Require Import Equations.
Require Import ZArith.
Require Import Stdlib.QArith.QArith.
Require Import Stdlib.QArith.Qcanon.
Require Import Field.

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
  | Lost: cid -> TEscaped
  | Stopped: cid -> nat -> TEscaped
  .

Module TE.
  Definition te_eqb (a b: TEscaped): bool :=
  match (a, b) with
  | (Escaped c1 a1, Escaped c2 a2) => mps.eqb c1 c2 && mps.eqb a1 a2
  | (LateWin c1 a1, LateWin c2 a2) => mps.eqb c1 c2 && mps.eqb a1 a2
  | (Stopped c1 a1, Stopped c2 a2) => mps.eqb c1 c2 && mps.eqb a1 a2
  | (Lost a1, Lost a2) => mps.eqb a1 a2
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
  | (Lost a1, Lost a2) => cmp a1 a2
  | (Lost _, _) => GT
  | (_, Lost _) => LT
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
    | H: Lost _ _ = Lost _ _ |- _ => inversion H; subst
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

Definition get_ratio (stt: Stt) (d: fight_details): Z :=
  let (osk, mds) := match d with Details sk _ mds => (sk, mds) end in
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

Definition moddmg (stt: Stt) (d: fight_details) (dmgs: nat * nat): (nat * nat) :=
  let (raw_op, raw_lw) := dmgs in
  let brr := match d with | Details skl opp_hp m => (skl, opp_hp, m) end in 
  let (brrr2, mds) := brr in
  let (skl, opp_hp) := brrr2 in
  let odmg_opponent := (raw_op + gdpr mds)%nat in
  let has_mod := fun m => existsb (FM.fm_eqb m) (Modifiers.get_cur_mods mds) in
  let dmg_lw := if has_mod PlayerInvulnerable then 0:nat else
        if (has_mod ForceEMindblast || (has_mod EnemyMindblast && negb (s_check MindShield (disciplines stt))))%bool then (raw_lw + 2:nat)%nat else raw_lw in
  let dmg_opp := if has_mod EnemyInvulnerable then 0:nat else
        if (has_mod DoubleDamage || (has_mod Undead && has_item (Weapon Sommerswerd) stt))%bool
            then (odmg_opponent * 2)%nat
            else odmg_opponent in
  (curendurance stt - dmg_lw, opp_hp - dmg_opp)%nat.

Definition fight_round (stt: Stt) (d: fight_details) : Proba (nat * nat) :=
  let ratio := get_ratio stt d in
  let hits := hits_from_ratio ratio in
  rebuild_proba (map (fun dmg => (moddmg stt d dmg, (Q2Qc (1%Q/10%Q)))) hits).

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
                | Some cid => Lost cid
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
          ) (fight_round stt (Details sk en mds))))
      | None => None
      end
    end.

Fixpoint all_timings (mds: list fight_modifier): nat :=
  match mds with
  | [] => 0
  | (Timed n _::xs) => n + all_timings xs
  | _::xs => all_timings xs
  end.

Definition counter (mode: option ((nat * nat) * Qc)) (stt: Stt) (d: fight_details): nat :=
      let nm := match mode with | Some _ => 1%nat | None => 0%nat end in
      (nm + curendurance stt + fd_opp_hp d + all_timings (fd_mods d))%nat.

Definition has_damage (p: ((nat * nat) * Qc)) (stt: Stt) (d: fight_details):=
    let (lw, op) := fst p in
    let lwp := curendurance stt in
    let opp := fd_opp_hp d in
        (lw <= lwp /\ op <= opp /\ lw + op < lwp + opp)%nat.

Definition FM_correct (mode: option ((nat * nat) * Qc)) (stt: Stt) (d: fight_details):=
    match mode with
    | Some pr => has_damage pr stt d
    | None => True
    end.

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

Lemma fight_round_correct: forall (stt: Stt) (d: fight_details) lst,
  (curendurance stt > 0)%nat ->
  ValidFight d -> lst = fight_round stt d -> Forall (fun p => FM_correct (Some p) stt d) lst.
Proof.
  (* intros *)
  intros stt d lst CE0 VF H.
  unfold FM_correct.
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
  remember (hits_from_ratio (get_ratio stt d)) as hr.
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
  remember (Modifiers.get_cur_mods (fd_mods d)) as curmods.
  destruct d.
  simpl in Heqcurmods, FR. rewrite <- Heqcurmods in H0.
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

Lemma fight_round_full: forall stt d, FullProba (fight_round stt d).
Proof.
  intros.
  unfold FullProba.
  unfold fight_round.
  rewrite rebuild_proba_keeps_sumproba.
  remember (hits_from_ratio (get_ratio stt d)).

  pose proof (hits_from_ratio_content _ _ Heql). clear Heql.
  unfold SumProba.
  unfold foldMap.
  rewrite map_map.
  replace (fun x : nat * nat => snd (moddmg stt d x, (1 / 10)%Q)) with
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

Program Fixpoint apply_fight
      (stt: Stt)
      (d: fight_details)
      (nxt: forall (md: option((nat * nat) * Qc)) (cs: Stt) (fd: fight_details) (correct: FM_correct md cs fd), Proba TEscaped)
      (frr : Proba (nat * nat)) 
      (COR: Forall (fun p => FM_correct (Some p) stt d) frr)
      : list (Proba TEscaped)
      :=
      match frr with
      | [] => []
      | x::xs =>
          let PP := _ : In x frr -> FM_correct (Some x) stt d in
            nxt (Some x) stt d (PP _) :: apply_fight stt d nxt xs _
      end.
Next Obligation.
  apply Forall_cons_iff in COR. tauto.
Defined.
Next Obligation.
  apply Forall_cons_iff in COR.
  constructor.
  reflexivity.
Defined.
Next Obligation.
  apply Forall_cons_iff in COR.
  tauto.
Defined.

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

Inductive ifight: Proba (nat * nat) -> Stt -> fight_details -> Proba TEscaped -> Prop :=
  | lost_onlose:
      forall opp p stt fd cid xs out res,
          ifight xs stt fd out ->
          res = add_event out (Lost cid, p) ->
          Modifiers.extractl Modifiers.gOnlose (fd_mods fd) = Some cid ->
          ifight (((0:nat, opp), p)::xs) stt fd res
  | lost_std:
      forall opp p stt fd xs out res,
          ifight xs stt fd out ->
          Modifiers.extractl Modifiers.gOnlose (fd_mods fd) = None ->
          res = add_event out (Std 0%nat, p) ->
          ifight (((0%nat, opp), p)::xs) stt fd res
  | win: forall lw p stt fd xs out res,
          lw <> 0%nat ->
          ifight xs stt fd out ->
          res = add_event out (Std lw, p) ->
          ifight (((lw, 0%nat), p)::xs) stt fd res
  | applydmg_shortcut:
        forall lw opp p stt fd nstt xs out subout res,
            (lw > 0)%nat -> (opp > 0)%nat ->
            nstt = update_endurance (fun _ => lw) stt ->
            ifight xs stt fd out ->
            fight_shortcut nstt fd = Some subout ->
            res = add_proba (mul_proba subout p) out ->
            ifight (((lw, opp), p)::xs) stt fd res
  | applydmg:
        forall lw opp p stt fd nstt nfd xs out fightroundres subout res,
            (lw > 0)%nat -> (opp > 0)%nat ->
            nstt = update_endurance (fun _ => lw) stt ->
            nfd = Details (fd_sk fd) opp (Modifiers.advance_time (fd_mods fd)) ->
            ifight xs stt fd out ->
            fight_shortcut nstt fd = None ->
            fight_round nstt fd = fightroundres ->
            ifight fightroundres nstt nfd subout ->
            res = add_proba (mul_proba subout p) out ->
            ifight (((lw, opp), p)::xs) stt fd res
  | done: forall stt fd, ifight [] stt fd []
.

Definition run_ifight (stt: Stt) (fd: fight_details) (res: Proba TEscaped) :=
  ifight [((curendurance stt, fd_opp_hp fd), 1)] stt fd res.

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

Lemma ifight_empty_is_done: forall cur stt fd out,
      ifight cur stt fd out -> (out = [] <-> cur = []).
Proof.
  assert (forall stt fd, fight_shortcut stt fd = Some [] -> False) as FS.
  {
    intros. pose proof (fight_shortcut_correct stt fd [] H) .
    compute in H0. inversion H0.
  }
  intros.
  induction H; split; intros; subst; auto; try (apply Insert.empty in H2; contradiction).
  {
    inversion H2.
  }
  {
    inversion H2.
  }
  {
    inversion H2.
  }
  {
    apply Merge.empty in H5. destruct H5; subst.
    apply map_eq_nil in H1; subst.
    apply FS in H3. contradiction.
  }
  {
    inversion H5.
  }
  {
    apply Merge.empty in H8. destruct H8; subst.
    apply map_eq_nil in H1; subst.
    eassert (fight_round _ _ = []).
    apply IHifight2. reflexivity.
    assert (FullProba ([]:Proba (nat * nat))).
    rewrite <- H1.
    apply fight_round_full.
    compute in H2.
    inversion H2.
  }
  inversion H8.
Qed.

Lemma ifight_empty_out: forall stt fd out,
    ifight [] stt fd out -> out = [].
Proof.
  intros.
  apply ifight_empty_is_done in H. tauto.
Qed.

Lemma ifight_correct: forall cur stt fd res,
    (curendurance stt > 0)%nat -> ValidFight fd -> ifight cur stt fd res ->
      SumProba cur = SumProba res.
Proof.
  intros. 
  induction H1; intros; subst.
  {
    rewrite add_event_adds_proba.
    rewrite sumproba_cons.
    f_equal.
    apply IHifight; auto.
  }
  {
    rewrite add_event_adds_proba.
    rewrite sumproba_cons.
    f_equal.
    apply IHifight; auto.
  }
  {
    rewrite add_event_adds_proba.
    rewrite sumproba_cons.
    f_equal.
    apply IHifight; auto.
  }
  {
    rewrite add_proba_adds_proba.
    rewrite sumproba_cons.
    f_equal.
    rewrite mul_proba_sum.
    pose proof (fight_shortcut_correct _ _ _ H5).
    rewrite H3.
    field.
    apply IHifight;auto.
  }
  {
    rewrite add_proba_adds_proba.
    rewrite sumproba_cons.
    rewrite mul_proba_sum.
    rewrite <- IHifight2; auto.
    * rewrite fight_round_full. 
      f_equal.
      field.
      apply IHifight1; auto.
    * apply valid_advanced; auto.
  }
  reflexivity.
Qed.

Lemma ifight_result_unique: forall cur stt d res1 res2,
    ValidFight d ->
      ifight cur stt d res1 ->
      ifight cur stt d res2 ->
      res1 = res2
      .
Proof.
  intros.
  generalize dependent res2.
  induction H0; intros.
  * inversion H3; subst; clear H3; try contradiction; try Lia.lia. 
    + rewrite H2 in H12. inversion H12; subst. f_equal.
      apply IHifight; auto.
    + rewrite H2 in H11. inversion H11.
  * inversion H3; subst; clear H3; try contradiction; try Lia.lia. 
    + rewrite H1 in H12. discriminate.
    + f_equal.
      apply IHifight; auto.
  * inversion H3; subst; clear H3; try contradiction; try Lia.lia. 
    + f_equal.
      apply IHifight; auto.
  * inversion H6; subst; clear H6; try contradiction; try Lia.lia. 
    + f_equal.
      - f_equal. rewrite H4 in H18.
        inversion H18; subst; reflexivity.
      - apply IHifight; auto.
    + rewrite H4 in H16. discriminate.
  * inversion H7; subst; clear H7; try contradiction; try Lia.lia. 
    + rewrite H4 in H19. discriminate.
    + f_equal.
      - f_equal. apply IHifight2; auto.
        apply valid_advanced; auto.
      - apply IHifight1; auto.
  * apply ifight_empty_out in H1.
    subst.
    reflexivity.
Qed.

Lemma ifight_validmap: forall cur stt d res,
  ifight cur stt d res -> ValidMap res.
Proof.
  intros.
  induction H; subst; try (apply Insert.insert_with_valid);
    try apply (add_proba_valid);
    try apply (map_proba_valid); auto.
  * eapply fight_shortcut_valid; eauto.
  * constructor.
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

Lemma ifight_swap: forall cur c1 c2 stt d res,
  ifight (c1::c2::cur) stt d res -> ifight (c2::c1::cur) stt d res.
Proof.

  Ltac gogo_ifight_swap := match goal with
  | H1: ?a = Some ?x, H2: ?a = Some ?y |- _ =>
      rewrite H1 in H2; inversion H2; subst; clear H2
  | H1: ?a = Some _, H2: ?a = None |- _ =>
      rewrite H1 in H2; discriminate
  | H: Modifiers.extractl Modifiers.gOnlose (fd_mods _) = Some _ |-
          ifight ((0%nat, _, _):: _) _ _ _ =>
            eapply lost_onlose; eauto
  | |- ifight ((_, 0%nat, _):: _) _ _ _ =>
            eapply win; eauto
  | H1: (?opp > 0)%nat, H2: fight_shortcut _ _ = Some _ |- ifight ((_, ?opp, _):: _) _ _ _ =>
            eapply applydmg_shortcut; eauto
  | |- add_event (add_event ?o ?a) ?b = add_event (add_event ?o ?b) ?a =>
        apply AddEvent.swap_l
  | H: ifight (_::_) _ _ _ |- _ => inversion H; subst; clear H
  | |- add_event (add_proba ?x ?y) ?z = add_proba ?x (add_event ?y ?z) =>
      apply AddProba.add_proba_event_swap
  | |- ValidMap (mul_proba _ _) => apply map_proba_valid
  | H: fight_shortcut _ _ = Some ?o |- ValidMap ?o =>
        eapply fight_shortcut_valid; eauto
  | H: ifight _ _ _ ?o |- ValidMap ?o =>
        eapply ifight_validmap; eauto
  | |- add_proba ?a (add_proba ?b ?c) = add_proba (add_proba ?a ?b) ?c =>
        apply Merge.assoc
  | |- add_proba ?a (add_event ?b ?c) = add_event (add_proba ?a ?b) ?c =>
        repeat rewrite AddProba.add_proba_event
  | |- forall _ _ _ , _ + (_ + _) = _ + _ + _ => intros; field
  | |- ValidMap [_] => constructor
  | |- add_proba ?a (add_proba ?b ?c) = add_proba ?b (add_proba ?a ?c) =>
        apply AddProba.swap
  | H1: fight_shortcut (update_endurance (fun _ : endurance => ?lw1) ?stt) ?d = Some _,
    H2: fight_shortcut (update_endurance (fun _ : endurance => _) ?stt) ?d = None |- _ =>
      apply (fight_shortcut_endurance_unchanged _ lw1) in H2
  end.

  intros.
  destruct c1 as [[lw1 opp1] p1].
  destruct c2 as [[lw2 opp2] p2].
  inversion H; subst; clear H; destruct lw2.
  * inversion H7; subst; repeat gogo_ifight_swap; try Lia.lia.
  * inversion H7; subst; repeat gogo_ifight_swap; try Lia.lia.
    eapply applydmg; eauto.
    eapply lost_onlose; eauto.
    rewrite H21.
    repeat gogo_ifight_swap.
  * inversion H7; subst; repeat gogo_ifight_swap; try Lia.lia.
    eapply lost_std; auto.
    eapply lost_std; auto.
    apply H2.
    apply AddEvent.swap_l.
  * inversion H8; subst; repeat gogo_ifight_swap; try Lia.lia; clear H8.
    eapply lost_std; eauto.
    gogo_ifight_swap.
    eapply lost_std; eauto.
    repeat gogo_ifight_swap.
    eapply applydmg; eauto.
    eapply lost_std; eauto.
    repeat gogo_ifight_swap.
  * repeat gogo_ifight_swap; try Lia.lia.
    eapply lost_std;eauto.
    eapply win;eauto.
    repeat gogo_ifight_swap.
  * repeat gogo_ifight_swap; try Lia.lia.
    eapply applydmg; eauto.
    eapply win;eauto.
    repeat gogo_ifight_swap.
  * repeat gogo_ifight_swap; try Lia.lia.
    eapply lost_std; eauto.
    eapply applydmg_shortcut; eauto.
    repeat gogo_ifight_swap.
  * repeat gogo_ifight_swap; try Lia.lia.
      Unshelve.
        assumption.
        assumption.
        assumption.
        assumption.
        assumption.
        assumption.
        assumption.
  * repeat gogo_ifight_swap; try Lia.lia.
    + eapply applydmg; eauto.
    + repeat gogo_ifight_swap.
    + eapply lost_std; eauto.
      eapply applydmg; eauto.
      repeat gogo_ifight_swap.
  * repeat gogo_ifight_swap; try Lia.lia.
    + eapply applydmg; eauto.
    + repeat gogo_ifight_swap.
    Unshelve.
      assumption.
      assumption.
      assumption.
    + eapply applydmg; eauto.
      eapply applydmg; eauto.
      repeat gogo_ifight_swap.
Qed.

Fixpoint transpose_list_option {A: Set} (l: list (option A)): option (list A) :=
  match l with
  | [] => Some []
  | None::_ => None
  | Some x :: xs =>
      match transpose_list_option xs with
      | Some out => Some (x :: out)
      | None => None
      end
  end.

Fixpoint fight_juice (juice: nat) (stt: Stt) (d: fight_details): option (Proba TEscaped) :=
  match juice with
  | 0%nat => None
  | S juice => if curendurance stt =? 0
      then
        match Modifiers.extractl Modifiers.gOnlose (fd_mods d) with
        | None => Some [(Std 0, 1)]
        | Some cid => Some [(Lost cid, 1)]
        end
      else if fd_opp_hp d =? 0
      then Some [(Std (curendurance stt), 1)]
      else
        match fight_shortcut stt d with
        | Some x => Some x
        | None =>
            let fight_round_result: Proba (nat * nat) := fight_round stt d in
            let fullresult := map (fun (lwop : (nat * nat) * Qc) => 
                let (lwo, p) := lwop in
                let (lw, opp) := lwo in
                let nstt := update_endurance (fun _ => lw) stt in
                let nfd := Details (fd_sk d) opp (Modifiers.advance_time (fd_mods d)) in
                match fight_juice juice nstt nfd with
                | None => None
                | Some lst => Some (mul_proba lst p)
                end
                ) fight_round_result in
            match transpose_list_option fullresult with
            | None => None
            | Some x => Some (rebuild_proba (concat x))
            end
        end
  end.
