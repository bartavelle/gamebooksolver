(* Plain fights are monotone: with more endurance, a weaker opponent or a better combat ratio, a fight is never worse. A
   plain fight is one whose modifiers only change the ratio or the damage, so that it can only end with the player alive with
   some endurance left, or dead. The fights that can lead to another chapter (when damaged, after some rounds, by evading or
   by losing) are left out, as whether that chapter is better depends on the book. *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import hits.
Require Import proba.
Require Import combat.
Require Import fight_nonneg.
Require Import Stdlib.Logic.FunctionalExtensionality.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Bool.Bool.
Require Import Stdlib.Arith.Arith.
From Stdlib Require Import ZArith.
From Stdlib Require Import Lia.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.QArith.QArith.

Open Scope nat_scope.

(* The combat table: a better ratio never gives the opponent less damage, nor the player more, for each number picked *)
Definition col_le (x y: nat * nat) : bool := Nat.leb (fst x) (fst y) && Nat.leb (snd y) (snd x).

Fixpoint cols_le (l1 l2: list (nat * nat)) : bool :=
  match l1, l2 with
  | [], [] => true
  | x :: q1, y :: q2 => col_le x y && cols_le q1 q2
  | _, _ => false
  end.

Definition clamp (z: Z) : Z := Z.max (-11) (Z.min 11 z).

Definition ratios : list Z := map (fun n => Z.of_nat n - 11)%Z (seq 0 23).

Lemma ratios_in: forall z, In (clamp z) ratios.
Proof.
  intro z. unfold ratios. apply in_map_iff. exists (Z.to_nat (clamp z + 11)). split.
  - unfold clamp. lia.
  - apply in_seq. unfold clamp. lia.
Qed.

Lemma hits_clamp: forall z, hits_from_ratio z = hits_from_ratio (clamp z).
Proof.
  intro z. unfold clamp.
  destruct (Z_lt_le_dec z (-10)) as [L|L].
  { replace (Z.max (-11) (Z.min 11 z)) with (Z.max (-11) z) by lia.
    unfold hits_from_ratio at 1. replace (z <? -10)%Z with true by (symmetry; apply Z.ltb_lt; lia).
    destruct (Z_lt_le_dec z (-11)); [replace (Z.max (-11) z) with (-11)%Z by lia; reflexivity|].
    replace (Z.max (-11) z) with z by lia. unfold hits_from_ratio. replace (z <? -10)%Z with true by (symmetry; apply Z.ltb_lt; lia).
    reflexivity. }
  destruct (Z_le_gt_dec z 10) as [H|H].
  { replace (Z.max (-11) (Z.min 11 z)) with z by lia. reflexivity. }
  replace (Z.max (-11) (Z.min 11 z)) with 11%Z by lia.
  unfold hits_from_ratio. replace (z <? -10)%Z with false by (symmetry; apply Z.ltb_ge; lia). simpl.
  destruct z as [|p|p]; try lia.
  do 4 (destruct p as [p|p|]; try (exfalso; lia); try reflexivity).
Qed.

Lemma table_check: forallb (fun z1 => forallb (fun z2 => negb (Z.leb z1 z2) || cols_le (hits_from_ratio z1) (hits_from_ratio z2))
  ratios) ratios = true.
Proof. vm_compute. reflexivity. Qed.

Lemma table_length_check: forallb (fun z => Nat.eqb (length (hits_from_ratio z)) 10) ratios = true.
Proof. vm_compute. reflexivity. Qed.

Lemma cols_le_forall2: forall l1 l2, cols_le l1 l2 = true ->
  Forall2 (fun x y => fst x <= fst y /\ snd y <= snd x) l1 l2.
Proof.
  induction l1 as [|x q IH]; intros [|y l2] H; simpl in H; try discriminate; constructor.
  - apply andb_true_iff in H. destruct H as [H _]. unfold col_le in H. apply andb_true_iff in H.
    destruct H as [A B]. apply Nat.leb_le in A, B. tauto.
  - apply IH. apply andb_true_iff in H. tauto.
Qed.

Lemma hits_mono: forall z1 z2, (z1 <= z2)%Z ->
  Forall2 (fun x y => fst x <= fst y /\ snd y <= snd x) (hits_from_ratio z1) (hits_from_ratio z2).
Proof.
  intros z1 z2 L. rewrite (hits_clamp z1), (hits_clamp z2). apply cols_le_forall2.
  pose proof table_check as C. rewrite forallb_forall in C. specialize (C _ (ratios_in z1)).
  rewrite forallb_forall in C. specialize (C _ (ratios_in z2)).
  assert (Z.leb (clamp z1) (clamp z2) = true) as E by (apply Z.leb_le; unfold clamp; lia).
  rewrite E in C. exact C.
Qed.

Lemma hits_length: forall z, length (hits_from_ratio z) = 10.
Proof.
  intro z. rewrite hits_clamp. pose proof table_length_check as C. rewrite forallb_forall in C.
  apply Nat.eqb_eq. apply C. apply ratios_in.
Qed.

(* The modifiers of a plain fight. A timed modifier is plain if what it holds is. *)
Fixpoint plain_mod (m: fight_modifier) : bool :=
  match m with
  | Timed _ x => plain_mod x
  | CombatBonus _ | MindblastImmune | EnemyMindblast | WeakMindblast | ForceEMindblast | BareHanded | DoubleDamage
  | Undead | Dpr _ | PlayerInvulnerable | EnemyInvulnerable | MultiFight | NoPotion => true
  | Evaded _ | OnDamage _ | OnNotYetWon _ | OnLose _ | StopFight _ | OnPickOne _ | FakeFight _ | Poisonous _ => false
  end.

Definition plain (mds: list fight_modifier) : bool := forallb plain_mod mds.

Lemma plain_extract {A: Type}: forall (g: fight_modifier -> option A) mds,
  (forall m, plain_mod m = true -> g m = None) -> plain mds = true -> Modifiers.extractl g mds = None.
Proof.
  intros g mds G P. induction mds as [|m q IH]; [reflexivity|]. simpl in *. apply andb_true_iff in P.
  rewrite G by apply P. apply IH. apply P.
Qed.

Lemma plain_cur: forall mds, plain mds = true -> plain (Modifiers.get_cur_mods mds) = true.
Proof.
  induction mds as [|m q IH]; intro P; [reflexivity|]. simpl in P. apply andb_true_iff in P. destruct P as [P1 P2].
  destruct m as [| |k x| | | | | | | | | | | | | | | | | | |]; simpl in *.
  all: try discriminate; try (apply IH; exact P2).
  destruct x; simpl in *; try discriminate; try (apply IH; exact P2); rewrite P1; simpl; apply IH; exact P2.
Qed.

Lemma plain_advance: forall mds, plain mds = true -> plain (Modifiers.advance_time mds) = true.
Proof.
  induction mds as [|m q IH]; intro P; [reflexivity|]. simpl in P. apply andb_true_iff in P. destruct P as [P1 P2].
  destruct m as [| |k x| | | | | | | | | | | | | | | | | | |]; simpl in *; try discriminate; try (apply IH; exact P2).
  destruct k as [|[|k]].
  - destruct x; simpl in *; try discriminate; try (apply IH; exact P2); rewrite P1; simpl; apply IH; exact P2.
  - destruct x; simpl in *; try discriminate; try (apply IH; exact P2); rewrite P1; simpl; apply IH; exact P2.
  - simpl. rewrite P1. simpl. apply IH. exact P2.
Qed.

Ltac not_plain := intros m; destruct m; simpl; congruence.

(* What is left of a plain fight: each round either goes on, or ends it with the endurance of the player *)
Section Value.
  Open Scope Qc_scope.
  Variable f: TEscaped -> Qc.
  Variable osk: Z.

  (* the value of a fight from a round, by going backwards *)
  Fixpoint W (stt: Stt) (n: nat) (mds: list fight_modifier) (k: nat * nat) : Qc :=
    match n with
    | O => 0
    | S m =>
        let r := fight_step (update_endurance (fun _ => fst k) stt) osk (snd k) mds in
        ev f (fst r) + ev (W stt m (Modifiers.advance_time mds)) (snd r)
    end.

  Lemma ev_zero {K: Set}: forall (d: Proba K), ev (fun _ => 0) d = 0.
  Proof. intro d. induction d as [|[k p] d IH]; unfold ev in *; simpl; [reflexivity|]. rewrite IH. ring. Qed.

  Lemma ev_nil {K: Set}: forall (g: K -> Qc), ev g [] = 0.
  Proof. reflexivity. Qed.

  Lemma rounds_value: forall n stt F mds acc,
    ev f (fight_rounds n stt osk F mds acc) = ev f acc + ev (W stt n mds) F.
  Proof.
    induction n as [|n IH]; intros stt F mds acc.
    - simpl. replace (ev (fun _ => 0) F) with 0 by (symmetry; apply ev_zero). ring.
    - destruct F as [|x F'].
      + simpl. unfold ev. simpl. ring.
      + cbn [fight_rounds]. rewrite IH, ev_add_proba.
        set (h := fun (st: (nat * nat) * Qc) (cur: RoundEnd) =>
          let '((lw, op), p) := st in
          let r := fight_step (update_endurance (fun _ => lw) stt) osk op mds in
          add_round_end (mul_proba (fst r) p, mul_proba (snd r) p) cur).
        assert (forall G, ev f (fst (fold_right h ([], []) G)) + ev (W stt n (Modifiers.advance_time mds)) (snd (fold_right h ([], []) G))
          = ev (W stt (S n) mds) G) as E.
        { induction G as [|[[lw op] p] G IHG]; [reflexivity|]. simpl fold_right. unfold h at 1. fold h.
          cbn [fst snd add_round_end]. rewrite !ev_add_proba, !ev_mul.
          unfold ev at 5. cbn [fold_right fst snd]. fold (ev (W stt (S n) mds) G). rewrite <- IHG. simpl W. ring. }
        rewrite <- E. ring.
  Qed.
End Value.

Section Plain.
  Open Scope Qc_scope.
  Variable f: TEscaped -> Qc.
  Variable osk: Z.

  Definition rsum (G: nat * nat -> Qc) (l: list (nat * nat)) : Qc := fold_right (fun x acc => G x + acc) 0 l.

  Lemma ev_fold_rounds: forall (h: nat * nat -> Qc) (r: nat * nat -> RoundEnd) l,
    ev f (fst (fold_rounds r l)) + ev h (snd (fold_rounds r l))
      = rsum (fun x => ev f (fst (r x)) + ev h (snd (r x))) l.
  Proof.
    intros h r l. unfold fold_rounds, rsum. induction l as [|x l IH]; [reflexivity|]. simpl fold_right.
    cbn [add_round_end fst snd]. rewrite !ev_add_proba, <- IH. ring.
  Qed.

  (* how a round of a plain fight ends *)
  Definition g_end (h: nat * nat -> Qc) (x: nat * nat) : Qc :=
    if (fst x =? 0)%nat then f (Std 0) else if (snd x =? 0)%nat then f (Std (fst x)) else h x.

  Lemma step_plain: forall (h: nat * nat -> Qc) stt op mds, plain mds = true ->
    ev f (fst (fight_step stt osk op mds)) + ev h (snd (fight_step stt osk op mds))
      = rsum (fun x => tenth * g_end h x) (round_results stt osk op mds).
  Proof.
    intros h stt op mds P. pose proof (plain_cur mds P) as PC.
    unfold fight_step.
    rewrite (plain_extract Modifiers.gStopFight) by (try not_plain; exact PC).
    rewrite (plain_extract gOnPickOne) by (try not_plain; exact P).
    rewrite (plain_extract Modifiers.gEvaded) by (try not_plain; exact PC).
    rewrite (plain_extract Modifiers.gPoisonous) by (try not_plain; exact PC).
    rewrite ev_fold_rounds. unfold rsum. f_equal. apply functional_extensionality. intros [lw o].
    apply functional_extensionality. intro acc. f_equal.
    unfold common_round, lost_result, win_result.
    rewrite (plain_extract gOnDamage) by (try not_plain; exact PC).
    rewrite (plain_extract Modifiers.gOnlose) by (try not_plain; exact PC).
    rewrite (plain_extract Modifiers.gOnNotYetWon) by (try not_plain; exact PC).
    unfold g_end. cbn [fst snd].
    destruct (lw =? 0)%nat; [unfold over; unfold ev; simpl; ring|].
    destruct (o =? 0)%nat; unfold over, next_round, ev; simpl; ring.
  Qed.

  Hypothesis F0: f (Std 0) = 0.
  Hypothesis FM: forall a b, (a <= b)%nat -> f (Std a) <= f (Std b).

  Lemma f_nonneg: forall a, 0 <= f (Std a).
  Proof. intro a. rewrite <- F0. apply FM. lia. Qed.

  Lemma rsum_le: forall (G1 G2: nat * nat -> Qc) (R: nat * nat -> nat * nat -> Prop) l1 l2,
    Forall2 R l1 l2 -> (forall x y, R x y -> G1 x <= G2 y) -> rsum G1 l1 <= rsum G2 l2.
  Proof.
    intros G1 G2 R l1 l2 F H. induction F as [|x y q1 q2 Rxy F IH]; unfold rsum; simpl; [apply Qcle_refl|].
    apply Qcplus_le_compat; [apply H; exact Rxy|exact IH].
  Qed.

  Lemma ten_tenths: tenth + (tenth + (tenth + (tenth + (tenth + (tenth + (tenth + (tenth + (tenth + (tenth + 0))))))))) = 1.
  Proof. apply Qc_is_canon. vm_compute. reflexivity. Qed.

  (* the rounds of a fight in a plain fight are ten, each with a tenth of the chances *)
  Lemma rsum_bound: forall (g: nat * nat -> Qc) l B, length l = 10%nat -> (forall x, In x l -> 0 <= g x <= B) ->
    0 <= rsum (fun x => tenth * g x) l <= B.
  Proof.
    intros g l B L H.
    do 10 (destruct l as [|? l]; [discriminate|]). destruct l; [|discriminate].
    unfold rsum. simpl fold_right.
    repeat match goal with |- context [g ?x] => let E := fresh "E" in assert (0 <= g x <= B) as E by (apply H; simpl; tauto); revert E; generalize (g x) end.
    intros. pose proof tenth_nn as T. pose proof ten_tenths as TT.
    assert (forall z, 0 <= z <= B -> 0 <= tenth * z /\ tenth * z <= tenth * B) as M.
    { intros z [Q1 Q2]. split.
      - rewrite (Qcmult_comm tenth z). rewrite <- (Qcmult_0_l tenth). apply Qcmult_le_compat_r; assumption.
      - rewrite (Qcmult_comm tenth z), (Qcmult_comm tenth B). apply Qcmult_le_compat_r; assumption. }
    repeat match goal with E: 0 <= ?z <= B |- _ => destruct (M z E) as [? ?]; clear E end.
    split.
    - repeat (rewrite <- (Qcplus_0_l (Q2Qc 0)) at 1; apply Qcplus_le_compat; [assumption|]). apply Qcle_refl.
    - apply Qcle_trans with (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + 0)))))))))).
      + repeat (apply Qcplus_le_compat; [assumption|]). apply Qcle_refl.
      + replace (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + (tenth * B + 0)))))))))) with ((tenth + (tenth + (tenth + (tenth + (tenth + (tenth + (tenth + (tenth + (tenth + (tenth + 0)))))))))) * B) by ring.
        rewrite TT, Qcmult_1_l. apply Qcle_refl.
    Qed.

  Lemma moddmg_mono: forall s t op1 op2 mds e1 l1 e2 l2,
    disciplines s = disciplines t -> has_item (Weapon Sommerswerd) s = has_item (Weapon Sommerswerd) t ->
    (curendurance s <= curendurance t)%nat -> (op2 <= op1)%nat -> (e1 <= e2)%nat -> (l2 <= l1)%nat ->
    (fst (moddmg s op1 mds (e1, l1)) <= fst (moddmg t op2 mds (e2, l2)))%nat /\
    (snd (moddmg t op2 mds (e2, l2)) <= snd (moddmg s op1 mds (e1, l1)))%nat.
  Proof.
    intros s t op1 op2 mds e1 l1 e2 l2 D S C O E L. unfold moddmg. rewrite D, S. cbn [fst snd].
    repeat match goal with |- context [if ?c then _ else _] => destruct c end; cbn [fst snd]; lia.
  Qed.

  Lemma moddmg_le: forall s op mds d, (fst (moddmg s op mds d) <= curendurance s)%nat.
  Proof. intros s op mds [e l]. unfold moddmg. cbn [fst]. lia. Qed.

  Lemma rsum_map: forall (G: nat * nat -> Qc) (m: nat * nat -> nat * nat) l, rsum G (map m l) = rsum (fun x => G (m x)) l.
  Proof. intros. induction l as [|x l IH]; unfold rsum in *; simpl; [reflexivity|]. rewrite IH. reflexivity. Qed.

  Lemma W_unfold: forall stt n mds lw op, plain mds = true ->
    W f osk stt (S n) mds (lw, op)
      = rsum (fun x => tenth * g_end (W f osk stt n (Modifiers.advance_time mds)) x)
             (round_results (update_endurance (fun _ => lw) stt) osk op mds).
  Proof. intros stt n mds lw op P. simpl W. apply step_plain. exact P. Qed.

  Lemma tenth_le: forall a b, a <= b -> tenth * a <= tenth * b.
  Proof.
    intros a b L. rewrite (Qcmult_comm tenth a), (Qcmult_comm tenth b). apply Qcmult_le_compat_r; [exact L|apply tenth_nn].
  Qed.

  Lemma W_bound: forall n stt mds lw op, plain mds = true -> 0 <= W f osk stt n mds (lw, op) <= f (Std lw).
  Proof.
    induction n as [|n IH]; intros stt mds lw op P.
    { split; [apply Qcle_refl|apply f_nonneg]. }
    rewrite W_unfold by exact P. apply rsum_bound.
    - unfold round_results. rewrite length_map. apply hits_length.
    - intros [a b] I. unfold round_results in I. apply in_map_iff in I. destruct I as [dm [E _]].
      pose proof (moddmg_le (update_endurance (fun _ => lw) stt) op mds dm) as LE. rewrite E in LE. simpl in LE.
      unfold g_end. cbn [fst snd].
      destruct (a =? 0)%nat; [rewrite F0; split; [apply Qcle_refl|apply f_nonneg]|].
      destruct (b =? 0)%nat; [split; [apply f_nonneg|apply FM; exact LE]|].
      destruct (IH stt (Modifiers.advance_time mds) a b (plain_advance mds P)) as [B1 B2].
      split; [exact B1|]. eapply Qcle_trans; [exact B2|]. apply FM. exact LE.
  Qed.

  Variables s t: Stt.
  Hypothesis RA: forall m, (get_ratio s osk m <= get_ratio t osk m)%Z.
  Hypothesis DI: disciplines s = disciplines t.
  Hypothesis SO: has_item (Weapon Sommerswerd) s = has_item (Weapon Sommerswerd) t.

  Lemma W_mono: forall n mds lw1 op1 lw2 op2, plain mds = true -> (lw1 <= lw2)%nat -> (op2 <= op1)%nat ->
    W f osk s n mds (lw1, op1) <= W f osk t n mds (lw2, op2).
  Proof.
    induction n as [|n IH]; intros mds lw1 op1 lw2 op2 P L1 L2; [apply Qcle_refl|].
    rewrite !W_unfold by exact P. unfold round_results. rewrite !rsum_map.
    apply (rsum_le _ _ (fun x y => (fst x <= fst y)%nat /\ (snd y <= snd x)%nat)).
    - apply hits_mono. apply RA.
    - intros [e1 l1] [e2 l2] [E L]. cbn [fst snd] in E, L.
      destruct (moddmg_mono (update_endurance (fun _ => lw1) s) (update_endurance (fun _ => lw2) t) op1 op2 mds e1 l1 e2 l2)
        as [A B]; try assumption.
      destruct (moddmg (update_endurance (fun _ => lw1) s) op1 mds (e1, l1)) as [a1 b1].
      destruct (moddmg (update_endurance (fun _ => lw2) t) op2 mds (e2, l2)) as [a2 b2].
      cbn [fst snd] in A, B. apply tenth_le. unfold g_end. cbn [fst snd].
      destruct (a1 =? 0)%nat eqn: Z1.
      { destruct (a2 =? 0)%nat; [apply Qcle_refl|].
        destruct (b2 =? 0)%nat; [apply FM; lia|]. rewrite F0. apply (W_bound n t _ a2 b2 (plain_advance mds P)). }
      apply Nat.eqb_neq in Z1. replace (a2 =? 0)%nat with false by (symmetry; apply Nat.eqb_neq; lia).
      destruct (b1 =? 0)%nat eqn: Z2.
      { apply Nat.eqb_eq in Z2. replace (b2 =? 0)%nat with true by (symmetry; apply Nat.eqb_eq; lia). apply FM. exact A. }
      destruct (b2 =? 0)%nat.
      + eapply Qcle_trans; [apply (W_bound n s _ a1 b1 (plain_advance mds P))|]. apply FM. exact A.
      + apply IH; [apply plain_advance; exact P|exact A|exact B].
  Qed.
End Plain.

(* A plain fight is never worse with more endurance, a weaker opponent and a ratio at least as good, for any way of scoring
   its end that is worth nothing when dead and never less with more endurance left *)
Theorem fight_monotone: forall (f: TEscaped -> Qc) (s t: Stt) (osk: Z) (op1 op2: nat) (mds: list fight_modifier),
  plain mds = true ->
  f (Std 0) = 0%Qc -> (forall a b, (a <= b)%nat -> (f (Std a) <= f (Std b))%Qc) ->
  (forall m, (get_ratio s osk m <= get_ratio t osk m)%Z) ->
  disciplines s = disciplines t -> has_item (Weapon Sommerswerd) s = has_item (Weapon Sommerswerd) t ->
  (curendurance s <= curendurance t)%nat -> (op2 <= op1)%nat ->
  (ev f (fight s osk op1 mds) <= ev f (fight t osk op2 mds))%Qc.
Proof.
  intros f s t osk op1 op2 mds P F0 FM RA DI SO C O. unfold fight. rewrite !rounds_value.
  (* without simpl, which would unfold the 1000 rounds *)
  change [((curendurance s, op1), 1%Qc)] with (certain (curendurance s, op1)).
  change [((curendurance t, op2), 1%Qc)] with (certain (curendurance t, op2)).
  rewrite !ev_single, !ev_nil, !Qcplus_0_l.
  apply (W_mono f osk F0 FM s t); assumption.
Qed.

(* The ratio only depends on the combat skill, the disciplines, the items and the flags *)
Lemma ratio_skill: forall s t osk m, disciplines s = disciplines t -> items s = items t -> flags s = flags t ->
  get_ratio t osk m = (get_ratio s osk m + (sk t - sk s))%Z.
Proof.
  intros s t osk m D I F. unfold get_ratio, has_item, has_flag, all_character_items_slot_count, all_character_items_slot.
  rewrite D, I, F. lia.
Qed.

(* So a plain fight is never worse with more combat skill and more endurance, everything else being the same *)
Corollary fight_more_skill: forall (f: TEscaped -> Qc) (s t: Stt) (osk: Z) (op: nat) (mds: list fight_modifier),
  plain mds = true ->
  f (Std 0) = 0%Qc -> (forall a b, (a <= b)%nat -> (f (Std a) <= f (Std b))%Qc) ->
  disciplines s = disciplines t -> items s = items t -> flags s = flags t ->
  (sk s <= sk t)%Z -> (curendurance s <= curendurance t)%nat ->
  (ev f (fight s osk op mds) <= ev f (fight t osk op mds))%Qc.
Proof.
  intros f s t osk op mds P F0 FM D I F K C. apply fight_monotone; auto.
  - intro m. rewrite (ratio_skill s t osk m D I F). lia.
  - unfold has_item. rewrite I. reflexivity.
Qed.
