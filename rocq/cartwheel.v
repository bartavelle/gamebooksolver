(* Staking a single coin is optimal at Cartwheel (Book 2, section 238).

   Each round, the player stakes any number of crowns. With probability 1/10 the
   stake wins 8 per crown, with probability 2/10 it wins 5 per crown, and otherwise
   it is lost. The player may stop whenever the rules allow it, and the value of
   stopping depends on the rest of the book.

   Nothing here depends on that rest of the book: the theorems hold for every
   exit value, even one that is not increasing with gold. They also hold for
   both readings of the payout and for any cap on the gold:

   - [net] true: "win 8" is a profit, the stake is kept
   - [net] false: "win 8" includes the stake, so the profit is 7 per crown
   - [cap]: the most gold that can be held, 50 for the pouch, or the arrival gold
     plus 40 for the table limit

   The free silver token is a separate one-crown stake that cannot be cashed.

   Proof idea: let G x be the distance between the value at x gold and the value
   at the cap. A function that can't be improved by staking 1 is nondecreasing in
   gold, and its G shrinks by at least 77/100 for each extra crown. A stake of 2
   or more loses at least 2 crowns with probability 7/10, which costs more than
   any win can bring back.

   Main results: [stake_one_is_excessive], [any_stake_le_excessive] and
   [stake_one_optimal]. *)

From Stdlib Require Import Reals Lra Lia Arith List Bool Classical_Prop.
Import ListNotations.
Open Scope R_scope.

(** * One layer: gold only, with a fixed profit per crown *)

Section Chain.
Variables (a b cap : nat) (V : nat -> R).
Hypothesis Hab : (a = 8 /\ b = 5)%nat \/ (a = 7 /\ b = 4)%nat.

Definition land (c : nat) (x m : nat) : nat := Nat.min (x + m) c.

Hypothesis SH : forall g, (1 <= g <= cap)%nat ->
  1/10 * V (land cap g a) + 1/5 * V (land cap g b) + 7/10 * V (g - 1) <= V g.

(* the minimum of V on [g, cap] can only be reached at g, so V (g - 1) <= V g *)
Lemma step_down : forall n g m, (1 <= g)%nat -> (g <= m <= cap)%nat -> (m - g = n)%nat ->
  (forall x, (g <= x <= cap)%nat -> V m <= V x) -> V (g - 1) <= V g.
Proof using Hab SH.
  induction n; intros g m Hg Hm Hn Hmin.
  - assert (m = g) by lia; subst m.
    assert (H := SH g ltac:(lia)).
    assert (V g <= V (land cap g a)) by (apply Hmin; unfold land; lia).
    assert (V g <= V (land cap g b)) by (apply Hmin; unfold land; lia).
    lra.
  - assert (H := SH m ltac:(lia)).
    assert (V m <= V (land cap m a)) by (apply Hmin; unfold land; lia).
    assert (V m <= V (land cap m b)) by (apply Hmin; unfold land; lia).
    assert (V (m - 1) <= V m) by lra.
    apply (IHn g (m - 1)%nat); try lia.
    intros x Hx. assert (V m <= V x) by (apply Hmin; lia). lra.
Qed.

Lemma exists_min : forall k g, (g + k = cap)%nat ->
  exists m, (g <= m <= cap)%nat /\ forall x, (g <= x <= cap)%nat -> V m <= V x.
Proof using Hab SH.
  induction k; intros g Hk.
  - exists g. split; [lia|]. intros x Hx. assert (x = g) by lia. subst. lra.
  - destruct (IHk (S g) ltac:(lia)) as [m [Hm Hmin]].
    destruct (Rle_dec (V g) (V m)).
    + exists g. split; [lia|]. intros x Hx.
      destruct (Nat.eq_dec x g); [subst; lra|].
      assert (V m <= V x) by (apply Hmin; lia). lra.
    + exists m. split; [lia|]. intros x Hx.
      destruct (Nat.eq_dec x g); [subst; lra|]. apply Hmin; lia.
Qed.

Lemma mono_step : forall g, (1 <= g <= cap)%nat -> V (g - 1) <= V g.
Proof using Hab SH.
  intros g Hg.
  destruct (exists_min (cap - g) g ltac:(lia)) as [m [Hm Hmin]].
  exact (step_down (m - g) g m ltac:(lia) Hm eq_refl Hmin).
Qed.

Lemma mono : forall x y, (x <= y <= cap)%nat -> V x <= V y.
Proof using Hab SH.
  intros x y H. induction y.
  - assert (x = 0%nat) by lia. subst. lra.
  - destruct (Nat.eq_dec x (S y)); [subst; lra|].
    assert (V (S y - 1) <= V (S y)) by (apply mono_step; lia).
    replace (S y - 1)%nat with y in H0 by lia.
    assert (V x <= V y) by (apply IHy; lia). lra.
Qed.

Definition G (x : nat) : R := V cap - V (Nat.min x cap).

Lemma G_nonneg : forall x, 0 <= G x.
Proof using Hab SH.
  intro x. unfold G.
  assert (V (Nat.min x cap) <= V cap) by (apply mono; lia). lra.
Qed.

Lemma G_anti : forall x y, (x <= y)%nat -> G y <= G x.
Proof using Hab SH.
  intros x y H. unfold G.
  assert (V (Nat.min x cap) <= V (Nat.min y cap)) by (apply mono; lia). lra.
Qed.

Lemma G_top : forall x, (cap <= x)%nat -> G x = 0.
Proof using Hab SH.
  intros x H. unfold G. rewrite Nat.min_r by lia. lra.
Qed.

Lemma V_land : forall x m, V (land cap x m) = V cap - G (x + m).
Proof using Hab SH. intros. unfold G, land. lra. Qed.

Lemma V_G : forall x, (x <= cap)%nat -> V x = V cap - G x.
Proof using Hab SH. intros x H. unfold G. rewrite Nat.min_l by lia. lra. Qed.

Lemma star : forall y, (1 <= y)%nat ->
  G y <= 7/10 * G (y - 1) + 1/10 * G (y + a) + 1/5 * G (y + b).
Proof using Hab SH.
  intros y Hy. destruct (Nat.le_gt_cases y cap).
  - assert (H1 := SH y ltac:(lia)).
    rewrite !V_land in H1. rewrite (V_G y), (V_G (y - 1)) in H1 by lia. lra.
  - rewrite (G_top y) by lia.
    assert (A := G_nonneg (y - 1)).
    assert (B := G_nonneg (y + a)).
    assert (C := G_nonneg (y + b)).
    lra.
Qed.

Definition r : R := 77/100.

Lemma shrink_aux : forall n x, (cap <= x + n)%nat -> G (x + 1) <= r * G x.
Proof using Hab SH.
  unfold r. induction n; intros x Hx.
  - rewrite (G_top (x + 1)) by lia. assert (P := G_nonneg x). lra.
  - destruct (Nat.le_gt_cases cap x).
    { rewrite (G_top (x + 1)) by lia. assert (P := G_nonneg x). lra. }
    assert (St := star (x + 1) ltac:(lia)).
    replace (x + 1 - 1)%nat with x in St by lia.
    assert (P := G_nonneg x).
    assert (R1 := IHn (x + 1)%nat ltac:(lia)).
    assert (R2 := IHn (x + 2)%nat ltac:(lia)).
    assert (R3 := IHn (x + 3)%nat ltac:(lia)).
    assert (R4 := IHn (x + 4)%nat ltac:(lia)).
    assert (R5 := IHn (x + 5)%nat ltac:(lia)).
    assert (R6 := IHn (x + 6)%nat ltac:(lia)).
    assert (R7 := IHn (x + 7)%nat ltac:(lia)).
    assert (R8 := IHn (x + 8)%nat ltac:(lia)).
    rewrite <- !Nat.add_assoc in R1, R2, R3, R4, R5, R6, R7, R8. simpl in R1, R2, R3, R4, R5, R6, R7, R8.
    destruct Hab as [[Ha Hb]|[Ha Hb]]; rewrite Ha, Hb in St;
      rewrite <- !Nat.add_assoc in St; simpl in St; lra.
Qed.

Lemma shrink : forall x, G (x + 1) <= r * G x.
Proof using Hab SH.
  intro x. apply (shrink_aux cap x). lia.
Qed.

Lemma shrink2 : forall x, G (x + 2) <= 7/10 * G x.
Proof using Hab SH.
  intro x.
  assert (S1 := shrink x). assert (S2 := shrink (x + 1)%nat).
  replace (x + 1 + 1)%nat with (x + 2)%nat in S2 by lia.
  assert (P := G_nonneg x). unfold r in *. lra.
Qed.

(* a stake of 2 or more: whatever the wins are, the possible loss is too high *)
Lemma big_stake : forall g s m n, (2 <= s <= g)%nat -> (g <= cap)%nat ->
  1/10 * V (land cap g m) + 1/5 * V (land cap g n) + 7/10 * V (g - s) <= V g.
Proof using Hab SH.
  intros g s m n Hs Hg.
  assert (D := shrink2 (g - s)%nat).
  assert (M := G_anti (g - s + 2) g ltac:(lia)).
  assert (A := G_nonneg (g + m)). assert (B := G_nonneg (g + n)).
  rewrite !V_land. rewrite (V_G g), (V_G (g - s)) by lia. lra.
Qed.

(* what the token stake with one coin needs when the payout is a profit *)
Lemma token_coin_net : a = 8%nat -> b = 5%nat -> forall g, (1 <= g <= cap)%nat ->
  1/10 * V (land cap g 16) + 1/5 * V (land cap g 10) + V (g - 1)
  - 1/10 * V (land cap (g - 1) 8) - 1/5 * V (land cap (g - 1) 5) <= V g.
Proof using Hab SH.
  intros Ha Hb g Hg.
  rewrite !V_land. rewrite (V_G g), (V_G (g - 1)) by lia.
  replace (g - 1 + 8)%nat with (g + 7)%nat by lia.
  replace (g - 1 + 5)%nat with (g + 4)%nat by lia.
  assert (S0 := shrink (g - 1)%nat). replace (g - 1 + 1)%nat with g in S0 by lia.
  assert (S1 := shrink g).
  assert (S2 := shrink (g + 1)%nat). assert (S3 := shrink (g + 2)%nat). assert (S4 := shrink (g + 3)%nat).
  rewrite <- !Nat.add_assoc in S2, S3, S4. simpl in S2, S3, S4.
  assert (M := G_anti (g + 4) (g + 7) ltac:(lia)).
  assert (P1 := G_nonneg (g + 16)). assert (P2 := G_nonneg (g + 10)). assert (P3 := G_nonneg (g + 4)).
  assert (P4 := G_nonneg g). unfold r in *. lra.
Qed.

(* what the token stake with coins needs when the payout includes the stake *)
Lemma token_coins_gross : forall g sg m n, (1 <= sg <= g)%nat -> (g <= cap)%nat ->
  1/10 * V (land cap g m) + 1/5 * V (land cap g n) + 7/10 * V (g - sg)
  <= 1/10 * V (land cap g 8) + 1/5 * V (land cap g 5) + 7/10 * V g.
Proof using Hab SH.
  intros g sg m n Hs Hg.
  rewrite !V_land. rewrite (V_G g), (V_G (g - sg)) by lia.
  assert (S0 := shrink (g - sg)%nat).
  assert (M := G_anti (g - sg + 1) g ltac:(lia)).
  assert (M5 := G_anti g (g + 5) ltac:(lia)). assert (M8 := G_anti g (g + 8) ltac:(lia)).
  assert (P1 := G_nonneg (g + m)). assert (P2 := G_nonneg (g + n)).
  assert (S1 := shrink g). assert (S2 := shrink (g + 1)%nat).
  rewrite <- !Nat.add_assoc in S2. simpl in S2.
  assert (M2 := G_anti (g + 2) (g + 5) ltac:(lia)). assert (M3 := G_anti (g + 2) (g + 8) ltac:(lia)).
  assert (P0 := G_nonneg g).
  unfold r in *. lra.
Qed.

End Chain.

(** * The game *)

Section Game.
Variables (net : bool) (cap : nat).

(* gold, and whether the token is still there *)
Definition State : Type := (nat * bool)%type.
(* crowns staked, and whether the token is staked too *)
Definition Stake : Type := (nat * bool)%type.

Definition total (st : Stake) : nat := (fst st + if snd st then 1 else 0)%nat.
Definition legal (s : State) (st : Stake) : Prop :=
  (fst st <= fst s)%nat /\ (snd st = true -> snd s = true) /\ (1 <= total st)%nat.

(* the token is lost when it is staked, unless a win gives the stake back *)
Definition after_win (pay : nat) (s : State) (st : Stake) : State :=
  if net
  then (Nat.min (fst s + pay * total st) cap, snd s)
  else (Nat.min (fst s - fst st + pay * total st) cap, snd s && negb (snd st)).
Definition after_loss (s : State) (st : Stake) : State :=
  ((fst s - fst st)%nat, snd s && negb (snd st)).

Definition Q (V : State -> R) (s : State) (st : Stake) : R :=
  1/10 * V (after_win 8 s st) + 1/5 * V (after_win 5 s st) + 7/10 * V (after_loss s st).

Definition valid (s : State) : Prop := (fst s <= cap)%nat.

Lemma valid_win : forall pay s st, valid (after_win pay s st).
Proof. intros. unfold valid, after_win. destruct net; simpl; lia. Qed.
Lemma valid_loss : forall s st, valid s -> legal s st -> valid (after_loss s st).
Proof. intros s st H [L _]. unfold valid, after_loss in *. simpl. lia. Qed.

(* the value of stopping now, 0 when that is not allowed *)
Variable base : State -> R.

(* no stake can improve V, when staking exactly one *)
Definition Excessive (V : State -> R) : Prop :=
  (forall s, valid s -> base s <= V s)
  /\ forall s st, valid s -> legal s st -> total st = 1%nat -> Q V s st <= V s.

Definition pa : nat := if net then 8%nat else 7%nat.
Definition pb : nat := if net then 5%nat else 4%nat.
Lemma Hpab : (pa = 8 /\ pb = 5)%nat \/ (pa = 7 /\ pb = 4)%nat.
Proof. unfold pa, pb. destruct net; auto. Qed.

Lemma layer_SH : forall V t, Excessive V -> forall g, (1 <= g <= cap)%nat ->
  1/10 * (fun x => V (x, t)) (land cap g pa) + 1/5 * (fun x => V (x, t)) (land cap g pb)
  + 7/10 * (fun x => V (x, t)) (g - 1)%nat <= (fun x => V (x, t)) g.
Proof.
  intros V t [_ E] g Hg. simpl.
  assert (H := E (g, t) (1%nat, false) ltac:(unfold valid; simpl; lia)
    ltac:(unfold legal, total; simpl; repeat split; try lia; discriminate)
    ltac:(unfold total; simpl; lia)).
  unfold Q, after_win, after_loss, total in H. simpl in H. rewrite andb_true_r in H.
  unfold land, pa, pb. destruct net; simpl in H.
  - replace (g + 8)%nat with (g + (8 + 0))%nat in H by lia. simpl in H. exact H.
  - replace (g - 1 + 8)%nat with (g + 7)%nat in H by lia.
    replace (g - 1 + 5)%nat with (g + 4)%nat in H by lia. exact H.
Qed.

(** ** Stakes larger than 1 can't improve on a function that stake 1 can't improve *)
Theorem stake_one_is_excessive : forall V s st,
  Excessive V -> valid s -> legal s st -> Q V s st <= V s.
Proof.
  intros V [g t] [sg tk] HE Hv Hl.
  destruct (Nat.eq_dec (total (sg, tk)) 1) as [Hone|Hbig].
  { apply (proj2 HE); assumption. }
  unfold valid in Hv. destruct Hl as [Hsg [Htk Hs]]. simpl in *.
  unfold total in Hbig, Hs. simpl in Hbig, Hs.
  assert (SH0 := layer_SH V false HE). assert (SH1 := layer_SH V true HE).
  assert (SHt := layer_SH V t HE).
  unfold Q, after_win, after_loss, total. simpl.
  destruct tk.
  - (* the token and at least one coin *)
    assert (Ht : t = true) by auto. subst t. simpl.
    assert (Hsg1 : (1 <= sg)%nat) by lia.
    assert (Tok : forall y, (y <= cap)%nat ->
      1/10 * V (after_win 8 (y, true) (0%nat, true))
      + 1/5 * V (after_win 5 (y, true) (0%nat, true))
      + 7/10 * V (y, false) <= V (y, true)).
    { intros y Hy.
      assert (H := proj2 HE (y, true) (0%nat, true) ltac:(unfold valid; simpl; lia)
        ltac:(unfold legal, total; simpl; repeat split; lia)
        ltac:(unfold total; simpl; lia)).
      unfold Q, after_loss in H. simpl in H. rewrite Nat.sub_0_r in H. exact H. }
    unfold after_win in Tok. unfold total in Tok. simpl in Tok.
    destruct net eqn:Hnet; simpl in *.
    + (* the wins keep the token, the loss doesn't *)
      unfold pa, pb in *. rewrite Hnet in *.
      assert (HpA := Hpab). unfold pa, pb in HpA. rewrite Hnet in HpA.
      assert (Mono1 := mono 8 5 cap (fun x => V (x, true)) HpA SH1).
      assert (Le01 : forall y, (y <= cap)%nat -> V (y, false) <= V (y, true)).
      { intros y Hy. assert (T := Tok y Hy).
        try rewrite !Nat.add_0_r in T.
        assert (A := Mono1 y (Nat.min (y + 8) cap) ltac:(lia)).
        assert (B := Mono1 y (Nat.min (y + 5) cap) ltac:(lia)).
        simpl in A, B. lra. }
      assert (L := Le01 (g - sg)%nat ltac:(lia)).
      destruct (Nat.le_gt_cases 2 sg).
      * assert (B := big_stake 8 5 cap (fun x => V (x, true)) HpA SH1 g sg
          (8 * (sg + 1)) (5 * (sg + 1)) ltac:(lia) Hv).
        simpl in B. unfold land in B. lra.
      * assert (sg = 1%nat) by lia. subst sg.
        assert (T := Tok (g - 1)%nat ltac:(lia)).
        assert (B := token_coin_net 8 5 cap (fun x => V (x, true)) HpA SH1 eq_refl eq_refl g ltac:(lia)).
        simpl in B. unfold land in B. try rewrite !Nat.add_0_r in T. simpl. lra.
    + (* every result loses the token *)
      unfold pa, pb in *. rewrite Hnet in *.
      assert (HpA := Hpab). unfold pa, pb in HpA. rewrite Hnet in HpA.
      assert (B := token_coins_gross 7 4 cap (fun x => V (x, false)) HpA SH0 g sg
        (8 * (sg + 1) - sg) (5 * (sg + 1) - sg) ltac:(lia) Hv).
      cbv beta in B. unfold land in B.
      assert (T := Tok g Hv).
      replace (g + (8 * (sg + 1) - sg))%nat with (g - sg + 8 * (sg + 1))%nat in B by lia.
      replace (g + (5 * (sg + 1) - sg))%nat with (g - sg + 5 * (sg + 1))%nat in B by lia.
      simpl in B. try rewrite !Nat.sub_0_r in T. try rewrite !Nat.add_0_r in T. simpl in T. simpl. lra.
  - (* only coins *)
    assert (Hs2 : (2 <= sg)%nat) by lia. rewrite Nat.add_0_r. rewrite andb_true_r.
    assert (B := big_stake pa pb cap (fun x => V (x, t)) Hpab SHt g sg
      (if net then 8 * sg else 7 * sg) (if net then 5 * sg else 4 * sg) ltac:(lia) Hv).
    simpl in B. unfold land in B.
    destruct net; simpl; [exact B|].
    replace (g - sg + (sg + (sg + (sg + (sg + (sg + (sg + (sg + (sg + 0)))))))))%nat with (g + (sg + (sg + (sg + (sg + (sg + (sg + (sg + 0))))))))%nat by lia.
    replace (g - sg + (sg + (sg + (sg + (sg + (sg + 0))))))%nat with (g + (sg + (sg + (sg + (sg + 0)))))%nat by lia. exact B.
Qed.

(** ** Values after a limited number of rounds *)

Definition stake_candidates (s : State) : list Stake :=
  list_prod (seq 0 (S (fst s))) (if snd s then [false; true] else [false]).
Definition any_stakes (s : State) : list Stake :=
  filter (fun st => (1 <=? total st)%nat) (stake_candidates s).
Definition one_stakes (s : State) : list Stake :=
  filter (fun st => (total st =? 1)%nat) (stake_candidates s).

Lemma in_candidates : forall s st, In st (stake_candidates s) <->
  (fst st <= fst s)%nat /\ (snd st = true -> snd s = true).
Proof.
  intros [g t] [sg tk]. unfold stake_candidates. simpl.
  change (In (sg, tk) (list_prod (seq 0 (S g)) (if t then [false; true] else [false]))
    <-> (sg <= g)%nat /\ (tk = true -> t = true)).
  rewrite in_prod_iff, in_seq.
  destruct t, tk; simpl; intuition (try discriminate; try lia; auto).
Qed.

Lemma in_any : forall s st, In st (any_stakes s) <-> legal s st.
Proof.
  intros. unfold any_stakes, legal. rewrite filter_In, in_candidates, Nat.leb_le. tauto.
Qed.

Lemma in_one : forall s st, In st (one_stakes s) <-> legal s st /\ total st = 1%nat.
Proof.
  intros. unfold one_stakes, legal. rewrite filter_In, in_candidates, Nat.eqb_eq.
  intuition lia.
Qed.

(* the best that can be done in n rounds, using only the given stakes *)
Fixpoint W (stakes : State -> list Stake) (n : nat) (s : State) : R :=
  match n with
  | O => base s
  | S m => fold_right Rmax (base s) (map (Q (W stakes m) s) (stakes s))
  end.

Lemma fold_ub : forall l (b : R) (U : R), b <= U -> (forall x, In x l -> x <= U) ->
  fold_right Rmax b l <= U.
Proof.
  induction l; intros b U Hb H; simpl; auto.
  apply Rmax_lub; [apply H; simpl; auto|]. apply IHl; auto. intros; apply H; simpl; auto.
Qed.
Lemma fold_lb_base : forall l (b : R), b <= fold_right Rmax b l.
Proof.
  induction l; intro b; simpl; [lra|]. eapply Rle_trans; [apply IHl|]. apply Rmax_r.
Qed.
Lemma fold_lb_in : forall l (b x : R), In x l -> x <= fold_right Rmax b l.
Proof.
  induction l; intros b x H; simpl in *; [tauto|]. destruct H.
  - subst. apply Rmax_l.
  - eapply Rle_trans; [apply IHl; eauto|]. apply Rmax_r.
Qed.

Lemma Q_mono : forall (U V : State -> R) s st,
  U (after_win 8 s st) <= V (after_win 8 s st) ->
  U (after_win 5 s st) <= V (after_win 5 s st) ->
  U (after_loss s st) <= V (after_loss s st) -> Q U s st <= Q V s st.
Proof. intros. unfold Q. lra. Qed.

(** ** No number of rounds, with any stakes, goes above an excessive function *)
Theorem any_stake_le_excessive : forall V, Excessive V ->
  forall n s, valid s -> W any_stakes n s <= V s.
Proof.
  intros V HE. induction n; intros s Hv; simpl.
  - apply (proj1 HE); auto.
  - apply fold_ub; [apply (proj1 HE); auto|].
    intros x Hx. apply in_map_iff in Hx. destruct Hx as [st [<- Hst]].
    apply in_any in Hst.
    eapply Rle_trans; [|apply (stake_one_is_excessive V s st HE Hv Hst)].
    apply Q_mono; apply IHn; try apply valid_win. apply valid_loss; auto.
Qed.

(** ** The limit of the stake 1 values is excessive *)

Variable B : R.
Hypothesis base_le : forall s, base s <= B.

Definition W1 := W one_stakes.

Lemma W1_le_B : forall n s, W1 n s <= B.
Proof.
  induction n; intro s.
  { apply base_le. }
  change (W1 (S n) s) with (fold_right Rmax (base s) (map (Q (W1 n) s) (one_stakes s))).
  apply fold_ub; [apply base_le|]. intros x Hx. apply in_map_iff in Hx. destruct Hx as [st [<- _]].
  unfold Q.
  assert (A := IHn (after_win 8 s st)). assert (C := IHn (after_win 5 s st)).
  assert (D := IHn (after_loss s st)). lra.
Qed.

Lemma W1_step : forall n s, W1 n s <= W1 (S n) s.
Proof.
  induction n; intro s.
  - simpl. apply fold_lb_base.
  - change (W1 (S n) s) with (fold_right Rmax (base s) (map (Q (W1 n) s) (one_stakes s))).
    change (W1 (S (S n)) s) with (fold_right Rmax (base s) (map (Q (W1 (S n)) s) (one_stakes s))).
    apply fold_ub; [apply fold_lb_base|].
    intros x Hx. apply in_map_iff in Hx. destruct Hx as [st [<- Hst]].
    eapply Rle_trans; [|apply fold_lb_in, in_map, Hst].
    apply Q_mono; apply IHn.
Qed.

Lemma W1_mono : forall n m s, (n <= m)%nat -> W1 n s <= W1 m s.
Proof.
  intros n m s H. induction H; [lra|]. eapply Rle_trans; [apply IHle|apply W1_step].
Qed.

Definition L (s : State) : R :=
  proj1_sig (completeness (fun y => exists n, y = W1 n s)
    (ex_intro _ B (fun y Hy => match Hy with ex_intro _ n e =>
       eq_ind_r (fun z => z <= B) (W1_le_B n s) e end))
    (ex_intro _ (W1 0 s) (ex_intro _ 0%nat eq_refl))).

Lemma L_ub : forall n s, W1 n s <= L s.
Proof.
  intros n s. unfold L. destruct completeness as [m Hm]. simpl. destruct Hm as [Hub _]. apply Hub. eauto.
Qed.

Lemma L_close : forall s eps, 0 < eps -> exists n, L s - eps <= W1 n s.
Proof.
  intros s eps He. unfold L. destruct completeness as [m Hm]. simpl. destruct Hm as [_ Hlub].
  destruct (classic (exists n, m - eps <= W1 n s)) as [Hex|Hnot]; [exact Hex|exfalso].
  assert (Hu : forall y, (exists n, y = W1 n s) -> y <= m - eps).
  { intros y [n Hn]. rewrite Hn. destruct (Rle_or_lt (W1 n s) (m - eps)); auto.
    exfalso. apply Hnot. exists n. lra. }
  assert (m <= m - eps) by (apply Hlub; exact Hu). lra.
Qed.

Lemma L_excessive : Excessive L.
Proof.
  split.
  - intros s _. apply (L_ub 0 s).
  - intros s st Hv Hl Ho.
    destruct (Rle_or_lt (Q L s st) (L s)) as [H|H]; auto. exfalso.
    set (eps := (Q L s st - L s) / 2).
    assert (He : 0 < eps) by (unfold eps; lra).
    destruct (L_close (after_win 8 s st) eps He) as [n1 H1].
    destruct (L_close (after_win 5 s st) eps He) as [n2 H2].
    destruct (L_close (after_loss s st) eps He) as [n3 H3].
    set (N := (n1 + n2 + n3)%nat).
    assert (A := W1_mono n1 N (after_win 8 s st) ltac:(unfold N; lia)).
    assert (C := W1_mono n2 N (after_win 5 s st) ltac:(unfold N; lia)).
    assert (D := W1_mono n3 N (after_loss s st) ltac:(unfold N; lia)).
    assert (In (Q (W1 N) s st) (map (Q (W1 N) s) (one_stakes s))).
    { apply in_map. apply in_one. auto. }
    assert (F : Q (W1 N) s st <= W1 (S N) s).
    { change (W1 (S N) s) with (fold_right Rmax (base s) (map (Q (W1 N) s) (one_stakes s))).
      apply fold_lb_in. auto. }
    assert (Lu := L_ub (S N) s).
    unfold Q in *. unfold eps in *. lra.
Qed.

(** ** The result *)

(* stakes of 1 are a subset of all the stakes *)
Lemma one_le_any : forall n s, W one_stakes n s <= W any_stakes n s.
Proof.
  induction n; intro s; simpl; [lra|].
  apply fold_ub; [apply fold_lb_base|].
  intros x Hx. apply in_map_iff in Hx. destruct Hx as [st [<- Hst]].
  apply in_one in Hst. destruct Hst as [Hst _].
  eapply Rle_trans; [|apply fold_lb_in, in_map, in_any, Hst].
  apply Q_mono; apply IHn.
Qed.

(* Whatever the player can get in n rounds with any stakes can be approached as
   closely as wanted by only ever staking 1, given enough rounds. *)
Theorem stake_one_optimal : forall n s eps, valid s -> 0 < eps ->
  exists m, W any_stakes n s <= W one_stakes m s + eps.
Proof.
  intros n s eps Hv He.
  destruct (L_close s eps He) as [m Hm]. exists m.
  assert (A := any_stake_le_excessive L L_excessive n s Hv).
  unfold W1 in Hm. lra.
Qed.

End Game.

Print Assumptions stake_one_optimal.
