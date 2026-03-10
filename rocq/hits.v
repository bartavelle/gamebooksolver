Require Import Stdlib.Lists.List.
Import ListNotations.
From Stdlib Require Import ZArith.
From Stdlib Require Import Lia.

Definition hits_from_ratio (z: Z) :=
    if (z <? -10)%Z then [ (* -6 *) (0, 100); (0, 100); (0, 8); (0, 8); (1, 7); (2, 6); (3, 5); (4, 4); (5, 3); (6, 0) ]
    else
     match z with
     | (-10)%Z | (-9)%Z => [ (* -5 *) (0, 100); (0, 8); (0, 7); (1, 7); (2, 6); (3, 6); (4, 5); (5, 4); (6, 3); (7, 0) ]
     | (-8)%Z | (-7)%Z => [ (* -4 *) (0, 8); (0, 7); (1, 6); (2, 6); (3, 5); (4, 5); (5, 4); (6, 3); (7, 2); (8, 0) ]
     | (-6)%Z | (-5)%Z => [ (0, 6); (1, 6); (2, 5); (3, 5); (4, 4); (5, 4); (6, 3); (7, 2); (8, 0); (9, 0) ]
     | (-4)%Z | (-3)%Z => [ (1, 6); (2, 5); (3, 5); (4, 4); (5, 4); (6, 3); (7, 2); (8, 1); (9, 0); (10, 0) ]
     | (-2)%Z | (-1)%Z => [ (2, 5); (3, 5); (4, 4); (5, 4); (6, 3); (7, 2); (8, 2); (9, 1); (10, 0); (11, 0) ]
     | (0)%Z => [ (3, 5); (4, 4); (5, 4); (6, 3); (7, 2); (8, 2); (10, 1); (10, 0); (11, 0); (12, 0) ]
     | (1)%Z | 2%Z => [ (4, 5); (5, 4); (6, 3); (7, 3); (8, 2); (9, 2); (11, 1); (11, 0); (12, 0); (14, 0) ]
     | (3)%Z | 4%Z => [ (5, 4); (6, 3); (7, 3); (8, 2); (9, 2); (10, 2); (12, 1); (12, 0); (14, 0); (16, 0) ]
     | (5)%Z | 6%Z => [ (6, 4); (7, 3); (8, 3); (9, 2); (10, 2); (11, 1); (14, 0); (14, 0); (16, 0); (18, 0) ]
     | (7)%Z | 8%Z => [ (7, 4); (8, 3); (9, 2); (10, 2); (11, 2); (12, 1); (14, 0); (16, 0); (18, 0); (100, 0) ]
     | (9)%Z | 10%Z => [ (8, 3); (9, 3); (10, 2); (11, 2); (12, 2); (14, 1); (16, 0); (18, 0); (100, 0); (100, 0) ]
     | _ => [ (9, 3); (10, 2); (11, 2); (12, 2); (14, 1); (16, 1); (18, 0); (100, 0); (100, 0); (100, 0) ]
     end.

Module HFR.
  Example hm11: hits_from_ratio 11 = 
     [ (9, 3); (10, 2); (11, 2); (12, 2); (14, 1); (16, 1); (18, 0); (100, 0); (100, 0); (100, 0) ].
  Proof.
    vm_compute. reflexivity.
  Qed.
  Example hm10: hits_from_ratio 10 = 
     [ (8, 3); (9, 3); (10, 2); (11, 2); (12, 2); (14, 1); (16, 0); (18, 0); (100, 0); (100, 0) ].
  Proof.
    vm_compute. reflexivity.
  Qed.
End HFR.

Definition HITSCHART: list (list (nat * nat)) := [
    [ (* -6 *) (0, 100); (0, 100); (0, 8); (0, 8); (1, 7); (2, 6); (3, 5); (4, 4); (5, 3); (6, 0) ];
    [ (* -5 *) (0, 100); (0, 8); (0, 7); (1, 7); (2, 6); (3, 6); (4, 5); (5, 4); (6, 3); (7, 0) ];
    [ (* -4 *) (0, 8); (0, 7); (1, 6); (2, 6); (3, 5); (4, 5); (5, 4); (6, 3); (7, 2); (8, 0) ];
    [ (* -3 *) (0, 6); (1, 6); (2, 5); (3, 5); (4, 4); (5, 4); (6, 3); (7, 2); (8, 0); (9, 0) ];
    [ (* -2 *) (1, 6); (2, 5); (3, 5); (4, 4); (5, 4); (6, 3); (7, 2); (8, 1); (9, 0); (10, 0) ];
    [ (* -1 *) (2, 5); (3, 5); (4, 4); (5, 4); (6, 3); (7, 2); (8, 2); (9, 1); (10, 0); (11, 0) ];
    [ (* 0 *) (3, 5); (4, 4); (5, 4); (6, 3); (7, 2); (8, 2); (10, 1); (10, 0); (11, 0); (12, 0) ];
    [ (* 1 *) (4, 5); (5, 4); (6, 3); (7, 3); (8, 2); (9, 2); (11, 1); (11, 0); (12, 0); (14, 0) ];
    [ (* 2 *) (5, 4); (6, 3); (7, 3); (8, 2); (9, 2); (10, 2); (12, 1); (12, 0); (14, 0); (16, 0) ];
    [ (* 3 *) (6, 4); (7, 3); (8, 3); (9, 2); (10, 2); (11, 1); (14, 0); (14, 0); (16, 0); (18, 0) ];
    [ (* 4 *) (7, 4); (8, 3); (9, 2); (10, 2); (11, 2); (12, 1); (14, 0); (16, 0); (18, 0); (100, 0) ];
    [ (* 5 *) (8, 3); (9, 3); (10, 2); (11, 2); (12, 2); (14, 1); (16, 0); (18, 0); (100, 0); (100, 0) ];
    [ (* 6 *) (9, 3); (10, 2); (11, 2); (12, 2); (14, 1); (16, 1); (18, 0); (100, 0); (100, 0); (100, 0) ]
].

Inductive i_hits: Z 
                    -> (nat * nat)
                    -> (nat * nat)
                    -> (nat * nat)
                    -> (nat * nat)
                    -> (nat * nat)
                    -> (nat * nat)
                    -> (nat * nat)
                    -> (nat * nat)
                    -> (nat * nat)
                    -> (nat * nat)
                    -> Prop :=
    | hm6: forall (r:Z), (r < (-10))%Z ->
       i_hits r (0, 100) (0, 100) (0, 8) (0, 8) (1, 7) (2, 6) (3, 5) (4, 4) (5, 3) (6, 0) 
    | hm5: forall r, r = (-10)%Z \/ r = (-9)%Z ->
       i_hits r (0, 100) (0, 8) (0, 7) (1, 7) (2, 6) (3, 6) (4, 5) (5, 4) (6, 3) (7, 0)
    | hm4: forall r, r = (-8)%Z \/ r = (-7)%Z ->
       i_hits r (0, 8) (0, 7) (1, 6) (2, 6) (3, 5) (4, 5) (5, 4) (6, 3) (7, 2) (8, 0)
    | hm3: forall r, r = (-6)%Z \/ r = (-5)%Z ->
       i_hits r (0, 6) (1, 6) (2, 5) (3, 5) (4, 4) (5, 4) (6, 3) (7, 2) (8, 0) (9, 0)
    | hm2: forall r, r = (-4)%Z \/ r = (-3)%Z ->
       i_hits r (1, 6) (2, 5) (3, 5) (4, 4) (5, 4) (6, 3) (7, 2) (8, 1) (9, 0) (10, 0)
    | hm1: forall r, r = (-2)%Z \/ r = (-1)%Z ->
       i_hits r (2, 5) (3, 5) (4, 4) (5, 4) (6, 3) (7, 2) (8, 2) (9, 1) (10, 0) (11, 0)
    | h00: i_hits 0 (3, 5) (4, 4) (5, 4) (6, 3) (7, 2) (8, 2) (10, 1) (10, 0) (11, 0) (12, 0)
    | hp1: forall r, r = 1%Z \/ r = 2%Z ->
           i_hits r (4, 5) (5, 4) (6, 3) (7, 3) (8, 2) (9, 2) (11, 1) (11, 0) (12, 0) (14, 0)
    | hp2: forall r, r = 3%Z \/ r = 4%Z ->
           i_hits r (5, 4) (6, 3) (7, 3) (8, 2) (9, 2) (10, 2) (12, 1) (12, 0) (14, 0) (16, 0)
    | hp3: forall r, r = 5%Z \/ r = 6%Z ->
           i_hits r (6, 4) (7, 3) (8, 3) (9, 2) (10, 2) (11, 1) (14, 0) (14, 0) (16, 0) (18, 0)
    | hp4: forall r, r = 7%Z \/ r = 8%Z ->
           i_hits r (7, 4) (8, 3) (9, 2) (10, 2) (11, 2) (12, 1) (14, 0) (16, 0) (18, 0) (100, 0)
    | hp5: forall r, r = 9%Z \/ r = 10%Z ->
           i_hits r (8, 3) (9, 3) (10, 2) (11, 2) (12, 2) (14, 1) (16, 0) (18, 0) (100, 0) (100, 0)
    | hp6: forall r, (r > 10)%Z ->
        i_hits r (9, 3) (10, 2) (11, 2) (12, 2) (14, 1) (16, 1) (18, 0) (100, 0) (100, 0) (100, 0)
    .

Lemma z_ranges: forall (z: Z),
        (z > 10 \/ z = 10 \/ z = 9 \/ z = 8 \/ z = 7 \/ z = 6 \/
                  z = 5 \/ z = 4 \/ z = 3 \/ z = 2 \/ z = 1 \/
                  z = 0 \/ z = (-1) \/ z = (-2) \/ z = (-3) \/ z = (-4) \/
                  z = (-5) \/ z = (-6) \/ z = (-7) \/ z = (-8) \/ z = (-9) \/
                  z = (-10) \/ z < (-10) )%Z.
Proof.
            Lia.lia.
Qed.

Lemma hits_from_ratio_content: forall (z: Z) hr, hr = hits_from_ratio z ->
            In hr HITSCHART.
Proof.
    intros.
    subst.
    unfold hits_from_ratio.

    destruct z; simpl; try tauto.

    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
    destruct p; simpl; try tauto.
Qed.

Lemma hits_from_ratio_always_10: forall (z: Z), length (hits_from_ratio z) = 10.
Proof.
    intros.
    remember (hits_from_ratio z) as hr.
    pose proof (hits_from_ratio_content z hr Heqhr).
    simpl in H.
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    destruct H; try (rewrite <- H; tauto).
    contradiction.
Qed.

Definition HasDamage (pr: nat * nat): Prop := let (a, b) := pr in a > 0 \/ b > 0.

Lemma hits_damage: Forall (Forall HasDamage) HITSCHART.
Proof.
    unfold HasDamage.
    repeat (apply Forall_cons); try (apply Forall_nil); try Lia.lia.
Qed.

Lemma i_hits_eq: forall sk a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9,
    i_hits sk a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 ->
    i_hits sk b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ->
    a0 = b0 /\ a1 = b1 /\ a2 = b2 /\
    a3 = b3 /\ a4 = b4 /\ a5 = b5 /\
    a6 = b6 /\ a7 = b7 /\ a8 = b8 /\
    a9 = b9.
Proof.
    intros.
    inversion H; inversion H0; subst; clear H H0; try Lia.lia; try tauto.
Qed.

Lemma ihits_correct: forall z res,
    hits_from_ratio z = res ->
        exists r0 r1 r2 r3 r4 r5 r6 r7 r8 r9,
            res = [r0; r1; r2; r3; r4; r5; r6; r7; r8 ;r9] /\
            i_hits z r0 r1 r2 r3 r4 r5 r6 r7 r8 r9.
Proof.
    intros.
    subst.
    unfold hits_from_ratio.

    Ltac __ihi := match goal with
    | H : (_ <? _)%Z = true |- _ => apply Z.ltb_lt in H
    | H : (_ <? _)%Z = false |- _ => apply Z.ltb_ge in H
    | |- exists _ _ _ _ _ _ _ _ _ _, 
        [_; _; _; _; _; _; _; _; _; _] = [_; _; _; _; _; _; _; _; _; _] /\ 
        i_hits _ _ _ _ _ _ _ _ _ _ _ => repeat eexists
    | |- i_hits _ _ _ _ _ _ _ _ _ _ _ => constructor
    | z: Z |- context [match ?z with _ => _ end] => destruct z eqn:?
    | p: positive |- context [match ?p with _ => _ end] => destruct p eqn:?
    end.

    destruct z; simpl; repeat __ihi; try tauto; try Lia.lia.
Qed.

Lemma ihits_damages: forall z h0 h1 h2 h3 h4 h5 h6 h7 h8 h9,
    i_hits z h0 h1 h2 h3 h4 h5 h6 h7 h8 h9 ->
    HasDamage h0
        /\ HasDamage h1
        /\ HasDamage h2
        /\ HasDamage h3
        /\ HasDamage h4
        /\ HasDamage h5
        /\ HasDamage h6
        /\ HasDamage h7
        /\ HasDamage h8
        /\ HasDamage h9.
Proof.
    intros.
    unfold HasDamage.
    inversion H; subst; clear H; Lia.lia.
Qed.
