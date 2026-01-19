Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import ZArith.
From Stdlib Require Import Lia.

Definition hits_from_ratio (z: Z) :=
    if (z >? 10)%Z then [ (* -6 *) (0, 100); (0, 100); (0, 8); (0, 8); (1, 7); (2, 6); (3, 5); (4, 4); (5, 3); (6, 0) ]
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

Lemma hits_damage: Forall (Forall (fun pr : (nat * nat) => let (a, b) := pr in a > 0 \/ b > 0)) HITSCHART.
Proof.
    repeat (apply Forall_cons); try (apply Forall_nil); try Lia.lia.
Qed.