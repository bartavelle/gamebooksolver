Require Import chapters.
Require Import mps.
Require Import proba.

Require Import Nat.
Require Import Stdlib.Lists.List.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
Require Import ZArith.
Open Scope nat_scope.

Record Stt : Set := mkStt
  { book: nat
  ; maxendurance : endurance
  ; curendurance : endurance
  ; sk : skill
  ; disciplines : DisciplineSet
  ; chapter : cid
  ; items : Items
  ; flags : FlagSet
  ; previtems : Items
  }.

Definition build_test_stt (e: nat) (sk: nat) (disc: list discipline) (items: list item) (flags: list flag): Stt
  := mkStt 1 e e (Z.of_nat sk) (set_from_list disc) 1 (from_list (map (fun i => (i, 1)) items)) (set_from_list flags) nil.

Module SH.
  Lemma ds_refl {K V: Set} `{ok: OrdDec K} `{ov: OrdDec V}: forall (a: Mp K V), ListLemma.list_cmp a a = EQ.
  Proof.
    induction a; intros; auto.
    simpl. unfold PairLemma.pair_cmp.
    destruct a as [k v].
    rewrite cmp_refl.
    rewrite cmp_refl.
    auto.
  Qed.

  Lemma rw: forall a b, NatLemma.nat_cmp a b = @cmp nat nat_EqDec nat_OrdDec a b.
  Proof.
    intros.
    unfold cmp. simpl. auto.
  Qed.
  Lemma rwz: forall a b, IntLemma.int_cmp a b = @cmp Z int_EqDec int_OrdDec a b.
  Proof.
    intros.
    unfold cmp. simpl. auto.
  Qed.
  Lemma rwm {A} `{OrdDec A} {B} `{OrdDec B}: forall a b, ListLemma.list_cmp a b = @cmp (list (A * B)) _ _ a b.
  Proof.
    intros.
    unfold cmp. simpl. auto.
  Qed.

  Definition chain (a b: Order) :=
    match a with
    | EQ => b
    | x => x
    end.

  Lemma chain_cmp: forall a b r, r <> EQ -> chain a b = r <-> a = r \/ a = EQ /\ b = r.
  Proof.
    intros a b r R.
    destruct r; try contradiction;
    destruct a, b; simpl; split; intros; try discriminate; try tauto; try destruct H as [A|[B C]]; subst; auto; try discriminate.
  Qed.

  Lemma chain_eq: forall a b, chain a b = EQ <-> a = EQ /\ b = EQ.
  Proof.
    intros.
    destruct a, b; split; intros; simpl in *; auto; try discriminate; destruct H as [X Y]; discriminate.
  Qed.

  Lemma chain_lt: forall a b, chain a b = LT <-> a = LT \/ a = EQ /\ b = LT.
  Proof.
    intros.
    apply chain_cmp. intro contra. discriminate.
  Qed.

  Lemma chain_gt: forall a b, chain a b = GT <-> a = GT \/ a = EQ /\ b = GT.
  Proof.
    intros.
    apply chain_cmp. intro contra. discriminate.
  Qed.

  Ltac run :=
    match goal with
    | |- chain LT _ = _ => simpl
    | |- chain EQ _ = _ => simpl
    | |- chain GT _ = _ => simpl
    | H: chain (NatLemma.nat_cmp _ _) _ = _ |- _ => rewrite rw in H
    | |- chain (NatLemma.nat_cmp _ _) _ = _ => rewrite rw
    | H: chain (IntLemma.int_cmp _ _) _ = _ |- _ => rewrite rwz in H
    | |- chain (IntLemma.int_cmp _ _) _ = _ => rewrite rwz
    | H: chain (ListLemma.list_cmp _ _) _ = _ |- _ => rewrite rwm in H
    | |- chain (ListLemma.list_cmp _ _) _ = _ => rewrite rwm
    | H: ListLemma.list_cmp _ _ = _ |- _ => rewrite rwm in H
    | |- ListLemma.list_cmp _ _ = _ => rewrite rwm
    | |- chain (cmp ?a ?a) _ = _ => rewrite cmp_refl
    | H: chain (cmp _ _) _ = LT |- _ => apply chain_lt in H
    | H: chain (cmp _ _) _ = GT |- _ => apply chain_gt in H
    | H: cmp ?a ?b = EQ |- _ => apply cmp_eq in H; subst
    | H: cmp ?a ?b = _ |- chain (cmp ?b ?a) _ = _ => apply cmp_opp in H
    | H: cmp ?a ?b = _ |- chain (cmp ?a ?b) _ = _ => rewrite H
    | H1: cmp ?a ?b = ?r, H2: cmp ?b ?c = ?r |- chain (cmp ?a ?c) _ = ?r =>
            pose proof (cmp_trans _ _ _ _ H1 H2)
    | H: _ \/ _ |- _ => destruct H
    | H: _ /\ _ |- _ => destruct H
    | H: chain _ _ = EQ |- _ => apply chain_eq in H
    end.
End SH.
      
Module Stt.
  Definition stt_beq (s1: Stt) (s2: Stt) : bool :=
    mps.eqb (book s1) (book s2)
    && mps.eqb (maxendurance s1) (maxendurance s2)
    && mps.eqb (curendurance s1) (curendurance s2)
    && mps.eqb (sk s1) (sk s2)
    && mps.eqb (disciplines s1) (disciplines s2)
    && mps.eqb (chapter s1) (chapter s2)
    && mps.eqb (items s1) (items s2)
    && mps.eqb (flags s1) (flags s2)
    && mps.eqb (previtems s1) (previtems s2)
    .

  Lemma stt_beq_correct : forall (x y: Stt), x = y <-> stt_beq x y = true.
  Proof.
    intros x y. split; intro Heq.
    * subst. unfold stt_beq.
      repeat (apply andb_true_intro; split); try (rewrite <- eqb_correct); auto.
    * unfold stt_beq in Heq.
      apply andb_prop in Heq.
      destruct Heq.
      apply andb_prop in H.
      destruct H.
      apply andb_prop in H.
      destruct H.
      apply andb_prop in H.
      destruct H.
      apply andb_prop in H.
      destruct H.
      apply andb_prop in H.
      destruct H.
      apply andb_prop in H.
      destruct H.
      apply andb_prop in H.
      destruct H.
      rewrite <- eqb_correct in H.
      rewrite <- eqb_correct in H0.
      rewrite <- eqb_correct in H1.
      rewrite <- eqb_correct in H2.
      rewrite <- eqb_correct in H3.
      rewrite <- eqb_correct in H4.
      rewrite <- eqb_correct in H5.
      rewrite <- eqb_correct in H6.
      rewrite <- eqb_correct in H7.
      destruct x, y.
      simpl in *.
      subst.
      auto.
  Qed.

  Definition stt_cmp (s1 s2: Stt) :=
    SH.chain (cmp (book s1) (book s2))
    (SH.chain (cmp (maxendurance s1) (maxendurance s2)) 
    (SH.chain (cmp (curendurance s1) (curendurance s2))
    (SH.chain (cmp (sk s1) (sk s2))
    (SH.chain (cmp (disciplines s1) (disciplines s2))
    (SH.chain (cmp (chapter s1) (chapter s2))
    (SH.chain (cmp (items s1) (items s2))
    (SH.chain (cmp (flags s1) (flags s2))
    (cmp (previtems s1) (previtems s2)
    )))))))).

  Definition lt (s1 s2: Stt) := stt_cmp s1 s2 = LT.

  Lemma cmp_correct: forall (a b: Stt), stt_cmp a b = LT <-> lt a b.
  Proof.
    intros. split; auto.
  Qed.

  Lemma cmp_opp: forall (a b: Stt), stt_cmp a b = LT <-> stt_cmp b a = GT.
  Proof.
    intros.
    unfold stt_cmp.
    destruct a, b; simpl.
    split; intros;
    repeat SH.run; try reflexivity.
    apply cmp_opp. auto.
    apply cmp_opp. auto.
  Qed.


  Lemma cmp_trans: forall r a b c, stt_cmp a b = r -> stt_cmp b c = r -> stt_cmp a c = r.
  Proof.
    destruct a, b, c; simpl; unfold stt_cmp. simpl.
    destruct r; intros; subst; auto; repeat SH.run; try reflexivity.
    eapply cmp_trans; eauto.
    apply cmp_refl.
    eapply cmp_trans; eauto.
  Qed.


  Lemma cmp_eq: forall a b, stt_cmp a b = EQ <-> a = b.
  Proof.
    unfold stt_cmp.
    split; intros; subst; auto; repeat SH.run; try reflexivity.
    {
      destruct a, b; simpl in *. subst. auto.
    }
    apply cmp_refl.
  Qed.
End Stt.

Instance stt_EqDec: EqDec Stt := {
  eqb := Stt.stt_beq;
  eqb_correct := Stt.stt_beq_correct;
}.

Instance stt_OrdDec: OrdDec Stt stt_EqDec :=
{
  cmp := Stt.stt_cmp;
  lt := Stt.lt;
  cmp_correct := Stt.cmp_correct;
  cmp_opp := Stt.cmp_opp;
  cmp_trans := Stt.cmp_trans;
  cmp_eq := Stt.cmp_eq;
}.
  

Definition update_endurance (f: endurance -> endurance) (stt: Stt) : Stt :=
       {| book := book stt
       ; maxendurance := maxendurance stt
       ; curendurance := f (curendurance stt)
       ; sk := sk stt
       ; disciplines := disciplines stt
       ; chapter := chapter stt
       ; items := items stt
       ; flags := flags stt
       ; previtems := previtems stt
       |}.

Definition update_items (f: Items -> Items) (stt: Stt): Stt :=
       {| book := book stt
       ; maxendurance := maxendurance stt
       ; curendurance := curendurance stt
       ; sk := sk stt
       ; disciplines := disciplines stt
       ; chapter := chapter stt
       ; items := f (items stt)
       ; flags := flags stt
       ; previtems := previtems stt
       |}.

Definition update_flags (f: FlagSet -> FlagSet) (stt: Stt) : Stt :=
       {| book := book stt
       ; maxendurance := maxendurance stt
       ; curendurance := curendurance stt
       ; sk := sk stt
       ; disciplines := disciplines stt
       ; chapter := chapter stt
       ; items := items stt
       ; flags := f (flags stt)
       ; previtems := previtems stt
       |}.

Definition update_chapter (cid: nat) (stt: Stt) : Stt :=
       {| book := book stt
       ; maxendurance := maxendurance stt
       ; curendurance := curendurance stt
       ; sk := sk stt
       ; disciplines := disciplines stt
       ; chapter := cid
       ; items := items stt
       ; flags := flags stt
       ; previtems := previtems stt
       |}.

Definition sitems (i: item) (stt: Stt): nat :=
    match lookup i (items stt) with
    | None => 0
    | Some x => x
    end.

Definition has_item (i: item) (stt: Stt) : bool :=
    match lookup i (items stt) with
    | None | Some 0 => false
    | _ => true
    end.

Definition has_flag (f: flag) (stt: Stt) : bool :=
    s_check f (flags stt).

Definition max_hp (stt: Stt) : nat :=
  maxendurance stt + (if has_item BodyArmor stt then 4 else 0)
    + (if has_item Helmet stt then 2 else 0).

Definition heal (stt: Stt) (q: nat) : Stt :=
  update_endurance (fun e => Nat.min (max_hp stt) (curendurance stt + q)) stt.

Definition level (stt: Stt): kai_level :=
  let amnt := length (filter (fun d => s_check d (disciplines stt)) all_disciplines)
   in nat_to_level amnt.

Definition all_character_items (s: Stt): list (item * nat) := items s.

Definition all_character_items_slot (s: Stt) (sl: slot): list (item * nat) :=
  map (fun i => (i, sitems i  s)) (slot_items sl).

Definition all_character_items_slot_count (s: Stt) (sl: slot): nat :=
  list_sum (map (fun i => sitems i s) (slot_items sl)).

Definition OneWeaponSpecMax (s: Stt) : Prop :=
  let discs := disciplines s in
   forall w1 w2,
      s_check (WeaponSkill w1) discs = true ->
      s_check (WeaponSkill w2) discs = true ->
      w1 <> w2.

Definition ValidState (s: Stt) :=
           lt 0 (curendurance s)
        /\ lt (book s) 6
        /\ lt 0 (book s)
        /\ lt 4 (level_to_nat (level s))
        /\ lt (sitems Gold s) 51
        /\ le (all_character_items_slot_count s SBackpack) 8
        /\ le (all_character_items_slot_count s SWeapon) 2
        /\ ValidMap (items s)
        /\ ValidMap (disciplines s)
        /\ ValidMap (flags s)
        /\ ValidMap (previtems s)
        /\ OneWeaponSpecMax s
        .