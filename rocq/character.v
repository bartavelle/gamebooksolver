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

Definition has_item (i: item) (stt: Stt) : bool := has i (items stt).

Definition has_flag (f: flag) (stt: Stt) : bool :=
    s_check f (flags stt).

Definition max_hp (stt: Stt) : nat :=
  maxendurance stt + (if has_item BodyArmor stt then 4 else 0)
    + (if has_item Helmet stt then 2 else 0).

Definition c_rm_items (i: item) (q: nat) (stt: Stt): Stt :=
  let nstt := update_items (rm_item i q) stt in
      update_endurance (fun cur => Nat.min cur (max_hp nstt)) nstt.

Definition heal (stt: Stt) (q: nat) : Stt :=
  update_endurance (fun e => Nat.min (max_hp stt) (curendurance stt + q)) stt.

Definition level (stt: Stt): kai_level :=
  let amnt := length (filter (fun d => s_check d (disciplines stt)) all_disciplines)
   in nat_to_level amnt.

Definition all_character_items (s: Stt): list (item * nat) := items s.

Definition all_character_items_slot (s: Stt) (sl: slot): list (item * nat) :=
  filter (fun kv: item * nat => mps.eqb (item_slot (fst kv)) sl) (items s).

Definition all_character_items_slot_count (s: Stt) (sl: slot): nat :=
  list_sum (map snd (all_character_items_slot s sl)).

Module Slot.
  Lemma valid: forall sl stt, ValidMap (items stt) -> ValidMap (all_character_items_slot stt sl).
  Proof.
    intros.
    unfold all_character_items_slot.
    apply Valid.filter.
    assumption.
  Qed.

  Lemma update_add0: forall stt i, update_items (add_item i 0) stt = stt.
  Proof.
    intros.
    unfold add_item.
    simpl.
    unfold update_items.
    destruct stt.
    reflexivity.
  Qed.

  Lemma increase: forall stt i q sl, ValidMap (items stt) -> sl = item_slot i -> i <> Gold -> all_character_items_slot_count (update_items (add_item i q) stt) sl
          = all_character_items_slot_count stt sl + q.
  Proof.
    intros.
    destruct q; try (rewrite update_add0; Lia.lia).
    destruct stt. unfold all_character_items_slot_count, update_items, sitems.
    unfold all_character_items_slot.
    Arguments mps.eqb : simpl never.
    simpl.
    rewrite add_item_nogold; auto.
    subst.
    simpl in H.
    clear book0 maxendurance0 curendurance0 sk0 disciplines0 chapter0 flags0 previtems0.
    induction items0; simpl.
    * rewrite mps.eqb_refl.
      simpl. Lia.lia.
    * destruct a.
      simpl.
      replace (Helpers.compare_with_list all_items i i0) with (mps.cmp i i0) by auto.
      destruct (mps.cmp i i0) eqn: II0; simpl.
      + rewrite mps.eqb_refl.
        destruct (mps.eq_dec (item_slot i0) (item_slot i)).
        - rewrite e.
          rewrite mps.eqb_refl; simpl. Lia.lia.
        - apply mps.eqb_not in n0.
          rewrite n0; simpl.
          Lia.lia.
      + apply cmp_eq in II0.
        subst.
        rewrite mps.eq_refl; simpl.
        Lia.lia.
      + destruct (mps.eq_dec (item_slot i0) (item_slot i)).
        - rewrite e.
          rewrite mps.eqb_refl; simpl.
          rewrite IHitems0.
          Lia.lia.
          apply Valid.head in H.
          tauto.
        - apply mps.eqb_not in n0.
          rewrite n0.
          rewrite IHitems0.
          Lia.lia.
          apply Valid.head in H.
          tauto.
    * Lia.lia.
  Qed.

  Lemma no_increase: forall stt i q sl, ValidBag (items stt) -> sl <> item_slot i -> all_character_items_slot_count (update_items (add_item i q) stt) sl
          = all_character_items_slot_count stt sl.
  Proof.
    intros.
    destruct q; try (rewrite update_add0; Lia.lia).
    unfold all_character_items_slot_count.
    unfold all_character_items_slot.
    f_equal.
    f_equal.
    unfold update_items.
    destruct stt.
    simpl in *.
    clear book0 maxendurance0 curendurance0 sk0 disciplines0 chapter0 flags0 previtems0.
    Arguments mps.eqb : simpl never.
    Arguments mps.cmp : simpl never.
    unfold add_item, bag_add.
    remember (fun kv : item * nat => mps.eqb (item_slot (fst kv)) sl) as pred.
    assert (forall f, filter pred (insert_with f i (S q) items0) = filter pred items0). {
      intros.
      subst.
      apply not_eq_sym in H0.
      apply eqb_not in H0.
      induction items0; simpl; try (rewrite H0); simpl; auto.
      destruct a as [ck cv].
      destruct H. apply Valid.head in H. inversion H1; subst; clear H1.
      destruct (cmp i ck) eqn: ICK; simpl; destruct (mps.eq_dec (item_slot ck) sl); subst; try apply mps.eqb_refl; simpl;
        try rewrite H0; auto; try rewrite mps.eq_refl; repeat inequalities.
      * rewrite mps.eq_refl in H0. discriminate.
      * apply eqb_not in n. rewrite n. reflexivity.
      * f_equal.
        apply IHitems0.
        unfold ValidBag.
        tauto.
      * apply eqb_not in n. rewrite n. apply IHitems0.
        unfold ValidBag.
        tauto.
    }
    simpl.
    destruct i; apply H1.
  Qed.

  Lemma list_sum_add {A: Type} `{EqDec A}: forall lst (i: A) (f: A -> nat),
    In i lst -> NoDup lst ->
    list_sum (map (fun x => if mps.eqb x i then f x + 1 else f x) lst) = list_sum (map f lst) + 1.
  Proof.
    induction lst; intros; simpl.
    * contradiction.
    * inversion H0; subst; clear H0.
      + rewrite mps.eqb_refl.
        inversion H1; subst; clear H1.
        replace (list_sum (map (fun x : A => if mps.eqb x i then f x + 1 else f x) lst)) with (list_sum (map f lst)).
        Lia.lia.
        f_equal.
        apply map_ext_in.
        intros.
        destruct (mps.eqb a i) eqn: EAI; auto.
        apply eqb_correct in EAI. subst. contradiction.
      + inversion H1; subst; clear H1.
        destruct (eq_dec a i).
        - subst. contradiction.
        - replace (mps.eqb a i) with false. 2: {
            symmetry.
            apply eqb_not.
            assumption.
          }
          rewrite IHlst; auto. Lia.lia.
  Qed.

  Lemma list_sum_sub {A: Type} `{EqDec A}: forall lst (drp: A) (f: A -> nat),
    In drp lst -> NoDup lst -> f drp > 0 ->
    list_sum (map (fun x => if mps.eqb x drp then f x - 1 else f x) lst) + 1 = list_sum (map f lst).
  Proof.
    induction lst; intros; simpl.
    * contradiction.
    * inversion H0; subst; clear H0.
      + rewrite mps.eqb_refl.
        inversion H1; subst; clear H1.
        replace (list_sum (map (fun x : A => if mps.eqb x drp then f x - 1 else f x) lst)) with (list_sum (map f lst)).
        Lia.lia.
        f_equal.
        apply map_ext_in.
        intros.
        destruct (mps.eqb a drp) eqn: EAI; auto.
        apply eqb_correct in EAI. subst. contradiction.
      + inversion H1; subst; clear H1.
        destruct (eq_dec a drp).
        - subst. contradiction.
        - replace (mps.eqb a drp) with false. 2: {
            symmetry.
            apply eqb_not.
            assumption.
          }
          rewrite <- Nat.add_assoc.
          rewrite IHlst; auto.
  Qed.

  Lemma nodup_slot_items: forall s, NoDup (slot_items s).
  Proof.
    intros.
    destruct s; simpl; repeat constructor; intro C; repeat destruct C as [C|C]; try discriminate; inversion C.
  Qed.

  Lemma in_slot_items_slot: forall x s, In x (slot_items s) -> item_slot x = s.
  Proof.
    intros x s. destruct s; destruct x; simpl in *; intros; repeat (destruct H as [H|H]; try discriminate);
      try (destruct w; try discriminate); auto; contradiction.
  Qed.

  Lemma sum_filter_insert_1: forall (m: Mp item nat) (i: item) (s: slot),
  list_sum (map snd (filter (fun kv => mps.eqb (item_slot (fst kv)) s) (insert_with (fun pq => pq + 1) i 1 m))) =
  list_sum (map snd (filter (fun kv => mps.eqb (item_slot (fst kv)) s) m)) + (if mps.eqb (item_slot i) s then 1 else 0).
  Proof.
    induction m as [| [ck cv] m' IH]; intros i s; simpl.
    - destruct (mps.eqb (item_slot i) s); simpl; Lia.lia.
    - destruct (cmp i ck) eqn:Hc; simpl.
      + destruct (mps.eqb (item_slot i) s); destruct (mps.eqb (item_slot ck) s); simpl; Lia.lia.
      + apply cmp_eq in Hc; subst.
        destruct (mps.eqb (item_slot ck) s); simpl; Lia.lia.
      + destruct (mps.eqb (item_slot ck) s); simpl.
        * rewrite IH. Lia.lia.
        * rewrite IH. reflexivity.
  Qed.

  Lemma sum_filter_rm_1: forall (m: Mp item nat) (drp: item) (s: slot),
    match lookup drp m with Some x => x | None => 0 end > 0 ->
    list_sum (map snd (filter (fun kv => mps.eqb (item_slot (fst kv)) s) m)) =
    list_sum (map snd (filter (fun kv => mps.eqb (item_slot (fst kv)) s) (rm_item drp 1 m))) + (if mps.eqb (item_slot drp) s then 1 else 0).
  Proof.
    induction m as [| [ck cv] m' IH]; intros drp s Hgt; simpl in *.
    - Lia.lia.
    - unfold rm_item in *; simpl in *.
      destruct (cmp drp ck) eqn:Hc; simpl in *.
      + Lia.lia. (* lookup returns None, contradicting Hgt > 0 *)
      + apply cmp_eq in Hc; subst.
        destruct (cv - 1 =? 0) eqn:Hz.
        * apply Nat.eqb_eq in Hz. 
          unfold bag_rm.
          simpl.
          rewrite cmp_refl.
          replace (cv <=? 1) with true.
          destruct (mps.eqb (item_slot ck) s) eqn:SLE; simpl; Lia.lia.
          symmetry.
          apply Nat.leb_le.
          Lia.lia.
        * unfold bag_rm.
          apply Nat.eqb_neq in Hz.
          destruct (mps.eqb (item_slot ck) s) eqn: SCK; simpl; rewrite cmp_refl; replace (cv <=? 1) with false; simpl; try rewrite SCK;
            simpl; try Lia.lia; symmetry; apply Nat.leb_gt; Lia.lia.
      + specialize (IH drp s Hgt).
        unfold bag_rm in *; simpl in *.
        rewrite Hc; simpl.
        destruct (mps.eqb (item_slot ck) s) eqn: SL; simpl in *; destruct (mps.eqb (item_slot drp) s); simpl in *; Lia.lia.
  Qed.

  Lemma add_rm: forall stt i drp,
    ValidBag (items stt) ->
    i <> drp ->
    item_slot i = item_slot drp ->
    sitems drp stt > 0 ->
    let nstt := update_items (fun ns => add_item i 1 (rm_item drp 1 ns)) stt in
    forall s,
    all_character_items_slot_count nstt s = all_character_items_slot_count stt s.
  Proof.
    intros stt i drp VM Hneq Hslot Hgt nstt s.

    (* Rule out Gold edge-cases since item_slot matches but they aren't equal *)
    assert (i <> Gold /\ drp <> Gold).
    {
      split; intro C; subst; simpl in Hslot.
      - destruct drp; simpl in Hslot; try discriminate. apply Hneq. reflexivity.
      - destruct i; simpl in Hslot; try discriminate. apply Hneq. reflexivity.
    }
    destruct H as [Hnotgold_i Hnotgold_drp].
  
    unfold all_character_items_slot_count, all_character_items_slot.
    unfold update_items in nstt.
    subst nstt.
    destruct stt.
    simpl in *.
    unfold sitems in Hgt.
    simpl in *.
    clear book0 maxendurance0 curendurance0 sk0 disciplines0 chapter0 flags0 previtems0.
    destruct VM as [VM NEO].
    rewrite add_item_nogold; auto.
    unfold rm_item, bag_rm.
    rewrite sum_filter_insert_1.
    rewrite Hslot.
    symmetry.
    apply sum_filter_rm_1.
    exact Hgt.
  Qed.

  Lemma rm: forall stt nstt drp q sl,
    nstt = update_items (rm_item drp q) stt ->
    all_character_items_slot_count nstt sl <= all_character_items_slot_count stt sl.
  Proof.
    intros.
    subst.
    unfold all_character_items_slot_count, update_items, all_character_items_slot; simpl.
    remember (items stt) as itms.
    clear Heqitms stt.
    induction itms; simpl; try Lia.lia.
    destruct (mps.eqb (item_slot (fst a)) sl) eqn: SLOT; simpl.
    * unfold rm_item, bag_rm in *.
      destruct a as [ck cv].
      simpl in *.
      destruct (cmp drp ck) eqn: DCK; simpl; try rewrite SLOT; simpl; try Lia.lia.
      apply cmp_eq in DCK.
      subst.
      destruct (cv <=? q) eqn: CVQ; simpl; try rewrite SLOT; simpl; Lia.lia.
    * unfold rm_item, bag_rm in *.
      destruct a as [ck cv].
      simpl in *.
      destruct (cmp drp ck) eqn: DCK; simpl; try rewrite SLOT; simpl; try Lia.lia.
      apply cmp_eq in DCK.
      subst.
      destruct (cv <=? q) eqn: CVQ; simpl; try rewrite SLOT; simpl; Lia.lia.
  Qed.
End Slot.

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
        /\ ValidBag (items s)
        /\ ValidMap (disciplines s)
        /\ ValidMap (flags s)
        /\ ValidBag (previtems s)
        /\ OneWeaponSpecMax s
        /\ le (curendurance s) (max_hp s)
        /\ lt 0 (maxendurance s)
        .

Module VS.

  Lemma max_hp_over_me: forall stt, max_hp stt >= maxendurance stt.
  Proof.
    intros.
    unfold max_hp; simpl. Lia.lia.
  Qed.

  Lemma update_flags: forall stt f, (forall flgs, ValidMap flgs -> ValidMap (f flgs)) -> ValidState stt -> ValidState (update_flags f stt).
  Proof.
    intros.
    unfold ValidState in *.
    unfold ValidBag in *.
    repeat split; try tauto.
    destruct stt. simpl in *.
    apply H. tauto.
  Qed.

  Lemma update_endurance: forall stt f, (forall hp : nat, lt 0%nat (f hp) /\ le (f hp) (max_hp stt)) -> ValidState stt -> ValidState (update_endurance f stt).
  Proof.
    intros.
    unfold ValidState, ValidBag in *.
    repeat split; try tauto.
    destruct stt; simpl in *.  apply H. 
    unfold update_endurance. destruct stt. simpl in *.
    destruct (H curendurance0).
    unfold max_hp in *.
    unfold has_item in *.
    simpl in *.
    assumption.
  Qed.

  Lemma has_item_kept: forall stt itm itm2 q, ValidMap (items stt) -> has_item itm stt = true -> has_item itm (update_items (add_item itm2 q) stt) = true.
  Proof.
    intros.
    destruct q. {
      rewrite Slot.update_add0.
      exact H0.
    }
    unfold has_item, update_items, add_item in *.
    destruct stt; simpl in *. clear book0 maxendurance0 curendurance0 sk0 disciplines0 chapter0 flags0 previtems0.
    destruct (eq_dec itm2 Gold).
    + subst.
      destruct (eq_dec itm Gold).
      - subst. unfold has. rewrite Lookup.lookup_insert_with; auto.
        destruct (lookup Gold items0); try congruence.
        destruct n; try discriminate. simpl.
        * destruct (S q <? 51) eqn: SQ; auto.
        * destruct (S n + S q <? 51) eqn: SQ; auto.
      - unfold has.
        rewrite Lookup.lookup_insert_with_diff; auto.
    + assert (forall b1, match itm2 with | Gold => b1 | _ => insert_with (fun pq : nat => (pq + S q)%nat) itm2 (S q) items0 end
            = insert_with (fun pq : nat => (pq + S q)%nat) itm2 (S q) items0) as RR. {
        intros. destruct itm2; auto. contradiction.
      }
      rewrite RR. clear RR.
      unfold has.
      destruct (eq_dec itm itm2).
      - subst.
        rewrite Lookup.lookup_insert_with; auto.
        destruct (lookup itm2 items0); auto; try discriminate.
        destruct n0; simpl; try discriminate; reflexivity.
      - rewrite Lookup.lookup_insert_with_diff; auto.
  Qed.

  Lemma max_hp_add_items: forall stt i q, ValidMap (items stt) -> q > 0 -> max_hp stt <= max_hp (update_items (add_item i q) stt).
  Proof.
    intros.
    lapply (has_item_kept stt BodyArmor i q); try tauto; intro BA.
    lapply (has_item_kept stt Helmet i q); try tauto; intro HL.
    unfold max_hp, update_items in *.
    simpl in *.
    Ltac bubu := match goal with
    | H: ?a = ?a -> _ |- _ => lapply H; auto; clear H; intros
    | H1: ?x = true, H2: ?x = false |- _ => rewrite H1 in H2; discriminate
    end.

    destruct (has_item BodyArmor stt) eqn: HBA;
    destruct (has_item Helmet stt) eqn: HHE;
    destruct (has_item BodyArmor (update_items (add_item i q) stt)) eqn: HBE2;
    destruct (has_item Helmet (update_items (add_item i q) stt)) eqn: HHE2; try Lia.lia;
    destruct stt; unfold has_item in *; simpl in *; try rewrite HBE2; try rewrite HHE2; try Lia.lia;
    repeat bubu.
  Qed.

  Lemma v_c_rm_items: forall stt i q, ValidState stt -> ValidState (c_rm_items i q stt).
  Proof.
    unfold ValidState, ValidBag.
    simpl.
    intros.
    pose proof (rm_valid (items stt) i q) as VB. unfold ValidBag in VB.
    repeat (split; try tauto).
    * unfold max_hp, update_items; simpl.
      Lia.lia.
    * unfold sitems, c_rm_items, rm_item, bag_rm; simpl.
      destruct (eq_dec i Gold).
      + subst.
        rewrite Lookup.update_eq_b; auto.
        unfold sitems in H.
        destruct (lookup Gold (items stt)); try Lia.lia.
        destruct (n <=? q); Lia.lia.
        unfold ValidBag in H.
        tauto.
      + rewrite Lookup.update_diff; auto.
        unfold sitems in H.
        destruct (lookup Gold (items stt)); try Lia.lia.
        unfold ValidBag in H.
        tauto.
    * lapply (Slot.rm stt (update_items (rm_item i q) stt) i q SBackpack); auto .
      intros.
      unfold c_rm_items.
      unfold all_character_items_slot_count, all_character_items_slot in *; simpl in *.
      Lia.lia.
    * lapply (Slot.rm stt (update_items (rm_item i q) stt) i q SWeapon); auto .
      intros.
      unfold c_rm_items.
      unfold all_character_items_slot_count, all_character_items_slot in *; simpl in *.
      Lia.lia.
    * unfold rm_item.
      Search c_rm_items.


End VS.

Module MH.
  Lemma update_flags: forall stt f,
      max_hp (update_flags f stt) = max_hp stt.
  Proof.
    intros.
    unfold max_hp.
    destruct stt.
    simpl in *.
    reflexivity.
  Qed.
End MH.