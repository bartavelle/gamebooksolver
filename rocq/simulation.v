(* The comparison of values: states that a Tracker can't tell apart have the same value. Each optimization that ignores a
   part of the state is a Tracker, and what it needs from each chapter. *)
Require Import character.
Require Import chapters.
Require Import mps.
Require Import lists.
Require Import proba.
Require Import combat.
Require Import rules.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.QArith.Qcanon.
Require Import Stdlib.QArith.QArith.
Require Import Stdlib.Arith.Arith.
Require Import Stdlib.Bool.Bool.
From Stdlib Require Import Lia.
Require Import Field.

Require Import value.
Require Import fight_facts.

Open Scope nat_scope.

Definition ALL : nat := 255.

(* What follows a fight, as in s_update_d *)
Definition after_fight (rec: Stt -> chapter_outcome -> Proba next_step) (stt: Stt) (mds: list fight_modifier)
    (co: chapter_outcome) (te: TEscaped) : Proba next_step :=
  let (nco, nhp) := match te with
    | Escaped c hp | LateWin c hp | Stopped c hp => (Goto c, hp)
    | TLost c => (Goto c, 1%nat)
    | Std hp => (co, hp)
    end in
  let stt1 := update_flags (s_set HadCombat) stt in
  let stt2 := if has_raw_mod MultiFight mds then stt1 else unset_potions stt1 in
  match Modifiers.extractl Modifiers.gFakeFight mds with
  | Some cid2 => if nhp =? 0 then single_outcome (New (update_chapter cid2 stt2)) else rec stt2 nco
  | None => if nhp =? 0 then single_outcome Lost
            else rec (update_endurance (fun _ => nhp) stt2) nco
  end.

(* The items that the rules use on their own, without being asked by the book *)
Definition potions : list item :=
  [Laumspur; StrengthPotion4; StrengthPotion; GenBackpack S2; Potion6Hp; Potion5Hp; Potion4Hp; Potion2Hp].

(* What the rules do on their own: drinking a potion, and being cured of poison *)
Definition rule_effect (so: simple_outcome) : bool :=
  match so with
  | HealPlayer _ | DamagePlayer _ | ClearFlag Poisonned2 | SetFlag StrengthPotionActive | SetFlag PotentStrengthPotionActive => true
  | LoseItem i 1 => existsb (order.eqb i) potions
  | _ => false
  end.

(* A way of telling that two states can't be told apart by what is left to play. R n s t reads: s and t are the same as
   far as a part of the game that needs n can tell. ALL is a need that only the same states can fulfil. The need_ fields
   say what each part of a book needs, and when they take a need, it is the need of what follows that part: need_simple
   for an effect, need_cond for a condition, need_held for looking at the items of a slot, need_take, need_buy and
   need_sell for items that change hands, need_cartwheel for the game of Book 2, and need_retrieve for getting the
   equipment back. *)
Record Tracker := {
  R : nat -> Stt -> Stt -> Prop;
  need_cond : bool_cond -> nat;
  need_simple : simple_outcome -> nat -> nat;
  need_held : slot -> nat;
  need_take : item -> nat -> nat;
  need_buy : item -> nat -> nat -> nat;
  need_sell : item -> nat -> nat -> nat;
  need_cartwheel : nat;
  need_retrieve : nat;

  R_mono : forall m n s t, m <= n -> R n s t -> R m s t;
  R_top : forall n m s t, ALL <= n -> ALL <= m -> R n s t -> R m s t;
  R_cur : forall n s t, R n s t -> curendurance s = curendurance t;
  R_book : forall n s t, R n s t -> book s = book t;
  R_chapter : forall n s t, R n s t -> chapter s = chapter t;
  R_disc : forall n s t, R n s t -> disciplines s = disciplines t;
  R_maxhp : forall n s t, R n s t -> max_hp s = max_hp t;
  R_fight : forall n s t, R n s t -> SameFight s t;
  R_poison : forall n s t, R n s t -> has_flag Poisonned2 s = has_flag Poisonned2 t;
  R_had : forall n s t, R n s t -> has_flag HadCombat s = has_flag HadCombat t;
  R_potion : forall n s t i, In i potions -> R n s t -> has_item i s = has_item i t;
  R_endur : forall n s t f g, f (curendurance s) = g (curendurance t) -> R n s t ->
      R n (update_endurance f s) (update_endurance g t);
  R_goto : forall n s t c, R n s t -> R n (update_chapter c s) (update_chapter c t);
  R_set_had : forall n s t, R n s t -> R n (update_flags (s_set HadCombat) s) (update_flags (s_set HadCombat) t);
  R_unset_had : forall n s t, R n s t -> R n (update_flags (s_unset HadCombat) s) (update_flags (s_unset HadCombat) t);
  R_unset_pot : forall n s t, R n s t -> R n (unset_potions s) (unset_potions t);
  R_simple : forall so a n s t, need_simple so a <= n -> R n s t -> R a (update_simple s so) (update_simple t so);
  R_cond : forall c n s t, need_cond c <= n -> R n s t -> check_cond s c = check_cond t c;
  R_held : forall sl n s t, need_held sl <= n -> R n s t -> held_items sl s = held_items sl t;
  R_all_held : forall n s t, need_retrieve <= n -> R n s t -> all_held_items s = all_held_items t;
  R_take : forall i a n s t, need_take i a <= n -> R n s t -> tcan_take i s = tcan_take i t;
  R_buy : forall i p a n s t, need_buy i p a <= n -> R n s t ->
      Nat.leb p (sitems Gold s) = Nat.leb p (sitems Gold t) /\ tcan_take i s = tcan_take i t;
  R_sell : forall i p a n s t, need_sell i p a <= n -> R n s t -> Nat.ltb 0 (sitems i s) = Nat.ltb 0 (sitems i t);
  R_cartwheel : forall n s t, need_cartwheel <= n -> R n s t -> sitems Gold s = sitems Gold t;

  need_simple_mono : forall so a b, a <= b -> need_simple so a <= need_simple so b;
  need_simple_ext : forall so a, a <= need_simple so a;
  need_simple_rule : forall so a, rule_effect so = true -> need_simple so a = a;
  need_simple_slot : forall sl i a, In i (slot_items sl) -> need_simple (LoseItem i 1) a <= Nat.max (need_held sl) a;
  need_simple_drop : forall i s lst l a, tcan_take i s = MustDrop lst -> In l lst -> need_simple (LoseItem l 1) a = a;
  need_simple_cartwheel : forall v a, need_simple (LoseItemKind [SPouch]) (need_simple (GainItem Gold v) a) <= Nat.max need_cartwheel a;

  need_take_ext : forall i a, a <= need_take i a;
  need_take_mono : forall i a b, a <= b -> need_take i a <= need_take i b;
  need_take_idem : forall i a, need_take i (need_take i a) <= need_take i a;
  need_take_gain : forall i q a, need_simple (GainItem i q) a <= need_take i a;
  need_buy_ext : forall i p a, a <= need_buy i p a;
  need_buy_gain : forall i p a, need_simple (LoseItem Gold p) (need_simple (GainItem i 1) a) <= need_buy i p a;
  need_sell_ext : forall i p a, a <= need_sell i p a;
  need_sell_gain : forall i p a, need_simple (LoseItem i 1) (need_simple (GainItem Gold p) a) <= need_sell i p a;
  need_retrieve_take : forall i a, need_retrieve < ALL -> need_take i a = a;
  need_retrieve_simple : forall a, need_retrieve < ALL -> need_simple RetrieveEquipment a = a;
}.

Section Sim.
  Variable T: Tracker.
  Variable ch: cid -> nat.
  Variable wn: nat.

  Fixpoint co_need (o: chapter_outcome) : nat :=
    match o with
    | Fight fd n => Nat.max (lmax ch (mods_dests (fd_mods fd))) (co_need n)
    | OneRound _ a b c => Nat.max (co_need a) (Nat.max (co_need b) (co_need c))
    | Randomly l => fold_right (fun x acc => Nat.max (co_need (fst x)) acc) 0 l
    | Conditionally l => fold_right (fun x acc => Nat.max (Nat.max (need_cond T (fst x)) (co_need (snd x))) acc) 0 l
    | Simple l n => fold_right (need_simple T) (co_need n) l
    | Goto c => ch c
    | LoseItemFrom sl _ n => Nat.max (need_held T sl) (co_need n)
    | GameLost => 0
    | GameWon => wn
    | EnterFight _ c => ch c
    | Round fd n l => Nat.max (lmax ch (mods_dests (fd_mods fd))) (Nat.max (co_need n) (Nat.max (ch 0) (lmax ch l)))
    end.

  Fixpoint dec_need (d: decision) : nat :=
    match d with
    | App a b => Nat.max (dec_need a) (dec_need b)
    | retrieve_equipment n => Nat.max (need_retrieve T) (dec_need n)
    | can_take i _ n => need_take T i (dec_need n)
    | can_buy i p n => need_buy T i p (dec_need n)
    | can_sell i p n => need_sell T i p (dec_need n)
    | conditional c n => Nat.max (need_cond T c) (dec_need n)
    | special Cartwheel => Nat.max (need_cartwheel T) (Nat.max (ch 169) (ch 136))
    | special Portholes => ch 197
    | special B05S127 => Nat.max (ch 159) (ch 93)
    | none o => co_need o
    | evade_fight _ _ _ _ => 0
    | after_combat n => dec_need n
    | remove_item_from sl _ n => Nat.max (need_held T sl) (dec_need n)
    end.

  Lemma apply_same: forall sos a n s t, fold_right (need_simple T) a sos <= n -> R T n s t ->
    R T a (apply_effects s sos) (apply_effects t sos).
  Proof.
    unfold apply_effects. induction sos as [|so q IH]; intros a n s t L H; simpl in *.
    - eapply R_mono; eauto.
    - eapply IH; [apply le_n|]. eapply R_simple; eauto.
  Qed.

  Lemma fold_mono: forall sos a b, a <= b -> fold_right (need_simple T) a sos <= fold_right (need_simple T) b sos.
  Proof.
    induction sos; intros; simpl; auto. apply need_simple_mono. auto.
  Qed.

  Lemma fold_ext: forall sos a, a <= fold_right (need_simple T) a sos.
  Proof.
    induction sos; intros; simpl; auto. eapply Nat.le_trans; [apply IHsos|apply need_simple_ext].
  Qed.

  Definition le_or_all (x n: nat) := x <= n \/ ALL <= n.

  Variable f: next_step -> Qc.
  Hypothesis fN: forall x y, R T (ch (chapter x)) x y -> f (New x) = f (New y).
  Hypothesis fW: forall x y, R T wn x y -> f (Won x) = f (Won y).

  Lemma after_fight_core: forall (rec1 rec2: Stt -> chapter_outcome -> Proba next_step) s t n mds nco nhp,
    R T n s t -> lmax ch (mods_dests mds) <= n -> co_need nco <= n ->
    (forall a x y o, co_need o <= a -> R T a x y -> ev f (rec1 x o) = ev f (rec2 y o)) ->
    let af (rec: Stt -> chapter_outcome -> Proba next_step) (stt: Stt) :=
      let stt1 := update_flags (s_set HadCombat) stt in
      let stt2 := if has_raw_mod MultiFight mds then stt1 else unset_potions stt1 in
      match Modifiers.extractl Modifiers.gFakeFight mds with
      | Some cid2 => if nhp =? 0 then single_outcome (New (update_chapter cid2 stt2)) else rec stt2 nco
      | None => if nhp =? 0 then single_outcome Lost
                else rec (update_endurance (fun _ => nhp) stt2) nco
      end in
    ev f (af rec1 s) = ev f (af rec2 t).
  Proof.
    intros rec1 rec2 s t n mds nco nhp HR HL HC REC af. unfold af. cbv zeta.
    assert (R T n (if has_raw_mod MultiFight mds then update_flags (s_set HadCombat) s else unset_potions (update_flags (s_set HadCombat) s))
                  (if has_raw_mod MultiFight mds then update_flags (s_set HadCombat) t else unset_potions (update_flags (s_set HadCombat) t))) as R2.
    { destruct (has_raw_mod MultiFight mds); [|apply R_unset_pot]; apply R_set_had; assumption. }
    destruct (Modifiers.extractl Modifiers.gFakeFight mds) as [c|] eqn: E.
    - destruct (nhp =? 0).
      + unfold single_outcome. rewrite !ev_single. apply fN. simpl.
        eapply R_goto, R_mono; [|exact R2].
        eapply Nat.le_trans; [|exact HL]. apply lmax_le. eapply extractl_dest; eauto. mod_cases.
      + eapply REC; eauto.
    - destruct (nhp =? 0); [reflexivity|].
      eapply REC; [exact HC|]. apply R_endur; [reflexivity|assumption].
  Qed.

  Lemma after_fight_same: forall rec1 rec2 s t n mds co te,
    R T n s t -> te_dest_in mds te -> lmax ch (mods_dests mds) <= n -> co_need co <= n ->
    (forall a x y o, co_need o <= a -> R T a x y -> ev f (rec1 x o) = ev f (rec2 y o)) ->
    ev f (after_fight rec1 s mds co te) = ev f (after_fight rec2 t mds co te).
  Proof.
    intros rec1 rec2 s t n mds co te HR HI HL HC REC.
    assert (forall c, In c (mods_dests mds) -> co_need (Goto c) <= n) as HG.
    { intros c I. simpl. eapply Nat.le_trans; [|exact HL]. apply lmax_le. assumption. }
    unfold after_fight. destruct te; simpl in HI; eapply after_fight_core; eauto.
  Qed.

  Lemma ev_update: forall fuel s t o n, co_need o <= n -> R T n s t ->
    ev f (s_update_d fuel s o) = ev f (s_update_d fuel t o).
  Proof.
    induction fuel as [|md IH]; intros s t o n HN HR.
    { simpl. rewrite (R_cur _ _ _ _ HR). reflexivity. }
    cbn [s_update_d]. rewrite (R_cur _ _ _ _ HR). destruct (curendurance t =? 0) eqn: C0; [reflexivity|].
    destruct o as [fd co|fd a b c|lst|lst|sos co|next|sl q co| | |fd c|fd co rounds].
    - (* Fight *)
      destruct fd as [osk opphp mds]. simpl in HN.
      change (ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) s mds co te)) (fight s osk opphp mds)))
            = ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) t mds co te)) (fight t osk opphp mds)))).
      rewrite <- (fight_same _ _ osk opphp mds (R_fight _ _ _ _ HR)).
      apply ev_merge_map2. intros [te p] I. simpl. split; [reflexivity|].
      eapply after_fight_same; eauto; try lia.
      apply (fight_dests s osk opphp). apply (in_map fst) in I. exact I.
    - (* OneRound *)
      destruct fd as [osk opphp mds]. simpl in HN.
      rewrite <- (fight_round_same _ _ (R_fight _ _ _ _ HR) osk opphp mds).
      apply ev_merge_map2. intros [[lw op] p] I. simpl. split; [reflexivity|].
      destruct (NatLemma.nat_cmp _ _); apply (IH _ _ _ n); try lia;
        (apply R_endur; [reflexivity | apply R_set_had, R_unset_pot; exact HR]).
    - (* Randomly *)
      apply ev_merge_map2. intros [co p] I. simpl. split; [reflexivity|].
      eapply IH; [|exact HR].
      eapply Nat.le_trans; [|exact HN]. exact (lmax_le (fun x => co_need (fst x)) lst (co, p) I).
    - (* Conditionally *)
      assert (forall x, In x lst -> Nat.max (need_cond T (fst x)) (co_need (snd x)) <= n) as HL.
      { intros x I. eapply Nat.le_trans; [|exact HN].
        exact (lmax_le (fun x => Nat.max (need_cond T (fst x)) (co_need (snd x))) lst x I). }
      assert (filter (fun kv => check_cond s (fst kv)) lst = filter (fun kv => check_cond t (fst kv)) lst) as FE.
      { apply filter_ext_in. intros x I. eapply R_cond; [|exact HR]. specialize (HL x I). lia. }
      assert (forall c co q, filter (fun kv => check_cond t (fst kv)) lst = (c, co) :: q -> In (c, co) lst) as IL.
      { intros c co q E. assert (In (c, co) (filter (fun kv => check_cond t (fst kv)) lst)) as J by (rewrite E; left; reflexivity).
        apply filter_In in J. tauto. }
      destruct (filter (fun kv => check_cond s (fst kv)) lst) as [|[c co] q];
      destruct (filter (fun kv => check_cond t (fst kv)) lst) as [|[c2 co2] q2] eqn: F2;
        try discriminate; [reflexivity|].
      inversion FE; subst.
      eapply IH; [|exact HR].
      specialize (HL _ (IL _ _ _ Logic.eq_refl)). simpl in HL. lia.
    - (* Simple *)
      eapply IH; [apply le_n|]. eapply apply_same; eauto.
    - (* Goto *)
      simpl in HN.
      assert (R T (ch next) (update_chapter next s) (update_chapter next t)) as RG.
      { apply R_goto. eapply R_mono; eauto. }
      rewrite (R_book _ _ _ _ HR), (R_chapter _ _ _ _ HR), (R_poison _ _ _ _ HR), (R_had _ _ _ _ HR), (R_disc _ _ _ _ HR).
      destruct (_ && has_flag Poisonned2 t)%bool.
      + simpl. rewrite (R_cur _ _ _ _ HR). destruct (curendurance t - 2 =? 0).
        * reflexivity.
        * unfold single_outcome. rewrite !ev_single. apply fN. simpl.
          apply R_endur; [simpl; rewrite (R_cur _ _ _ _ HR); reflexivity|assumption].
      + destruct (_ && _ && _)%bool; unfold single_outcome; rewrite !ev_single; apply fN; simpl.
        * unfold heal. apply R_endur; [|assumption].
          rewrite (R_maxhp _ _ _ _ RG). simpl. rewrite (R_cur _ _ _ _ HR). reflexivity.
        * assumption.
    - (* LoseItemFrom *)
      simpl in HN.
      assert (held_items sl s = held_items sl t) as HH by (eapply R_held; [|exact HR]; lia).
      rewrite HH. destruct q as [|nq].
      + eapply IH; [|exact HR]. lia.
      + destruct (held_items sl t) as [|x tl] eqn: E.
        * eapply IH; [|exact HR]. lia.
        * apply ev_merge_map2. intros [i amnt] I. cbn [fst snd]. split; [reflexivity|].
          eapply IH; [apply le_n|].
          eapply R_simple; [|exact HR].
          assert (In (i, amnt) (held_items sl s)) as I2 by (rewrite HH; exact I).
          pose proof (held_in_slot _ _ _ I2) as IS. simpl in IS.
          pose proof (need_simple_slot T sl i (co_need (LoseItemFrom sl nq co)) IS) as SL.
          change (co_need (LoseItemFrom sl nq co)) with (Nat.max (need_held T sl) (co_need co)) in *. lia.
    - (* GameLost *)
      reflexivity.
    - (* GameWon *)
      unfold single_outcome. rewrite !ev_single. apply fW. eapply R_mono; eauto.
    - (* EnterFight *)
      unfold single_outcome. rewrite !ev_single. apply fN. simpl.
      apply R_goto, R_set_had. eapply R_mono; eauto.
    - (* Round *)
      destruct fd as [osk opphp mds]. simpl in HN.
      rewrite <- (one_round_same _ _ (R_fight _ _ _ _ HR) osk opphp mds).
      rewrite !ev_add_proba. f_equal.
      + change (ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) s mds co te)) (fst (one_round s osk opphp mds))))
              = ev f (merge_probas (map (fun tep : TEscaped * Qc => let (te, p) := tep in (p, after_fight (s_update_d md) t mds co te)) (fst (one_round s osk opphp mds))))).
        apply ev_merge_map2. intros [te p] I. simpl. split; [reflexivity|].
        eapply after_fight_same; eauto; try lia.
        apply (one_round_ok s osk opphp). apply (in_map fst) in I. exact I.
      + apply ev_map_same. intros [[lw op] p] I. simpl. split; [reflexivity|].
        apply fN. simpl. apply R_goto.
        apply R_endur; [reflexivity|]. apply R_set_had.
        eapply R_mono; [|exact HR].
        assert (ch (nth op rounds 0) <= Nat.max (ch 0) (lmax ch rounds)).
        { destruct (nth_in_or_default op rounds 0) as [J|J].
          - pose proof (lmax_le ch rounds _ J). lia.
          - rewrite J. lia. }
        lia.
  Qed.

  Lemma le_or_all_up: forall sos x a n, le_or_all x a -> fold_right (need_simple T) a sos <= n -> le_or_all (fold_right (need_simple T) x sos) n.
  Proof.
    intros sos x a n [H|H] L.
    - left. eapply Nat.le_trans; [apply fold_mono; exact H|exact L].
    - right. eapply Nat.le_trans; [exact H|]. eapply Nat.le_trans; [apply fold_ext|exact L].
  Qed.

  Definition FlatOk (fl: nat) (n: nat) (s t: Stt) (d: decision) : Prop :=
    flatten_d fl s d = flatten_d fl t d /\ forall o, In o (flatten_d fl s d) -> le_or_all (co_need o) n.

  Lemma with_effect_ok: forall fl sos nxt n s t,
    (forall m x y d, dec_need d <= m -> R T m x y -> FlatOk fl m x y d) ->
    fold_right (need_simple T) (dec_need nxt) sos <= n -> R T n s t ->
    map (Simple sos) (flatten_d fl (apply_effects s sos) nxt) = map (Simple sos) (flatten_d fl (apply_effects t sos) nxt)
    /\ forall o, In o (map (Simple sos) (flatten_d fl (apply_effects s sos) nxt)) -> le_or_all (co_need o) n.
  Proof.
    intros fl sos nxt n s t IH L HR.
    destruct (IH (dec_need nxt) _ _ nxt (le_n _) (apply_same _ _ _ _ _ L HR)) as [E M].
    split.
    - rewrite E. reflexivity.
    - intros o I. apply in_map_iff in I. destruct I as [x [X J]]. subst. simpl.
      eapply le_or_all_up; [apply M; exact J|exact L].
  Qed.

  Lemma app_ok: forall (a b c d: list chapter_outcome) n,
    a = b -> c = d -> (forall o, In o a -> le_or_all (co_need o) n) -> (forall o, In o c -> le_or_all (co_need o) n) ->
    a ++ c = b ++ d /\ forall o, In o (a ++ c) -> le_or_all (co_need o) n.
  Proof.
    intros. subst. split; auto. intros o I. apply in_app_iff in I. destruct I; auto.
  Qed.

  Lemma app_ok2: forall (a b c d: list chapter_outcome) n,
    a = b -> (forall o, In o a -> le_or_all (co_need o) n) ->
    (c = d /\ forall o, In o c -> le_or_all (co_need o) n) ->
    a ++ c = b ++ d /\ forall o, In o (a ++ c) -> le_or_all (co_need o) n.
  Proof.
    intros a b c d n E M [E2 M2]. apply app_ok; auto.
  Qed.

  Lemma flat_map_ok {A: Type}: forall (l: list A) (g h: A -> list chapter_outcome) n,
    (forall a, In a l -> g a = h a /\ forall o, In o (g a) -> le_or_all (co_need o) n) ->
    flat_map g l = flat_map h l /\ forall o, In o (flat_map g l) -> le_or_all (co_need o) n.
  Proof.
    induction l as [|a l IH]; intros g h n P; simpl.
    - split; auto. intros o [].
    - destruct (P a) as [E M]; [left; reflexivity|].
      destruct (IH g h n) as [E2 M2]; [intros; apply P; right; assumption|].
      apply app_ok; auto.
  Qed.

  Lemma cartwheel_choice_need: forall g tgt, co_need (cartwheel_choice g tgt) <= Nat.max (need_cartwheel T) (Nat.max (ch 169) (ch 136)).
  Proof.
    intros. unfold cartwheel_choice. generalize (cartwheel_with_token g tgt). intro l.
    induction l as [|[v p] l IH]; simpl; [lia|].
    apply Nat.max_lub; [|exact IH].
    eapply Nat.le_trans; [apply need_simple_cartwheel|].
    destruct (v <? cartwheel_fare); lia.
  Qed.

  Definition potion_list (bk: nat) : list (bool * item * nat) :=
    [ (Nat.eqb bk 5, GenBackpack S2, 10%nat)
    ; (true, Potion6Hp, 6%nat)
    ; (true, Potion5Hp, 5%nat)
    ; (true, Potion4Hp, 4%nat)
    ; (negb (Nat.eqb bk 1), Laumspur, 4%nat)
    ; (true, Potion2Hp, 2%nat)
    ].

  Lemma drink_sub: forall (U: list (bool * item * nat)) m x,
    In x (filter (fun ih : bool * item * nat => let '(_, _, h) := ih in Nat.leb h m) U
          ++ match rev (filter (fun ih : bool * item * nat => let '(_, _, h) := ih in Nat.ltb m h) U) with
             | [] => [] | y :: _ => [y] end) -> In x U.
  Proof.
    intros U m x I. apply in_app_iff in I. destruct I as [I|I].
    - apply filter_In in I. tauto.
    - destruct (rev _) as [|y q] eqn: E; [destruct I|].
      destruct I as [I|[]]. subst.
      assert (In x (rev (filter (fun ih : bool * item * nat => let '(_, _, h) := ih in Nat.ltb m h) U))) as J by (rewrite E; left; reflexivity).
      apply in_rev in J. apply filter_In in J. tauto.
  Qed.

  Lemma fold_take_need: forall (l: list (item * nat)) nxt, need_retrieve T < ALL ->
    dec_need (fold_left (fun acc (iq: item * nat) => can_take (fst iq) (snd iq) acc) l nxt) = dec_need nxt.
  Proof.
    induction l as [|x l IH]; intros nxt S; simpl; auto. rewrite IH by assumption. simpl. apply need_retrieve_take. assumption.
  Qed.

  Lemma flatten_same: forall fl n s t d, dec_need d <= n -> R T n s t -> FlatOk fl n s t d.
  Proof.
    induction fl as [|fl IH]; intros n s t d HN HR.
    { split; [reflexivity|intros o []]. }
    assert (forall sos nxt, fold_right (need_simple T) (dec_need nxt) sos <= n ->
      map (Simple sos) (flatten_d fl (apply_effects s sos) nxt) = map (Simple sos) (flatten_d fl (apply_effects t sos) nxt)
      /\ forall o, In o (map (Simple sos) (flatten_d fl (apply_effects s sos) nxt)) -> le_or_all (co_need o) n) as WE.
    { intros. eapply with_effect_ok; eauto. }
    assert (forall so x, rule_effect so = true -> need_simple T so x = x) as RU by (intros; apply need_simple_rule; assumption).
    assert (forall d2, dec_need d2 <= n -> FlatOk fl n s t d2) as NX by (intros; apply IH; auto).
    unfold FlatOk. cbn [flatten_d].
    rewrite (R_poison _ _ _ _ HR), (R_potion _ n s t Laumspur ltac:(simpl; tauto) HR).
    destruct (has_flag Poisonned2 t && has_item Laumspur t)%bool.
    { apply WE. simpl. rewrite RU by reflexivity. rewrite RU by reflexivity. exact HN. }
    destruct d as [d1 d2|nxt|i q nxt|i p nxt|i p nxt|c nxt|sp|o|a b c' o|nxt|sl k nxt]; cbn [dec_need] in HN.
    - (* App *)
      destruct (NX d1) as [E1 M1]; [lia|]. destruct (NX d2) as [E2 M2]; [lia|].
      apply app_ok; auto.
    - (* retrieve_equipment *)
      rewrite (R_all_held T n s t ltac:(lia) HR).
      match goal with |- context [map (Simple [RetrieveEquipment]) (flatten_d fl (apply_effects t [RetrieveEquipment]) ?nd)] =>
        set (ndec := nd) end.
      destruct (Nat.le_gt_cases ALL (need_retrieve T)) as [BIG|SM].
      2: { apply WE. simpl. unfold ndec. rewrite need_retrieve_simple by assumption. rewrite fold_take_need by assumption. lia. }
      assert (ALL <= n) as AN by lia.
      set (n2 := need_simple T RetrieveEquipment (Nat.max n (dec_need ndec))).
      assert (Nat.max n (dec_need ndec) <= n2) as N2 by apply need_simple_ext.
      assert (R T n2 s t) as R2 by (eapply R_top; [exact AN| |exact HR]; lia).
      destruct (with_effect_ok fl [RetrieveEquipment] ndec n2 s t) as [E M].
      + intros. apply IH; auto.
      + simpl. apply need_simple_mono. lia.
      + exact R2.
      + split; [exact E|]. intros x I. right. exact AN.
    - (* can_take *)
      destruct (eq_dec i Gold) as [EG|NG].
      { subst i. apply WE. simpl. eapply Nat.le_trans; [apply need_take_gain|exact HN]. }
      assert (dec_need nxt <= n) as HA by (eapply Nat.le_trans; [apply need_take_ext|exact HN]).
      destruct i; try congruence;
      (destruct q as [|[|q]];
      [ apply NX; exact HA
      | rewrite (R_take T _ (dec_need nxt) n s t HN HR), (R_book _ _ _ _ HR);
        match goal with |- context [tcan_take ?it t] => destruct (tcan_take it t) as [|lst|] eqn: TC end;
        [ apply WE; simpl; eapply Nat.le_trans; [apply need_take_gain|exact HN]
        | destruct (NX nxt HA) as [E1 M1]; apply app_ok2; auto; apply flat_map_ok;
          intros l Il; (destruct (_ <? backpack_slots _ _)%nat;
            [ apply WE; simpl; rewrite (need_simple_drop T _ t lst l _ TC Il); exact HN
            | destruct (order.eqb l _);
              [ split; [reflexivity|intros x []]
              | apply WE; simpl; rewrite (need_simple_drop T _ t lst l _ TC Il); eapply Nat.le_trans; [apply need_take_gain|exact HN]]])
        | apply NX; exact HA ]
      | apply NX; simpl; eapply Nat.le_trans; [apply need_take_idem|exact HN] ]).
    - (* can_buy *)
      assert (dec_need nxt <= n) as HA by (eapply Nat.le_trans; [apply need_buy_ext|exact HN]).
      destruct (R_buy T i p _ n s t HN HR) as [BG BT].
      rewrite BG, BT. destruct (NX nxt HA) as [E1 M1].
      destruct (p <=? sitems Gold t)%nat.
      + destruct (tcan_take i t) as [|lst|] eqn: TC.
        * edestruct (WE [LoseItem Gold p; GainItem i 1] nxt) as [E2 M2]; [simpl; eapply Nat.le_trans; [apply need_buy_gain|exact HN]|].
          apply app_ok; auto.
        * apply app_ok2; auto. apply flat_map_ok. intros l Il. apply WE. simpl. rewrite (need_simple_drop T _ t lst l _ TC Il).
          eapply Nat.le_trans; [apply need_buy_gain|exact HN].
        * apply app_ok; auto. intros x [].
      + apply app_ok; auto. intros x [].
    - (* can_sell *)
      assert (dec_need nxt <= n) as HA by (eapply Nat.le_trans; [apply need_sell_ext|exact HN]).
      rewrite (R_sell T i p _ n s t HN HR). destruct (NX nxt HA) as [E1 M1].
      destruct (0 <? sitems i t)%nat.
      + edestruct (WE [LoseItem i 1; GainItem Gold p] nxt) as [E2 M2]; [simpl; eapply Nat.le_trans; [apply need_sell_gain|exact HN]|].
        apply app_ok; auto.
      + apply app_ok; auto. intros x [].
    - (* conditional *)
      rewrite (R_cond T c n s t ltac:(lia) HR). destruct (check_cond t c).
      + apply NX. lia.
      + split; [reflexivity|intros x []].
    - (* special *)
      destruct sp; cbn [dec_need] in HN.
      + (* Cartwheel *)
        rewrite (R_cartwheel T n s t ltac:(lia) HR).
        assert (forall tgt, dec_need (none (cartwheel_choice (sitems Gold t) tgt)) <= n) as HC.
        { intro tgt. simpl. eapply Nat.le_trans; [apply cartwheel_choice_need|exact HN]. }
        destruct (NX (none (Goto 136))) as [E1 M1]; [simpl; lia|].
        apply app_ok2; [|  |apply flat_map_ok; intros tgt _; exact (NX _ (HC tgt))].
        * destruct (cartwheel_fare <=? sitems Gold t)%nat; [|reflexivity].
          destruct (NX _ (HC (sitems Gold t))) as [E2 _]. rewrite E1, E2. reflexivity.
        * destruct (cartwheel_fare <=? sitems Gold t)%nat; [|intros x []].
          destruct (NX _ (HC (sitems Gold t))) as [_ M2]. intros x I.
          apply in_app_iff in I. destruct I; auto.
      + (* Portholes *)
        apply NX. simpl. exact HN.
      + (* B05S127 *)
        rewrite (sf_sk _ _ (R_fight _ _ _ _ HR)). split; [reflexivity|]. intros x [I|[]]. subst. left. simpl.
        rewrite RU by reflexivity. lia.
    - (* none *)
      rewrite (R_potion _ n s t StrengthPotion4 ltac:(simpl; tauto) HR), (R_potion _ n s t StrengthPotion ltac:(simpl; tauto) HR).
      split; [reflexivity|]. intros x I.
      destruct (has_combat o) as [fd|].
      + destruct (fd_has NoPotion fd).
        * destruct I as [I|[]]. subst. left. exact HN.
        * destruct I as [I|I]; [subst; left; exact HN|].
          apply in_app_iff in I.
          destruct I as [I|I]; [destruct (has_item StrengthPotion4 t)|destruct (has_item StrengthPotion t)];
            try destruct I as [I|[]]; try destruct I; subst; left; simpl; rewrite !RU by reflexivity; exact HN.
      + destruct I as [I|[]]. subst. left. exact HN.
    - (* evade_fight *)
      split; [reflexivity|intros x []].
    - (* after_combat *)
      destruct (NX nxt HN) as [E1 M1].
      rewrite (R_maxhp _ _ _ _ HR), (R_cur _ _ _ _ HR), (R_book _ _ _ _ HR).
      destruct ((max_hp t - curendurance t =? 0)%nat); [split; assumption|].
      assert (forall x, In x (potion_list (book t)) -> In (snd (fst x)) potions) as PL.
      { intros x J. simpl in J. repeat (destruct J as [J|J]; [subst; simpl; tauto|]). destruct J. }
      assert (filter (fun ih : bool * item * nat => let '(u, i, _) := ih in u && has_item i s) (potion_list (book t))
            = filter (fun ih : bool * item * nat => let '(u, i, _) := ih in u && has_item i t) (potion_list (book t))) as PF.
      { apply filter_ext_in. intros [[u i] h] J. specialize (PL _ J). simpl in PL. rewrite (R_potion _ n s t i PL HR). reflexivity. }
      unfold potion_list in PF. rewrite PF.
      apply app_ok2; [exact E1|exact M1|]. apply flat_map_ok.
      intros [[u i] h] J. cbn beta iota. apply WE.
      apply drink_sub in J. apply filter_In in J. destruct J as [J _].
      specialize (PL _ J). simpl in PL. simpl.
      rewrite RU by reflexivity. rewrite need_simple_rule; [exact HN|].
      repeat (destruct PL as [PL|PL]; [subst; reflexivity|]). destruct PL.
    - (* remove_item_from *)
      assert (dec_need nxt <= n) as HA by lia.
      assert (held_items sl s = held_items sl t) as HH by (eapply R_held; [|exact HR]; lia).
      rewrite HH. destruct k as [|m]; [apply NX; exact HA|].
      destruct (held_items sl t) as [|x tl] eqn: E; [apply NX; exact HA|].
      apply flat_map_ok. intros iq J. apply WE. simpl.
      assert (In iq (held_items sl s)) as J2 by (rewrite HH; exact J).
      pose proof (need_simple_slot T sl (fst iq) (Nat.max (need_held T sl) (dec_need nxt)) (held_in_slot _ _ _ J2)). lia.
  Qed.
End Sim.

(* Two decisions that give different outcomes can still have the same value: a decision is fine if its outcomes in two
   states have the same value, one by one, for everything that can't tell apart what comes next. *)
Definition TopSim (T: Tracker) (ch: cid -> nat) (wn: nat) (d: decision) (n: nat) : Prop :=
  forall f: next_step -> Qc,
    (forall x y, R T (ch (chapter x)) x y -> f (New x) = f (New y)) ->
    (forall x y, R T wn x y -> f (Won x) = f (Won y)) ->
    forall fl fuel s t, R T n s t ->
      Forall2 (fun o1 o2 =>
        ev f (s_update_d fuel (update_flags (s_unset HadCombat) s) o1) = ev f (s_update_d fuel (update_flags (s_unset HadCombat) t) o2))
        (flatten_d fl s d) (flatten_d fl t d).

(* States that R can't tell apart at the need of their chapter have the same value, if each chapter needs no more than
   what is written for it, or has outcomes that have the same value. *)
Theorem value_same_top: forall (T: Tracker) (ch: cid -> nat) (wn: nat) (chapters: list (cid * decision)) (score: Stt -> Qc),
  (forall s t, R T wn s t -> score s = score t) ->
  (forall c d, find_chapter c chapters = Some d -> dec_need T ch wn d <= ch c \/ TopSim T ch wn d (ch c)) ->
  forall fuel s t, R T (ch (chapter s)) s t -> value chapters score fuel s = value chapters score fuel t.
Proof.
  intros T ch wn chapters score HS HB.
  induction fuel as [|fuel IH]; intros s t HR; [reflexivity|].
  cbn [value]. unfold step.
  rewrite <- (R_chapter _ _ _ _ HR).
  destruct (find_chapter (chapter s) chapters) as [d|] eqn: F; [|reflexivity].
  set (g := fun ns => match ns with Lost => 0%Qc | Won x => score x | New x => value chapters score fuel x end).
  destruct (HB _ _ F) as [HN|TS].
  - destruct (flatten_same T ch wn 1000 (ch (chapter s)) s t d HN HR) as [E M].
    unfold flatten_decision. rewrite <- E. rewrite !map_map. f_equal. apply map_ext_in. intros o I.
    assert (R T (ch (chapter s)) (update_flags (s_unset HadCombat) s) (update_flags (s_unset HadCombat) t)) as RN
      by (apply R_unset_had; assumption).
    unfold s_update.
    assert (forall n, co_need T ch wn o <= n -> R T n (update_flags (s_unset HadCombat) s) (update_flags (s_unset HadCombat) t) ->
      ev g (s_update_d 1000 (update_flags (s_unset HadCombat) s) o) = ev g (s_update_d 1000 (update_flags (s_unset HadCombat) t) o)) as G.
    { intros n L R2.
      apply (ev_update T ch wn g (fun x y H => IH x y H) (fun x y H => HS x y H) 1000 _ _ o n L R2). }
    destruct (M o I) as [L|A].
    + apply (G _ L RN).
    + apply (G (Nat.max (ch (chapter s)) (co_need T ch wn o))); [lia|].
      eapply R_top; [exact A| |exact RN]. lia.
  - unfold flatten_decision, s_update. rewrite !map_map. f_equal.
    apply forall2_map. apply TS.
    + intros x y H. apply IH. exact H.
    + intros x y H. apply HS. exact H.
    + exact HR.
Qed.

Theorem value_same: forall (T: Tracker) (ch: cid -> nat) (wn: nat) (chapters: list (cid * decision)) (score: Stt -> Qc),
  (forall s t, R T wn s t -> score s = score t) ->
  (forall c d, find_chapter c chapters = Some d -> dec_need T ch wn d <= ch c) ->
  forall fuel s t, R T (ch (chapter s)) s t -> value chapters score fuel s = value chapters score fuel t.
Proof.
  intros T ch wn chapters score HS HB. apply (value_same_top T ch wn chapters score HS).
  intros c d F. left. eapply HB. exact F.
Qed.
