From Stdlib Require Import Lia.
Import Nat.
Require Import Stdlib.Lists.List.
Import ListNotations.
Require Import Stdlib.Arith.Peano_dec.
Require Import mps.
Require Import proba.
Require Import ZArith.
Require Import Stdlib.QArith.Qcanon.

Inductive weapon : Set :=
    | Dagger
    | Spear
    | Mace
    | ShortSword
    | Warhammer
    | Sword
    | Axe
    | Quarterstaff
    | BroadSword
    | MagicSpear
    | Sommerswerd.

Scheme Equality for weapon.

Definition all_weapons: list weapon : Set := 
    [ Dagger
    ; Spear
    ; Mace
    ; ShortSword
    ; Warhammer
    ; Sword
    ; Axe
    ; Quarterstaff
    ; BroadSword
    ; MagicSpear
    ; Sommerswerd
    ].

Module m_weapon.
    Lemma eqb_correct: forall x y : weapon, x = y <-> weapon_beq x y = true.
    Proof.
        destruct x, y; split; intro; try discriminate; auto.
    Qed.

    Lemma complete: forall x, In x all_weapons.
    Proof.
        intros.
        unfold all_weapons.
        destruct x; repeat (try (left; reflexivity); right).
    Qed.

End m_weapon.

Instance weapon_EqDec : EqDec weapon := {
  eqb := weapon_beq;
  eqb_correct := m_weapon.eqb_correct;
}.

Instance weapon_OrdDec : OrdDec weapon weapon_EqDec := enumOrdDec weapon weapon_EqDec all_weapons m_weapon.complete.

Inductive sslot : Set :=
    | S0
    | S1
    | S2
    | S3
    | S4
    | S5
    | S6
    | S7
    | S8
    | S9
    | S10
    | S11
    .

Scheme Equality for sslot.

Definition all_sslot: list sslot :=
    [ S0
    ; S1
    ; S2
    ; S3
    ; S4
    ; S5
    ; S6
    ; S7
    ; S8
    ; S9
    ; S10
    ; S11 ].

Module m_sslot.
    Lemma eqb_correct: forall x y : sslot, x = y <-> sslot_beq x y = true.
    Proof.
        destruct x, y; split; intro; try discriminate; auto.
    Qed.

    Lemma complete: forall x, In x all_sslot.
    Proof.
        intros.
        unfold all_sslot.
        destruct x; repeat (try (left; reflexivity); right).
    Qed.
End m_sslot.

Instance sslot_EqDec : EqDec sslot := {
  eqb := sslot_beq;
  eqb_correct := m_sslot.eqb_correct;
}.

Instance sslot_OrdDec : OrdDec sslot sslot_EqDec := enumOrdDec sslot sslot_EqDec all_sslot m_sslot.complete.

Inductive item : Set :=
    | Weapon: weapon -> item
    | Backpack
    | StrengthPotion4
    | Shield
    | BodyArmor
    | Potion2Hp
    | Potion4Hp
    | Potion5Hp
    | Potion6Hp
    | StrengthPotion
    | GenSpecial: sslot -> item
    | GenBackpack: sslot -> item
    | Meal
    | Gold
    | Laumspur
    | Helmet
    .

Scheme Equality for item.

Definition all_items : list item := 
    [ Backpack
    ; StrengthPotion4
    ; Shield
    ; BodyArmor
    ; Potion2Hp
    ; Potion4Hp
    ; Potion5Hp
    ; Potion6Hp
    ; StrengthPotion
    ; Meal
    ; Gold
    ; Laumspur
    ; Helmet
    ] ++ map Weapon all_weapons
    ++ map GenSpecial all_sslot
    ++ map GenBackpack all_sslot
    .

Module m_item.

    Lemma eqb_correct: forall x y : item, x = y <-> item_beq x y = true.
    Proof.
        destruct x, y; split; intro; try discriminate; auto.
        destruct w, w0; try discriminate; auto.
        destruct w, w0; try discriminate; auto.
        destruct s, s0; try discriminate; auto.
        destruct s, s0; try discriminate; auto.
        destruct s, s0; try discriminate; auto.
        destruct s, s0; try discriminate; auto.
    Qed.

    Lemma complete: forall x, In x all_items.
    Proof.
        intros.
        unfold all_items. simpl.
        destruct x. 
        destruct w; repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        destruct s; repeat (try (left; reflexivity); right).
        destruct s; repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
        repeat (try (left; reflexivity); right).
    Qed.
End m_item.

Instance item_EqDec : EqDec item := {
  eqb := item_beq;
  eqb_correct := m_item.eqb_correct;
}.

Instance item_OrdDec : OrdDec item item_EqDec := enumOrdDec item item_EqDec all_items m_item.complete.

Inductive discipline : Set :=
    | Camouflage
    | Hunting
    | SixthSense
    | Tracking
    | Healing
    | WeaponSkill : weapon -> discipline
    | MindShield
    | MindBlast
    | AnimalKinship
    | MindOverMatter
    .

Scheme Equality for discipline.


Definition all_disciplines :=
    [ Camouflage
    ; Hunting
    ; SixthSense
    ; Tracking
    ; Healing
    ; MindShield
    ; MindBlast
    ; AnimalKinship
    ; MindOverMatter
    ] ++ map WeaponSkill all_weapons.


Module m_discipline.
    Lemma eqb_correct: forall x y : discipline, x = y <-> discipline_beq x y = true.
    Proof.
        destruct x, y; split; intro; try discriminate; auto.
        destruct w, w0; try discriminate; auto.
        destruct w, w0; try discriminate; auto.
    Qed.

    Lemma complete: forall x, In x all_disciplines.
    Proof.
        intros.
        unfold all_disciplines.
        destruct x; try destruct w; repeat (try (left; reflexivity); right).
    Qed.
End m_discipline.

Instance discipline_EqDec : EqDec discipline := {
  eqb := discipline_beq;
  eqb_correct := m_discipline.eqb_correct;
}.

Instance discipline_OrdDec : OrdDec discipline discipline_EqDec := enumOrdDec discipline discipline_EqDec all_disciplines m_discipline.complete.

Inductive special_chapter : Set :=
    | Cartwheel
    | Portholes
    | B05S127
    | B05S357
    .

Scheme Equality for special_chapter.

Definition all_special_chapters := 
    [ Cartwheel
    ; Portholes
    ; B05S127
    ; B05S357
    ].

Module m_special_chapter.
    Lemma eqb_correct: forall x y : special_chapter, x = y <-> special_chapter_beq x y = true.
    Proof.
        destruct x, y; split; intro; try discriminate; auto.
    Qed.

    Lemma complete: forall x, In x all_special_chapters.
    Proof.
        intros.
        unfold all_special_chapters.
        destruct x; try destruct w; repeat (try (left; reflexivity); right).
    Qed.
End m_special_chapter.

Instance special_chapter_EqDec : EqDec special_chapter := {
  eqb := special_chapter_beq;
  eqb_correct := m_special_chapter.eqb_correct;
}.

Instance special_chapter_OrdDec : OrdDec special_chapter special_chapter_EqDec := enumOrdDec special_chapter special_chapter_EqDec all_special_chapters m_special_chapter.complete.

Inductive flag : Set :=
    |PermanentSkillReduction 
    |StrengthPotionActive 
    |FoughtElix 
    |LimbDeath 
    |ReceivedCrystalStarPendant 
    |Knowledge01 
    |Knowledge02 
    |Knowledge03 
    |Knowledge04 
    |Special01 
    |Special02 
    |Special03 
    |Special04 
    |Poisonned2 
    |HadCombat 
    |PermanentSkillReduction2 
    |PotentStrengthPotionActive 
    .

Definition all_flags: list flag :=
    [PermanentSkillReduction 
    ;StrengthPotionActive 
    ;FoughtElix 
    ;LimbDeath 
    ;ReceivedCrystalStarPendant 
    ;Knowledge01 
    ;Knowledge02 
    ;Knowledge03 
    ;Knowledge04 
    ;Special01 
    ;Special02 
    ;Special03 
    ;Special04 
    ;Poisonned2 
    ;HadCombat 
    ;PermanentSkillReduction2 
    ;PotentStrengthPotionActive 
    ].

Scheme Equality for flag.

Module m_flag.
    Lemma eqb_correct: forall x y : flag, x = y <-> flag_beq x y = true.
    Proof.
        destruct x, y; split; intro; try discriminate; auto.
    Qed.

    Lemma complete: forall x, In x all_flags.
    Proof.
        intros.
        unfold all_flags.
        destruct x; try destruct w; repeat (try (left; reflexivity); right).
    Qed.
End m_flag.

Instance flag_EqDec : EqDec flag := {
  eqb := flag_beq;
  eqb_correct := m_flag.eqb_correct;
}.

Instance flag_OrdDec : OrdDec flag flag_EqDec := enumOrdDec flag flag_EqDec all_flags m_flag.complete.

Inductive kai_level : Set :=
    |Novice
    |Intuite
    |Doan
    |Acolyte
    |Initiate
    |Aspirant
    |Guardian
    |Warmarn
    |Savant
    |Master
    .

Definition all_kai_levels  :=
    [Novice
    ;Intuite
    ;Doan
    ;Acolyte
    ;Initiate
    ;Aspirant
    ;Guardian
    ;Warmarn
    ;Savant
    ;Master
    ].

Scheme Equality for kai_level.

Module m_kai_level.
    Lemma eqb_correct: forall x y : kai_level, x = y <-> kai_level_beq x y = true.
    Proof.
        destruct x, y; split; intro; try discriminate; auto.
    Qed.

    Lemma complete: forall x, In x all_kai_levels.
    Proof.
        intros.
        unfold all_kai_levels.
        destruct x; try destruct w; repeat (try (left; reflexivity); right).
    Qed.
End m_kai_level.

Instance kai_level_EqDec : EqDec kai_level := {
  eqb := kai_level_beq;
  eqb_correct := m_kai_level.eqb_correct;
}.

Instance kai_level_OrdDec : OrdDec kai_level kai_level_EqDec := enumOrdDec kai_level kai_level_EqDec all_kai_levels m_kai_level.complete.

Definition level_to_nat (lvl: kai_level): nat :=
    match lvl with
    |Novice => 0
    |Intuite => 1
    |Doan => 2
    |Acolyte => 3
    |Initiate => 4
    |Aspirant => 5
    |Guardian => 6
    |Warmarn => 7
    |Savant => 8
    |Master => 9
    end.

Definition nat_to_level (n: nat): kai_level :=
   match n with
      | 0 => Novice
      | 1 => Novice
      | 2 => Intuite
      | 3 => Doan
      | 4 => Acolyte
      | 5 => Initiate
      | 6 => Aspirant
      | 7 => Guardian
      | 8 => Warmarn
      | 9 => Savant
      | _ => Master
      end.

Inductive bool_cond: Set :=
    |HasDiscipline: discipline -> bool_cond
    |Not : bool_cond -> bool_cond
    |COr : bool_cond -> bool_cond -> bool_cond
    |CAnd : bool_cond -> bool_cond -> bool_cond
    |HasItem : item -> nat -> bool_cond
    |Always : bool -> bool_cond
    |HasEndurance : nat -> bool_cond
    |HasFlag : flag -> bool_cond
    |HasLevel : kai_level -> bool_cond
    .

Definition skill := Z.

Definition endurance := nat.

Definition cid := nat.

Inductive fight_modifier : Set :=
    |Undead
    |MindblastImmune
    |Timed : nat -> fight_modifier -> fight_modifier
    |CombatBonus: skill -> fight_modifier
    |BareHanded
    |FakeFight: cid -> fight_modifier
    |EnemyMindblast
    |ForceEMindblast
    |PlayerInvulnerable
    |DoubleDamage
    |Evaded: cid -> fight_modifier
    |OnDamage: cid -> fight_modifier
    |OnNotYetWon: cid -> fight_modifier
    |MultiFight
    |EnemyInvulnerable
    |OnLose: cid -> fight_modifier
    |StopFight: cid -> fight_modifier
    |Dpr: endurance -> fight_modifier
    |NoPotion
    |Poisonous: Qc -> fight_modifier
    .

Module FM.

    Fixpoint fm_eqb (a b: fight_modifier): bool :=
    match (a, b) with
    | (Undead, Undead) => true
    | (MindblastImmune, MindblastImmune) => true
    | (BareHanded, BareHanded) => true
    | (EnemyMindblast, EnemyMindblast) => true
    | (ForceEMindblast, ForceEMindblast) => true
    | (PlayerInvulnerable, PlayerInvulnerable) => true
    | (DoubleDamage, DoubleDamage) => true
    | (MultiFight, MultiFight) => true
    | (EnemyInvulnerable, EnemyInvulnerable) => true
    | (NoPotion, NoPotion) => true
    | (OnLose na, OnLose nb) => Nat.eqb na nb
    | (StopFight na, StopFight nb) => Nat.eqb na nb
    | (Dpr na, Dpr nb) => Nat.eqb na nb
    | (Timed xa na, Timed xb nb) => fm_eqb na nb && Nat.eqb xa xb
    | (CombatBonus na, CombatBonus nb) => Z.eqb na nb
    | (FakeFight na, FakeFight nb) => Nat.eqb na nb
    | (Poisonous na, Poisonous nb) => Qc_eq_bool na nb
    | (Evaded na, Evaded nb) => Nat.eqb na nb
    | (OnDamage na, OnDamage nb) => Nat.eqb na nb
    | (OnNotYetWon na, OnNotYetWon nb) => Nat.eqb na nb
    | _ => false
    end.
    
    Lemma eqb_correct : forall (x y: fight_modifier), (x = y) <-> fm_eqb x y = true.
    Proof.
    split; intros.
    * subst. induction y; simpl; auto; try rewrite Nat.eqb_refl; try Lia.lia. 
        - apply Bool.andb_true_iff.
        split; auto.
        - apply RatioL.rat_eqb_correct.
        reflexivity.
    * unfold fm_eqb in H.
        generalize dependent y.
        induction x; intros;
        destruct y; simpl in H; auto; try discriminate; try (apply Bool.andb_true_iff in H; destruct H);
        try (apply NatLemma.nat_eqb_correct in H; subst; reflexivity ).
        (* timed *)
        apply NatLemma.nat_eqb_correct in H0. subst; f_equal. apply IHx. simpl. apply H.
        apply IntLemma.int_eqb_correct in H. subst. reflexivity.
        apply RatioL.rat_eqb_correct in H.
        subst.
        reflexivity.
    Qed.
End FM.

Inductive fight_details: Set :=
  Details: skill -> endurance -> list fight_modifier -> fight_details
  .

Inductive slot : Set :=
    |SWeapon
    |SBackpack
    |SSpecial
    |SPouch
    .

Scheme Equality for slot.

Inductive can_hunt : Set :=
    |Hunt
    |NoHunt
    .

Scheme Equality for can_hunt.

Inductive simple_outcome : Set :=
    |DamagePlayer : endurance -> simple_outcome
    |HealPlayer : endurance -> simple_outcome
    |FullHeal
    |HalfHeal
    |GainItem: item -> nat -> simple_outcome
    |LoseItem: item -> nat -> simple_outcome
    |LoseItemKind: list slot -> simple_outcome
    |MustEat: can_hunt -> simple_outcome
    |StoreEquipment
    |SetFlag: flag -> simple_outcome
    |ClearFlag: flag -> simple_outcome
    .

Inductive chapter_outcome: Set :=
    | Fight : fight_details -> chapter_outcome -> chapter_outcome
    | OneRound : fight_details -> chapter_outcome -> chapter_outcome -> chapter_outcome -> chapter_outcome
    | Randomly : Proba chapter_outcome -> chapter_outcome
    | Conditionally: list (bool_cond * chapter_outcome) -> chapter_outcome
    | Simple: list simple_outcome -> chapter_outcome -> chapter_outcome
    | Goto: cid -> chapter_outcome
    | LoseItemFrom: slot -> nat -> chapter_outcome -> chapter_outcome
    | GameLost
    | GameWon
    .

Inductive decision : Set :=
  | Decisions: list decision -> decision
  | retrieve_equipment: decision -> decision
  | can_take: item -> nat -> decision -> decision
  | can_buy: item -> nat -> decision -> decision
  | can_sell: item -> nat -> decision -> decision
  | conditional: bool_cond -> decision -> decision
  | special: special_chapter -> decision
  | none: chapter_outcome -> decision
  | evade_fight: nat -> cid -> fight_details -> chapter_outcome -> decision
  | after_combat: decision -> decision
  | remove_item_from: slot -> nat -> decision -> decision
  .


Definition Items := Mp item nat.
Definition FlagSet := St flag.
Definition DisciplineSet := St discipline.
Definition StandardDisciplineSet (s: DisciplineSet) := s_check Healing s.

Definition add_item (i: item) (q: nat) (items: Items) :=
  match i with
  | Gold => mps.insert_with (fun pq => Nat.min 50 (pq + q)) Gold q items
  | _ => mps.insert_with (fun pq => pq + q)%nat i q items
  end.

Definition rm_item (i: item) (q: nat) (items: Items) :=
   mps.delete i items.

Definition item_slot (i: item): slot :=
  match i with
    | Weapon _ => SWeapon
    | Backpack => SSpecial
    | StrengthPotion4 => SBackpack
    | Shield => SSpecial
    | BodyArmor => SSpecial
    | Potion2Hp => SBackpack
    | Potion4Hp => SBackpack
    | Potion5Hp => SBackpack
    | Potion6Hp => SBackpack
    | StrengthPotion => SBackpack
    | GenSpecial _ => SSpecial
    | GenBackpack _ => SBackpack
    | Meal => SBackpack
    | Gold => SPouch
    | Laumspur => SBackpack
    | Helmet => SSpecial
    end.

Definition lose_all (i: item) (items: Items) :=
    mps.delete i items.

Definition all_special_items : list item :=
    [ Backpack
    ; Shield
    ; BodyArmor
    ; Helmet
    ] ++ map GenSpecial all_sslot
    .

Definition all_backpack_items : list item :=
    [ StrengthPotion4
    ; Potion2Hp
    ; Potion4Hp
    ; Potion5Hp
    ; Potion6Hp
    ; StrengthPotion
    ; Meal
    ; Laumspur
    ] ++ map GenBackpack all_sslot
    .

Definition all_weapon_items : list item := map Weapon all_weapons.

Definition all_pouch_items : list item := [Gold].

Lemma all_items_complete: forall i, In i all_items.
Proof.
    intro i.
    unfold all_items.
    simpl.
    destruct i; try tauto.
    - destruct w; try tauto.
    - destruct s; try tauto.
    - destruct s; try tauto.
Qed.

Lemma all_items_per_slot: forall i,
        In i all_special_items \/ In i all_backpack_items \/ In i all_pouch_items \/ In i all_weapon_items.
Proof.
    intro i.
    unfold all_weapon_items, all_special_items, all_backpack_items, all_pouch_items.
    simpl.
    destruct i; try tauto.
    - destruct w; try tauto.
    - destruct s; try tauto.
    - destruct s; try tauto.
Qed.

Definition slot_items (s: slot): list item :=
    match s with
    |SWeapon => all_weapon_items
    |SBackpack => all_backpack_items
    |SSpecial => all_special_items
    |SPouch => all_pouch_items
    end.

Definition lose_all_slot (s: slot) (items: Items): Items :=
    fold_right (fun i itms => mps.delete i itms) items (slot_items s).