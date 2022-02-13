use crate::mini::Discipline;
use crate::mini::Flag;
use crate::Item;
use rug::Rational;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Chapter {
  pub title: String,
  pub desc: String,
  pub pchoice: Decision,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Rounds(pub u8);
#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Price(pub u8);
#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct ChapterId(pub u16);
#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Endurance(pub i8);
#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct CombatSkill(pub u8);

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Decision {
  Decisions(Vec<(String, Decision)>),
  RetrieveEquipment(Box<Decision>),
  CanTake(Item, u8, Box<Decision>),
  Canbuy(Item, Price, Box<Decision>),
  Cansell(Item, Price, Box<Decision>),
  Conditional(BoolCond, Box<Decision>),
  Special(SpecialChapter),
  None(ChapterOutcome),
  EvadeFight(Rounds, ChapterId, FightDetails, ChapterOutcome),
  AfterCombat(Box<Decision>),
  RemoveItemFrom(Slot, u8, Box<Decision>),
  LoseItemFrom(Slot, u8, Box<Decision>),
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum SpecialChapter {
  Cartwheel,
  Portholes,
  B05S127,
  B05S357,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Slot {
  Weapon,
  Backpack,
  Special,
  Pouch,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum ChapterOutcome {
  Fight(FightDetails, Box<ChapterOutcome>),
  OneRound(
    FightDetails,
    Box<ChapterOutcome>,
    Box<ChapterOutcome>,
    Box<ChapterOutcome>,
  ),
  Randomly(Vec<(Rational, ChapterOutcome)>),
  Conditionally(Vec<(BoolCond, ChapterOutcome)>),
  Simple(Vec<SimpleOutcome>, Box<ChapterOutcome>),
  Goto(ChapterId),
  GameLost,
  GameWon,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum SimpleOutcome {
  DamagePlayer(Endurance),
  HealPlayer(Endurance),
  FullHeal,
  HalfHeal,
  GainItem(Item, u8),
  LoseItem(Item, u8),
  LoseItemKind(Vec<Slot>),
  MustEat(CanHunt),
  StoreEquipment,
  SetFlag(Flag),
  ClearFlag(Flag),
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum CanHunt {
  Hunt,
  NoHunt,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum BoolCond {
  HasDiscipline(Discipline),
  Not(Box<BoolCond>),
  COr(Box<BoolCond>, Box<BoolCond>),
  CAnd(Box<BoolCond>, Box<BoolCond>),
  HasItem(Item, u8),
  Always(bool),
  HasEndurance(Endurance),
  HasFlag(Flag),
  HasLevel(KaiLevel),
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct FightDetails {
  pub opponent: String,
  pub combat_skill: CombatSkill,
  pub endurance: Endurance,
  pub fight_mod: Vec<FightModifier>,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum FightModifier {
  Undead,
  MindblastImmune,
  Timed(u8, Box<FightModifier>),
  CombatBonus(CombatSkill),
  BareHanded,
  FakeFight(ChapterId),
  EnemyMindblast,
  ForceEMindblast,
  PlayerInvulnerable,
  DoubleDamage,
  Evaded(ChapterId),
  OnDamage(ChapterId),
  OnNotYetWon(ChapterId),
  MultiFight,
  EnemyInvulnerable,
  OnLose(ChapterId),
  StopFight(ChapterId),
  Dpr(Endurance),
  NoPotion,
  Poisonous(Rational),
}

#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Serialize, Deserialize)]
pub enum KaiLevel {
  Novice,
  Intuite,
  Doan,
  Acolyte,
  Initiate,
  Aspirant,
  Guardian,
  Warmarn,
  Savant,
  Master,
}
