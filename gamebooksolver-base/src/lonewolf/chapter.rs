use super::mini::{Discipline, Flag, Item, Slot};
use serde::{Deserialize, Serialize};
use std::hash::Hash;

#[derive(PartialEq, Eq, Debug, Deserialize, Clone)]
pub struct Chapter<P> {
  pub title: String,
  pub desc: String,
  pub pchoice: Decision<P>,
}

#[derive(PartialEq, Eq, Debug, Deserialize, Hash, Clone, Copy, PartialOrd, Ord, Serialize)]
pub struct Rounds(pub u8);
#[derive(PartialEq, Eq, Debug, Deserialize, Hash, Clone, Copy, PartialOrd, Ord, Serialize)]
pub struct Price(pub u8);
#[derive(PartialEq, Eq, Debug, Deserialize, Hash, Clone, Copy, PartialOrd, Ord, Serialize)]
pub struct ChapterId(pub u16);
#[derive(PartialEq, Eq, Debug, Deserialize, Hash, Clone, Copy, PartialOrd, Ord, Serialize)]
pub struct Endurance(pub i8);
#[derive(PartialEq, Eq, Debug, Deserialize, Hash, Clone, Copy, PartialOrd, Ord, Serialize)]
pub struct CombatSkill(pub i8);

impl Endurance {
  pub fn damage(&self, v: u8) -> Self {
    Endurance(std::cmp::max(self.0 - v as i8, 0))
  }

  pub const fn dead(&self) -> bool {
    self.0 <= 0
  }
}

#[derive(PartialEq, Eq, Debug, Deserialize, Clone)]
pub enum Decision<P> {
  Decisions(Vec<(String, Decision<P>)>),
  RetrieveEquipment(Box<Decision<P>>),
  CanTake(Item, u8, Box<Decision<P>>),
  Canbuy(Item, Price, Box<Decision<P>>),
  Cansell(Item, Price, Box<Decision<P>>),
  Conditional(BoolCond, Box<Decision<P>>),
  Special(SpecialChapter),
  #[serde(rename = "NoDecision")]
  None(ChapterOutcome<P>),
  EvadeFight(Rounds, ChapterId, FightDetails<P>, ChapterOutcome<P>),
  AfterCombat(Box<Decision<P>>),
  RemoveItemFrom(Slot, u8, Box<Decision<P>>),
  LoseItemFrom(Slot, u8, Box<Decision<P>>),
}

#[derive(PartialEq, Eq, Debug, Deserialize, Clone, Copy)]
pub enum SpecialChapter {
  Cartwheel,
  Portholes,
  B05S127,
  B05S357,
}

#[derive(PartialEq, Eq, Debug, Deserialize, Clone)]
pub enum ChapterOutcome<P> {
  Fight(FightDetails<P>, Box<ChapterOutcome<P>>),
  OneRound(
    FightDetails<P>,
    Box<ChapterOutcome<P>>,
    Box<ChapterOutcome<P>>,
    Box<ChapterOutcome<P>>,
  ),
  Randomly(Vec<(P, ChapterOutcome<P>)>),
  Conditionally(Vec<(BoolCond, ChapterOutcome<P>)>),
  Simple(Vec<SimpleOutcome>, Box<ChapterOutcome<P>>),
  Goto(ChapterId),
  GameLost,
  GameWon,
}

#[derive(PartialEq, Eq, Debug, Deserialize, Clone)]
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

#[derive(PartialEq, Eq, Debug, Deserialize, Clone, Copy)]
pub enum CanHunt {
  Hunt,
  NoHunt,
}

#[derive(PartialEq, Eq, Debug, Deserialize, Clone)]
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

#[derive(PartialEq, Eq, Debug, Deserialize, Clone)]
pub struct FightDetails<P> {
  pub opponent: String,
  #[serde(rename = "fcombatSkill")]
  pub combat_skill: CombatSkill,
  #[serde(rename = "fendurance")]
  pub endurance: Endurance,
  #[serde(rename = "fightMod")]
  pub fight_mod: Vec<FightModifier<P>>,
}

#[derive(PartialEq, Eq, Debug, Deserialize, Hash, Clone, PartialOrd, Ord)]
pub enum FightModifier<P> {
  Undead,
  MindblastImmune,
  Timed(u8, Box<FightModifier<P>>),
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
  #[serde(rename = "DPR")]
  Dpr(Endurance),
  NoPotion,
  Poisonous(P),
}

#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Deserialize, Clone, Copy)]
#[repr(u8)]
pub enum KaiLevel {
  Novice = 1,
  Intuite = 2,
  Doan = 3,
  Acolyte = 4,
  Initiate = 5,
  Aspirant = 6,
  Guardian = 7,
  Warmarn = 8,
  Savant = 9,
  Master = 10,
}
