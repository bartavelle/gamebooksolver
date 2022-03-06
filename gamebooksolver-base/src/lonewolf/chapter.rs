use super::mini::{Discipline, Flag, Item, Slot};
use serde::{Deserialize, Serialize};
use std::hash::Hash;

#[derive(PartialEq, Eq, Debug, Deserialize, Clone)]
pub struct Chapter<P> {
  pub title: String,
  pub desc: String,
  pub pchoice: Decision<P>,
}

impl<P: Clone> Chapter<P> {
  pub fn map_proba<F, P2>(self, f: &F) -> Chapter<P2>
  where
    F: Fn(&P) -> P2,
  {
    Chapter {
      title: self.title,
      desc: self.desc,
      pchoice: self.pchoice.map_proba(f),
    }
  }
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

impl<P: Clone> Decision<P> {
  pub fn map_proba<F, P2>(self, f: &F) -> Decision<P2>
  where
    F: Fn(&P) -> P2,
  {
    use Decision::*;
    let convert = |b: Box<Decision<P>>| -> Box<Decision<P2>> {
      Box::new({
        let b2: &Decision<P> = &b;
        b2.clone().map_proba(f)
      })
    };
    match self {
      Decisions(v) => Decisions(
        v.into_iter()
          .map(|(t, sub)| (t, sub.map_proba(f)))
          .collect(),
      ),
      RetrieveEquipment(nxt) => RetrieveEquipment(convert(nxt)),
      CanTake(i, q, nxt) => CanTake(i, q, convert(nxt)),
      Canbuy(i, p, nxt) => Canbuy(i, p, convert(nxt)),
      Cansell(i, p, nxt) => Cansell(i, p, convert(nxt)),
      Conditional(bc, nxt) => Conditional(bc, convert(nxt)),
      Special(s) => Special(s),
      None(co) => None(co.map_proba(f)),
      EvadeFight(r, cid, fd, co) => EvadeFight(r, cid, fd.map_proba(f), co.map_proba(f)),
      AfterCombat(nxt) => AfterCombat(convert(nxt)),
      RemoveItemFrom(sl, q, nxt) => RemoveItemFrom(sl, q, convert(nxt)),
      LoseItemFrom(sl, q, nxt) => LoseItemFrom(sl, q, convert(nxt)),
    }
  }
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

impl<P: Clone> ChapterOutcome<P> {
  pub fn map_proba<F, P2>(self, f: &F) -> ChapterOutcome<P2>
  where
    F: Fn(&P) -> P2,
  {
    let convert = |b: Box<ChapterOutcome<P>>| -> Box<ChapterOutcome<P2>> {
      Box::new({
        let b2: &ChapterOutcome<P> = &b;
        b2.clone().map_proba(f)
      })
    };
    use ChapterOutcome::*;
    match self {
      Fight(fd, co) => Fight(fd.map_proba(f), convert(co)),
      OneRound(fd, cl, ce, cw) => OneRound(fd.map_proba(f), convert(cl), convert(ce), convert(cw)),
      Randomly(lst) => Randomly(
        lst
          .into_iter()
          .map(|(p, co2)| (f(&p), co2.map_proba(f)))
          .collect(),
      ),
      Conditionally(lst) => Conditionally(
        lst
          .into_iter()
          .map(|(c, co2)| (c, co2.map_proba(f)))
          .collect(),
      ),
      Simple(sos, nxt) => Simple(sos, convert(nxt)),
      Goto(cid) => Goto(cid),
      GameLost => GameLost,
      GameWon => GameWon,
    }
  }
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

impl<P: Clone> FightDetails<P> {
  pub fn map_proba<F, P2>(self, f: &F) -> FightDetails<P2>
  where
    F: Fn(&P) -> P2,
  {
    FightDetails {
      opponent: self.opponent,
      combat_skill: self.combat_skill,
      endurance: self.endurance,
      fight_mod: self.fight_mod.into_iter().map(|m| m.map_proba(f)).collect(),
    }
  }
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

impl<P: Clone> FightModifier<P> {
  pub fn map_proba<F, P2>(self, f: &F) -> FightModifier<P2>
  where
    F: Fn(&P) -> P2,
  {
    use FightModifier::*;
    match self {
      Undead => Undead,
      MindblastImmune => MindblastImmune,
      Timed(t, nxt) => Timed(t, {
        let nxt2: &FightModifier<P> = &nxt;
        Box::new(nxt2.clone().map_proba(f))
      }),
      CombatBonus(cs) => CombatBonus(cs),
      BareHanded => BareHanded,
      FakeFight(cid) => FakeFight(cid),
      EnemyMindblast => EnemyMindblast,
      ForceEMindblast => ForceEMindblast,
      PlayerInvulnerable => PlayerInvulnerable,
      DoubleDamage => DoubleDamage,
      Evaded(cid) => Evaded(cid),
      OnDamage(cid) => OnDamage(cid),
      OnNotYetWon(cid) => OnNotYetWon(cid),
      MultiFight => MultiFight,
      EnemyInvulnerable => EnemyInvulnerable,
      OnLose(cid) => OnLose(cid),
      StopFight(cid) => StopFight(cid),
      Dpr(e) => Dpr(e),
      NoPotion => NoPotion,
      Poisonous(p) => Poisonous(f(&p)),
    }
  }
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
