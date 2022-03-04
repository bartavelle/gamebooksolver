use crate::solver::base::{cbor_option, e_cbor_option, ChoppedSolution};
use minicbor::decode::{Decode, Decoder, Error};
use minicbor::encode::{self, Encode, Encoder, Write};
use serde::de::Visitor;
use serde::Deserializer;
use serde::{Deserialize, Serialize};
use std::hash::Hash;
use structopt::StructOpt;

#[derive(Debug, PartialEq, Eq)]
pub struct SolutionDump {
  pub soldesc: SolDesc,
  pub content: Vec<(NextStep, ChoppedSolution<NextStep>)>,
}

impl<'b> Decode<'b> for SolutionDump {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    let ln = d.array()?;
    if ln != Some(3) {
      return Err(Error::Message("invalid length for SolutionDump"));
    }
    d.u8()?;
    let soldesc = d.decode()?;
    let rcontent: Result<Vec<_>, Error> = d.array_iter()?.collect();
    Ok(SolutionDump {
      soldesc,
      content: rcontent?,
    })
  }
}

impl Encode for SolutionDump {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.array(3)?.u8(0)?.encode(&self.soldesc)?;
    e.array(self.content.len() as u64)?;
    for c in &self.content {
      e.encode(c)?;
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SolDesc {
  pub finalchapters: Vec<u16>,
  pub ccst: CharacterConstant,
  pub cvar: CVarState,
}

impl<'b> Decode<'b> for SolDesc {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    let ln = d.array()?;
    if ln != Some(4) {
      return Err(Error::Message("invalid length for SolDesc"));
    }
    d.u8()?;
    let finalchaptersr: Result<Vec<u16>, Error> = d.array_iter()?.collect();
    let finalchapters = finalchaptersr?;
    let ccst = d.decode()?;
    let cvar = d.decode()?;
    Ok(SolDesc {
      finalchapters,
      ccst,
      cvar,
    })
  }
}
impl Encode for SolDesc {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.array(4)?.u8(0)?;
    e.array(self.finalchapters.len() as u64)?;
    for fc in &self.finalchapters {
      e.encode(fc)?;
    }
    e.encode(&self.ccst)?;
    e.encode(&self.cvar)?;
    Ok(())
  }
}

impl SolDesc {
  pub fn cvariable(&self) -> CharacterVariable {
    let mut cvar = CharacterVariable::new(self.ccst.maxendurance);
    cvar.cequipment.add_item(&Item::Backpack, 1);
    cvar.cequipment.set_gold(self.cvar.gold);
    let itms = self
      .cvar
      .items
      .as_ref()
      .expect("default items not yet implemented");
    for (i, q) in itms {
      cvar.cequipment.add_item(i, *q);
    }
    for f in &self.cvar.flags {
      cvar.set_flag(*f);
    }
    cvar.curendurance = max_hp(&self.ccst, &cvar);
    cvar
  }
}

#[derive(Debug, PartialEq, Eq, Clone, StructOpt)]
pub struct CharacterConstant {
  pub maxendurance: i8,
  pub combat_skill: u8,
  pub discipline: Vec<Discipline>,
  pub bookid: Book,
}

impl<'b> Decode<'b> for CharacterConstant {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    let ln = d.array()?;
    if ln != Some(5) {
      return Err(Error::Message("invalid length for CharacterConstant"));
    }
    d.u8()?;
    let maxendurance = d.i8()?;
    let combat_skill = d.u8()?;
    let discipline: Result<Vec<Discipline>, Error> = d.array_iter()?.collect();
    let bookid = d.decode()?;

    Ok(CharacterConstant {
      maxendurance,
      combat_skill,
      discipline: discipline?,
      bookid,
    })
  }
}

impl Encode for CharacterConstant {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.array(5)?
      .u8(0)?
      .i8(self.maxendurance)?
      .u8(self.combat_skill)?;
    e.array(self.discipline.len() as u64)?;
    for d in &self.discipline {
      e.encode(d)?;
    }
    e.encode(self.bookid)?;
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct CVarState {
  pub items: Option<Vec<(Item, i64)>>,
  pub gold: u8,
  pub flags: Vec<Flag>,
}

impl<'b> Decode<'b> for CVarState {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    let ln = d.array()?;
    if ln != Some(4) {
      return Err(Error::Message("invalid length for CVarState"));
    }
    d.u8()?;
    let ritems: Result<Option<Vec<(Item, i64)>>, Error> =
      cbor_option(d, |d| d.array_iter().and_then(|i| i.collect()));
    let items = ritems?;

    let gold = d.u8()?;
    let rflags: Result<Vec<Flag>, Error> = d.array_iter()?.collect();
    Ok(CVarState {
      items,
      gold,
      flags: rflags?,
    })
  }
}

impl Encode for CVarState {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.array(4)?.u8(0)?;
    e_cbor_option(e, self.items.as_ref(), |e, lst| {
      e.array(lst.len() as u64)?;
      for l in lst {
        e.encode(l)?;
      }
      Ok(())
    })?;
    e.u8(self.gold)?;
    e.array(self.flags.len() as u64)?;
    for f in &self.flags {
      e.encode(f)?;
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub enum NextStep {
  HasLost(u16),
  HasWon(CharacterVariable),
  NewChapter(u16, CharacterVariable),
}

impl std::fmt::Display for NextStep {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      NextStep::HasLost(cid) => write!(f, "lost:{}", cid),
      NextStep::HasWon(cv) => write!(f, "won:{}", cv),
      NextStep::NewChapter(cid, cv) => write!(f, "c:{} {}", cid, cv),
    }
  }
}

impl<'b> Decode<'b> for NextStep {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    let ln = d.array()?;
    let tg = d.u8()?;
    match (tg, ln) {
      (0, Some(2)) => d.u16().map(NextStep::HasLost),
      (1, Some(2)) => d.decode().map(NextStep::HasWon),
      (2, Some(3)) => {
        let cid = d.u16()?;
        let cv = d.decode()?;
        Ok(NextStep::NewChapter(cid, cv))
      }
      _ => Err(Error::Message("Invalid variant for NextStep")),
    }
  }
}

impl Encode for NextStep {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    match self {
      NextStep::HasLost(cid) => {
        e.array(2)?.u8(0)?.u16(*cid)?;
      }
      NextStep::HasWon(score) => {
        e.array(2)?.u8(1)?.encode(score)?;
      }
      NextStep::NewChapter(cid, cv) => {
        e.array(3)?.u8(2)?.u16(*cid)?.encode(cv)?;
      }
    }
    Ok(())
  }
}

impl NextStep {
  pub fn chapter(&self) -> Option<u16> {
    match self {
      NextStep::HasLost(c) => Some(*c),
      NextStep::HasWon(_) => None,
      NextStep::NewChapter(c, _) => Some(*c),
    }
  }
  pub fn cvar(&self) -> Option<&CharacterVariable> {
    match self {
      NextStep::HasLost(_) => None,
      NextStep::HasWon(s) => Some(s),
      NextStep::NewChapter(_, s) => Some(s),
    }
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, PartialOrd, Ord)]
pub struct Equipment(pub u64);

#[derive(PartialEq, Eq, Debug, Deserialize, Clone, Copy)]
pub enum Slot {
  #[serde(rename = "WeaponSlot")]
  Weapon,
  #[serde(rename = "BackpackSlot")]
  Backpack,
  #[serde(rename = "SpecialSlot")]
  Special,
  #[serde(rename = "PouchSlot")]
  Pouch,
}

impl Equipment {
  fn set_gold(&mut self, q: u8) {
    self.0 = (self.0 & 0xf00fffffffffffff) | ((q as u64 & 0xff) << 52);
  }
  fn set_laumspur(&mut self, q: u8) {
    self.0 = (self.0 & 0x0fffffffffffffff) | ((q as u64 & 0xf) << 60);
  }
  fn set_meal(&mut self, q: u8) {
    self.0 = (self.0 & 0xfff0ffffffffffff) | ((q as u64 & 0xf) << 48);
  }
  fn get_gold(&self) -> u8 {
    (self.0 >> 52) as u8
  }
  fn get_meal(&self) -> u8 {
    (self.0 >> 48) as u8 & 0xf
  }
  fn get_laumspur(&self) -> u8 {
    (self.0 >> 60) as u8 & 0xf
  }
  pub fn add_item(&mut self, i: &Item, q: i64) {
    if q <= 0 {
      return;
    }
    match i {
      Item::Gold => {
        self.set_gold(std::cmp::min(50, q as u8 + self.get_gold()));
      }
      Item::Meal => {
        self.set_meal(q as u8 + self.get_meal());
      }
      Item::Laumspur => {
        self.set_laumspur(q as u8 + self.get_laumspur());
      }
      _ => {
        let idx = i.get_idx();
        self.0 |= 1 << idx;
      }
    }
  }
  pub fn del_item(&mut self, i: &Item, q: i64) {
    if q <= 0 {
      return;
    }
    match i {
      Item::Gold => {
        let cur = self.get_gold();
        if cur <= q as u8 {
          self.set_gold(0)
        } else {
          self.set_gold(cur - q as u8)
        }
      }
      Item::Meal => {
        let cur = self.get_meal();
        if cur <= q as u8 {
          self.set_meal(0)
        } else {
          self.set_meal(cur - q as u8)
        }
      }
      Item::Laumspur => {
        let cur = self.get_laumspur();
        if cur <= q as u8 {
          self.set_laumspur(0)
        } else {
          self.set_laumspur(cur - q as u8)
        }
      }
      _ => {
        let idx = i.get_idx();
        self.0 &= !(1 << idx);
      }
    }
  }
  pub fn get_item_count(&self, i: &Item) -> u8 {
    match i {
      Item::Gold => self.get_gold(),
      Item::Meal => self.get_meal(),
      Item::Laumspur => self.get_laumspur(),
      _ => {
        let idx = i.get_idx();
        if self.0 & (1 << idx) != 0 {
          1
        } else {
          0
        }
      }
    }
  }
  pub fn del_slot(&mut self, s: Slot) {
    match s {
      Slot::Pouch => self.set_gold(0),
      Slot::Special => {
        for i in Item::VALUES.iter().filter(|i| i.slot() == Slot::Special) {
          self.del_item(i, 99)
        }
      }
      Slot::Backpack => {
        for i in Item::VALUES.iter().filter(|i| i.slot() == Slot::Backpack) {
          self.del_item(i, 99)
        }
      }
      Slot::Weapon => {
        for w in Weapon::VALUES.iter() {
          self.del_item(&Item::Weapon(*w), 1)
        }
      }
    }
  }

  pub fn in_backpack(&self) -> Vec<Item> {
    Item::VALUES
      .iter()
      .flat_map(|i| {
        if i.slot() == Slot::Backpack {
          let cnt = self.get_item_count(i);
          let mut o = Vec::new();
          o.resize(cnt as usize, *i);
          o
        } else {
          Vec::new()
        }
      })
      .collect()
  }

  pub fn items(&self) -> Vec<(Item, u8)> {
    Item::VALUES
      .iter()
      .filter_map(|i| {
        let cnt = self.get_item_count(i);
        if cnt > 0 {
          Some((*i, cnt))
        } else {
          None
        }
      })
      .collect()
  }

  pub fn has_item(&self, i: &Item, q: u8) -> bool {
    self.get_item_count(i) >= q
  }

  pub fn has_itemb(&self, i: &Item) -> bool {
    self.has_item(i, 1)
  }

  pub fn weapons(&self) -> Vec<Weapon> {
    Weapon::VALUES
      .iter()
      .copied()
      .filter(|w| self.has_item(&Item::Weapon(*w), 1))
      .collect()
  }
}

impl<'b> Decode<'b> for Equipment {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    d.u64().map(Equipment)
  }
}

impl Encode for Equipment {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.u64(self.0)?;
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, PartialOrd, Ord, Copy)]
pub struct Flags(pub u32);

impl Flags {
  pub fn set(&mut self, f: Flag) {
    self.0 |= 1 << (f as usize);
  }

  pub fn unset(&mut self, f: Flag) {
    self.0 &= !(1 << (f as usize));
  }

  pub const fn setn(self, f: Flag) -> Flags {
    Flags(self.0 | 1 << (f as usize))
  }

  pub const fn has(&self, f: Flag) -> bool {
    self.0 & (1 << (f as usize)) > 0
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, PartialOrd, Ord)]
pub struct CharacterVariable {
  pub curendurance: i8,
  pub flags: Flags,
  pub cequipment: Equipment,
  pub cprevequipment: Equipment,
}

impl std::fmt::Display for CharacterVariable {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(
      f,
      "{} {:?} {:?} {:?}",
      self.curendurance,
      self.flags,
      self.cequipment.items(),
      self.cprevequipment.items()
    )
  }
}

impl CharacterVariable {
  pub fn new(endurance: i8) -> Self {
    CharacterVariable {
      curendurance: endurance,
      flags: Flags(0),
      cequipment: Equipment(0),
      cprevequipment: Equipment(0),
    }
  }

  pub fn add_item(&mut self, i: &Item, q: i64) {
    self.cequipment.add_item(i, q);
  }

  pub fn set_flag(&mut self, f: Flag) {
    self.flags.set(f)
  }

  pub fn damage(&mut self, dmg: i8) {
    self.curendurance = std::cmp::max(0, self.curendurance - dmg);
  }

  pub fn heal(&mut self, ccst: &CharacterConstant, hp: i8) {
    self.curendurance = std::cmp::min(max_hp(ccst, self), self.curendurance + hp)
  }
}

impl<'b> Decode<'b> for CharacterVariable {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    let ln = d.array()?;
    if ln != Some(5) {
      return Err(Error::Message("invalid length for CharacterVariable"));
    }
    d.u8()?;
    let curendurance = d.i8()?;
    let flags = Flags(d.u32()?);
    let cequipment = d.decode()?;
    let cprevequipment = d.decode()?;
    Ok(CharacterVariable {
      curendurance,
      flags,
      cequipment,
      cprevequipment,
    })
  }
}

impl Encode for CharacterVariable {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.array(5)?
      .u8(0)?
      .i8(self.curendurance)?
      .u32(self.flags.0)?
      .encode(self.cequipment)?
      .encode(self.cprevequipment)?;
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, Serialize, Hash, Clone, Copy)]
pub enum Item {
  Weapon(Weapon),
  Backpack,
  StrengthPotion4,
  Shield,
  BodyArmor,
  Potion2Hp,
  Potion4Hp,
  Potion5Hp,
  Potion6Hp,
  StrengthPotion,
  GenSpecial(u8),
  GenBackpack(u8),
  Meal,
  Gold,
  Laumspur,
  Helmet,
}

struct ItemVisitor;

impl std::str::FromStr for Item {
  type Err = String;
  fn from_str(s: &str) -> std::result::Result<Self, <Self as std::str::FromStr>::Err> {
    match s {
      "Backpack" => Ok(Item::Backpack),
      "StrengthPotion4" => Ok(Item::StrengthPotion4),
      "Shield" => Ok(Item::Shield),
      "BodyArmor" => Ok(Item::BodyArmor),
      "Potion2Hp" => Ok(Item::Potion2Hp),
      "Potion4Hp" => Ok(Item::Potion4Hp),
      "Potion5Hp" => Ok(Item::Potion5Hp),
      "Potion6Hp" => Ok(Item::Potion6Hp),
      "StrengthPotion" => Ok(Item::StrengthPotion),
      "Meal" => Ok(Item::Meal),
      "Gold" => Ok(Item::Gold),
      "Laumspur" => Ok(Item::Laumspur),
      "Helmet" => Ok(Item::Helmet),
      "Dagger" => Ok(Item::Weapon(Weapon::Dagger)),
      "Spear" => Ok(Item::Weapon(Weapon::Spear)),
      "Mace" => Ok(Item::Weapon(Weapon::Mace)),
      "ShortSword" => Ok(Item::Weapon(Weapon::ShortSword)),
      "Warhammer" => Ok(Item::Weapon(Weapon::Warhammer)),
      "Sword" => Ok(Item::Weapon(Weapon::Sword)),
      "Axe" => Ok(Item::Weapon(Weapon::Axe)),
      "Quarterstaff" => Ok(Item::Weapon(Weapon::Quarterstaff)),
      "BroadSword" => Ok(Item::Weapon(Weapon::BroadSword)),
      "MagicSpear" => Ok(Item::Weapon(Weapon::MagicSpear)),
      "Sommerswerd" => Ok(Item::Weapon(Weapon::Sommerswerd)),
      _ => match s.strip_prefix("BGenCounter ") {
        Some(n) => n
          .parse::<u8>()
          .map(Item::GenBackpack)
          .map_err(|rr| format!("unexpected item {}: {}", s, rr)),
        None => match s.strip_prefix("SGenCounter ") {
          Some(n) => n
            .parse::<u8>()
            .map(Item::GenSpecial)
            .map_err(|rr| format!("unexpected item {}: {}", s, rr)),
          None => Err(format!("unexpected item {}", s)),
        },
      },
    }
  }
}

impl<'de> Visitor<'de> for ItemVisitor {
  type Value = Item;

  fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    formatter.write_str("an item")
  }

  fn visit_map<A: serde::de::MapAccess<'de>>(self, omap: A) -> Result<Item, A::Error> {
    let mut mmap = omap;
    let mk: Option<String> = mmap.next_key()?;
    let key = mk.unwrap();
    if key != "Weapon" {
      return Err(serde::de::Error::invalid_value(
        serde::de::Unexpected::Str(&key),
        &self,
      ));
    }
    let v: String = mmap.next_value()?;
    match v.parse() {
      Ok(d) => Ok(d),
      _ => Err(serde::de::Error::invalid_value(
        serde::de::Unexpected::Str(&v),
        &self,
      )),
    }
  }

  fn visit_str<E>(self, s: &str) -> Result<Item, E>
  where
    E: serde::de::Error,
  {
    match s.parse() {
      Ok(x) => Ok(x),
      Err(_) => Err(serde::de::Error::invalid_value(
        serde::de::Unexpected::Str(s),
        &self,
      )),
    }
  }
}

impl<'de> Deserialize<'de> for Item {
  fn deserialize<D>(deserializer: D) -> Result<Item, D::Error>
  where
    D: Deserializer<'de>,
  {
    deserializer.deserialize_any(ItemVisitor)
  }
}

impl<'b> Decode<'b> for Item {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    use Item::*;
    let ln = d.array()?;
    let tg = d.u8()?;

    if ln != Some(1) && ln != Some(2) {
      return Err(Error::Message("invalid length for item"));
    }
    match tg {
      0 => d.decode().map(Weapon),
      1 => Ok(Backpack),
      2 => Ok(StrengthPotion4),
      3 => Ok(Shield),
      4 => Ok(BodyArmor),
      5 => Ok(Potion2Hp),
      6 => Ok(Potion4Hp),
      7 => Ok(Potion5Hp),
      8 => Ok(Potion6Hp),
      9 => Ok(StrengthPotion),
      10 => d.u8().map(GenSpecial),
      11 => d.u8().map(GenBackpack),
      12 => Ok(Meal),
      13 => Ok(Gold),
      14 => Ok(Laumspur),
      15 => Ok(Helmet),
      _ => Err(Error::Message("invalid item id")),
    }
  }
}

impl Encode for Item {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    use Item::*;
    match self {
      Weapon(w) => {
        e.array(2)?.u8(0)?.encode(w)?;
      }
      Backpack => {
        e.array(1)?.u8(1)?;
      }
      StrengthPotion4 => {
        e.array(1)?.u8(2)?;
      }
      Shield => {
        e.array(1)?.u8(3)?;
      }
      BodyArmor => {
        e.array(1)?.u8(4)?;
      }
      Potion2Hp => {
        e.array(1)?.u8(5)?;
      }
      Potion4Hp => {
        e.array(1)?.u8(6)?;
      }
      Potion5Hp => {
        e.array(1)?.u8(7)?;
      }
      Potion6Hp => {
        e.array(1)?.u8(8)?;
      }
      StrengthPotion => {
        e.array(1)?.u8(9)?;
      }
      GenSpecial(n) => {
        e.array(2)?.u8(10)?.u8(*n)?;
      }
      GenBackpack(n) => {
        e.array(2)?.u8(11)?.u8(*n)?;
      }
      Meal => {
        e.array(1)?.u8(12)?;
      }
      Gold => {
        e.array(1)?.u8(13)?;
      }
      Laumspur => {
        e.array(1)?.u8(14)?;
      }
      Helmet => {
        e.array(1)?.u8(15)?;
      }
    }
    Ok(())
  }
}

impl Item {
  pub const VALUES: [Item; 48] = [
    Item::Backpack,
    Item::StrengthPotion4,
    Item::Shield,
    Item::BodyArmor,
    Item::Potion2Hp,
    Item::Potion4Hp,
    Item::Potion5Hp,
    Item::Potion6Hp,
    Item::StrengthPotion,
    Item::Meal,
    Item::Gold,
    Item::Laumspur,
    Item::Helmet,
    Item::Weapon(Weapon::Dagger),
    Item::Weapon(Weapon::Spear),
    Item::Weapon(Weapon::Mace),
    Item::Weapon(Weapon::ShortSword),
    Item::Weapon(Weapon::Warhammer),
    Item::Weapon(Weapon::Sword),
    Item::Weapon(Weapon::Axe),
    Item::Weapon(Weapon::Quarterstaff),
    Item::Weapon(Weapon::BroadSword),
    Item::Weapon(Weapon::MagicSpear),
    Item::Weapon(Weapon::Sommerswerd),
    Item::GenSpecial(0),
    Item::GenSpecial(1),
    Item::GenSpecial(2),
    Item::GenSpecial(3),
    Item::GenSpecial(4),
    Item::GenSpecial(5),
    Item::GenSpecial(6),
    Item::GenSpecial(7),
    Item::GenSpecial(8),
    Item::GenSpecial(9),
    Item::GenSpecial(10),
    Item::GenSpecial(11),
    Item::GenBackpack(0),
    Item::GenBackpack(1),
    Item::GenBackpack(2),
    Item::GenBackpack(3),
    Item::GenBackpack(4),
    Item::GenBackpack(5),
    Item::GenBackpack(6),
    Item::GenBackpack(7),
    Item::GenBackpack(8),
    Item::GenBackpack(9),
    Item::GenBackpack(10),
    Item::GenBackpack(11),
  ];

  pub const fn get_idx(&self) -> usize {
    match self {
      Item::Backpack => 0,
      Item::StrengthPotion4 => 1,
      Item::Shield => 2,
      Item::BodyArmor => 3,
      Item::Potion2Hp => 4,
      Item::Potion4Hp => 5,
      Item::Potion5Hp => 6,
      Item::Potion6Hp => 7,
      Item::StrengthPotion => 8,
      Item::Weapon(w) => 9 + *w as usize,
      Item::GenSpecial(n) => 20 + *n as usize,
      Item::GenBackpack(n) => 32 + *n as usize,
      Item::Helmet => 44,
      Item::Laumspur => 45,
      Item::Gold => 46,
      Item::Meal => 47,
    }
  }

  pub const fn slot(&self) -> Slot {
    match self {
      Item::Weapon(_) => Slot::Weapon,
      Item::Gold => Slot::Pouch,
      Item::Meal => Slot::Backpack,
      Item::Laumspur => Slot::Backpack,
      Item::StrengthPotion => Slot::Backpack,
      Item::GenBackpack(_) => Slot::Backpack,
      Item::Potion2Hp => Slot::Backpack,
      Item::Potion4Hp => Slot::Backpack,
      Item::Potion5Hp => Slot::Backpack,
      Item::Potion6Hp => Slot::Backpack,
      _ => Slot::Special,
    }
  }
}

#[derive(Debug, PartialEq, Eq, Serialize, Hash, Clone, Copy)]
#[repr(u8)]
pub enum Discipline {
  Camouflage,
  Hunting,
  SixthSense,
  Tracking,
  Healing,
  WeaponSkill(Weapon),
  MindShield,
  MindBlast,
  AnimalKinship,
  MindOverMatter,
}

struct DisciplineVisitor;

impl std::str::FromStr for Discipline {
  type Err = String;
  fn from_str(s: &str) -> std::result::Result<Self, <Self as std::str::FromStr>::Err> {
    use Discipline::*;
    match s {
      "Dagger" => Ok(WeaponSkill(Weapon::Dagger)),
      "Spear" => Ok(WeaponSkill(Weapon::Spear)),
      "Mace" => Ok(WeaponSkill(Weapon::Mace)),
      "ShortSword" => Ok(WeaponSkill(Weapon::ShortSword)),
      "Warhammer" => Ok(WeaponSkill(Weapon::Warhammer)),
      "Sword" => Ok(WeaponSkill(Weapon::Sword)),
      "Axe" => Ok(WeaponSkill(Weapon::Axe)),
      "Quarterstaff" => Ok(WeaponSkill(Weapon::Quarterstaff)),
      "BroadSword" => Ok(WeaponSkill(Weapon::BroadSword)),
      "MagicSpear" => Ok(WeaponSkill(Weapon::MagicSpear)),
      "Sommerswerd" => Ok(WeaponSkill(Weapon::Sommerswerd)),
      "Camouflage" => Ok(Camouflage),
      "Hunting" => Ok(Hunting),
      "SixthSense" => Ok(SixthSense),
      "Tracking" => Ok(Tracking),
      "Healing" => Ok(Healing),
      "MindShield" => Ok(MindShield),
      "MindBlast" => Ok(MindBlast),
      "AnimalKinship" => Ok(AnimalKinship),
      "MindOverMatter" => Ok(MindOverMatter),
      "SS" => Ok(WeaponSkill(Weapon::ShortSword)),
      "SW" => Ok(WeaponSkill(Weapon::Sword)),
      "CA" => Ok(Camouflage),
      "HU" => Ok(Hunting),
      "6S" => Ok(SixthSense),
      "TR" => Ok(Tracking),
      "HL" => Ok(Healing),
      "MS" => Ok(MindShield),
      "MB" => Ok(MindBlast),
      "AK" => Ok(AnimalKinship),
      "MO" => Ok(MindOverMatter),
      _ => Err(format!("Unrecognized discipline {}", s)),
    }
  }
}

impl<'de> Visitor<'de> for DisciplineVisitor {
  type Value = Discipline;

  fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    formatter.write_str("a discipline")
  }

  fn visit_str<E>(self, s: &str) -> Result<Discipline, E>
  where
    E: serde::de::Error,
  {
    match s.parse() {
      Ok(d) => Ok(d),
      _ => Err(serde::de::Error::invalid_value(
        serde::de::Unexpected::Str(s),
        &self,
      )),
    }
  }

  fn visit_map<A: serde::de::MapAccess<'de>>(self, omap: A) -> Result<Discipline, A::Error> {
    let mut mmap = omap;
    let mk: Option<String> = mmap.next_key()?;
    let key = mk.unwrap();
    if key != "WeaponSkill" {
      return Err(serde::de::Error::invalid_value(
        serde::de::Unexpected::Str(&key),
        &self,
      ));
    }
    let v: String = mmap.next_value()?;
    match v.parse() {
      Ok(d) => Ok(d),
      _ => Err(serde::de::Error::invalid_value(
        serde::de::Unexpected::Str(&v),
        &self,
      )),
    }
  }
}

impl<'de> Deserialize<'de> for Discipline {
  fn deserialize<D>(deserializer: D) -> Result<Discipline, D::Error>
  where
    D: Deserializer<'de>,
  {
    deserializer.deserialize_any(DisciplineVisitor)
  }
}

impl<'b> Decode<'b> for Discipline {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    use Discipline::*;
    let ln = d.array()?;
    let tg = d.u8()?;

    if ln != Some(1) && (ln != Some(2) && tg != 5) {
      return Err(Error::Message("invalid length for discipline"));
    }
    match tg {
      0 => Ok(Camouflage),
      1 => Ok(Hunting),
      2 => Ok(SixthSense),
      3 => Ok(Tracking),
      4 => Ok(Healing),
      5 => d.decode().map(WeaponSkill),
      6 => Ok(MindShield),
      7 => Ok(MindBlast),
      8 => Ok(AnimalKinship),
      9 => Ok(MindOverMatter),
      _ => Err(Error::Message("invalid discipline id")),
    }
  }
}

impl Encode for Discipline {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    use Discipline::*;
    if let WeaponSkill(w) = self {
      e.array(2)?;
      e.u8(5)?;
      e.encode(w)?;
    } else {
      e.array(1)?;
      e.u8(match self {
        Camouflage => 0,
        Hunting => 1,
        SixthSense => 2,
        Tracking => 3,
        Healing => 4,
        MindShield => 6,
        MindBlast => 7,
        AnimalKinship => 8,
        MindOverMatter => 9,
        WeaponSkill(_) => panic!("can't happen"),
      })?;
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash)]
pub enum Weapon {
  Dagger = 0,
  Spear = 1,
  Mace = 2,
  ShortSword = 3,
  Warhammer = 4,
  Sword = 5,
  Axe = 6,
  Quarterstaff = 7,
  BroadSword = 8,
  MagicSpear = 9,
  Sommerswerd = 10,
}

impl Weapon {
  const VALUES: [Self; 11] = [
    Self::Dagger,
    Self::Spear,
    Self::Mace,
    Self::ShortSword,
    Self::Warhammer,
    Self::Sword,
    Self::Axe,
    Self::Quarterstaff,
    Self::BroadSword,
    Self::MagicSpear,
    Self::Sommerswerd,
  ];
}

impl<'b> Decode<'b> for Weapon {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    use Weapon::*;
    let ln = d.array()?;
    let tg = d.u8()?;

    if ln != Some(1) {
      return Err(Error::Message("invalid length for weapon"));
    }

    match tg {
      0 => Ok(Dagger),
      1 => Ok(Spear),
      2 => Ok(Mace),
      3 => Ok(ShortSword),
      4 => Ok(Warhammer),
      5 => Ok(Sword),
      6 => Ok(Axe),
      7 => Ok(Quarterstaff),
      8 => Ok(BroadSword),
      9 => Ok(MagicSpear),
      10 => Ok(Sommerswerd),
      _ => Err(Error::Message("invalid weapon id")),
    }
  }
}

impl Encode for Weapon {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.array(1)?.u8(*self as u8)?;
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum Book {
  Book01,
  Book02,
  Book03,
  Book04,
  Book05,
}

impl std::str::FromStr for Book {
  type Err = String;
  fn from_str(s: &str) -> std::result::Result<Self, <Self as std::str::FromStr>::Err> {
    match s {
      "01" => Ok(Book::Book01),
      "02" => Ok(Book::Book02),
      "03" => Ok(Book::Book03),
      "04" => Ok(Book::Book04),
      "05" => Ok(Book::Book05),
      "book01" => Ok(Book::Book01),
      "book02" => Ok(Book::Book02),
      "book03" => Ok(Book::Book03),
      "book04" => Ok(Book::Book04),
      "book05" => Ok(Book::Book05),
      _ => Err(format!("Invalid book {}", s)),
    }
  }
}

impl<'b> Decode<'b> for Book {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    use Book::*;
    let ln = d.array()?;
    let tg = d.u8()?;

    if ln != Some(1) {
      return Err(Error::Message("invalid length for book"));
    }
    match tg {
      0 => Ok(Book01),
      1 => Ok(Book02),
      2 => Ok(Book03),
      3 => Ok(Book04),
      4 => Ok(Book05),
      _ => Err(Error::Message("Invalid book id")),
    }
  }
}

impl Encode for Book {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.array(1)?.u8(*self as u8)?;
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash)]
pub enum Flag {
  PermanentSkillReduction = 0,
  StrengthPotionActive = 1,
  FoughtElix = 2,
  LimbDeath = 3,
  ReceivedCrystalStarPendant = 4,
  Knowledge01 = 5,
  Knowledge02 = 6,
  Knowledge03 = 7,
  Knowledge04 = 8,
  Special01 = 9,
  Special02 = 10,
  Special03 = 11,
  Special04 = 12,
  Poisonned2 = 13,
  HadCombat = 14,
  PermanentSkillReduction2 = 15,
  HelmetIsSilver = 16,
  PotentStrengthPotionActive = 17,
}

impl Flag {
  pub const VALUES: [Flag; 18] = [
    Flag::PermanentSkillReduction,
    Flag::StrengthPotionActive,
    Flag::FoughtElix,
    Flag::LimbDeath,
    Flag::ReceivedCrystalStarPendant,
    Flag::Knowledge01,
    Flag::Knowledge02,
    Flag::Knowledge03,
    Flag::Knowledge04,
    Flag::Special01,
    Flag::Special02,
    Flag::Special03,
    Flag::Special04,
    Flag::Poisonned2,
    Flag::HadCombat,
    Flag::PermanentSkillReduction2,
    Flag::HelmetIsSilver,
    Flag::PotentStrengthPotionActive,
  ];
}

impl std::str::FromStr for Flag {
  type Err = String;
  fn from_str(s: &str) -> std::result::Result<Self, <Self as std::str::FromStr>::Err> {
    match s {
      "PermanentSkillReduction" => Ok(Flag::PermanentSkillReduction),
      "StrengthPotionActive" => Ok(Flag::StrengthPotionActive),
      "FoughtElix" => Ok(Flag::FoughtElix),
      "LimbDeath" => Ok(Flag::LimbDeath),
      "ReceivedCrystalStarPendant" => Ok(Flag::ReceivedCrystalStarPendant),
      "Knowledge01" => Ok(Flag::Knowledge01),
      "Knowledge02" => Ok(Flag::Knowledge02),
      "Knowledge03" => Ok(Flag::Knowledge03),
      "Knowledge04" => Ok(Flag::Knowledge04),
      "Special01" => Ok(Flag::Special01),
      "Special02" => Ok(Flag::Special02),
      "Special03" => Ok(Flag::Special03),
      "Special04" => Ok(Flag::Special04),
      "Poisonned2" => Ok(Flag::Poisonned2),
      "HadCombat" => Ok(Flag::HadCombat),
      "PermanentSkillReduction2" => Ok(Flag::PermanentSkillReduction2),
      "HelmetIsSilver" => Ok(Flag::HelmetIsSilver),
      "PotentStrengthPotionActive" => Ok(Flag::PotentStrengthPotionActive),
      _ => Err(format!("Invalid flag: {}", s)),
    }
  }
}

impl<'b> Decode<'b> for Flag {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    use Flag::*;
    let ln = d.array()?;
    let tg = d.u8()?;

    if ln != Some(1) {
      return Err(Error::Message("invalid length for flag"));
    }

    match tg {
      0 => Ok(PermanentSkillReduction),
      1 => Ok(StrengthPotionActive),
      2 => Ok(FoughtElix),
      3 => Ok(LimbDeath),
      4 => Ok(ReceivedCrystalStarPendant),
      5 => Ok(Knowledge01),
      6 => Ok(Knowledge02),
      7 => Ok(Knowledge03),
      8 => Ok(Knowledge04),
      9 => Ok(Special01),
      10 => Ok(Special02),
      11 => Ok(Special03),
      12 => Ok(Special04),
      13 => Ok(Poisonned2),
      14 => Ok(HadCombat),
      15 => Ok(PermanentSkillReduction2),
      16 => Ok(HelmetIsSilver),
      17 => Ok(PotentStrengthPotionActive),
      _ => Err(Error::Message("invalid flag id")),
    }
  }
}

impl Encode for Flag {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.array(1)?.u8(*self as u8)?;
    Ok(())
  }
}

pub fn max_hp(ccst: &CharacterConstant, cvar: &CharacterVariable) -> i8 {
  ccst.maxendurance
    + (if cvar.cequipment.has_itemb(&Item::BodyArmor) {
      4
    } else {
      0
    })
    + (if cvar.cequipment.has_itemb(&Item::Helmet) && !cvar.flags.has(Flag::HelmetIsSilver) {
      2
    } else {
      0
    })
}

pub fn mkchar(ccst: &CharacterConstant, cvar: &CVarState) -> CharacterVariable {
  let mut cv = CharacterVariable::new(ccst.maxendurance);
  for (itm, qty) in cvar.items.as_ref().unwrap().iter() {
    cv.add_item(itm, *qty);
  }
  cv.add_item(&Item::Backpack, 1);
  cv.add_item(&Item::Gold, cvar.gold as i64);
  for f in &cvar.flags {
    cv.set_flag(*f);
  }
  cv.curendurance = max_hp(ccst, &cv);
  cv
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct CompactState {
  pub chapter: u16,
  pub character: CharacterVariable,
  pub score: f32,
}

impl CompactState {
  pub fn from_choppedsolution(cur: NextStep, cs: ChoppedSolution<NextStep>) -> Option<Self> {
    let score = cs.score().to_f32();
    match cur {
      NextStep::NewChapter(cid, cv) => Some(CompactState {
        chapter: cid,
        character: cv,
        score,
      }),
      _ => None,
    }
  }
}

impl Encode for CompactState {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.u16(self.chapter)?
      .encode(&self.character)?
      .f32(self.score)?;
    Ok(())
  }
}
impl<'b> Decode<'b> for CompactState {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    let chapter = d.u16()?;
    let character = d.decode()?;
    let score = d.f32()?;
    Ok(CompactState {
      chapter,
      character,
      score,
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct CompactSolution {
  pub soldesc: SolDesc,
  pub content: Vec<CompactState>,
}

impl<'b> Decode<'b> for CompactSolution {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    let ln = d.array()?;
    if ln != Some(3) {
      return Err(Error::Message("invalid length for SolutionDump"));
    }
    d.u8()?;
    let soldesc = d.decode()?;
    let rcontent: Result<Vec<_>, Error> = d.array_iter()?.collect();
    Ok(CompactSolution {
      soldesc,
      content: rcontent?,
    })
  }
}

impl Encode for CompactSolution {
  fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    e.array(3)?.u8(0)?.encode(&self.soldesc)?;
    e.array(self.content.len() as u64)?;
    for c in &self.content {
      e.encode(c)?;
    }
    Ok(())
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn t_flag() {
    use Flag::*;
    for f in &[
      PermanentSkillReduction,
      StrengthPotionActive,
      FoughtElix,
      LimbDeath,
      ReceivedCrystalStarPendant,
      Knowledge01,
      Knowledge02,
      Knowledge03,
      Knowledge04,
      Special01,
      Special02,
      Special03,
      Special04,
      Poisonned2,
      HadCombat,
      PermanentSkillReduction2,
      HelmetIsSilver,
      PotentStrengthPotionActive,
    ] {
      let mut buffer = [0u8; 128];
      minicbor::encode(f, buffer.as_mut()).unwrap();
      let output: Flag = minicbor::decode(buffer.as_ref()).unwrap();
      assert_eq!(f, &output);
    }
  }

  #[test]
  fn t_weapon() {
    for f in Weapon::VALUES {
      let mut buffer = [0u8; 128];
      minicbor::encode(f, buffer.as_mut()).unwrap();
      let output: Weapon = minicbor::decode(buffer.as_ref()).unwrap();
      assert_eq!(f, output);
    }
  }

  #[test]
  fn t_discipline() {
    use Discipline::*;
    for d in &[
      WeaponSkill(Weapon::Dagger),
      WeaponSkill(Weapon::Spear),
      WeaponSkill(Weapon::Mace),
      WeaponSkill(Weapon::ShortSword),
      WeaponSkill(Weapon::Warhammer),
      WeaponSkill(Weapon::Sword),
      WeaponSkill(Weapon::Axe),
      WeaponSkill(Weapon::Quarterstaff),
      WeaponSkill(Weapon::BroadSword),
      WeaponSkill(Weapon::MagicSpear),
      WeaponSkill(Weapon::Sommerswerd),
      Camouflage,
      Hunting,
      SixthSense,
      Tracking,
      Healing,
      MindShield,
      MindBlast,
      AnimalKinship,
      MindOverMatter,
      WeaponSkill(Weapon::ShortSword),
      WeaponSkill(Weapon::Sword),
      Camouflage,
      Hunting,
      SixthSense,
      Tracking,
      Healing,
      MindShield,
      MindBlast,
      AnimalKinship,
      MindOverMatter,
    ] {
      let mut buffer = [0u8; 128];
      minicbor::encode(d, buffer.as_mut()).unwrap();
      let output: Discipline = minicbor::decode(buffer.as_ref()).unwrap();
      assert_eq!(d, &output);
    }
  }

  #[test]
  fn t_item() {
    for d in &[
      Item::Weapon(Weapon::Dagger),
      Item::Shield,
      Item::GenBackpack(4),
    ] {
      let mut buffer = [0u8; 128];
      minicbor::encode(d, buffer.as_mut()).unwrap();
      let output: Item = minicbor::decode(buffer.as_ref()).unwrap();
      assert_eq!(d, &output);
    }
  }

  #[test]
  fn t_cvariable() {
    fn check(c: &CharacterVariable) {
      let mut buffer = [0u8; 128];
      minicbor::encode(c, buffer.as_mut()).unwrap();
      let output: CharacterVariable = minicbor::decode(buffer.as_ref()).unwrap();
      assert_eq!(c, &output);
    }
    let mut cv = CharacterVariable::new(12);
    check(&cv);
    cv.cequipment = Equipment(1561486);
    cv.flags = Flags(1561568);
    check(&cv);
    cv.cprevequipment = Equipment(541866158);
    check(&cv);
  }

  #[test]
  fn t_nextstep() {
    for d in &[
      NextStep::HasLost(12),
      NextStep::HasWon(CharacterVariable::new(12)),
    ] {
      let mut buffer = [0u8; 128];
      minicbor::encode(d, buffer.as_mut()).unwrap();
      let output: NextStep = minicbor::decode(buffer.as_ref()).unwrap();
      assert_eq!(d, &output);
    }
  }

  #[test]
  fn t_cvarstate() {
    let c = CVarState {
      flags: vec![
        Flag::FoughtElix,
        Flag::Knowledge02,
        Flag::PermanentSkillReduction,
      ],
      gold: 12,
      items: Some(vec![(Item::Meal, 3), (Item::Weapon(Weapon::BroadSword), 1)]),
    };
    let mut buffer = [0u8; 128];
    minicbor::encode(&c, buffer.as_mut()).unwrap();
    let output: CVarState = minicbor::decode(buffer.as_ref()).unwrap();
    assert_eq!(&c, &output);
  }

  #[test]
  fn t_soldesc() {
    let cvar = CVarState {
      flags: vec![
        Flag::FoughtElix,
        Flag::Knowledge02,
        Flag::PermanentSkillReduction,
      ],
      gold: 12,
      items: Some(vec![(Item::Meal, 3), (Item::Weapon(Weapon::BroadSword), 1)]),
    };
    let soldesc = SolDesc {
      ccst: CharacterConstant {
        combat_skill: 12,
        maxendurance: 30,
        discipline: vec![
          Discipline::WeaponSkill(Weapon::Sword),
          Discipline::SixthSense,
        ],
        bookid: Book::Book03,
      },
      finalchapters: vec![5, 8],
      cvar,
    };
    let mut buffer = [0u8; 128];
    minicbor::encode(&soldesc, buffer.as_mut()).unwrap();
    let output: SolDesc = minicbor::decode(buffer.as_ref()).unwrap();
    assert_eq!(&soldesc, &output);
  }

  #[test]
  fn t_soldump() {
    let cvar = CVarState {
      flags: vec![
        Flag::FoughtElix,
        Flag::Knowledge02,
        Flag::PermanentSkillReduction,
      ],
      gold: 12,
      items: Some(vec![(Item::Meal, 3), (Item::Weapon(Weapon::BroadSword), 1)]),
    };
    let sd = SolutionDump {
      soldesc: SolDesc {
        ccst: CharacterConstant {
          combat_skill: 12,
          maxendurance: 30,
          discipline: vec![
            Discipline::WeaponSkill(Weapon::Sword),
            Discipline::SixthSense,
          ],
          bookid: Book::Book03,
        },
        finalchapters: vec![5, 8],
        cvar,
      },
      content: vec![(NextStep::HasLost(5), ChoppedSolution::CLeafLost)],
    };
    let mut buffer = [0u8; 128];
    minicbor::encode(&sd, buffer.as_mut()).unwrap();
    let output: SolutionDump = minicbor::decode(buffer.as_ref()).unwrap();
    assert_eq!(&sd, &output);
  }
}
