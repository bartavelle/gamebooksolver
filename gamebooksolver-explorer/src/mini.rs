use minicbor::decode::{Decode, Decoder, Error};
use rug::Integer;
use rug::Rational;
use serde::{Deserialize, Serialize};

pub type Probably<A> = Vec<(A, Rational)>;

#[derive(Debug, PartialEq, Eq)]
pub struct SolutionDump {
  pub soldesc: SolDesc,
  pub content: Vec<(NextStep, ChoppedSolution<NextStep>)>,
}

fn cbor_pair<'a, F1, F2, V1, V2>(d: &mut Decoder<'a>, p1: F1, p2: F2) -> Result<(V1, V2), Error>
where
  F1: Fn(&mut Decoder<'a>) -> Result<V1, Error>,
  F2: Fn(&mut Decoder<'a>) -> Result<V2, Error>,
{
  let ln = d.array()?;
  if ln != Some(2) {
    return Err(Error::Message("Invalid length for pair"));
  }
  let a = p1(d)?;
  let b = p2(d)?;
  Ok((a, b))
}

fn cbor_option<'a, F, V>(d: &mut Decoder<'a>, p: F) -> Result<Option<V>, Error>
where
  F: Fn(&mut Decoder<'a>) -> Result<V, Error>,
{
  let ln = d.array()?;
  match ln {
    Some(0) => Ok(None),
    Some(1) => p(d).map(Some),
    _ => Err(Error::Message("Invalid optional size")),
  }
}

fn rational(d: &mut Decoder) -> Result<Rational, Error> {
  fn read_bigint(d: &mut Decoder) -> Result<Integer, Error> {
    let tp = d.datatype()?;
    match tp {
      minicbor::data::Type::Tag => {
        let tg = d.tag()?;
        match tg {
          minicbor::data::Tag::PosBignum => {
            Ok(Integer::from_digits(d.bytes()?, rug::integer::Order::Msf))
          }
          _ => Err(Error::Message("bad tag for bignum")),
        }
      }
      _ => d.u64().map(Integer::from),
    }
  }
  let (n, d) = cbor_pair(d, read_bigint, read_bigint)?;
  Ok(Rational::from((n, d)))
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

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum ChoppedSolution<State> {
  CNode(Rational, Probably<Option<State>>),
  CJump(Rational, State),
  CLeafLost,
  CLeaf(Rational),
}

struct PPart<A>((Option<A>, Rational));
impl<'b, A: Decode<'b>> Decode<'b> for PPart<A> {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    cbor_pair(d, |d| cbor_option(d, |d| d.decode()), rational).map(PPart)
  }
}

impl<'b, A: Decode<'b>> Decode<'b> for ChoppedSolution<A> {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    let ln = d.array()?;
    let tg = d.u8()?;
    match (tg, ln) {
      (0, Some(3)) => {
        let sc = rational(d)?;
        let content: Result<Probably<Option<A>>, _> = d
          .array_iter()?
          .map(|x: Result<PPart<A>, _>| x.map(|e| e.0))
          .collect();
        Ok(ChoppedSolution::CNode(sc, content?))
      }
      (1, Some(3)) => {
        let sc = rational(d)?;
        let stt = d.decode()?;
        Ok(ChoppedSolution::CJump(sc, stt))
      }
      (2, Some(1)) => Ok(ChoppedSolution::CLeafLost),
      (3, Some(2)) => rational(d).map(ChoppedSolution::CLeaf),
      _ => Err(Error::Message("Invalid variant for ChoppedSolution")),
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum NextStep {
  HasLost(u16),
  HasWon(CharacterVariable),
  NewChapter(u16, CharacterVariable),
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize)]
pub struct Equipment(pub u64);

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
  pub fn add_item(&mut self, i: Item, q: i64) {
    if q <= 0 {
      return;
    }
    match i {
      Item::Gold => {
        self.set_gold(q as u8 + self.get_gold());
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
}

impl<'b> Decode<'b> for Equipment {
  fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
    d.u64().map(Equipment)
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize)]
pub struct CharacterVariable {
  pub curendurance: i8,
  pub flags: u32,
  pub cequipment: Equipment,
  pub cprevequipment: Equipment,
}

impl CharacterVariable {
  pub fn new(endurance: i8) -> Self {
    CharacterVariable {
      curendurance: endurance,
      flags: 0,
      cequipment: Equipment(0),
      cprevequipment: Equipment(0),
    }
  }

  pub fn add_item(&mut self, i: Item, q: i64) {
    self.cequipment.add_item(i, q);
  }

  pub fn set_flag(&mut self, f: Flag) {
    self.flags |= 1 << (f as usize);
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
    let flags = d.u32()?;
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

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Item {
  Weapon(Weapon),
  Backpack,
  StrengthPotion4,
  Shield,
  ChainMail,
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
      4 => Ok(ChainMail),
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

impl Item {
  fn get_idx(&self) -> usize {
    match self {
      Item::Backpack => 0,
      Item::StrengthPotion4 => 1,
      Item::Shield => 2,
      Item::ChainMail => 3,
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
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Book {
  Book01,
  Book02,
  Book03,
  Book04,
  Book05,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
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
pub fn parse_soldump(input: &[u8]) -> Result<SolutionDump, Error> {
  Decoder::new(input).decode()
}
