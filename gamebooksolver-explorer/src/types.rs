use anyhow::{anyhow, Context, Result};
use cbor::Cbor;
use rug::Integer;
use rug::Rational;
use rustc_serialize::Decodable;

pub type Probably<A> = Vec<(A, Rational)>;

#[derive(Debug, PartialEq, Eq)]
pub struct SolDesc {
  pub finalchapters: Vec<u16>,
  pub ccst: CharacterConstant,
  pub cvar: CVarState,
}

fn cbor_array(c: Cbor) -> Result<Vec<Cbor>> {
  match c {
    Cbor::Array(x) => Ok(x),
    _ => Err(anyhow!("expected an array, got {:?}", c)),
  }
}

fn cbor_enum<A, F>(tp: &str, c: Cbor, f: F) -> Result<A>
where
  F: Fn(u8, &mut std::vec::IntoIter<Cbor>) -> Result<A>,
{
  let mut r = cbor_array(c)?.into_iter();
  let tg = r
    .next()
    .ok_or_else(|| anyhow!("Could not pop enum id for {}", tp))?
    .decode()
    .with_context(|| anyhow!("when decoding enum id for {}", tp))?;
  f(tg, &mut r).with_context(|| anyhow!("can't decode enum {}", tp))
}

fn cbor_vector<F, A>(c: Cbor, f: F) -> Result<Vec<A>>
where
  F: Fn(Cbor) -> Result<A>,
{
  let r = cbor_array(c)?;
  r.into_iter().map(f).collect()
}

fn cbor_pair<F1, A, F2, B>(c: Cbor, f1: F1, f2: F2) -> Result<(A, B)>
where
  F1: Fn(Cbor) -> Result<A>,
  F2: Fn(Cbor) -> Result<B>,
{
  let mut r = cbor_array(c)?;
  if r.len() != 2 {
    return Err(anyhow!("pair does have {} elements :(", r.len()));
  }
  let a2 = f2(pop_cbor(&mut r)?).with_context(|| "when decoding snd")?;
  let a1 = f1(pop_cbor(&mut r)?).with_context(|| "when decoding fst")?;
  Ok((a1, a2))
}

fn cbor_optional<F, A>(c: Cbor, f: F) -> Result<Option<A>>
where
  F: Fn(Cbor) -> Result<A>,
{
  let r = cbor_array(c)?;
  if r.len() > 1 {
    return Err(anyhow!("array to large for option"));
  }
  match r.into_iter().next() {
    None => Ok(None),
    Some(x) => f(x).map(Some),
  }
}

fn cbor_next(r: &mut std::vec::IntoIter<Cbor>) -> Result<Cbor> {
  r.next().ok_or_else(|| anyhow!("could not pop"))
}

fn cbor_next_decode<A: Decodable>(tp: &str, r: &mut std::vec::IntoIter<Cbor>) -> Result<A> {
  let c = cbor_next(r)?;
  c.decode().map_err(|r| anyhow!("can't pop {}: {}", tp, r))
}

fn pop_cbor(v: &mut Vec<Cbor>) -> Result<Cbor> {
  match v.pop() {
    Some(x) => Ok(x),
    None => Err(anyhow!("pop_cbor")),
  }
}

impl Decodable for SolDesc {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    d.read_u8()?;
    d.read_seq(|d, sz| {
      if sz != 4 {
        return Err(d.error("invalid length for SolDesc"));
      }
      d.read_u8()?;
      let finalchapters = Decodable::decode(d)?;
      let ccst = Decodable::decode(d)?;
      let cvar = Decodable::decode(d)?;

      Ok(SolDesc {
        finalchapters,
        ccst,
        cvar,
      })
    })
  }
}

fn soldesc_from_cbor(c: Cbor) -> Result<SolDesc> {
  let mut r = cbor_array(c)?;
  if r.len() != 4 {
    return Err(anyhow!("Soldump: invalid length {}", r.len()));
  }
  let cvar = cvarstate_from_cbor(pop_cbor(&mut r).with_context(|| "cvar")?)?;
  let ccst = pop_cbor(&mut r)?
    .decode()
    .with_context(|| "when decoding character constant")?;
  let finalchapters = pop_cbor(&mut r)?
    .decode()
    .with_context(|| "when decoding final chapters")?;

  Ok(SolDesc {
    finalchapters,
    ccst,
    cvar,
  })
}
#[derive(Debug, PartialEq, Eq)]
pub struct SolutionDump {
  pub soldesc: SolDesc,
  pub content: Vec<(NextStep, ChoppedSolution<NextStep>)>,
}

pub fn soldump_from_cbor(c: Cbor) -> Result<SolutionDump> {
  let mut r = cbor_array(c)?;
  if r.len() != 3 {
    return Err(anyhow!("Soldump: invalid length {}", r.len()));
  }
  let ccbor = pop_cbor(&mut r)?;
  let scbor = pop_cbor(&mut r)?;

  let soldesc = soldesc_from_cbor(scbor).with_context(|| "when decoding soldesc")?;
  let content = cbor_vector(ccbor, |c| {
    cbor_pair(c, nextstep_from_cbor, chopped_solution_from_cbor)
  })
  .with_context(|| "when decoding content")?;
  Ok(SolutionDump { soldesc, content })
}

fn ratio_from_cbor(c: Cbor) -> Result<Rational> {
  fn read_bigint(c: Cbor) -> Result<Integer> {
    match c {
      Cbor::Tag(tg) => match tg.tag {
        2 => match *tg.data {
          Cbor::Bytes(bts) => Ok(Integer::from_digits(&bts, rug::integer::Order::Msf)),
          cnt => Err(anyhow!("Invalid bigint content {:?}", cnt)),
        },
        t => Err(anyhow!("invalid tag for bigint {}", t)),
      },
      _ => c
        .decode::<i64>()
        .map_err(|r| anyhow!("could not decode bigint A: {}", r))
        .map(Integer::from),
    }
  }
  let (n, d) = cbor_pair(c, read_bigint, read_bigint)?;
  Ok(Rational::from((n, d)))
}

#[derive(Debug, PartialEq, Eq)]
enum Optiond<X> {
  None,
  Some(X),
}

impl<X: Decodable> Decodable for Optiond<X> {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    d.read_seq(|d, sz| match sz {
      0 => Ok(Optiond::None),
      1 => Decodable::decode(d).map(Optiond::Some),
      _ => Err(d.error("invalid length for Optiond")),
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

fn chopped_solution_from_cbor(c: Cbor) -> Result<ChoppedSolution<NextStep>> {
  cbor_enum("ChoppedSolution", c, |tg, r| match tg {
    0 => {
      let score = cbor_next(r).and_then(ratio_from_cbor)?;
      let nxt = cbor_next(r).and_then(|r| {
        cbor_vector(r, |r| {
          cbor_pair(r, |r| cbor_optional(r, nextstep_from_cbor), ratio_from_cbor)
        })
      })?;
      Ok(ChoppedSolution::CNode(score, nxt))
    }
    1 => {
      let score = cbor_next(r).and_then(ratio_from_cbor)?;
      let ns = cbor_next(r).and_then(nextstep_from_cbor)?;
      Ok(ChoppedSolution::CJump(score, ns))
    }
    2 => Ok(ChoppedSolution::CLeafLost),
    3 => r
      .next()
      .ok_or(anyhow!("Could not pop score"))
      .and_then(ratio_from_cbor)
      .map(ChoppedSolution::CLeaf),
    _ => Err(anyhow!("unsupported ChoppedSolution tag {}", tg)),
  })
}

#[derive(Debug, PartialEq, Eq)]
pub struct CVarState {
  pub items: Option<Vec<(Item, i64)>>,
  pub gold: u8,
  pub flags: Vec<Flag>,
}

impl Decodable for CVarState {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    d.read_seq(|d, sz| {
      if sz != 4 {
        panic!("invalid sz for CVarState");
      }
      d.read_u8()?;
      let items = d.read_seq(|d, sz| match sz {
        0 => Ok(None),
        1 => d.read_seq_elt(0, |d| Decodable::decode(d)).map(Some),
        _ => panic!("invalid sz for optional items {}", sz),
      })?;
      let gold = d.read_u8()?;
      let flags = Decodable::decode(d)?;
      Ok(CVarState { items, gold, flags })
    })
  }
}

fn cvarstate_from_cbor(c: Cbor) -> Result<CVarState> {
  let mut r = cbor_array(c)?;
  if r.len() != 4 {
    return Err(anyhow!("Soldump: invalid length {}", r.len()));
  }
  let flags = cbor_vector(pop_cbor(&mut r)?, |x| {
    x.decode().map_err(|rr| anyhow!("flags {}", rr))
  })?;
  let gold = pop_cbor(&mut r)?
    .decode()
    .map_err(|r| anyhow!("gold: {}", r))?;
  let items = cbor_optional(pop_cbor(&mut r)?, |c| {
    cbor_vector(c, |x| {
      cbor_pair(
        x,
        |d| d.decode::<Item>().map_err(|r| anyhow!("{}", r)),
        |d| d.decode::<i64>().map_err(|r| anyhow!("{}", r)),
      )
    })
  })?;

  Ok(CVarState { items, gold, flags })
}

#[derive(Debug, PartialEq, Eq)]
pub enum Item {
  Weapon(Weapon),
  Backpack,
  Helmet,
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
  SilverHelm,
}

impl Item {
  fn get_idx(&self) -> usize {
    match self {
      Item::Backpack => 0,
      Item::Helmet => 1,
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
      Item::SilverHelm => 44,
      Item::Laumspur => 45,
      Item::Gold => 46,
      Item::Meal => 47,
    }
  }
}

impl Decodable for Item {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    use Item::*;
    d.read_seq(|d, sz| {
      let tg = d.read_u8()?;
      if (sz != 1) && (tg != 0 && sz != 2) {
        panic!("invalid length for item! sz={} tg={}", sz, tg);
      }
      match tg {
        0 => Decodable::decode(d).map(Weapon),
        1 => Ok(Backpack),
        2 => Ok(Helmet),
        3 => Ok(Shield),
        4 => Ok(ChainMail),
        5 => Ok(Potion2Hp),
        6 => Ok(Potion4Hp),
        7 => Ok(Potion5Hp),
        8 => Ok(Potion6Hp),
        9 => Ok(StrengthPotion),
        10 => d.read_u8().map(GenSpecial),
        11 => d.read_u8().map(GenBackpack),
        12 => Ok(Meal),
        13 => Ok(Gold),
        14 => Ok(Laumspur),
        15 => Ok(SilverHelm),
        _ => Err(d.error(&format!("Unknown item tag: {}", tg))),
      }
    })
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum NextStep {
  HasLost(u16),
  HasWon(CharacterVariable),
  NewChapter(u16, CharacterVariable),
}

impl NextStep {
  pub fn chapter(&self) -> Option<u16> {
    match self {
      NextStep::HasLost(c) => Some(*c),
      NextStep::HasWon(_) => None,
      NextStep::NewChapter(c, _) => Some(*c),
    }
  }
}

fn nextstep_from_cbor(c: Cbor) -> Result<NextStep> {
  cbor_enum("NextStep", c, |tg, r| match tg {
    0 => cbor_next_decode("chapter id", r).map(NextStep::HasLost),
    1 => cbor_next_decode("character variable", r).map(NextStep::HasWon),
    2 => {
      let cid = cbor_next_decode("chapter id", r)?;
      let cv = cbor_next_decode("character variable", r)?;
      Ok(NextStep::NewChapter(cid, cv))
    }
    _ => Err(anyhow!("invalid tag {}", tg)),
  })
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Equipment(pub u64);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct CharacterVariable {
  pub curendurance: i8,
  flags: u32,
  cequipment: Equipment,
  cprevequipment: Equipment,
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

impl Decodable for CharacterVariable {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    d.read_seq(|d, sz| {
      if sz != 5 {
        panic!("invalid sz for CharacterVariable {}", sz);
      }
      d.read_u8()?;
      let out = CharacterVariable {
        curendurance: d.read_i8()?,
        flags: d.read_u32()?,
        cequipment: Equipment(d.read_u64()?),
        cprevequipment: Equipment(d.read_u64()?),
      };
      Ok(out)
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

impl Decodable for CharacterConstant {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    d.read_seq(|d, sz| {
      if sz != 5 {
        panic!("ln={}", sz);
      }
      d.read_u8()?;
      let maxendurance = d.read_i8()?;
      let combat_skill = d.read_u8()?;
      let discipline = Decodable::decode(d)?;
      let bookid = Decodable::decode(d)?;
      Ok(CharacterConstant {
        maxendurance,
        combat_skill,
        discipline,
        bookid,
      })
    })
  }
}

#[derive(Debug, PartialEq, Eq)]
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

impl Decodable for Discipline {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    use Discipline::*;
    d.read_seq(|d, sz| {
      let tg = d.read_u8()?;
      if (sz != 1) && (tg != 5 && sz != 2) {
        panic!("invalid length for discipline! sz={} tg={}", sz, tg);
      }
      match tg {
        0 => Ok(Camouflage),
        1 => Ok(Hunting),
        2 => Ok(SixthSense),
        3 => Ok(Tracking),
        4 => Ok(Healing),
        5 => Decodable::decode(d).map(WeaponSkill),
        6 => Ok(MindShield),
        7 => Ok(MindBlast),
        8 => Ok(AnimalKinship),
        9 => Ok(MindOverMatter),
        _ => Err(d.error(&format!("Unknown discipline tag: {}", tg))),
      }
    })
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

impl Decodable for Weapon {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    use Weapon::*;
    d.read_seq(|d, sz| {
      if sz != 1 {
        panic!("invalid sz for weapon");
      }
      let tg = d.read_u8()?;
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
        _ => Err(d.error(&format!("Unknown weapon tag: {}", tg))),
      }
    })
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

impl Decodable for Book {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    use Book::*;
    d.read_seq(|d, sz| {
      if sz != 1 {
        panic!("invalid sz for book");
      }
      let tg = d.read_u8()?;
      match tg {
        0 => Ok(Book01),
        1 => Ok(Book02),
        2 => Ok(Book03),
        3 => Ok(Book04),
        4 => Ok(Book05),
        _ => Err(d.error(&format!("Unknown book tag: {}", tg))),
      }
    })
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
}

impl Decodable for Flag {
  fn decode<D>(d: &mut D) -> std::result::Result<Self, <D as rustc_serialize::Decoder>::Error>
  where
    D: rustc_serialize::Decoder,
  {
    use Flag::*;
    d.read_seq(|d, sz| {
      if sz != 1 {
        panic!("invalid sz for flag");
      }
      let tg = d.read_u8()?;
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
        _ => Err(d.error(&format!("Unknown flag tag: {}", tg))),
      }
    })
  }
}
