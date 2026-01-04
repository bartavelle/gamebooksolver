use crate::solver::base::ChoppedSolution;
use crate::solver::rational::Rational;
use bincode::{Decode, Encode};
use serde::Deserializer;
use serde::de::Visitor;
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};
use std::collections::{BTreeMap, HashSet};
use std::hash::Hash;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Default, PartialOrd, Ord, Copy, Clone, Hash, Encode, Decode, Serialize, Deserialize)]
pub struct NoPrevEq;

impl std::fmt::Display for NoPrevEq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NA")
    }
}

impl From<Equipment> for NoPrevEq {
    fn from(_value: Equipment) -> Self {
        unreachable!("should never be converted")
    }
}

impl From<NoPrevEq> for Equipment {
    fn from(_value: NoPrevEq) -> Self {
        Equipment::default()
    }
}

#[derive(Debug, PartialEq, Eq, Decode, Encode)]
pub enum GSolDump<P> {
    Prev(SolutionDump<P, Equipment>),
    Noprev(SolutionDump<P, NoPrevEq>),
}

#[derive(Debug, PartialEq, Eq, Decode, Encode)]
pub struct SolutionDump<P, PREV: Into<Equipment> + From<Equipment> + Ord> {
    pub soldesc: SolDesc,
    pub content: BTreeMap<NextStep<PREV>, ChoppedSolution<P, NextStep<PREV>>>,
}

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Encode, Decode)]
pub struct SolDesc {
    #[serde(alias = "_finalchapters")]
    pub finalchapters: Vec<u16>,
    #[serde(alias = "_ccst")]
    pub ccst: CharacterConstant,
    #[serde(alias = "_cvar")]
    pub cvar: CVarState,
}

impl SolDesc {
    pub fn cvariable<PREV: From<Equipment> + Into<Equipment> + Default>(&self) -> CharacterVariableG<PREV> {
        let mut cvar = CharacterVariableG::new(self.ccst.maxendurance);
        cvar.cequipment.add_item(&Item::Backpack, 1);
        cvar.cequipment.set_gold(self.cvar.gold);
        let itms = self.cvar.items.as_ref().expect("default items not yet implemented");
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

#[derive(Debug, PartialEq, Eq, Clone, Deserialize, Serialize, Encode, Decode)]
pub struct CharacterConstant {
    #[serde(alias = "_maxendurance")]
    pub maxendurance: i8,
    #[serde(alias = "_combatSkill")]
    pub combat_skill: u8,
    #[serde(alias = "_discipline")]
    pub discipline: Vec<Discipline>,
    #[serde(alias = "_bookid")]
    pub bookid: Book,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Encode, Decode)]
pub struct CVarState {
    #[serde(alias = "_cvitems")]
    pub items: Option<Vec<(Item, i64)>>,
    #[serde(alias = "_cvgold")]
    pub gold: u8,
    #[serde(alias = "_cvflags")]
    pub flags: Vec<Flag>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Serialize, Deserialize, Encode, Decode)]
pub enum NextStep<PREV: Into<Equipment> + From<Equipment>> {
    HasLost(u16),
    HasWon(CharacterVariableG<PREV>),
    NewChapter(u16, CharacterVariableG<PREV>),
}

impl<PREV: Into<Equipment> + From<Equipment> + std::fmt::Display + Copy> std::fmt::Display for NextStep<PREV> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            NextStep::HasLost(cid) => write!(f, "lost:{}", cid),
            NextStep::HasWon(cv) => write!(f, "won:{}", cv),
            NextStep::NewChapter(cid, cv) => write!(f, "c:{} {}", cid, cv),
        }
    }
}

impl<PREV: Into<Equipment> + From<Equipment>> NextStep<PREV> {
    pub fn chapter(&self) -> Option<u16> {
        match self {
            NextStep::HasLost(c) => Some(*c),
            NextStep::HasWon(_) => None,
            NextStep::NewChapter(c, _) => Some(*c),
        }
    }
    pub fn cvar(&self) -> Option<&CharacterVariableG<PREV>> {
        match self {
            NextStep::HasLost(_) => None,
            NextStep::HasWon(s) => Some(s),
            NextStep::NewChapter(_, s) => Some(s),
        }
    }
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Hash,
    Clone,
    Copy,
    SerializeDisplay,
    PartialOrd,
    Ord,
    Default,
    DeserializeFromStr,
    Encode,
    Decode,
)]
pub struct Equipment {
    pub gold: u8,
    pub meal_ls: u8,
    pub a: u16,
    pub b: u32,
}

impl std::fmt::Display for Equipment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut spc = false;
        for (i, q) in self.items() {
            if spc {
                write!(f, "/")?;
            }
            match i {
                Item::Weapon(weapon) => write!(f, "{weapon:?}")?,
                Item::GenSpecial(s) => write!(f, "SGen{s:?}")?,
                Item::GenBackpack(p) => write!(f, "BGen{p:?}")?,
                _ => write!(f, "{i:?}")?,
            }
            if q != 1 {
                write!(f, ":{q}")?;
            }
            spc = true;
        }
        Ok(())
    }
}

impl FromStr for Equipment {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut base = Equipment::empty();

        for p in s.split("/") {
            let (iname, quantity) = p.split_once(':').unwrap_or((p, "1"));
            let itm = iname.parse()?;
            let q = quantity.parse().map_err(|rr: ParseIntError| rr.to_string())?;
            base.add_item(&itm, q);
        }

        Ok(base)
    }
}

pub enum ItemStorageSlot {
    A(u8),
    B(u8),
}

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
    pub const fn empty() -> Self {
        Self {
            gold: 0,
            meal_ls: 0,
            a: 0,
            b: 0,
        }
    }

    const fn set_gold(&mut self, q: u8) {
        self.gold = q;
    }
    const fn set_laumspur(&mut self, q: u8) {
        self.meal_ls = (self.meal_ls & 0x0f) | ((q & 0xf) << 4);
    }
    const fn set_meal(&mut self, q: u8) {
        self.meal_ls = (self.meal_ls & 0xf0) | (q & 0xf);
    }
    const fn get_gold(&self) -> u8 {
        self.gold
    }
    const fn get_meal(&self) -> u8 {
        self.meal_ls & 0xf
    }
    const fn get_laumspur(&self) -> u8 {
        self.meal_ls >> 4
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
            _ => match i.get_idx() {
                ItemStorageSlot::A(i) => self.a |= 1 << i,
                ItemStorageSlot::B(i) => self.b |= 1 << i,
            },
        }
    }
    pub const fn del_item(&mut self, i: &Item, q: i64) {
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
            _ => match i.get_idx() {
                ItemStorageSlot::A(i) => self.a &= !(1 << i),
                ItemStorageSlot::B(i) => self.b &= !(1 << i),
            },
        }
    }
    pub const fn get_item_count(&self, i: &Item) -> u8 {
        match i {
            Item::Gold => self.get_gold(),
            Item::Meal => self.get_meal(),
            Item::Laumspur => self.get_laumspur(),
            _ => match i.get_idx() {
                ItemStorageSlot::A(i) => {
                    if self.a & (1 << i) != 0 {
                        1
                    } else {
                        0
                    }
                }
                ItemStorageSlot::B(i) => {
                    if self.b & (1 << i) != 0 {
                        1
                    } else {
                        0
                    }
                }
            },
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
                if cnt > 0 { Some((*i, cnt)) } else { None }
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, PartialOrd, Ord, Copy, Deserialize, Encode, Decode)]
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

    pub fn all(&self) -> Vec<Flag> {
        Flag::VALUES.iter().filter(|f| self.has(**f)).copied().collect()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, PartialOrd, Ord, Deserialize, Encode, Decode)]
pub struct CharacterVariableG<PREV: Into<Equipment> + From<Equipment>> {
    pub curendurance: i8,
    pub flags: Flags,
    pub cequipment: Equipment,
    /// used to store the "stored" equipment, for example when lonewolf loses his equipment
    pub cprevequipment: PREV,
}

pub type CharacterVariable = CharacterVariableG<Equipment>;

impl<PREV: From<Equipment> + Into<Equipment> + Copy> std::fmt::Display for CharacterVariableG<PREV> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} {:?} {:?} {:?}",
            self.curendurance,
            self.flags,
            self.cequipment.items(),
            self.cprevequipment.into().items()
        )
    }
}

impl<PREV: From<Equipment> + Into<Equipment> + Default> CharacterVariableG<PREV> {
    pub fn new(endurance: i8) -> Self {
        CharacterVariableG {
            curendurance: endurance,
            flags: Flags(0),
            cequipment: Equipment::default(),
            cprevequipment: PREV::default(),
        }
    }
}

impl<PREV: From<Equipment> + Into<Equipment>> CharacterVariableG<PREV> {
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

#[derive(Debug, PartialEq, Eq, Serialize, Hash, Clone, Copy, Encode, Decode)]
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
            _ => match s.strip_prefix("BGenCounter ").or_else(|| s.strip_prefix("BGen")) {
                Some(n) => n
                    .parse::<u8>()
                    .map(Item::GenBackpack)
                    .map_err(|rr| format!("unexpected item {}: {}", s, rr)),
                None => match s.strip_prefix("SGenCounter ").or_else(|| s.strip_prefix("SGen")) {
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
            return Err(serde::de::Error::invalid_value(serde::de::Unexpected::Str(&key), &self));
        }
        let v: String = mmap.next_value()?;
        match v.parse() {
            Ok(d) => Ok(d),
            _ => Err(serde::de::Error::invalid_value(serde::de::Unexpected::Str(&v), &self)),
        }
    }

    fn visit_str<E>(self, s: &str) -> Result<Item, E>
    where
        E: serde::de::Error,
    {
        match s.parse() {
            Ok(x) => Ok(x),
            Err(_) => Err(serde::de::Error::invalid_value(serde::de::Unexpected::Str(s), &self)),
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

    pub const fn get_idx(&self) -> ItemStorageSlot {
        use ItemStorageSlot::*;
        match self {
            Item::Backpack => A(0),
            Item::StrengthPotion4 => A(1),
            Item::Shield => A(2),
            Item::BodyArmor => A(3),
            Item::Potion2Hp => A(4),
            Item::Potion4Hp => A(5),
            Item::Potion5Hp => A(6),
            Item::Potion6Hp => A(7),
            Item::StrengthPotion => A(8),
            Item::Helmet => A(9),
            Item::Laumspur => A(10),
            Item::Gold => A(11),
            Item::Meal => A(12),
            Item::Weapon(w) => {
                let n = *w as u8;
                match n {
                    0..=2 => A(13 + n),
                    _ => B(n - 3),
                }
            }
            Item::GenSpecial(n) => B(*n + 9),
            Item::GenBackpack(n) => B(*n + 20),
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

#[derive(Debug, PartialEq, Eq, Serialize, Hash, Clone, Copy, Encode, Decode)]
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
            _ => Err(serde::de::Error::invalid_value(serde::de::Unexpected::Str(s), &self)),
        }
    }

    fn visit_map<A: serde::de::MapAccess<'de>>(self, omap: A) -> Result<Discipline, A::Error> {
        let mut mmap = omap;
        let mk: Option<String> = mmap.next_key()?;
        let key = mk.unwrap();
        if key != "WeaponSkill" {
            return Err(serde::de::Error::invalid_value(serde::de::Unexpected::Str(&key), &self));
        }
        let v: String = mmap.next_value()?;
        match v.parse() {
            Ok(d) => Ok(d),
            _ => Err(serde::de::Error::invalid_value(serde::de::Unexpected::Str(&v), &self)),
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash, Encode, Decode)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Encode, Decode)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash, Encode, Decode)]
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

pub fn max_hp<PREV: From<Equipment> + Into<Equipment>>(
    ccst: &CharacterConstant,
    cvar: &CharacterVariableG<PREV>,
) -> i8 {
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

pub fn mkchar<PREV: Into<Equipment> + From<Equipment> + Default>(
    ccst: &CharacterConstant,
    cvar: &CVarState,
) -> CharacterVariableG<PREV> {
    let mut cv = CharacterVariableG::new(ccst.maxendurance);
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

#[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize, Encode, Decode)]
pub struct CompactState<PREV: Into<Equipment> + From<Equipment>> {
    pub chapter: u16,
    pub character: CharacterVariableG<PREV>,
    pub score: f32,
}

impl<PREV: Into<Equipment> + From<Equipment>> CompactState<PREV> {
    pub fn from_choppedsolution<P: Rational>(
        cur: NextStep<PREV>,
        cs: ChoppedSolution<P, NextStep<PREV>>,
        useless_chapters: &HashSet<u16>,
    ) -> Option<Self> {
        let score = cs.score().to_f32();
        match cur {
            NextStep::NewChapter(cid, cv) if !useless_chapters.contains(&cid) => Some(CompactState {
                chapter: cid,
                character: cv,
                score,
            }),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct CompactSolution<PREV: Into<Equipment> + From<Equipment>> {
    pub soldesc: SolDesc,
    pub content: Vec<CompactState<PREV>>,
}

impl<PREV: Into<Equipment> + From<Equipment> + Eq> CompactSolution<PREV> {
    fn find_scored_chapter(&self, cid: u16, cv: &CharacterVariableG<PREV>) -> Option<&CompactState<PREV>> {
        self.content.iter().find(|x| x.chapter == cid && &x.character == cv)
    }

    pub fn get_score(&self, v: &NextStep<PREV>) -> Option<f64> {
        match v {
            NextStep::HasLost(_) => Some(0.0),
            NextStep::HasWon(_) => Some(1.0),
            NextStep::NewChapter(cid, cv) => self.find_scored_chapter(*cid, cv).map(|s| s.score as f64),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum CompactSolutionG {
    WithStorage(CompactSolution<Equipment>),
    NoStorage(CompactSolution<NoPrevEq>),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn equipment_rb() {
        let mut base = Equipment::empty();
        base.set_gold(12);
        base.set_laumspur(2);
        base.set_meal(3);
        for i in Item::VALUES {
            base.add_item(&i, 1);
            let encoded = serde_json::to_string(&base).unwrap();
            let decoded = serde_json::from_str::<Equipment>(&encoded).unwrap();
            assert_eq!(base, decoded);
        }
    }
}
