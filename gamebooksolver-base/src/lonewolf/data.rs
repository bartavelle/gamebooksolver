use crate::lonewolf::chapter::{
    BoolCond, Chapter, ChapterId, ChapterOutcome, CombatSkill, Decision, Endurance, FightModifier, SimpleOutcome,
    SpecialChapter,
};
use crate::lonewolf::mini::{
    Book, CVarState, CharacterConstant, CharacterVariableG, Discipline, Equipment, Flag, Flags, Item, NextStep, Slot,
    SolutionDump, StoredEquipment, Weapon,
};
use crate::solver::base::FastMap;
use crate::solver::rational::Rational;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::collections::{BTreeSet, HashMap};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Multistat<P> {
    pub mbook: Book,
    pub msdisciplines: BTreeSet<Discipline>,
    pub variable: CVarState,
    pub msentries: Vec<MultistatEntry<P>>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct MultistatEntry<P> {
    pub meendurance: Endurance,
    pub mskill: CombatSkill,
    pub mscore: f64,
    pub mratio: P,
    pub states: usize,
}

impl<P: Rational> Multistat<P> {
    pub fn from_soldump<PREV: StoredEquipment>(dump: &SolutionDump<P, PREV>) -> Option<Self> {
        let score = dump
            .content
            .iter()
            .filter_map(|x| match &x.0 {
                NextStep::NewChapter(cid, _) => {
                    if *cid == 1 {
                        Some(x.1.score())
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .next()?;

        Some(Multistat {
            mbook: dump.soldesc.ccst.bookid,
            msdisciplines: dump.soldesc.ccst.discipline.iter().cloned().collect(),
            variable: dump.soldesc.cvar.clone(),
            msentries: vec![MultistatEntry {
                meendurance: Endurance(dump.soldesc.ccst.maxendurance),
                mscore: score.t_f64(),
                mratio: score,
                mskill: CombatSkill(dump.soldesc.ccst.combat_skill as i8),
                states: dump.content.len(),
            }],
        })
    }
}

pub fn get_destinations<P>(dec: &Decision<P>) -> HashSet<ChapterId> {
    fn get_destinations_fd<P>(fd: &FightModifier<P>) -> Option<ChapterId> {
        match fd {
            FightModifier::Timed(_, sub) => get_destinations_fd(sub),
            FightModifier::Evaded(cid) => Some(*cid),
            FightModifier::FakeFight(cid) => Some(*cid),
            FightModifier::OnDamage(cid) => Some(*cid),
            FightModifier::OnLose(cid) => Some(*cid),
            FightModifier::OnNotYetWon(cid) => Some(*cid),
            FightModifier::StopFight(cid) => Some(*cid),
            FightModifier::OnPickOne(cid) => Some(*cid),
            _ => None,
        }
    }
    fn get_destinations_o<P>(co: &ChapterOutcome<P>) -> HashSet<ChapterId> {
        match co {
            ChapterOutcome::Conditionally(lst) => lst.iter().flat_map(|(_, o)| get_destinations_o(o)).collect(),
            ChapterOutcome::Fight(fds, nxt) => {
                let mut o = get_destinations_o(nxt);
                for d in fds.fight_mod.iter().filter_map(get_destinations_fd) {
                    o.insert(d);
                }
                o
            }
            ChapterOutcome::EnterFight(_, cid) => [*cid].into_iter().collect(),
            ChapterOutcome::Round(fds, nxt, next_rounds) => {
                let mut o = get_destinations_o(nxt);
                o.extend(fds.fight_mod.iter().filter_map(get_destinations_fd));
                // the first one is for an opponent without any endurance left, and the fight is over by then
                o.extend(next_rounds.iter().skip(1).copied());
                o
            }
            ChapterOutcome::GameLost => HashSet::new(),
            ChapterOutcome::GameWon => HashSet::new(),
            ChapterOutcome::Goto(cid) => [*cid].into_iter().collect(),
            ChapterOutcome::OneRound(fds, o1, o2, o3) => {
                let mut o = HashSet::new();
                for d in fds.fight_mod.iter().filter_map(get_destinations_fd) {
                    o.insert(d);
                }
                o.extend(get_destinations_o(o1));
                o.extend(get_destinations_o(o2));
                o.extend(get_destinations_o(o3));
                o
            }
            ChapterOutcome::Randomly(lst) => lst.iter().flat_map(|(_, o)| get_destinations_o(o)).collect(),
            ChapterOutcome::Simple(_, nxt) => get_destinations_o(nxt),
            ChapterOutcome::LoseItemFrom(_, _, o) => get_destinations_o(o),
        }
    }
    match dec {
        Decision::AfterCombat(nxt) => get_destinations(nxt),
        Decision::Canbuy(_, _, nxt) => get_destinations(nxt),
        Decision::Cansell(_, _, nxt) => get_destinations(nxt),
        Decision::CanTake(_, _, nxt) => get_destinations(nxt),
        Decision::Conditional(_, nxt) => get_destinations(nxt),
        Decision::Decisions(lst) => lst.iter().flat_map(|(_, d)| get_destinations(d)).collect(),
        Decision::EvadeFight(_, cid, _, co) => {
            let mut o = get_destinations_o(co);
            o.insert(*cid);
            o
        }
        Decision::None(co) => get_destinations_o(co),
        Decision::RemoveItemFrom(_, _, nxt) => get_destinations(nxt),
        Decision::RetrieveEquipment(nxt) => get_destinations(nxt),
        Decision::Special(s) => match s {
            SpecialChapter::B05S127 => [159, 93].iter().copied().map(ChapterId).collect(),
            SpecialChapter::Cartwheel => [169, 136].iter().copied().map(ChapterId).collect(),
            SpecialChapter::Portholes => [197].iter().copied().map(ChapterId).collect(),
        },
    }
}

// the number of generic special and backpack items
const GEN_ITEMS: u8 = 12;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub struct WeaponSet(u16);

impl WeaponSet {
    const ALL: WeaponSet = WeaponSet(u16::MAX);

    pub fn contains(self, w: Weapon) -> bool {
        self.0 & (1 << w as u8) != 0
    }

    fn add_item(&mut self, i: &Item) {
        if let Item::Weapon(w) = i {
            self.0 |= 1 << *w as u8;
        }
    }

    // all the weapons this condition accepts, when it only checks for the presence of one of them
    fn any_of(bc: &BoolCond) -> Option<WeaponSet> {
        match bc {
            BoolCond::HasItem(i @ Item::Weapon(_), 1) => {
                let mut s = WeaponSet::default();
                s.add_item(i);
                Some(s)
            }
            BoolCond::COr(a, b) => Some(WeaponSet(Self::any_of(a)?.0 | Self::any_of(b)?.0)),
            _ => None,
        }
    }

    fn cond(&mut self, bc: &BoolCond) {
        // having any weapon does not depend on which one it is
        if Self::any_of(bc).is_some_and(|s| Weapon::VALUES.iter().all(|w| s.contains(*w))) {
            return;
        }
        match bc {
            BoolCond::HasItem(i, _) => self.add_item(i),
            BoolCond::Not(c) => self.cond(c),
            BoolCond::COr(a, b) | BoolCond::CAnd(a, b) => {
                self.cond(a);
                self.cond(b);
            }
            BoolCond::HasDiscipline(_)
            | BoolCond::Always(_)
            | BoolCond::HasEndurance(_)
            | BoolCond::HasFlag(_)
            | BoolCond::HasLevel(_) => (),
        }
    }

    fn outcome<P>(&mut self, co: &ChapterOutcome<P>) {
        match co {
            ChapterOutcome::Fight(_, o) | ChapterOutcome::Round(_, o, _) | ChapterOutcome::LoseItemFrom(_, _, o) => {
                self.outcome(o)
            }
            ChapterOutcome::OneRound(_, a, b, c) => {
                self.outcome(a);
                self.outcome(b);
                self.outcome(c);
            }
            ChapterOutcome::Randomly(lst) => lst.iter().for_each(|(_, o)| self.outcome(o)),
            ChapterOutcome::Conditionally(lst) => lst.iter().for_each(|(c, o)| {
                self.cond(c);
                self.outcome(o);
            }),
            ChapterOutcome::Simple(sos, o) => {
                for so in sos {
                    match so {
                        SimpleOutcome::GainItem(i, _) | SimpleOutcome::LoseItem(i, _) => self.add_item(i),
                        SimpleOutcome::DamagePlayer(_)
                        | SimpleOutcome::HealPlayer(_)
                        | SimpleOutcome::FullHeal
                        | SimpleOutcome::HalfHeal
                        | SimpleOutcome::LoseItemKind(_)
                        | SimpleOutcome::MustEat(_)
                        | SimpleOutcome::StoreEquipment
                        | SimpleOutcome::RetrieveEquipment
                        | SimpleOutcome::SetFlag(_)
                        | SimpleOutcome::ClearFlag(_) => (),
                    }
                }
                self.outcome(o);
            }
            ChapterOutcome::Goto(_)
            | ChapterOutcome::EnterFight(_, _)
            | ChapterOutcome::GameLost
            | ChapterOutcome::GameWon => (),
        }
    }

    fn decision<P>(&mut self, dec: &Decision<P>) {
        match dec {
            Decision::Decisions(lst) => lst.iter().for_each(|(_, d)| self.decision(d)),
            Decision::RetrieveEquipment(d) | Decision::AfterCombat(d) | Decision::RemoveItemFrom(_, _, d) => {
                self.decision(d)
            }
            Decision::CanTake(i, _, d) | Decision::Canbuy(i, _, d) | Decision::Cansell(i, _, d) => {
                self.add_item(i);
                self.decision(d);
            }
            Decision::Conditional(c, d) => {
                self.cond(c);
                self.decision(d);
            }
            Decision::Special(_) => (),
            Decision::None(o) => self.outcome(o),
            Decision::EvadeFight(_, _, _, o) => self.outcome(o),
        }
    }
}

/// The items and flags that are kept from a book to the next one, and so that have an influence on the final score.
pub fn get_boundary(bookid: Book) -> (HashSet<Item>, HashSet<Flag>) {
    use Flag::*;
    use Item::{BodyArmor, Gold, Helmet, StrengthPotion, StrengthPotion4};

    match bookid {
        Book::Book01 => ([Gold].into_iter().collect(), HashSet::new()),
        Book::Book02 => ([Helmet, BodyArmor].into_iter().collect(), HashSet::new()),
        Book::Book03 => (
            [Item::Weapon(Weapon::Sommerswerd), StrengthPotion4, Item::SILVERHELMET]
                .into_iter()
                .collect(),
            HashSet::new(),
        ),
        Book::Book04 => (
            [
                Item::Weapon(Weapon::Sommerswerd),
                Item::SILVERHELMET,
                BodyArmor,
                StrengthPotion,
                StrengthPotion4,
            ]
            .into_iter()
            .collect(),
            [FoughtElix, PermanentSkillReduction2].into_iter().collect(),
        ),
        Book::Book05 => (HashSet::new(), HashSet::new()),
    }
}

/// The parts of the state that can have an influence on the rest of the game. This only covers the parts that are only
/// read by what is written in the books, the others being always kept: the shield, armor, helmets, backpack, meals and
/// potions are read by the rules, as well as the flags in RULES_FLAGS.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
struct Reads {
    special: u16,
    backpack: u16,
    flags: u32,
    // the number of items in the backpack, that can make it impossible to take something
    backpack_space: bool,
}

impl Reads {
    const ALL: Reads = Reads {
        special: u16::MAX,
        backpack: u16::MAX,
        flags: u32::MAX,
        backpack_space: true,
    };

    fn merge(self, o: Reads) -> Reads {
        Reads {
            special: self.special | o.special,
            backpack: self.backpack | o.backpack,
            flags: self.flags | o.flags,
            backpack_space: self.backpack_space || o.backpack_space,
        }
    }

    /// What is always read in this book, by the rules or for the final score
    fn always(bookid: Book) -> Reads {
        let mut r = Reads {
            flags: RULES_FLAGS.0,
            ..Reads::default()
        };
        // the silver helmet is used in fights, the potion of book 5 is used by the rules
        r.item(&Item::SILVERHELMET);
        if bookid == Book::Book05 {
            r.item(&Item::GenBackpack(2));
        }
        let (items, flags) = get_boundary(bookid);
        items.iter().for_each(|i| r.item(i));
        flags.iter().for_each(|f| r.flags |= 1 << (*f as usize));
        r
    }

    fn item(&mut self, i: &Item) {
        match i {
            Item::GenSpecial(n) => self.special |= 1 << *n,
            Item::GenBackpack(n) => self.backpack |= 1 << *n,
            _ => (),
        }
    }

    fn slot(&mut self, s: &Slot) {
        match s {
            Slot::Backpack => self.backpack_space = true,
            Slot::Special => self.special = u16::MAX,
            Slot::Pouch | Slot::Weapon => (),
        }
    }

    fn cond(&mut self, bc: &BoolCond) {
        match bc {
            BoolCond::HasItem(i, _) => self.item(i),
            BoolCond::HasFlag(f) => self.flags |= 1 << (*f as usize),
            BoolCond::Not(c) => self.cond(c),
            BoolCond::COr(a, b) | BoolCond::CAnd(a, b) => {
                self.cond(a);
                self.cond(b);
            }
            BoolCond::HasDiscipline(_) | BoolCond::Always(_) | BoolCond::HasEndurance(_) | BoolCond::HasLevel(_) => (),
        }
    }

    fn outcome<P>(&mut self, co: &ChapterOutcome<P>) {
        match co {
            ChapterOutcome::Fight(_, o) | ChapterOutcome::Round(_, o, _) | ChapterOutcome::Simple(_, o) => self.outcome(o),
            ChapterOutcome::LoseItemFrom(s, _, o) => {
                self.slot(s);
                self.outcome(o);
            }
            ChapterOutcome::OneRound(_, a, b, c) => {
                self.outcome(a);
                self.outcome(b);
                self.outcome(c);
            }
            ChapterOutcome::Randomly(lst) => lst.iter().for_each(|(_, o)| self.outcome(o)),
            ChapterOutcome::Conditionally(lst) => lst.iter().for_each(|(c, o)| {
                self.cond(c);
                self.outcome(o);
            }),
            ChapterOutcome::Goto(_)
            | ChapterOutcome::EnterFight(_, _)
            | ChapterOutcome::GameLost
            | ChapterOutcome::GameWon => (),
        }
    }

    fn decision<P>(&mut self, dec: &Decision<P>) {
        match dec {
            Decision::Decisions(lst) => lst.iter().for_each(|(_, d)| self.decision(d)),
            Decision::AfterCombat(d) => self.decision(d),
            // taking back the items depends on the space in the backpack
            Decision::RetrieveEquipment(d) => {
                self.backpack_space = true;
                self.decision(d);
            }
            Decision::RemoveItemFrom(s, _, d) => {
                self.slot(s);
                self.decision(d);
            }
            Decision::CanTake(i, _, d) => {
                self.slot_space(i);
                self.decision(d);
            }
            Decision::Canbuy(i, _, d) => {
                // the gold can be spent only if the item can be taken, and it depends on the items that are held
                self.item(i);
                self.slot_space(i);
                self.decision(d);
            }
            Decision::Cansell(i, _, d) => {
                self.item(i);
                self.decision(d);
            }
            Decision::Conditional(c, d) => {
                self.cond(c);
                self.decision(d);
            }
            Decision::Special(_) => (),
            Decision::None(o) => self.outcome(o),
            Decision::EvadeFight(_, _, _, o) => self.outcome(o),
        }
    }

    fn slot_space(&mut self, i: &Item) {
        if i.slot() == Slot::Backpack {
            self.backpack_space = true;
        }
    }
}

const RULES_FLAGS: Flags = Flags(0)
    .setn(Flag::Poisonned2)
    .setn(Flag::HadCombat)
    .setn(Flag::LimbDeath)
    .setn(Flag::PermanentSkillReduction)
    .setn(Flag::PermanentSkillReduction2)
    .setn(Flag::StrengthPotionActive)
    .setn(Flag::PotentStrengthPotionActive);

/// The gold that is enough for everything that can be done from now on, given the same value for the following chapters.
/// A state with at least that much gold can go through the same paths as a state with more. There is no need to take
/// gains into account, as they can only leave more gold, nor what makes all the gold disappear.
struct GoldNeeds<'t>(&'t HashMap<ChapterId, u8>);

impl GoldNeeds<'_> {
    const ALL: u8 = u8::MAX;

    fn chapter(&self, cid: &ChapterId) -> u8 {
        self.0.get(cid).copied().unwrap_or(Self::ALL)
    }

    fn cond(&self, bc: &BoolCond) -> u8 {
        match bc {
            BoolCond::HasItem(Item::Gold, q) => *q,
            BoolCond::Not(c) => self.cond(c),
            BoolCond::COr(a, b) | BoolCond::CAnd(a, b) => self.cond(a).max(self.cond(b)),
            _ => 0,
        }
    }

    fn outcome<P>(&self, co: &ChapterOutcome<P>) -> u8 {
        match co {
            ChapterOutcome::Fight(_, o) => self.outcome(o),
            ChapterOutcome::OneRound(_, a, b, c) => self.outcome(a).max(self.outcome(b)).max(self.outcome(c)),
            ChapterOutcome::Randomly(lst) => lst.iter().map(|(_, o)| self.outcome(o)).max().unwrap_or(0),
            ChapterOutcome::Conditionally(lst) => lst
                .iter()
                .map(|(c, o)| self.cond(c).max(self.outcome(o)))
                .max()
                .unwrap_or(0),
            ChapterOutcome::Simple(sos, o) => sos.iter().fold(self.outcome(o), |acc, so| match so {
                SimpleOutcome::LoseItem(Item::Gold, q) => acc.saturating_add(*q),
                _ => acc,
            }),
            ChapterOutcome::Goto(cid) => self.chapter(cid),
            ChapterOutcome::GameLost | ChapterOutcome::GameWon => 0,
            // it depends on how much gold there is
            ChapterOutcome::LoseItemFrom(Slot::Pouch, _, _) => Self::ALL,
            ChapterOutcome::LoseItemFrom(_, _, o) | ChapterOutcome::Round(_, o, _) => self.outcome(o),
            ChapterOutcome::EnterFight(_, cid) => self.chapter(cid),
        }
    }

    fn decision<P>(&self, dec: &Decision<P>) -> u8 {
        match dec {
            Decision::Decisions(lst) => lst.iter().map(|(_, d)| self.decision(d)).max().unwrap_or(0),
            Decision::RemoveItemFrom(Slot::Pouch, _, _) | Decision::Special(SpecialChapter::Cartwheel) => Self::ALL,
            Decision::RetrieveEquipment(d)
            | Decision::AfterCombat(d)
            | Decision::RemoveItemFrom(_, _, d)
            | Decision::CanTake(_, _, d)
            | Decision::Cansell(_, _, d) => self.decision(d),
            Decision::Canbuy(_, p, d) => self.decision(d).saturating_add(p.0),
            Decision::Conditional(c, d) => self.cond(c).max(self.decision(d)),
            Decision::Special(_) => 0,
            Decision::None(o) | Decision::EvadeFight(_, _, _, o) => self.outcome(o),
        }
    }
}

fn gold_needs<P>(bookid: Book, book: &HashMap<ChapterId, Chapter<P>>) -> HashMap<ChapterId, u8> {
    // what is left at the end of the book is part of the score
    if get_boundary(bookid).0.contains(&Item::Gold) {
        return HashMap::new();
    }
    let mut needs: HashMap<ChapterId, u8> = book.keys().map(|cid| (*cid, 0)).collect();
    loop {
        let mut changed = false;
        for (cid, chapter) in book {
            let cur = GoldNeeds(&needs);
            // all the destinations count, including those that are found in the fight modifiers
            let need = get_destinations(&chapter.pchoice)
                .iter()
                .fold(cur.decision(&chapter.pchoice), |acc, d| acc.max(cur.chapter(d)));
            if need != needs[cid] {
                needs.insert(*cid, need);
                changed = true;
            }
        }
        if !changed {
            return needs;
        }
    }
}

/// What can matter from a state in each chapter. For the weapons, those that the rest of the game can tell apart from the
/// others, because they are named in that chapter or in one that can be reached from it. Fights are not looked at, as
/// they only depend on the weapon skill and on the two magical weapons. For the other items and flags, those that can
/// be read in that chapter or in one that can be reached from it, see `Reads`. For the gold, how much of it can be of any
/// use, see `GoldNeeds`.
#[derive(Debug, Clone, Default)]
pub struct WeaponUse {
    weapons: FastMap<ChapterId, WeaponSet>,
    reads: FastMap<ChapterId, Reads>,
    gold: FastMap<ChapterId, u8>,
}

/// A condition, with what can be known from the disciplines of the character already computed.
fn specialize_cond(ccst: &CharacterConstant, bc: &BoolCond) -> BoolCond {
    use BoolCond::*;
    match bc {
        HasDiscipline(d) => Always(ccst.discipline.contains(d)),
        HasLevel(lvl) => Always(ccst.discipline.len() >= *lvl as usize),
        Not(c) => match specialize_cond(ccst, c) {
            Always(b) => Always(!b),
            c => Not(Box::new(c)),
        },
        COr(a, b) => match (specialize_cond(ccst, a), specialize_cond(ccst, b)) {
            (Always(true), _) | (_, Always(true)) => Always(true),
            (Always(false), c) | (c, Always(false)) => c,
            (a, b) => COr(Box::new(a), Box::new(b)),
        },
        CAnd(a, b) => match (specialize_cond(ccst, a), specialize_cond(ccst, b)) {
            (Always(false), _) | (_, Always(false)) => Always(false),
            (Always(true), c) | (c, Always(true)) => c,
            (a, b) => CAnd(Box::new(a), Box::new(b)),
        },
        c => c.clone(),
    }
}

fn specialize_outcome<P: Clone>(ccst: &CharacterConstant, co: &ChapterOutcome<P>) -> ChapterOutcome<P> {
    let sub = |o: &ChapterOutcome<P>| Box::new(specialize_outcome(ccst, o));
    match co {
        ChapterOutcome::Fight(fd, o) => ChapterOutcome::Fight(fd.clone(), sub(o)),
        ChapterOutcome::OneRound(fd, a, b, c) => ChapterOutcome::OneRound(fd.clone(), sub(a), sub(b), sub(c)),
        ChapterOutcome::Randomly(lst) => {
            ChapterOutcome::Randomly(lst.iter().map(|(p, o)| (p.clone(), specialize_outcome(ccst, o))).collect())
        }
        // the first condition that holds is the one that is used
        ChapterOutcome::Conditionally(lst) => {
            let mut out = Vec::new();
            for (c, o) in lst {
                let c = specialize_cond(ccst, c);
                let stop = c == BoolCond::Always(true);
                if c != BoolCond::Always(false) {
                    out.push((c, specialize_outcome(ccst, o)));
                }
                if stop {
                    break;
                }
            }
            ChapterOutcome::Conditionally(out)
        }
        ChapterOutcome::Simple(sos, o) => ChapterOutcome::Simple(sos.clone(), sub(o)),
        ChapterOutcome::LoseItemFrom(s, n, o) => ChapterOutcome::LoseItemFrom(*s, *n, sub(o)),
        ChapterOutcome::Round(fd, o, nxt) => ChapterOutcome::Round(fd.clone(), sub(o), nxt.clone()),
        ChapterOutcome::Goto(_)
        | ChapterOutcome::EnterFight(_, _)
        | ChapterOutcome::GameLost
        | ChapterOutcome::GameWon => co.clone(),
    }
}

/// The decision, without what this character can never do. This is only fit for analysing the book, as a choice that
/// can't be made is not removed from the list, but replaced by an empty decision.
fn specialize_decision<P: Clone>(ccst: &CharacterConstant, dec: &Decision<P>) -> Decision<P> {
    let sub = |d: &Decision<P>| Box::new(specialize_decision(ccst, d));
    match dec {
        Decision::Decisions(lst) => {
            Decision::Decisions(lst.iter().map(|(n, d)| (n.clone(), specialize_decision(ccst, d))).collect())
        }
        Decision::RetrieveEquipment(d) => Decision::RetrieveEquipment(sub(d)),
        Decision::CanTake(i, q, d) => Decision::CanTake(*i, *q, sub(d)),
        Decision::Canbuy(i, p, d) => Decision::Canbuy(*i, *p, sub(d)),
        Decision::Cansell(i, p, d) => Decision::Cansell(*i, *p, sub(d)),
        Decision::Conditional(c, d) => match specialize_cond(ccst, c) {
            BoolCond::Always(false) => Decision::Decisions(Vec::new()),
            c => Decision::Conditional(c, sub(d)),
        },
        Decision::Special(s) => Decision::Special(*s),
        Decision::None(o) => Decision::None(specialize_outcome(ccst, o)),
        Decision::EvadeFight(r, cid, fd, o) => Decision::EvadeFight(*r, *cid, fd.clone(), specialize_outcome(ccst, o)),
        Decision::AfterCombat(d) => Decision::AfterCombat(sub(d)),
        Decision::RemoveItemFrom(s, n, d) => Decision::RemoveItemFrom(*s, *n, sub(d)),
    }
}

impl WeaponUse {
    /// The analysis of the book for a given character. The disciplines do not change during a book, so what is only
    /// available to other characters can be ignored.
    pub fn for_character<P: Clone>(ccst: &CharacterConstant, book: &HashMap<ChapterId, Chapter<P>>) -> Self {
        let specialized: HashMap<ChapterId, Chapter<P>> = book
            .iter()
            .map(|(cid, chapter)| {
                let pchoice = specialize_decision(ccst, &chapter.pchoice);
                (
                    *cid,
                    Chapter {
                        title: String::new(),
                        desc: String::new(),
                        pchoice,
                    },
                )
            })
            .collect();
        WeaponUse::new(ccst.bookid, &specialized)
    }

    pub fn new<P>(bookid: Book, book: &HashMap<ChapterId, Chapter<P>>) -> Self {
        let always = Reads::always(bookid);
        let mut reads: HashMap<ChapterId, Reads> = book
            .iter()
            .map(|(cid, chapter)| {
                let mut r = always;
                r.decision(&chapter.pchoice);
                (*cid, r)
            })
            .collect();
        let mut used: HashMap<ChapterId, WeaponSet> = book
            .iter()
            .map(|(cid, chapter)| {
                let mut s = WeaponSet::default();
                s.decision(&chapter.pchoice);
                (*cid, s)
            })
            .collect();
        let edges: Vec<(ChapterId, HashSet<ChapterId>)> = book
            .iter()
            .map(|(cid, chapter)| (*cid, get_destinations(&chapter.pchoice)))
            .collect();
        loop {
            let mut changed = false;
            for (cid, dsts) in &edges {
                let merged = dsts.iter().fold(used[cid], |acc, d| {
                    WeaponSet(acc.0 | used.get(d).copied().unwrap_or(WeaponSet::ALL).0)
                });
                if merged != used[cid] {
                    used.insert(*cid, merged);
                    changed = true;
                }
                let merged = dsts
                    .iter()
                    .fold(reads[cid], |acc, d| acc.merge(reads.get(d).copied().unwrap_or(Reads::ALL)));
                if merged != reads[cid] {
                    reads.insert(*cid, merged);
                    changed = true;
                }
            }
            if !changed {
                return WeaponUse {
                    weapons: used.into_iter().collect(),
                    reads: reads.into_iter().collect(),
                    gold: gold_needs(bookid, book).into_iter().collect(),
                };
            }
        }
    }

    /// Gives the same names to the weapons of equivalent states. When entering a chapter, the weapons that it can't tell
    /// apart can be exchanged without changing the score, so that only the lowest ones are used. This means that the
    /// weapon names of a state aren't always the ones that were picked up.
    pub fn canonical<PREV: StoredEquipment>(
        &self,
        ccst: &CharacterConstant,
        cid: ChapterId,
        cvar: &CharacterVariableG<PREV>,
    ) -> CharacterVariableG<PREV> {
        let used = self.weapons.get(&cid).copied().unwrap_or(WeaponSet::ALL);
        let plain: Vec<Weapon> = Weapon::VALUES
            .into_iter()
            .filter(|w| {
                !matches!(w, Weapon::Sommerswerd | Weapon::MagicSpear)
                    && !used.contains(*w)
                    && !ccst.discipline.contains(&Discipline::WeaponSkill(*w))
            })
            .collect();
        let mut cur = cvar.cequipment;
        let mut prev: Equipment = cvar.cprevequipment.into();
        let held = |e: &Equipment| -> Vec<Weapon> {
            plain
                .iter()
                .copied()
                .filter(|w| e.has_itemb(&Item::Weapon(*w)))
                .collect()
        };
        let (hcur, hprev) = (held(&cur), held(&prev));
        // weapons present in both equipments must stay so
        let mut names = plain.iter().copied();
        let mut rename = |e: &mut Equipment, w: &[Weapon]| {
            for x in w {
                e.del_item(&Item::Weapon(*x), 1);
            }
            w.iter()
                .map(|_| names.next().expect("as many names as weapons"))
                .collect::<Vec<_>>()
        };
        let in_both: Vec<Weapon> = hcur.iter().copied().filter(|w| hprev.contains(w)).collect();
        let only_cur: Vec<Weapon> = hcur.iter().copied().filter(|w| !hprev.contains(w)).collect();
        let only_prev: Vec<Weapon> = hprev.iter().copied().filter(|w| !hcur.contains(w)).collect();
        let shared = rename(&mut cur, &in_both);
        for x in &in_both {
            prev.del_item(&Item::Weapon(*x), 1);
        }
        for n in &shared {
            cur.add_item(&Item::Weapon(*n), 1);
            prev.add_item(&Item::Weapon(*n), 1);
        }
        for n in rename(&mut cur, &only_cur) {
            cur.add_item(&Item::Weapon(n), 1);
        }
        for n in rename(&mut prev, &only_prev) {
            prev.add_item(&Item::Weapon(n), 1);
        }
        let mut out = *cvar;
        // what can't be read anymore does not matter
        let reads = self.reads.get(&cid).copied().unwrap_or(Reads::ALL);
        let gold = self.gold.get(&cid).copied().unwrap_or(GoldNeeds::ALL);
        for e in [&mut cur, &mut prev] {
            let extra = e.get_item_count(&Item::Gold).saturating_sub(gold);
            e.del_item(&Item::Gold, extra as i64);
            for n in 0..u16::BITS as u8 {
                if reads.special & (1 << n) == 0 && n < GEN_ITEMS {
                    e.del_item(&Item::GenSpecial(n), 1);
                }
                if !reads.backpack_space && reads.backpack & (1 << n) == 0 && n < GEN_ITEMS {
                    e.del_item(&Item::GenBackpack(n), 1);
                }
            }
        }
        out.flags = Flags(cvar.flags.0 & reads.flags);
        out.cequipment = cur;
        out.cprevequipment = PREV::from(prev);
        out
    }

    /// The weapons that matter once this decision is about to be made. An unknown chapter could do anything, so all
    /// the weapons matter.
    pub fn after<P>(&self, dec: &Decision<P>) -> WeaponSet {
        let mut s = WeaponSet::default();
        s.decision(dec);
        get_destinations(dec).iter().fold(s, |acc, d| {
            WeaponSet(acc.0 | self.weapons.get(d).copied().unwrap_or(WeaponSet::ALL).0)
        })
    }
}

pub fn order_chapters<P>(book: &HashMap<ChapterId, Chapter<P>>) -> HashMap<ChapterId, u32> {
    let mut edgemap: HashMap<ChapterId, HashSet<ChapterId>> = HashMap::new();
    for (cid, chapter) in book {
        for dst in get_destinations(&chapter.pchoice) {
            if dst != *cid {
                let entry = edgemap.entry(dst).or_default();
                entry.insert(*cid);
            }
        }
    }
    let startedges: Vec<ChapterId> = book.keys().filter(|k| !edgemap.contains_key(k)).copied().collect();
    fn go(edges: Vec<ChapterId>, acc: Vec<ChapterId>, emap: HashMap<ChapterId, HashSet<ChapterId>>) -> Vec<ChapterId> {
        let mut redges = edges;
        match redges.pop() {
            None => acc,
            Some(x) => {
                let mut emap2 = HashMap::new();
                let mut macc = acc;
                for (k, mut s) in emap.into_iter() {
                    s.remove(&x);
                    if s.is_empty() {
                        macc.push(k);
                    } else {
                        emap2.insert(k, s);
                    }
                }
                go(redges, macc, emap2)
            }
        }
    }
    let mut ordered_list = go(startedges, Vec::new(), edgemap);
    ordered_list.reverse();
    ordered_list
        .into_iter()
        .enumerate()
        .map(|(i, c)| (c, i as u32))
        .collect()
}
