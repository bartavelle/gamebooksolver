use crate::lonewolf::chapter::{
    BoolCond, Chapter, ChapterId, ChapterOutcome, CombatSkill, Decision, Endurance, FightModifier, SimpleOutcome,
    SpecialChapter,
};
use crate::lonewolf::mini::{
    Book, CVarState, CharacterConstant, CharacterVariableG, Discipline, Equipment, Item, NextStep, SolutionDump,
    StoredEquipment, Weapon,
};
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
            SpecialChapter::B05S357 => [293, 207, 224].iter().copied().map(ChapterId).collect(),
            SpecialChapter::Cartwheel => [169, 136].iter().copied().map(ChapterId).collect(),
            SpecialChapter::Portholes => [197].iter().copied().map(ChapterId).collect(),
        },
    }
}

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
            ChapterOutcome::Fight(_, o) | ChapterOutcome::LoseItemFrom(_, _, o) => self.outcome(o),
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
            ChapterOutcome::Goto(_) | ChapterOutcome::GameLost | ChapterOutcome::GameWon => (),
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

/// For each chapter, the weapons the rest of the game can tell apart from the others, because they are named in that
/// chapter or in one that can be reached from it. Fights are not looked at, as they only depend on the weapon skill and
/// on the two magical weapons.
#[derive(Debug, Clone, Default)]
pub struct WeaponUse(HashMap<ChapterId, WeaponSet>);

impl WeaponUse {
    pub fn new<P>(book: &HashMap<ChapterId, Chapter<P>>) -> Self {
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
            }
            if !changed {
                return WeaponUse(used);
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
        let used = self.0.get(&cid).copied().unwrap_or(WeaponSet::ALL);
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
            WeaponSet(acc.0 | self.0.get(d).copied().unwrap_or(WeaponSet::ALL).0)
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
