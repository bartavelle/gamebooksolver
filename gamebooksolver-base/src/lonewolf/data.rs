use crate::lonewolf::chapter::{
    Chapter, ChapterId, ChapterOutcome, CombatSkill, Decision, Endurance, FightModifier,
    SpecialChapter,
};
use crate::lonewolf::mini::{Book, CVarState, Discipline, Equipment, NextStep, SolutionDump};
use crate::solver::rational::Rational;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Multistat<P> {
    pub mbook: Book,
    pub msdisciplines: Vec<Discipline>,
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
    pub fn from_soldump<PREV: Into<Equipment> + From<Equipment> + Ord>(
        dump: &SolutionDump<P, PREV>,
    ) -> Option<Self> {
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
            msdisciplines: dump.soldesc.ccst.discipline.clone(),
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
            ChapterOutcome::Conditionally(lst) => lst
                .iter()
                .flat_map(|(_, o)| get_destinations_o(o))
                .collect(),
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
            ChapterOutcome::Randomly(lst) => lst
                .iter()
                .flat_map(|(_, o)| get_destinations_o(o))
                .collect(),
            ChapterOutcome::Simple(_, nxt) => get_destinations_o(nxt),
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
        Decision::LoseItemFrom(_, _, nxt) => get_destinations(nxt),
        Decision::None(co) => get_destinations_o(co),
        Decision::RemoveItemFrom(_, _, nxt) => get_destinations(nxt),
        Decision::RetrieveEquipment(nxt) => get_destinations(nxt),
        Decision::Special(s) => match s {
            SpecialChapter::B05S127 => [159, 93].iter().copied().map(ChapterId).collect(),
            SpecialChapter::B05S357 => [293, 207, 224].iter().copied().map(ChapterId).collect(),
            SpecialChapter::Cartwheel => [169, 186].iter().copied().map(ChapterId).collect(),
            SpecialChapter::Portholes => [197].iter().copied().map(ChapterId).collect(),
        },
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
    let startedges: Vec<ChapterId> = book
        .keys()
        .filter(|k| !edgemap.contains_key(k))
        .copied()
        .collect();
    fn go(
        edges: Vec<ChapterId>,
        acc: Vec<ChapterId>,
        emap: HashMap<ChapterId, HashSet<ChapterId>>,
    ) -> Vec<ChapterId> {
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
