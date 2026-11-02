//! Many chapters only lead to a single other one, without any choice or randomness. A state in such a chapter has the
//! score of the state that follows it, so this is done right away by the chapters that lead there: these states do not
//! have to be found, kept, or solved again. Only for solving, as this skips what the player should be shown.

use crate::lonewolf::chapter::{Chapter, ChapterId, ChapterOutcome, Decision};
use crate::solver::rational::Rational;
use std::collections::{HashMap, HashSet};

/// Whether there is only a single possible next state, and nothing else to do than going there.
fn is_forced<P>(o: &ChapterOutcome<P>) -> bool {
    match o {
        ChapterOutcome::Goto(_) | ChapterOutcome::GameLost | ChapterOutcome::GameWon => true,
        ChapterOutcome::Simple(_, nxt) => is_forced(nxt),
        ChapterOutcome::Conditionally(lst) => lst.iter().all(|(_, o)| is_forced(o)),
        _ => false,
    }
}

struct Elider<'t, P> {
    book: &'t HashMap<ChapterId, Chapter<P>>,
    resolved: HashMap<ChapterId, ChapterOutcome<P>>,
    // a loop is a problem for the solver to report
    visiting: HashSet<ChapterId>,
}

impl<P: Rational> Elider<'_, P> {
    fn forced_chapter(&mut self, cid: ChapterId) -> Option<ChapterOutcome<P>> {
        if let Some(o) = self.resolved.get(&cid) {
            return Some(o.clone());
        }
        let book = self.book;
        let o = match &book.get(&cid)?.pchoice {
            Decision::None(o) if is_forced(o) => o,
            _ => return None,
        };
        if !self.visiting.insert(cid) {
            return None;
        }
        let out = self.outcome(o);
        self.visiting.remove(&cid);
        self.resolved.insert(cid, out.clone());
        Some(out)
    }

    fn outcome(&mut self, o: &ChapterOutcome<P>) -> ChapterOutcome<P> {
        use ChapterOutcome::*;
        match o {
            Goto(cid) => match self.forced_chapter(*cid) {
                Some(forced) => PassThrough(*cid, Box::new(forced)),
                None => Goto(*cid),
            },
            Fight(fd, nxt) => Fight(fd.clone(), Box::new(self.outcome(nxt))),
            OneRound(fd, a, b, c) => OneRound(
                fd.clone(),
                Box::new(self.outcome(a)),
                Box::new(self.outcome(b)),
                Box::new(self.outcome(c)),
            ),
            Randomly(lst) => Randomly(lst.iter().map(|(p, o)| (p.clone(), self.outcome(o))).collect()),
            Conditionally(lst) => Conditionally(lst.iter().map(|(c, o)| (c.clone(), self.outcome(o))).collect()),
            Simple(sos, nxt) => Simple(sos.clone(), Box::new(self.outcome(nxt))),
            LoseItemFrom(s, n, nxt) => LoseItemFrom(*s, *n, Box::new(self.outcome(nxt))),
            Round(fd, nxt, next_rounds) => Round(fd.clone(), Box::new(self.outcome(nxt)), next_rounds.clone()),
            PassThrough(cid, nxt) => PassThrough(*cid, Box::new(self.outcome(nxt))),
            GameLost | GameWon | EnterFight(_, _) => o.clone(),
        }
    }

    fn decision(&mut self, d: &Decision<P>) -> Decision<P> {
        use Decision::*;
        match d {
            Decisions(lst) => Decisions(lst.iter().map(|(n, d)| (n.clone(), self.decision(d))).collect()),
            RetrieveEquipment(d) => RetrieveEquipment(Box::new(self.decision(d))),
            CanTake(i, q, d) => CanTake(*i, *q, Box::new(self.decision(d))),
            Canbuy(i, p, d) => Canbuy(*i, *p, Box::new(self.decision(d))),
            Cansell(i, p, d) => Cansell(*i, *p, Box::new(self.decision(d))),
            Conditional(c, d) => Conditional(c.clone(), Box::new(self.decision(d))),
            Special(s) => Special(*s),
            None(o) => None(self.outcome(o)),
            EvadeFight(r, cid, fd, o) => EvadeFight(*r, *cid, fd.clone(), self.outcome(o)),
            AfterCombat(d) => AfterCombat(Box::new(self.decision(d))),
            RemoveItemFrom(s, n, d) => RemoveItemFrom(*s, *n, Box::new(self.decision(d))),
        }
    }
}

/// Replaces the arrivals in the chapters with a single next state by what happens in these chapters. All the chapters stay
/// in the book, as they can still be the first one, or be reached by the modifiers of a fight.
pub fn elide_forced_chapters<P: Rational>(book: &mut HashMap<ChapterId, Chapter<P>>) {
    let cids: Vec<ChapterId> = book.keys().copied().collect();
    let mut decisions = Vec::with_capacity(cids.len());
    {
        let mut elider = Elider {
            book,
            resolved: HashMap::new(),
            visiting: HashSet::new(),
        };
        for cid in cids {
            let pchoice = elider.decision(&elider.book[&cid].pchoice);
            decisions.push((cid, pchoice));
        }
    }
    for (cid, pchoice) in decisions {
        book.get_mut(&cid).expect("known chapter").pchoice = pchoice;
    }
}
