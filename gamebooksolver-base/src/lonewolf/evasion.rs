//! Most fights can be evaded, usually at any time after some number of rounds. The decision depends on how the fight is
//! going, so these fights are played one round at a time. As the state of a character can't hold the state of the fight,
//! this is done by adding one chapter for each state of the fight to the book: the remaining endurance of the opponent,
//! and the number of rounds played as long as it matters. Each round changes the state of the character or of the fight,
//! so that this does not create cycles.

use crate::lonewolf::chapter::{Chapter, ChapterId, ChapterOutcome, Decision, Endurance, FightDetails, FightModifier};
use crate::lonewolf::combat::decrement_timed;
use crate::lonewolf::mini::Book;
use crate::solver::rational::Rational;
use std::collections::HashMap;

// The chapters added for a fight behave like the one holding that fight, with regards to `is_story_chapter`.
const FIRST_STORY_ROUND: u16 = 2000;
const FIRST_OTHER_ROUND: u16 = 30000;

/// Whether leaving this chapter has the effects of the passing of time. It is not the case for the epilogue and for the
/// chapters that were added to the books.
pub fn is_story_chapter(bookid: Book, cid: ChapterId) -> bool {
    let max_chapter = if bookid == Book::Book05 { 400 } else { 350 };
    cid.0 < max_chapter || (FIRST_STORY_ROUND..FIRST_OTHER_ROUND).contains(&cid.0)
}

struct NewChapters<P> {
    next_story: u16,
    next_other: u16,
    chapters: Vec<(ChapterId, Chapter<P>)>,
}

fn timer<P>(m: &FightModifier<P>) -> u8 {
    match m {
        FightModifier::Timed(n, _) => *n,
        _ => 0,
    }
}

fn round_chapter<P>(pchoice: Decision<P>) -> Chapter<P> {
    Chapter {
        title: "Fight".to_string(),
        desc: String::new(),
        pchoice,
    }
}

impl<P: Rational> NewChapters<P> {
    fn expand_fight(
        &mut self,
        story: bool,
        min_rounds: u8,
        evaded: ChapterId,
        fd: &FightDetails<P>,
        after: &ChapterOutcome<P>,
    ) -> ChapterId {
        let opponent = fd.endurance.0 as u16;
        let last_round = fd.fight_mod.iter().map(timer).chain([min_rounds]).max().unwrap_or(0) as u16;
        let count = (last_round + 1) * (opponent + 1);
        let next = if story {
            &mut self.next_story
        } else {
            &mut self.next_other
        };
        let first = *next;
        *next = next.checked_add(count).expect("too many fight chapters");
        assert!(!story || *next <= FIRST_OTHER_ROUND, "too many fight chapters");
        let id = |round: u16, ep: u16| ChapterId(first + round * (opponent + 1) + ep);

        let mut mods = fd.fight_mod.clone();
        for round in 0..=last_round {
            // indexed by the endurance of the opponent
            let next_rounds: Vec<ChapterId> = (0..=opponent).map(|ep| id((round + 1).min(last_round), ep)).collect();
            for ep in 1..=opponent {
                let details = FightDetails {
                    opponent: fd.opponent.clone(),
                    combat_skill: fd.combat_skill,
                    endurance: Endurance(ep as i8),
                    fight_mod: mods.clone(),
                };
                let fight_on = Decision::None(ChapterOutcome::Round(
                    details.clone(),
                    Box::new(after.clone()),
                    next_rounds.clone(),
                ));
                let pchoice = if round >= min_rounds as u16 {
                    let mut evading = details;
                    evading.fight_mod.push(FightModifier::Evaded(evaded));
                    evading.fight_mod.push(FightModifier::NoPotion);
                    Decision::Decisions(vec![
                        ("Keep fighting".to_string(), fight_on),
                        (
                            "Evade".to_string(),
                            Decision::None(ChapterOutcome::Fight(evading, Box::new(after.clone()))),
                        ),
                    ])
                } else {
                    fight_on
                };
                self.chapters.push((id(round, ep), round_chapter(pchoice)));
            }
            mods = mods.into_iter().filter_map(decrement_timed).collect();
        }
        id(0, opponent)
    }

    fn expand(&mut self, story: bool, dec: Decision<P>) -> Decision<P> {
        use Decision::*;
        let mut sub = |d: Box<Decision<P>>| Box::new(self.expand(story, *d));
        match dec {
            EvadeFight(rounds, evaded, fd, after) => {
                let first = self.expand_fight(story, rounds.0, evaded, &fd, &after);
                None(ChapterOutcome::EnterFight(fd, first))
            }
            Decisions(lst) => Decisions(lst.into_iter().map(|(t, d)| (t, self.expand(story, d))).collect()),
            RetrieveEquipment(d) => RetrieveEquipment(sub(d)),
            CanTake(i, q, d) => CanTake(i, q, sub(d)),
            Canbuy(i, p, d) => Canbuy(i, p, sub(d)),
            Cansell(i, p, d) => Cansell(i, p, sub(d)),
            Conditional(c, d) => Conditional(c, sub(d)),
            AfterCombat(d) => AfterCombat(sub(d)),
            RemoveItemFrom(sl, n, d) => RemoveItemFrom(sl, n, sub(d)),
            Special(s) => Special(s),
            None(o) => None(o),
        }
    }
}

/// Must be called on the book before using it, as `step` does not know about `Decision::EvadeFight`.
pub fn expand_evasions<P: Rational>(bookid: Book, book: &mut HashMap<ChapterId, Chapter<P>>) {
    let mut new = NewChapters {
        next_story: FIRST_STORY_ROUND,
        next_other: FIRST_OTHER_ROUND,
        chapters: Vec::new(),
    };
    let mut cids: Vec<ChapterId> = book.keys().copied().collect();
    // so that a chapter keeps the same number from one run to the other
    cids.sort();
    for cid in cids {
        let story = is_story_chapter(bookid, cid);
        let chapter = book.get_mut(&cid).expect("known chapter");
        let dec = std::mem::replace(&mut chapter.pchoice, Decision::None(ChapterOutcome::GameLost));
        chapter.pchoice = new.expand(story, dec);
    }
    book.extend(new.chapters);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lonewolf::chapter::{CombatSkill, Rounds};
    use crate::lonewolf::combat::{CombatInfo, Escaped, Memoz, RoundEnd};
    use crate::lonewolf::data::get_destinations;
    use crate::lonewolf::mini::{CharacterConstant, CharacterVariable, Discipline, NextStep};
    use crate::lonewolf::solve::solve_lws;
    use crate::solver::base::{Outcome, Proba, SolNode, optimize_outcome};
    use num_rational::BigRational;

    fn ccst(discipline: Vec<Discipline>) -> CharacterConstant {
        CharacterConstant {
            bookid: Book::Book02,
            combat_skill: 15,
            discipline,
            maxendurance: 20,
        }
    }

    fn details(endurance: i8, fight_mod: Vec<FightModifier<BigRational>>) -> FightDetails<BigRational> {
        FightDetails {
            opponent: "x".to_string(),
            combat_skill: CombatSkill(15),
            endurance: Endurance(endurance),
            fight_mod,
        }
    }

    #[test]
    fn all_destinations_exist() {
        let chapter = |pchoice| Chapter {
            title: String::new(),
            desc: String::new(),
            pchoice,
        };
        let evade = Decision::EvadeFight(
            Rounds(2),
            ChapterId(2),
            details(3, vec![]),
            ChapterOutcome::Goto(ChapterId(2)),
        );
        let mut book: HashMap<ChapterId, Chapter<BigRational>> = [
            (ChapterId(1), chapter(evade)),
            (ChapterId(2), chapter(Decision::None(ChapterOutcome::GameWon))),
        ]
        .into_iter()
        .collect();
        expand_evasions(Book::Book02, &mut book);
        assert!(book.len() > 2);
        for chapter in book.values() {
            for d in get_destinations(&chapter.pchoice) {
                assert!(book.contains_key(&d), "{d:?}");
            }
        }
    }

    fn by_rounds(cinfo: &CombatInfo<BigRational>) -> Outcome<BigRational, Escaped<Endurance>> {
        let out = cinfo.one_round().into_iter().flat_map(|r| match r.v {
            RoundEnd::Over(e) => vec![Proba { p: r.p, v: e }],
            RoundEnd::Next(lw, op) => {
                let mut n = cinfo.clone();
                n.lwendurance = lw;
                n.opendurance = op;
                n.modifiers = n.modifiers.into_iter().filter_map(decrement_timed).collect();
                by_rounds(&n)
                    .into_iter()
                    .map(|r2| Proba {
                        p: r2.p.mul(&r.p),
                        v: r2.v,
                    })
                    .collect()
            }
        });
        optimize_outcome(out.collect())
    }

    #[test]
    fn rounds_are_the_same_as_the_whole_fight() {
        use FightModifier::*;
        let timed = |n, m| Timed(n, Box::new(m));
        let cases: Vec<Vec<FightModifier<BigRational>>> = vec![
            vec![],
            vec![BareHanded],
            vec![EnemyMindblast, Undead, MindblastImmune],
            vec![OnDamage(ChapterId(66))],
            vec![timed(4, OnNotYetWon(ChapterId(203)))],
            vec![timed(2, PlayerInvulnerable)],
            vec![timed(2, CombatBonus(CombatSkill(-2)))],
            vec![DoubleDamage],
            vec![OnLose(ChapterId(9))],
        ];
        for fight_mod in cases {
            for disc in [vec![], vec![Discipline::MindShield]] {
                let c = ccst(disc);
                let cinfo = CombatInfo::make(&c, &CharacterVariable::new(12), &details(9, fight_mod.clone()));
                let sorted = |mut o: Outcome<BigRational, Escaped<Endurance>>| {
                    o.sort_by(|a, b| a.v.cmp(&b.v));
                    o
                };
                let whole = sorted(cinfo.fight(&mut Memoz::default()));
                assert_eq!(sorted(by_rounds(&cinfo)), whole, "{fight_mod:?}");
            }
        }
    }

    fn score(pchoice: Decision<BigRational>) -> BigRational {
        let chapter = |pchoice| Chapter {
            title: String::new(),
            desc: String::new(),
            pchoice,
        };
        let book = [
            (ChapterId(1), chapter(pchoice)),
            // only those that escaped in good health win
            (
                ChapterId(2),
                chapter(Decision::None(ChapterOutcome::Conditionally(vec![
                    (
                        crate::lonewolf::chapter::BoolCond::HasEndurance(Endurance(9)),
                        ChapterOutcome::GameWon,
                    ),
                    (
                        crate::lonewolf::chapter::BoolCond::Always(true),
                        ChapterOutcome::GameLost,
                    ),
                ]))),
            ),
            (ChapterId(3), chapter(Decision::None(ChapterOutcome::GameWon))),
        ];
        let cvar = CharacterVariable::new(12);
        let sol = solve_lws::<BigRational, _, _>(
            |_, _| BigRational::from_i64(1, 1),
            &[],
            &book,
            &ccst(vec![]),
            &cvar,
            1,
            false,
        );
        match sol.get(&NextStep::NewChapter(1, cvar)) {
            Some(SolNode::Chosen(p, _)) | Some(SolNode::Single(p, _)) | Some(SolNode::Win(p)) => p.clone(),
            None => panic!("not solved"),
        }
    }

    fn evade(rounds: u8, fight_mod: Vec<FightModifier<BigRational>>) -> Decision<BigRational> {
        Decision::EvadeFight(
            Rounds(rounds),
            ChapterId(2),
            details(30, fight_mod),
            ChapterOutcome::Goto(ChapterId(3)),
        )
    }

    #[test]
    fn evading_is_at_least_as_good_as_fixed_rounds() {
        let adaptive = score(evade(0, vec![]));
        for n in 0..4 {
            let timed = FightModifier::Timed(n, Box::new(FightModifier::Evaded(ChapterId(2))));
            let fixed = Decision::None(ChapterOutcome::Fight(
                details(
                    30,
                    if n == 0 {
                        vec![FightModifier::Evaded(ChapterId(2))]
                    } else {
                        vec![timed]
                    },
                ),
                Box::new(ChapterOutcome::Goto(ChapterId(3))),
            ));
            assert!(score(fixed) <= adaptive);
        }
    }

    #[test]
    fn can_not_evade_before_the_given_round() {
        // being forced to fight for more rounds can only be worse
        assert!(score(evade(4, vec![])) < score(evade(0, vec![])));
    }

    #[test]
    fn book_chapter_kinds() {
        assert!(is_story_chapter(Book::Book02, ChapterId(349)));
        assert!(!is_story_chapter(Book::Book02, ChapterId(351)));
        assert!(is_story_chapter(Book::Book05, ChapterId(399)));
        assert!(!is_story_chapter(Book::Book05, ChapterId(401)));
        assert!(is_story_chapter(Book::Book05, ChapterId(FIRST_STORY_ROUND)));
        assert!(!is_story_chapter(Book::Book05, ChapterId(FIRST_OTHER_ROUND)));
    }
}
