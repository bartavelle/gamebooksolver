use crate::lonewolf::chapter::{Chapter, ChapterId};
use crate::lonewolf::choices::flatten_decision;
use crate::lonewolf::combat::Memoz;
use crate::lonewolf::data::{WeaponUse, order_chapters};
use crate::lonewolf::elide::elide_forced_chapters;
use crate::lonewolf::evasion::{expand_evasions, is_round_chapter};
use crate::lonewolf::mini::{CharacterConstant, Equipment, Flag, NextStep, StoredEquipment};
use crate::lonewolf::mini::{CharacterVariableG, Flags, max_hp};
use crate::lonewolf::rules::update;
use crate::solver::base::{Choice, FastMap, Outcome, Proba, Solution, optimize_outcome};
use crate::solver::rational::Rational;
use crate::solver::{CompactSolved, Retention, solve, solve_compact};
use std::collections::HashMap;
use std::hash::BuildHasher;

pub fn step<P: Rational, PREV: StoredEquipment, S1: BuildHasher, S2: BuildHasher>(
    memo: &mut Memoz<P>,
    order: &HashMap<ChapterId, u32, S1>,
    wuse: &WeaponUse,
    chapters: &HashMap<ChapterId, Chapter<P>, S2>,
    ccst: &CharacterConstant,
    ns: &NextStep<PREV>,
) -> Vec<Choice<P, String, NextStep<PREV>>> {
    match ns {
        NextStep::NewChapter(cid, cvar) => {
            if cvar.curendurance > max_hp(ccst, cvar) {
                panic!("Too many HPs!!");
            }
            let chapter = chapters.get(&ChapterId(*cid)).expect("could not find chapter");
            let mut nv = cvar.clone();
            nv.flags.unset(Flag::HadCombat);
            let mut out: Vec<Choice<P, String, NextStep<PREV>>> = flatten_decision(wuse, ccst, cvar, &chapter.pchoice)
                .into_iter()
                .map(|(d, outcome)| Choice {
                    desc: d.join(" "),
                    res: optimize_outcome(
                        update(memo, ccst, &nv, ChapterId(*cid), &outcome)
                            .into_iter()
                            .map(|r| Proba {
                                p: r.p,
                                v: match r.v {
                                    NextStep::NewChapter(nid, c) => {
                                        NextStep::NewChapter(nid, wuse.canonical(ccst, ChapterId(nid), &c))
                                    }
                                    other => other,
                                },
                            })
                            .collect(),
                    ),
                })
                .collect();
            let scorei = |ns: &NextStep<PREV>| -> u32 {
                match ns {
                    NextStep::HasLost(_) => 0,
                    NextStep::HasWon(_) => 0,
                    NextStep::NewChapter(cid, _) => *order.get(&ChapterId(*cid)).unwrap_or(&0),
                }
            };
            let scorer = |outcome: &Outcome<P, NextStep<PREV>>| outcome.iter().map(|r| scorei(&r.v)).max().unwrap_or(0);
            if out.len() > 1 {
                out.sort_by_cached_key(|c| scorer(&c.res));
            }
            if out.is_empty() {
                panic!("Empty! {:?}", ns);
            } else {
                out
            }
        }
        NextStep::HasWon(c) => vec![Choice {
            desc: "won!".into(),
            res: vec![Proba::certain(NextStep::HasWon(c.clone()))],
        }],
        NextStep::HasLost(c) => vec![Choice {
            desc: "lost!".into(),
            res: vec![Proba::certain(NextStep::HasLost(*c))],
        }],
    }
}

fn get_score<P: Rational, F, PREV: StoredEquipment>(scorer: F, tgt: &[ChapterId], ns: &NextStep<PREV>) -> Option<P>
where
    F: Fn(Equipment, Flags) -> P,
{
    let score_for = |cvar: &CharacterVariableG<PREV>| scorer(cvar.cequipment, cvar.flags);
    match ns {
        NextStep::HasWon(cvar) => Some(score_for(cvar)),
        NextStep::HasLost(_) => Some(P::from_i64(0, 1)),
        NextStep::NewChapter(x, cvar) => {
            if tgt.contains(&ChapterId(*x)) {
                Some(score_for(cvar))
            } else {
                None
            }
        }
    }
}

/// The scorer must only depend on what `get_boundary` lists for this book, as the rest of the state can be changed, see
/// `WeaponUse`.
pub fn solve_lws<P: Rational + Eq + std::hash::Hash, F, PREV: StoredEquipment>(
    scorer: F,
    tgt: &[ChapterId],
    book: &[(ChapterId, Chapter<P>)],
    ccst: &CharacterConstant,
    cvar: &CharacterVariableG<PREV>,
    startat: u16,
    verbose: bool,
) -> Solution<P, NextStep<PREV>>
where
    F: Fn(Equipment, Flags) -> P,
{
    let mut mbook: HashMap<ChapterId, Chapter<P>> = book.iter().cloned().collect();
    expand_evasions(ccst.bookid, &mut mbook);
    let order: FastMap<ChapterId, u32> = order_chapters(&mbook).into_iter().collect();
    let wuse = WeaponUse::for_character(ccst, &mbook);
    let mbook: FastMap<ChapterId, Chapter<P>> = mbook.into_iter().collect();
    let mut memo = Memoz::default();
    let o = solve::<NextStep<PREV>, _, _, _, _, _>(
        &mut |ns| step(&mut memo, &order, &wuse, &mbook, ccst, ns),
        &|ns| get_score(&scorer, tgt, ns),
        &NextStep::NewChapter(startat, cvar.clone()),
        &|c| {
            if verbose {
                eprintln!("State cache efficiency {:.2}%", c.efficiency() * 100.0)
            }
        },
    );
    if verbose {
        eprintln!(
            "Cache efficiency: wholefight[{:.2}%] vanilla[{:.2}%] mindblasted[{:.2}%]",
            memo.whole_fight.efficiency() * 100.0,
            memo.fight_vanilla.efficiency() * 100.0,
            memo.fight_mindblasted.efficiency() * 100.0
        );
    }
    o
}

// The states of a fight are mostly useful while the same equipment is being looked at. Keeping more of them saves little
// time, and they would use more memory than all the other states.
const MAX_ROUND_STATES: usize = 1_000_000;

/// Same as `solve_lws`, keeping only the score and the index of the best choice of the states that take longer to solve
/// again, see `Retention`, which uses a lot less memory.
/// The scorer must only depend on what `get_boundary` lists for this book, as the rest of the state can be changed, see
/// `WeaponUse`.
pub fn solve_lws_compact<P: Rational + Eq + std::hash::Hash, F, PREV: StoredEquipment>(
    scorer: F,
    tgt: &[ChapterId],
    book: &[(ChapterId, Chapter<P>)],
    ccst: &CharacterConstant,
    cvar: &CharacterVariableG<PREV>,
    startat: u16,
    verbose: bool,
) -> CompactSolved<P, NextStep<PREV>>
where
    F: Fn(Equipment, Flags) -> P,
{
    let mut mbook: HashMap<ChapterId, Chapter<P>> = book.iter().cloned().collect();
    expand_evasions(ccst.bookid, &mut mbook);
    elide_forced_chapters(&mut mbook, tgt);
    let order: FastMap<ChapterId, u32> = order_chapters(&mbook).into_iter().collect();
    let wuse = WeaponUse::for_character(ccst, &mbook);
    let mbook: FastMap<ChapterId, Chapter<P>> = mbook.into_iter().collect();
    let mut memo = Memoz::default();
    let o = solve_compact::<NextStep<PREV>, _, _, _, _, _>(
        &mut |ns| step(&mut memo, &order, &wuse, &mbook, ccst, ns),
        &|ns| get_score(&scorer, tgt, ns),
        &NextStep::NewChapter(startat, cvar.clone()),
        &|c| {
            if verbose {
                eprintln!("State cache efficiency {:.2}%", c.efficiency() * 100.0)
            }
        },
        Retention::Reduced {
            transient: &|ns: &NextStep<PREV>| ns.chapter().is_some_and(is_round_chapter),
            max_transient: MAX_ROUND_STATES,
        },
    );
    if verbose {
        eprintln!(
            "Cache efficiency: wholefight[{:.2}%] vanilla[{:.2}%] mindblasted[{:.2}%]",
            memo.whole_fight.efficiency() * 100.0,
            memo.fight_vanilla.efficiency() * 100.0,
            memo.fight_mindblasted.efficiency() * 100.0
        );
    }
    o
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lonewolf::chapter::{BoolCond, ChapterOutcome, Decision};
    use crate::lonewolf::mini::{Book, CharacterVariable, Item};
    use crate::solver::base::SolNode;
    use num_rational::BigRational;

    #[test]
    fn passing_through_chapters_gives_the_same_scores() {
        use crate::lonewolf::chapter::{Endurance, SimpleOutcome};
        use crate::lonewolf::mini::{CharacterConstant, Discipline, Flag, NoPrevEq};
        use ChapterOutcome::*;
        let half = || BigRational::from_i64(1, 2);
        let goto = |c: u16| Box::new(Goto(ChapterId(c)));
        let chapter = |pchoice| Chapter {
            title: String::new(),
            desc: String::new(),
            pchoice,
        };
        // the score only depends on the endurance when arriving in chapter 4, after going through 2 and 3
        for needed in 1..30 {
            let book = [
                (
                    ChapterId(1),
                    chapter(Decision::Decisions(vec![
                        ("a".to_string(), Decision::None(Goto(ChapterId(2)))),
                        (
                            "b".to_string(),
                            Decision::None(Simple(vec![SimpleOutcome::DamagePlayer(Endurance(1))], goto(3))),
                        ),
                    ])),
                ),
                (
                    ChapterId(2),
                    chapter(Decision::None(Simple(vec![SimpleOutcome::DamagePlayer(Endurance(4))], goto(3)))),
                ),
                (ChapterId(3), chapter(Decision::None(Goto(ChapterId(4))))),
                (
                    ChapterId(4),
                    chapter(Decision::None(Conditionally(vec![
                        (BoolCond::HasEndurance(Endurance(needed)), Goto(ChapterId(5))),
                        (
                            BoolCond::Always(true),
                            Randomly(vec![(half(), Goto(ChapterId(5))), (half(), Goto(ChapterId(6)))]),
                        ),
                    ]))),
                ),
                (ChapterId(5), chapter(Decision::None(GameWon))),
                (ChapterId(6), chapter(Decision::None(GameLost))),
            ];
            for discipline in [vec![], vec![Discipline::Healing]] {
                for flags in [vec![], vec![Flag::Poisonned2], vec![Flag::HadCombat]] {
                    let ccst = CharacterConstant {
                        bookid: Book::Book01,
                        combat_skill: 15,
                        discipline: discipline.clone(),
                        maxendurance: 20,
                    };
                    let mut c = CharacterVariableG::<NoPrevEq>::new(10);
                    flags.iter().for_each(|f| c.set_flag(*f));
                    let win = |_, _| BigRational::from_i64(1, 1);
                    let full = solve_lws(win, &[], &book, &ccst, &c, 1, false);
                    let compact = solve_lws_compact(win, &[], &book, &ccst, &c, 1, false);
                    let start = NextStep::NewChapter(1, c);
                    let expected = match &full[&start] {
                        SolNode::Single(s, _) | SolNode::Win(s) | SolNode::Chosen(s, _) => s,
                    };
                    assert_eq!(&compact.get(&start).unwrap().score, expected, "{needed} {discipline:?} {flags:?}");
                }
            }
        }
    }

    #[test]
    fn the_target_chapters_are_not_passed_through() {
        use crate::lonewolf::chapter::{Endurance, SimpleOutcome};
        use crate::lonewolf::mini::{CharacterConstant, NoPrevEq};
        use ChapterOutcome::*;
        let chapter = |pchoice| Chapter {
            title: String::new(),
            desc: String::new(),
            pchoice,
        };
        // the gold is only lost after the target, and it is the score
        let book = [
            (ChapterId(1), chapter(Decision::None(Goto(ChapterId(2))))),
            (
                ChapterId(2),
                chapter(Decision::None(Simple(
                    vec![SimpleOutcome::LoseItem(Item::Gold, 99), SimpleOutcome::DamagePlayer(Endurance(1))],
                    Box::new(GameWon),
                ))),
            ),
        ];
        let ccst = CharacterConstant {
            bookid: Book::Book01,
            combat_skill: 15,
            discipline: vec![],
            maxendurance: 20,
        };
        let mut c = CharacterVariableG::<NoPrevEq>::new(10);
        c.add_item(&Item::Gold, 7);
        let score = |e: crate::lonewolf::mini::Equipment, _| BigRational::from_i64(e.get_item_count(&Item::Gold) as i64, 10);
        let got = solve_lws_compact(score, &[ChapterId(2)], &book, &ccst, &c, 1, false);
        assert_eq!(got.get(&NextStep::NewChapter(1, c)).unwrap().score, BigRational::from_i64(7, 10));
    }

    #[test]
    fn dummy_book() {
        let book = &[
            (
                ChapterId(1),
                Chapter {
                    title: "1".to_string(),
                    desc: "1".to_string(),
                    pchoice: Decision::CanTake(
                        Item::GenBackpack(1),
                        1,
                        Box::new(Decision::None(ChapterOutcome::Goto(ChapterId(2)))),
                    ),
                },
            ),
            (
                ChapterId(2),
                Chapter {
                    title: "1".to_string(),
                    desc: "1".to_string(),
                    pchoice: Decision::None(ChapterOutcome::Conditionally(vec![
                        (BoolCond::HasItem(Item::GenBackpack(1), 1), ChapterOutcome::GameWon),
                        (BoolCond::Always(true), ChapterOutcome::GameLost),
                    ])),
                },
            ),
        ];
        let mut cvar = CharacterVariable::new(20);
        cvar.add_item(&Item::Backpack, 1);
        let sol = solve_lws::<BigRational, _, _>(
            |_, _| BigRational::from_i64(1, 1),
            &[ChapterId(3)],
            book,
            &CharacterConstant {
                bookid: Book::Book01,
                combat_skill: 10,
                maxendurance: 20,
                discipline: Vec::new(),
            },
            &cvar,
            1,
            false,
        );

        let mut nvar = cvar.clone();
        nvar.cequipment.add_item(&Item::GenBackpack(1), 1);

        let c1 = sol.get(&NextStep::NewChapter(1, cvar)).unwrap();

        assert_eq!(
            c1,
            &SolNode::Single(BigRational::from_i64(1, 1), NextStep::NewChapter(2, nvar))
        )
    }
}
