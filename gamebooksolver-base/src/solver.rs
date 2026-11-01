pub mod base;
pub mod rational;

use base::*;
use rational::Rational;

use std::collections::HashMap;
use std::hash::Hash;

/// What is kept for a state while solving: its score and the index of the best choice. The outcomes can be computed again
/// from the choice, and keeping them all would use several times more memory than the rest of the solver.
#[derive(Debug, Clone, PartialEq)]
pub struct CompactNode<P> {
    pub score: P,
    pub choice: u16,
}

pub type CompactSolved<P, STT> = HashMap<STT, CompactNode<P>>;

// the values of the choice that do not stand for a choice
const SEARCHING: u16 = u16::MAX;
const WIN: u16 = u16::MAX - 1;

/// Finds the score and the best choice of all the states that can be reached.
pub fn solve_compact<
    STT: Eq + Hash + Clone + std::fmt::Debug + PartialOrd + std::fmt::Display,
    FC,
    FS,
    INSPECT,
    DESC: std::fmt::Debug,
    P: Rational,
>(
    get_choices: &mut FC,
    get_score: &FS,
    inistate: &STT,
    inspect: &INSPECT,
) -> CompactSolved<P, STT>
where
    FC: FnMut(&STT) -> Vec<Choice<P, DESC, STT>>,
    FS: Fn(&STT) -> Option<P>,
    INSPECT: Fn(&Cache<STT, CompactNode<P>>),
{
    let mut stt: Cache<STT, CompactNode<P>> = Cache::default();

    go(get_choices, get_score, &mut stt, inistate);
    inspect(&stt);

    stt.cache
}

/// Same as `solve_compact`, with the outcomes of the best choice of each state.
pub fn solve<
    STT: Eq + Hash + Clone + std::fmt::Debug + PartialOrd + std::fmt::Display,
    FC,
    FS,
    INSPECT,
    DESC: std::fmt::Debug,
    P: Rational,
>(
    get_choices: &mut FC,
    get_score: &FS,
    inistate: &STT,
    inspect: &INSPECT,
) -> Solution<P, STT>
where
    FC: FnMut(&STT) -> Vec<Choice<P, DESC, STT>>,
    FS: Fn(&STT) -> Option<P>,
    INSPECT: Fn(&Cache<STT, CompactNode<P>>),
{
    solve_compact(get_choices, get_score, inistate, inspect)
        .into_iter()
        .map(|(k, node)| {
            let sol = if node.choice == WIN {
                SolNode::Win(node.score)
            } else {
                let choice = get_choices(&k).swap_remove(node.choice as usize);
                SolNode::from_choices(node.score, choice.res)
            };
            (k, sol)
        })
        .collect()
}

fn go<
    STT: Eq + Hash + Clone + std::fmt::Debug + std::fmt::Display + PartialOrd,
    FC,
    FS,
    DESC: std::fmt::Debug,
    P: Rational,
>(
    get_choices: &mut FC,
    get_score: &FS,
    search_state: &mut Cache<STT, CompactNode<P>>,
    curstate: &STT,
) -> P
where
    FC: FnMut(&STT) -> Vec<Choice<P, DESC, STT>>,
    FS: Fn(&STT) -> Option<P>,
{
    if let Some(score) = get_score(curstate) {
        search_state.insert(
            curstate.clone(),
            CompactNode {
                score: score.clone(),
                choice: WIN,
            },
        );
        return score;
    }

    if let Some(ns) = search_state.get(curstate) {
        if ns.choice == SEARCHING {
            for (k, v) in &search_state.cache {
                if v.choice == SEARCHING {
                    println!(" * {k}")
                }
            }
            panic!("loop at {}", curstate)
        }
        return ns.score.clone();
    }

    search_state.insert(
        curstate.clone(),
        CompactNode {
            score: P::from_i64(0, 1),
            choice: SEARCHING,
        },
    );

    let choices = get_choices(curstate);
    if choices.is_empty() {
        panic!("Empty choice at state {}", curstate);
    }
    let mut best_choice: Option<(u16, SolNode<P, STT>)> = None;
    let mut best_score = P::from_i64(-1, 1);
    for (idx, choice) in choices.into_iter().enumerate() {
        let idx = u16::try_from(idx).expect("too many choices");
        let mut cur_score = P::from_i64(0, 1);
        for outcome in &choice.res {
            cur_score = cur_score.add(&go(get_choices, get_score, search_state, &outcome.v).mul(&outcome.p));
        }
        if cur_score > best_score {
            best_choice = Some((idx, SolNode::from_choices(cur_score.clone(), choice.res)));
            best_score = cur_score;
        } else if cur_score == best_score {
            let newchoice = SolNode::from_choices(cur_score.clone(), choice.res);
            if let Some((_, bc)) = &best_choice {
                if &newchoice > bc {
                    best_choice = Some((idx, newchoice));
                }
            } else {
                best_choice = Some((idx, newchoice));
            }
        }
    }

    match best_choice {
        None => panic!(
            "No score, but no choices for {}, choices: {:?}",
            curstate,
            get_choices(curstate)
        ),
        Some((choice, _)) => {
            search_state.insert(
                curstate.clone(),
                CompactNode {
                    score: best_score.clone(),
                    choice,
                },
            );
        }
    }

    best_score
}

#[cfg(test)]
mod test {
    use super::*;
    use num_rational::BigRational;

    type Choices = Vec<Choice<BigRational, u8, u8>>;

    // 0 is a choice between 1 and 2, that lead to the winning state 3 and the losing state 4 with different odds
    fn choices(s: &u8) -> Choices {
        let pr = |n, v| Proba {
            p: BigRational::from_i64(n, 4),
            v,
        };
        match s {
            0 => vec![
                Choice { desc: 0, res: vec![pr(4, 1)] },
                Choice { desc: 1, res: vec![pr(4, 2)] },
            ],
            1 => vec![Choice { desc: 0, res: vec![pr(1, 3), pr(3, 4)] }],
            2 => vec![
                Choice { desc: 0, res: vec![pr(4, 4)] },
                Choice { desc: 1, res: vec![pr(3, 3), pr(1, 4)] },
            ],
            _ => vec![],
        }
    }

    fn score(s: &u8) -> Option<BigRational> {
        match s {
            3 => Some(BigRational::from_i64(1, 1)),
            4 => Some(BigRational::from_i64(0, 1)),
            _ => None,
        }
    }

    #[test]
    fn best_choice_is_kept() {
        let compact = solve_compact(&mut choices, &score, &0, &|_| ());
        assert_eq!(compact[&0].score, BigRational::from_i64(3, 4));
        assert_eq!(compact[&0].choice, 1);
        assert_eq!(compact[&2].choice, 1);
        assert_eq!(compact[&3].choice, WIN);
    }

    #[test]
    fn outcomes_are_computed_again() {
        let sol = solve(&mut choices, &score, &0, &|_| ());
        assert_eq!(sol[&0], SolNode::Single(BigRational::from_i64(3, 4), 2));
        assert_eq!(sol[&3], SolNode::Win(BigRational::from_i64(1, 1)));
        match &sol[&2] {
            SolNode::Chosen(p, o) => {
                assert_eq!(*p, BigRational::from_i64(3, 4));
                assert_eq!(o.len(), 2);
            }
            x => panic!("{x:?}"),
        }
    }

    #[test]
    #[should_panic(expected = "loop at")]
    fn loops_are_detected() {
        let mut looping = |s: &u8| -> Choices {
            vec![Choice {
                desc: 0,
                res: vec![Proba {
                    p: BigRational::from_i64(1, 1),
                    v: *s,
                }],
            }]
        };
        let none = |_: &u8| -> Option<BigRational> { None };
        solve_compact(&mut looping, &none, &0, &|_| ());
    }
}
