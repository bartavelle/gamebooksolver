pub mod base;
pub mod rational;

use base::*;
use rational::Rational;

use std::hash::Hash;

/// What is kept for a state while solving: its score and the index of the best choice. The outcomes can be computed again
/// from the choice, and keeping them all would use several times more memory than the rest of the solver.
#[derive(Debug, Clone, PartialEq)]
pub struct CompactNode<P> {
    pub score: P,
    pub choice: u16,
}

/// Which states are kept once they are solved.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Retention {
    All,
    /// Not the states with a known score, nor those with a single choice and outcome, which are nearly as fast to solve
    /// again as to find. The scores are the same, only the first state and the others are not always available afterwards.
    Reduced,
}

/// The score and best choice of the states that were kept, see `Retention`.
pub struct CompactSolved<P, STT: PackedState> {
    nodes: Shards<STT::Key, CompactNode<P>>,
    common: STT::Common,
}

impl<P, STT: PackedState> CompactSolved<P, STT> {
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn get(&self, state: &STT) -> Option<&CompactNode<P>> {
        self.nodes.get(&state.find(&self.common)?)
    }

    pub fn iter(&self) -> impl Iterator<Item = (STT, &CompactNode<P>)> {
        self.nodes.iter().map(|(k, node)| (STT::unpack(k, &self.common), node))
    }
}

// the values of the choice that do not stand for a choice
const SEARCHING: u16 = u16::MAX;
const WIN: u16 = u16::MAX - 1;

struct Search<'t, P, STT: PackedState> {
    cache: Cache<STT::Key, CompactNode<P>>,
    common: STT::Common,
    retention: Retention,
    root: &'t STT,
}

impl<P, STT: PackedState + PartialEq> Search<'_, P, STT> {
    fn discardable(&self, state: &STT) -> bool {
        self.retention == Retention::Reduced && state != self.root
    }
}

/// Finds the score and the best choice of the states that can be reached, keeping those that `retention` asks for.
pub fn solve_compact<
    STT: Eq + Hash + Clone + std::fmt::Debug + PartialOrd + std::fmt::Display + PackedState,
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
    retention: Retention,
) -> CompactSolved<P, STT>
where
    FC: FnMut(&STT) -> Vec<Choice<P, DESC, STT>>,
    FS: Fn(&STT) -> Option<P>,
    INSPECT: Fn(&Cache<STT::Key, CompactNode<P>>),
{
    let mut search = Search {
        cache: Cache::default(),
        common: STT::Common::default(),
        retention,
        root: inistate,
    };

    go(get_choices, get_score, &mut search, inistate);
    inspect(&search.cache);

    CompactSolved {
        nodes: search.cache.cache,
        common: search.common,
    }
}

/// Same as `solve_compact`, with the outcomes of the best choice of each state.
pub fn solve<
    STT: Eq + Hash + Clone + std::fmt::Debug + PartialOrd + std::fmt::Display + PackedState,
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
    INSPECT: Fn(&Cache<STT::Key, CompactNode<P>>),
{
    let solved = solve_compact(get_choices, get_score, inistate, inspect, Retention::All);
    solved
        .nodes
        .into_iter()
        .map(|(k, node)| {
            let k = STT::unpack(&k, &solved.common);
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
    STT: Eq + Hash + Clone + std::fmt::Debug + std::fmt::Display + PartialOrd + PackedState,
    FC,
    FS,
    DESC: std::fmt::Debug,
    P: Rational,
>(
    get_choices: &mut FC,
    get_score: &FS,
    search: &mut Search<'_, P, STT>,
    curstate: &STT,
) -> P
where
    FC: FnMut(&STT) -> Vec<Choice<P, DESC, STT>>,
    FS: Fn(&STT) -> Option<P>,
{
    if let Some(score) = get_score(curstate) {
        if !search.discardable(curstate) {
            search.cache.insert(
                curstate.pack(&mut search.common),
                CompactNode {
                    score: score.clone(),
                    choice: WIN,
                },
            );
        }
        return score;
    }

    let key = curstate.pack(&mut search.common);
    if let Some(ns) = search.cache.get(&key) {
        if ns.choice == SEARCHING {
            for (k, v) in search.cache.cache.iter() {
                if v.choice == SEARCHING {
                    println!(" * {}", STT::unpack(k, &search.common))
                }
            }
            panic!("loop at {}", curstate)
        }
        return ns.score.clone();
    }

    search.cache.insert(
        curstate.pack(&mut search.common),
        CompactNode {
            score: P::from_i64(0, 1),
            choice: SEARCHING,
        },
    );

    let choices = get_choices(curstate);
    if choices.is_empty() {
        panic!("Empty choice at state {}", curstate);
    }
    let single_outcome = matches!(choices.as_slice(), [c] if c.res.len() == 1);
    let mut best_choice: Option<(u16, SolNode<P, STT>)> = None;
    let mut best_score = P::from_i64(-1, 1);
    for (idx, choice) in choices.into_iter().enumerate() {
        let idx = u16::try_from(idx).expect("too many choices");
        let mut cur_score = P::from_i64(0, 1);
        for outcome in &choice.res {
            cur_score = cur_score.add(&go(get_choices, get_score, search, &outcome.v).mul(&outcome.p));
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
            if single_outcome && search.discardable(curstate) {
                search.cache.cache.remove(&key);
            } else {
                search.cache.insert(
                    key,
                    CompactNode {
                        score: best_score.clone(),
                        choice,
                    },
                );
            }
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
        let compact = solve_compact(&mut choices, &score, &0, &|_| (), Retention::All);
        assert_eq!(compact.get(&0).unwrap().score, BigRational::from_i64(3, 4));
        assert_eq!(compact.get(&0).unwrap().choice, 1);
        assert_eq!(compact.get(&2).unwrap().choice, 1);
        assert_eq!(compact.get(&3).unwrap().choice, WIN);
    }

    #[test]
    fn reduced_retention_gives_the_same_scores() {
        let compact = solve_compact(&mut choices, &score, &0, &|_| (), Retention::Reduced);
        assert_eq!(compact.get(&0).unwrap().score, BigRational::from_i64(3, 4));
        assert_eq!(compact.get(&0).unwrap().choice, 1);
        // a choice is kept, what is known is not
        assert!(compact.get(&2).is_some());
        assert!(compact.get(&3).is_none());
        // the first state is always kept
        let forced = solve_compact(&mut |_: &u8| vec![Choice { desc: 0, res: vec![Proba::certain(3)] }], &score, &5, &|_| (), Retention::Reduced);
        assert!(forced.get(&5).is_some());
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
        solve_compact(&mut looping, &none, &0, &|_| (), Retention::All);
    }
}
