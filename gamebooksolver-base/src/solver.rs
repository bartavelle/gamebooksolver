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
pub enum Retention<'t, STT> {
    All,
    /// The scores are the same, but the first state is the only one that is always available afterwards. These states
    /// are not kept:
    /// * those with a known score, as it is as fast to find again
    /// * those for which `transient` holds, once there are `max_transient` of them, and until the end of the search only
    ///   those being searched are kept. They should be states that are only useful for a short time
    Reduced {
        transient: &'t dyn Fn(&STT) -> bool,
        max_transient: usize,
    },
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
    transient: Shards<STT::Key, CompactNode<P>>,
    transient_count: usize,
    common: STT::Common,
    retention: Retention<'t, STT>,
    roots: &'t [STT],
}

impl<P, STT: PackedState + PartialEq> Search<'_, P, STT> {
    fn discardable(&self, state: &STT) -> bool {
        !matches!(self.retention, Retention::All) && !self.roots.contains(state)
    }

    fn is_transient(&self, state: &STT) -> bool {
        match self.retention {
            Retention::All => false,
            Retention::Reduced { transient, .. } => transient(state) && !self.roots.contains(state),
        }
    }

    fn make_room(&mut self) {
        if let Retention::Reduced { max_transient, .. } = self.retention
            && self.transient_count >= max_transient
        {
            self.transient.retain(|_, node| node.choice == SEARCHING);
            self.transient_count = self.transient.len();
        }
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
    retention: Retention<'_, STT>,
) -> CompactSolved<P, STT>
where
    FC: FnMut(&STT) -> Vec<Choice<P, DESC, STT>>,
    FS: Fn(&STT) -> Option<P>,
    INSPECT: Fn(&Cache<STT::Key, CompactNode<P>>),
{
    solve_compact_starts(get_choices, get_score, std::slice::from_ref(inistate), inspect, retention)
}

/// Same as `solve_compact`, for several initial states that share what they have in common. All of them are kept.
pub fn solve_compact_starts<
    STT: Eq + Hash + Clone + std::fmt::Debug + PartialOrd + std::fmt::Display + PackedState,
    FC,
    FS,
    INSPECT,
    DESC: std::fmt::Debug,
    P: Rational,
>(
    get_choices: &mut FC,
    get_score: &FS,
    inistates: &[STT],
    inspect: &INSPECT,
    retention: Retention<'_, STT>,
) -> CompactSolved<P, STT>
where
    FC: FnMut(&STT) -> Vec<Choice<P, DESC, STT>>,
    FS: Fn(&STT) -> Option<P>,
    INSPECT: Fn(&Cache<STT::Key, CompactNode<P>>),
{
    let mut search = Search {
        cache: Cache::default(),
        transient: Shards::default(),
        transient_count: 0,
        common: STT::Common::default(),
        retention,
        roots: inistates,
    };

    for inistate in inistates {
        go(get_choices, get_score, &mut search, inistate);
    }
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
    let transient = search.is_transient(curstate);
    let found = if transient {
        search.transient.get(&key)
    } else {
        search.cache.get(&key)
    };
    if let Some(ns) = found {
        if ns.choice == SEARCHING {
            for (k, v) in search.cache.cache.iter().chain(search.transient.iter()) {
                if v.choice == SEARCHING {
                    println!(" * {}", STT::unpack(k, &search.common))
                }
            }
            panic!("loop at {}", curstate)
        }
        return ns.score.clone();
    }

    let searching = CompactNode {
        score: P::from_i64(0, 1),
        choice: SEARCHING,
    };
    let key2 = curstate.pack(&mut search.common);
    if transient {
        search.make_room();
        search.transient.insert(key2, searching);
        search.transient_count += 1;
    } else {
        search.cache.insert(key2, searching);
    }

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
            let node = CompactNode {
                score: best_score.clone(),
                choice,
            };
            if transient {
                search.transient.insert(key, node);
            } else {
                search.cache.insert(key, node);
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

    fn reduced<'t>(transient: &'t dyn Fn(&u8) -> bool) -> Retention<'t, u8> {
        Retention::Reduced {
            transient,
            max_transient: 1,
        }
    }

    #[test]
    fn transient_states_are_not_kept() {
        let compact = solve_compact(&mut choices, &score, &0, &|_| (), reduced(&|s| *s == 2));
        assert_eq!(compact.get(&0).unwrap().score, BigRational::from_i64(3, 4));
        assert!(compact.get(&2).is_none());
        // a search with almost no room for these states, and the root is never one of them
        let all = solve_compact(&mut choices, &score, &0, &|_| (), reduced(&|_| true));
        assert_eq!(all.get(&0).unwrap().score, BigRational::from_i64(3, 4));
        assert_eq!(all.get(&0).unwrap().choice, 1);
    }

    #[test]
    fn several_starts_have_the_scores_of_separate_solves() {
        let separate = |s: u8| solve_compact(&mut choices, &score, &s, &|_| (), reduced(&|_| true)).get(&s).unwrap().score.clone();
        // everything is transient, but the starts must still be kept
        let starts = [0, 2];
        let both = solve_compact_starts(&mut choices, &score, &starts, &|_| (), reduced(&|_| true));
        for s in starts {
            assert_eq!(both.get(&s).unwrap().score, separate(s));
        }
    }

    #[test]
    fn loops_are_detected_in_transient_states() {
        let res = std::panic::catch_unwind(|| {
            let mut looping = |s: &u8| vec![Choice { desc: 0, res: vec![Proba::<BigRational, u8>::certain(1 - *s)] }, Choice { desc: 1, res: vec![Proba::certain(1 - *s)] }];
            solve_compact(&mut looping, &|_: &u8| None, &0, &|_| (), reduced(&|_| true));
        });
        assert!(res.is_err());
    }

    #[test]
    fn reduced_retention_gives_the_same_scores() {
        let compact = solve_compact(&mut choices, &score, &0, &|_| (), reduced(&|_| false));
        assert_eq!(compact.get(&0).unwrap().score, BigRational::from_i64(3, 4));
        assert_eq!(compact.get(&0).unwrap().choice, 1);
        // a choice is kept, what is known is not
        assert!(compact.get(&2).is_some());
        assert!(compact.get(&3).is_none());
        // the first state is always kept
        let forced = solve_compact(&mut |_: &u8| vec![Choice { desc: 0, res: vec![Proba::certain(3)] }], &score, &5, &|_| (), reduced(&|_| false));
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
