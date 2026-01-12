pub mod base;
pub mod rational;

use base::*;
use rational::Rational;

use std::hash::Hash;

pub enum NodeState<P, STT> {
    Searching,
    Solved(SolNode<P, STT>),
}

impl<P, STT> NodeState<P, STT> {
    pub fn get_solnode(self) -> Option<SolNode<P, STT>> {
        match self {
            NodeState::Searching => None,
            NodeState::Solved(sn) => Some(sn),
        }
    }
}

pub fn solve<
    STT: Eq + Hash + Clone + std::fmt::Debug + PartialOrd,
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
    INSPECT: Fn(&Cache<STT, NodeState<P, STT>>),
{
    let mut stt: Cache<STT, NodeState<P, STT>> = Cache::default();

    go(get_choices, get_score, &mut stt, inistate);
    inspect(&stt);

    stt.cache
        .into_iter()
        .filter_map(|(k, v)| v.get_solnode().map(|v2| (k, v2)))
        .collect()
}

fn go<STT: Eq + Hash + Clone + std::fmt::Debug + PartialOrd, FC, FS, DESC: std::fmt::Debug, P: Rational>(
    get_choices: &mut FC,
    get_score: &FS,
    search_state: &mut Cache<STT, NodeState<P, STT>>,
    curstate: &STT,
) -> P
where
    FC: FnMut(&STT) -> Vec<Choice<P, DESC, STT>>,
    FS: Fn(&STT) -> Option<P>,
{
    if let Some(score) = get_score(curstate) {
        search_state.insert(curstate.clone(), NodeState::Solved(SolNode::Win(score.clone())));
        return score;
    }

    if let Some(ns) = search_state.get(curstate) {
        return match ns {
            NodeState::Searching => panic!("loop at {:?}", curstate),
            NodeState::Solved(yeah) => match yeah {
                SolNode::Win(s) | SolNode::Chosen(s, _) | SolNode::Single(s, _) => s.clone(),
            },
        };
    }

    search_state.insert(curstate.clone(), NodeState::Searching);

    let choices = get_choices(curstate);
    if choices.is_empty() {
        panic!("Empty choice at state {:?}", curstate);
    }
    let mut best_choice: Option<SolNode<P, STT>> = None;
    let mut best_score = P::from_i64(-1, 1);
    for choice in choices {
        let mut cur_score = P::from_i64(0, 1);
        for outcome in &choice.res {
            cur_score = cur_score.add(&go(get_choices, get_score, search_state, &outcome.v).mul(&outcome.p));
        }
        if cur_score > best_score {
            best_choice = Some(SolNode::from_choices(cur_score.clone(), choice.res));
            best_score = cur_score;
        } else if cur_score == best_score {
            let newchoice = SolNode::from_choices(cur_score.clone(), choice.res);
            if let Some(bc) = &best_choice {
                if &newchoice > bc {
                    best_choice = Some(newchoice);
                }
            } else {
                best_choice = Some(newchoice);
            }
        }
    }

    match best_choice {
        None => panic!(
            "No score, but no choices for {:?}, choices: {:?}",
            curstate,
            get_choices(curstate)
        ),
        Some(sol) => {
            search_state.insert(curstate.clone(), NodeState::Solved(sol));
        }
    }

    best_score
}
