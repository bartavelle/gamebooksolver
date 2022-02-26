pub mod base;

use rug::Rational;
use std::hash::Hash;

use base::*;

pub enum NodeState<STT> {
    Searching,
    Solved(SolNode<STT>),
}

impl<STT> NodeState<STT> {
    pub fn get_solnode(self) -> Option<SolNode<STT>> {
        match self {
            NodeState::Searching => None,
            NodeState::Solved(sn) => Some(sn),
        }
    }
}

pub fn solve<STT: Eq + Hash + Clone + std::fmt::Debug, FC, FS, INSPECT, DESC: std::fmt::Debug>(
    get_choices: &mut FC,
    get_score: &FS,
    inistate: &STT,
    inspect: &INSPECT,
) -> Solution<STT>
where
    FC: FnMut(&STT) -> Vec<Choice<DESC, STT>>,
    FS: Fn(&STT) -> Option<Rational>,
    INSPECT: Fn(&Cache<STT, NodeState<STT>>),
{
    let mut stt: Cache<STT, NodeState<STT>> = Cache::default();

    go(get_choices, get_score, &mut stt, inistate);
    inspect(&stt);

    stt.cache
        .into_iter()
        .filter_map(|(k, v)| v.get_solnode().map(|v2| (k, v2)))
        .collect()
}

fn go<STT: Eq + Hash + Clone + std::fmt::Debug, FC, FS, DESC: std::fmt::Debug>(
    get_choices: &mut FC,
    get_score: &FS,
    search_state: &mut Cache<STT, NodeState<STT>>,
    curstate: &STT,
) -> Score
where
    FC: FnMut(&STT) -> Vec<Choice<DESC, STT>>,
    FS: Fn(&STT) -> Option<Rational>,
{
    if let Some(score) = get_score(curstate) {
        search_state.insert(
            curstate.clone(),
            NodeState::Solved(SolNode::Win(score.clone())),
        );
        return score;
    }

    if let Some(ns) = search_state.get(curstate) {
        return match ns {
            NodeState::Searching => panic!("loop at {:?}", curstate),
            NodeState::Solved(yeah) => match yeah {
                SolNode::Win(r) => r.clone(),
                SolNode::Chosen(s, _) => s.clone(),
                SolNode::Single(s, _) => s.clone(),
            },
        };
    }

    search_state.insert(curstate.clone(), NodeState::Searching);

    let choices = get_choices(curstate);
    if choices.is_empty() {
        panic!("Empty choice at state {:?}", curstate);
    }
    let mut best_choice: Option<SolNode<STT>> = None;
    let mut best_score = Rational::from(-1);
    for choice in choices {
        let mut cur_score = Rational::from(0);
        for outcome in &choice.res {
            cur_score += go(get_choices, get_score, search_state, &outcome.v) * &outcome.p;
        }
        if cur_score > best_score {
            best_choice = Some(SolNode::from_choices(cur_score.clone(), choice.res));
            best_score = cur_score;
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
