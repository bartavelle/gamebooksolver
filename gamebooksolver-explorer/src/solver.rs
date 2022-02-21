pub mod base;
// pub mod comparer;

use rug::Rational;
use std::collections::HashMap;
use std::hash::Hash;

use base::*;

pub enum NodeState<STT, DESC> {
    Searching,
    Solved(SolNode<STT, DESC>),
}

pub struct SearchState<STT, DESC> {
    pub nodes: HashMap<STT, NodeState<STT, DESC>>,
}

fn populate_solution<STT: Eq + Hash + Clone, DESC: Clone>(
    sol: &mut Solution<STT, DESC>,
    searched: &SearchState<STT, DESC>,
    curstate: &STT,
) {
    if sol.get(curstate).is_some() {
        return;
    }
    if let Some(NodeState::Solved(lsol)) = searched.nodes.get(curstate) {
        sol.insert(curstate.clone(), lsol.clone());
        if let SolNode::Chosen(_, _, outcomes) = lsol {
            for outcome in outcomes {
                populate_solution(sol, searched, &outcome.v);
            }
        }
    }
}

pub fn solve<STT: Eq + Hash + Clone + std::fmt::Debug, DESC: std::fmt::Debug + Clone, FC, FS, DBGSOL, INSPECT>(
    get_choices: &FC,
    get_score: &FS,
    inistate: STT,
    mdbg: Option<&DBGSOL>,
    inspect: &INSPECT,
) -> Solution<STT, DESC>
where
    FC: Fn(&STT) -> Vec<Choice<STT, DESC>>,
    FS: Fn(&STT) -> Option<Rational>,
    DBGSOL: Fn(&STT, &SolNode<STT, DESC>),
    INSPECT: Fn(&SearchState<STT, DESC>),
{
    let mut stt: SearchState<STT, DESC> = SearchState { nodes: HashMap::new() };

    go(get_choices, get_score, &mut stt, inistate.clone(), mdbg);
    inspect(&stt);

    let mut out: Solution<STT, DESC> = HashMap::new();

    populate_solution(&mut out, &stt, &inistate);
    out
}

fn go<STT: Eq + Hash + Clone + std::fmt::Debug, DESC: std::fmt::Debug, FC, FS, DBGSOL>(
    get_choices: &FC,
    get_score: &FS,
    search_state: &mut SearchState<STT, DESC>,
    curstate: STT,
    mdbg: Option<&DBGSOL>,
) -> Score
where
    FC: Fn(&STT) -> Vec<Choice<STT, DESC>>,
    FS: Fn(&STT) -> Option<Rational>,
    DBGSOL: Fn(&STT, &SolNode<STT, DESC>),
{
    if let Some(score) = get_score(&curstate) {
        search_state
            .nodes
            .insert(curstate, NodeState::Solved(SolNode::Win(score.clone())));
        return score;
    }

    if let Some(ns) = search_state.nodes.get(&curstate) {
        return match ns {
            NodeState::Searching => panic!("loop at {:?}", curstate),
            NodeState::Solved(yeah) => match yeah {
                SolNode::Lost => Rational::from(0),
                SolNode::Win(r) => r.clone(),
                SolNode::Chosen(_, s, _) => s.clone(),
            },
        };
    }

    search_state.nodes.insert(curstate.clone(), NodeState::Searching);

    let choices = get_choices(&curstate);
    if choices.is_empty() {
        panic!("Empty choice at state {:?}", curstate);
    }
    let mut best_choice: Option<SolNode<STT, DESC>> = None;
    let mut best_score = Rational::from(-1);
    for choice in choices {
        let mut cur_score = Rational::from(0);
        for outcome in &choice.res {
            cur_score += go(get_choices, get_score, search_state, outcome.v.clone(), mdbg) * outcome.p.clone();
        }
        if cur_score > best_score {
            best_choice = Some(SolNode::Chosen(choice.desc, cur_score.clone(), choice.res));
            best_score = cur_score;
        }
    }

    match best_choice {
        None => panic!(
            "No score, but no choices for {:?}, choices: {:?}",
            curstate,
            get_choices(&curstate)
        ),
        Some(sol) => {
            if let Some(f) = mdbg {
                f(&curstate, &sol);
            }
            search_state.nodes.insert(curstate, NodeState::Solved(sol));
        }
    }

    best_score
}
