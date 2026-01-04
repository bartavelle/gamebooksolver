use bincode::{Decode, Encode};
use serde::{Deserialize, Serialize};

use crate::solver::rational::Rational;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub struct Proba<P, T> {
    pub v: T,
    pub p: P,
}

impl<P: Rational, T> Proba<P, T> {
    pub fn certain(nv: T) -> Proba<P, T> {
        Proba {
            p: P::from_i64(1, 1),
            v: nv,
        }
    }
}

pub type Outcome<P, T> = Vec<Proba<P, T>>;

#[derive(Debug, serde::Serialize, Clone)]
pub struct Choice<P, DESC, STT> {
    pub desc: DESC,
    pub res: Outcome<P, STT>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolNode<P, S> {
    Chosen(P, Outcome<P, S>),
    Single(P, S),
    Win(P),
}

impl<P, S> SolNode<P, S> {
    pub fn from_choices(sc: P, outcome: Outcome<P, S>) -> Self {
        if outcome.len() == 1 {
            let p = outcome.into_iter().next().unwrap();
            SolNode::Single(sc, p.v)
        } else {
            SolNode::Chosen(sc, outcome)
        }
    }
}

pub type Probably<P, A> = Vec<(A, P)>;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Encode, Decode)]
pub enum ChoppedSolution<P, State> {
    CNode(P, Probably<P, Option<State>>),
    CJump(P, State),
    CLeafLost,
    CLeaf(P),
}

pub type Solution<P, S> = HashMap<S, SolNode<P, S>>;

pub struct Cache<K, V> {
    pub cache: HashMap<K, V>,
    checks: u64,
    hits: u64,
}

impl<K, V> Default for Cache<K, V> {
    fn default() -> Self {
        Cache {
            cache: HashMap::new(),
            checks: 0,
            hits: 0,
        }
    }
}

impl<K: Eq + std::hash::Hash, V> Cache<K, V> {
    pub fn get(&mut self, k: &K) -> Option<&V> {
        self.checks += 1;
        let o = self.cache.get(k);
        if o.is_some() {
            self.hits += 1;
        }
        o
    }

    pub fn efficiency(&self) -> f64 {
        if self.checks > 0 {
            self.hits as f64 / self.checks as f64
        } else {
            0.0
        }
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.cache.insert(k, v);
    }
}

/// regroup identical outcomes, sort by decreasing probability
pub fn optimize_outcome<T: Eq + std::hash::Hash + Ord, P: Rational>(o: Outcome<P, T>) -> Outcome<P, T> {
    // fist, convert to hashmap
    let mut hm: HashMap<T, P> = HashMap::new();
    for pb in o.into_iter() {
        // why can't I use an entry for this ? :(
        let val = match hm.get(&pb.v) {
            None => pb.p,
            Some(other) => pb.p.add(other),
        };
        hm.insert(pb.v, val);
    }

    // back to an ordered vec ...
    let mut o: Outcome<P, T> = hm.into_iter().map(|(v, p)| Proba { p, v }).collect();
    sort_outcome(&mut o);
    o
}

pub fn sort_outcome<P: Rational, T: Ord>(o: &mut Outcome<P, T>) {
    o.sort_by(|a, b| {
        b.p.partial_cmp(&a.p)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| b.v.cmp(&a.v))
    });
}

impl<P: Rational, A> ChoppedSolution<P, A> {
    pub fn score(&self) -> P {
        match self {
            ChoppedSolution::CNode(r, _) => r.clone(),
            ChoppedSolution::CJump(r, _) => r.clone(),
            ChoppedSolution::CLeafLost => P::from_i64(0, 1),
            ChoppedSolution::CLeaf(r) => r.clone(),
        }
    }
}
