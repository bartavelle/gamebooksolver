use bincode::{Decode, Encode};
use serde::{Deserialize, Serialize};

use crate::solver::rational::Rational;
use std::collections::HashMap;
use std::hash::BuildHasher;

/// Hashing is a large part of the solving time, and no key comes from an untrusted source.
pub type FastHash = foldhash::fast::RandomState;
pub type FastMap<K, V> = HashMap<K, V, FastHash>;

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
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

/// A map split in many smaller ones. A map needs its old and its new table when growing, which would be a very large single
/// allocation for the hundreds of millions of states of the largest books.
pub struct Shards<K, V> {
    hasher: FastHash,
    maps: Vec<FastMap<K, V>>,
}

const SHARD_BITS: u32 = 8;

impl<K, V> Default for Shards<K, V> {
    fn default() -> Self {
        Shards {
            hasher: FastHash::default(),
            maps: (0..1 << SHARD_BITS).map(|_| FastMap::default()).collect(),
        }
    }
}

impl<K, V> Shards<K, V> {
    pub fn len(&self) -> usize {
        self.maps.iter().map(|m| m.len()).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.maps.iter().all(|m| m.is_empty())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.maps.iter().flatten()
    }
}

impl<K: Eq + std::hash::Hash, V> Shards<K, V> {
    fn shard(&self, k: &K) -> usize {
        // the maps use the lowest and the highest bits of their own hash
        ((self.hasher.hash_one(k) >> 48) & ((1 << SHARD_BITS) - 1)) as usize
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.maps[self.shard(k)].get(k)
    }

    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        let shard = self.shard(&k);
        self.maps[shard].insert(k, v)
    }

    pub fn retain(&mut self, mut keep: impl FnMut(&K, &mut V) -> bool) {
        self.maps.iter_mut().for_each(|m| m.retain(&mut keep));
    }

    pub fn remove(&mut self, k: &K) -> Option<V> {
        let shard = self.shard(k);
        self.maps[shard].remove(k)
    }
}

impl<K: Eq + std::hash::Hash, V> std::ops::Index<&K> for Shards<K, V> {
    type Output = V;

    fn index(&self, k: &K) -> &V {
        self.get(k).expect("no entry found for key")
    }
}

impl<K, V> IntoIterator for Shards<K, V> {
    type Item = (K, V);
    type IntoIter = std::iter::Flatten<std::vec::IntoIter<FastMap<K, V>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.maps.into_iter().flatten()
    }
}

/// A state in the form kept by the solver. The largest books have about a billion states, so that each byte counts.
pub trait PackedState: Sized {
    type Key: Eq + std::hash::Hash;
    /// What is needed to go from a state to its key and back, that is common to many states.
    type Common: Default;

    fn pack(&self, common: &mut Self::Common) -> Self::Key;
    /// The key of a state, if it can be one of those that have already been packed.
    fn find(&self, common: &Self::Common) -> Option<Self::Key>;
    fn unpack(key: &Self::Key, common: &Self::Common) -> Self;
}

impl PackedState for u8 {
    type Key = u8;
    type Common = ();

    fn pack(&self, _: &mut ()) -> u8 {
        *self
    }
    fn find(&self, _: &()) -> Option<u8> {
        Some(*self)
    }
    fn unpack(key: &u8, _: &()) -> u8 {
        *key
    }
}

pub struct Cache<K, V> {
    pub cache: Shards<K, V>,
    checks: u64,
    hits: u64,
}

impl<K, V> Default for Cache<K, V> {
    fn default() -> Self {
        Cache {
            cache: Shards::default(),
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
    let mut hm: FastMap<T, P> = FastMap::default();
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn shards_behave_like_a_map() {
        let mut s: Shards<u32, u32> = Shards::default();
        assert!(s.is_empty());
        for i in 0..10000 {
            assert_eq!(s.insert(i, i * 2), None);
        }
        assert_eq!(s.insert(5, 7), Some(10));
        assert_eq!(s.len(), 10000);
        assert_eq!(s.get(&5), Some(&7));
        assert_eq!(s[&6], 12);
        assert_eq!(s.get(&10000), None);
        assert_eq!(s.iter().count(), 10000);
        let all: HashMap<u32, u32> = s.into_iter().collect();
        assert_eq!(all.len(), 10000);
        assert_eq!(all[&7], 14);
    }
}
