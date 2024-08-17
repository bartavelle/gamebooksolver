use crate::solver::rational::Rational;
use minicbor::decode::{Decode, Decoder, Error};
use minicbor::encode::{self, Encode, Encoder, Write};
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

// CBOR stuff

struct PPart<P, A>((Option<A>, P));
impl<'b, A: Decode<'b>, P: Rational> Decode<'b> for PPart<P, A> {
    fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
        cbor_pair(d, |d| cbor_option(d, |d| d.decode()), P::cbor_decode).map(PPart)
    }
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

impl<'b, A: Decode<'b>, P: Rational> Decode<'b> for ChoppedSolution<P, A> {
    fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
        let ln = d.array()?;
        let tg = d.u8()?;
        match (tg, ln) {
            (0, Some(3)) => {
                let sc = P::cbor_decode(d)?;
                let content: Result<Probably<P, Option<A>>, _> = d
                    .array_iter()?
                    .map(|x: Result<PPart<P, A>, _>| x.map(|e| e.0))
                    .collect();
                Ok(ChoppedSolution::CNode(sc, content?))
            }
            (1, Some(3)) => {
                let sc = P::cbor_decode(d)?;
                let stt = d.decode()?;
                Ok(ChoppedSolution::CJump(sc, stt))
            }
            (2, Some(1)) => Ok(ChoppedSolution::CLeafLost),
            (3, Some(2)) => P::cbor_decode(d).map(ChoppedSolution::CLeaf),
            _ => Err(Error::Message("Invalid variant for ChoppedSolution")),
        }
    }
}

impl<'t, A: Encode, P: Rational> Encode for ChoppedSolution<P, A> {
    fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
        match self {
            ChoppedSolution::CNode(sc, content) => {
                e.array(3)?.u8(0)?;
                sc.cbor_encode(e)?;
                e.array(content.len() as u64)?;
                for c in content {
                    e_cbor_pair(
                        e,
                        |e, x| e_cbor_option(e, x.as_ref(), |e, v| e.encode(v).map(|_| ())),
                        |e, y| y.cbor_encode(e),
                        c,
                    )?;
                }
            }
            ChoppedSolution::CJump(sc, stt) => {
                e.array(3)?.u8(1)?;
                sc.cbor_encode(e)?;
                e.encode(stt)?;
            }
            ChoppedSolution::CLeafLost => {
                e.array(1)?.u8(2)?;
            }
            ChoppedSolution::CLeaf(sc) => {
                e.array(2)?.u8(3)?;
                sc.cbor_encode(e)?;
            }
        }
        Ok(())
    }
}

pub fn cbor_pair<'a, F1, F2, V1, V2>(d: &mut Decoder<'a>, p1: F1, p2: F2) -> Result<(V1, V2), Error>
where
    F1: Fn(&mut Decoder<'a>) -> Result<V1, Error>,
    F2: Fn(&mut Decoder<'a>) -> Result<V2, Error>,
{
    let ln = d.array()?;
    if ln != Some(2) {
        return Err(Error::Message("Invalid length for pair"));
    }
    let a = p1(d)?;
    let b = p2(d)?;
    Ok((a, b))
}

pub fn e_cbor_pair<W: Write, A, B, FA, FB>(
    e: &mut Encoder<W>,
    fa: FA,
    fb: FB,
    p: &(A, B),
) -> Result<(), encode::Error<W::Error>>
where
    FA: Fn(&mut Encoder<W>, &A) -> Result<(), encode::Error<W::Error>>,
    FB: Fn(&mut Encoder<W>, &B) -> Result<(), encode::Error<W::Error>>,
{
    e.array(2)?;
    fa(e, &p.0)?;
    fb(e, &p.1)?;
    Ok(())
}

pub fn cbor_option<'a, F, V>(d: &mut Decoder<'a>, p: F) -> Result<Option<V>, Error>
where
    F: Fn(&mut Decoder<'a>) -> Result<V, Error>,
{
    let ln = d.array()?;
    match ln {
        Some(0) => Ok(None),
        Some(1) => p(d).map(Some),
        _ => Err(Error::Message("Invalid optional size")),
    }
}

pub fn e_cbor_option<W: Write, A, F>(e: &mut Encoder<W>, a: Option<A>, f: F) -> Result<(), encode::Error<W::Error>>
where
    F: Fn(&mut Encoder<W>, A) -> Result<(), encode::Error<W::Error>>,
{
    match a {
        None => {
            e.array(0)?;
        }
        Some(a) => {
            e.array(1)?;
            f(e, a)?;
        }
    }
    Ok(())
}
