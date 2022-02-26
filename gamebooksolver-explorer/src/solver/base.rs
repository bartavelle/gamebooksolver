use minicbor::decode::{Decode, Decoder, Error};
use minicbor::encode::{self, Encode, Encoder, Write};
use rug::Integer;
use rug::Rational;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Proba<T> {
    pub v: T,
    pub p: Rational,
}

impl<T> Proba<T> {
    pub fn certain(nv: T) -> Proba<T> {
        Proba {
            p: Rational::from(1),
            v: nv,
        }
    }
}

pub type Score = Rational;

pub type Outcome<T> = Vec<Proba<T>>;

#[derive(Debug)]
pub struct Choice<DESC, STT> {
    pub desc: DESC,
    pub res: Outcome<STT>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolNode<S> {
    Chosen(Score, Outcome<S>),
    Single(Score, S),
    Win(Score),
}

impl<S> SolNode<S> {
    pub fn from_choices(sc: Score, outcome: Outcome<S>) -> Self {
        if outcome.len() == 1 {
            let p = outcome.into_iter().next().unwrap();
            SolNode::Single(sc, p.v)
        } else {
            SolNode::Chosen(sc, outcome)
        }
    }
}

pub type Probably<A> = Vec<(A, Rational)>;

#[derive(Debug, PartialEq, Eq)]
pub enum ChoppedSolution<State> {
    CNode(Rational, Probably<Option<State>>),
    CJump(Rational, State),
    CLeafLost,
    CLeaf(Rational),
}

pub type Solution<S> = HashMap<S, SolNode<S>>;

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
pub fn optimize_outcome<T: Eq + std::hash::Hash + Ord>(o: Outcome<T>) -> Outcome<T> {
    // fist, convert to hashmap
    let mut hm: HashMap<T, Rational> = HashMap::new();
    for pb in o.into_iter() {
        // why can't I use an entry for this ? :(
        let val = match hm.get(&pb.v) {
            None => pb.p,
            Some(other) => pb.p + other,
        };
        hm.insert(pb.v, val);
    }

    // back to an ordered vec ...
    let mut o: Outcome<T> = hm.into_iter().map(|(v, p)| Proba { p, v }).collect();
    sort_outcome(&mut o);
    o
}

pub fn sort_outcome<T: Ord>(o: &mut Outcome<T>) {
    o.sort_by(|a, b| b.p.cmp(&a.p).then_with(|| b.v.cmp(&a.v)));
}

// CBOR stuff

struct PPart<A>((Option<A>, Rational));
impl<'b, A: Decode<'b>> Decode<'b> for PPart<A> {
    fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
        cbor_pair(d, |d| cbor_option(d, |d| d.decode()), rational).map(PPart)
    }
}

impl<A> ChoppedSolution<A> {
    pub fn score(&self) -> Rational {
        match self {
            ChoppedSolution::CNode(r, _) => r.clone(),
            ChoppedSolution::CJump(r, _) => r.clone(),
            ChoppedSolution::CLeafLost => Rational::from(0),
            ChoppedSolution::CLeaf(r) => r.clone(),
        }
    }
}

impl<'b, A: Decode<'b>> Decode<'b> for ChoppedSolution<A> {
    fn decode(d: &mut Decoder<'b>) -> Result<Self, Error> {
        let ln = d.array()?;
        let tg = d.u8()?;
        match (tg, ln) {
            (0, Some(3)) => {
                let sc = rational(d)?;
                let content: Result<Probably<Option<A>>, _> = d
                    .array_iter()?
                    .map(|x: Result<PPart<A>, _>| x.map(|e| e.0))
                    .collect();
                Ok(ChoppedSolution::CNode(sc, content?))
            }
            (1, Some(3)) => {
                let sc = rational(d)?;
                let stt = d.decode()?;
                Ok(ChoppedSolution::CJump(sc, stt))
            }
            (2, Some(1)) => Ok(ChoppedSolution::CLeafLost),
            (3, Some(2)) => rational(d).map(ChoppedSolution::CLeaf),
            _ => Err(Error::Message("Invalid variant for ChoppedSolution")),
        }
    }
}

impl<A: Encode> Encode for ChoppedSolution<A> {
    fn encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
        match self {
            ChoppedSolution::CNode(sc, content) => {
                e.array(3)?.u8(0)?;
                e_rational(e, sc)?;
                e.array(content.len() as u64)?;
                for c in content {
                    e_cbor_pair(
                        e,
                        |e, x| e_cbor_option(e, x.as_ref(), |e, v| e.encode(v).map(|_| ())),
                        |e, y| e_rational(e, y),
                        c,
                    )?;
                }
            }
            ChoppedSolution::CJump(sc, stt) => {
                e.array(3)?.u8(1)?;
                e_rational(e, sc)?;
                e.encode(stt)?;
            }
            ChoppedSolution::CLeafLost => {
                e.array(1)?.u8(2)?;
            }
            ChoppedSolution::CLeaf(sc) => {
                e.array(2)?.u8(3)?;
                e_rational(e, sc)?;
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

pub fn e_cbor_option<W: Write, A, F>(
    e: &mut Encoder<W>,
    a: Option<A>,
    f: F,
) -> Result<(), encode::Error<W::Error>>
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

pub fn rational(d: &mut Decoder) -> Result<Rational, Error> {
    fn read_bigint(d: &mut Decoder) -> Result<Integer, Error> {
        let tp = d.datatype()?;
        match tp {
            minicbor::data::Type::Tag => {
                let tg = d.tag()?;
                match tg {
                    minicbor::data::Tag::PosBignum => {
                        Ok(Integer::from_digits(d.bytes()?, rug::integer::Order::Msf))
                    }
                    _ => Err(Error::Message("bad tag for bignum")),
                }
            }
            _ => d.u64().map(Integer::from),
        }
    }
    let (n, d) = cbor_pair(d, read_bigint, read_bigint)?;
    Ok(Rational::from((n, d)))
}

pub fn e_rational<W: Write>(
    e: &mut Encoder<W>,
    r: &Rational,
) -> Result<(), encode::Error<W::Error>> {
    fn e_bigint<W: Write>(e: &mut Encoder<W>, i: &Integer) -> Result<(), encode::Error<W::Error>> {
        e.tag(minicbor::data::Tag::PosBignum)?
            .bytes(&i.to_digits(rug::integer::Order::Msf))?;
        Ok(())
    }
    e.array(2)?;
    e_bigint(e, r.numer())?;
    e_bigint(e, r.denom())?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn t_rational() {
        let r = Rational::from((5615165615_u64, 8122123_u64));
        let mut buffer = [0u8; 128];
        let mut e = minicbor::encode::Encoder::new(&mut buffer as &mut [u8]);
        e_rational(&mut e, &r).unwrap();
        let mut d = minicbor::decode::Decoder::new(&buffer);
        let r2 = rational(&mut d).unwrap();
        assert_eq!(r, r2);
    }

    #[test]
    fn test_optimize_outcome() {
        let to: Outcome<u8> = [(8, 2), (12, 1), (5, 5), (12, 2)]
            .iter()
            .map(|(v, p): &(u8, u8)| Proba {
                v: *v,
                p: Rational::from(*p) / Rational::from(10),
            })
            .collect();
        let new = optimize_outcome(to);
        let expected: Outcome<u8> = [(5, 5), (12, 3), (8, 2)]
            .iter()
            .map(|(v, p): &(u8, u8)| Proba {
                v: *v,
                p: Rational::from(*p) / Rational::from(10),
            })
            .collect();

        assert_eq!(new, expected);
    }
}
