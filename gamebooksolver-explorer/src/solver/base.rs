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
pub struct Choice<STT, DESC> {
    pub desc: DESC,
    pub res: Outcome<STT>,
}

#[derive(Debug, Clone)]
pub enum SolNode<S, D> {
    Chosen(D, Score, Outcome<S>),
    Lost,
    Win(Score),
}

pub type Solution<S, D> = HashMap<S, SolNode<S, D>>;

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

#[cfg(test)]
mod test {
    use super::*;

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
