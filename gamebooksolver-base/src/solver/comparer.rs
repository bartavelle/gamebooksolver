use crate::solver::base::{Choice, Outcome, Score};
use rug::Rational;

#[derive(Clone, Copy)]
pub enum ScoreComparison {
    L,
    EQ,
    R,
    ExploreLeft,
    ExploreRight,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MScoreComparison {
    Select(usize),
    Search(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Searching<S, D> {
    pub desc: D,
    pub curscore: Score,
    pub curknown: Rational,
    pub unexplored: Outcome<S>,
}

struct ScoreBound {
    min: Score,
    max: Score,
}

impl<S: Eq + Clone, D> Searching<S, D> {
    pub fn new(d: D, o: Outcome<S>) -> Searching<S, D> {
        Searching {
            desc: d,
            curscore: Rational::from(0),
            curknown: Rational::from(0),
            unexplored: o,
        }
    }

    pub fn from_choice(choice: Choice<S, D>) -> Searching<S, D> {
        Searching::new(choice.desc, choice.res)
    }

    pub fn update(&mut self, foundstate: &S, score: Score) -> Option<&Score> {
        if let Some(pos) = self.unexplored.iter().position(|x| &x.v == foundstate) {
            let v = &self.unexplored[pos];
            self.curscore += &v.p * score;
            self.curknown += &v.p;
            self.unexplored.remove(pos);
            Some(&self.curscore)
        } else {
            None
        }
    }
}

impl<S, D> Searching<S, D> {
    fn pscore_bounds(&self) -> ScoreBound {
        ScoreBound {
            min: self.curscore.clone(),
            max: self.curscore.clone() + Rational::from(1) - self.curknown.clone(),
        }
    }

    /// finds the best alternative, if known, or tells which one to search first
    pub fn mscore_compare(scores: &[Searching<S, D>]) -> MScoreComparison {
        if scores.len() <= 1 {
            return MScoreComparison::Select(0);
        }

        struct BestMargin {
            idx: usize,
            margin: Rational,
        }

        struct Dominating {
            idx: usize,
            bnds: ScoreBound,
        }

        struct SState {
            mk: BestMargin,
            dom: Option<Dominating>,
        }
        fn margin<S, D>(elem: &Searching<S, D>) -> Option<&Rational> {
            elem.unexplored.first().map(|x| &x.p)
        }

        let mut curstate = SState {
            mk: BestMargin {
                idx: 0,
                margin: margin(&scores[0]).cloned().unwrap_or_else(|| Rational::from(0)),
            },
            dom: Some(Dominating {
                idx: 0,
                bnds: scores[0].pscore_bounds(),
            }),
        };

        for (curidx, elem) in scores.iter().enumerate().skip(1) {
            if let Some(nmargin) = margin(elem) {
                if &curstate.mk.margin < nmargin {
                    curstate.mk.margin = nmargin.clone();
                    curstate.mk.idx = curidx;
                }
            }
            if let Some(curdom) = &curstate.dom {
                let curbound = elem.pscore_bounds();
                if curbound.min >= curdom.bnds.max {
                    curstate.dom = Some(Dominating {
                        idx: curidx,
                        bnds: curbound,
                    });
                } else if curbound.max > curdom.bnds.min {
                    curstate.dom = None;
                }
            }
        }

        match curstate.dom {
            Some(dom) => MScoreComparison::Select(dom.idx),
            None => MScoreComparison::Search(curstate.mk.idx),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::solver::base::Probably;

    #[test]
    fn update_certain() {
        let mut s: Searching<&str, ()> = Searching {
            desc: (),
            curknown: Rational::from(1),
            curscore: Rational::from_f64(0.3).unwrap(),
            unexplored: Vec::new(),
        };
        let s2 = s.clone();
        s.update(&"lala", Rational::from(1));
        assert_eq!(s, s2);
    }

    fn tenth(x: u8) -> Rational {
        Rational::from(x) / Rational::from(10)
    }

    #[test]
    fn update_already_known() {
        let mut s: Searching<&str, ()> = Searching {
            desc: (),
            curknown: tenth(5),
            curscore: tenth(3),
            unexplored: vec![Probably { p: tenth(3), v: "lulu" }, Probably { p: tenth(2), v: "lolo" }],
        };
        let s2 = s.clone();
        s.update(&"lala", Rational::from(1));
        assert_eq!(s, s2);
    }

    #[test]
    fn update_best() {
        let p1 = Probably { p: tenth(3), v: "lulu" };
        let p2 = Probably { p: tenth(2), v: "lolo" };
        let score = tenth(8);
        let mut s: Searching<&str, ()> = Searching {
            desc: (),
            curknown: tenth(5),
            curscore: tenth(3),
            unexplored: vec![p1.clone(), p2.clone()],
        };
        let s2: Searching<&str, ()> = Searching {
            desc: (),
            curknown: s.curknown.clone() + p1.p.clone(),
            curscore: s.curscore.clone() + p1.p * score.clone(),
            unexplored: vec![p2],
        };
        s.update(&p1.v, score);
        assert_eq!(s, s2);
    }

    #[test]
    fn update_second() {
        let p1 = Probably { p: tenth(3), v: "lulu" };
        let p2 = Probably { p: tenth(2), v: "lolo" };
        let score = tenth(8);
        let mut s: Searching<&str, ()> = Searching {
            desc: (),
            curknown: tenth(5),
            curscore: tenth(3),
            unexplored: vec![p1.clone(), p2.clone()],
        };
        let s2: Searching<&str, ()> = Searching {
            desc: (),
            curknown: s.curknown.clone() + p2.p.clone(),
            curscore: s.curscore.clone() + p2.p * score.clone(),
            unexplored: vec![p1],
        };
        s.update(&p2.v, score);
        assert_eq!(s, s2);
    }

    #[test]
    fn update_all() {
        let p1 = Probably { p: tenth(3), v: "lulu" };
        let p2 = Probably { p: tenth(2), v: "lolo" };
        let score = tenth(8);
        let mut s: Searching<&str, ()> = Searching {
            desc: (),
            curknown: tenth(5),
            curscore: tenth(3),
            unexplored: vec![p1.clone(), p2.clone()],
        };
        let s2: Searching<&str, ()> = Searching {
            desc: (),
            curknown: tenth(10),
            curscore: s.curscore.clone() + (p1.p + p2.p) * score.clone(),
            unexplored: Vec::new(),
        };
        s.update(&p1.v, score.clone());
        s.update(&p2.v, score);
        assert_eq!(s, s2);
    }

    fn to_outcome<S: Clone>(lst: &[(u8, S)]) -> Outcome<S> {
        fn to_proba<S: Clone>(tpl: &(u8, S)) -> Probably<S> {
            Probably {
                p: tenth(tpl.0),
                v: tpl.1.clone(),
            }
        }
        lst.iter().map(to_proba).collect()
    }

    fn to_search<S: Eq + Clone, D>(i: D, outcome: &[(u8, S)]) -> Searching<S, D> {
        Searching::new(i, to_outcome(outcome))
    }

    fn to_searching<S: Eq + Clone, D: Clone>(cases: &[(D, &[(u8, S)])]) -> Vec<Searching<S, D>> {
        cases
            .iter()
            .map(|(d, scases)| to_search((*d).clone(), scases))
            .collect()
    }

    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    enum DummyState {
        S1,
        S2,
        S3,
        S4,
    }

    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    enum DummyDesc {
        D1,
        D2,
        D3,
        D4,
    }

    #[test]
    fn test_certain_a() {
        use DummyDesc::*;

        let s1: Vec<Searching<DummyState, DummyDesc>> = vec![
            Searching {
                desc: D1,
                curknown: tenth(10),
                curscore: tenth(8),
                unexplored: Vec::new(),
            },
            Searching {
                desc: D2,
                curknown: tenth(10),
                curscore: tenth(5),
                unexplored: Vec::new(),
            },
        ];

        assert_eq!(Searching::mscore_compare(&s1), MScoreComparison::Select(0));
    }

    #[test]
    fn test_certain_b() {
        use DummyDesc::*;

        let s1: Vec<Searching<DummyState, DummyDesc>> = vec![
            Searching {
                desc: D1,
                curknown: tenth(10),
                curscore: tenth(3),
                unexplored: Vec::new(),
            },
            Searching {
                desc: D2,
                curknown: tenth(10),
                curscore: tenth(5),
                unexplored: Vec::new(),
            },
            Searching {
                desc: D3,
                curknown: tenth(10),
                curscore: tenth(8),
                unexplored: Vec::new(),
            },
            Searching {
                desc: D4,
                curknown: tenth(10),
                curscore: tenth(1),
                unexplored: Vec::new(),
            },
        ];

        assert_eq!(Searching::mscore_compare(&s1), MScoreComparison::Select(2));
    }

    #[test]
    fn test_one_score() {
        use DummyDesc::*;

        let s1: Vec<Searching<DummyState, DummyDesc>> = vec![
            Searching {
                desc: D1,
                curknown: tenth(10),
                curscore: tenth(8),
                unexplored: Vec::new(),
            },
            Searching {
                desc: D2,
                curknown: tenth(10),
                curscore: tenth(5),
                unexplored: Vec::new(),
            },
        ];

        assert_eq!(Searching::mscore_compare(&s1), MScoreComparison::Select(0));
    }

    #[test]
    fn test_single_possibility() {
        use DummyDesc::*;
        use DummyState::*;
        let s1: Vec<Searching<DummyState, DummyDesc>> = to_searching(&[(D1, &[(10, S1)])]);
        assert_eq!(Searching::mscore_compare(&s1), MScoreComparison::Select(0));
    }

    #[test]
    fn all_unknown() {
        use DummyDesc::*;
        use DummyState::*;
        let s1: Vec<Searching<DummyState, DummyDesc>> =
            to_searching(&[(D1, &[(10, S1)]), (D2, &[(10, S2)]), (D3, &[(10, S3)])]);
        assert_eq!(Searching::mscore_compare(&s1), MScoreComparison::Search(0));
    }

    #[test]
    fn all_unknown_diff_proba() {
        use DummyDesc::*;
        use DummyState::*;
        let s1: Vec<Searching<DummyState, DummyDesc>> = to_searching(&[
            (D1, &[(6, S1), (4, S4)]),
            (D2, &[(8, S2), (2, S1)]),
            (D3, &[(5, S3), (5, S4)]),
        ]);
        assert_eq!(Searching::mscore_compare(&s1), MScoreComparison::Search(1));
    }

    #[test]
    fn domination_simple() {
        use DummyDesc::*;
        use DummyState::*;
        let s1: Vec<Searching<DummyState, DummyDesc>> = vec![
            Searching {
                desc: D1,
                curknown: tenth(10),
                curscore: tenth(8),
                unexplored: Vec::new(),
            },
            Searching {
                desc: D2,
                curknown: tenth(5),
                curscore: tenth(0),
                unexplored: to_outcome(&[(2, S1), (3, S2)]),
            },
        ];
        assert_eq!(Searching::mscore_compare(&s1), MScoreComparison::Select(0));
    }
}
