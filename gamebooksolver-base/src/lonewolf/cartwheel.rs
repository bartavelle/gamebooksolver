use crate::solver::base::Outcome;
use crate::solver::base::Proba;
use crate::solver::base::optimize_outcome;
use crate::solver::rational::{JRatio, Rational};

use lazy_static::lazy_static;
use num_rational::BigRational;
use num_traits::identities::{One, Zero};

pub static MAX_CARTWHEEL: u8 = 30;
pub static CARTWHEEL_FREE_BET: &str = "Cartwheel token only";
pub static CARTWHEEL_LABEL: &[&str] = &[
    "Cartwheel target 20",
    "Cartwheel target 21",
    "Cartwheel target 22",
    "Cartwheel target 23",
    "Cartwheel target 24",
    "Cartwheel target 25",
    "Cartwheel target 26",
    "Cartwheel target 27",
    "Cartwheel target 28",
    "Cartwheel target 29",
    "Cartwheel target 30",
    "Cartwheel target 31",
    "Cartwheel target 32",
    "Cartwheel target 33",
    "Cartwheel target 34",
    "Cartwheel target 35",
    "Cartwheel target 36",
    "Cartwheel target 37",
    "Cartwheel target 38",
    "Cartwheel target 39",
    "Cartwheel target 40",
    "Cartwheel target 41",
    "Cartwheel target 42",
    "Cartwheel target 43",
    "Cartwheel target 44",
    "Cartwheel target 45",
    "Cartwheel target 46",
    "Cartwheel target 47",
    "Cartwheel target 48",
    "Cartwheel target 49",
];

/// The exact chance of ending with each amount of gold, when playing with one crown each round until the gold is lost
/// or until it reaches the target, for each amount of gold below the target. A game can last for any number of rounds,
/// but these chances are the solution of a system of linear equations, and so are rational numbers: writing x(m) for
/// the chance of ending with a given amount when holding m crowns, x(m) = 7/10 x(m-1) + 2/10 x(m+5) + 1/10 x(m+8).
fn sol_for_target(target: usize) -> Vec<Outcome<JRatio, u8>> {
    // the amounts that end a game, that come after the amounts from 1 to target - 1 in the columns
    let ends: Vec<usize> = std::iter::once(0).chain(target..target + 8).collect();
    let unknowns = target - 1;
    // what is gained in a round, nothing standing for the lost crown, and its chance in tenths
    let round = [(0, 7), (5, 2), (8, 1)];
    let mut rows = vec![vec![BigRational::zero(); unknowns + ends.len()]; unknowns];
    for money in 1..target {
        let row = &mut rows[money - 1];
        row[money - 1] = BigRational::one();
        for (gain, tenths) in round {
            let next = if gain == 0 { money - 1 } else { money + gain };
            let p = <BigRational as Rational>::from_i64(tenths, 10);
            if (1..target).contains(&next) {
                row[next - 1] -= p;
            } else {
                let end = ends.iter().position(|e| *e == next).expect("known end");
                row[unknowns + end] += p;
            }
        }
    }
    for col in 0..unknowns {
        let pivot = (col..unknowns)
            .find(|r| !rows[*r][col].is_zero())
            .expect("all games end, so there is a single solution");
        rows.swap(col, pivot);
        let scale = rows[col][col].clone();
        rows[col].iter_mut().for_each(|v| *v = &*v / &scale);
        let pivot_row = rows[col].clone();
        for (r, row) in rows.iter_mut().enumerate() {
            if r != col && !row[col].is_zero() {
                let factor = row[col].clone();
                row.iter_mut()
                    .zip(&pivot_row)
                    .for_each(|(v, pv)| *v = &*v - &factor * pv);
            }
        }
    }
    let lost = vec![Proba {
        p: JRatio::from(&BigRational::one()),
        v: 0,
    }];
    std::iter::once(lost)
        .chain(rows.iter().map(|row| {
            ends.iter()
                .zip(&row[unknowns..])
                .filter(|(_, p)| !p.is_zero())
                .map(|(end, p)| Proba {
                    p: JRatio::from(p),
                    v: *end as u8,
                })
                .collect()
        }))
        .collect()
}

lazy_static! {
    static ref CARTWHEEL: Vec<Vec<Outcome<JRatio, u8>>> = (20..50).map(sol_for_target).collect();
}

pub fn cartwheel<P: Rational>(starting_money: u8, target: u8) -> Outcome<P, u8> {
    if starting_money >= target {
        vec![Proba::certain(target)]
    } else if !(20..50).contains(&target) {
        panic!("Target must be >= 20 && < 50, was {}", target)
    } else {
        CARTWHEEL[target as usize - 20][starting_money as usize]
            .iter()
            .map(|pb| Proba {
                v: pb.v,
                p: P::from_jratio(&pb.p),
            })
            .collect()
    }
}

/// The final amounts of gold when arriving with this gold and the free silver token. It is played first, as the amount
/// of money evolves the same way with a token or a crown, and otherwise it could be left unused, and it can't be
/// cashed. A win pays like any other stake: the stake is kept as a crown. The game then goes on if the gold is below
/// the target.
pub fn cartwheel_with_token<P: Rational>(gold: u8, target: u8) -> Outcome<P, u8> {
    let mut out = Vec::new();
    for (gain, tenths) in [(0, 7), (6, 2), (9, 1)] {
        let money = gold + gain;
        let p = P::from_i64(tenths, 10);
        if money >= target {
            out.push(Proba { v: money, p });
        } else {
            out.extend(cartwheel::<P>(money, target).into_iter().map(|r| Proba {
                v: r.v,
                p: r.p.mul(&p),
            }));
        }
    }
    optimize_outcome(out)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::solver::rational::MF64;

    #[test]
    fn chances_are_exact() {
        let bust = cartwheel::<BigRational>(1, 20)
            .into_iter()
            .find(|r| r.v == 0)
            .unwrap()
            .p;
        let expected = BigRational::new(
            "3786146215199390399".parse().unwrap(),
            "5124287423038106270".parse().unwrap(),
        );
        assert_eq!(bust, expected);
        for target in [20, 30, 49] {
            let chance = |money: u8, end: u8| -> BigRational {
                if money >= target {
                    return if money == end {
                        BigRational::one()
                    } else {
                        BigRational::zero()
                    };
                }
                let o = cartwheel::<BigRational>(money, target);
                o.into_iter()
                    .find(|r| r.v == end)
                    .map(|r| r.p)
                    .unwrap_or_else(BigRational::zero)
            };
            let tenths = |n: i64| <BigRational as Rational>::from_i64(n, 10);
            for money in 1..target {
                let total: BigRational = cartwheel::<BigRational>(money, target).into_iter().map(|r| r.p).sum();
                assert_eq!(total, BigRational::one());
                for end in std::iter::once(0).chain(target..target + 8) {
                    let next = tenths(7) * chance(money - 1, end)
                        + tenths(2) * chance(money + 5, end)
                        + tenths(1) * chance(money + 8, end);
                    assert_eq!(chance(money, end), next, "{money} {target} {end}");
                }
            }
        }
    }

    #[test]
    fn the_token_is_played_first() {
        let tenths = |n: i64| <BigRational as Rational>::from_i64(n, 10);
        let get = |o: &Outcome<BigRational, u8>, v: u8| o.iter().find(|r| r.v == v).map(|r| r.p.clone());
        // a bet that can't lose any crown
        let free = cartwheel_with_token::<BigRational>(25, 25);
        assert_eq!(free.len(), 3);
        assert_eq!(get(&free, 25), Some(tenths(7)));
        assert_eq!(get(&free, 31), Some(tenths(2)));
        assert_eq!(get(&free, 34), Some(tenths(1)));
        // it is possible to stop at the fare with 19 crowns: a win ends the game, otherwise it goes on with 19 crowns
        let o = cartwheel_with_token::<BigRational>(19, 20);
        for r in cartwheel::<BigRational>(19, 20) {
            let win = match r.v {
                25 => tenths(2),
                28 => tenths(1),
                _ => tenths(0),
            };
            assert_eq!(get(&o, r.v).unwrap(), win + r.p * tenths(7), "{}", r.v);
        }
        let fare: BigRational = o.iter().filter(|r| r.v >= 20).map(|r| r.p.clone()).sum();
        assert!(fare.t_f64() > 0.99844 && fare.t_f64() < 0.99845, "{fare}");
        for (gold, target) in [(0, 20), (19, 20), (20, 21), (28, 30)] {
            let o = cartwheel_with_token::<BigRational>(gold, target);
            let total: BigRational = o.into_iter().map(|r| r.p).sum();
            assert_eq!(total, BigRational::one());
        }
        // without any gold, losing the token ends the game
        let o = cartwheel_with_token::<BigRational>(0, 20);
        assert!(get(&o, 0).unwrap() > tenths(7));
    }

    #[test]
    fn games_are_over_at_the_end() {
        for target in 20..50 {
            for money in 1..target {
                let o = cartwheel::<MF64>(money, target);
                assert!(o.iter().all(|r| r.v == 0 || r.v >= target), "{money} {target}: {o:?}");
                let total: f64 = o.iter().map(|r| r.p.t_f64()).sum();
                assert!((total - 1.0).abs() < 1e-12, "{money} {target}: {total}");
            }
        }
    }

    #[test]
    fn payout_is_a_profit() {
        // a crown on the right number wins 8, a crown on a neighbouring number wins 5, and the stake is kept
        let o = cartwheel::<MF64>(19, 20);
        for (expected, p) in [(24, 0.2), (27, 0.1)] {
            assert!(o.iter().any(|r| r.v == expected && r.p.t_f64() >= p), "{expected}: {o:?}");
        }
        assert!(o.iter().all(|r| r.v <= 27), "{o:?}");
    }

    // #[test]
    // fn cartwheel_t0() {
    //     let o1 = cartwheel(0, 30);
    //     assert_eq!(
    //         o1,
    //         vec![Proba {
    //             p: Rational::from(1),
    //             v: 0
    //         }]
    //     );
    // }

    // #[test]
    // fn cartwheel_t1() {
    //     let o1 = cartwheel(1, 30);
    //     let all_pb: Rational = Rational::sum(o1.iter().map(|p| &p.p)).into();
    //     assert!(Rational::from(1) - all_pb < (1, 10000000));
    //     let lose_pb = o1.iter().find(|p| p.v == 0).map(|pb| pb.p.clone()).unwrap();
    //     assert!(lose_pb > (75, 100));
    // }
}
