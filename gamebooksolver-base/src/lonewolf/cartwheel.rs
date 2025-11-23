use crate::solver::base::Outcome;
use crate::solver::base::Proba;
use crate::solver::rational::Rational;

use lazy_static::lazy_static;

pub static MAX_CARTWHEEL: u8 = 30;
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

#[derive(Debug, PartialEq)]
struct Matrix {
    sx: usize,
    sy: usize,
    content: Vec<f64>,
}

impl Matrix {
    fn new(sx: usize, sy: usize) -> Self {
        Matrix {
            sx,
            sy,
            content: std::iter::repeat_n(0.0, sx * sy).collect(),
        }
    }

    #[cfg(test)]
    fn from_slice(sx: usize, sy: usize, sl: &[f64]) -> Self {
        assert_eq!(sx * sy, sl.len());
        Matrix {
            sx,
            sy,
            content: sl.to_vec(),
        }
    }

    fn set(&mut self, x: usize, y: usize, v: f64) {
        assert!(x < self.sx);
        assert!(y < self.sy);
        self.content[x + y * self.sx] = v;
    }

    fn get(&self, x: usize, y: usize) -> f64 {
        assert!(x < self.sx);
        assert!(y < self.sy);
        self.content[x + y * self.sx]
    }

    fn mul(&self, other: &Self) -> Self {
        assert_eq!(self.sx, other.sy);

        let calc = |idx| {
            let x = idx % other.sx;
            let y = idx / other.sx;

            let mut o = 0.0;
            for i in 0..self.sx {
                o += self.get(i, y) * other.get(x, i);
            }

            o
        };

        Matrix {
            sx: other.sx,
            sy: self.sy,
            content: (0..other.sx * self.sy).map(calc).collect(),
        }
    }

    fn square(&self) -> Self {
        self.mul(self)
    }
}

fn sol_for_target(target: usize) -> Vec<Outcome<f64, u8>> {
    let mut o = Matrix::new(target + 7, target + 7);
    // if money == 0, we lose
    o.set(0, 0, 1.0);
    // for money < target
    for curmoney in 1..target {
        o.set(curmoney, curmoney - 1, 0.7);
        o.set(curmoney, curmoney + 4, 0.2);
        o.set(curmoney, curmoney + 7, 0.1);
    }
    // once we reach target, we are done
    for curmoney in target..target + 7 {
        o.set(curmoney, curmoney, 1.0);
    }

    for _ in 0..12 {
        o = o.square()
    }

    (0..target + 1)
        .map(|start_money| {
            (0..target + 7)
                .map(|end_money| Proba {
                    p: o.get(start_money, end_money),
                    v: end_money as u8,
                })
                .filter(|pb| pb.p != 0.0)
                .collect()
        })
        .collect()
}

lazy_static! {
    static ref CARTWHEEL: Vec<Vec<Outcome<f64, u8>>> = (20..50).map(sol_for_target).collect();
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
                p: P::f_f64(pb.p),
            })
            .collect()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn matrix_mul_trivial() {
        let m1 = Matrix::from_slice(2, 2, &[1.0, 2.0, 3.0, 4.0]);
        let m2 = Matrix::from_slice(2, 2, &[5.0, 6.0, 7.0, 8.0]);
        let expected = Matrix::from_slice(2, 2, &[19.0, 22.0, 43.0, 50.0]);

        let mr = m1.mul(&m2);

        assert_eq!(mr, expected);
    }

    #[test]
    fn matrix_mul_nonsquare() {
        let m1 = Matrix::from_slice(2, 3, &[1.0, 2.0, 3.0, 4.0, 7.0, 7.0]);
        let m2 = Matrix::from_slice(2, 2, &[5.0, 6.0, 7.0, 8.0]);
        let expected = Matrix::from_slice(2, 3, &[19.0, 22.0, 43.0, 50.0, 84.0, 98.0]);

        let mr = m1.mul(&m2);

        assert_eq!(mr, expected);
    }

    #[test]
    fn matrix_squared() {
        let m = Matrix::from_slice(2, 2, &[5.0, 6.0, 7.0, 8.0]);
        let sq = m.square();
        let expected = Matrix::from_slice(2, 2, &[67.0, 78.0, 91.0, 106.0]);
        assert_eq!(sq, expected);
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
