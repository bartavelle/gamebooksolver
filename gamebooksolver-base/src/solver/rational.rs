use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::cast::FromPrimitive;
use num_traits::cast::ToPrimitive;
use num_traits::identities::Zero;
use serde::{Deserialize, Serialize};

// representation of rationals as json
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct JRatio {
    radix: i32,
    value: String,
}

impl From<&BigRational> for JRatio {
    fn from(r: &BigRational) -> Self {
        JRatio {
            radix: 10,
            value: format!("{}/{}", r.numer(), r.denom()),
        }
    }
}

pub trait Rational: Sized + Clone + Eq + Ord + std::fmt::Debug + std::hash::Hash + std::iter::Sum {
    fn from_i64(n: i64, d: i64) -> Self;
    fn f_f64(i: f64) -> Self;
    fn to_f32(&self) -> f32;
    fn t_f64(&self) -> f64;
    fn mul(self, other: &Self) -> Self;
    fn div(self, other: &Self) -> Self;
    fn add(self, other: &Self) -> Self;
    fn sub(self, other: &Self) -> Self;
    fn is_z(&self) -> bool;
    fn from_jratio(r: &JRatio) -> Self;
}

impl Rational for BigRational {
    fn from_i64(n: i64, d: i64) -> Self {
        BigRational::new(BigInt::from(n), BigInt::from(d))
    }
    fn f_f64(i: f64) -> Self {
        BigRational::from_f64(i).unwrap()
    }
    fn to_f32(&self) -> f32 {
        self.t_f64() as f32
    }
    fn t_f64(&self) -> f64 {
        self.to_f64().unwrap()
    }
    fn mul(self, other: &Self) -> Self {
        self * other
    }
    fn add(self, other: &Self) -> Self {
        self + other
    }
    fn div(self, other: &Self) -> Self {
        self / other
    }
    fn sub(self, other: &Self) -> Self {
        self - other
    }
    fn is_z(&self) -> bool {
        self.is_zero()
    }
    fn from_jratio(r: &JRatio) -> Self {
        if r.radix != 10 {
            panic!("only support radix=10 for now");
        }
        let splt = r.value.split('/').collect::<Vec<_>>();
        match splt.as_slice() {
            [sn, sd] => {
                let n = sn.parse().unwrap();
                let d = sd.parse().unwrap();
                BigRational::new(n, d)
            }
            _ => panic!("invalid string {}", r.value),
        }
    }
}

#[cfg(feature = "rug")]
impl Rational for rug::Rational {
    fn from_i64(n: i64, d: i64) -> Self {
        rug::Rational::from((n, d))
    }
    fn f_f64(i: f64) -> Self {
        rug::Rational::from_f64(i).unwrap()
    }
    fn to_f32(&self) -> f32 {
        self.t_f64() as f32
    }
    fn t_f64(&self) -> f64 {
        self.to_f64()
    }
    fn mul(self, other: &Self) -> Self {
        self * other
    }
    fn add(self, other: &Self) -> Self {
        self + other
    }
    fn div(self, other: &Self) -> Self {
        self / other
    }
    fn sub(self, other: &Self) -> Self {
        self - other
    }
    fn is_z(&self) -> bool {
        self == &0.0
    }
    fn from_jratio(r: &JRatio) -> Self {
        rug::Rational::from_str_radix(&r.value, r.radix).unwrap()
    }
}

#[cfg(feature = "rug")]
pub mod r {
    use bincode::{BorrowDecode, Decode, Encode};

    use super::*;

    #[repr(transparent)]
    #[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
    #[serde(transparent)]
    pub struct MRational {
        inner: rug::Rational,
    }

    impl Encode for MRational {
        fn encode<E: bincode::enc::Encoder>(&self, encoder: &mut E) -> Result<(), bincode::error::EncodeError> {
            Encode::encode(&bincode::serde::Compat(&self.inner), encoder)
        }
    }

    impl<'de, CTX> BorrowDecode<'de, CTX> for MRational {
        fn borrow_decode<D: bincode::de::BorrowDecoder<'de, Context = CTX>>(
            decoder: &mut D,
        ) -> Result<Self, bincode::error::DecodeError> {
            bincode::serde::BorrowCompat::borrow_decode(decoder).map(|x| Self { inner: x.0 })
        }
    }

    impl<CTX> Decode<CTX> for MRational {
        fn decode<D: bincode::de::Decoder<Context = CTX>>(
            decoder: &mut D,
        ) -> Result<Self, bincode::error::DecodeError> {
            bincode::serde::Compat::decode(decoder).map(|x| Self { inner: x.0 })
        }
    }

    impl MRational {
        pub fn to_f64(&self) -> f64 {
            self.inner.to_f64()
        }

        /// The exact decimal representation, with halves rounded to the even digit
        pub fn to_fixed(&self, decimals: u32) -> String {
            let scale = rug::Integer::from(rug::Integer::u_pow_u(10, decimals));
            let exact = self.inner.clone().abs() * scale;
            let tie = *exact.denom() == 2;
            // ties are rounded away from zero here
            let rounded = exact.round().into_numer_denom().0;
            let scaled = if tie && rounded.is_odd() { rounded - 1 } else { rounded };
            let digits = format!("{scaled:0>width$}", width = decimals as usize + 1);
            let (int, frac) = digits.split_at(digits.len() - decimals as usize);
            let sign = if self.inner < 0 { "-" } else { "" };
            if decimals == 0 {
                format!("{sign}{int}")
            } else {
                format!("{sign}{int}.{frac}")
            }
        }
    }

    impl std::fmt::Debug for MRational {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.inner.fmt(f)
        }
    }

    impl std::fmt::Display for MRational {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.inner.fmt(f)
        }
    }

    impl From<i64> for MRational {
        fn from(value: i64) -> Self {
            Self {
                inner: rug::Rational::from(value),
            }
        }
    }

    impl std::ops::Add<&MRational> for MRational {
        type Output = Self;

        fn add(self, rhs: &MRational) -> Self::Output {
            Self {
                inner: self.inner + &rhs.inner,
            }
        }
    }

    impl std::ops::Mul<&MRational> for MRational {
        type Output = Self;

        fn mul(self, rhs: &MRational) -> Self::Output {
            Self {
                inner: self.inner * &rhs.inner,
            }
        }
    }

    impl std::ops::AddAssign for MRational {
        fn add_assign(&mut self, rhs: Self) {
            self.inner += rhs.inner;
        }
    }

    impl std::iter::Sum for MRational {
        fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
            iter.fold(Self::default(), |a, b| a + &b)
        }
    }

    impl Rational for MRational {
        fn from_i64(n: i64, d: i64) -> Self {
            Self {
                inner: rug::Rational::from((n, d)),
            }
        }
        fn f_f64(i: f64) -> Self {
            Self {
                inner: rug::Rational::from_f64(i).unwrap(),
            }
        }
        fn to_f32(&self) -> f32 {
            self.inner.t_f64() as f32
        }
        fn t_f64(&self) -> f64 {
            self.inner.to_f64()
        }
        fn mul(self, other: &Self) -> Self {
            self * other
        }
        fn add(self, other: &Self) -> Self {
            Self {
                inner: self.inner + &other.inner,
            }
        }
        fn div(self, other: &Self) -> Self {
            Self {
                inner: self.inner / &other.inner,
            }
        }
        fn sub(self, other: &Self) -> Self {
            Self {
                inner: self.inner - &other.inner,
            }
        }
        fn is_z(&self) -> bool {
            self.inner == 0.0
        }
        fn from_jratio(r: &JRatio) -> Self {
            Self {
                inner: rug::Rational::from_str_radix(&r.value, r.radix).unwrap(),
            }
        }
    }
}

// the alignment is lowered so that the solver only uses 10 bytes for a score and a choice
#[repr(packed(2))]
#[derive(Clone, Copy, PartialEq, Default, Debug)]
pub struct MF64(f64);

impl Serialize for MF64 {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let v = self.0;
        v.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for MF64 {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        f64::deserialize(deserializer).map(MF64)
    }
}

impl std::iter::Sum for MF64 {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::default(), |a, b| MF64(a.0 + b.0))
    }
}

impl std::hash::Hash for MF64 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.to_le_bytes()).hash(state);
    }
}

impl Eq for MF64 {}

impl Ord for MF64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let (a, b) = (self.0, other.0);
        match a.partial_cmp(&b) {
            Some(x) => x,
            None => a.to_le_bytes().cmp(&b.to_le_bytes()),
        }
    }
}

impl PartialOrd for MF64 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Rational for MF64 {
    fn from_i64(n: i64, d: i64) -> Self {
        MF64(n as f64 / d as f64)
    }

    fn f_f64(i: f64) -> Self {
        MF64(i)
    }

    fn to_f32(&self) -> f32 {
        self.0 as f32
    }

    fn t_f64(&self) -> f64 {
        self.0
    }

    fn mul(self, other: &Self) -> Self {
        MF64(self.0 * other.0)
    }

    fn div(self, other: &Self) -> Self {
        MF64(self.0 / other.0)
    }

    fn add(self, other: &Self) -> Self {
        MF64(self.0 + other.0)
    }

    fn sub(self, other: &Self) -> Self {
        MF64(self.0 - other.0)
    }

    fn is_z(&self) -> bool {
        self.0 == 0.0
    }

    fn from_jratio(r: &JRatio) -> Self {
        MF64(BigRational::from_jratio(r).to_f64().unwrap_or_default())
    }
}

#[cfg(all(test, feature = "rug"))]
mod tests {
    use super::Rational;
    use super::r::MRational;

    #[test]
    fn to_fixed() {
        let fixed = |n, d, decimals| MRational::from_i64(n, d).to_fixed(decimals);
        assert_eq!(fixed(1, 1, 3), "1.000");
        assert_eq!(fixed(1, 3, 2), "0.33");
        assert_eq!(fixed(2, 3, 2), "0.67");
        assert_eq!(fixed(1, 2000, 3), "0.000");
        assert_eq!(fixed(1, 1000, 3), "0.001");
        assert_eq!(fixed(3053, 400, 3), "7.632");
        assert_eq!(fixed(3, 2000, 3), "0.002");
        assert_eq!(fixed(-1, 4, 1), "-0.2");
        assert_eq!(fixed(5, 2, 0), "2");
    }
}
