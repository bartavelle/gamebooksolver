use minicbor::decode::{Decoder, Error};
use minicbor::encode::{self, Encoder, Write};
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

pub trait Rational:
  Sized + Clone + Eq + Ord + std::fmt::Debug + std::hash::Hash + std::iter::Sum
{
  fn from_i64(n: i64, d: i64) -> Self;
  fn f_f64(i: f64) -> Self;
  fn to_f32(&self) -> f32;
  fn t_f64(&self) -> f64;
  fn cbor_decode(d: &mut Decoder) -> Result<Self, Error>;
  fn cbor_encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>>;
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
  fn cbor_decode(d: &mut Decoder) -> Result<Self, Error> {
    fn read_bigint(d: &mut Decoder) -> Result<BigInt, Error> {
      let tp = d.datatype()?;
      match tp {
        minicbor::data::Type::Tag => {
          let tg = d.tag()?;
          match tg {
            minicbor::data::Tag::PosBignum => Ok(BigInt::from_signed_bytes_be(d.bytes()?)),
            _ => Err(Error::Message("bad tag for bignum")),
          }
        }
        _ => d.u64().map(BigInt::from),
      }
    }
    let ln = d.array()?;
    if ln != Some(2) {
      return Err(Error::Message("Invalid length for pair"));
    }
    let nu = read_bigint(d)?;
    let de = read_bigint(d)?;
    Ok(BigRational::new(nu, de))
  }
  fn cbor_encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    fn e_bigint<W: Write>(e: &mut Encoder<W>, i: &BigInt) -> Result<(), encode::Error<W::Error>> {
      e.tag(minicbor::data::Tag::PosBignum)?
        .bytes(&i.to_signed_bytes_be())?;
      Ok(())
    }
    e.array(2)?;
    e_bigint(e, self.numer())?;
    e_bigint(e, self.denom())?;
    Ok(())
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
  fn cbor_decode(d: &mut Decoder) -> Result<Self, Error> {
    fn read_bigint(d: &mut Decoder) -> Result<rug::Integer, Error> {
      let tp = d.datatype()?;
      match tp {
        minicbor::data::Type::Tag => {
          let tg = d.tag()?;
          match tg {
            minicbor::data::Tag::PosBignum => Ok(rug::Integer::from_digits(
              d.bytes()?,
              rug::integer::Order::Msf,
            )),
            _ => Err(Error::Message("bad tag for bignum")),
          }
        }
        _ => d.u64().map(rug::Integer::from),
      }
    }
    let ln = d.array()?;
    if ln != Some(2) {
      return Err(Error::Message("Invalid length for pair"));
    }
    let nu = read_bigint(d)?;
    let de = read_bigint(d)?;
    Ok(rug::Rational::from((nu, de)))
  }
  fn cbor_encode<W: Write>(&self, e: &mut Encoder<W>) -> Result<(), encode::Error<W::Error>> {
    fn e_bigint<W: Write>(
      e: &mut Encoder<W>,
      i: &rug::Integer,
    ) -> Result<(), encode::Error<W::Error>> {
      e.tag(minicbor::data::Tag::PosBignum)?
        .bytes(&i.to_digits(rug::integer::Order::Msf))?;
      Ok(())
    }
    e.array(2)?;
    e_bigint(e, self.numer())?;
    e_bigint(e, self.denom())?;
    Ok(())
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

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn t_rational() {
    let r: BigRational = Rational::from_i64(5615165615_i64, 8122123_i64);
    let mut buffer = [0u8; 128];
    let mut e = minicbor::encode::Encoder::new(&mut buffer as &mut [u8]);
    r.cbor_encode(&mut e).unwrap();
    let mut d = minicbor::decode::Decoder::new(&buffer);
    let r2 = BigRational::cbor_decode(&mut d).unwrap();
    assert_eq!(r, r2);
  }

  #[test]
  #[cfg(feature = "rug")]
  fn t_interop_a() {
    let r: BigRational = Rational::from_i64(5615165615_i64, 8122123_i64);
    let expected: rug::Rational = Rational::from_i64(5615165615_i64, 8122123_i64);
    let mut buffer = [0u8; 128];
    let mut e = minicbor::encode::Encoder::new(&mut buffer as &mut [u8]);
    r.cbor_encode(&mut e).unwrap();
    let mut d = minicbor::decode::Decoder::new(&buffer);
    let r2 = rug::Rational::cbor_decode(&mut d).unwrap();
    assert_eq!(r2, expected);
  }

  #[test]
  #[cfg(feature = "rug")]
  fn t_interop_b() {
    let r: rug::Rational = Rational::from_i64(5615165615_i64, 8122123_i64);
    let expected: BigRational = Rational::from_i64(5615165615_i64, 8122123_i64);
    let mut buffer = [0u8; 128];
    let mut e = minicbor::encode::Encoder::new(&mut buffer as &mut [u8]);
    r.cbor_encode(&mut e).unwrap();
    let mut d = minicbor::decode::Decoder::new(&buffer);
    let r2 = BigRational::cbor_decode(&mut d).unwrap();
    assert_eq!(r2, expected);
  }
}
