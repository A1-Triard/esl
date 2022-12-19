use std::fmt::{self, Display, Formatter};
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::de::{self, Unexpected, EnumAccess, MapAccess, SeqAccess, DeserializeSeed, VariantAccess};
use serde::de::Error as de_Error;
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeMap, SerializeTuple, SerializeSeq, SerializeTupleVariant};
use serde_serialize_seed::{SerializeSeed, ValueWithSeed};
use either::{Either, Left,  Right};
use nameof::name_of;
use std::str::FromStr;
use crate::code::SHORT_STRING_VARIANT_INDEX;

#[derive(Clone)]
pub struct VecSerde<T>(pub T);

impl<T: SerializeSeed + Clone> SerializeSeed for VecSerde<T> where T::Value: Sized {
    type Value = [T::Value];

    fn serialize<S: Serializer>(&self, value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        let mut serializer = serializer.serialize_seq(Some(value.len()))?;
        for item in value {
            serializer.serialize_element(&ValueWithSeed(item, self.0.clone()))?;
        }
        serializer.end()
    }
}

struct VecDeVisitor<T>(VecSerde<T>);

impl<'de, T: DeserializeSeed<'de> + Clone> de::Visitor<'de> for VecDeVisitor<T> where T::Value: Sized {
    type Value = Vec<T::Value>;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "vector")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: SeqAccess<'de> {
        let mut vec = seq.size_hint().map_or_else(Vec::new, Vec::with_capacity);
        while let Some(f) = seq.next_element_seed(self.0.0.clone())? {
            vec.push(f);
        }
        Ok(vec)
    }
}

impl<'de, T: DeserializeSeed<'de> + Clone> DeserializeSeed<'de> for VecSerde<T> where T::Value: Sized {
    type Value = Vec<T::Value>;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        deserializer.deserialize_seq(VecDeVisitor(self))
    }
}

pub struct NoneU8Serde { pub none: u8 }

impl SerializeSeed for NoneU8Serde {
    type Value = ();

    fn serialize<S: Serializer>(&self, _value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            serializer.serialize_unit()
        } else {
            serializer.serialize_u8(self.none)
        }
    }
}

impl<'de> DeserializeSeed<'de> for NoneU8Serde {
    type Value = ();

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            <()>::deserialize(deserializer)
        } else {
            let d = u8::deserialize(deserializer)?;
            if d != self.none {
                let e: &str = &format!("{}", self.none);
                Err(D::Error::invalid_value(Unexpected::Unsigned(d as u64), &e))
            } else {
                Ok(())
            }
        }
    }
}

pub struct BoolU8Serde;

impl SerializeSeed for BoolU8Serde {
    type Value = bool;

    fn serialize<S: Serializer>(&self, &value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            serializer.serialize_bool(value)
        } else {
            serializer.serialize_u8(value.into())
        }
    }
}

impl<'de> DeserializeSeed<'de> for BoolU8Serde {
    type Value = bool;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            Ok(bool::deserialize(deserializer)?)
        } else {
            let d = u8::deserialize(deserializer)?;
            match d {
                0 => Ok(false),
                1 => Ok(true),
                d => Err(D::Error::invalid_value(Unexpected::Unsigned(d as u64), &"0 or 1"))
            }
        }
    }
}

pub struct BoolU32Serde;

impl SerializeSeed for BoolU32Serde {
    type Value = bool;

    fn serialize<S: Serializer>(&self, &value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            serializer.serialize_bool(value)
        } else {
            serializer.serialize_u32(value.into())
        }
    }
}

impl<'de> DeserializeSeed<'de> for BoolU32Serde {
    type Value = bool;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            Ok(bool::deserialize(deserializer)?)
        } else {
            let d = u32::deserialize(deserializer)?;
            match d {
                0 => Ok(false),
                1 => Ok(true),
                d => Err(D::Error::invalid_value(Unexpected::Unsigned(d as u64), &"0 or 1"))
            }
        }
    }
}

pub struct OptionIndexSerde<
    I: Copy + Eq + Display,
    T: Copy,
    From: Fn(I) -> Option<T>,
    Into: Fn(T) -> I,
> {
    pub name: &'static str,
    pub none: I,
    pub from: From,
    pub into: Into,
}

impl<
    I: Copy + Eq + Display + Serialize,
    T: Copy + Serialize,
    From: Fn(I) -> Option<T>,
    Into: Fn(T) -> I,
> SerializeSeed for OptionIndexSerde<I, T, From, Into> {
    type Value = Either<Option<I>, T>;

    fn serialize<S: Serializer>(&self, &value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            match value {
                Left(i) => OptionIndexHRSurrogate::None(i),
                Right(v) => OptionIndexHRSurrogate::Some(v),
            }.serialize(serializer)
        } else {
            let v = match value {
                Left(Some(i)) => {
                    if i == self.none || (self.from)(i).is_some() {
                        let err = format!("{} is not valid undefined {} value", i, self.name);
                        return Err(S::Error::custom(err));
                    }
                    i
                },
                Left(None) => self.none,
                Right(a) => (self.into)(a)
            };
            v.serialize(serializer)
        }
    }
}

impl<
    'de,
    I: Copy + Eq + Display + Deserialize<'de>,
    T: Copy + Deserialize<'de>,
    From: Fn(I) -> Option<T>,
    Into: Fn(T) -> I,
> DeserializeSeed<'de> for OptionIndexSerde<I, T, From, Into> {
    type Value = Either<Option<I>, T>;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            let v = <OptionIndexHRSurrogate<I, T>>::deserialize(deserializer)?;
            Ok(match v {
                OptionIndexHRSurrogate::None(i) => Left(i),
                OptionIndexHRSurrogate::Some(v) => Right(v),
            })
        } else {
            let d = I::deserialize(deserializer)?;
            if d == self.none { return Ok(Left(None)); }
            if let Some(a) = (self.from)(d) {
                Ok(Right(a))
            } else {
                Ok(Left(Some(d)))
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
#[serde(rename="OptionIndex")]
enum OptionIndexHRSurrogate<I, T> {
    None(Option<I>),
    Some(T)
}

pub struct F32sAsIsSerde;

impl SerializeSeed for F32sAsIsSerde {
    type Value = [f32];

    fn serialize<S>(&self, value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        let mut serializer = serializer.serialize_seq(Some(value.len()))?;
        for f in value.iter() {
            serializer.serialize_element(&ValueWithSeed(f, F32AsIsSerde))?;
        }
        serializer.end()
    }
}

impl<'de> DeserializeSeed<'de> for F32sAsIsSerde {
    type Value = Vec<f32>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where D: Deserializer<'de> {
        deserializer.deserialize_seq(F32sDeserializer)
    }
}

struct F32sDeserializer;

impl<'de> de::Visitor<'de> for F32sDeserializer {
    type Value = Vec<f32>;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "list of floats")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: SeqAccess<'de> {
        let mut vec = seq.size_hint().map_or_else(Vec::new, Vec::with_capacity);
        while let Some(f) = seq.next_element_seed(F32AsIsSerde)? {
            vec.push(f);
        }
        Ok(vec)
    }
}

pub struct F32AsIsSerde;

impl SerializeSeed for F32AsIsSerde {
    type Value = f32;

    fn serialize<S>(&self, &value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if !serializer.is_human_readable() {
            serializer.serialize_f32(value)
        } else if value.is_nan() {
            let d: u32 = value.to_bits();
            if d == 0xFFFFFFFF {
                serializer.serialize_f32(value)
            } else {
                serializer.serialize_str(&format!("nan{d:08X}"))
            }
        } else {
            serializer.serialize_f64(f64::from_str(&value.to_string()).unwrap().copysign(value as f64))
        }
    }
}

impl<'de> DeserializeSeed<'de> for F32AsIsSerde {
    type Value = f32;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            deserializer.deserialize_any(F32HRDeserializer)
        } else {
            Ok(f32::deserialize(deserializer)?)
        }
    }
}

struct F32HRDeserializer;

impl<'de> de::Visitor<'de> for F32HRDeserializer {
    type Value = f32;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "32-bit float value or 'nanXXXXXXXX' (where X is hex digit)")
    }

    fn visit_f32<E>(self, v: f32) -> Result<Self::Value, E> where E: de::Error {
        Ok(if v.is_nan() { f32::from_bits(0xFFFFFFFFu32) } else { v })
    }

    fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E> where E: de::Error {
        let v_as_f32 = v as f32;
        if !v_as_f32.is_nan() || !v.is_nan() {
            let v_as_u64: u64 = v.to_bits();
            let v_as_u32_as_u64: u64 = f64::from_str(&v_as_f32.to_string()).unwrap().copysign(v_as_f32 as f64).to_bits();
            if v_as_u32_as_u64 != v_as_u64 {
                return Err(E::invalid_value(Unexpected::Float(v), &self));
            }
        }
        self.visit_f32(v_as_f32)
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> where E: de::Error {
        if s.len() != 3 + 8 || !s.starts_with("nan") || &s[4..5] == "+" {
            return Err(E::invalid_value(Unexpected::Str(s), &self));
        }
        let d = u32::from_str_radix(&s[3..], 16).map_err(|_| E::invalid_value(Unexpected::Str(s), &self))?;
        if d == 0xFFFFFFFF {
            return Err(E::invalid_value(Unexpected::Str(s), &self));
        }
        Ok(f32::from_bits(d))
    }
}

struct Zeroes {
    len: usize
}

impl Serialize for Zeroes {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut serializer = serializer.serialize_tuple(self.len)?;
        for _ in 0 .. self.len {
            serializer.serialize_element(&0u8)?;
        }
        serializer.end()
    }
}

struct ZeroesDeserializer {
    len: usize
}

impl<'de> DeserializeSeed<'de> for ZeroesDeserializer {
    type Value = Zeroes;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        deserializer.deserialize_tuple(self.len, self)
    }
}

impl<'de> de::Visitor<'de> for ZeroesDeserializer {
    type Value = Zeroes;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} zeroes", self.len)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        for _ in 0 .. self.len {
            let zero: u8 = seq.next_element()?.unwrap();
            if zero != 0 {
                return Err(A::Error::invalid_value(Unexpected::Unsigned(zero.into()), &"0"));
            }
        }
        Ok(Zeroes { len: self.len })
    }
}

struct ShortStr<'a> {
    string: &'a str,
    len: usize,
}

struct ShortString {
    string: String,
    #[allow(dead_code)]
    len: usize,
}

impl<'a> Serialize for ShortStr<'a> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut serializer = serializer.serialize_tuple_variant(name_of!(type ShortStr), SHORT_STRING_VARIANT_INDEX, "", 2)?;
        serializer.serialize_field(&Zeroes { len: self.len })?;
        serializer.serialize_field(self.string)?;
        serializer.end()
    }
}

#[derive(Clone)]
struct ShortStringDeserializer { len: usize }

impl<'de> DeserializeSeed<'de> for ShortStringDeserializer {
    type Value = ShortString;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        deserializer.deserialize_enum(name_of!(type ShortStr), &[""], self)
    }
}

impl<'de> de::Visitor<'de> for ShortStringDeserializer {
    type Value = ShortString;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} character string", self.len)
    }

    fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<Self::Value, A::Error> {
        let (variant_index, variant): (u32, _) = data.variant()?;
        if variant_index != SHORT_STRING_VARIANT_INDEX {
            return Err(A::Error::unknown_variant(&variant_index.to_string(), &[""]));
        }
        variant.tuple_variant(2, self)
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
        if seq.next_element_seed(ZeroesDeserializer { len: self.len })?.is_none() {
            return Err(A::Error::missing_field("0"));
        }
        let Some(string): Option<String> = seq.next_element()? else {
            return Err(A::Error::missing_field("1"));
        };
        Ok(ShortString { string, len: self.len })
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
        let Some(key) = map.next_key_seed(self.clone())? else {
            return Err(A::Error::custom("missing map entry"));
        };
        let _: () = map.next_value()?;
        if map.next_key_seed(self)?.is_some() {
            return Err(A::Error::custom("extra map entry"));
        }
        Ok(key)
    }
}

pub struct ShortStringSerde {
    pub len: usize
}

impl SerializeSeed for ShortStringSerde {
    type Value = str;

    fn serialize<S: Serializer>(&self, value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            serializer.serialize_str(value)
        } else {
            let mut serializer = serializer.serialize_map(Some(1))?;
            serializer.serialize_entry(&ShortStr { string: value, len: self.len }, &())?;
            serializer.end()
        }
    }
}

impl<'de> DeserializeSeed<'de> for ShortStringSerde {
    type Value = String;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            String::deserialize(deserializer)
        } else {
            Ok(deserializer.deserialize_map(ShortStringDeserializer { len: self.len })?.string)
        }
    }
}

pub struct StringListSerde {
    pub separator: &'static str,
    pub len: Option<usize>
}

impl SerializeSeed for StringListSerde {
    type Value = [String];

    fn serialize<S: Serializer>(&self, value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            value.serialize(serializer)
        } else {
            if self.separator.is_empty() {
                return Err(S::Error::custom("empty string list separator"));
            }
            if value.is_empty() {
                return Err(S::Error::custom("\
                    empty string list cannot be serialized bacause \
                    it is indistinguishable from list with one empty string\
                "));
            }
            let mut capacity = 0;
            for line in value {
                if line.contains(self.separator) {
                    return Err(S::Error::custom("string list item contains separator"));
                }
                capacity += line.len() + self.separator.len();
            }
            let mut text = String::with_capacity(capacity);
            for line in value.iter() {
                text.push_str(line);
                text.push_str(self.separator);
            }
            text.truncate(text.len() - self.separator.len());
            if let Some(len) = self.len {
                ValueWithSeed(text.as_str(), ShortStringSerde { len }).serialize(serializer)
            } else {
                text.serialize(serializer)
            }
        }
    }
}

impl<'de> DeserializeSeed<'de> for StringListSerde {
    type Value = Vec<String>;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            <Vec<String>>::deserialize(deserializer)
        } else {
            if self.separator.is_empty() {
                return Err(D::Error::custom("empty string list separator"));
            }
            let s = if let Some(len) = self.len {
                ShortStringSerde { len }.deserialize(deserializer)?
            } else {
                String::deserialize(deserializer)?
            };
            Ok(s.split(self.separator).map(|x| x.into()).collect())
        }
    }
}
