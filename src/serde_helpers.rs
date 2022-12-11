use std::fmt::{self, Display, Formatter};
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::de::{self, Unexpected, EnumAccess, MapAccess, SeqAccess, DeserializeSeed, VariantAccess};
use serde::de::Error as de_Error;
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeMap, SerializeTuple, SerializeSeq, SerializeTupleVariant};
use either::{Either, Left,  Right};
use nameof::name_of;
use std::str::FromStr;
use crate::code::SHORT_STRING_VARIANT_INDEX;

pub fn serialize_none_u8<S>(none: u8, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if serializer.is_human_readable() {
        serializer.serialize_unit()
    } else {
        serializer.serialize_u8(none)
    }
}

pub fn deserialize_none_u8<'de, D>(none: u8, deserializer: D) -> Result<(), D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        <()>::deserialize(deserializer)
    } else {
        let d = u8::deserialize(deserializer)?;
        if d != none {
            let e: &str = &format!("{none}");
            Err(D::Error::invalid_value(Unexpected::Unsigned(d as u64), &e))
        } else {
            Ok(())
        }
    }
}

pub fn serialize_bool_u8<S>(v: bool, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if serializer.is_human_readable() {
        serializer.serialize_bool(v)
    } else {
        serializer.serialize_u8(v.into())
    }
}

pub fn deserialize_bool_u8<'de, D>(deserializer: D) -> Result<bool, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        bool::deserialize(deserializer)
    } else {
        let d = u8::deserialize(deserializer)?;
        match d {
            0 => Ok(false),
            1 => Ok(true),
            d => Err(D::Error::invalid_value(Unexpected::Unsigned(d as u64), &"0 or 1"))
        }
    }
}

pub fn serialize_bool_u32<S>(v: bool, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if serializer.is_human_readable() {
        serializer.serialize_bool(v)
    } else {
        serializer.serialize_u32(v.into())
    }
}

pub fn deserialize_bool_u32<'de, D>(deserializer: D) -> Result<bool, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        bool::deserialize(deserializer)
    } else {
        let d = u32::deserialize(deserializer)?;
        match d {
            0 => Ok(false),
            1 => Ok(true),
            d => Err(D::Error::invalid_value(Unexpected::Unsigned(d as u64), &"0 or 1"))
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

pub fn serialize_option_index<I: Copy + Eq + Display, T: Copy, S>(
    name: &str, none: I, from: impl Fn(I) -> Option<T>, to: impl Fn(T) -> I, v: Either<Option<I>, T>, serializer: S
) -> Result<S::Ok, S::Error> where S: Serializer, I: Serialize, T: Serialize {
    if serializer.is_human_readable() {
        match v {
            Left(i) => OptionIndexHRSurrogate::None(i),
            Right(v) => OptionIndexHRSurrogate::Some(v),
        }.serialize(serializer)
    } else {
        let v = match v {
            Left(Some(i)) => {
                if i == none || from(i).is_some() {
                    let err = format!("{i} is not valid undefined {name} value");
                    return Err(S::Error::custom(err));
                }
                i
            },
            Left(None) => none,
            Right(a) => to(a)
        };
        v.serialize(serializer)
    }
}

pub fn deserialize_option_index<'de, I: Copy + Eq, T: Copy, D>(
    none: I, from: impl Fn(I) -> Option<T>, deserializer: D
) -> Result<Either<Option<I>, T>, D::Error> where D: Deserializer<'de>, I: Deserialize<'de>, T: Deserialize<'de> {
    if deserializer.is_human_readable() {
        let v = <OptionIndexHRSurrogate<I, T>>::deserialize(deserializer)?;
        Ok(match v {
            OptionIndexHRSurrogate::None(i) => Left(i),
            OptionIndexHRSurrogate::Some(v) => Right(v),
        })
    } else {
        let d = I::deserialize(deserializer)?;
        if d == none { return Ok(Left(None)); }
        if let Some(a) = from(d) {
            Ok(Right(a))
        } else {
            Ok(Left(Some(d)))
        }
    }
}

struct F32Surrogate(f32);

impl Serialize for F32Surrogate {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_f32_as_is(self.0, serializer)
    }
}

impl<'de> Deserialize<'de> for F32Surrogate {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
        Ok(F32Surrogate(deserialize_f32_as_is(deserializer)?))
    }
}

pub fn serialize_f32_s_as_is<S>(v: &[f32], serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    let mut serializer = serializer.serialize_seq(Some(v.len()))?;
    for &f in v.iter() {
        serializer.serialize_element(&F32Surrogate(f))?;
    }
    serializer.end()
}

struct F32sDeserializer;

impl<'de> de::Visitor<'de> for F32sDeserializer {
    type Value = Vec<f32>;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "list of floats")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: SeqAccess<'de> {
        let mut vec = seq.size_hint().map_or_else(Vec::new, Vec::with_capacity);
        while let Some(f) = seq.next_element::<F32Surrogate>()? {
            vec.push(f.0);
        }
        Ok(vec)
    }
}

pub fn deserialize_f32_s_as_is<'de, D>(deserializer: D) -> Result<Vec<f32>, D::Error> where D: Deserializer<'de> {
    deserializer.deserialize_seq(F32sDeserializer)
}

pub fn serialize_f32_as_is<S>(v: f32, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if !serializer.is_human_readable() {
        serializer.serialize_f32(v)
    } else if v.is_nan() {
        let d: u32 = v.to_bits();
        if d == 0xFFFFFFFF {
            serializer.serialize_f32(v)
        } else {
            serializer.serialize_str(&format!("nan{d:08X}"))
        }
    } else {
        serializer.serialize_f64(f64::from_str(&v.to_string()).unwrap().copysign(v as f64))
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

pub fn deserialize_f32_as_is<'de, D>(deserializer: D) -> Result<f32, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        deserializer.deserialize_any(F32HRDeserializer)
    } else {
        f32::deserialize(deserializer)
    }
}

pub fn serialize_f64_as_is<S>(v: f64, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if !serializer.is_human_readable() {
        serializer.serialize_f64(v)
    } else if v.is_nan() {
        let d: u64 = v.to_bits();
        if d == 0xFFFFFFFFFFFFFFFF {
            serializer.serialize_f64(v)
        } else {
            serializer.serialize_str(&format!("nan{d:016X}"))
        }
    } else {
        serializer.serialize_f64(f64::from_str(&v.to_string()).unwrap().copysign(v as f64))
    }
}

struct F64HRDeserializer;

impl<'de> de::Visitor<'de> for F64HRDeserializer {
    type Value = f64;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "64-bit float value or 'nanXXXXXXXXXXXXXXXX' (where X is hex digit)")
    }

    fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E> where E: de::Error {
        Ok(if v.is_nan() { f64::from_bits(0xFFFFFFFFFFFFFFFFu64) } else { v })
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> where E: de::Error {
        if s.len() != 3 + 16 || !s.starts_with("nan") || &s[4..5] == "+" {
            return Err(E::invalid_value(Unexpected::Str(s), &self));
        }
        let d = u64::from_str_radix(&s[3..], 16).map_err(|_| E::invalid_value(Unexpected::Str(s), &self))?;
        if d == 0xFFFFFFFFFFFFFFFF {
            return Err(E::invalid_value(Unexpected::Str(s), &self));
        }
        Ok(f64::from_bits(d))
    }
}

pub fn deserialize_f64_as_is<'de, D>(deserializer: D) -> Result<f64, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        deserializer.deserialize_any(F64HRDeserializer)
    } else {
        f64::deserialize(deserializer)
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

pub fn serialize_short_string<S>(s: &str, len: usize, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if serializer.is_human_readable() {
        serializer.serialize_str(s)
    } else {
        let mut serializer = serializer.serialize_map(Some(1))?;
        serializer.serialize_entry(&ShortStr { string: s, len }, &())?;
        serializer.end()
    }
}

pub fn deserialize_short_string<'de, D>(len: usize, deserializer: D) -> Result<String, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        String::deserialize(deserializer)
    } else {
        Ok(deserializer.deserialize_map(ShortStringDeserializer { len })?.string)
    }
}

pub fn serialize_string_list<S>(lines: &[String], separator: &str, len: Option<usize>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if serializer.is_human_readable() {
        lines.serialize(serializer)
    } else {
        if separator.is_empty() {
            return Err(S::Error::custom("empty string list separator"));
        }
        if lines.is_empty() {
            return Err(S::Error::custom("\
                empty string list cannot be serialized bacause \
                it is indistinguishable from list with one empty string\
            "));
        }
        let mut capacity = 0;
        for line in lines {
            if line.contains(separator) {
                return Err(S::Error::custom("string list item contains separator"));
            }
            capacity += line.len() + separator.len();
        }
        let mut text = String::with_capacity(capacity);
        for line in lines.iter() {
            text.push_str(line);
            text.push_str(separator);
        }
        text.truncate(text.len() - separator.len());
        if let Some(len) = len {
            serialize_short_string(&text, len, serializer)
        } else {
            text.serialize(serializer)
        }
    }
}

pub fn deserialize_string_list<'de, D>(
    separator: &str,
    len: Option<usize>,
    deserializer: D
) -> Result<Vec<String>, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        <Vec<String>>::deserialize(deserializer)
    } else {
        if separator.is_empty() {
            return Err(D::Error::custom("empty string list separator"));
        }
        let s = if let Some(len) = len {
            deserialize_short_string(len, deserializer)?
        } else {
            String::deserialize(deserializer)?
        };
        Ok(s.split(separator).map(|x| x.into()).collect())
    }
}
