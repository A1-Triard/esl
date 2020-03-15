use std::fmt::{self, Display};
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::de::{self, Unexpected, SeqAccess};
use serde::de::Error as de_Error;
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeTuple, SerializeSeq};
use std::mem::{transmute};
use either::{Either, Left,  Right};
use std::str::FromStr;

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
            let e: &str = &format!("{}", none);
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
        serializer.serialize_u8(if v { 1 } else { 0 })
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
        serializer.serialize_u32(if v { 1 } else { 0 })
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
                    let err = format!("{} is not valid undefined {} value", i, name);
                    return Err(S::Error::custom(&err));
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

struct FloatSurrogate(f32);

impl Serialize for FloatSurrogate {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_f32_as_is(self.0, serializer)
    }
}

impl<'de> Deserialize<'de> for FloatSurrogate {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
        Ok(FloatSurrogate(deserialize_f32_as_is(deserializer)?))
    }
}

pub fn serialize_f32_s_as_is<S>(v: &[f32], serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    let mut serializer = serializer.serialize_seq(Some(v.len()))?;
    for &f in v.iter() {
        serializer.serialize_element(&FloatSurrogate(f))?;
    }
    serializer.end()
}

struct FloatsDeserializer;

impl<'de> de::Visitor<'de> for FloatsDeserializer {
    type Value = Vec<f32>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "list of floats")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: SeqAccess<'de> {
        let mut vec = seq.size_hint().map_or_else(Vec::new, Vec::with_capacity);
        while let Some(f) = seq.next_element::<FloatSurrogate>()? {
            vec.push(f.0);
        }
        Ok(vec)
    }
}

pub fn deserialize_f32_s_as_is<'de, D>(deserializer: D) -> Result<Vec<f32>, D::Error> where D: Deserializer<'de> {
    deserializer.deserialize_seq(FloatsDeserializer)
}

pub fn serialize_f32_as_is<S>(v: f32, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if !serializer.is_human_readable() {
        serializer.serialize_f32(v)
    } else if v.is_nan() {
        let d: u32 = unsafe { transmute(v) };
        if d == 0xFFFFFFFF {
            serializer.serialize_f32(v)
        } else {
            serializer.serialize_str(&format!("nan{:08X}", d))
        }
    } else {
        serializer.serialize_f64(f64::from_str(&v.to_string()).unwrap().copysign(v as f64))
    }
}

struct FloatHRDeserializer;

impl<'de> de::Visitor<'de> for FloatHRDeserializer {
    type Value = f32;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "32-bit float value or 'nanXXXXXXXX' (where X is hex digit)")
    }

    fn visit_f32<E>(self, v: f32) -> Result<Self::Value, E> where E: de::Error {
        Ok(if v.is_nan() { unsafe { transmute(0xFFFFFFFFu32) } } else { v })
    }

    fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E> where E: de::Error {
        let v_as_f32 = v as f32;
        if !v_as_f32.is_nan() || !v.is_nan() {
            let v_as_u64: u64 = unsafe { transmute(v) };
            let v_as_u32_as_u64: u64 = unsafe{ transmute(f64::from_str(&v_as_f32.to_string()).unwrap().copysign(v_as_f32 as f64)) };
            if v_as_u32_as_u64 != v_as_u64 {
                return Err(E::invalid_value(Unexpected::Float(v), &self));
            }
        }
        self.visit_f32(v_as_f32)
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> where E: de::Error {
        if s.len() != 11 || !s.starts_with("nan") || &s[4..5] == "+" {
            return Err(E::invalid_value(Unexpected::Str(s), &self));
        }
        let d = u32::from_str_radix(&s[3..], 16).map_err(|_| E::invalid_value(Unexpected::Str(s), &self))?;
        if d == 0xFFFFFFFF {
            return Err(E::invalid_value(Unexpected::Str(s), &self));
        }
        Ok(unsafe { transmute(d) })
    }
}

pub fn deserialize_f32_as_is<'de, D>(deserializer: D) -> Result<f32, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        deserializer.deserialize_any(FloatHRDeserializer)
    } else {
        f32::deserialize(deserializer)
    }
}

pub fn serialize_string_tuple<S>(s: &str, len: usize, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if serializer.is_human_readable() {
        serializer.serialize_str(s)
    } else {
        if s.as_bytes().last().map_or(false, |&x| x == 0) {
            return Err(S::Error::custom("string tuple value has tail zero"));
        }
        let mut serializer = serializer.serialize_tuple(len)?;
        let mut s_len = 0;
        for c in s.chars() {
            serializer.serialize_element(&c)?;
            s_len += 1;
        }
        if s_len > len {
            return Err(S::Error::custom(&format!("string length is above {} chars", len)));
        }
        for _ in s_len .. len {
            serializer.serialize_element(&'\0')?;
        }
        serializer.end()
    }
}

struct StringNHRDeserializer { len: usize }

impl<'de> de::Visitor<'de> for StringNHRDeserializer {
    type Value = String;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} character string", self.len)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        if let Some(n) = seq.size_hint() {
            if n != self.len {
                return Err(A::Error::invalid_length(n, &self));
            }
        }
        let mut string: String = String::with_capacity(self.len);
        let mut string_len = 0;
        while let Some(c) = seq.next_element()? {
            string.push(c);
            string_len += 1;
        }
        if string_len != self.len {
            Err(A::Error::invalid_length(string_len, &self))
        } else {
            let cut_to = string.rfind(|c| c != '\0').map_or(0, |n| n + string[n..].chars().nth(0).unwrap().len_utf8());
            string.truncate(cut_to);
            Ok(string)
        }
    }
}

pub fn deserialize_string_tuple<'de, D>(len: usize, deserializer: D) -> Result<String, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        String::deserialize(deserializer)
    } else {
        deserializer.deserialize_tuple(len, StringNHRDeserializer { len })
    }
}

pub fn serialize_string_list<S>(lines: &[String], separator: &str, len: Option<usize>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if serializer.is_human_readable() {
        lines.serialize(serializer)
    } else {
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
            if text.as_bytes().last().map_or(false, |&x| x == 0) {
                return Err(S::Error::custom("string list has tail zero"));
            }
            let mut serializer = serializer.serialize_tuple(len)?;
            let mut text_len = 0;
            for c in text.chars() {
                serializer.serialize_element(&c)?;
                text_len += 1;
            }
            if text_len > len {
                return Err(S::Error::custom(&format!("string list total length is above {} chars", len)));
            }
            for _ in text_len .. len {
                serializer.serialize_element(&'\0')?;
            }
            serializer.end()
        } else {
            let text_len = text.chars().count();
            let mut serializer = serializer.serialize_seq(Some(text_len))?;
            for c in text.chars() {
                serializer.serialize_element(&c)?;
            }
            serializer.end()
        }
    }
}

struct StringListNHRDeserializer<'a> { len: Option<usize>, separator: &'a str }

impl<'a, 'de> de::Visitor<'de> for StringListNHRDeserializer<'a> {
    type Value = Vec<String>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(len) = self.len {
            write!(f, "{} character string", len)
        } else {
            write!(f, "string")
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        let mut size_hint = seq.size_hint();
        if let Some(len) = self.len {
            if let Some(n) = size_hint {
                if n != len {
                    return Err(A::Error::invalid_length(n, &self));
                }
            } else {
                size_hint = Some(len);
            }
        }
        let mut string: String = size_hint.map_or_else(String::new, String::with_capacity);
        let mut string_len = 0;
        while let Some(c) = seq.next_element()? {
            string.push(c);
            string_len += 1;
        }
        if let Some(len) = self.len {
            if string_len != len {
                return Err(A::Error::invalid_length(string_len, &self));
            } else {
                let cut_to = string.rfind(|c| c != '\0').map_or(0, |n| n + string[n..].chars().nth(0).unwrap().len_utf8());
                string.truncate(cut_to);
            }
        }
        Ok(string.split(self.separator).map(|x| x.into()).collect())
    }
}

pub fn deserialize_string_list<'de, D>(separator: &str, len: Option<usize>, deserializer: D) -> Result<Vec<String>, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        <Vec<String>>::deserialize(deserializer)
    } else {
        if let Some(len) = len {
            deserializer.deserialize_tuple(len, StringListNHRDeserializer { len: Some(len), separator })
        } else {
            deserializer.deserialize_seq(StringListNHRDeserializer { len: None, separator })
        }
    }
}
