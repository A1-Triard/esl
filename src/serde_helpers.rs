use crate::code::CodePage;
use educe::Educe;
use either::{Either, Left,  Right};
use phantom_type::PhantomType;
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::de::{self, Unexpected, DeserializeSeed};
use serde::de::Error as de_Error;
use serde::ser::Error as ser_Error;
use serde::ser::SerializeTuple;
use serde_serialize_seed::{SerializeSeed, ValueWithSeed};
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

#[derive(Educe)]
#[educe(Clone)]
pub struct ArraySerde<T> {
    pub len: usize,
    pub phantom: PhantomType<T>,
}

impl<T: Serialize> SerializeSeed for ArraySerde<T> {
    type Value = [T];

    fn serialize<S: Serializer>(&self, value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        assert_eq!(value.len(), self.len);
        let mut serializer = serializer.serialize_tuple(self.len)?;
        for item in value {
            serializer.serialize_element(item)?;
        }
        serializer.end()
    }
}

struct ArrayDeVisitor<T>(PhantomType<T>);

impl<'de, T: Deserialize<'de>> de::Visitor<'de> for ArrayDeVisitor<T> {
    type Value = Vec<T>;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "array")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        let mut res = seq.size_hint().map_or_else(Vec::new, Vec::with_capacity);
        while let Some(item) = seq.next_element()? {
            res.push(item);
        }
        Ok(res)
    }
}

impl<'de, T: Deserialize<'de>> DeserializeSeed<'de> for ArraySerde<T> {
    type Value = Vec<T>;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        deserializer.deserialize_tuple(self.len, ArrayDeVisitor(PhantomType::new()))
    }
}

#[derive(Clone)]
pub struct NoneU8Serde { pub none: u8 }

impl SerializeSeed for NoneU8Serde {
    type Value = ();

    fn serialize<S: Serializer>(&self, _value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            ().serialize(serializer)
        } else {
            self.none.serialize(serializer)
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

#[derive(Clone)]
pub struct BoolU8Serde;

impl SerializeSeed for BoolU8Serde {
    type Value = bool;

    fn serialize<S: Serializer>(&self, &value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            value.serialize(serializer)
        } else {
            u8::from(value).serialize(serializer)
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

#[derive(Clone)]
pub struct BoolU32Serde;

impl SerializeSeed for BoolU32Serde {
    type Value = bool;

    fn serialize<S: Serializer>(&self, &value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            value.serialize(serializer)
        } else {
            u32::from(value).serialize(serializer)
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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct F32AsIsSerde;

impl SerializeSeed for F32AsIsSerde {
    type Value = f32;

    fn serialize<S>(&self, &value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if !serializer.is_human_readable() {
            value.serialize(serializer)
        } else if value.is_nan() {
            let d: u32 = value.to_bits();
            if d == 0xFFFFFFFF {
                value.serialize(serializer)
            } else {
                format!("nan{d:08X}").serialize(serializer)
            }
        } else {
            f64::from_str(&value.to_string()).unwrap().copysign(value as f64).serialize(serializer)
        }
    }
}

impl<'de> DeserializeSeed<'de> for F32AsIsSerde {
    type Value = f32;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            deserializer.deserialize_any(F32HRDeVisitor)
        } else {
            Ok(f32::deserialize(deserializer)?)
        }
    }
}

struct F32HRDeVisitor;

impl<'de> de::Visitor<'de> for F32HRDeVisitor {
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

#[derive(Clone)]
pub struct StringSerde {
    pub code_page: Option<CodePage>,
    pub len: Option<usize>
}

impl SerializeSeed for StringSerde {
    type Value = str;

    fn serialize<S: Serializer>(&self, value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            value.serialize(serializer)
        } else {
            let Some(code_page) = self.code_page else {
                return Err(S::Error::custom("code page required for binary serialization"));
            };
            let mut bytes = code_page.encode(value).map_err(|e| match e {
                None => S::Error::custom(format!("the '{value}' string does not correspond to any source byte sequence")),
                Some(c) => S::Error::custom(format!("the '{c}' char is not representable in {code_page:?} code page"))
            })?;
            if let Some(len) = self.len {
                if bytes.len() > len {
                    return Err(S::Error::custom(format!(
                        "short string (len = {}) does not fit (max len = {})", bytes.len(), len
                    )));
                }
                if bytes.last() == Some(&0) {
                    return Err(S::Error::custom("short string value has tail zero"));
                }
                bytes.resize(len, 0);
                ValueWithSeed(&bytes[..], ArraySerde { len, phantom: PhantomType::new() }).serialize(serializer)
            } else {
                bytes.serialize(serializer)
            }
        }
    }
}

impl<'de> DeserializeSeed<'de> for StringSerde {
    type Value = String;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            String::deserialize(deserializer)
        } else {
            let bytes = if let Some(len) = self.len {
                ArraySerde { len, phantom: PhantomType::new() }.deserialize(deserializer)?
            } else {
                <Vec<u8>>::deserialize(deserializer)?
            };
            let bytes = if self.len.is_some() {
                let trim = bytes.iter().copied().rev().take_while(|&x| x == 0).count();
                &bytes[.. bytes.len() - trim]
            } else {
                &bytes
            };
            let Some(code_page) = self.code_page else {
                return Err(D::Error::custom("code page required for binary serialization"));
            };
            Ok(code_page.decode(bytes))
        }
    }
}

#[derive(Clone)]
pub struct StringListSerde {
    pub code_page: Option<CodePage>,
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
            ValueWithSeed(text.as_str(), StringSerde { code_page: self.code_page, len: self.len }).serialize(serializer)
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
            let s = StringSerde { code_page: self.code_page, len: self.len }.deserialize(deserializer)?;
            Ok(s.split(self.separator).map(|x| x.into()).collect())
        }
    }
}
