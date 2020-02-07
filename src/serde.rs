use std::cell::Cell;
use serde::{Deserializer, Serializer};
use serde::de::{self, Error as de_Error};
use encoding::{DecoderTrap, EncoderTrap};
use serde::ser::{SerializeTuple, SerializeSeq, Error as ser_Error};
use std::iter::{self};

use crate::base::*;

thread_local!(pub static CODE_PAGE: Cell<CodePage> = Cell::new(CodePage::English));

pub fn serialize_string<S>(code_page: CodePage, len: Option<usize>, s: &str, serializer: S)
    -> Result<S::Ok, S::Error> where
    S: Serializer {

    if serializer.is_human_readable() {
        serializer.serialize_str(s)
    } else {
        if len.is_some() && s.as_bytes().last().map_or(false, |&x| x == 0) {
            return Err(S::Error::custom("string has tail zero"));
        }
        let bytes = code_page.encoding().encode(s, EncoderTrap::Strict)
            .map_err(|x| S::Error::custom(&format!("unencodable char: {}", x)))?;
        let len = len.unwrap_or(bytes.len());
        if bytes.len() > len {
            return Err(S::Error::custom(&format!("string length is above {} bytes", len)));
        }
        let mut serializer = serializer.serialize_tuple(len)?;
        for byte in &bytes {
            serializer.serialize_element(byte)?;
        }
        for _ in bytes.len() .. len {
            serializer.serialize_element(&0u8)?;
        }
        serializer.end()
    }
}

pub fn serialize_string_z<S>(code_page: CodePage, s: &StringZ, serializer: S) 
    -> Result<S::Ok, S::Error> where
    S: Serializer {

    if serializer.is_human_readable() {
        let mut carets = s.str.len() - s.str.rfind(|x| x != '^').map_or(0, |i| 1 + i);
        if !s.has_tail_zero {
            carets += 1;
        }
        let mut e = String::with_capacity(s.str.len() + carets);
        e.push_str(&s.str);
        e.extend(iter::repeat('^').take(carets));
        serializer.serialize_str(&e)
    } else {
        if !s.has_tail_zero && s.str.as_bytes().last().map_or(false, |&x| x == 0) {
            return Err(S::Error::custom("zero-terminated string value has tail zero"));
        }
        let bytes = code_page.encoding().encode(&s.str, EncoderTrap::Strict)
            .map_err(|x| S::Error::custom(&format!("unencodable char: {}", x)))?;
        let len = bytes.len() + if s.has_tail_zero { 1 } else { 0 };
        let mut serializer = serializer.serialize_tuple(len)?;
        for byte in &bytes {
            serializer.serialize_element(byte)?;
        }
        if s.has_tail_zero {
            serializer.serialize_element(&0u8)?;
        }
        serializer.end()
    }
}

pub fn serialize_string_list<S>(code_page: CodePage, separator: &str, len: Option<usize>, list: &[String], serializer: S)
    -> Result<S::Ok, S::Error> where
    S: Serializer {

    if serializer.is_human_readable() {
        let mut serializer = serializer.serialize_seq(Some(list.len()))?;
        for str in list {
            serializer.serialize_element(str)?;
        }
        serializer.end()
    } else {
        if list.iter().find(|x| x.contains(separator)).is_some() {
            return Err(S::Error::custom("string list item contains separator"));
        }
        let text = list.join(separator);
        if len.is_some() && text.as_bytes().last().map_or(false, |&x| x == 0) {
            return Err(S::Error::custom("string list has tail zero"));
        }
        let bytes = code_page.encoding().encode(&text, EncoderTrap::Strict)
            .map_err(|x| S::Error::custom(&format!("unencodable char: {}", x)))?;
        let len = len.unwrap_or(bytes.len());
        if bytes.len() > len {
            return Err(S::Error::custom(&format!("string list total length is above {} bytes", len)));
        }
        let mut serializer = serializer.serialize_tuple(len)?;
        for byte in &bytes {
            serializer.serialize_element(byte)?;
        }
        for _ in bytes.len() .. len {
            serializer.serialize_element(&0u8)?;
        }
        serializer.end()
    }
}

pub fn serialize_string_z_list<S>(code_page: CodePage, list: &StringZList, serializer: S) 
    -> Result<S::Ok, S::Error> where
    S: Serializer {

    if serializer.is_human_readable() {
        let mut carets = list.vec.len() - list.vec.iter().rposition(|x| x != "^").map_or(0, |i| 1 + i);
        if !list.has_tail_zero {
            carets += 1;
        }
        let mut serializer = serializer.serialize_seq(Some(list.vec.len() + carets))?;
        for s in &list.vec {
            serializer.serialize_element(s)?;
        }
        for _ in 0 .. carets {
            serializer.serialize_element("^")?;
        }
        serializer.end()
    } else {
        if list.vec.iter().find(|x| x.contains('\0')).is_some() {
            return Err(S::Error::custom("zero-terminated string list item contains zero byte"));
        }
        let text = list.vec.join("\0");
        let bytes = code_page.encoding().encode(&text, EncoderTrap::Strict)
            .map_err(|x| S::Error::custom(&format!("unencodable char: {}", x)))?;
        let len = bytes.len() + if list.has_tail_zero { 1 } else { 0 };
        let mut serializer = serializer.serialize_tuple(len)?;
        for byte in &bytes {
            serializer.serialize_element(byte)?;
        }
        if list.has_tail_zero {
            serializer.serialize_element(&0u8)?;
        }
        serializer.end()
    }
}

struct StringDeserializer {
    is_human_readable: bool,
    len: usize,
    trim_tail_zeros: bool,
    code_page: CodePage,
}

fn trim_end_nulls(mut bytes: &[u8]) -> &[u8] {
    while !bytes.is_empty() && bytes[bytes.len() - 1] == 0 {
        bytes = &bytes[..bytes.len() - 1];
    }
    bytes
}

impl<'de> de::Visitor<'de> for StringDeserializer {
    type Value = String;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if self.is_human_readable {
            write!(f, "string")
        } else {
            write!(f, "array of {} bytes", self.len)
        }
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
        if self.is_human_readable {
            Ok(v.into())
        } else {
            Err(E::invalid_type(de::Unexpected::Str(v), &self))
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut bytes: Vec<u8> = Vec::with_capacity(self.len);
        while let Some(byte) = seq.next_element()? {
            bytes.push(byte);
        }
        if self.is_human_readable {
            Err(A::Error::invalid_type(de::Unexpected::Bytes(&bytes[..]), &self))
        } else if bytes.len() != self.len {
            Err(A::Error::invalid_value(de::Unexpected::Bytes(&bytes[..]), &self))
        } else {
            let bytes = if self.trim_tail_zeros {
                trim_end_nulls(&bytes[..])
            } else {
                &bytes[..]
            };
            Ok(self.code_page.encoding().decode(bytes, DecoderTrap::Strict).unwrap())
        }
    }
}

pub fn deserialize_string<'de, D>(code_page: CodePage, len: usize, trim_tail_zeros: bool, deserializer: D)
    -> Result<String, D::Error> where
    D: Deserializer<'de> {

    if deserializer.is_human_readable() {
        deserializer.deserialize_str(StringDeserializer { 
            code_page, len, is_human_readable: true,
            trim_tail_zeros
        })
    } else {
        deserializer.deserialize_tuple(len, StringDeserializer {
            code_page, len, is_human_readable: false,
            trim_tail_zeros
        })
    }
}

struct StringZDeserializer {
    is_human_readable: bool,
    len: usize,
    code_page: CodePage,
}

impl<'de> de::Visitor<'de> for StringZDeserializer {
    type Value = StringZ;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if self.is_human_readable {
            write!(f, "string")
        } else {
            write!(f, "array of {} bytes", self.len)
        }
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
        if self.is_human_readable {
            let carets = v.len() - v.rfind(|x| x != '^').map_or(0, |i| 1 + i);
            let has_tail_zero = carets % 2 == 1;
            let carets = (carets + 1) / 2;
            Ok(StringZ { str: v[.. v.len() - carets].into(), has_tail_zero })
        } else {
            Err(E::invalid_type(de::Unexpected::Str(v), &self))
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut bytes: Vec<u8> = Vec::with_capacity(self.len);
        while let Some(byte) = seq.next_element()? {
            bytes.push(byte);
        }
        if self.is_human_readable {
            Err(A::Error::invalid_type(de::Unexpected::Bytes(&bytes[..]), &self))
        } else if bytes.len() != self.len {
            Err(A::Error::invalid_value(de::Unexpected::Bytes(&bytes[..]), &self))
        } else {
            let has_tail_zero = bytes.last().map_or(false, |&x| x == 0);
            let bytes = if has_tail_zero { &bytes[.. bytes.len() - 1] } else { &bytes };
            let str = self.code_page.encoding().decode(bytes, DecoderTrap::Strict).unwrap();
            Ok(StringZ { str, has_tail_zero })
        }
    }
}

pub fn deserialize_string_z<'de, D>(code_page: CodePage, len: usize, deserializer: D) 
    -> Result<StringZ, D::Error> where
    D: Deserializer<'de> {

    if deserializer.is_human_readable() {
        deserializer.deserialize_str(StringZDeserializer {
            code_page, len, is_human_readable: true,
        })
    } else {
        deserializer.deserialize_tuple(len, StringZDeserializer {
            code_page, len, is_human_readable: false,
        })
    }
}

struct StringListDeserializer<'a> {
    is_human_readable: bool,
    len: usize,
    code_page: CodePage,
    separator: &'a str,
}

impl<'de, 'a> de::Visitor<'de> for StringListDeserializer<'a> {
    type Value = Vec<String>;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if self.is_human_readable {
            write!(f, "string sequence")
        } else {
            write!(f, "array of {} bytes", self.len)
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        if self.is_human_readable {
            let mut list: Vec<String> =
                seq.size_hint().map_or_else(|| Vec::new(), |x| Vec::with_capacity(x));
            while let Some(line) = seq.next_element()? {
                list.push(line);
            }
            Ok(list)
        } else {
            let mut bytes: Vec<u8> = Vec::with_capacity(self.len);
            while let Some(byte) = seq.next_element()? {
                bytes.push(byte);
            }
            if bytes.len() != self.len {
                Err(A::Error::invalid_value(de::Unexpected::Bytes(&bytes[..]), &self))
            } else {
                let s = self.code_page.encoding().decode(&bytes[..], DecoderTrap::Strict).unwrap();
                Ok(s.split(self.separator).map(String::from).collect())
            }
        }
    }
}

pub fn deserialize_string_list<'de, D>(code_page: CodePage, separator: &str, len: usize, deserializer: D)
    -> Result<Vec<String>, D::Error> where
    D: Deserializer<'de> {

    if deserializer.is_human_readable() {
        deserializer.deserialize_seq(StringListDeserializer { 
            code_page, separator, len, is_human_readable: true
        })
    } else {
        deserializer.deserialize_tuple(len, StringListDeserializer {
            code_page, separator, len, is_human_readable: false
        })
    }
}

struct StringZListDeserializer {
    is_human_readable: bool,
    len: usize,
    code_page: CodePage,
}

impl<'de> de::Visitor<'de> for StringZListDeserializer {
    type Value = StringZList;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if self.is_human_readable {
            write!(f, "string sequence")
        } else {
            write!(f, "array of {} bytes", self.len)
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        if self.is_human_readable {
            let mut vec: Vec<String> =
                seq.size_hint().map_or_else(|| Vec::new(), |x| Vec::with_capacity(x));
            while let Some(line) = seq.next_element()? {
                vec.push(line);
            }
            let carets = vec.len() - vec.iter().rposition(|x| x != "^").map_or(0, |i| 1 + i);
            let has_tail_zero = carets % 2 == 1;
            let carets = (carets + 1) / 2;
            Ok(StringZList { vec: vec[.. vec.len() - carets].into(), has_tail_zero })
        } else {
            let mut bytes: Vec<u8> = Vec::with_capacity(self.len);
            while let Some(byte) = seq.next_element()? {
                bytes.push(byte);
            }
            if bytes.len() != self.len {
                Err(A::Error::invalid_value(de::Unexpected::Bytes(&bytes[..]), &self))
            } else {
                let has_tail_zero = bytes.last().map_or(false, |&x| x == 0);
                let bytes = if has_tail_zero { &bytes[.. bytes.len() - 1] } else { &bytes };
                let s = self.code_page.encoding().decode(&bytes[..], DecoderTrap::Strict).unwrap();
                Ok(StringZList { vec: s.split("\0").map(String::from).collect(), has_tail_zero })
            }
        }
    }
}

pub fn deserialize_string_z_list<'de, D>(code_page: CodePage, len: usize, deserializer: D) 
    -> Result<StringZList, D::Error> where
    D: Deserializer<'de> {

    if deserializer.is_human_readable() {
        deserializer.deserialize_seq(StringZListDeserializer {
            code_page, len, is_human_readable: true
        })
    } else {
        deserializer.deserialize_tuple(len, StringZListDeserializer {
            code_page, len, is_human_readable: false
        })
    }
}
