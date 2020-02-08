use std::cell::Cell;
use serde::{Deserializer, Serializer};
use serde::de::{self, Error as de_Error};
use encoding::{DecoderTrap, EncoderTrap};
use serde::ser::{SerializeTuple, SerializeSeq, Error as ser_Error};
use std::iter::{self};

use crate::base::*;

thread_local!(pub static CODE_PAGE: Cell<CodePage> = Cell::new(CodePage::English));

pub trait SerializerTes3Ext: Serializer {
    fn serialize_string(self, code_page: CodePage, len: Option<usize>, s: &str) 
        -> Result<Self::Ok, Self::Error> {

        if self.is_human_readable() {
            self.serialize_str(s)
        } else {
            if len.is_some() && s.as_bytes().last().map_or(false, |&x| x == 0) {
                return Err(Self::Error::custom("string has tail zero"));
            }
            let bytes = code_page.encoding().encode(s, EncoderTrap::Strict)
                .map_err(|x| Self::Error::custom(&format!("unencodable char: {}", x)))?;
            let len = len.unwrap_or(bytes.len());
            if bytes.len() > len {
                return Err(Self::Error::custom(&format!("string length is above {} bytes", len)));
            }
            let mut serializer = self.serialize_tuple(len)?;
            for byte in &bytes {
                serializer.serialize_element(byte)?;
            }
            for _ in bytes.len()..len {
                serializer.serialize_element(&0u8)?;
            }
            serializer.end()
        }
    }

    fn serialize_hr_string_z(self, s: &StringZ) -> Result<Self::Ok, Self::Error> {
        let mut carets = s.str.len() - s.str.rfind(|x| x != '^').map_or(0, |i| 1 + i);
        if !s.has_tail_zero {
            carets += 1;
        }
        let mut e = String::with_capacity(s.str.len() + carets);
        e.push_str(&s.str);
        e.extend(iter::repeat('^').take(carets));
        self.serialize_str(&e)
    }

    fn serialize_string_z(self, code_page: CodePage, s: &StringZ) -> Result<Self::Ok, Self::Error> {
        if self.is_human_readable() {
            self.serialize_hr_string_z(s)
        } else {
            if !s.has_tail_zero && s.str.as_bytes().last().map_or(false, |&x| x == 0) {
                return Err(Self::Error::custom("zero-terminated string value has tail zero"));
            }
            let bytes = code_page.encoding().encode(&s.str, EncoderTrap::Strict)
                .map_err(|x| Self::Error::custom(&format!("unencodable char: {}", x)))?;
            let len = bytes.len() + if s.has_tail_zero { 1 } else { 0 };
            let mut serializer = self.serialize_tuple(len)?;
            for byte in &bytes {
                serializer.serialize_element(byte)?;
            }
            if s.has_tail_zero {
                serializer.serialize_element(&0u8)?;
            }
            serializer.end()
        }
    }

    fn serialize_hr_string_list(self, list: &[String]) -> Result<Self::Ok, Self::Error> {
        let mut serializer = self.serialize_seq(Some(list.len()))?;
        for str in list {
            serializer.serialize_element(str)?;
        }
        serializer.end()
    }

    fn serialize_string_list(self, code_page: CodePage, separator: &str, len: Option<usize>, list: &[String]) 
        -> Result<Self::Ok, Self::Error> {

        if self.is_human_readable() {
            self.serialize_hr_string_list(list)
        } else {
            if list.iter().find(|x| x.contains(separator)).is_some() {
                return Err(Self::Error::custom("string list item contains separator"));
            }
            let text = list.join(separator);
            if len.is_some() && text.as_bytes().last().map_or(false, |&x| x == 0) {
                return Err(Self::Error::custom("string list has tail zero"));
            }
            let bytes = code_page.encoding().encode(&text, EncoderTrap::Strict)
                .map_err(|x| Self::Error::custom(&format!("unencodable char: {}", x)))?;
            let len = len.unwrap_or(bytes.len());
            if bytes.len() > len {
                return Err(Self::Error::custom(&format!("string list total length is above {} bytes", len)));
            }
            let mut serializer = self.serialize_tuple(len)?;
            for byte in &bytes {
                serializer.serialize_element(byte)?;
            }
            for _ in bytes.len()..len {
                serializer.serialize_element(&0u8)?;
            }
            serializer.end()
        }
    }

    fn serialize_hr_string_z_list(self, list: &StringZList) -> Result<Self::Ok, Self::Error> {
        let mut carets = list.vec.len() - list.vec.iter().rposition(|x| x != "^").map_or(0, |i| 1 + i);
        if !list.has_tail_zero {
            carets += 1;
        }
        let mut serializer = self.serialize_seq(Some(list.vec.len() + carets))?;
        for s in &list.vec {
            serializer.serialize_element(s)?;
        }
        for _ in 0..carets {
            serializer.serialize_element("^")?;
        }
        serializer.end()
    }

    fn serialize_string_z_list(self, code_page: CodePage, list: &StringZList) 
        -> Result<Self::Ok, Self::Error> {
        
        if self.is_human_readable() {
            self.serialize_hr_string_z_list(list)
        } else {
            if list.vec.iter().find(|x| x.contains('\0')).is_some() {
                return Err(Self::Error::custom("zero-terminated string list item contains zero byte"));
            }
            let text = list.vec.join("\0");
            let bytes = code_page.encoding().encode(&text, EncoderTrap::Strict)
                .map_err(|x| Self::Error::custom(&format!("unencodable char: {}", x)))?;
            let len = bytes.len() + if list.has_tail_zero { 1 } else { 0 };
            let mut serializer = self.serialize_tuple(len)?;
            for byte in &bytes {
                serializer.serialize_element(byte)?;
            }
            if list.has_tail_zero {
                serializer.serialize_element(&0u8)?;
            }
            serializer.end()
        }
    }
}

impl<S: Serializer> SerializerTes3Ext for S { }

struct StringHRDeserializer;

impl<'de> de::Visitor<'de> for StringHRDeserializer {
    type Value = String;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "string")
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
        Ok(v.into())
    }
}

struct StringDeserializer {
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
        write!(f, "array of {} bytes", self.len)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut bytes: Vec<u8> = Vec::with_capacity(self.len);
        while let Some(byte) = seq.next_element()? {
            bytes.push(byte);
        }
        if bytes.len() != self.len {
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

struct StringZHRDeserializer;

impl<'de> de::Visitor<'de> for StringZHRDeserializer {
    type Value = StringZ;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "string")
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
        let carets = v.len() - v.rfind(|x| x != '^').map_or(0, |i| 1 + i);
        let has_tail_zero = carets % 2 == 1;
        let carets = (carets + 1) / 2;
        Ok(StringZ { str: v[.. v.len() - carets].into(), has_tail_zero })
    }
}

struct StringZDeserializer {
    len: usize,
    code_page: CodePage,
}

impl<'de> de::Visitor<'de> for StringZDeserializer {
    type Value = StringZ;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "array of {} bytes", self.len)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut bytes: Vec<u8> = Vec::with_capacity(self.len);
        while let Some(byte) = seq.next_element()? {
            bytes.push(byte);
        }
        if bytes.len() != self.len {
            Err(A::Error::invalid_value(de::Unexpected::Bytes(&bytes[..]), &self))
        } else {
            let has_tail_zero = bytes.last().map_or(false, |&x| x == 0);
            let bytes = if has_tail_zero { &bytes[.. bytes.len() - 1] } else { &bytes };
            let str = self.code_page.encoding().decode(bytes, DecoderTrap::Strict).unwrap();
            Ok(StringZ { str, has_tail_zero })
        }
    }
}

struct StringListHRDeserializer;

impl<'de> de::Visitor<'de> for StringListHRDeserializer {
    type Value = Vec<String>;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "string sequence")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut list: Vec<String> =
            seq.size_hint().map_or_else(|| Vec::new(), |x| Vec::with_capacity(x));
        while let Some(line) = seq.next_element()? {
            list.push(line);
        }
        Ok(list)
    }
}

struct StringListDeserializer<'a> {
    len: usize,
    code_page: CodePage,
    separator: &'a str,
    trim_tail_zeros: bool,
}

impl<'de, 'a> de::Visitor<'de> for StringListDeserializer<'a> {
    type Value = Vec<String>;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "array of {} bytes", self.len)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut bytes: Vec<u8> = Vec::with_capacity(self.len);
        while let Some(byte) = seq.next_element()? {
            bytes.push(byte);
        }
        if bytes.len() != self.len {
            Err(A::Error::invalid_value(de::Unexpected::Bytes(&bytes[..]), &self))
        } else {
            let bytes = if self.trim_tail_zeros {
                trim_end_nulls(&bytes[..])
            } else {
                &bytes[..]
            };
            let s = self.code_page.encoding().decode(&bytes[..], DecoderTrap::Strict).unwrap();
            Ok(s.split(self.separator).map(String::from).collect())
        }
    }
}

struct StringZListHRDeserializer;

impl<'de> de::Visitor<'de> for StringZListHRDeserializer {
    type Value = StringZList;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "string sequence")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut vec: Vec<String> =
            seq.size_hint().map_or_else(|| Vec::new(), |x| Vec::with_capacity(x));
        while let Some(line) = seq.next_element()? {
            vec.push(line);
        }
        let carets = vec.len() - vec.iter().rposition(|x| x != "^").map_or(0, |i| 1 + i);
        let has_tail_zero = carets % 2 == 1;
        let carets = (carets + 1) / 2;
        Ok(StringZList { vec: vec[.. vec.len() - carets].into(), has_tail_zero })
    }
}

struct StringZListDeserializer {
    len: usize,
    code_page: CodePage,
}

impl<'de> de::Visitor<'de> for StringZListDeserializer {
    type Value = StringZList;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "array of {} bytes", self.len)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

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

pub trait DeserializerTes3Ext<'de>: Deserializer<'de> {
    fn deserialize_hr_string(self) -> Result<String, Self::Error> {
        self.deserialize_str(StringHRDeserializer)
    }

    fn deserialize_string_ext(self, code_page: CodePage, len: usize, trim_tail_zeros: bool) 
        -> Result<String, Self::Error> {

        if self.is_human_readable() {
            self.deserialize_hr_string()
        } else {
            self.deserialize_tuple(len, StringDeserializer {
                code_page, len, trim_tail_zeros
            })
        }
    }

    fn deserialize_hr_string_z(self) -> Result<StringZ, Self::Error> {
        self.deserialize_str(StringZHRDeserializer)
    }

    fn deserialize_string_z(self, code_page: CodePage, len: usize) -> Result<StringZ, Self::Error> {
        if self.is_human_readable() {
            self.deserialize_hr_string_z()
        } else {
            self.deserialize_tuple(len, StringZDeserializer {
                code_page, len
            })
        }
    }

    fn deserialize_hr_string_list(self) -> Result<Vec<String>, Self::Error> {
        self.deserialize_seq(StringListHRDeserializer)
    }

    fn deserialize_string_list(self, code_page: CodePage, separator: &str, len: usize, trim_tail_zeros: bool)
        -> Result<Vec<String>, Self::Error> {

        if self.is_human_readable() {
            self.deserialize_hr_string_list()
        } else {
            self.deserialize_tuple(len, StringListDeserializer {
                code_page, separator, len, trim_tail_zeros
            })
        }
    }

    fn deserialize_hr_string_z_list(self) -> Result<StringZList, Self::Error> {
        self.deserialize_seq(StringZListHRDeserializer)
    }

    fn deserialize_string_z_list(self, code_page: CodePage, len: usize) -> Result<StringZList, Self::Error> {
        if self.is_human_readable() {
            self.deserialize_hr_string_z_list()
        } else {
            self.deserialize_tuple(len, StringZListDeserializer {
                code_page, len
            })
        }
    }
}

impl<'de, D: Deserializer<'de>> DeserializerTes3Ext<'de> for D { }
