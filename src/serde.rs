use std::cell::Cell;
use serde::{Deserializer, Serializer};
use serde::de::{self, Error as de_Error};
use encoding::{DecoderTrap, EncoderTrap};
use serde::ser::{SerializeTuple, SerializeSeq, Error as ser_Error};
use std::iter::{self};
use std::marker::PhantomData;

use crate::base::*;

thread_local!(pub static CODE_PAGE: Cell<CodePage> = Cell::new(CodePage::English));

pub fn serialize_string<S>(code_page: CodePage, len: Option<usize>, s: &str, serializer: S)
    -> Result<S::Ok, S::Error> where
    S: Serializer {

    if serializer.is_human_readable() {
        serializer.serialize_str(s)
    } else {
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
        let mut carets = s.str.rfind(|x| x != '^').map_or(0, |i| s.str.len() - i);
        if !s.has_tail_zero {
            carets += 1;
        }
        let mut e = String::with_capacity(s.str.len() + carets);
        e.push_str(&s.str);
        e.extend(iter::repeat('^').take(carets));
        serializer.serialize_str(&e)
    } else {
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

pub fn serialize_multiline<S>(code_page: CodePage, linebreaks: LinebreakStyle, len: Option<usize>, lines: &[String], serializer: S)
    -> Result<S::Ok, S::Error> where
    S: Serializer {

    if serializer.is_human_readable() {
        let mut serializer = serializer.serialize_seq(Some(lines.len()))?;
        for line in lines {
            serializer.serialize_element(line)?;
        }
        serializer.end()
    } else {
        let text = lines.join(linebreaks.new_line());
        let bytes = code_page.encoding().encode(&text, EncoderTrap::Strict)
            .map_err(|x| S::Error::custom(&format!("unencodable char: {}", x)))?;
        let len = len.unwrap_or(bytes.len());
        if bytes.len() > len {
            return Err(S::Error::custom(&format!("lines total length is above {} bytes", len)));
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
        let mut serializer = serializer.serialize_seq(Some(list.vec.len()))?;
        for s in &list.vec {
            serializer.serialize_element(s)?;
        }
        serializer.end()
    } else {
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

struct StringDeserializer<T> {
    is_human_readable: bool,
    len: usize,
    code_page: CodePage,
    phantom: PhantomData<T>
}

impl<'de> de::Visitor<'de> for StringDeserializer<String> {
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
            Ok(self.code_page.encoding().decode(&bytes[..], DecoderTrap::Strict).unwrap())
        }
    }
}

pub fn deserialize_string<'de, D>(code_page: CodePage, len: usize, deserializer: D)
    -> Result<String, D::Error> where
    D: Deserializer<'de> {

    if deserializer.is_human_readable() {
        deserializer.deserialize_str(StringDeserializer::<String> { 
            code_page, len, is_human_readable: true,
            phantom: PhantomData
        })
    } else {
        deserializer.deserialize_tuple(len, StringDeserializer::<String> {
            code_page, len, is_human_readable: false,
            phantom: PhantomData
        })
    }
}

impl<'de> de::Visitor<'de> for StringDeserializer<StringZ> {
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
            let carets = v.rfind(|x| x != '^').map_or(0, |i| v.len() - i);
            let has_tail_zero = carets % 2 == 1;
            let carets = (carets + 1) / 2;
            Ok(StringZ { str: v[v.len() - carets .. ].into(), has_tail_zero })
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
        deserializer.deserialize_str(StringDeserializer::<StringZ> {
            code_page, len, is_human_readable: true,
            phantom: PhantomData
        })
    } else {
        deserializer.deserialize_tuple(len, StringDeserializer::<StringZ> {
            code_page, len, is_human_readable: false,
            phantom: PhantomData
        })
    }
}

struct MultilineDeserializer {
    is_human_readable: bool,
    len: usize,
    code_page: CodePage,
    linebreaks: LinebreakStyle,
}

impl<'de> de::Visitor<'de> for MultilineDeserializer {
    type Value = Vec<String>;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if self.is_human_readable {
            write!(f, "lines sequence")
        } else {
            write!(f, "array of {} bytes", self.len)
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        if self.is_human_readable {
            let mut lines: Vec<String> =
                seq.size_hint().map_or_else(|| Vec::new(), |x| Vec::with_capacity(x));
            while let Some(line) = seq.next_element()? {
                lines.push(line);
            }
            Ok(lines)
        } else {
            let mut bytes: Vec<u8> = Vec::with_capacity(self.len);
            while let Some(byte) = seq.next_element()? {
                bytes.push(byte);
            }
            if bytes.len() != self.len {
                Err(A::Error::invalid_value(de::Unexpected::Bytes(&bytes[..]), &self))
            } else {
                let s = self.code_page.encoding().decode(&bytes[..], DecoderTrap::Strict).unwrap();
                Ok(s.split(self.linebreaks.new_line()).map(String::from).collect())
            }
        }
    }
}

pub fn deserialize_multiline<'de, D>(code_page: CodePage, linebreaks: LinebreakStyle, len: usize, deserializer: D)
    -> Result<Vec<String>, D::Error> where
    D: Deserializer<'de> {

    if deserializer.is_human_readable() {
        deserializer.deserialize_seq(MultilineDeserializer { code_page, linebreaks, len, is_human_readable: true })
    } else {
        deserializer.deserialize_tuple(len, MultilineDeserializer { code_page, linebreaks, len, is_human_readable: false })
    }
}
