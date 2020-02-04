use std::cell::Cell;
use serde::{Deserializer, Serializer};
use serde::de::{self, Error as de_Error};
use encoding::{DecoderTrap, EncoderTrap};
use serde::ser::{SerializeTuple, SerializeSeq, Error as ser_Error};

use crate::base::*;

thread_local!(pub static CODE_PAGE: Cell<CodePage> = Cell::new(CodePage::English));

pub fn serialize_string<S>(len: usize, s: &str, serializer: S) -> Result<S::Ok, S::Error> where
    S: Serializer {

    if serializer.is_human_readable() {
        serializer.serialize_str(s)
    } else {
        let bytes = CODE_PAGE.with(|x| x.get().encoding().encode(s, EncoderTrap::Strict)
            .map_err(|x| S::Error::custom(&format!("unencodable char: {}", x))))?;
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

pub fn serialize_multiline<S>(len: usize, lines: &[String], serializer: S) -> Result<S::Ok, S::Error> where
    S: Serializer {

    if serializer.is_human_readable() {
        let mut serializer = serializer.serialize_seq(Some(lines.len()))?;
        for line in lines {
            serializer.serialize_element(line)?;
        }
        serializer.end()
    } else {
        let mut serializer = serializer.serialize_tuple(len)?;
        let mut lines_len = 0;
        let mut first_line = true;
        for line in lines {
            let bytes = CODE_PAGE.with(|x| x.get().encoding().encode(line, EncoderTrap::Strict)
                .map_err(|x| S::Error::custom(&format!("unencodable char: {}", x))))?;
            if first_line {
                first_line = false;
            } else {
                lines_len += 2;
                if lines_len > len {
                    return Err(S::Error::custom(&format!("lines total length is above {} bytes", len)));
                }
                serializer.serialize_element(&13u8)?;
                serializer.serialize_element(&10u8)?;
            }
            lines_len += bytes.len();
            if lines_len > len {
                return Err(S::Error::custom(&format!("lines total length is above {} bytes", len)));
            }
            for byte in &bytes {
                serializer.serialize_element(byte)?;
            }
        }
        for _ in lines_len..len {
            serializer.serialize_element(&0u8)?;
        }
        serializer.end()
    }
}

struct StringDeserializer {
    is_human_readable: bool,
    len: usize
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
            Ok(CODE_PAGE.with(|x| x.get().encoding().decode(&bytes[..], DecoderTrap::Strict).unwrap()))
        }
    }
}

pub fn deserialize_string<'de, D>(len: usize, deserializer: D) -> Result<String, D::Error> where
    D: Deserializer<'de> {

    if deserializer.is_human_readable() {
        deserializer.deserialize_str(StringDeserializer { len, is_human_readable: true })
    } else {
        deserializer.deserialize_tuple(len, StringDeserializer { len, is_human_readable: false })
    }
}

struct MultilineDeserializer {
    is_human_readable: bool,
    len: usize
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
                let s = CODE_PAGE.with(|x| x.get().encoding().decode(&bytes[..], DecoderTrap::Strict).unwrap());
                Ok(s.split(LinebreakStyle::Dos.new_line()).map(String::from).collect())
            }
        }
    }
}

pub fn deserialize_multiline<'de, D>(len: usize, deserializer: D) -> Result<Vec<String>, D::Error> where
    D: Deserializer<'de> {

    if deserializer.is_human_readable() {
        deserializer.deserialize_seq(MultilineDeserializer { len, is_human_readable: true })
    } else {
        deserializer.deserialize_tuple(len, MultilineDeserializer { len, is_human_readable: false })
    }
}
