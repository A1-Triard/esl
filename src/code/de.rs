use serde::{Deserializer};
use std::fmt::{self, Display};
use encoding::{DecoderTrap};
use serde::de::{self, Visitor, SeqAccess, DeserializeSeed};
use std::io::{self, Read};
use byteorder::{LittleEndian, ReadBytesExt};
use std::borrow::Cow;
use std::marker::PhantomData;

use crate::code::code_page::*;

#[derive(Debug)]
pub enum DeError {
    Custom(String),
    InvalidBoolEncoding(u8),
    InvalidSize { actual: usize, expected: u32 },
    DeserializeAnyNotSupported
}

impl Display for DeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DeError::Custom(s) => Display::fmt(s, f),
            DeError::InvalidBoolEncoding(b) => write!(f, "invalid bool encoding ({})", b),
            DeError::DeserializeAnyNotSupported => write!(f, "deserialize_any not supported"),
            DeError::InvalidSize { actual, expected } => write!(f, "object size mismatch (actual = {}, expected = {})", actual, expected),
        }
    }
}

impl std::error::Error for DeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl de::Error for DeError {
    fn custom<T: Display>(msg: T) -> Self { DeError::Custom(format!("{}", msg)) }
}

#[derive(Debug)]
pub enum DeOrIoError {
    Io(io::Error),
    De(DeError),
}

impl Display for DeOrIoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DeOrIoError::Io(e) => Display::fmt(e, f),
            DeOrIoError::De(e) => Display::fmt(e, f),
        }
    }
}

impl std::error::Error for DeOrIoError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            DeOrIoError::Io(e) => Some(e),
            DeOrIoError::De(e) => Some(e),
        }
    }
}

impl de::Error for DeOrIoError {
    fn custom<T: Display>(msg: T) -> Self { DeOrIoError::De(DeError::custom(msg)) }
}

impl From<DeError> for DeOrIoError {
    fn from(e: DeError) -> DeOrIoError { DeOrIoError::De(e) }
}

impl From<io::Error> for DeOrIoError {
    fn from(e: io::Error) -> DeOrIoError { DeOrIoError::Io(e) }
}

trait Reader<'de>: Read {
    fn read_bytes(&mut self, len: usize) -> io::Result<Cow<'de, [u8]>>;
    fn pos(&self) -> isize;
}

struct GenericReader<'de, R: Read + ?Sized> {
    reader: &'de mut R,
    pos: isize,
}

impl<'de, R: Read + ?Sized> Read for GenericReader<'de, R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let read = self.reader.read(buf)?;
        self.pos += read as isize;
        Ok(read)
    }
}

impl<'de, R: Read + ?Sized> Reader<'de> for GenericReader<'de, R> {
    fn read_bytes(&mut self, len: usize) -> io::Result<Cow<'de, [u8]>> {
        let mut buf = Vec::with_capacity(len);
        buf.resize(len, 0);
        self.read_exact(&mut buf[..])?;
        Ok(Cow::Owned(buf))
    }

    fn pos(&self) -> isize { self.pos }
}

impl<'de> Reader<'de> for &'de [u8] {
    fn read_bytes(&mut self, len: usize) -> io::Result<Cow<'de, [u8]>> {
        if self.len() < len {
            Err(io::ErrorKind::UnexpectedEof.into())
        } else {
            let res = &self[0..len];
            *self = &self[len..];
            Ok(Cow::Borrowed(res))
        }
    }

    fn pos(&self) -> isize { -(self.len() as isize) }
}

#[derive(Debug)]
struct SeqDeserializer<'a, 'de, R: Reader<'de>> {
    start_pos: isize,
    size: u32,
    code_page: CodePage,
    reader: &'a mut R,
    phantom: PhantomData<&'de ()>
}

impl <'a, 'de, R: Reader<'de>> SeqAccess<'de> for SeqDeserializer<'a, 'de, R> {
    type Error = DeOrIoError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error> where T: DeserializeSeed<'de> {
        if self.reader.pos() == self.start_pos + self.size as isize { return Ok(None); }
        let element = seed.deserialize(EslDeserializer { 
            isolated: None, code_page: self.code_page, reader: self.reader,
            phantom: PhantomData
        })?;
        if self.reader.pos() > self.start_pos + self.size as isize {
            return Err(DeError::InvalidSize { expected: self.size, actual: (self.reader.pos() - self.start_pos) as usize }.into());
        }
        Ok(Some(element))
    }
}

#[derive(Debug)]
struct StructDeserializer<'a, 'de, R: Reader<'de>> {
    len: usize,
    isolated: Option<(isize, u32)>,
    code_page: CodePage,
    reader: &'a mut R,
    phantom: PhantomData<&'de ()>
}

impl <'a, 'de, R: Reader<'de>> SeqAccess<'de> for StructDeserializer<'a, 'de, R> {
    type Error = DeOrIoError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error> where T: DeserializeSeed<'de> {
        if self.len == 0 { return Ok(None); }
        self.len -= 1;
        let isolated = self.isolated.and_then(|(start_pos, _)| {
            if self.len == 0 { Some((self.reader.pos() - start_pos) as u32) } else { None }
        });
        let element = seed.deserialize(EslDeserializer {
            isolated, code_page: self.code_page, reader: self.reader,
            phantom: PhantomData
        })?;
        if let Some((start_pos, size)) = self.isolated {
            if self.reader.pos() > start_pos + size as isize {
                return Err(DeError::InvalidSize { expected: size, actual: (self.reader.pos() - start_pos) as usize }.into());
            }
        }
        Ok(Some(element))
    }
}

impl<'a, 'de, R: Reader<'de>> EslDeserializer<'a, 'de, R> {
    fn deserialize_size(&mut self) -> Result<u32, io::Error> {
        if let Some(size) = self.isolated {
            Ok(size)
        } else {
            self.reader.read_u32::<LittleEndian>()
        }
    }
}

#[derive(Debug)]
struct EslDeserializer<'a, 'de, R: Reader<'de>> {
    isolated: Option<u32>,
    code_page: CodePage,
    reader: &'a mut R,
    phantom: PhantomData<&'de ()>
}

impl<'a, 'de, R: Reader<'de>> Deserializer<'de> for EslDeserializer<'a, 'de, R> {
    type Error = DeOrIoError;

    fn deserialize_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(DeError::DeserializeAnyNotSupported.into())
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        let b = self.reader.read_u8()?;
        let v = match b {
            0 => false,
            1 => true,
            b => return Err(DeError::InvalidBoolEncoding(b).into())
        };
        visitor.visit_bool(v)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_i8(self.reader.read_i8()?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_i16(self.reader.read_i16::<LittleEndian>()?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_i32(self.reader.read_i32::<LittleEndian>()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_i64(self.reader.read_i64::<LittleEndian>()?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_f32(self.reader.read_f32::<LittleEndian>()?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_f64(self.reader.read_f64::<LittleEndian>()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_u8(self.reader.read_u8()?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_u16(self.reader.read_u16::<LittleEndian>()?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_u32(self.reader.read_u32::<LittleEndian>()?)
    }
    
    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_u64(self.reader.read_u64::<LittleEndian>()?)
    }

    serde_if_integer128! {
        fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
            visitor.visit_i128(self.reader.read_i128::<LittleEndian>()?)
        }

        fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
            visitor.visit_u128(self.reader.read_u128::<LittleEndian>()?)
        }
    }
    
    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        let b = self.reader.read_u8()?;
        let v = self.code_page.encoding().decode(&[b], DecoderTrap::Strict).unwrap();
        visitor.visit_char(v.chars().nth(0).unwrap())
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        self.deserialize_string(visitor)
    }

    fn deserialize_string<V>(mut self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        let size = self.deserialize_size()? as usize;
        let bytes = self.reader.read_bytes(size)?;
        let s = self.code_page.encoding().decode(&bytes, DecoderTrap::Strict).unwrap();
        visitor.visit_string(s)
    }

    fn deserialize_bytes<V>(mut self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        let size = self.deserialize_size()? as usize;
        let bytes = self.reader.read_bytes(size)?;
        match bytes {
            Cow::Borrowed(bytes) => visitor.visit_borrowed_bytes(bytes),
            Cow::Owned(byte_buf) => visitor.visit_byte_buf(byte_buf)
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(mut self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        let size = self.deserialize_size()?;
        if size == 0 {
            visitor.visit_none()
        } else {
            visitor.visit_some(EslDeserializer { isolated: Some(size), ..self })
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(self, _: &'static str, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(self, _: &'static str, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(mut self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        let size = self.deserialize_size()?;
        visitor.visit_seq(SeqDeserializer {
            size, start_pos: self.reader.pos(),
            code_page: self.code_page,
            reader: self.reader, phantom: PhantomData
        })
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_seq(StructDeserializer {
            len,
            isolated: self.isolated.map(|size| (self.reader.pos(), size)),
            code_page: self.code_page,
            reader: self.reader,
            phantom: PhantomData,
        })
    }

    fn deserialize_tuple_struct<V>(self, _: &'static str, _len: usize, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!()
    }

    fn deserialize_map<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!()
    }

    fn deserialize_struct<V>(self, _: &'static str, _: &'static [&'static str], _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!()
    }

    fn deserialize_enum<V>(self, _: &'static str, _: &'static [&'static str], _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!()
    }

    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!()
    }

    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!()
    }
}