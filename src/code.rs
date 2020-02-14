use serde::{Serializer, Serialize};
use std::io::{self, Write};
use std::mem::{transmute};
use std::fmt::{self, Display};
use encoding::{EncoderTrap};
use serde::ser::{self, SerializeSeq, SerializeTuple, SerializeTupleStruct, SerializeStruct, SerializeTupleVariant, SerializeStructVariant, SerializeMap};
use either::{Either, Left, Right};

use crate::code_vec::*;

#[derive(Debug)]
pub enum SerOrIoError {
    Io(io::Error),
    Ser(SerError),
}

impl Display for SerOrIoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SerOrIoError::Io(e) => Display::fmt(e, f),
            SerOrIoError::Ser(e) => Display::fmt(e, f),
        }
    }
}

impl std::error::Error for SerOrIoError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            SerOrIoError::Io(e) => Some(e),
            SerOrIoError::Ser(e) => Some(e),
        }
    }
}

impl ser::Error for SerOrIoError {
    fn custom<T: Display>(msg: T) -> Self { SerOrIoError::Ser(SerError::custom(msg)) }
}

impl From<SerError> for SerOrIoError {
    fn from(e: SerError) -> SerOrIoError { SerOrIoError::Ser(e) }
}

#[derive(Debug)]
struct EslSerializer<'a, W: Write + ?Sized> {
    isolated: bool,
    code_page: CodePage,
    writer: &'a mut W
}

#[derive(Debug)]
struct SeqSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    last_element_has_zero_size: bool,
    buf: Either<usize, Vec<u8>>,
}

#[derive(Debug)]
struct StructSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    len: Option<usize>,
    size: usize
}

#[derive(Debug)]
struct KeySeqSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    last_element_has_zero_size: bool,
    buf: Vec<u8>,
}

#[derive(Debug)]
struct BaseMapSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    key_tail: Option<Vec<u8>>,
    size: usize
}

#[derive(Debug)]
struct MapSerializer<'a, W: Write + ?Sized>(BaseMapSerializer<'a, W>);

#[derive(Debug)]
struct KeyMapSerializer<'a, W: Write + ?Sized>(BaseMapSerializer<'a, W>);

#[derive(Debug)]
struct KeyStructSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    size: usize
}

#[derive(Debug)]
struct KeyTupleSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    tail: Option<Vec<u8>>,
    size: usize
}

#[derive(Debug)]
struct BaseStructVariantSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    variant_index: u32,
    size: usize
}

#[derive(Debug)]
struct StructVariantSerializer<'a, W: Write + ?Sized> {
    base: BaseStructVariantSerializer<'a, W>,
    len: Option<usize>,
}

#[derive(Debug)]
struct KeyStructVariantSerializer<'a, W: Write + ?Sized>(BaseStructVariantSerializer<'a, W>);

#[derive(Debug)]
struct KeySerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
}

fn serialize_u8(writer: &mut (impl Write + ?Sized), v: u8) -> Result<(), SerOrIoError> {
    writer.write_all(&[v]).map_err(SerOrIoError::Io)
}

fn serialize_u32(writer: &mut (impl Write + ?Sized), v: u32) -> Result<(), SerOrIoError> {
    writer.write_all(&[
        (v & 0xFF) as u8,
        ((v >> 8) & 0xFF) as u8,
        ((v >> 16) & 0xFF) as u8,
        (v >> 24) as u8
    ]).map_err(SerOrIoError::Io)
}

fn serialize_bytes(writer: &mut (impl Write + ?Sized), v: &[u8]) -> Result<(), SerOrIoError> {
    writer.write_all(v).map_err(SerOrIoError::Io)
}

fn size(len: usize) -> Result<u32, SerError> {
    if len > u32::max_value() as usize {
        Err(SerError::LargeObject(len))
    } else {
        Ok(len as u32)
    }
}

impl<'a, W: Write + ?Sized> SerializeSeq for SeqSerializer<'a, W> {
    type Ok = usize;
    type Error = SerOrIoError;
    
    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        let size = match self.buf.as_mut() {
            Left(self_size) => {
                let size = v.serialize(EslSerializer {
                    isolated: false,
                    writer: self.writer, code_page: self.code_page,
                })?;
                *self_size += size;
                size
            },
            Right(buf) => {
                v.serialize(EslSerializer {
                    isolated: false,
                    writer: buf,
                    code_page: self.code_page
                })?
            }
        };
        self.last_element_has_zero_size = size == 0;
        Ok(())
    }
    
    fn end(self) -> Result<Self::Ok, Self::Error> {
        if self.last_element_has_zero_size {
            return Err(SerError::ZeroSizedLastSequenceElement.into());
        }
        match self.buf.as_ref() {
            Left(&size) => Ok(size),
            Right(buf) => {
                serialize_u32(self.writer, size(buf.len())?)?;
                serialize_bytes(self.writer, &buf)?;
                Ok(4 + buf.len())
            }
        }
    }
}

impl<'a, W: Write + ?Sized> SerializeSeq for KeySeqSerializer<'a, W> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = SerOrIoError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.last_element_has_zero_size = v.serialize(EslSerializer {
            isolated: false,
            writer: &mut self.buf,
            code_page: self.code_page
        })? == 0;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        if self.last_element_has_zero_size {
            return Err(SerError::ZeroSizedLastSequenceElement.into());
        }
        serialize_u32(self.writer, size(self.buf.len())?)?;
        serialize_bytes(self.writer, &self.buf)?;
        Ok((None, 4 + self.buf.len()))
    }
}

impl<'a, W: Write + ?Sized> BaseMapSerializer<'a, W> {
    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), SerOrIoError> {
        let (buf, size) = key.serialize(KeySerializer { 
            writer: self.writer,
            code_page: self.code_page
        })?;
        self.size += size;
        self.key_tail = buf;
        Ok(())
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), SerOrIoError> {
        let mut value = Vec::new();
        v.serialize(EslSerializer {
            isolated: true,
            writer: &mut value,
            code_page: self.code_page
        })?;
        serialize_u32(self.writer, size(value.len())?)?;
        if let Some(key_tail) = &self.key_tail {
            serialize_bytes(self.writer, key_tail)?;
        }
        serialize_bytes(self.writer, &value)?;
        self.size += 4 + value.len();
        Ok(())
    }
}

impl<'a, W: Write + ?Sized> SerializeMap for MapSerializer<'a, W> {
    type Ok = usize;
    type Error = SerOrIoError;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.0.serialize_key(key)
    }
    
    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.0.serialize_value(v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.0.size)
    }
}

impl<'a, W: Write + ?Sized> SerializeMap for KeyMapSerializer<'a, W> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = SerOrIoError;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.0.serialize_key(key)
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.0.serialize_value(v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok((None, self.0.size))
    }
}

fn serialize_field<T: Serialize + ?Sized>(writer: &mut (impl Write + ?Sized), code_page: CodePage, len: &mut Option<usize>, v: &T)
    -> Result<usize, SerOrIoError> {
    
    len.as_mut().map(|len| {
        if *len == 0 { panic!() }
        *len -= 1;
    });
    v.serialize(EslSerializer {
        isolated: len.map_or(false, |len| len == 0),
        writer, code_page
    })
}

fn serialize_key_field<T: Serialize + ?Sized>(writer: &mut (impl Write + ?Sized), code_page: CodePage, v: &T)
    -> Result<usize, SerOrIoError> {

    v.serialize(EslSerializer {
        isolated: false,
        writer, code_page
    })
}

impl<'a, W: Write + ?Sized> SerializeTuple for StructSerializer<'a, W> {
    type Ok = usize;
    type Error = SerOrIoError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_field(self.writer, self.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.size)
    }
}

impl<'a, W: Write + ?Sized> SerializeTupleStruct for StructSerializer<'a, W> {
    type Ok = usize;
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_field(self.writer, self.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.size)
    }
}

impl<'a, W: Write + ?Sized> SerializeStruct for StructSerializer<'a, W> {
    type Ok = usize;
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_field(self.writer, self.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.size)
    }
}

impl<'a, W: Write + ?Sized> SerializeTuple for KeyTupleSerializer<'a, W> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = SerOrIoError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.size += if let Some(tail) = self.tail.as_mut() {
            serialize_key_field(tail, self.code_page, v)
        } else {
            self.tail = Some(Vec::new());
            serialize_key_field(self.writer, self.code_page, v)
        }?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok((self.tail, self.size))
    }
}

impl<'a, W: Write + ?Sized> SerializeStruct for KeyStructSerializer<'a, W> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_key_field(self.writer, self.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok((None, self.size))
    }
}

impl<'a, W: Write + ?Sized> SerializeTupleStruct for KeyStructSerializer<'a, W> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_key_field(self.writer, self.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok((None, self.size))
    }
}

impl<'a, W: Write + ?Sized> BaseStructVariantSerializer<'a, W> {
    fn end(self) -> Result<usize, SerOrIoError> {
        let variant_size = size(self.size)?;
        if self.variant_index != variant_size {
            return Err(SerError::VariantIndexMismatch { variant_index: self.variant_index, variant_size }.into());
        }
        Ok(self.size)
    }
}

impl<'a, W: Write + ?Sized> SerializeTupleVariant for StructVariantSerializer<'a, W> {
    type Ok = usize;
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.base.size += serialize_field(self.base.writer, self.base.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.base.end()
    }
}

impl<'a, W: Write + ?Sized> SerializeStructVariant for StructVariantSerializer<'a, W> {
    type Ok = usize;
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.base.size += serialize_field(self.base.writer, self.base.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.base.end()
    }
}

impl<'a, W: Write + ?Sized> SerializeTupleVariant for KeyStructVariantSerializer<'a, W> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.0.size += serialize_key_field(self.0.writer, self.0.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok((None, self.0.end()?))
    }
}

impl<'a, W: Write + ?Sized> SerializeStructVariant for KeyStructVariantSerializer<'a, W> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.0.size += serialize_key_field(self.0.writer, self.0.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok((None, self.0.end()?))
    }
}

fn bool_byte(v: bool) -> u8 {
    if v { 1 } else { 0 }
}

fn serialize_u16(writer: &mut (impl Write + ?Sized), v: u16) -> Result<(), SerOrIoError> {
    writer.write_all(&[(v & 0xFF) as u8, (v >> 8) as u8]).map_err(SerOrIoError::Io)
}

fn serialize_u64(writer: &mut (impl Write + ?Sized), v: u64) -> Result<(), SerOrIoError> {
    writer.write_all(&[
        (v & 0xFF) as u8,
        ((v >> 8) & 0xFF) as u8,
        ((v >> 16) & 0xFF) as u8,
        ((v >> 24) & 0xFF) as u8,
        ((v >> 32) & 0xFF) as u8,
        ((v >> 40) & 0xFF) as u8,
        ((v >> 48) & 0xFF) as u8,
        (v >> 56) as u8
    ]).map_err(SerOrIoError::Io)
}

serde_if_integer128! {
    fn serialize_u128(writer: &mut (impl Write + ?Sized), v: u128) -> Result<(), SerOrIoError> {
        writer.write_all(&[
            (v & 0xFF) as u8,
            ((v >> 8) & 0xFF) as u8,
            ((v >> 16) & 0xFF) as u8,
            ((v >> 24) & 0xFF) as u8,
            ((v >> 32) & 0xFF) as u8,
            ((v >> 40) & 0xFF) as u8,
            ((v >> 48) & 0xFF) as u8,
            ((v >> 56) & 0xFF) as u8,
            ((v >> 64) & 0xFF) as u8,
            ((v >> 72) & 0xFF) as u8,
            ((v >> 80) & 0xFF) as u8,
            ((v >> 88) & 0xFF) as u8,
            ((v >> 96) & 0xFF) as u8,
            ((v >> 104) & 0xFF) as u8,
            ((v >> 112) & 0xFF) as u8,
            (v >> 120) as u8
        ]).map_err(SerOrIoError::Io)
    }
}

fn char_byte(code_page: CodePage, v: char) -> Result<u8, SerError> {
    let v = code_page.encoding()
        .encode(&v.to_string(), EncoderTrap::Strict)
        .map_err(|_| SerError::UnrepresentableChar(v, code_page))?;
    debug_assert_eq!(v.len(), 1);
    Ok(v[0])
}

fn str_bytes(code_page: CodePage, v: &str) -> Result<Vec<u8>, SerError> {
    code_page.encoding()
        .encode(v, EncoderTrap::Strict)
        .map_err(|s| SerError::UnrepresentableChar(s.chars().nth(0).unwrap(), code_page))
}

impl<'a, W: Write + ?Sized> Serializer for EslSerializer<'a, W> {
    type Ok = usize;
    type Error = SerOrIoError;
    type SerializeSeq = SeqSerializer<'a, W>;
    type SerializeTuple = StructSerializer<'a, W>;
    type SerializeTupleStruct = StructSerializer<'a, W>;
    type SerializeTupleVariant = StructVariantSerializer<'a, W>;
    type SerializeStruct = StructSerializer<'a, W>;
    type SerializeStructVariant = StructVariantSerializer<'a, W>;
    type SerializeMap = MapSerializer<'a, W>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(bool_byte(v))
    }
    
    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        if !self.isolated {
            serialize_u32(self.writer, size(v.len())?)?;
        }
        serialize_bytes(self.writer, v)?;
        Ok(if self.isolated { 0 } else { 4 } + v.len())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        serialize_u8(self.writer, v)?;
        Ok(1)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(unsafe { transmute(v) })
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        serialize_u16(self.writer, v)?;
        Ok(2)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u16(unsafe { transmute(v) })
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        serialize_u32(self.writer, v)?;
        Ok(4)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        serialize_u64(self.writer, v)?;
        Ok(8)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(unsafe { transmute(v) })
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(unsafe { transmute(v) })
    }

    serde_if_integer128! {
        fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
            serialize_u128(self.writer, v)?;
            Ok(16)
        }

        fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
            self.serialize_u128(unsafe { transmute(v) })
        }
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        let byte = char_byte(self.code_page, v)?;
        self.serialize_u8(byte)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        let bytes = str_bytes(self.code_page, v)?;
        self.serialize_bytes(&bytes)
    }
    
    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(0)
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(0)
    }

    fn serialize_unit_variant(self, _: &'static str, variant_index: u32, _: &'static str)
        -> Result<Self::Ok, Self::Error> {

        if variant_index != 0 { 
            return Err(SerError::VariantIndexMismatch { variant_index, variant_size: 0 }.into());
        }
        if !self.isolated {
            self.serialize_u32(0)
        } else {
            Ok(0)
        }
    }

    fn serialize_newtype_struct<T: Serialize + ?Sized>(self, _: &'static str, v: &T)
        -> Result<Self::Ok, Self::Error> {

        v.serialize(self)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        if !self.isolated {
            serialize_u32(self.writer, 0)?;
        }
        Ok(if self.isolated { 0 } else { 4 })
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        let mut bytes = Vec::new(); 
        if value.serialize(EslSerializer {
            isolated: true,
            writer: &mut bytes,
            code_page: self.code_page
        })? == 0 {
            return Err(SerError::ZeroSizedOptional.into());
        }
        self.serialize_bytes(&bytes)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _: &'static str, variant_index: u32, _: &'static str, v: &T) 
        -> Result<Self::Ok, Self::Error> {

        if !self.isolated {
            serialize_u32(self.writer, variant_index)?;
        }
        let v_size = v.serialize(EslSerializer {
            isolated: true,
            writer: self.writer,
            code_page: self.code_page
        })?;
        let variant_size = size(v_size)?;
        if variant_index != variant_size {
            return Err(SerError::VariantIndexMismatch { variant_index, variant_size }.into());
        }
        Ok(if self.isolated { 0 } else { 4 } + v_size)
    }

    fn serialize_tuple_variant(self, _: &'static str, variant_index: u32, _: &'static str, len: usize) 
        -> Result<Self::SerializeTupleVariant, Self::Error> {

        if !self.isolated {
            serialize_u32(self.writer, variant_index)?;
        }
        Ok(StructVariantSerializer {
            len: if self.isolated { Some(len) } else { None },
            base: BaseStructVariantSerializer {
                variant_index,
                writer: self.writer, code_page: self.code_page,
                size: 0
            }
        })
    }

    fn serialize_struct_variant(self, _: &'static str, variant_index: u32, _: &'static str, len: usize)
        -> Result<Self::SerializeStructVariant, Self::Error> {

        if !self.isolated {
            serialize_u32(self.writer, variant_index)?;
        }
        Ok(StructVariantSerializer {
            len: if self.isolated { Some(len) } else { None },
            base: BaseStructVariantSerializer {
                variant_index,
                writer: self.writer, code_page: self.code_page,
                size: 0
            }
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(StructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_tuple_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(StructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(StructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(SeqSerializer { 
            last_element_has_zero_size: false,
            buf: if self.isolated { Left(0) } else { Right(Vec::new()) },
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(MapSerializer(BaseMapSerializer {
            key_tail: None,
            writer: self.writer, code_page: self.code_page,
            size: 0
        }))
    }
}

impl<'a, W: Write + ?Sized> Serializer for KeySerializer<'a, W> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = SerOrIoError;
    type SerializeSeq = KeySeqSerializer<'a, W>;
    type SerializeTuple = KeyTupleSerializer<'a, W>;
    type SerializeTupleStruct = KeyStructSerializer<'a, W>;
    type SerializeTupleVariant = KeyStructVariantSerializer<'a, W>;
    type SerializeStruct = KeyStructSerializer<'a, W>;
    type SerializeStructVariant = KeyStructVariantSerializer<'a, W>;
    type SerializeMap = KeyMapSerializer<'a, W>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(bool_byte(v))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        serialize_u32(self.writer, size(v.len())?)?;
        serialize_bytes(self.writer, v)?;
        Ok((None, 4 + v.len()))
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        serialize_u8(self.writer, v)?;
        Ok((None, 1))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(unsafe { transmute(v) })
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        serialize_u16(self.writer, v)?;
        Ok((None, 2))
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u16(unsafe { transmute(v) })
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        serialize_u32(self.writer, v)?;
        Ok((None, 4))
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        serialize_u64(self.writer, v)?;
        Ok((None, 8))
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(unsafe { transmute(v) })
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.serialize_u64(unsafe { transmute(v) })
    }

    serde_if_integer128! {
        fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
            serialize_u128(self.writer, v)?;
            Ok((None, 16))
        }

        fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
            self.serialize_u128(unsafe { transmute(v) })
        }
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        let byte = char_byte(self.code_page, v)?;
        self.serialize_u8(byte)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        let bytes = str_bytes(self.code_page, v)?;
        self.serialize_bytes(&bytes)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok((None, 0))
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok((None, 0))
    }

    fn serialize_newtype_struct<T: Serialize + ?Sized>(self, _: &'static str, v: &T)
        -> Result<Self::Ok, Self::Error> {

        let size = v.serialize(EslSerializer {
            isolated: false,
            writer: self.writer, code_page: self.code_page
        })?;
        Ok((None, size))
    }

    fn serialize_unit_variant(self, _: &'static str, variant_index: u32, _: &'static str)
        -> Result<Self::Ok, Self::Error> {

        if variant_index != 0 {
            return Err(SerError::VariantIndexMismatch { variant_index, variant_size: 0 }.into());
        }
        self.serialize_u32(0)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(0)
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        let mut bytes = Vec::new();
        if value.serialize(EslSerializer {
            isolated: true,
            writer: &mut bytes,
            code_page: self.code_page
        })? == 0 {
            return Err(SerError::ZeroSizedOptional.into());
        }
        self.serialize_bytes(&bytes)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _: &'static str, variant_index: u32, _: &'static str, v: &T)
        -> Result<Self::Ok, Self::Error> {

        serialize_u32(self.writer, variant_index)?;
        let v_size = v.serialize(EslSerializer {
            isolated: true,
            writer: self.writer,
            code_page: self.code_page
        })?;
        let variant_size = size(v_size)?;
        if variant_index != variant_size {
            return Err(SerError::VariantIndexMismatch { variant_index, variant_size }.into());
        }
        Ok((None, 4 + v_size))
    }

    fn serialize_tuple_variant(self, _: &'static str, variant_index: u32, _: &'static str, _: usize)
        -> Result<Self::SerializeTupleVariant, Self::Error> {

        serialize_u32(self.writer, variant_index)?;
        Ok(KeyStructVariantSerializer(BaseStructVariantSerializer { writer: self.writer, code_page: self.code_page, variant_index, size: 0}))
    }

    fn serialize_struct_variant(self, _: &'static str, variant_index: u32, _: &'static str, _: usize)
        -> Result<Self::SerializeStructVariant, Self::Error> {

        serialize_u32(self.writer, variant_index)?;
        Ok(KeyStructVariantSerializer(BaseStructVariantSerializer { writer: self.writer, code_page: self.code_page, variant_index, size: 0}))
    }

    fn serialize_tuple(self, _: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(KeyTupleSerializer {
            writer: self.writer, code_page: self.code_page,
            tail: None,
            size: 0
        })
    }

    fn serialize_tuple_struct(self, _: &'static str, _: usize) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(KeyStructSerializer {
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_struct(self, _: &'static str, _: usize) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(KeyStructSerializer {
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(KeySeqSerializer {
            last_element_has_zero_size: false,
            buf: Vec::new(),
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(KeyMapSerializer(BaseMapSerializer {
            key_tail: None,
            writer: self.writer, code_page: self.code_page,
            size: 0
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::code::*;
    use encoding::{DecoderTrap, EncoderTrap};
    use serde::{Serialize, Serializer};
    use std::collections::HashMap;

    #[test]
    fn all_code_pages_are_single_byte_encodings() {
        for code_page in CodePage::iter_variants() {
            let encoding = code_page.encoding();
            for byte in 0u8 ..= 255 {
                let c = encoding.decode(&[byte], DecoderTrap::Strict).unwrap();
                let b = encoding.encode(&c, EncoderTrap::Strict).unwrap();
                assert_eq!(b.len(), 1);
                assert_eq!(b[0], byte);
            }
            for byte in 0u8 .. 128 {
                let c = encoding.decode(&[byte], DecoderTrap::Strict).unwrap();
                assert_eq!(c.len(), 1);
                assert_eq!(byte as u32, c.as_bytes()[0] as u32);
            }
        }
    }
    
    #[derive(Serialize)]
    struct Abcd {
        a: i16,
        b: char,
        c: u32,
        d: String
    }
    
    #[test]
    fn serialize_struct() {
        let s = Abcd { a: 5, b: 'Ы', c: 90, d: "S".into() };
        let mut v = Vec::new();
        s.serialize(EslSerializer {
            isolated: true,
            code_page: CodePage::Russian,
            writer: &mut v
        }).unwrap();
        assert_eq!(v, [5, 0, 219, 90, 0, 0, 0, 83]);
    }

    #[test]
    fn serialize_struct_not_isolated() {
        let s = Abcd { a: 5, b: 'Ы', c: 90, d: "S".into() };
        let mut v = Vec::new();
        s.serialize(EslSerializer {
            isolated: false,
            code_page: CodePage::Russian,
            writer: &mut v
        }).unwrap();
        assert_eq!(v, [5, 0, 219, 90, 0, 0, 0, 1, 0, 0, 0, 83]);
    }

    #[derive(Hash, Eq, PartialEq)]
    enum Variant { Variant1, Variant2 }
    
    impl Serialize for Variant {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
            serializer.serialize_u8(match self {
                Variant::Variant1 => 1,
                Variant::Variant2 => 2
            })
        }
    }

    #[derive(Serialize, Hash, Eq, PartialEq)]
    struct Key {
        variant: Variant,
        s: String
    }    
    
    #[derive(Serialize)]
    struct Map {
        map: HashMap<Key, String>,
        unit: (),
        i: i8
    }
    
    #[test]
    fn serialize_map() {
        let mut s = Map {
            map: HashMap::new(),
            unit: (),
            i: -3
        };
        s.map.insert(Key { variant: Variant::Variant2, s: "str".into() }, "value".into());
        let mut v = Vec::new();
        s.serialize(EslSerializer {
            isolated: true,
            code_page: CodePage::Russian,
            writer: &mut v
        }).unwrap();
        assert_eq!(v, vec![
            2, 3, 0, 0, 0, 115, 116, 114,
            5, 0, 0, 0, 118, 97, 108, 117, 101,
            253
        ]);
    }

    #[test]
    fn serialize_tuple_key() {
        let mut s: HashMap<(Key, Key), u64> = HashMap::new();
        s.insert((
            Key { variant: Variant::Variant2, s: "str".into() },
            Key { variant: Variant::Variant1, s: "стр".into() }
        ), 22);
        let mut v = Vec::new();
        s.serialize(EslSerializer {
            isolated: true,
            code_page: CodePage::Russian,
            writer: &mut v
        }).unwrap();
        assert_eq!(v, vec![
            2, 3, 0, 0, 0, 115, 116, 114,
            8, 0, 0, 0,
            1, 3, 0, 0, 0, 241, 242, 240,
            22, 0, 0, 0, 0, 0, 0, 0
        ]);
    }

    #[derive(Serialize, Hash, Eq, PartialEq)]
    struct Key2((Key, Key));
    
    #[test]
    fn serialize_newtype_key() {
        let mut s: HashMap<Key2, u64> = HashMap::new();
        s.insert(Key2((
            Key { variant: Variant::Variant2, s: "str".into() },
            Key { variant: Variant::Variant1, s: "стр".into() }
        )), 22);
        let mut v = Vec::new();
        s.serialize(EslSerializer {
            isolated: true,
            code_page: CodePage::Russian,
            writer: &mut v
        }).unwrap();
        assert_eq!(v, vec![
            2, 3, 0, 0, 0, 115, 116, 114,
            1, 3, 0, 0, 0, 241, 242, 240,
            8, 0, 0, 0,
            22, 0, 0, 0, 0, 0, 0, 0
        ]);
    }
}
