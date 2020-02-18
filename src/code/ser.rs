use serde::{Serializer, Serialize};
use std::mem::{replace};
use std::fmt::{self, Display, Debug};
use encoding::{EncoderTrap};
use serde::ser::{self, SerializeSeq, SerializeTuple, SerializeTupleStruct, SerializeStruct, SerializeTupleVariant, SerializeStructVariant, SerializeMap};
use std::io::{self, Write};
use byteorder::{WriteBytesExt, LittleEndian};

use crate::code::code_page::*;

#[derive(Debug)]
pub enum SerError {
    Custom(String),
    LargeObject(usize),
    UnrepresentableChar(char, CodePage),
    ZeroSizedLastSequenceElement,
    VariantIndexMismatch { variant_index: u32, variant_size: u32 },
    ZeroSizedOptional,
}

impl Display for SerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SerError::Custom(s) => Display::fmt(s, f),
            SerError::LargeObject(size) => write!(f, "object has too large size ({} B)", size),
            SerError::UnrepresentableChar(c, p) => write!(f, "the '{}' char is not representable in {:?} code page", c, p),
            SerError::ZeroSizedLastSequenceElement => write!(f, "last element in sequence or map cannot have zero size"),
            SerError::VariantIndexMismatch { variant_index, variant_size } =>
                write!(f, "variant index ({}) should be equal to variant size ({})", variant_index, variant_size),
            SerError::ZeroSizedOptional => write!(f, "optional element cannot have zero size"),
        }
    }
}

impl std::error::Error for SerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl ser::Error for SerError {
    fn custom<T: Display>(msg: T) -> Self { SerError::Custom(format!("{}", msg)) }
}

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

impl From<io::Error> for SerOrIoError {
    fn from(e: io::Error) -> SerOrIoError { SerOrIoError::Io(e) }
}

const SIZE_STUB: u32 = 0x1375F17B;

fn size(len: usize) -> Result<u32, SerError> {
    if len > u32::max_value() as usize {
        Err(SerError::LargeObject(len))
    } else {
        Ok(len as u32)
    }
}

pub trait Writer: Write {
    type Buf: Debug;
    
    fn pos(&self) -> usize;
    fn begin_isolate(&mut self) -> Self::Buf;
    fn end_isolate(&mut self, buf: Self::Buf, variadic_part_pos: usize) -> Result<(), SerOrIoError>;
}

impl Writer for Vec<u8> {
    type Buf = usize;

    fn pos(&self) -> usize { self.len() }

    fn begin_isolate(&mut self) -> Self::Buf {
        let value_size_stub_offset = self.len();
        self.write_u32::<LittleEndian>(SIZE_STUB).unwrap();
        value_size_stub_offset
    }

    fn end_isolate(&mut self, value_size_stub_offset: usize, value_offset: usize) -> Result<(), SerOrIoError> {
        let value_size = size(self.len() - value_offset)?;
        (&mut self[value_size_stub_offset..value_size_stub_offset + 4]).write_u32::<LittleEndian>(value_size).unwrap();
        Ok(())
    }
}

pub struct GenericWriter<'a, W: Write + ?Sized> {
    writer: &'a mut W,
    write_buf: Option<Vec<u8>>,
    pos: usize,
}

impl<'a, W: Write + ?Sized> GenericWriter<'a, W> {
    pub fn new(writer: &'a mut W) -> Self {
        GenericWriter {
            writer,
            write_buf: None,
            pos: 0
        }
    }
}

impl<'a, W: Write + ?Sized> Write for GenericWriter<'a, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.pos += buf.len();
        if let Some(write_buf) = &mut self.write_buf {
            write_buf.extend_from_slice(buf);
            Ok(buf.len())
        } else {
            self.writer.write(buf)
        }
    }
    
    fn flush(&mut self) -> io::Result<()> {
        if self.write_buf.is_none() {
            self.writer.flush()
        } else {
            Ok(())
        }
    }
}

impl<'a, W: Write + ?Sized> Writer for GenericWriter<'a, W> {
    type Buf = Option<usize>;

    fn pos(&self) -> usize { self.pos }

    fn begin_isolate(&mut self) -> Self::Buf {
         if let Some(write_buf) = &self.write_buf {
             let value_size_stub_offset = write_buf.len();
             self.write_u32::<LittleEndian>(SIZE_STUB).unwrap();
             Some(value_size_stub_offset)
         } else {
             self.pos += 4;
             self.write_buf = Some(Vec::new());
             None
         }
    }

    fn end_isolate(&mut self, value_size_stub_offset: Option<usize>, value_pos: usize) -> Result<(), SerOrIoError> {
        let value_size = size(self.pos - value_pos)?;
        if let Some(value_size_stub_offset) = value_size_stub_offset {
            let write_buf = self.write_buf.as_mut().unwrap();
            (&mut write_buf[value_size_stub_offset..value_size_stub_offset + 4]).write_u32::<LittleEndian>(value_size).unwrap();
        } else {
            let write_buf = self.write_buf.take().unwrap();
            self.writer.write_u32::<LittleEndian>(value_size).unwrap();
            self.writer.write_all(&write_buf[..])?;
        }
        Ok(())
    }
}
    
#[derive(Debug)]
pub struct EslSerializer<'r, 'a, W: Writer> {
    isolated: bool,
    code_page: CodePage,
    writer: &'a mut W,
    map_entry_value_buf: Option<&'r mut Option<W::Buf>>
}

impl<'r, 'a, W: Writer> EslSerializer<'r, 'a, W> {
    pub fn new(isolated: bool, code_page: CodePage, writer: &'a mut W) -> Self {
        EslSerializer { isolated, code_page, writer, map_entry_value_buf: None }
    }
}

#[derive(Debug)]
pub struct SeqSerializer<'a, W: Writer> {
    code_page: CodePage,
    writer: &'a mut W,
    last_element_has_zero_size: bool,
    buf_and_start_pos: Option<(W::Buf, usize)>,
}

#[derive(Debug)]
pub struct MapSerializer<'a, W: Writer> {
    code_page: CodePage,
    writer: &'a mut W,
    value_buf_and_pos: Option<(W::Buf, usize)>,
    buf_and_start_pos: Option<(W::Buf, usize)>,
}

#[derive(Debug)]
pub struct StructSerializer<'r, 'a, W: Writer> {
    code_page: CodePage,
    writer: &'a mut W,
    len: Option<usize>,
    start_pos_and_variant_index: Option<(usize, u32)>,
    value_buf: Option<&'r mut Option<W::Buf>>,
}

impl<'a, W: Writer> SerializeSeq for SeqSerializer<'a, W> {
    type Ok = ();
    type Error = SerOrIoError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        let element_pos = self.writer.pos();
        v.serialize(EslSerializer {
            isolated: false,
            writer: self.writer, code_page: self.code_page,
            map_entry_value_buf: None
        })?;
        self.last_element_has_zero_size = self.writer.pos() == element_pos;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        if self.last_element_has_zero_size {
            return Err(SerError::ZeroSizedLastSequenceElement.into());
        }
        if let Some((buf, start_pos)) = self.buf_and_start_pos {
            self.writer.end_isolate(buf, start_pos)?;
        }
        Ok(())
    }
}

impl<'a, W: Writer> SerializeMap for MapSerializer<'a, W> {
    type Ok = ();
    type Error = SerOrIoError;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
        let mut value_buf = None;
        key.serialize(EslSerializer {
            writer: self.writer,
            code_page: self.code_page,
            map_entry_value_buf: Some(&mut value_buf),
            isolated: false
        })?;
        let b = replace(&mut self.value_buf_and_pos, Some((if let Some(value_buf) = value_buf {
            value_buf
        } else {
            self.writer.begin_isolate()
        }, self.writer.pos())));
        assert!(b.is_none());
        Ok(())
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        v.serialize(EslSerializer {
            isolated: true,
            writer: self.writer,
            code_page: self.code_page,
            map_entry_value_buf: None
        })?;
        let (value_buf, value_pos) = self.value_buf_and_pos.take().unwrap();
        self.writer.end_isolate(value_buf, value_pos)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        if let Some((buf, start_pos)) = self.buf_and_start_pos {
            self.writer.end_isolate(buf, start_pos)?;
        }
        Ok(())
    }
}

impl<'r, 'a, W: Writer> StructSerializer<'r, 'a, W> {
    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), SerOrIoError> {
        if let Some(len) = self.len.as_mut() {
            if *len == 0 { panic!() }
            *len -= 1;
        }
        v.serialize(EslSerializer {
            isolated: self.len.map_or(false, |len| len == 0),
            writer: self.writer, code_page: self.code_page,
            map_entry_value_buf: None
        })?;
        if let &mut Some(ref mut value_buf) = &mut self.value_buf {
            if value_buf.is_none() {
                **value_buf = Some(self.writer.begin_isolate());
            }
        }
        Ok(())
    }

    fn end(self) -> Result<(), SerOrIoError> {
        if let Some((start_pos, variant_index)) = self.start_pos_and_variant_index {
            let variant_size = size(self.writer.pos() - start_pos)?;
            if variant_index != variant_size {
                return Err(SerError::VariantIndexMismatch { variant_index, variant_size }.into());
            }
        }
        Ok(())
    }
}

impl<'r, 'a, W: Writer> SerializeTuple for StructSerializer<'r, 'a, W> {
    type Ok = ();
    type Error = SerOrIoError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.serialize_element(v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end()
    }
}

impl<'r, 'a, W: Writer> SerializeTupleStruct for StructSerializer<'r, 'a, W> {
    type Ok = ();
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.serialize_element(v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end()
    }
}

impl<'r, 'a, W: Writer> SerializeStruct for StructSerializer<'r, 'a, W> {
    type Ok = ();
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.serialize_element(v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end()
    }
}

impl<'r, 'a, W: Writer> SerializeTupleVariant for StructSerializer<'r, 'a, W> {
    type Ok = ();
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.serialize_element(v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end()
    }
}

impl<'r, 'a, W: Writer> SerializeStructVariant for StructSerializer<'r, 'a, W> {
    type Ok = ();
    type Error = SerOrIoError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.serialize_element(v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end()
    }
}

fn bool_byte(v: bool) -> u8 {
    if v { 1 } else { 0 }
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

impl<'r, 'a, W: Writer> Serializer for EslSerializer<'r, 'a, W> {
    type Ok = ();
    type Error = SerOrIoError;
    type SerializeSeq = SeqSerializer<'a, W>;
    type SerializeTuple = StructSerializer<'r, 'a, W>;
    type SerializeTupleStruct = StructSerializer<'r, 'a, W>;
    type SerializeTupleVariant = StructSerializer<'r, 'a, W>;
    type SerializeStruct = StructSerializer<'r, 'a, W>;
    type SerializeStructVariant = StructSerializer<'r, 'a, W>;
    type SerializeMap = MapSerializer<'a, W>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(bool_byte(v))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        if !self.isolated {
            self.writer.write_u32::<LittleEndian>(size(v.len())?)?;
        }
        self.writer.write(v)?;
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.writer.write_u8(v)?;
        Ok(())
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.writer.write_i8(v)?;
        Ok(())
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.writer.write_u16::<LittleEndian>(v)?;
        Ok(())
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.writer.write_i16::<LittleEndian>(v)?;
        Ok(())
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.writer.write_u32::<LittleEndian>(v)?;
        Ok(())
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.writer.write_i32::<LittleEndian>(v)?;
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.writer.write_f32::<LittleEndian>(v)?;
        Ok(())
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.writer.write_u64::<LittleEndian>(v)?;
        Ok(())
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.writer.write_i64::<LittleEndian>(v)?;
        Ok(())
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.writer.write_f64::<LittleEndian>(v)?;
        Ok(())
    }

    serde_if_integer128! {
        fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
            self.writer.write_u128::<LittleEndian>(v)?;
            Ok(())
        }

        fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
            self.writer.write_i128::<LittleEndian>(v)?;
            Ok(())
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
        Ok(())
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }

    fn serialize_unit_variant(self, _: &'static str, variant_index: u32, _: &'static str)
        -> Result<Self::Ok, Self::Error> {

        if variant_index != 0 {
            return Err(SerError::VariantIndexMismatch { variant_index, variant_size: 0 }.into());
        }
        if !self.isolated {
            self.serialize_u32(0)
        } else {
            Ok(())
        }
    }

    fn serialize_newtype_struct<T: Serialize + ?Sized>(self, _: &'static str, v: &T)
        -> Result<Self::Ok, Self::Error> {

        v.serialize(EslSerializer { map_entry_value_buf: None, ..self })
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        if !self.isolated {
            self.writer.write_u32::<LittleEndian>(0)?;
        }
        Ok(())
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        let buf = if !self.isolated {
            Some(self.writer.begin_isolate())
        } else {
            None
        };
        let value_pos = self.writer.pos();
        value.serialize(EslSerializer {
            isolated: true,
            writer: self.writer,
            code_page: self.code_page,
            map_entry_value_buf: None
        })?;
        if self.writer.pos() == value_pos {
            return Err(SerError::ZeroSizedOptional.into());
        }
        if let Some(buf) = buf {
            self.writer.end_isolate(buf, value_pos)?;
        }
        Ok(())
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _: &'static str, variant_index: u32, _: &'static str, v: &T)
        -> Result<Self::Ok, Self::Error> {

        if !self.isolated {
            self.writer.write_u32::<LittleEndian>(variant_index)?;
        }
        let value_pos = self.writer.pos();
        v.serialize(EslSerializer {
            isolated: true,
            writer: self.writer,
            code_page: self.code_page,
            map_entry_value_buf: None
        })?;
        let variant_size = size(self.writer.pos() - value_pos)?;
        if variant_index != variant_size {
            return Err(SerError::VariantIndexMismatch { variant_index, variant_size }.into());
        }
        Ok(())
    }

    fn serialize_tuple_variant(self, _: &'static str, variant_index: u32, _: &'static str, len: usize)
        -> Result<Self::SerializeTupleVariant, Self::Error> {

        if !self.isolated {
            self.writer.write_u32::<LittleEndian>(variant_index)?;
        }
        Ok(StructSerializer {
            len: Some(len),
            start_pos_and_variant_index: Some((self.writer.pos(), variant_index)),
            writer: self.writer, code_page: self.code_page,
            value_buf: None
        })
    }

    fn serialize_struct_variant(self, _: &'static str, variant_index: u32, _: &'static str, len: usize)
        -> Result<Self::SerializeStructVariant, Self::Error> {

        if !self.isolated {
            self.writer.write_u32::<LittleEndian>(variant_index)?;
        }
        Ok(StructSerializer {
            len: Some(len),
            start_pos_and_variant_index: Some((self.writer.pos(), variant_index)),
            writer: self.writer, code_page: self.code_page,
            value_buf: None
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(StructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            start_pos_and_variant_index: None,
            value_buf: self.map_entry_value_buf
        })
    }

    fn serialize_tuple_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(StructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            start_pos_and_variant_index: None,
            value_buf: None
        })
    }

    fn serialize_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(StructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            start_pos_and_variant_index: None,
            value_buf: None
        })
    }

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        let buf = if !self.isolated { Some((self.writer.begin_isolate(), self.writer.pos())) } else { None };
        Ok(SeqSerializer {
            writer: self.writer,
            code_page: self.code_page,
            last_element_has_zero_size: false,
            buf_and_start_pos: buf
        })
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        let buf = if !self.isolated { Some((self.writer.begin_isolate(), self.writer.pos())) } else { None };
        Ok(MapSerializer {
            value_buf_and_pos: None,
            writer: self.writer, code_page: self.code_page,
            buf_and_start_pos: buf
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::code::ser::*;
    use serde::{Serialize, Serializer};
    use std::collections::HashMap;

    #[derive(Serialize)]
    struct Abcd {
        a: i16,
        b: char,
        c: u32,
        d: String
    }

    #[test]
    fn vec_serialize_struct() {
        let s = Abcd { a: 5, b: 'Ы', c: 90, d: "S".into() };
        let mut v = Vec::new();
        s.serialize(EslSerializer::new(true, CodePage::Russian, &mut v)).unwrap();
        assert_eq!(v, [5, 0, 219, 90, 0, 0, 0, 83]);
    }

    #[test]
    fn vec_serialize_struct_not_isolated() {
        let s = Abcd { a: 5, b: 'Ы', c: 90, d: "S".into() };
        let mut v = Vec::new();
        s.serialize(EslSerializer::new(false, CodePage::Russian, &mut v)).unwrap();
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
    fn vec_serialize_map() {
        let mut s = Map {
            map: HashMap::new(),
            unit: (),
            i: -3
        };
        s.map.insert(Key { variant: Variant::Variant2, s: "str".into() }, "value".into());
        let mut v = Vec::new();
        s.serialize(EslSerializer::new(true, CodePage::Russian, &mut v)).unwrap();
        assert_eq!(v, vec![
            17, 0, 0, 0, 2, 3, 0, 0, 0, 115, 116, 114,
            5, 0, 0, 0, 118, 97, 108, 117, 101,
            253
        ]);
    }

    #[test]
    fn vec_serialize_tuple_key() {
        let mut s: HashMap<(Key, Key), u64> = HashMap::new();
        s.insert((
            Key { variant: Variant::Variant2, s: "str".into() },
            Key { variant: Variant::Variant1, s: "стр".into() }
        ), 22);
        let mut v = Vec::new();
        s.serialize(EslSerializer::new(true, CodePage::Russian, &mut v)).unwrap();
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
    fn vec_serialize_newtype_key() {
        let mut s: HashMap<Key2, u64> = HashMap::new();
        s.insert(Key2((
            Key { variant: Variant::Variant2, s: "str".into() },
            Key { variant: Variant::Variant1, s: "стр".into() }
        )), 22);
        let mut v = Vec::new();
        s.serialize(EslSerializer::new(true, CodePage::Russian, &mut v)).unwrap();
        assert_eq!(v, vec![
            2, 3, 0, 0, 0, 115, 116, 114,
            1, 3, 0, 0, 0, 241, 242, 240,
            8, 0, 0, 0,
            22, 0, 0, 0, 0, 0, 0, 0
        ]);
    }

    #[test]
    fn serialize_struct() {
        let s = Abcd { a: 5, b: 'Ы', c: 90, d: "S".into() };
        let mut v = Vec::new();
        let mut w = GenericWriter { write_buf: None, writer: &mut v, pos: 0 };
        s.serialize(EslSerializer::new(true, CodePage::Russian, &mut w)).unwrap();
        assert_eq!(v, [5, 0, 219, 90, 0, 0, 0, 83]);
    }

    #[test]
    fn serialize_struct_not_isolated() {
        let s = Abcd { a: 5, b: 'Ы', c: 90, d: "S".into() };
        let mut v = Vec::new();
        let mut w = GenericWriter { write_buf: None, writer: &mut v, pos: 0 };
        s.serialize(EslSerializer::new(false, CodePage::Russian, &mut w)).unwrap();
        assert_eq!(v, [5, 0, 219, 90, 0, 0, 0, 1, 0, 0, 0, 83]);
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
        let mut w = GenericWriter { write_buf: None, writer: &mut v, pos: 0 };
        s.serialize(EslSerializer::new(true, CodePage::Russian, &mut w)).unwrap();
        assert_eq!(v, vec![
            17, 0, 0, 0, 2, 3, 0, 0, 0, 115, 116, 114,
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
        let mut w = GenericWriter { write_buf: None, writer: &mut v, pos: 0 };
        s.serialize(EslSerializer::new(true, CodePage::Russian, &mut w)).unwrap();
        assert_eq!(v, vec![
            2, 3, 0, 0, 0, 115, 116, 114,
            8, 0, 0, 0,
            1, 3, 0, 0, 0, 241, 242, 240,
            22, 0, 0, 0, 0, 0, 0, 0
        ]);
    }

    #[test]
    fn serialize_newtype_key() {
        let mut s: HashMap<Key2, u64> = HashMap::new();
        s.insert(Key2((
            Key { variant: Variant::Variant2, s: "str".into() },
            Key { variant: Variant::Variant1, s: "стр".into() }
        )), 22);
        let mut v = Vec::new();
        let mut w = GenericWriter { write_buf: None, writer: &mut v, pos: 0 };
        s.serialize(EslSerializer::new(true, CodePage::Russian, &mut w)).unwrap();
        assert_eq!(v, vec![
            2, 3, 0, 0, 0, 115, 116, 114,
            1, 3, 0, 0, 0, 241, 242, 240,
            8, 0, 0, 0,
            22, 0, 0, 0, 0, 0, 0, 0
        ]);
    }
}
