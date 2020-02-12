use serde::{Serializer, Serialize};
use std::io::{self, Write};
use std::mem::{transmute, replace};
use std::fmt::{self, Display};
use encoding::{Encoding, EncoderTrap};
use encoding::all::{WINDOWS_1251, WINDOWS_1252};
use serde::ser::{self, SerializeSeq, SerializeTuple, SerializeTupleStruct, SerializeStruct, SerializeTupleVariant, SerializeStructVariant, SerializeMap};

macro_attr! {
    #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[derive(IterVariants!(CodePageVariants))]
    pub enum CodePage {
        English,
        Russian,
    }
}

impl CodePage {
    pub fn encoding(self) -> &'static dyn Encoding {
        match self {
            CodePage::English => WINDOWS_1252,
            CodePage::Russian => WINDOWS_1251,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    LargeObject(usize),
    UnrepresentableChar(char, CodePage),
    LastSequenceElementHasZeroSize,
    Custom(String)
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::IoError(e) => Display::fmt(e, f),
            Error::LargeObject(size) => write!(f, "object has too large size ({} B)", size),
            Error::UnrepresentableChar(c, p) => 
                write!(f, "the '{}' char is not representable in {:?} code page", c, p),
            Error::LastSequenceElementHasZeroSize =>
                write!(f, "last element in sequence or map cannot havezero size"),
            Error::Custom(s) => Display::fmt(s, f)
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        if let Error::IoError(e) = self {
            Some(e)
        } else {
            None
        }
    }
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self { Error::Custom(format!("{}", msg)) }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum SeqState {
    NoElements,
    LastElementHasZeroSize,
    LastElementHasNonZeroSize
}

#[derive(Debug)]
struct TesSerializer<'a, W: Write + ?Sized> {
    isolated: bool,
    code_page: CodePage,
    writer: &'a mut W
}

#[derive(Debug)]
struct SeqSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    state: SeqState,
    buf: Option<Vec<u8>>
}

#[derive(Debug)]
struct StructSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    result: bool,
    len: Option<usize>
}

#[derive(Debug)]
struct StructVariantSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    len: Option<usize>
}

#[derive(Debug)]
struct KeySeqSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    last_element_has_zero_size: bool,
    buf: Vec<u8>
}

#[derive(Debug)]
struct BaseMapSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    value: Vec<u8>,
    key: Option<Vec<u8>>
}

#[derive(Debug)]
struct MapSerializer<'a, W: Write + ?Sized> {
    base: BaseMapSerializer<'a, W>,
    has_elements: bool,
}

#[derive(Debug)]
struct KeyMapSerializer<'a, W: Write + ?Sized>(BaseMapSerializer<'a, W>);

#[derive(Debug)]
struct KeyStructSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
}

#[derive(Debug)]
struct KeyTupleSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
    result: Option<Vec<u8>>,
}

#[derive(Debug)]
struct KeyStructVariantSerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
}

#[derive(Debug)]
struct KeySerializer<'a, W: Write + ?Sized> {
    code_page: CodePage,
    writer: &'a mut W,
}

fn serialize_u8(writer: &mut (impl Write + ?Sized), v: u8) -> Result<(), Error> {
    writer.write_all(&[v]).map_err(Error::IoError)
}

fn serialize_u32(writer: &mut (impl Write + ?Sized), v: u32) -> Result<(), Error> {
    writer.write_all(&[
        (v & 0xFF) as u8,
        ((v >> 8) & 0xFF) as u8,
        ((v >> 16) & 0xFF) as u8,
        (v >> 24) as u8
    ]).map_err(Error::IoError)
}

fn serialize_bytes(writer: &mut (impl Write + ?Sized), v: &[u8]) -> Result<(), Error> {
    writer.write_all(v).map_err(Error::IoError)
}

fn size(len: usize) -> Result<u32, Error> {
    if len > u32::max_value() as usize {
        Err(Error::LargeObject(len))
    } else {
        Ok(len as u32)
    }
}

impl<'a, W: Write + ?Sized> SerializeSeq for SeqSerializer<'a, W> {
    type Ok = bool;
    type Error = Error;
    
    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        let has_size = if let Some(buf) = self.buf.as_mut() {
            v.serialize(TesSerializer {
                writer: buf,
                isolated: false,
                code_page: self.code_page
            })
        } else {
            v.serialize(TesSerializer {
                isolated: false,
                code_page: self.code_page,
                writer: self.writer
            })
        }?;
        self.state = if has_size {
            SeqState::LastElementHasNonZeroSize
        } else {
            SeqState::LastElementHasZeroSize
        };
        Ok(())
    }
    
    fn end(self) -> Result<Self::Ok, Self::Error> {
        if self.state == SeqState::LastElementHasZeroSize {
            return Err(Error::LastSequenceElementHasZeroSize);
        }
        if let Some(buf) = &self.buf {
            serialize_u32(self.writer, size(buf.len())?)?;
            serialize_bytes(self.writer, &buf)?;
            Ok(true)
        } else {
            Ok(self.state != SeqState::NoElements)
        }
    }
}

impl<'a, W: Write + ?Sized> SerializeSeq for KeySeqSerializer<'a, W> {
    type Ok = Vec<u8>;
    type Error = Error;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.last_element_has_zero_size = !v.serialize(TesSerializer {
            writer: &mut self.buf,
            isolated: false,
            code_page: self.code_page
        })?;
        Ok(())
    }

    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        if self.last_element_has_zero_size {
            return Err(Error::LastSequenceElementHasZeroSize);
        }
        serialize_u32(self.writer, size(self.buf.len())?)?;
        serialize_bytes(self.writer, &self.buf)?;
        self.buf.clear();
        Ok(self.buf)
    }
}

impl<'a, W: Write + ?Sized> BaseMapSerializer<'a, W> {
    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Error> {
        if self.key.replace(key.serialize(KeySerializer {
            code_page: self.code_page,
            writer: self.writer
        })?).is_some() {
            panic!()
        }
        Ok(())
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Error> {
        let key = self.key.take().unwrap();
        v.serialize(TesSerializer {
            writer: &mut self.value,
            isolated: true,
            code_page: self.code_page
        })?;
        serialize_u32(self.writer, size(self.value.len())?)?;
        serialize_bytes(self.writer, &key)?;
        serialize_bytes(self.writer, &self.value)?;
        self.value.clear();
        Ok(())
    }

    fn end(&self) {
        assert!(self.key.is_none() && self.value.is_empty());
    }
}

impl<'a, W: Write + ?Sized> SerializeMap for MapSerializer<'a, W> {
    type Ok = bool;
    type Error = Error;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.has_elements = true;
        self.base.serialize_key(key)
    }
    
    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.base.serialize_value(v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.base.end();
        Ok(self.has_elements)
    }
}

impl<'a, W: Write + ?Sized> SerializeMap for KeyMapSerializer<'a, W> {
    type Ok = Vec<u8>;
    type Error = Error;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.0.serialize_key(key)
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.0.serialize_value(v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.0.end();
        Ok(self.0.value)
    }
}

fn serialize_field<T: Serialize + ?Sized>(len: &mut Option<usize>, writer: &mut (impl Write + ?Sized), code_page: CodePage, v: &T)
    -> Result<bool, Error> {
    
    len.as_mut().map(|len| {
        if *len == 0 { panic!() }
        replace(len, *len - 1);
    });
    v.serialize(TesSerializer {
        isolated: len.map_or(false, |len| len == 0),
        writer, code_page
    })
}

impl<'a, W: Write + ?Sized> SerializeTuple for StructSerializer<'a, W> {
    type Ok = bool;
    type Error = Error;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.result |= serialize_field(&mut self.len, self.writer, self.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.result)
    }
}

impl<'a, W: Write + ?Sized> SerializeTupleStruct for StructSerializer<'a, W> {
    type Ok = bool;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.result |= serialize_field(&mut self.len, self.writer, self.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.result)
    }
}

impl<'a, W: Write + ?Sized> SerializeStruct for StructSerializer<'a, W> {
    type Ok = bool;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.result |= serialize_field(&mut self.len, self.writer, self.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.result)
    }
}

fn serialize_key_field<T: Serialize + ?Sized>(writer: &mut (impl Write + ?Sized), code_page: CodePage, v: &T)
    -> Result<(), Error> {

    v.serialize(TesSerializer {
        isolated: false,
        writer, code_page
    })?;
    Ok(())
}

impl<'a, W: Write + ?Sized> SerializeTuple for KeyTupleSerializer<'a, W> {
    type Ok = Vec<u8>;
    type Error = Error;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        if let Some(result) = self.result.as_mut() {
            serialize_key_field(result, self.code_page, v)
        } else {
            self.result = Some(Vec::new());
            serialize_key_field(self.writer, self.code_page, v)
        }
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.result.unwrap_or(Vec::new()))
    }
}

impl<'a, W: Write + ?Sized> SerializeStruct for KeyStructSerializer<'a, W> {
    type Ok = Vec<u8>;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        serialize_key_field(self.writer, self.code_page, v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Vec::new())
    }
}

impl<'a, W: Write + ?Sized> SerializeTupleStruct for KeyStructSerializer<'a, W> {
    type Ok = Vec<u8>;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        serialize_key_field(self.writer, self.code_page, v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Vec::new())
    }
}

impl<'a, W: Write + ?Sized> SerializeTupleVariant for StructVariantSerializer<'a, W> {
    type Ok = bool;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        serialize_field(&mut self.len, self.writer, self.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(true)
    }
}

impl<'a, W: Write + ?Sized> SerializeStructVariant for StructVariantSerializer<'a, W> {
    type Ok = bool;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        serialize_field(&mut self.len, self.writer, self.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(true)
    }
}

impl<'a, W: Write + ?Sized> SerializeTupleVariant for KeyStructVariantSerializer<'a, W> {
    type Ok = Vec<u8>;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        serialize_key_field(self.writer, self.code_page, v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Vec::new())
    }
}

impl<'a, W: Write + ?Sized> SerializeStructVariant for KeyStructVariantSerializer<'a, W> {
    type Ok = Vec<u8>;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        serialize_key_field(self.writer, self.code_page, v)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Vec::new())
    }
}

fn byte(v: bool) -> u8 {
    if v { 1 } else { 0 }
}

fn serialize_u16(writer: &mut (impl Write + ?Sized), v: u16) -> Result<(), Error> {
    writer.write_all(&[(v & 0xFF) as u8, (v >> 8) as u8]).map_err(Error::IoError)
}

fn serialize_u64(writer: &mut (impl Write + ?Sized), v: u64) -> Result<(), Error> {
    writer.write_all(&[
        (v & 0xFF) as u8,
        ((v >> 8) & 0xFF) as u8,
        ((v >> 16) & 0xFF) as u8,
        ((v >> 24) & 0xFF) as u8,
        ((v >> 32) & 0xFF) as u8,
        ((v >> 40) & 0xFF) as u8,
        ((v >> 48) & 0xFF) as u8,
        (v >> 56) as u8
    ]).map_err(Error::IoError)
}

serde_if_integer128! {
    fn serialize_u128(writer: &mut (impl Write + ?Sized), v: u128) -> Result<(), Error> {
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
        ]).map_err(Error::IoError)
    }
}

impl<'a, W: Write + ?Sized> Serializer for TesSerializer<'a, W> {
    type Ok = bool;
    type Error = Error;
    type SerializeSeq = SeqSerializer<'a, W>;
    type SerializeTuple = StructSerializer<'a, W>;
    type SerializeTupleStruct = StructSerializer<'a, W>;
    type SerializeTupleVariant = StructVariantSerializer<'a, W>;
    type SerializeStruct = StructSerializer<'a, W>;
    type SerializeStructVariant = StructVariantSerializer<'a, W>;
    type SerializeMap = MapSerializer<'a, W>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(byte(v))
    }
    
    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        if !self.isolated {
            serialize_u32(self.writer, size(v.len())?)?;
        }
        serialize_bytes(self.writer, v)?;
        Ok(!self.isolated || !v.is_empty())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        serialize_u8(self.writer, v)?;
        Ok(true)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(unsafe { transmute(v) })
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        serialize_u16(self.writer, v)?;
        Ok(true)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u16(unsafe { transmute(v) })
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        serialize_u32(self.writer, v)?;
        Ok(true)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        serialize_u64(self.writer, v)?;
        Ok(true)
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
            Ok(true)
        }

        fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
            self.serialize_u128(unsafe { transmute(v) })
        }
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        let v = self.code_page.encoding()
            .encode(&v.to_string(), EncoderTrap::Strict)
            .map_err(|_| Error::UnrepresentableChar(v, self.code_page))?;
        self.serialize_bytes(&v)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        let v = self.code_page.encoding()
            .encode(v, EncoderTrap::Strict)
            .map_err(|s| Error::UnrepresentableChar(s.chars().nth(0).unwrap(), self.code_page))?;
        self.serialize_bytes(&v)
    }
    
    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(0)
    }
    
    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        serialize_u8(self.writer, 1)?;
        value.serialize(self)?;
        Ok(true)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(false)
    }

    fn serialize_unit_variant(self, _: &'static str, variant_index: u32, _: &'static str) 
        -> Result<Self::Ok, Self::Error> {

        self.serialize_u32(variant_index)
    }

    fn serialize_newtype_struct<T: Serialize + ?Sized>(self, _: &'static str, v: &T) 
        -> Result<Self::Ok, Self::Error> {

        v.serialize(self)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _: &'static str, variant_index: u32, _: &'static str, v: &T) 
        -> Result<Self::Ok, Self::Error> {

        serialize_u32(self.writer, variant_index)?;
        v.serialize(self)?;
        Ok(true)
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(StructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            result: false
        })
    }

    fn serialize_tuple_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(StructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            result: false
        })
    }

    fn serialize_tuple_variant(self, _: &'static str, variant_index: u32, _: &'static str, len: usize) 
        -> Result<Self::SerializeTupleVariant, Self::Error> {

        serialize_u32(self.writer, variant_index)?;
        Ok(StructVariantSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(StructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            result: false
        })
    }

    fn serialize_struct_variant(self, _: &'static str, variant_index: u32, _: &'static str, len: usize) 
        -> Result<Self::SerializeStructVariant, Self::Error> {

        serialize_u32(self.writer, variant_index)?;
        Ok(StructVariantSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(SeqSerializer { 
            state: SeqState::NoElements,
            buf: if self.isolated { None } else { Some(Vec::new()) },
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(MapSerializer {
            has_elements: false,
            base: BaseMapSerializer {
                key: None,
                value: Vec::new(),
                writer: self.writer,
                code_page: self.code_page,
            }
        })
    }
}

impl<'a, W: Write + ?Sized> Serializer for KeySerializer<'a, W> {
    type Ok = Vec<u8>;
    type Error = Error;
    type SerializeSeq = KeySeqSerializer<'a, W>;
    type SerializeTuple = KeyTupleSerializer<'a, W>;
    type SerializeTupleStruct = KeyStructSerializer<'a, W>;
    type SerializeTupleVariant = KeyStructVariantSerializer<'a, W>;
    type SerializeStruct = KeyStructSerializer<'a, W>;
    type SerializeStructVariant = KeyStructVariantSerializer<'a, W>;
    type SerializeMap = KeyMapSerializer<'a, W>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(byte(v))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        serialize_u32(self.writer, size(v.len())?)?;
        serialize_bytes(self.writer, v)?;
        Ok(Vec::new())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        serialize_u8(self.writer, v)?;
        Ok(Vec::new())
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(unsafe { transmute(v) })
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        serialize_u16(self.writer, v)?;
        Ok(Vec::new())
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u16(unsafe { transmute(v) })
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        serialize_u32(self.writer, v)?;
        Ok(Vec::new())
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        serialize_u64(self.writer, v)?;
        Ok(Vec::new())
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
            Ok(Vec::new())
        }

        fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
            self.serialize_u128(unsafe { transmute(v) })
        }
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        let v = self.code_page.encoding()
            .encode(&v.to_string(), EncoderTrap::Strict)
            .map_err(|_| Error::UnrepresentableChar(v, self.code_page))?;
        self.serialize_bytes(&v)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        let v = self.code_page.encoding()
            .encode(v, EncoderTrap::Strict)
            .map_err(|s| Error::UnrepresentableChar(s.chars().nth(0).unwrap(), self.code_page))?;
        self.serialize_bytes(&v)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(0)
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        serialize_u8(self.writer, 1)?;
        value.serialize(self)?;
        Ok(Vec::new())
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(Vec::new())
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(Vec::new())
    }

    fn serialize_unit_variant(self, _: &'static str, variant_index: u32, _: &'static str)
        -> Result<Self::Ok, Self::Error> {

        self.serialize_u32(variant_index)
    }

    fn serialize_newtype_struct<T: Serialize + ?Sized>(self, _: &'static str, v: &T)
        -> Result<Self::Ok, Self::Error> {

        v.serialize(self)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _: &'static str, variant_index: u32, _: &'static str, v: &T)
        -> Result<Self::Ok, Self::Error> {

        serialize_u32(self.writer, variant_index)?;
        v.serialize(self)?;
        Ok(Vec::new())
    }

    fn serialize_tuple(self, _: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(KeyTupleSerializer {
            writer: self.writer, code_page: self.code_page,
            result: None
        })
    }

    fn serialize_tuple_struct(self, _: &'static str, _: usize) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(KeyStructSerializer {
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_tuple_variant(self, _: &'static str, variant_index: u32, _: &'static str, _: usize)
        -> Result<Self::SerializeTupleVariant, Self::Error> {

        serialize_u32(self.writer, variant_index)?;
        Ok(KeyStructVariantSerializer {
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_struct(self, _: &'static str, _: usize) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(KeyStructSerializer {
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_struct_variant(self, _: &'static str, variant_index: u32, _: &'static str, _: usize)
        -> Result<Self::SerializeStructVariant, Self::Error> {

        serialize_u32(self.writer, variant_index)?;
        Ok(KeyStructVariantSerializer {
            writer: self.writer, code_page: self.code_page,
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
            key: None,
            value: Vec::new(),
            writer: self.writer, code_page: self.code_page,
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::code::*;
    use encoding::{DecoderTrap, EncoderTrap};

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
}
