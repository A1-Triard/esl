use serde::{Serializer, Serialize};
use std::mem::{transmute};
use std::fmt::{self, Display};
use encoding::{Encoding, EncoderTrap};
use encoding::all::{WINDOWS_1251, WINDOWS_1252};
use serde::ser::{self, SerializeSeq, SerializeTuple, SerializeTupleStruct, SerializeStruct, SerializeTupleVariant, SerializeStructVariant, SerializeMap};
use either::Either;
use either::Either::{Left, Right};

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
    Custom(String),
    LargeObject(usize),
    UnrepresentableChar(char, CodePage),
    ZeroSizedLastSequenceElement,
    VariantIndexMismatch { variant_index: u32, variant_size: u32 },
    ZeroSizedOptional,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Custom(s) => Display::fmt(s, f),
            Error::LargeObject(size) => write!(f, "object has too large size ({} B)", size),
            Error::UnrepresentableChar(c, p) => write!(f, "the '{}' char is not representable in {:?} code page", c, p),
            Error::ZeroSizedLastSequenceElement => write!(f, "last element in sequence or map cannot have zero size"),
            Error::VariantIndexMismatch { variant_index, variant_size } => 
                write!(f, "variant index ({}) should be equal to variant size ({})", variant_index, variant_size),
            Error::ZeroSizedOptional => write!(f, "optional element cannot have zero size"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self { Error::Custom(format!("{}", msg)) }
}

#[derive(Debug)]
struct VecEslSerializer<'a> {
    isolated: bool,
    code_page: CodePage,
    writer: &'a mut Vec<u8>
}

#[derive(Debug)]
struct VecSeqSerializer<'a> {
    code_page: CodePage,
    writer: &'a mut Vec<u8>,
    last_element_has_zero_size: bool,
    buf: Either<usize, Vec<u8>>,
}

#[derive(Debug)]
struct VecStructSerializer<'a> {
    code_page: CodePage,
    writer: &'a mut Vec<u8>,
    len: Option<usize>,
    size: usize
}

#[derive(Debug)]
struct VecKeySeqSerializer<'a> {
    code_page: CodePage,
    writer: &'a mut Vec<u8>,
    last_element_has_zero_size: bool,
    buf: Vec<u8>,
}

#[derive(Debug)]
struct VecBaseMapSerializer<'a> {
    code_page: CodePage,
    writer: &'a mut Vec<u8>,
    key_tail: Option<Vec<u8>>,
    size: usize
}

#[derive(Debug)]
struct VecMapSerializer<'a>(VecBaseMapSerializer<'a>);

#[derive(Debug)]
struct VecKeyMapSerializer<'a>(VecBaseMapSerializer<'a>);

#[derive(Debug)]
struct VecKeyStructSerializer<'a> {
    code_page: CodePage,
    writer: &'a mut Vec<u8>,
    size: usize
}

#[derive(Debug)]
struct VecKeyTupleSerializer<'a> {
    code_page: CodePage,
    writer: &'a mut Vec<u8>,
    tail: Option<Vec<u8>>,
    size: usize
}

#[derive(Debug)]
struct VecBaseStructVariantSerializer<'a> {
    code_page: CodePage,
    writer: &'a mut Vec<u8>,
    variant_index: u32,
    size: usize
}

#[derive(Debug)]
struct VecStructVariantSerializer<'a> {
    base: VecBaseStructVariantSerializer<'a>,
    len: Option<usize>,
}

#[derive(Debug)]
struct VecKeyStructVariantSerializer<'a>(VecBaseStructVariantSerializer<'a>);

#[derive(Debug)]
struct VecKeySerializer<'a> {
    code_page: CodePage,
    writer: &'a mut Vec<u8>,
}

fn serialize_u32(writer: &mut Vec<u8>, v: u32) {
    writer.push((v & 0xFF) as u8);
    writer.push(((v >> 8) & 0xFF) as u8);
    writer.push(((v >> 16) & 0xFF) as u8);
    writer.push((v >> 24) as u8);
}

fn size(len: usize) -> Result<u32, Error> {
    if len > u32::max_value() as usize {
        Err(Error::LargeObject(len))
    } else {
        Ok(len as u32)
    }
}

impl<'a> SerializeSeq for VecSeqSerializer<'a> {
    type Ok = usize;
    type Error = Error;
    
    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        let size = match self.buf.as_mut() {
            Left(self_size) => {
                let size = v.serialize(VecEslSerializer {
                    isolated: false,
                    writer: self.writer, code_page: self.code_page,
                })?;
                *self_size += size;
                size
            },
            Right(buf) => {
                v.serialize(VecEslSerializer {
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
            return Err(Error::ZeroSizedLastSequenceElement);
        }
        match self.buf.as_ref() {
            Left(&size) => Ok(size),
            Right(buf) => {
                serialize_u32(self.writer, size(buf.len())?);
                self.writer.extend_from_slice(&buf);
                Ok(4 + buf.len())
            }
        }
    }
}

impl<'a> SerializeSeq for VecKeySeqSerializer<'a> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = Error;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.last_element_has_zero_size = v.serialize(VecEslSerializer {
            isolated: false,
            writer: &mut self.buf,
            code_page: self.code_page
        })? == 0;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        if self.last_element_has_zero_size {
            return Err(Error::ZeroSizedLastSequenceElement);
        }
        serialize_u32(self.writer, size(self.buf.len())?);
        self.writer.extend_from_slice(&self.buf);
        Ok((None, 4 + self.buf.len()))
    }
}

impl<'a> VecBaseMapSerializer<'a> {
    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Error> {
        let (buf, size) = key.serialize(VecKeySerializer { 
            writer: self.writer,
            code_page: self.code_page
        })?;
        self.size += size;
        self.key_tail = buf;
        Ok(())
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Error> {
        let mut value = Vec::new();
        v.serialize(VecEslSerializer {
            isolated: true,
            writer: &mut value,
            code_page: self.code_page
        })?;
        serialize_u32(self.writer, size(value.len())?);
        if let Some(key_tail) = &self.key_tail {
            self.writer.extend_from_slice(key_tail);
        }
        self.writer.extend_from_slice(&value);
        self.size += 4 + value.len();
        Ok(())
    }
}

impl<'a> SerializeMap for VecMapSerializer<'a> {
    type Ok = usize;
    type Error = Error;

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

impl<'a> SerializeMap for VecKeyMapSerializer<'a> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = Error;

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

fn serialize_field<T: Serialize + ?Sized>(writer: &mut Vec<u8>, code_page: CodePage, len: &mut Option<usize>, v: &T)
    -> Result<usize, Error> {
    
    len.as_mut().map(|len| {
        if *len == 0 { panic!() }
        *len -= 1;
    });
    v.serialize(VecEslSerializer {
        isolated: len.map_or(false, |len| len == 0),
        writer, code_page
    })
}

fn serialize_key_field<T: Serialize + ?Sized>(writer: &mut Vec<u8>, code_page: CodePage, v: &T)
    -> Result<usize, Error> {

    v.serialize(VecEslSerializer {
        isolated: false,
        writer, code_page
    })
}

impl<'a> SerializeTuple for VecStructSerializer<'a> {
    type Ok = usize;
    type Error = Error;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_field(self.writer, self.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.size)
    }
}

impl<'a> SerializeTupleStruct for VecStructSerializer<'a> {
    type Ok = usize;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_field(self.writer, self.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.size)
    }
}

impl<'a> SerializeStruct for VecStructSerializer<'a> {
    type Ok = usize;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_field(self.writer, self.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.size)
    }
}

impl<'a> SerializeTuple for VecKeyTupleSerializer<'a> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = Error;

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

impl<'a> SerializeStruct for VecKeyStructSerializer<'a> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_key_field(self.writer, self.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok((None, self.size))
    }
}

impl<'a> SerializeTupleStruct for VecKeyStructSerializer<'a> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.size += serialize_key_field(self.writer, self.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok((None, self.size))
    }
}

impl<'a> VecBaseStructVariantSerializer<'a> {
    fn end(self) -> Result<usize, Error> {
        let variant_size = size(self.size)?;
        if self.variant_index != variant_size {
            return Err(Error::VariantIndexMismatch { variant_index: self.variant_index, variant_size });
        }
        Ok(self.size)
    }
}

impl<'a> SerializeTupleVariant for VecStructVariantSerializer<'a> {
    type Ok = usize;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.base.size += serialize_field(self.base.writer, self.base.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.base.end()
    }
}

impl<'a> SerializeStructVariant for VecStructVariantSerializer<'a> {
    type Ok = usize;
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, _: &'static str, v: &T) -> Result<(), Self::Error> {
        self.base.size += serialize_field(self.base.writer, self.base.code_page, &mut self.len, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.base.end()
    }
}

impl<'a> SerializeTupleVariant for VecKeyStructVariantSerializer<'a> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> Result<(), Self::Error> {
        self.0.size += serialize_key_field(self.0.writer, self.0.code_page, v)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok((None, self.0.end()?))
    }
}

impl<'a> SerializeStructVariant for VecKeyStructVariantSerializer<'a> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = Error;

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

fn serialize_u16(writer: &mut Vec<u8>, v: u16) {
    writer.push((v & 0xFF) as u8);
    writer.push((v >> 8) as u8);
}

fn serialize_u64(writer: &mut Vec<u8>, v: u64) {
    writer.push((v & 0xFF) as u8);
    writer.push(((v >> 8) & 0xFF) as u8);
    writer.push(((v >> 16) & 0xFF) as u8);
    writer.push(((v >> 24) & 0xFF) as u8);
    writer.push(((v >> 32) & 0xFF) as u8);
    writer.push(((v >> 40) & 0xFF) as u8);
    writer.push(((v >> 48) & 0xFF) as u8);
    writer.push((v >> 56) as u8);
}

serde_if_integer128! {
    fn serialize_u128(writer: &mut Vec<u8>, v: u128) {
        writer.push((v & 0xFF) as u8);
        writer.push(((v >> 8) & 0xFF) as u8);
        writer.push(((v >> 16) & 0xFF) as u8);
        writer.push(((v >> 24) & 0xFF) as u8);
        writer.push(((v >> 32) & 0xFF) as u8);
        writer.push(((v >> 40) & 0xFF) as u8);
        writer.push(((v >> 48) & 0xFF) as u8);
        writer.push(((v >> 56) & 0xFF) as u8);
        writer.push(((v >> 64) & 0xFF) as u8);
        writer.push(((v >> 72) & 0xFF) as u8);
        writer.push(((v >> 80) & 0xFF) as u8);
        writer.push(((v >> 88) & 0xFF) as u8);
        writer.push(((v >> 96) & 0xFF) as u8);
        writer.push(((v >> 104) & 0xFF) as u8);
        writer.push(((v >> 112) & 0xFF) as u8);
        writer.push((v >> 120) as u8);
    }
}

fn char_byte(code_page: CodePage, v: char) -> Result<u8, Error> {
    let v = code_page.encoding()
        .encode(&v.to_string(), EncoderTrap::Strict)
        .map_err(|_| Error::UnrepresentableChar(v, code_page))?;
    debug_assert_eq!(v.len(), 1);
    Ok(v[0])
}

fn str_bytes(code_page: CodePage, v: &str) -> Result<Vec<u8>, Error> {
    code_page.encoding()
        .encode(v, EncoderTrap::Strict)
        .map_err(|s| Error::UnrepresentableChar(s.chars().nth(0).unwrap(), code_page))
}

impl<'a> Serializer for VecEslSerializer<'a> {
    type Ok = usize;
    type Error = Error;
    type SerializeSeq = VecSeqSerializer<'a>;
    type SerializeTuple = VecStructSerializer<'a>;
    type SerializeTupleStruct = VecStructSerializer<'a>;
    type SerializeTupleVariant = VecStructVariantSerializer<'a>;
    type SerializeStruct = VecStructSerializer<'a>;
    type SerializeStructVariant = VecStructVariantSerializer<'a>;
    type SerializeMap = VecMapSerializer<'a>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(bool_byte(v))
    }
    
    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        if !self.isolated {
            serialize_u32(self.writer, size(v.len())?);
        }
        self.writer.extend_from_slice(v);
        Ok(if self.isolated { 0 } else { 4 } + v.len())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.writer.push(v);
        Ok(1)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(unsafe { transmute(v) })
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        serialize_u16(self.writer, v);
        Ok(2)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u16(unsafe { transmute(v) })
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        serialize_u32(self.writer, v);
        Ok(4)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        serialize_u64(self.writer, v);
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
            serialize_u128(self.writer, v);
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
            return Err(Error::VariantIndexMismatch { variant_index, variant_size: 0 });
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
            serialize_u32(self.writer, 0);
        }
        Ok(if self.isolated { 0 } else { 4 })
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        let mut bytes = Vec::new(); 
        if value.serialize(VecEslSerializer {
            isolated: true,
            writer: &mut bytes,
            code_page: self.code_page
        })? == 0 {
            return Err(Error::ZeroSizedOptional);
        }
        self.serialize_bytes(&bytes)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _: &'static str, variant_index: u32, _: &'static str, v: &T) 
        -> Result<Self::Ok, Self::Error> {

        let mut bytes = Vec::new();
        v.serialize(VecEslSerializer {
            isolated: true,
            writer: &mut bytes,
            code_page: self.code_page
        })?;
        let variant_size = size(bytes.len())?;
        if variant_index != variant_size {
            return Err(Error::VariantIndexMismatch { variant_index, variant_size });
        }
        if !self.isolated {
            serialize_u32(self.writer, variant_index);
        }
        self.writer.extend_from_slice(&bytes);
        Ok(if self.isolated { 0 } else { 4 } + bytes.len())
    }

    fn serialize_tuple_variant(self, _: &'static str, variant_index: u32, _: &'static str, len: usize) 
        -> Result<Self::SerializeTupleVariant, Self::Error> {

        if !self.isolated {
            serialize_u32(self.writer, variant_index);
        }
        Ok(VecStructVariantSerializer {
            len: if self.isolated { Some(len) } else { None },
            base: VecBaseStructVariantSerializer {
                variant_index,
                writer: self.writer, code_page: self.code_page,
                size: 0
            }
        })
    }

    fn serialize_struct_variant(self, _: &'static str, variant_index: u32, _: &'static str, len: usize)
        -> Result<Self::SerializeStructVariant, Self::Error> {

        if !self.isolated {
            serialize_u32(self.writer, variant_index);
        }
        Ok(VecStructVariantSerializer {
            len: if self.isolated { Some(len) } else { None },
            base: VecBaseStructVariantSerializer {
                variant_index,
                writer: self.writer, code_page: self.code_page,
                size: 0
            }
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(VecStructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_tuple_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(VecStructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(VecStructSerializer {
            len: if self.isolated { Some(len) } else { None },
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(VecSeqSerializer { 
            last_element_has_zero_size: false,
            buf: if self.isolated { Left(0) } else { Right(Vec::new()) },
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(VecMapSerializer(VecBaseMapSerializer {
            key_tail: None,
            writer: self.writer, code_page: self.code_page,
            size: 0
        }))
    }
}

impl<'a> Serializer for VecKeySerializer<'a> {
    type Ok = (Option<Vec<u8>>, usize);
    type Error = Error;
    type SerializeSeq = VecKeySeqSerializer<'a>;
    type SerializeTuple = VecKeyTupleSerializer<'a>;
    type SerializeTupleStruct = VecKeyStructSerializer<'a>;
    type SerializeTupleVariant = VecKeyStructVariantSerializer<'a>;
    type SerializeStruct = VecKeyStructSerializer<'a>;
    type SerializeStructVariant = VecKeyStructVariantSerializer<'a>;
    type SerializeMap = VecKeyMapSerializer<'a>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(bool_byte(v))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        serialize_u32(self.writer, size(v.len())?);
        self.writer.extend_from_slice(v);
        Ok((None, 4 + v.len()))
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.writer.push(v);
        Ok((None, 1))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_u8(unsafe { transmute(v) })
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        serialize_u16(self.writer, v);
        Ok((None, 2))
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_u16(unsafe { transmute(v) })
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        serialize_u32(self.writer, v);
        Ok((None, 4))
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(unsafe { transmute(v) })
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        serialize_u64(self.writer, v);
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
            serialize_u128(self.writer, v);
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

        let size = v.serialize(VecEslSerializer {
            isolated: false,
            writer: self.writer, code_page: self.code_page
        })?;
        Ok((None, size))
    }

    fn serialize_unit_variant(self, _: &'static str, variant_index: u32, _: &'static str)
        -> Result<Self::Ok, Self::Error> {

        if variant_index != 0 {
            return Err(Error::VariantIndexMismatch { variant_index, variant_size: 0 });
        }
        self.serialize_u32(0)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(0)
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        let mut bytes = Vec::new();
        if value.serialize(VecEslSerializer {
            isolated: true,
            writer: &mut bytes,
            code_page: self.code_page
        })? == 0 {
            return Err(Error::ZeroSizedOptional);
        }
        self.serialize_bytes(&bytes)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(self, _: &'static str, variant_index: u32, _: &'static str, v: &T)
        -> Result<Self::Ok, Self::Error> {

        let mut bytes = Vec::new();
        v.serialize(VecEslSerializer {
            isolated: true,
            writer: &mut bytes,
            code_page: self.code_page
        })?;
        let variant_size = size(bytes.len())?;
        if variant_index != variant_size {
            return Err(Error::VariantIndexMismatch { variant_index, variant_size });
        }
        serialize_u32(self.writer, variant_index);
        self.writer.extend_from_slice(&bytes);
        Ok((None, 4 + bytes.len()))
    }

    fn serialize_tuple_variant(self, _: &'static str, variant_index: u32, _: &'static str, _: usize)
        -> Result<Self::SerializeTupleVariant, Self::Error> {

        serialize_u32(self.writer, variant_index);
        Ok(VecKeyStructVariantSerializer(VecBaseStructVariantSerializer { writer: self.writer, code_page: self.code_page, variant_index, size: 0}))
    }

    fn serialize_struct_variant(self, _: &'static str, variant_index: u32, _: &'static str, _: usize)
        -> Result<Self::SerializeStructVariant, Self::Error> {

        serialize_u32(self.writer, variant_index);
        Ok(VecKeyStructVariantSerializer(VecBaseStructVariantSerializer { writer: self.writer, code_page: self.code_page, variant_index, size: 0}))
    }

    fn serialize_tuple(self, _: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(VecKeyTupleSerializer {
            writer: self.writer, code_page: self.code_page,
            tail: None,
            size: 0
        })
    }

    fn serialize_tuple_struct(self, _: &'static str, _: usize) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(VecKeyStructSerializer {
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_struct(self, _: &'static str, _: usize) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(VecKeyStructSerializer {
            writer: self.writer, code_page: self.code_page,
            size: 0
        })
    }

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(VecKeySeqSerializer {
            last_element_has_zero_size: false,
            buf: Vec::new(),
            writer: self.writer, code_page: self.code_page,
        })
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(VecKeyMapSerializer(VecBaseMapSerializer {
            key_tail: None,
            writer: self.writer, code_page: self.code_page,
            size: 0
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::code_vec::*;
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
        s.serialize(VecEslSerializer {
            isolated: true,
            code_page: CodePage::Russian,
            writer: &mut v
        }).unwrap();
        assert_eq!(v, [5, 0, 219, 90, 0, 0, 0, 83]);
    }

    #[test]
    fn vec_serialize_struct_not_isolated() {
        let s = Abcd { a: 5, b: 'Ы', c: 90, d: "S".into() };
        let mut v = Vec::new();
        s.serialize(VecEslSerializer {
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
    fn vec_serialize_map() {
        let mut s = Map {
            map: HashMap::new(),
            unit: (),
            i: -3
        };
        s.map.insert(Key { variant: Variant::Variant2, s: "str".into() }, "value".into());
        let mut v = Vec::new();
        s.serialize(VecEslSerializer {
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
    fn vec_serialize_tuple_key() {
        let mut s: HashMap<(Key, Key), u64> = HashMap::new();
        s.insert((
            Key { variant: Variant::Variant2, s: "str".into() },
            Key { variant: Variant::Variant1, s: "стр".into() }
        ), 22);
        let mut v = Vec::new();
        s.serialize(VecEslSerializer {
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
    fn vec_serialize_newtype_key() {
        let mut s: HashMap<Key2, u64> = HashMap::new();
        s.insert(Key2((
            Key { variant: Variant::Variant2, s: "str".into() },
            Key { variant: Variant::Variant1, s: "стр".into() }
        )), 22);
        let mut v = Vec::new();
        s.serialize(VecEslSerializer {
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
