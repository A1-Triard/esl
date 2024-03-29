use serde::{Deserializer};
use std::fmt::{self, Display, Formatter};
use serde::de::{self, Visitor, SeqAccess, DeserializeSeed, MapAccess, EnumAccess, VariantAccess, IntoDeserializer};
use serde::serde_if_integer128;
use std::io::{self, Read};
use byteorder::{LittleEndian, ReadBytesExt};
use std::borrow::Cow;
use std::marker::PhantomData;
use std::str::{self};

#[derive(Debug)]
pub enum Error {
    Custom(String),
    Io(io::Error),
    InvalidBoolEncoding(u8),
    InvalidSize { actual: usize, expected: u32 },
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::Custom(s) => Display::fmt(s, f),
            Error::Io(e) => Display::fmt(e, f),
            Error::InvalidBoolEncoding(b) => write!(f, "invalid bool encoding ({b})"),
            Error::InvalidSize { actual, expected } => write!(f, "object size mismatch (actual = {actual}, expected = {expected})"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        if let Error::Io(e) = self {
            Some(e)
        } else {
            None
        }
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self { Error::Custom(format!("{msg}")) }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error { Error::Io(e) }
}

#[derive(Debug)]
pub enum ExtError<'de> {
    Error(Error),
    Unread(&'de [u8]),
}

impl<'de> Display for ExtError<'de> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ExtError::Error(s) => Display::fmt(s, f),
            ExtError::Unread(b) => write!(f, "not all input read, {} bytes left", b.len()),
        }
    }
}

impl<'de> std::error::Error for ExtError<'de> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        if let ExtError::Error(e) = self {
            Some(e)
        } else {
            None
        }
    }
}

impl<'de> de::Error for ExtError<'de> {
    fn custom<T: Display>(msg: T) -> Self { ExtError::Error(Error::custom(msg)) }
}

impl<'de> From<io::Error> for ExtError<'de> {
    fn from(e: io::Error) -> Self { Error::Io(e).into() }
}

impl<'de> From<Error> for ExtError<'de> {
    fn from(e: Error) -> Self { ExtError::Error(e) }
}

pub(crate) trait Reader<'de>: Read {
    fn read_bytes(&mut self, len: usize) -> io::Result<Cow<'de, [u8]>>;
    fn pos(&self) -> isize;
}

pub(crate) struct GenericReader<'de, R: Read + ?Sized> {
    reader: &'de mut R,
    pos: isize,
}

impl<'de, R: Read + ?Sized> GenericReader<'de, R> {
    pub fn new(reader: &'de mut R) -> Self { GenericReader { reader, pos: 0 } }
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
        let mut buf = vec![0; len];
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
    reader: &'a mut R,
    phantom: PhantomData<&'de ()>
}

impl <'a, 'de, R: Reader<'de>> SeqAccess<'de> for SeqDeserializer<'a, 'de, R> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error> where T: DeserializeSeed<'de> {
        if self.reader.pos() == self.start_pos + self.size as isize { return Ok(None); }
        let element = seed.deserialize(EslDeserializer { 
            isolated: None, reader: self.reader,
            phantom: PhantomData, map_entry_value_size: None
        })?;
        if self.reader.pos() > self.start_pos + self.size as isize {
            return Err(Error::InvalidSize { expected: self.size, actual: (self.reader.pos() - self.start_pos) as usize });
        }
        Ok(Some(element))
    }
}

#[derive(Debug)]
struct StructDeserializer<'r, 'a, 'de, R: Reader<'de>> {
    len: usize,
    isolated: Option<(isize, u32)>,
    reader: &'a mut R,
    map_entry_value_size: Option<&'r mut Option<u32>>,
    phantom: PhantomData<&'de ()>
}

impl <'r, 'a, 'de, R: Reader<'de>> SeqAccess<'de> for StructDeserializer<'r, 'a, 'de, R> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error> where T: DeserializeSeed<'de> {
        if self.len == 0 { return Ok(None); }
        self.len -= 1;
        let isolated = self.isolated.and_then(|(start_pos, size)| {
            if self.len == 0 { Some(size - (self.reader.pos() - start_pos) as u32) } else { None }
        });
        let element = seed.deserialize(EslDeserializer {
            isolated, reader: self.reader,
            phantom: PhantomData, map_entry_value_size: None
        })?;
        if let Some((start_pos, size)) = self.isolated {
            if self.reader.pos() > start_pos + size as isize {
                return Err(Error::InvalidSize { expected: size, actual: (self.reader.pos() - start_pos) as usize });
            }
        }
        if let Some(map_entry_value_size) = self.map_entry_value_size.take() {
            *map_entry_value_size = Some(self.reader.read_u32::<LittleEndian>()?)
        }
        Ok(Some(element))
    }
}

#[derive(Debug)]
struct MapDeserializer<'a, 'de, R: Reader<'de>> {
    reader: &'a mut R,
    phantom: PhantomData<&'de ()>,
    value_size: Option<u32>
}

impl <'a, 'de, R: Reader<'de>> MapAccess<'de> for MapDeserializer<'a, 'de, R> {
    type Error = Error;

    fn size_hint(&self) -> Option<usize> { Some(1) }
    
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error> where K: DeserializeSeed<'de> {
        if self.value_size.is_some() { return Ok(None); }
        let mut value_size = None;
        let key = seed.deserialize(EslDeserializer {
            isolated: None, reader: self.reader,
            phantom: PhantomData, map_entry_value_size: Some(&mut value_size)
        })?;
        self.value_size = Some(value_size.map_or_else(|| self.reader.read_u32::<LittleEndian>(), Ok)?);
        Ok(Some(key))
    }
    
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error> where V: DeserializeSeed<'de> {
        seed.deserialize(EslDeserializer {
            isolated: Some(self.value_size.unwrap()), reader: self.reader,
            phantom: PhantomData, map_entry_value_size: None
        })
    }
}

#[derive(Debug)]
pub(crate) struct EnumDeserializer<'a, 'de, R: Reader<'de>> {
    size: u32,
    reader: &'a mut R,
    phantom: PhantomData<&'de ()>
}

impl<'a, 'de, R: Reader<'de>> VariantAccess<'de> for EnumDeserializer<'a, 'de, R> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }
    
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error> where T: DeserializeSeed<'de> {
        seed.deserialize(EslDeserializer {
            map_entry_value_size: None,
            isolated: Some(self.size),
            reader: self.reader,
            phantom: PhantomData
        })
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_seq(StructDeserializer {
            len,
            map_entry_value_size: None,
            isolated: Some((self.reader.pos(), self.size)),
            reader: self.reader,
            phantom: PhantomData,
        })
    }

    fn struct_variant<V>(self, fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_seq(StructDeserializer {
            len: fields.len(),
            map_entry_value_size: None,
            isolated: Some((self.reader.pos(), self.size)),
            reader: self.reader,
            phantom: PhantomData,
        })
    }
}

#[derive(Debug)]
pub(crate) struct EslDeserializer<'r, 'a, 'de, R: Reader<'de>> {
    map_entry_value_size: Option<&'r mut Option<u32>>,
    isolated: Option<u32>,
    reader: &'a mut R,
    phantom: PhantomData<&'de ()>
}

impl<'r, 'a, 'de, R: Reader<'de>> EslDeserializer<'r, 'a, 'de, R> {
    pub fn new(isolated: Option<u32>, reader: &'a mut R) -> Self {
        EslDeserializer {
            map_entry_value_size: None,
            isolated, reader,
            phantom: PhantomData
        }
    }
    
    fn deserialize_size(&mut self) -> Result<u32, io::Error> {
        if let Some(size) = self.isolated {
            Ok(size)
        } else {
            self.reader.read_u32::<LittleEndian>()
        }
    }
}

impl<'r, 'a, 'de, R: Reader<'de>> Deserializer<'de> for EslDeserializer<'r, 'a, 'de, R> {
    type Error = Error;

    fn is_human_readable(&self) -> bool { false }

    fn deserialize_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        panic!("deserialize_any not supported")
    }

    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        panic!("deserialize_identifier not supported")
    }
    
    fn deserialize_ignored_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        panic!("deserialize_ignored_any not supported")
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        let b = self.reader.read_u8()?;
        let v = match b {
            0 => false,
            1 => true,
            b => return Err(Error::InvalidBoolEncoding(b))
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
    
    fn deserialize_char<V>(self, _: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        panic!("deserialize_char not supported");
    }

    fn deserialize_str<V>(self, _: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        panic!("deserialize_str not supported");
    }

    fn deserialize_string<V>(self, _: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        panic!("deserialize_string not supported");
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
            reader: self.reader, phantom: PhantomData
        })
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_map(MapDeserializer {
            reader: self.reader, phantom: PhantomData,
            value_size: None
        })
    }

    fn deserialize_tuple<V>(mut self, len: usize, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        let map_entry_value_size = self.map_entry_value_size.take();
        visitor.visit_seq(StructDeserializer {
            len,
            map_entry_value_size,
            isolated: self.isolated.map(|size| (self.reader.pos(), size)),
            reader: self.reader,
            phantom: PhantomData,
        })
    }

    fn deserialize_tuple_struct<V>(self, _: &'static str, len: usize, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_seq(StructDeserializer {
            len,
            map_entry_value_size: None,
            isolated: self.isolated.map(|size| (self.reader.pos(), size)),
            reader: self.reader,
            phantom: PhantomData,
        })
    }

    fn deserialize_struct<V>(self, _: &'static str, fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_seq(StructDeserializer {
            len: fields.len(),
            map_entry_value_size: None,
            isolated: self.isolated.map(|size| (self.reader.pos(), size)),
            reader: self.reader,
            phantom: PhantomData,
        })
    }

    fn deserialize_enum<V>(self, _: &'static str, _: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_enum(self)
    }
}

impl <'r, 'a, 'de, R: Reader<'de>> EnumAccess<'de> for EslDeserializer<'r, 'a, 'de, R> {
    type Error = Error;
    type Variant = EnumDeserializer<'a, 'de, R>;

    fn variant_seed<V>(mut self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error> where V: DeserializeSeed<'de> {
        debug_assert!(self.map_entry_value_size.is_none());
        let variant_index = self.deserialize_size()?;
        let res: Result<V::Value, Self::Error> = seed.deserialize(variant_index.into_deserializer());
        Ok((res?, EnumDeserializer {
            size: variant_index,
            reader: self.reader,
            phantom: PhantomData,
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::code::de::*;
    use serde::{Deserialize, Deserializer};
    use std::collections::HashMap;
    use std::marker::PhantomData;
    use serde::de::Error as de_Error;
    use serde::de::Unexpected;
 
    #[derive(Deserialize, Eq, PartialEq, Debug)]
    struct Abcd {
        a: i16,
        c: u32,
        d: Vec<u8>
    }

    #[test]
    fn vec_deserialize_struct() {
        let data = [5, 0, 90, 0, 0, 0, 83];
        let d = Abcd::deserialize(EslDeserializer {
            reader: &mut (&data[..]),
            map_entry_value_size: None,
            isolated: Some(data.len() as u32),
            phantom: PhantomData
        }).unwrap();
        assert_eq!(d, Abcd { a: 5, c: 90, d: "S".into() });
    }

    #[test]
    fn vec_serialize_struct_not_isolated() {
        let data = [5, 0, 90, 0, 0, 0, 1, 0, 0, 0, 83];
        let d = Abcd::deserialize(EslDeserializer {
            reader: &mut (&data[..]),
            map_entry_value_size: None,
            isolated: None,
            phantom: PhantomData
        }).unwrap();
        assert_eq!(d, Abcd { a: 5, c: 90, d: "S".into() });
    }

    #[derive(Hash, Eq, PartialEq)]
    enum Variant { Variant1, Variant2 }

    impl<'de> Deserialize<'de> for Variant {
        fn deserialize<D>(deserializer: D) -> Result<Variant, D::Error> where D: Deserializer<'de> {
            match u8::deserialize(deserializer)? {
                1 => Ok(Variant::Variant1),
                2 => Ok(Variant::Variant2),
                n => Err(D::Error::invalid_value(Unexpected::Unsigned(n as u64), &"1 or 2"))
            }
        }
    }

    #[derive(Deserialize, Hash, Eq, PartialEq)]
    struct Key {
        variant: Variant,
        s: Vec<u8>
    }

    #[derive(Deserialize)]
    struct Map {
        map: HashMap<Key, Vec<u8>>,
        unit: (),
        i: i8
    }

    #[allow(clippy::unit_cmp)]
    #[test]
    fn vec_deserialize_map() {
        let data = vec![
            2, 3, 0, 0, 0, 115, 116, 114,
            5, 0, 0, 0, 118, 97, 108, 117, 101,
            253
        ];
        let d = Map::deserialize(EslDeserializer {
            reader: &mut (&data[..]),
            map_entry_value_size: None,
            isolated: Some(data.len() as u32),
            phantom: PhantomData
        }).unwrap();
        assert_eq!(d.i, -3);
        assert_eq!(d.unit, ());
        assert_eq!(1, d.map.len());
        assert_eq!(d.map[&Key { variant: Variant::Variant2, s: "str".into() }], b"value");
    }

    #[test]
    fn vec_deserialize_tuple_key() {
        let data = vec![
            2, 3, 0, 0, 0, 115, 116, 114,
            8, 0, 0, 0,
            1, 3, 0, 0, 0, 120, 121, 122,
            22, 0, 0, 0, 0, 0, 0, 0
        ];
        let d: HashMap<(Key, Key), u64> = HashMap::deserialize(EslDeserializer {
            reader: &mut (&data[..]),
            map_entry_value_size: None,
            isolated: Some(data.len() as u32),
            phantom: PhantomData
        }).unwrap();
        assert_eq!(d.len(), 1);
        assert_eq!(d[&(
            Key { variant: Variant::Variant2, s: "str".into() },
            Key { variant: Variant::Variant1, s: "xyz".into() }
        )], 22);
    }
}
