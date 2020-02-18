mod ser;
mod de;
mod code_page;
pub use code_page::*;
use serde::de::DeserializeSeed;
use crate::code::de::{EslDeserializer, GenericReader};
pub use crate::code::de::{DeError, DeOrIoError};
use serde::{Deserialize, Serialize};
use std::io::{Read, Write};
pub use crate::code::ser::{SerOrIoError, SerError};
use crate::code::ser::{GenericWriter, EslSerializer};

pub fn deserialize_from<'de, T: Deserialize<'de>>(reader: &'de mut (impl Read + ?Sized), code_page: CodePage, isolated: Option<u32>)
    -> Result<T, DeOrIoError> {

    let mut reader = GenericReader::new(reader);
    let deserializer = EslDeserializer::new(isolated, code_page, &mut reader);
    T::deserialize(deserializer)
}

pub fn deserialize_seed_from<'de, T: DeserializeSeed<'de>>(seed: T, reader: &'de mut (impl Read + ?Sized), code_page: CodePage, isolated: Option<u32>)
    -> Result<T::Value, DeOrIoError> {

    let mut reader = GenericReader::new(reader);
    let deserializer = EslDeserializer::new(isolated, code_page, &mut reader);
    seed.deserialize(deserializer)
}

pub fn deserialize<'de, T: Deserialize<'de>>(mut bytes: &'de [u8], code_page: CodePage, isolated: bool)
    -> Result<T, DeOrIoError> {

    let deserializer = bytes_deserializer(&mut bytes, code_page, isolated);
    T::deserialize(deserializer)
}

pub fn deserialize_seed<'de, T: DeserializeSeed<'de>>(seed: T, mut bytes: &'de [u8], code_page: CodePage, isolated: bool)
    -> Result<T::Value, DeOrIoError> {
    
    let deserializer = bytes_deserializer(&mut bytes, code_page, isolated);
    seed.deserialize(deserializer)
}

fn bytes_deserializer<'a, 'de>(bytes: &'a mut (&'de [u8]), code_page: CodePage, isolated: bool) -> EslDeserializer<'static, 'a, 'de, &'de [u8]> {
    assert!(!isolated || bytes.len() <= u32::max_value() as usize);
    EslDeserializer::new(if isolated { Some(bytes.len() as u32) } else { None }, code_page, bytes)
}

pub fn serialize_to<T: Serialize + ?Sized>(v: &T, writer: &mut (impl Write + ?Sized), code_page: CodePage, isolated: bool) -> Result<(), SerOrIoError> {
    let mut writer = GenericWriter::new(writer);
    let serializer = EslSerializer::new(isolated, code_page, &mut writer);
    v.serialize(serializer)
}

pub fn serialize<T: Serialize + ?Sized>(v: &T, bytes: &mut Vec<u8>, code_page: CodePage, isolated: bool) -> Result<(), SerError> {
    let serializer = EslSerializer::new(isolated, code_page, bytes);
    v.serialize(serializer).map_err(|e| {
        match e {
            SerOrIoError::Ser(e) => e,
            SerOrIoError::Io(_) => unreachable!()
        }
    })
}
