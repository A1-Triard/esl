pub mod ser;
pub mod de;

use serde::de::DeserializeSeed;
use crate::code::de::*;
use serde::{Deserialize, Serialize};
use std::io::{Read, Write};
use crate::code::ser::*;

pub fn deserialize_from<'de, T: Deserialize<'de>>(reader: &'de mut (impl Read + ?Sized), isolated: Option<u32>)
    -> Result<T, de::Error> {

    let mut reader = GenericReader::new(reader);
    let deserializer = EslDeserializer::new(isolated, &mut reader);
    T::deserialize(deserializer)
}

pub fn deserialize_from_seed<'de, T: DeserializeSeed<'de>>(seed: T, reader: &'de mut (impl Read + ?Sized), isolated: Option<u32>)
    -> Result<T::Value, de::Error> {

    let mut reader = GenericReader::new(reader);
    let deserializer = EslDeserializer::new(isolated, &mut reader);
    seed.deserialize(deserializer)
}

pub fn deserialize_from_slice<'a, 'de, T: Deserialize<'de>>(bytes: &'a mut &'de [u8], isolated: bool)
    -> Result<T, de::Error> {

    let deserializer = bytes_deserializer(bytes, isolated);
    T::deserialize(deserializer)
}

pub fn deserialize_from_slice_seed<'a, 'de, T: DeserializeSeed<'de>>(seed: T, bytes: &'a mut &'de [u8], isolated: bool)
    -> Result<T::Value, de::Error> {

    let deserializer = bytes_deserializer(bytes, isolated);
    seed.deserialize(deserializer)
}

pub fn deserialize<'de, T: Deserialize<'de>>(mut bytes: &'de [u8], isolated: bool)
    -> Result<T, de::ExtError<'de>> {

    let deserializer = bytes_deserializer(&mut bytes, isolated);
    let res = T::deserialize(deserializer)?;
    if !bytes.is_empty() {
        Err(de::ExtError::Unread(bytes))
    } else {
        Ok(res)
    }
}

pub fn deserialize_seed<'de, T: DeserializeSeed<'de>>(seed: T, mut bytes: &'de [u8], isolated: bool)
    -> Result<T::Value, de::ExtError<'de>> {
    
    let deserializer = bytes_deserializer(&mut bytes, isolated);
    let res = seed.deserialize(deserializer)?;
    if !bytes.is_empty() {
        Err(de::ExtError::Unread(bytes))
    } else {
        Ok(res)
    }
}

fn bytes_deserializer<'a, 'de>(bytes: &'a mut &'de [u8], isolated: bool) -> EslDeserializer<'static, 'a, 'de, &'de [u8]> {
    assert!(!isolated || bytes.len() <= u32::MAX as usize);
    EslDeserializer::new(if isolated { Some(bytes.len() as u32) } else { None }, bytes)
}

pub fn serialized_size<T: Serialize + ?Sized>(v: &T, isolated: bool) -> Result<usize, ser::IoError> {
    let mut writer = Size(0);
    let serializer = EslSerializer::new(isolated, &mut writer);
    v.serialize(serializer)?;
    Ok(writer.0)
}

pub fn serialize_into<T: Serialize + ?Sized>(
    v: &T, writer: &mut (impl Write + ?Sized), isolated: bool
) -> Result<(), ser::IoError> {
    let mut writer = GenericWriter::new(writer);
    let serializer = EslSerializer::new(isolated, &mut writer);
    v.serialize(serializer)
}

pub fn serialize_into_slice<'a, 'b: 'a, T: Serialize + ?Sized>(
    v: &T, bytes: &'b mut &'a mut [u8], isolated: bool
) -> Result<(), ser::IoError> {
    let mut writer = SliceWriter::new(bytes);
    let serializer = EslSerializer::new(isolated, &mut writer);
    v.serialize(serializer)?;
    let written = writer.written();
    *bytes = &mut (*bytes)[written ..];
    Ok(())
}

fn no_io_error(e: ser::IoError) -> ser::Error {
    match e {
        ser::IoError::Other(e) => e,
        ser::IoError::Io(_) => unreachable!()
    }
}

pub fn serialize_into_vec<T: Serialize + ?Sized>(v: &T, bytes: &mut Vec<u8>, isolated: bool) -> Result<(), ser::Error> {
    let serializer = EslSerializer::new(isolated, bytes);
    v.serialize(serializer).map_err(no_io_error)
}

pub fn serialize<T: Serialize + ?Sized>(v: &T, isolated: bool) -> Result<Vec<u8>, ser::Error> {
    let mut bytes = Vec::new();
    let serializer = EslSerializer::new(isolated, &mut bytes);
    v.serialize(serializer).map_err(no_io_error)?;
    Ok(bytes)
}
