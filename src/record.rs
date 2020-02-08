use either::{Either, Left, Right};
use std::fmt::{self, Display, Debug};
use std::str::{FromStr};
use ::nom::IResult;
use ::nom::branch::alt;
use ::nom::combinator::{value as nom_value, map, opt};
use ::nom::bytes::complete::tag as nom_tag;
use ::nom::multi::{fold_many0};
use ::nom::sequence::{preceded, terminated, pair};
use ::nom::bytes::complete::take_while;
//use serde::{Serialize, Serializer, Deserialize, Deserializer};
//use serde::ser::Error as ser_Error;
//use serde::ser::{SerializeMap, SerializeSeq, SerializeTuple};
//use serde::de::{self};
use serde::{Serialize, Serializer};
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeMap, SerializeSeq, SerializeTuple};

use crate::field::*;
use crate::field_serde::*;

bitflags! {
    pub struct RecordFlags: u64 {
        const PERSISTENT = 0x40000000000;
        const BLOCKED = 0x200000000000;
        const DELETED = 0x2000000000;
    }
}

impl Display for RecordFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

fn pipe(input: &str) -> IResult<&str, (), ()> {
    nom_value((),
              terminated(preceded(take_while(char::is_whitespace), nom_tag("|")), take_while(char::is_whitespace))
    )(input)
}

fn record_flag(input: &str) -> IResult<&str, RecordFlags, ()> {
    alt((
        nom_value(RecordFlags::PERSISTENT, nom_tag("PERSISTENT")),
        nom_value(RecordFlags::BLOCKED, nom_tag("BLOCKED")),
        nom_value(RecordFlags::DELETED, nom_tag("DELETED"))
    ))(input)
}

impl FromStr for RecordFlags {
    type Err = ();

    fn from_str(s: &str) -> Result<RecordFlags, Self::Err> {
        let (unconsumed, flags) = map(opt(map(pair(
            record_flag,
            fold_many0(preceded(pipe, record_flag), RecordFlags::empty(), |a, v| a | v)
        ), |(a, b)| a | b)), |m| m.unwrap_or(RecordFlags::empty()))(s).map_err(|_: ::nom::Err<()>| ())?;
        if !unconsumed.is_empty() { return Err(()); }
        Ok(flags)
    }
}

enum_serde!({
    RecordFlags, RecordFlagsDeserializer, "record flags",
    u64, from_bits, bits, visit_u64, serialize_u64, deserialize_u64,
    Unsigned, u64
});

#[derive(Debug, Clone)]
pub struct Record {
    pub tag: Tag,
    pub flags: RecordFlags,
    pub fields: Vec<(Tag, Field)>,
}

struct FieldBodySerializer<'a>(Tag, Tag, &'a Field);

impl<'a> Serialize for FieldBodySerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {
 
        serializer.serialize_field(self.0, self.1, self.2)
    }
}

struct FieldHRSerializer<'a>(Tag, Either<RecordFlags, (Tag, &'a Field)>);

impl<'a> Serialize for FieldHRSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {

        let mut serializer = serializer.serialize_map(Some(1))?;
        match self.1 {
            Left(flags) => serializer.serialize_entry(&META, &flags)?,
            Right((field_tag, field)) => {
                if field_tag == META {
                    return Err(S::Error::custom("META tag is reserved"));
                }
                serializer.serialize_entry(&field_tag, &FieldBodySerializer(self.0, field_tag, field))?;
            }
        };
        serializer.end()
    }
}

struct BytesSerializer<'a>(&'a [u8]);

impl<'a> Serialize for BytesSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {
    
        serializer.serialize_bytes_ext(self.0)
    }
}

struct FieldNHRSerializer<'a>(Tag, Tag, &'a Field);

impl<'a> Serialize for FieldNHRSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {

        let bytes = bincode::serialize(&FieldBodySerializer(self.0, self.1, self.2))
            .map_err(S::Error::custom)?;
        let mut serializer = serializer.serialize_tuple(3)?;
        serializer.serialize_element(&self.1)?;
        serializer.serialize_element(&(bytes.len() as u32))?;
        serializer.serialize_element(&BytesSerializer(&bytes))?;
        serializer.end()
    }
}

struct RecordBodyHRSerializer<'a>(&'a Record);

impl<'a> Serialize for RecordBodyHRSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {

        let has_flags = !self.0.flags.is_empty();
        let entry_count = self.0.fields.len() + if has_flags { 1 } else { 0 };
        let mut serializer = serializer.serialize_seq(Some(entry_count))?;
        if has_flags {
            serializer.serialize_element(&FieldHRSerializer(self.0.tag, Left(self.0.flags)))?;
        }
        for &(field_tag, ref field) in &self.0.fields {
            serializer.serialize_element(&FieldHRSerializer(self.0.tag, Right((field_tag, &field))))?;
        }
        serializer.end()
    }
}

struct RecordBodyNHRSerializer<'a>(&'a Record);

impl<'a> Serialize for RecordBodyNHRSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {

        let mut serializer = serializer.serialize_tuple(self.0.fields.len())?;
        for &(field_tag, ref field) in &self.0.fields {
            serializer.serialize_element(&FieldNHRSerializer(self.0.tag, field_tag, &field))?;
        }
        serializer.end()
    }
}

impl Serialize for Record {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {

        if serializer.is_human_readable() {
            let mut serializer = serializer.serialize_map(Some(1))?;
            serializer.serialize_entry(&self.tag, &RecordBodyHRSerializer(self))?;
            serializer.end()
        } else {
            let bytes = bincode::serialize(&RecordBodyNHRSerializer(self)).map_err(S::Error::custom)?;
            let mut serializer = serializer.serialize_tuple(4)?;
            serializer.serialize_element(&self.tag)?;
            serializer.serialize_element(&(bytes.len() as u32))?;
            serializer.serialize_element(&self.flags)?;
            serializer.serialize_element(&BytesSerializer(&bytes))?;
            serializer.end()
        }
    }
}

/*
struct RecordHRDeserializer;

impl<'de> de::Visitor<'de> for RecordHRDeserializer {
    type Value = Record;

    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "record")
    }

    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error> where
        A: de::MapAccess<'de> {
        
        let record: (Tag, ) = map.next_entry()?;
    }
}


impl<'de> Deserialize<'de> for Record {
    fn deserialize<D>(deserializer: D) -> Result<Record, D::Error> where
        D: Deserializer {

        if deserializer.is_human_readable() {
            deserializer.deserialize_map(RecordHRDeserializer)
        } else {
            let bytes = bincode::serialize(&RecordBodyNHRSerializer(self)).map_err(S::Error::custom)?;
            let mut serializer = serializer.serialize_tuple(4)?;
            serializer.serialize_element(&self.tag)?;
            serializer.serialize_element(&(bytes.len() as u32))?;
            serializer.serialize_element(&self.flags)?;
            serializer.serialize_element(&BytesSerializer(&bytes))?;
            serializer.end()
        }
    }
}*/

#[cfg(test)]
mod tests {
    use crate::*;
    use std::str::FromStr;
    use std::hash::Hash;
    use std::collections::hash_map::DefaultHasher;

    #[test]
    fn record_flags_traits() {
        assert_eq!(RecordFlags::PERSISTENT, *&RecordFlags::PERSISTENT);
        assert!(RecordFlags::PERSISTENT < RecordFlags::BLOCKED);
        let mut hasher = DefaultHasher::new();
        RecordFlags::DELETED.hash(&mut hasher);
    }

    #[test]
    fn test_record_flags() {
        assert_eq!("PERSISTENT", format!("{}", RecordFlags::PERSISTENT));
        assert_eq!("PERSISTENT", format!("{:?}", RecordFlags::PERSISTENT));
        assert_eq!("PERSISTENT | DELETED", format!("{}", RecordFlags::PERSISTENT | RecordFlags::DELETED));
        assert_eq!(0x202000000000, (RecordFlags::BLOCKED | RecordFlags::DELETED).bits);
        assert_eq!(Some(RecordFlags::BLOCKED | RecordFlags::DELETED), RecordFlags::from_bits(0x202000000000));
        assert_eq!(Ok(RecordFlags::DELETED), RecordFlags::from_str("DELETED"));
        assert_eq!(Ok(RecordFlags::DELETED | RecordFlags::PERSISTENT), RecordFlags::from_str("DELETED|PERSISTENT"));
        assert_eq!(Ok(RecordFlags::DELETED | RecordFlags::PERSISTENT), RecordFlags::from_str("PERSISTENT | DELETED"));
        assert_eq!(Ok(RecordFlags::empty()), RecordFlags::from_str(""));
        assert_eq!(Err(()), RecordFlags::from_str(" "));
    }
}
