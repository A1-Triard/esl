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
use serde::{Serialize, Serializer};
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeMap, SerializeSeq};

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

struct FieldSerializer<'a>(Tag, Either<RecordFlags, (Tag, &'a Field)>);

impl<'a> Serialize for FieldSerializer<'a> {
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

struct RecordBodySerializer<'a>(&'a Record);

impl<'a> Serialize for RecordBodySerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {

        let has_flags = !self.0.flags.is_empty();
        let entry_count = self.0.fields.len() + if has_flags { 1 } else { 0 };
        let mut serializer = serializer.serialize_seq(Some(entry_count))?;
        if has_flags {
            serializer.serialize_element(&FieldSerializer(self.0.tag, Left(self.0.flags)))?;
        }
        for &(field_tag, ref field) in &self.0.fields {
            serializer.serialize_element(&FieldSerializer(self.0.tag, Right((field_tag, &field))))?;
        }
        serializer.end()
    }
}

impl Serialize for Record {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {

        if serializer.is_human_readable() {
            let mut serializer = serializer.serialize_map(Some(1))?;
            serializer.serialize_entry(&self.tag, &RecordBodySerializer(self))?;
            serializer.end()
        } else {
            unimplemented!()
//            let mut record = serializer.serialize_struct("Record", 4)?;
//            record.serialize_field("tag", &self.tag)?;
//            record.serialize_field("size", &)
//            record.serialize_field("flags", &self.flags)?;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use num_traits::cast::FromPrimitive;
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
