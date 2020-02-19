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
use serde::{Serialize, Serializer, Deserialize, Deserializer};
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeMap, SerializeSeq};
use serde::de::{self, DeserializeSeed};
use serde::de::Error as de_Error;

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

struct FieldBodySerializer<'a> {
    record_tag: Tag,
    field_tag: Tag,
    field: &'a Field
}

impl<'a> Serialize for FieldBodySerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        match FieldType::from_tags(self.record_tag, self.field_tag) {
            FieldType::String(_) => if let Field::String(s) = self.field {
                serializer.serialize_str(&s)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have string type", self.record_tag, self.field_tag)))
            },
            FieldType::StringZ => if let Field::StringZ(s) = self.field {
                s.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have zero-terminated string type", self.record_tag, self.field_tag)))
            },
            FieldType::Multiline(_, lb) => if let Field::StringList(s) = self.field {
                if serializer.is_human_readable() {
                    let mut serializer = serializer.serialize_seq(Some(s.len()))?;
                    for line in s {
                        serializer.serialize_element(line)?;
                    }
                    serializer.end()
                } else {
                    let new_line = lb.new_line();
                    let mut capacity = 0;
                    for line in s {
                        if line.contains(new_line) {
                            return Err(S::Error::custom("string list item contains separator"));
                        }
                        capacity += line.len() + new_line.len();
                    }
                    let mut text = String::with_capacity(capacity);
                    for line in s {
                        text.push_str(line);
                        text.push_str(new_line);
                    }
                    text.truncate(text.len() - new_line.len());
                    serializer.serialize_str(&text)
                }
            } else {
                Err(S::Error::custom(&format!("{} {} field should have string list type", self.record_tag, self.field_tag)))
            },
            FieldType::StringZList => if let Field::StringZList(s) = self.field {
                s.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have zero-terminated string list type", self.record_tag, self.field_tag)))
            },
            FieldType::Binary | FieldType::Compressed => if let Field::Binary(v) = self.field {
                serializer.serialize_bytes(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have binary type", self.record_tag, self.field_tag)))
            },
            FieldType::Item => if let Field::Item(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have item type", self.record_tag, self.field_tag)))
            },
            FieldType::Ingredient => if let Field::Ingredient(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have ingredient type", self.record_tag, self.field_tag)))
            },
            FieldType::ScriptMetadata => if let Field::ScriptMetadata(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have script metadata type", self.record_tag, self.field_tag)))
            },
            FieldType::FileMetadata => if let Field::FileMetadata(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have file metadata type", self.record_tag, self.field_tag)))
            },
            FieldType::SavedNpc => if let Field::SavedNpc(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have saved npc type", self.record_tag, self.field_tag)))
            },
            FieldType::Effect => if let Field::Effect(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have effect type", self.record_tag, self.field_tag)))
            },
            FieldType::Npc => if let Field::Npc(v) = self.field {
                if serializer.is_human_readable() {
                    v.serialize(serializer)
                } else {
                    Npc12Or52::from(v.clone()).serialize(serializer)
                }
            } else {
                Err(S::Error::custom(&format!("{} {} field should have NPC type", self.record_tag, self.field_tag)))
            },
            FieldType::DialogMetadata => if let Field::DialogMetadata(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have dialog metadata type", self.record_tag, self.field_tag)))
            },
            FieldType::Float => if let &Field::Float(v) = self.field {
                serializer.serialize_f32(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have float type", self.record_tag, self.field_tag)))
            },
            FieldType::Int => if let &Field::Int(v) = self.field {
                serializer.serialize_i32(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have int type", self.record_tag, self.field_tag)))
            },
            FieldType::Short => if let &Field::Short(v) = self.field {
                serializer.serialize_i16(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have short type", self.record_tag, self.field_tag)))
            },
            FieldType::Long => if let &Field::Long(v) = self.field {
                serializer.serialize_i64(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have long type", self.record_tag, self.field_tag)))
            },
            FieldType::Byte => if let &Field::Byte(v) = self.field {
                serializer.serialize_u8(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have byte type", self.record_tag, self.field_tag)))
            },
        }
    }
}

struct FieldSerializer<'a>(Tag, Either<RecordFlags, (Tag, &'a Field)>);

impl<'a> Serialize for FieldSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        let is_human_readable = serializer.is_human_readable();
        let mut serializer = serializer.serialize_map(Some(1))?;
        match self.1 {
            Left(flags) => serializer.serialize_entry(&META, &flags)?,
            Right((field_tag, field)) => {
                if is_human_readable && field_tag == META {
                    return Err(S::Error::custom("META tag is reserved"));
                }
                serializer.serialize_entry(&field_tag, &FieldBodySerializer { record_tag: self.0, field_tag, field })?;
            }
        };
        serializer.end()
    }
}

struct RecordBodySerializer<'a>(&'a Record);

impl<'a> Serialize for RecordBodySerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        let has_flags = serializer.is_human_readable() && !self.0.flags.is_empty();
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
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        let is_human_readable = serializer.is_human_readable();
        let mut serializer = serializer.serialize_map(Some(1))?;
        if is_human_readable {
            serializer.serialize_entry(&self.tag, &RecordBodySerializer(self))?;
        } else {
            serializer.serialize_entry(&(self.tag, self.flags), &RecordBodySerializer(self))?;
        }
        serializer.end()
    }
}

struct FieldBodyHRDeserializer {
    record_tag: Tag,
    field_tag: Tag
}

impl<'de> DeserializeSeed<'de> for FieldBodyHRDeserializer {
    type Value = Either<RecordFlags, (Tag, Field)>;
    
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where
        D: Deserializer<'de> {

        if self.field_tag == META {
            RecordFlags::deserialize(deserializer).map(Left)
        } else {
            deserializer.deserialize_field(self.record_tag, self.field_tag, None)
                .map(|x| Right((self.field_tag, x)))
        }
    }
}

struct FieldHRDeserializer {
    record_tag: Tag
}

impl<'de> de::Visitor<'de> for FieldHRDeserializer {
    type Value = Either<RecordFlags, (Tag, Field)>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "record flags or field")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error> where
        A: de::MapAccess<'de> {

        let field_tag: Tag = map.next_key()?
            .ok_or_else(|| A::Error::custom("missed field tag"))?;
        let body = map.next_value_seed(FieldBodyHRDeserializer { record_tag: self.record_tag, field_tag })?;
        if map.next_key::<Tag>()?.is_some() {
            return Err(A::Error::custom("duplicated field tag"));
        }
        Ok(body)
    }
}

impl<'de> DeserializeSeed<'de> for FieldHRDeserializer {
    type Value = Either<RecordFlags, (Tag, Field)>;
    
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where
        D: Deserializer<'de> {

        deserializer.deserialize_map(self)
    }
}

struct RecordBodyHRDeserializer {
    record_tag: Tag
}

impl<'de> de::Visitor<'de> for RecordBodyHRDeserializer {
    type Value = Record;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "field list")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut record_flags = None;
        let mut fields = seq.size_hint().map_or_else(Vec::new, Vec::with_capacity);
        while let Some(field) = seq.next_element_seed(FieldHRDeserializer { record_tag: self.record_tag })? {
            match field {
                Left(flags) => {
                    if record_flags.replace(flags).is_some() {
                        return Err(A::Error::custom("duplicated record flags"));
                    }
                },
                Right(field) => fields.push(field)
            }
        }
        Ok(Record { tag: self.record_tag, flags: record_flags.unwrap_or(RecordFlags::empty()), fields })
    }
}

impl<'de> DeserializeSeed<'de> for RecordBodyHRDeserializer {
    type Value = Record;
    
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where
        D: Deserializer<'de> {

        deserializer.deserialize_seq(self)
    }
}

struct RecordHRDeserializer;

impl<'de> de::Visitor<'de> for RecordHRDeserializer {
    type Value = Record;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "record")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error> where
        A: de::MapAccess<'de> {
        
        let record_tag: Tag = map.next_key()?
            .ok_or_else(|| A::Error::custom("missed record tag"))?;
        let body = map.next_value_seed(RecordBodyHRDeserializer { record_tag })?;
        if map.next_key::<Tag>()?.is_some() {
            return Err(A::Error::custom("duplicated record tag"));
        }
        Ok(body)
    }
}

impl<'de> Deserialize<'de> for Record {
    fn deserialize<D>(deserializer: D) -> Result<Record, D::Error> where
        D: Deserializer<'de> {

        if deserializer.is_human_readable() {
            deserializer.deserialize_map(RecordHRDeserializer)
        } else {
            panic!()
//            let bytes = bincode::serialize(&RecordBodyNHRSerializer(self)).map_err(S::Error::custom)?;
//            let mut serializer = serializer.serialize_tuple(4)?;
//            serializer.serialize_element(&self.tag)?;
//            serializer.serialize_element(&(bytes.len() as u32))?;
//            serializer.serialize_element(&self.flags)?;
//            serializer.serialize_element(&BytesSerializer(&bytes))?;
//            serializer.end()
        }
    }
}

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

    #[test]
    fn record_yaml() {
        let record = Record {
            tag: SCPT,
            flags: RecordFlags::PERSISTENT,
            fields: vec![
                (SCHD, Field::ScriptMetadata(ScriptMetadata {
                    name: "Scr1".into(),
                    shorts: 1, longs: 2, floats: 3,
                    data_size: 800, var_table_size: 35
                })),
                (TEXT, Field::StringList(vec![
                    "Begin Scr1\\".into(),
                    "    short\u{7} i".into(),
                    "End Scr1".into(),
                ]))
            ]
        };
        let yaml = serde_yaml::to_string(&record).unwrap();
        let res: Record = serde_yaml::from_str(&yaml).unwrap();
        assert_eq!(res.tag, record.tag);
        assert_eq!(res.flags, record.flags);
        assert_eq!(res.fields.len(), 2);
        assert_eq!(res.fields[0].0, SCHD);
        assert_eq!(res.fields[1].0, TEXT);
        if let Field::ScriptMetadata(res) = &res.fields[0].1 {
            assert_eq!(res.name, "Scr1");
            assert_eq!(res.shorts, 1);
            assert_eq!(res.longs, 2);
            assert_eq!(res.floats, 3);
            assert_eq!(res.data_size, 800);
            assert_eq!(res.var_table_size, 35);
        } else {
            panic!()
        }
        if let Field::StringList(res) = &res.fields[1].1 {
            assert_eq!(res.len(), 3);
            assert_eq!(res[0], "Begin Scr1\\");
            assert_eq!(res[1], "    short\u{7} i");
            assert_eq!(res[2], "End Scr1");
        } else {
            panic!()
        }
    }
}
