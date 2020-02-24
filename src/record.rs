use either::{Either, Left, Right};
use std::fmt::{self, Debug};
use serde::{Serialize, Serializer, Deserialize, Deserializer};
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeMap, SerializeSeq};
use serde::de::{self, DeserializeSeed};
use serde::de::Error as de_Error;
use flate2::write::{ZlibDecoder, ZlibEncoder};
use flate2::Compression;
use std::io::Write;

use crate::field::*;
use crate::serde_helpers::*;
use crate::strings::*;

pub_bitflags_display!(RecordFlags, u64, PERSIST = 0x40000000000, BLOCKED = 0x200000000000, DELETED = 0x2000000000);

enum_serde!(RecordFlags, "record flags", u64, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    pub tag: Tag,
    pub flags: RecordFlags,
    pub fields: Vec<(Tag, Field)>,
}

impl Record {
    pub fn coerce(&mut self) {
        for &mut (field_tag, ref mut field) in self.fields.iter_mut() {
            field.coerce(self.tag, field_tag);
        }
    }
}
    
struct FieldBodySerializer<'a> {
    record_tag: Tag,
    field_tag: Tag,
    field: &'a Field
}

impl<'a> Serialize for FieldBodySerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        match FieldType::from_tags(self.record_tag, self.field_tag) {
            FieldType::String(len) => if let Field::String(s) = self.field {
                if let Some(len) = len {
                    serialize_string_tuple(&s, len as usize, serializer)
                } else {
                    serializer.serialize_str(&s)
                }
            } else {
                Err(S::Error::custom(&format!("{} {} field should have string type", self.record_tag, self.field_tag)))
            },
            FieldType::StringZ => if let Field::StringZ(s) = self.field {
                s.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have zero-terminated string type", self.record_tag, self.field_tag)))
            },
            FieldType::Multiline(newline) => if let Field::StringList(s) = self.field {
                serialize_string_list(&s, newline.as_str(), None, serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have string list type", self.record_tag, self.field_tag)))
            },
            FieldType::StringZList => if let Field::StringZList(s) = self.field {
                s.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have zero-terminated string list type", self.record_tag, self.field_tag)))
            },
            FieldType::Binary => if let Field::Binary(v) = self.field {
                serializer.serialize_bytes(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have binary type", self.record_tag, self.field_tag)))
            },
            FieldType::Compressed => if let Field::Binary(v) = self.field {
                if serializer.is_human_readable() {
                    serializer.serialize_bytes(v)
                } else {
                    let uncompressed = (|| {
                        let mut decoder = ZlibDecoder::new(Vec::new());
                        decoder.write_all(&v[..])?;
                        decoder.finish()
                    })().map_err(|_| S::Error::custom("invalid compressed data"))?;
                    serializer.serialize_bytes(&uncompressed)
                }
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
            FieldType::SpellMetadata => if let Field::SpellMetadata(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have spell metadata type", self.record_tag, self.field_tag)))
            },
            FieldType::Ai => if let Field::Ai(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have AI type", self.record_tag, self.field_tag)))
            },
            FieldType::AiWander => if let Field::AiWander(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have AI wander type", self.record_tag, self.field_tag)))
            },
            FieldType::NpcFlags => if let Field::NpcFlags(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have NPC flags type", self.record_tag, self.field_tag)))
            },
            FieldType::CreatureFlags => if let Field::CreatureFlags(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have creature flags type", self.record_tag, self.field_tag)))
            },
            FieldType::Book => if let Field::Book(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have book type", self.record_tag, self.field_tag)))
            },
            FieldType::Creature => if let Field::Creature(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have creature type", self.record_tag, self.field_tag)))
            },
            FieldType::ContainerFlags => if let Field::ContainerFlags(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have container flags type", self.record_tag, self.field_tag)))
            },
            FieldType::Light => if let Field::Light(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have light type", self.record_tag, self.field_tag)))
            },
            FieldType::MiscItem => if let Field::MiscItem(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have misc item type", self.record_tag, self.field_tag)))
            },
            FieldType::Apparatus => if let Field::Apparatus(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have apparatus type", self.record_tag, self.field_tag)))
            },
            FieldType::Float => if let &Field::Float(v) = self.field {
                serialize_f32_as_is(v, serializer)
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

struct FieldBodyDeserializer {
    record_tag: Tag,
    field_tag: Tag
}

impl<'de> DeserializeSeed<'de> for FieldBodyDeserializer {
    type Value = Either<RecordFlags, (Tag, Field)>;
    
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where
        D: Deserializer<'de> {

        if deserializer.is_human_readable() && self.field_tag == META {
            RecordFlags::deserialize(deserializer).map(Left)
        } else {
            match FieldType::from_tags(self.record_tag, self.field_tag) {
                FieldType::String(len) => if let Some(len) = len {
                    deserialize_string_tuple(len as usize, deserializer)
                } else {
                    String::deserialize(deserializer)
                }.map(Field::String),
                FieldType::StringZ =>
                    StringZ::deserialize(deserializer).map(Field::StringZ),
                FieldType::Multiline(newline) =>
                    deserialize_string_list(newline.as_str(), None, deserializer).map(Field::StringList),
                FieldType::StringZList =>
                    StringZList::deserialize(deserializer).map(Field::StringZList),
                FieldType::Binary => <Vec<u8>>::deserialize(deserializer).map(Field::Binary),
                FieldType::Compressed => if deserializer.is_human_readable() {
                    <Vec<u8>>::deserialize(deserializer)
                } else {
                    let bytes = <&[u8]>::deserialize(deserializer)?;
                    let mut encoder = ZlibEncoder::new(Vec::new(), Compression::new(5));
                    encoder.write_all(bytes).unwrap();
                    Ok(encoder.finish().unwrap())
                }.map(Field::Binary),
                FieldType::Item => Item::deserialize(deserializer).map(Field::Item),
                FieldType::Ingredient => Ingredient::deserialize(deserializer).map(Field::Ingredient),
                FieldType::ScriptMetadata => ScriptMetadata::deserialize(deserializer).map(Field::ScriptMetadata),
                FieldType::FileMetadata => FileMetadata::deserialize(deserializer).map(Field::FileMetadata),
                FieldType::SavedNpc => SavedNpc::deserialize(deserializer).map(Field::SavedNpc),
                FieldType::Effect => Effect::deserialize(deserializer).map(Field::Effect),
                FieldType::Npc => if deserializer.is_human_readable() {
                    Npc::deserialize(deserializer)
                } else {
                    Npc12Or52::deserialize(deserializer).map(Npc::from)
                }.map(Field::Npc),
                FieldType::DialogMetadata => DialogTypeOption::deserialize(deserializer).map(Field::DialogMetadata),
                FieldType::SpellMetadata => SpellMetadata::deserialize(deserializer).map(Field::SpellMetadata),
                FieldType::Ai => Ai::deserialize(deserializer).map(Field::Ai),
                FieldType::AiWander => AiWander::deserialize(deserializer).map(Field::AiWander),
                FieldType::NpcFlags => <FlagsAndBloodTexture<NpcFlags>>::deserialize(deserializer).map(Field::NpcFlags),
                FieldType::CreatureFlags => <FlagsAndBloodTexture<CreatureFlags>>::deserialize(deserializer).map(Field::CreatureFlags),
                FieldType::Book => Book::deserialize(deserializer).map(Field::Book),
                FieldType::Light => Light::deserialize(deserializer).map(Field::Light),
                FieldType::MiscItem => MiscItem::deserialize(deserializer).map(Field::MiscItem),
                FieldType::Apparatus => Apparatus::deserialize(deserializer).map(Field::Apparatus),
                FieldType::Creature => Creature::deserialize(deserializer).map(Field::Creature),
                FieldType::ContainerFlags => ContainerFlags::deserialize(deserializer).map(Field::ContainerFlags),
                FieldType::Float => deserialize_f32_as_is(deserializer).map(Field::Float),
                FieldType::Int => i32::deserialize(deserializer).map(Field::Int),
                FieldType::Short => i16::deserialize(deserializer).map(Field::Short),
                FieldType::Long => i64::deserialize(deserializer).map(Field::Long),
                FieldType::Byte => u8::deserialize(deserializer).map(Field::Byte),
            }.map(|x| Right((self.field_tag, x)))
        }
    }
}

struct FieldDeserializer {
    record_tag: Tag
}

impl<'de> de::Visitor<'de> for FieldDeserializer {
    type Value = Either<RecordFlags, (Tag, Field)>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "record flags or field")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error> where
        A: de::MapAccess<'de> {

        let field_tag: Tag = map.next_key()?
            .ok_or_else(|| A::Error::custom("missed field tag"))?;
        let body = map.next_value_seed(FieldBodyDeserializer { record_tag: self.record_tag, field_tag })?;
        if map.next_key::<Tag>()?.is_some() {
            return Err(A::Error::custom("duplicated field tag"));
        }
        Ok(body)
    }
}

impl<'de> DeserializeSeed<'de> for FieldDeserializer {
    type Value = Either<RecordFlags, (Tag, Field)>;
    
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where
        D: Deserializer<'de> {

        deserializer.deserialize_map(self)
    }
}

struct RecordBodyDeserializer {
    record_tag: Tag,
    record_flags: Option<RecordFlags>
}

impl<'de> de::Visitor<'de> for RecordBodyDeserializer {
    type Value = Record;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "field list")
    }

    fn visit_seq<A>(mut self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut fields = seq.size_hint().map_or_else(Vec::new, Vec::with_capacity);
        while let Some(field) = seq.next_element_seed(FieldDeserializer { record_tag: self.record_tag })? {
            match field {
                Left(flags) => {
                    if self.record_flags.replace(flags).is_some() {
                        return Err(A::Error::custom("duplicated record flags"));
                    }
                },
                Right(field) => fields.push(field)
            }
        }
        Ok(Record { tag: self.record_tag, flags: self.record_flags.unwrap_or(RecordFlags::empty()), fields })
    }
}

impl<'de> DeserializeSeed<'de> for RecordBodyDeserializer {
    type Value = Record;
    
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where
        D: Deserializer<'de> {

        deserializer.deserialize_seq(self)
    }
}

struct RecordDeserializer {
    is_human_readable: bool
}

impl<'de> de::Visitor<'de> for RecordDeserializer {
    type Value = Record;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "record")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error> where A: de::MapAccess<'de> {
        let (record_tag, record_flags) = if self.is_human_readable {
            let record_tag: Tag = map.next_key()?
                .ok_or_else(|| A::Error::custom("missed record tag"))?;
            (record_tag, None)
        } else {
            let (record_tag, record_flags): (Tag, RecordFlags) = map.next_key()?
                .ok_or_else(|| A::Error::custom("missed record tag and flags"))?;
            (record_tag, Some(record_flags))
        };
        let body = map.next_value_seed(RecordBodyDeserializer { record_tag, record_flags })?;
        if map.next_key::<Tag>()?.is_some() {
            return Err(A::Error::custom("duplicated record tag"));
        }
        Ok(body)
    }
}

impl<'de> Deserialize<'de> for Record {
    fn deserialize<D>(deserializer: D) -> Result<Record, D::Error> where D: Deserializer<'de> {
        let is_human_readable = deserializer.is_human_readable();
        deserializer.deserialize_map(RecordDeserializer { is_human_readable } )
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
        assert_eq!(RecordFlags::PERSIST, *&RecordFlags::PERSIST);
        assert!(RecordFlags::PERSIST < RecordFlags::BLOCKED);
        let mut hasher = DefaultHasher::new();
        RecordFlags::DELETED.hash(&mut hasher);
    }

    #[test]
    fn test_record_flags() {
        assert_eq!("PERSISTENT", format!("{}", RecordFlags::PERSIST));
        assert_eq!("PERSISTENT", format!("{:?}", RecordFlags::PERSIST));
        assert_eq!("PERSISTENT DELETED", format!("{}", RecordFlags::PERSIST | RecordFlags::DELETED));
        assert_eq!(0x202000000000, (RecordFlags::BLOCKED | RecordFlags::DELETED).bits);
        assert_eq!(Some(RecordFlags::BLOCKED | RecordFlags::DELETED), RecordFlags::from_bits(0x202000000000));
        assert_eq!(Ok(RecordFlags::DELETED), RecordFlags::from_str("DELETED"));
        assert_eq!(Ok(RecordFlags::DELETED | RecordFlags::PERSIST), RecordFlags::from_str("DELETED PERSISTENT"));
        assert_eq!(Ok(RecordFlags::DELETED | RecordFlags::PERSIST), RecordFlags::from_str("PERSISTENT  DELETED"));
        assert_eq!(Ok(RecordFlags::empty()), RecordFlags::from_str(""));
        assert_eq!(Err(()), RecordFlags::from_str(" "));
    }

    #[test]
    fn record_yaml() {
        let record = Record {
            tag: SCPT,
            flags: RecordFlags::PERSIST,
            fields: vec![
                (SCHD, Field::ScriptMetadata(ScriptMetadata {
                    name: "Scr1".into(),
                    shorts: 1, longs: 2, floats: 3,
                    data_size: 800, var_table_size: 35
                })),
                (SCTX, Field::StringList(vec![
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
        assert_eq!(res.fields[1].0, SCTX);
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
