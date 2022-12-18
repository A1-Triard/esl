use either::{Either, Left, Right};
use std::fmt::{self, Debug, Formatter};
use serde::{Serialize, Serializer, Deserialize, Deserializer};
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeMap, SerializeSeq};
use serde::de::{self, DeserializeSeed, Unexpected, VariantAccess};
use serde::de::Error as de_Error;
use flate2::write::{ZlibDecoder, ZlibEncoder};
use flate2::Compression;
use std::io::Write;
use nameof::name_of;

use crate::field::*;
use crate::script_data::*;
use crate::serde_helpers::*;
use crate::strings::*;

bitflags_ext! {
    pub struct RecordFlags: u64 {
        PERSIST = 0x40000000000,
        BLOCKED = 0x200000000000,
        DELETED = 0x2000000000,
    }
}

enum_serde!(RecordFlags, "record flags", u64, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    pub tag: Tag,
    pub flags: RecordFlags,
    pub fields: Vec<(Tag, Field)>,
}

impl Record {
    pub fn fit(&mut self) {
        let mut prev_tag = META;
        for &mut (field_tag, ref mut field) in self.fields.iter_mut() {
            field.fit(self.tag, prev_tag, field_tag);
            prev_tag = field_tag;
        }
    }
}
    
struct FieldBodySerializer<'a> {
    record_tag: Tag,
    prev_tag: Tag,
    field_tag: Tag,
    field: &'a Field
}

impl<'a> Serialize for FieldBodySerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        match FieldType::from_tags(self.record_tag, self.prev_tag, self.field_tag) {
            FieldType::String(len) => if let Field::String(s) = self.field {
                if let Some(len) = len {
                    serialize_short_string(s, len as usize, serializer)
                } else {
                    serializer.serialize_str(s)
                }
            } else {
                Err(S::Error::custom(format!("{} {} field should have string type", self.record_tag, self.field_tag)))
            },
            FieldType::StringZ => if let Field::StringZ(s) = self.field {
                s.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have zero-terminated string type", self.record_tag, self.field_tag)))
            },
            FieldType::Multiline(newline) => if let Field::StringList(s) = self.field {
                StringListSer { lines: s, separator: newline.as_str(), len: None }.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have string list type", self.record_tag, self.field_tag)))
            },
            FieldType::StringZList => if let Field::StringZList(s) = self.field {
                s.serialize(serializer)
            } else {
                Err(S::Error::custom(
                    format!("{} {} field should have zero-terminated string list type", self.record_tag, self.field_tag)
                ))
            },
            FieldType::U8List => if let Field::U8List(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have byte list type", self.record_tag, self.field_tag)))
            },
            FieldType::ScriptData => if let Field::ScriptData(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have script data type", self.record_tag, self.field_tag)))
            },
            FieldType::U8ListZip => if let Field::U8List(v) = self.field {
                if serializer.is_human_readable() {
                    serializer.serialize_str(&base64::encode(v))
                } else {
                    let uncompressed = (|| {
                        let mut decoder = ZlibDecoder::new(Vec::new());
                        decoder.write_all(&v[..])?;
                        decoder.finish()
                    })().map_err(|_| S::Error::custom("invalid compressed data"))?;
                    serializer.serialize_bytes(&uncompressed)
                }
            } else {
                Err(S::Error::custom(format!("{} {} field should have byte list type", self.record_tag, self.field_tag)))
            },
            FieldType::CurrentTime => if let Field::CurrentTime(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have current time type", self.record_tag, self.field_tag)))
            },
            FieldType::Time => if let Field::Time(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have time type", self.record_tag, self.field_tag)))
            },
            FieldType::Item => if let Field::Item(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have item type", self.record_tag, self.field_tag)))
            },
            FieldType::Skill => if let Field::Skill(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have skill type", self.record_tag, self.field_tag)))
            },
            FieldType::EffectArg => if let Field::EffectArg(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have effect arg type", self.record_tag, self.field_tag)))
            },
            FieldType::Ingredient => if let Field::Ingredient(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have ingredient type", self.record_tag, self.field_tag)))
            },
            FieldType::ScriptMetadata => if let Field::ScriptMetadata(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have script metadata type", self.record_tag, self.field_tag)))
            },
            FieldType::ScriptVars => if let Field::ScriptVars(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have script vars type", self.record_tag, self.field_tag)))
            },
            FieldType::FileMetadata => if let Field::FileMetadata(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have file metadata type", self.record_tag, self.field_tag)))
            },
            FieldType::NpcState => if let Field::NpcState(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have saved npc type", self.record_tag, self.field_tag)))
            },
            FieldType::Effect => if let Field::Effect(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have effect type", self.record_tag, self.field_tag)))
            },
            FieldType::Npc => if let Field::Npc(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have NPC type", self.record_tag, self.field_tag)))
            },
            FieldType::DialogMetadata => match self.field {
                &Field::DialogType(v) => DialogTypeOption::Some(v).serialize(serializer),
                &Field::I32(v) => DialogTypeOption::None(v).serialize(serializer),
                _ => Err(S::Error::custom(format!("{} {} field should have dialog or 32-bit int type", self.record_tag, self.field_tag)))
            },
            FieldType::PosRotOrCell => match &self.field {
                &Field::PosRot(v) => PosRotOrCell::PosRot(v.clone()).serialize(serializer),
                &Field::Cell(v) => PosRotOrCell::Cell(v.clone()).serialize(serializer),
                _ => Err(S::Error::custom(format!("{} {} field should have pos_rot or cell type", self.record_tag, self.field_tag)))
            },
            FieldType::Spell => if let Field::Spell(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have spell type", self.record_tag, self.field_tag)))
            },
            FieldType::Ai => if let Field::Ai(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have AI type", self.record_tag, self.field_tag)))
            },
            FieldType::AiWander => if let Field::AiWander(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have AI wander type", self.record_tag, self.field_tag)))
            },
            FieldType::AiTravel => if let Field::AiTravel(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have AI travel type", self.record_tag, self.field_tag)))
            },
            FieldType::AiTarget => if let Field::AiTarget(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have AI target type", self.record_tag, self.field_tag)))
            },
            FieldType::AiActivate => if let Field::AiActivate(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have AI activate type", self.record_tag, self.field_tag)))
            },
            FieldType::NpcFlags => if let Field::NpcFlags(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have NPC flags type", self.record_tag, self.field_tag)))
            },
            FieldType::CreatureFlags => if let Field::CreatureFlags(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have creature flags type", self.record_tag, self.field_tag)))
            },
            FieldType::Book => if let Field::Book(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have book type", self.record_tag, self.field_tag)))
            },
            FieldType::Info => if let Field::Info(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have info type", self.record_tag, self.field_tag)))
            },
            FieldType::Tool => if let Field::Tool(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have tool type", self.record_tag, self.field_tag)))
            },
            FieldType::RepairItem => if let Field::Tool(v) = self.field {
                if serializer.is_human_readable() {
                    v.serialize(serializer)
                } else {
                    RepairItem::from(v.clone()).serialize(serializer)
                }
            } else {
                Err(S::Error::custom(format!("{} {} field should have tool type", self.record_tag, self.field_tag)))
            },
            FieldType::Creature => if let Field::Creature(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have creature type", self.record_tag, self.field_tag)))
            },
            FieldType::ContainerFlags => if let Field::ContainerFlags(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have container flags type", self.record_tag, self.field_tag)))
            },
            FieldType::Light => if let Field::Light(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have light type", self.record_tag, self.field_tag)))
            },
            FieldType::Interior => if let Field::Interior(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have interior type", self.record_tag, self.field_tag)))
            },
            FieldType::MiscItem => if let Field::MiscItem(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have misc item type", self.record_tag, self.field_tag)))
            },
            FieldType::Apparatus => if let Field::Apparatus(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have apparatus type", self.record_tag, self.field_tag)))
            },
            FieldType::Weapon => if let Field::Weapon(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have weapon type", self.record_tag, self.field_tag)))
            },
            FieldType::Armor => if let Field::Armor(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have armor type", self.record_tag, self.field_tag)))
            },
            FieldType::Pos => if let Field::Pos(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have pos type", self.record_tag, self.field_tag)))
            },
            FieldType::PosRot => if let Field::PosRot(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have pos_rot type", self.record_tag, self.field_tag)))
            },
            FieldType::BipedObject => if let Field::BipedObject(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have biped object type", self.record_tag, self.field_tag)))
            },
            FieldType::BodyPart => if let Field::BodyPart(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have body part type", self.record_tag, self.field_tag)))
            },
            FieldType::Clothing => if let Field::Clothing(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have clothing type", self.record_tag, self.field_tag)))
            },
            FieldType::Enchantment => if let Field::Enchantment(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have enchantment type", self.record_tag, self.field_tag)))
            },
            FieldType::Weather => if let Field::Weather(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have weather type", self.record_tag, self.field_tag)))
            },
            FieldType::SoundChance => if let Field::SoundChance(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have sound chance type", self.record_tag, self.field_tag)))
            },
            FieldType::Color => if let Field::Color(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have color type", self.record_tag, self.field_tag)))
            },
            FieldType::Sound => if let Field::Sound(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have sound type", self.record_tag, self.field_tag)))
            },
            FieldType::Potion => if let Field::Potion(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have potion type", self.record_tag, self.field_tag)))
            },
            FieldType::Class => if let Field::Class(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have class type", self.record_tag, self.field_tag)))
            },
            FieldType::Attributes => if let Field::Attributes(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have attributes type", self.record_tag, self.field_tag)))
            },
            FieldType::Skills => if let Field::Skills(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have skills type", self.record_tag, self.field_tag)))
            },
            FieldType::Grid => if let Field::Grid(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have grid type", self.record_tag, self.field_tag)))
            },
            FieldType::PathGrid => if let Field::PathGrid(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have path grid type", self.record_tag, self.field_tag)))
            },
            FieldType::SoundGen => if let Field::SoundGen(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have sound gen type", self.record_tag, self.field_tag)))
            },
            FieldType::Tag => if let Field::Tag(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have tag type", self.record_tag, self.field_tag)))
            },
            FieldType::EffectIndex => if let Field::EffectIndex(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have effect index type", self.record_tag, self.field_tag)))
            },
            FieldType::EffectMetadata => if let Field::EffectMetadata(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have effect metadata type", self.record_tag, self.field_tag)))
            },
            FieldType::Race => if let Field::Race(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have race type", self.record_tag, self.field_tag)))
            },
            FieldType::Faction => if let Field::Faction(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have faction type", self.record_tag, self.field_tag)))
            },
            FieldType::SkillMetadata => if let Field::SkillMetadata(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have skill metadata type", self.record_tag, self.field_tag)))
            },
            FieldType::F32 => if let &Field::F32(v) = self.field {
                F32AsIsSerDe(v).serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have 32-bit float type", self.record_tag, self.field_tag)))
            },
            FieldType::MarkerU8(none) => if let Field::None = self.field {
                NoneU8SerDe { none }.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have none type", self.record_tag, self.field_tag)))
            },
            FieldType::Bool8 => if let &Field::Bool(v) = self.field {
                BoolU8SerDe(v).serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have bool type", self.record_tag, self.field_tag)))
            },
            FieldType::Bool32 => if let &Field::Bool(v) = self.field {
                BoolU32SerDe(v).serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have bool type", self.record_tag, self.field_tag)))
            },
            FieldType::I32 => if let &Field::I32(v) = self.field {
                serializer.serialize_i32(v)
            } else {
                Err(S::Error::custom(format!("{} {} field should have 32-bit int type", self.record_tag, self.field_tag)))
            },
            FieldType::I16 => if let &Field::I16(v) = self.field {
                serializer.serialize_i16(v)
            } else {
                Err(S::Error::custom(format!("{} {} field should have 16-bit int type", self.record_tag, self.field_tag)))
            },
            FieldType::I64 => if let &Field::I64(v) = self.field {
                serializer.serialize_i64(v)
            } else {
                Err(S::Error::custom(format!("{} {} field should have 64-bit int type", self.record_tag, self.field_tag)))
            },
            FieldType::F32List => if let Field::F32List(v) = self.field {
                F32sAsIsSer(v).serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have 32-bit float list type", self.record_tag, self.field_tag)))
            },
            FieldType::I32List => if let Field::I32List(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have 32-bit int list type", self.record_tag, self.field_tag)))
            },
            FieldType::I16List => if let Field::I16List(v) = self.field {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(format!("{} {} field should have 16-bit int list type", self.record_tag, self.field_tag)))
            },
            FieldType::U8 => if let &Field::U8(v) = self.field {
                serializer.serialize_u8(v)
            } else {
                Err(S::Error::custom(format!("{} {} field should have byte type", self.record_tag, self.field_tag)))
            },
        }
    }
}

struct FieldSerializer<'a>(Tag, Tag, Either<RecordFlags, (Tag, &'a Field)>);

impl<'a> Serialize for FieldSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        let is_human_readable = serializer.is_human_readable();
        let mut serializer = serializer.serialize_map(Some(1))?;
        match self.2 {
            Left(flags) => serializer.serialize_entry(&META, &flags)?,
            Right((field_tag, field)) => {
                if is_human_readable && field_tag == META {
                    return Err(S::Error::custom("META tag is reserved"));
                }
                serializer.serialize_entry(&field_tag, &FieldBodySerializer {
                    record_tag: self.0, prev_tag: self.1, field_tag, field
                })?;
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
            serializer.serialize_element(&FieldSerializer(self.0.tag, META, Left(self.0.flags)))?;
        }
        let mut prev_tag = META;
        for &(field_tag, ref field) in &self.0.fields {
            serializer.serialize_element(&FieldSerializer(self.0.tag, prev_tag, Right((field_tag, field))))?;
            prev_tag = field_tag;
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

struct Base64Deserializer;

impl<'de> de::Visitor<'de> for Base64Deserializer {
    type Value = Vec<u8>;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result { write!(f, "base64") }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> where E: de::Error {
        base64::decode(s).map_err(|_| E::invalid_value(Unexpected::Str(s), &self))
    }
}

struct ZlibEncoderDeserializer;

impl<'de> de::Visitor<'de> for ZlibEncoderDeserializer {
    type Value = Vec<u8>;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result { write!(f, "bytes") }

    fn visit_bytes<E>(self, bytes: &[u8]) -> Result<Self::Value, E> where E: de::Error {
        let mut encoder = ZlibEncoder::new(Vec::new(), Compression::new(5));
        encoder.write_all(bytes).unwrap();
        Ok(encoder.finish().unwrap())
    }
}

struct FieldBodyDeserializer {
    record_tag: Tag,
    prev_tag: Tag,
    field_tag: Tag
}

impl<'de> DeserializeSeed<'de> for FieldBodyDeserializer {
    type Value = Either<RecordFlags, (Tag, Field)>;
    
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error> where
        D: Deserializer<'de> {

        if deserializer.is_human_readable() && self.field_tag == META {
            RecordFlags::deserialize(deserializer).map(Left)
        } else {
            match FieldType::from_tags(self.record_tag, self.prev_tag, self.field_tag) {
                FieldType::String(len) => if let Some(len) = len {
                    deserialize_short_string(len as usize, deserializer)
                } else {
                    String::deserialize(deserializer)
                }.map(Field::String),
                FieldType::StringZ =>
                    StringZ::deserialize(deserializer).map(Field::StringZ),
                FieldType::Multiline(newline) =>
                    StringListDe { separator: newline.as_str(), len: None }.deserialize(deserializer).map(Field::StringList),
                FieldType::StringZList =>
                    StringZList::deserialize(deserializer).map(Field::StringZList),
                FieldType::U8List => <Vec<u8>>::deserialize(deserializer).map(Field::U8List),
                FieldType::ScriptData => ScriptData::deserialize(deserializer).map(Field::ScriptData),
                FieldType::U8ListZip => if deserializer.is_human_readable() {
                    deserializer.deserialize_str(Base64Deserializer)
                } else {
                    deserializer.deserialize_bytes(ZlibEncoderDeserializer)
                }.map(Field::U8List),
                FieldType::Info => Info::deserialize(deserializer).map(Field::Info),
                FieldType::Item => Item::deserialize(deserializer).map(Field::Item),
                FieldType::CurrentTime => CurrentTime::deserialize(deserializer).map(Field::CurrentTime),
                FieldType::Time => Time::deserialize(deserializer).map(Field::Time),
                FieldType::Ingredient => Ingredient::deserialize(deserializer).map(Field::Ingredient),
                FieldType::ScriptMetadata => ScriptMetadata::deserialize(deserializer).map(Field::ScriptMetadata),
                FieldType::ScriptVars => ScriptVars::deserialize(deserializer).map(Field::ScriptVars),
                FieldType::FileMetadata => FileMetadata::deserialize(deserializer).map(Field::FileMetadata),
                FieldType::NpcState => NpcState::deserialize(deserializer).map(Field::NpcState),
                FieldType::Effect => Effect::deserialize(deserializer).map(Field::Effect),
                FieldType::Potion => Potion::deserialize(deserializer).map(Field::Potion),
                FieldType::Npc => Npc::deserialize(deserializer).map(Field::Npc),
                FieldType::DialogMetadata => DialogTypeOption::deserialize(deserializer).map(|x| x.into()),
                FieldType::PosRotOrCell => PosRotOrCell::deserialize(deserializer).map(|x| x.into()),
                FieldType::Spell => Spell::deserialize(deserializer).map(Field::Spell),
                FieldType::Pos => Pos::deserialize(deserializer).map(Field::Pos),
                FieldType::PosRot => PosRot::deserialize(deserializer).map(Field::PosRot),
                FieldType::Sound => Sound::deserialize(deserializer).map(Field::Sound),
                FieldType::Skill => Skill::deserialize(deserializer).map(Field::Skill),
                FieldType::EffectArg => EffectArg::deserialize(deserializer).map(Field::EffectArg),
                FieldType::EffectIndex => EffectIndex::deserialize(deserializer).map(Field::EffectIndex),
                FieldType::Tag => Tag::deserialize(deserializer).map(Field::Tag),
                FieldType::EffectMetadata => EffectMetadata::deserialize(deserializer).map(Field::EffectMetadata),
                FieldType::Ai => Ai::deserialize(deserializer).map(Field::Ai),
                FieldType::AiWander => AiWander::deserialize(deserializer).map(Field::AiWander),
                FieldType::AiTravel => AiTravel::deserialize(deserializer).map(Field::AiTravel),
                FieldType::AiTarget => AiTarget::deserialize(deserializer).map(Field::AiTarget),
                FieldType::AiActivate => AiActivate::deserialize(deserializer).map(Field::AiActivate),
                FieldType::NpcFlags => <FlagsAndBlood<NpcFlags>>::deserialize(deserializer).map(Field::NpcFlags),
                FieldType::CreatureFlags => <FlagsAndBlood<CreatureFlags>>::deserialize(deserializer).map(Field::CreatureFlags),
                FieldType::SoundChance => SoundChance::deserialize(deserializer).map(Field::SoundChance),
                FieldType::SkillMetadata => SkillMetadata::deserialize(deserializer).map(Field::SkillMetadata),
                FieldType::Color => Color::deserialize(deserializer).map(Field::Color),
                FieldType::Interior => Interior::deserialize(deserializer).map(Field::Interior),
                FieldType::Book => Book::deserialize(deserializer).map(Field::Book),
                FieldType::SoundGen => SoundGen::deserialize(deserializer).map(Field::SoundGen),
                FieldType::Tool => Tool::deserialize(deserializer).map(Field::Tool),
                FieldType::RepairItem => if deserializer.is_human_readable() {
                    Tool::deserialize(deserializer)
                } else {
                    RepairItem::deserialize(deserializer).map(|x| x.into())
                }.map(Field::Tool),
                FieldType::Weather => Weather::deserialize(deserializer).map(Field::Weather),
                FieldType::Light => Light::deserialize(deserializer).map(Field::Light),
                FieldType::Race => Race::deserialize(deserializer).map(Field::Race),
                FieldType::Faction => Faction::deserialize(deserializer).map(Field::Faction),
                FieldType::MiscItem => MiscItem::deserialize(deserializer).map(Field::MiscItem),
                FieldType::Apparatus => Apparatus::deserialize(deserializer).map(Field::Apparatus),
                FieldType::Weapon => Weapon::deserialize(deserializer).map(Field::Weapon),
                FieldType::Armor => Armor::deserialize(deserializer).map(Field::Armor),
                FieldType::BipedObject => BipedObject::deserialize(deserializer).map(Field::BipedObject),
                FieldType::BodyPart => BodyPart::deserialize(deserializer).map(Field::BodyPart),
                FieldType::Clothing => Clothing::deserialize(deserializer).map(Field::Clothing),
                FieldType::Enchantment => Enchantment::deserialize(deserializer).map(Field::Enchantment),
                FieldType::Creature => Creature::deserialize(deserializer).map(Field::Creature),
                FieldType::ContainerFlags => ContainerFlags::deserialize(deserializer).map(Field::ContainerFlags),
                FieldType::Class => Class::deserialize(deserializer).map(Field::Class),
                FieldType::Attributes => Attributes::deserialize(deserializer).map(Field::Attributes),
                FieldType::Skills => Skills::deserialize(deserializer).map(Field::Skills),
                FieldType::Grid => Grid::deserialize(deserializer).map(Field::Grid),
                FieldType::PathGrid => PathGrid::deserialize(deserializer).map(Field::PathGrid),
                FieldType::MarkerU8(none) => NoneU8SerDe { none }.deserialize(deserializer).map(|()| Field::None),
                FieldType::Bool8 => BoolU8SerDe::deserialize(deserializer).map(|x| Field::Bool(x.0)),
                FieldType::Bool32 => BoolU32SerDe::deserialize(deserializer).map(|x| Field::Bool(x.0)),
                FieldType::F32 => F32AsIsSerDe::deserialize(deserializer).map(|x| Field::F32(x.0)),
                FieldType::I32 => i32::deserialize(deserializer).map(Field::I32),
                FieldType::I16 => i16::deserialize(deserializer).map(Field::I16),
                FieldType::I64 => i64::deserialize(deserializer).map(Field::I64),
                FieldType::F32List => F32sAsIsDe::deserialize(deserializer).map(|x| Field::F32List(x.0)),
                FieldType::I32List => <Vec<i32>>::deserialize(deserializer).map(Field::I32List),
                FieldType::I16List => <Vec<i16>>::deserialize(deserializer).map(Field::I16List),
                FieldType::U8 => u8::deserialize(deserializer).map(Field::U8),
            }.map(|x| Right((self.field_tag, x)))
        }
    }
}

struct FieldDeserializer {
    record_tag: Tag,
    prev_tag: Tag,
}

impl<'de> de::Visitor<'de> for FieldDeserializer {
    type Value = Either<RecordFlags, (Tag, Field)>;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "record flags or field")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error> where
        A: de::MapAccess<'de> {

        let field_tag: Tag = map.next_key()?
            .ok_or_else(|| A::Error::custom("missed field tag"))?;
        let body = map.next_value_seed(FieldBodyDeserializer {
            record_tag: self.record_tag, prev_tag: self.prev_tag, field_tag
        })?;
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

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "field list")
    }

    fn visit_seq<A>(mut self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: de::SeqAccess<'de> {

        let mut fields = seq.size_hint().map_or_else(Vec::new, Vec::with_capacity);
        let mut prev_tag = META;
        while let Some(field) = seq.next_element_seed(FieldDeserializer { record_tag: self.record_tag, prev_tag })? {
            match field {
                Left(flags) => {
                    if self.record_flags.replace(flags).is_some() {
                        return Err(A::Error::custom("duplicated record flags"));
                    }
                },
                Right(field) => {
                    prev_tag = field.0;
                    fields.push(field);
                }
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

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
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

#[derive(Debug, Clone, Eq, PartialEq)]
enum DialogTypeOption {
    None(i32),
    Some(DialogType)
}

impl From<DialogTypeOption> for Field {
    fn from(v: DialogTypeOption) -> Self {
        match v {
            DialogTypeOption::None(i) => Field::I32(i),
            DialogTypeOption::Some(v) => Field::DialogType(v),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
#[serde(rename="DialogTypeOption")]
enum DialogTypeOptionHRSurrogate {
    None(i32),
    Some(DialogType)
}

impl From<DialogTypeOption> for DialogTypeOptionHRSurrogate {
    fn from(t: DialogTypeOption) -> Self {
        match t {
            DialogTypeOption::None(i) => DialogTypeOptionHRSurrogate::None(i),
            DialogTypeOption::Some(v) => DialogTypeOptionHRSurrogate::Some(v),
        }
    }
}

impl From<DialogTypeOptionHRSurrogate> for DialogTypeOption {
    fn from(t: DialogTypeOptionHRSurrogate) -> Self {
        match t {
            DialogTypeOptionHRSurrogate::None(i) => DialogTypeOption::None(i),
            DialogTypeOptionHRSurrogate::Some(v) => DialogTypeOption::Some(v),
        }
    }
}

const I32_SERDE_SIZE: u32 = 4;
const DIALOG_TYPE_SERDE_SIZE: u32 = 1;

impl Serialize for DialogTypeOption {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            DialogTypeOptionHRSurrogate::from(self.clone()).serialize(serializer)
        } else {
            match self {
                DialogTypeOption::None(padding) => serializer.serialize_newtype_variant(
                    name_of!(type DialogTypeOption),
                    I32_SERDE_SIZE,
                    name_of!(const None in DialogTypeOption),
                    padding
                ),
                DialogTypeOption::Some(c) => serializer.serialize_newtype_variant(
                    name_of!(type DialogTypeOption),
                    DIALOG_TYPE_SERDE_SIZE,
                    name_of!(const Some in DialogTypeOption),
                    c
                ),
            }
        }
    }
}

struct DialogTypeOptionNHRDeserializer;

impl<'de> de::Visitor<'de> for DialogTypeOptionNHRDeserializer {
    type Value = DialogTypeOption;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "32-bit integer or dialog type")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error> where A: de::EnumAccess<'de> {
        let (variant_index, variant) = data.variant::<u32>()?;
        match variant_index {
            I32_SERDE_SIZE => Ok(DialogTypeOption::None(variant.newtype_variant()?)),
            DIALOG_TYPE_SERDE_SIZE => Ok(DialogTypeOption::Some(variant.newtype_variant()?)),
            n => Err(A::Error::invalid_value(Unexpected::Unsigned(n as u64), &self))
        }
    }
}

impl<'de> Deserialize<'de> for DialogTypeOption {
    fn deserialize<D>(deserializer: D) -> Result<DialogTypeOption, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            DialogTypeOptionHRSurrogate::deserialize(deserializer).map(|x| x.into())
        } else {
            deserializer.deserialize_enum(
                name_of!(type DialogTypeOption),
                &[name_of!(const None in DialogTypeOption), name_of!(const Some in DialogTypeOption)],
                DialogTypeOptionNHRDeserializer
            )
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum PosRotOrCell {
    PosRot(PosRot),
    Cell(Cell)
}

impl From<PosRotOrCell> for Field {
    fn from(v: PosRotOrCell) -> Self {
        match v {
            PosRotOrCell::PosRot(p) => Field::PosRot(p),
            PosRotOrCell::Cell(c) => Field::Cell(c),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
#[serde(rename="PosRotOrCell")]
enum PosRotOrCellHRSurrogate {
    PosRot(PosRot),
    Cell(Cell)
}

impl From<PosRotOrCell> for PosRotOrCellHRSurrogate {
    fn from(t: PosRotOrCell) -> Self {
        match t {
            PosRotOrCell::PosRot(p) => PosRotOrCellHRSurrogate::PosRot(p),
            PosRotOrCell::Cell(c) => PosRotOrCellHRSurrogate::Cell(c),
        }
    }
}

impl From<PosRotOrCellHRSurrogate> for PosRotOrCell {
    fn from(t: PosRotOrCellHRSurrogate) -> Self {
        match t {
            PosRotOrCellHRSurrogate::PosRot(p) => PosRotOrCell::PosRot(p),
            PosRotOrCellHRSurrogate::Cell(c) => PosRotOrCell::Cell(c),
        }
    }
}

const POS_ROT_SERDE_SIZE: u32 = 24;
const CELL_SERDE_SIZE: u32 = 12;

impl Serialize for PosRotOrCell {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            PosRotOrCellHRSurrogate::from(self.clone()).serialize(serializer)
        } else {
            match self {
                PosRotOrCell::PosRot(pos_rot) => serializer.serialize_newtype_variant(
                    name_of!(type PosRotOrCell),
                    POS_ROT_SERDE_SIZE,
                    name_of!(const PosRot in PosRotOrCell),
                    pos_rot
                ),
                PosRotOrCell::Cell(cell) => serializer.serialize_newtype_variant(
                    name_of!(type PosRotOrCell),
                    CELL_SERDE_SIZE,
                    name_of!(const Cell in PosRotOrCell),
                    cell
                ),
            }
        }
    }
}

struct PosRotOrCellNHRDeserializer;

impl<'de> de::Visitor<'de> for PosRotOrCellNHRDeserializer {
    type Value = PosRotOrCell;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "pos_rot or cell")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error> where A: de::EnumAccess<'de> {
        let (variant_index, variant) = data.variant::<u32>()?;
        match variant_index {
            POS_ROT_SERDE_SIZE => Ok(PosRotOrCell::PosRot(variant.newtype_variant()?)),
            CELL_SERDE_SIZE => Ok(PosRotOrCell::Cell(variant.newtype_variant()?)),
            n => Err(A::Error::invalid_value(Unexpected::Unsigned(n as u64), &self))
        }
    }
}

impl<'de> Deserialize<'de> for PosRotOrCell {
    fn deserialize<D>(deserializer: D) -> Result<PosRotOrCell, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            PosRotOrCellHRSurrogate::deserialize(deserializer).map(|x| x.into())
        } else {
            deserializer.deserialize_enum(
                name_of!(type PosRotOrCell),
                &[name_of!(const PosRot in PosRotOrCell), name_of!(const Cell in PosRotOrCell)],
                PosRotOrCellNHRDeserializer
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use std::str::FromStr;
    use std::hash::Hash;
    use std::collections::hash_map::DefaultHasher;

    #[allow(clippy::deref_addrof)]
    #[test]
    fn record_flags_traits() {
        assert_eq!(RecordFlags::PERSIST, *&RecordFlags::PERSIST);
        assert!(RecordFlags::PERSIST < RecordFlags::BLOCKED);
        let mut hasher = DefaultHasher::new();
        RecordFlags::DELETED.hash(&mut hasher);
    }

    #[test]
    fn test_record_flags() {
        assert_eq!("PERSIST", format!("{}", RecordFlags::PERSIST));
        assert_eq!("PERSIST", format!("{:?}", RecordFlags::PERSIST));
        assert_eq!("PERSIST DELETED", format!("{}", RecordFlags::PERSIST | RecordFlags::DELETED));
        assert_eq!(0x202000000000, (RecordFlags::BLOCKED | RecordFlags::DELETED).bits);
        assert_eq!(Some(RecordFlags::BLOCKED | RecordFlags::DELETED), RecordFlags::from_bits(0x202000000000));
        assert_eq!(Ok(RecordFlags::DELETED), RecordFlags::from_str("DELETED"));
        assert_eq!(Ok(RecordFlags::DELETED | RecordFlags::PERSIST), RecordFlags::from_str("DELETED PERSIST"));
        assert_eq!(Ok(RecordFlags::DELETED | RecordFlags::PERSIST), RecordFlags::from_str("PERSIST  DELETED"));
        assert_eq!(Ok(RecordFlags::empty()), RecordFlags::from_str(""));
        assert_eq!(Ok(RecordFlags::empty()), RecordFlags::from_str(" "));
    }

    #[test]
    fn record_yaml() {
        let record = Record {
            tag: SCPT,
            flags: RecordFlags::PERSIST,
            fields: vec![
                (SCHD, Field::ScriptMetadata(ScriptMetadata {
                    name: "Scr1".into(),
                    vars: ScriptVars { shorts: 1, longs: 2, floats: 3 },
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
            assert_eq!(res.vars.shorts, 1);
            assert_eq!(res.vars.longs, 2);
            assert_eq!(res.vars.floats, 3);
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
