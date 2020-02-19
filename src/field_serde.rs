use either::{Right};
use serde::{Serialize, Deserializer, Deserialize};
use serde::de::Error as de_Error;
use serde::de::{self};
use std::fmt::{self};

use crate::strings_serde::*;
use crate::field::*;

struct BytesHRDeserializer;

impl<'de> de::Visitor<'de> for BytesHRDeserializer {
    type Value = Vec<u8>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "byte array")
    }

    fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E> where E: de::Error {
        Ok(v)
    }
}

struct BytesNHRDeserializer {
    len: usize
}

impl<'de> de::Visitor<'de> for BytesNHRDeserializer {
    type Value = Vec<u8>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "array of {} bytes", self.len)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        if let Some(n) = seq.size_hint() {
            if n != self.len {
                return Err(A::Error::invalid_length(n, &self));
            }
        }
        let mut v = Vec::with_capacity(self.len);
        while let Some(b) = seq.next_element()? {
            v.push(b);
        }
        if v.len() != self.len {
            Err(A::Error::invalid_length(v.len(), &self))
        } else {
            Ok(v)
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum DialogTypeSurrogate {
    Right(DialogType),
    Left(u32),
}

pub trait DeserializerFieldExt<'de>: Deserializer<'de> {
    fn deserialize_hr_bytes(self) -> Result<Vec<u8>, Self::Error> {
        self.deserialize_byte_buf(BytesHRDeserializer)
    }

    fn deserialize_bytes_ext(self, len: usize) -> Result<Vec<u8>, Self::Error> {
        if self.is_human_readable() {
            self.deserialize_hr_bytes()
        } else {
            self.deserialize_tuple(len, BytesNHRDeserializer { len })
        }
    }

    fn deserialize_field(self, record_tag: Tag, field_tag: Tag, field_size: Option<usize>) -> Result<Field, Self::Error> {
        assert_eq!(field_size.is_none(), self.is_human_readable());
        match FieldType::from_tags(record_tag, field_tag) {
            FieldType::String(p) => if let Some(field_size) = field_size {
                if let Right(n) = p {
                    if n as usize != field_size {
                        let expected: &str = &format!("array of {} bytes", n);
                        return Err(Self::Error::invalid_length(field_size, &expected));
                    }
                }
                CODE_PAGE.with(|x| self.deserialize_string_ext(x.get(), field_size, p.is_right()))
            } else {
                self.deserialize_hr_string()
            }.map(Field::String),
            FieldType::StringZ => if let Some(field_size) = field_size {
                CODE_PAGE.with(|x| self.deserialize_string_z(x.get(), field_size))
            } else {
                self.deserialize_hr_string_z()
            }.map(Field::StringZ),
            FieldType::Multiline(_, lb) => if let Some(field_size) = field_size {
                CODE_PAGE.with(|x| self.deserialize_string_list(x.get(), lb.new_line(), field_size, false))
            } else {
                self.deserialize_hr_string_list()
            }.map(Field::StringList),
            FieldType::StringZList => if let Some(field_size) = field_size {
                CODE_PAGE.with(|x| self.deserialize_string_z_list(x.get(), field_size))
            } else {
                self.deserialize_hr_string_z_list()
            }.map(Field::StringZList),
            FieldType::Binary | FieldType::Compressed => if let Some(field_size) = field_size {
                self.deserialize_bytes_ext(field_size)
            } else {
                self.deserialize_hr_bytes()
            }.map(Field::Binary),
            FieldType::Item => Item::deserialize(self).map(Field::Item),
            FieldType::Ingredient => Ingredient::deserialize(self).map(Field::Ingredient),
            FieldType::ScriptMetadata => ScriptMetadata::deserialize(self).map(Field::ScriptMetadata),
            FieldType::FileMetadata => FileMetadata::deserialize(self).map(Field::FileMetadata),
            FieldType::SavedNpc => SavedNpc::deserialize(self).map(Field::SavedNpc),
            FieldType::Effect => Effect::deserialize(self).map(Field::Effect),
            FieldType::Npc => if let Some(field_size) = field_size {
                match field_size {
                    12 => Npc12::deserialize(self).map(|x| x.into()),
                    52 => Npc52::deserialize(self).map(|x| x.into()),
                    _ => return Err(Self::Error::invalid_length(field_size, &"NPC (12 or 52 bytes)"))
                }
            } else {
                Npc::deserialize(self)
            }.map(Field::Npc),
            FieldType::DialogMetadata => if let Some(field_size) = field_size {
                match field_size {
                    1 => DialogType::deserialize(self).map(DialogTypeOption::Some),
                    4 => u32::deserialize(self).map(DialogTypeOption::None),
                    _ => return Err(Self::Error::invalid_length(field_size, &"dialog metadata or dword"))
                }
            } else {
                match DialogTypeSurrogate::deserialize(self)? {
                    DialogTypeSurrogate::Left(x) => Ok(DialogTypeOption::None(x)),
                    DialogTypeSurrogate::Right(x) => Ok(DialogTypeOption::Some(x))
                }
            }.map(Field::DialogMetadata),
            FieldType::Float => f32::deserialize(self).map(Field::Float),
            FieldType::Int => i32::deserialize(self).map(Field::Int),
            FieldType::Short => i16::deserialize(self).map(Field::Short),
            FieldType::Long => i64::deserialize(self).map(Field::Long),
            FieldType::Byte => u8::deserialize(self).map(Field::Byte),
        }
    }
}

impl<'de, D: Deserializer<'de>> DeserializerFieldExt<'de> for D { }
