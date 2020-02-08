use either::{Left, Right};
use serde::{Serialize, Serializer};
use serde::ser::SerializeTuple;
use serde::ser::Error as ser_Error;

use crate::strings_serde::*;
use crate::field::*;

pub trait SerializerFieldExt: Serializer {
    fn serialize_bytes_ext(self, bytes: &[u8]) -> Result<Self::Ok, Self::Error> {
        if self.is_human_readable() {
            self.serialize_bytes(bytes)
        } else {
            let mut serializer = self.serialize_tuple(bytes.len())?;
            for byte in bytes {
                serializer.serialize_element(byte)?;
            }
            serializer.end()
        }
    }
    
    fn serialize_field(self, record_tag: Tag, field_tag: Tag, field: &Field)
        -> Result<Self::Ok, Self::Error> {

        match FieldType::from_tags(record_tag, field_tag) {
            FieldType::String(p) => if let Field::String(s) = field {
                CODE_PAGE.with(|x| self.serialize_string(x.get(), p.either(|_| None, |n| Some(n as usize)), s))
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have string type", record_tag, field_tag)))
            },
            FieldType::StringZ => if let Field::StringZ(s) = field {
                CODE_PAGE.with(|x| self.serialize_string_z(x.get(), s))
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have zero-terminated string type", record_tag, field_tag)))
            },
            FieldType::Multiline(_, lb) => if let Field::StringList(s) = field {
                CODE_PAGE.with(|x| self.serialize_string_list(x.get(), lb.new_line(), None, s))
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have string list type", record_tag, field_tag)))
            },
            FieldType::StringZList => if let Field::StringZList(s) = field {
                CODE_PAGE.with(|x| self.serialize_string_z_list(x.get(), s))
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have zero-terminated string list type", record_tag, field_tag)))
            },
            FieldType::Binary | FieldType::Compressed => if let Field::Binary(v) = field {
                self.serialize_bytes_ext(v)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have binary type", record_tag, field_tag)))
            },
            FieldType::Item => if let Field::Item(v) = field {
                v.serialize(self)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have item type", record_tag, field_tag)))
            },
            FieldType::Ingredient => if let Field::Ingredient(v) = field {
                v.serialize(self)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have ingredient type", record_tag, field_tag)))
            },
            FieldType::ScriptMetadata => if let Field::ScriptMetadata(v) = field {
                v.serialize(self)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have script metadata type", record_tag, field_tag)))
            },
            FieldType::FileMetadata => if let Field::FileMetadata(v) = field {
                v.serialize(self)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have file metadata type", record_tag, field_tag)))
            },
            FieldType::SavedNpc => if let Field::SavedNpc(v) = field {
                v.serialize(self)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have saved npc type", record_tag, field_tag)))
            },
            FieldType::Effect => if let Field::Effect(v) = field {
                v.serialize(self)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have effect type", record_tag, field_tag)))
            },
            FieldType::Npc => if let Field::Npc(v) = field {
                if self.is_human_readable() {
                    v.serialize(self)
                } else {
                    match v.variant() {
                        Left(v) => v.serialize(self),
                        Right(v) => v.serialize(self)
                    }
                }
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have NPC type", record_tag, field_tag)))
            },
            FieldType::DialogMetadata => if let &Field::DialogMetadata(v) = field {
                match v {
                    Left(v) => self.serialize_u32(v),
                    Right(v) => v.serialize(self)
                }
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have dialog metadata type", record_tag, field_tag)))
            },
            FieldType::Float => if let &Field::Float(v) = field {
                self.serialize_f32(v)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have float type", record_tag, field_tag)))
            },
            FieldType::Int => if let &Field::Int(v) = field {
                self.serialize_i32(v)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have int type", record_tag, field_tag)))
            },
            FieldType::Short => if let &Field::Short(v) = field {
                self.serialize_i16(v)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have short type", record_tag, field_tag)))
            },
            FieldType::Long => if let &Field::Long(v) = field {
                self.serialize_i64(v)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have long type", record_tag, field_tag)))
            },
            FieldType::Byte => if let &Field::Byte(v) = field {
                self.serialize_u8(v)
            } else {
                Err(Self::Error::custom(&format!("{} {} field should have byte type", record_tag, field_tag)))
            },
        }
    }
}

impl<S: Serializer> SerializerFieldExt for S { }
