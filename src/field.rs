use either::{Either, Left, Right};
use std::fmt::{self, Debug};
use std::mem::size_of;
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::de::{self, Unexpected, VariantAccess};
use serde::de::Error as de_Error;
use std::marker::PhantomData;

use crate::strings::*;

pub use crate::tag::*;

include!(concat!(env!("OUT_DIR"), "/tags.rs"));

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(u32)]
    pub enum FileType {
        ESP = 0,
        ESM = 1,
        ESS = 32
    }
}

enum_serde!([
    FileType, FileTypeDeserializer, "file type",
    u32, from_u32, to_u32, visit_u32, serialize_u32, deserialize_u32,
    Unsigned, u64
]);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(u8)]
    pub enum DialogType {
        Topic = 0,
        Voice = 1,
        Greeting = 2,
        Persuasion = 3,
        Journal = 4
    }
}

enum_serde!([
    DialogType, DialogTypeDeserializer, "dialog type",
    u8, from_u8, to_u8, visit_u8, serialize_u8, deserialize_u8,
    Unsigned, u64
]);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(i32)]
    pub enum EffectRange {
        Oneself = 0,
        Touch = 1,
        Target = 2,
    }
}

enum_serde!([
    EffectRange, EffectRangeDeserializer, "effect range",
    i32, from_i32, to_i32, visit_i32, serialize_i32, deserialize_i32,
    Signed, i64
]);

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum StringCoerce {
    None,
    TrimTailZeros
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum LinebreakStyle {
    Unix,
    Dos
}

impl LinebreakStyle {
    pub fn new_line(self) -> &'static str {
        if self == LinebreakStyle::Unix { "\n" } else { "\r\n" }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum FieldType {
    Binary,
    String(Either<StringCoerce, u32>),
    StringZ,
    Multiline(StringCoerce, LinebreakStyle),
    StringZList,
    Item,
    Float,
    Int,
    Short,
    Long,
    Byte,
    Compressed,
    Ingredient,
    ScriptMetadata,
    DialogMetadata,
    FileMetadata,
    Npc,
    SavedNpc,
    Effect,
}

impl FieldType {
    pub fn from_tags(record_tag: Tag, field_tag: Tag) -> FieldType {
        match (record_tag, field_tag) {
            (INFO, ACDT) => FieldType::String(Left(StringCoerce::None)),
            (CELL, ACTN) => FieldType::Int,
            (NPC_, ANAM) => FieldType::StringZ,
            (_, ANAM) => FieldType::String(Left(StringCoerce::None)),
            (_, ASND) => FieldType::String(Left(StringCoerce::None)),
            (_, AVFX) => FieldType::String(Left(StringCoerce::None)),
            (ARMO, BNAM) => FieldType::String(Left(StringCoerce::TrimTailZeros)),
            (BODY, BNAM) => FieldType::String(Left(StringCoerce::TrimTailZeros)),
            (CELL, BNAM) => FieldType::StringZ,
            (CLOT, BNAM) => FieldType::String(Left(StringCoerce::TrimTailZeros)),
            (CONT, BNAM) => FieldType::Multiline(StringCoerce::TrimTailZeros, LinebreakStyle::Dos),
            (INFO, BNAM) => FieldType::Multiline(StringCoerce::TrimTailZeros, LinebreakStyle::Dos),
            (NPC_, BNAM) => FieldType::StringZ,
            (PCDT, BNAM) => FieldType::String(Left(StringCoerce::None)),
            (REGN, BNAM) => FieldType::String(Left(StringCoerce::None)),
            (_, BNAM) => FieldType::Multiline(StringCoerce::None, LinebreakStyle::Dos),
            (_, BSND) => FieldType::String(Left(StringCoerce::None)),
            (_, BVFX) => FieldType::String(Left(StringCoerce::None)),
            (ARMO, CNAM) => FieldType::String(Left(StringCoerce::TrimTailZeros)),
            (KLST, CNAM) => FieldType::Int,
            (NPC_, CNAM) => FieldType::StringZ,
            (REGN, CNAM) => FieldType::Int,
            (_, CNAM) => FieldType::String(Left(StringCoerce::None)),
            (_, CSND) => FieldType::String(Left(StringCoerce::None)),
            (_, CVFX) => FieldType::String(Left(StringCoerce::None)),
            (DIAL, DATA) => FieldType::DialogMetadata,
            (LAND, DATA) => FieldType::Int,
            (LEVC, DATA) => FieldType::Int,
            (LEVI, DATA) => FieldType::Int,
            (LTEX, DATA) => FieldType::String(Left(StringCoerce::None)),
            (SSCR, DATA) => FieldType::String(Left(StringCoerce::TrimTailZeros)),
            (TES3, DATA) => FieldType::Long,
            (QUES, DATA) => FieldType::String(Left(StringCoerce::None)),
            (DIAL, DELE) => FieldType::Int,
            (_, DESC) => FieldType::String(Left(StringCoerce::None)),
            (_, DNAM) => FieldType::String(Left(StringCoerce::None)),
            (ALCH, ENAM) => FieldType::Effect,
            (ARMO, ENAM) => FieldType::String(Left(StringCoerce::None)),
            (ENCH, ENAM) => FieldType::Effect,
            (PCDT, ENAM) => FieldType::Long,
            (SPEL, ENAM) => FieldType::Effect,
            (CELL, FGTN) => FieldType::String(Left(StringCoerce::None)),
            (_, FLAG) => FieldType::Int,
            (_, FLTV) => FieldType::Float,
            (ACTI, FNAM) => FieldType::StringZ,
            (PCDT, FNAM) => FieldType::Binary,
            (RACE, FNAM) => FieldType::StringZ,
            (_, FNAM) => FieldType::String(Left(StringCoerce::None)),
            (CELL, FRMR) => FieldType::Int,
            (TES3, HEDR) => FieldType::FileMetadata,
            (_, HSND) => FieldType::String(Left(StringCoerce::None)),
            (_, HVFX) => FieldType::String(Left(StringCoerce::None)),
            (_, INAM) => FieldType::String(Left(StringCoerce::None)),
            (ARMO, INDX) => FieldType::Byte,
            (CLOT, INDX) => FieldType::Byte,
            (_, INDX) => FieldType::Int,
            (LAND, INTV) => FieldType::Long,
            (LEVC, INTV) => FieldType::Short,
            (LEVI, INTV) => FieldType::Short,
            (_, INTV) => FieldType::Int,
            (INGR, IRDT) => FieldType::Ingredient,
            (_, ITEX) => FieldType::String(Left(StringCoerce::None)),
            (NPC_, KNAM) => FieldType::StringZ,
            (PCDT, KNAM) => FieldType::Binary,
            (_, KNAM) => FieldType::String(Left(StringCoerce::None)),
            (PCDT, LNAM) => FieldType::Long,
            (CELL, LSHN) => FieldType::String(Left(StringCoerce::None)),
            (CELL, LSTN) => FieldType::String(Left(StringCoerce::None)),
            (_, LVCR) => FieldType::Byte,
            (FMAP, MAPD) => FieldType::Compressed,
            (FMAP, MAPH) => FieldType::Long,
            (TES3, MAST) => FieldType::String(Left(StringCoerce::None)),
            (PCDT, MNAM) => FieldType::String(Left(StringCoerce::None)),
            (CELL, MNAM) => FieldType::Byte,
            (LIGH, MODL) => FieldType::StringZ,
            (_, MODL) => FieldType::String(Left(StringCoerce::None)),
            (CELL, NAM0) => FieldType::Int,
            (SPLM, NAM0) => FieldType::Byte,
            (CELL, NAM5) => FieldType::Int,
            (CELL, NAM9) => FieldType::Int,
            (PCDT, NAM9) => FieldType::Int,
            (CELL, NAME) => FieldType::StringZ,
            (JOUR, NAME) => FieldType::Multiline(StringCoerce::None, LinebreakStyle::Unix), // TODO None need check
            (SPLM, NAME) => FieldType::Int,
            (SSCR, NAME) => FieldType::String(Left(StringCoerce::TrimTailZeros)),
            (_, NAME) => FieldType::String(Left(StringCoerce::None)),
            (_, ND3D) => FieldType::Byte,
            (INFO, NNAM) => FieldType::StringZ,
            (LEVC, NNAM) => FieldType::Byte,
            (LEVI, NNAM) => FieldType::Byte,
            (_, NNAM) => FieldType::String(Left(StringCoerce::None)),
            (_, NPCO) => FieldType::Item,
            (NPC_, NPDT) => FieldType::Npc,
            (NPCC, NPDT) => FieldType::SavedNpc,
            (BSGN, NPCS) => FieldType::String(Right(32)),
            (NPC_, NPCS) => FieldType::String(Right(32)),
            (RACE, NPCS) => FieldType::String(Right(32)),
            (_, NPCS) => FieldType::String(Left(StringCoerce::None)),
            (_, ONAM) => FieldType::String(Left(StringCoerce::None)),
            (INFO, PNAM) => FieldType::StringZ,
            (PCDT, PNAM) => FieldType::Binary,
            (_, PNAM) => FieldType::String(Left(StringCoerce::None)),
            (_, PTEX) => FieldType::String(Left(StringCoerce::None)),
            (_, RGNN) => FieldType::String(Left(StringCoerce::None)),
            (FACT, RNAM) => FieldType::String(Right(32)),
            (_, RNAM) => FieldType::String(Left(StringCoerce::None)),
            (SCPT, SCHD) => FieldType::ScriptMetadata,
            (_, SCRI) => FieldType::String(Left(StringCoerce::None)),
            (_, SCTX) => FieldType::Multiline(StringCoerce::TrimTailZeros, LinebreakStyle::Dos),
            (SCPT, SCVR) => FieldType::StringZList,
            (_, SCVR) => FieldType::String(Left(StringCoerce::None)),
            (CELL, SLSD) => FieldType::Binary,
            (PCDT, SNAM) => FieldType::Binary,
            (REGN, SNAM) => FieldType::Binary,
            (_, SNAM) => FieldType::String(Left(StringCoerce::None)),
            (_, STRV) => FieldType::String(Left(StringCoerce::None)),
            (ALCH, TEXT) => FieldType::String(Left(StringCoerce::None)),
            (BOOK, TEXT) => FieldType::Multiline(StringCoerce::TrimTailZeros, LinebreakStyle::Dos),
            (_, TEXT) => FieldType::Multiline(StringCoerce::None, LinebreakStyle::Dos),
            (_, TNAM) => FieldType::String(Left(StringCoerce::None)),
            (_, VCLR) => FieldType::Compressed,
            (_, VHGT) => FieldType::Compressed,
            (_, VNML) => FieldType::Compressed,
            (_, VTEX) => FieldType::Compressed,
            (_, WEAT) => FieldType::Binary,
            (CELL, WHGT) => FieldType::Int,
            (_, WIDX) => FieldType::Long,
            (_, WNAM) => FieldType::Compressed,
            (_, XCHG) => FieldType::Int,
            (_, XHLT) => FieldType::Int,
            (_, XIDX) => FieldType::Int,
            (_, XSOL) => FieldType::String(Left(StringCoerce::None)),
            (SPLM, XNAM) => FieldType::Byte,
            (CELL, XSCL) => FieldType::Int,
            (CELL, ZNAM) => FieldType::Byte,
            _ => FieldType::Binary
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ingredient {
    pub weight: f32,
    pub value: u32,
    pub effects: [i32; 4],
    pub skills: [i32; 4],
    pub attributes: [i32; 4]
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScriptMetadata {
    #[serde(with = "string_32")]
    pub name: String,
    pub shorts: u32,
    pub longs: u32,
    pub floats: u32,
    pub data_size: u32,
    pub var_table_size: u32
}

mod string_32 {
    use std::fmt::{self};
    use serde::{Serializer, Deserializer, Deserialize};
    use serde::de::{self};
    use serde::ser::Error as ser_Error;
    use serde::ser::SerializeTuple;
    use serde::de::Error as de_Error;

    pub fn serialize<S>(s: &str, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            serializer.serialize_str(s)
        } else {
            if s.len() > 32 {
                return Err(S::Error::custom("string length is above 32 chars"));
            }
            let mut serializer = serializer.serialize_tuple(32)?;
            for c in s.chars() {
                serializer.serialize_element(&c)?;
            }
            for _ in s.len() .. 32 {
                serializer.serialize_element(&'\0')?;
            }
            serializer.end()
        }
    }

    struct StringNHRDeserializer;

    impl<'de> de::Visitor<'de> for StringNHRDeserializer {
        type Value = String;

        fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "32 character string")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
            if let Some(n) = seq.size_hint() {
                if n != 32 {
                    return Err(A::Error::invalid_length(n, &self));
                }
            }
            let mut string: String = String::with_capacity(32);
            while let Some(c) = seq.next_element()? {
                string.push(c);
            }
            if string.len() != 32 {
                Err(A::Error::invalid_length(string.len(), &self))
            } else {
                let cut_to = string.rfind(|c| c != '\0').map_or(0, |n| 1 + n);
                string.truncate(cut_to);
                Ok(string)
            }
        }
    }
    
    pub fn deserialize<'de, D>(deserializer: D) -> Result<String, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            String::deserialize(deserializer)
        } else {
            deserializer.deserialize_tuple(32, StringNHRDeserializer)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileMetadata {
    pub version: u32,
    pub file_type: FileType,
    #[serde(with = "string_32")]
    pub author: String,
    #[serde(with = "multiline_256_dos")]
    pub description: Vec<String>,
    pub records_count: u32
}

mod multiline_256_dos {
    use std::fmt::{self};
    use serde::{Serializer, Deserializer, Serialize, Deserialize};
    use serde::de::{self};
    use serde::ser::Error as ser_Error;
    use serde::ser::SerializeTuple;
    use serde::de::Error as de_Error;
    use crate::field::*;

    pub fn serialize<S>(lines: &[String], serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            lines.serialize(serializer)
        } else {
            let new_line = LinebreakStyle::Dos.new_line();
            let mut capacity = 0;
            for line in lines {
                if line.contains(new_line) {
                    return Err(S::Error::custom("string list item contains separator"));
                }
                capacity += line.len() + new_line.len();
            }
            let mut text = String::with_capacity(capacity);
            for line in lines.iter() {
                text.push_str(line);
                text.push_str(new_line);
            }
            text.truncate(text.len() - new_line.len());
            if text.len() > 256 {
                return Err(S::Error::custom("string list total length is above 256 bytes"));
            }
            let mut serializer = serializer.serialize_tuple(256)?;
            for c in text.chars() {
                serializer.serialize_element(&c)?;
            }
            for _ in text.len() .. 256 {
                serializer.serialize_element(&'\0')?;
            }
            serializer.end()
        }
    }

    struct StringListNHRDeserializer;

    impl<'de> de::Visitor<'de> for StringListNHRDeserializer {
        type Value = Vec<String>;

        fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "256 character string")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
            if let Some(n) = seq.size_hint() {
                if n != 256 {
                    return Err(A::Error::invalid_length(n, &self));
                }
            }
            let mut string: String = String::with_capacity(256);
            while let Some(c) = seq.next_element()? {
                string.push(c);
            }
            if string.len() != 256 {
                Err(A::Error::invalid_length(string.len(), &self))
            } else {
                let cut_to = string.rfind(|c| c != '\0').map_or(0, |n| 1 + n);
                string.truncate(cut_to);
                Ok(string.split(LinebreakStyle::Dos.new_line()).map(|x| x.into()).collect())
            }
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            <Vec<String>>::deserialize(deserializer)
        } else {
            deserializer.deserialize_tuple(256, StringListNHRDeserializer)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Effect {
    pub id: i16,
    pub skill: i8,
    pub attribute: i8,
    pub range: EffectRange,
    pub area: i32,
    pub duration: i32,
    pub magnitude_min: i32,
    pub magnitude_max: i32
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SavedNpc {
    pub disposition: i16,
    pub reputation: i16,
    pub index: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NpcCharacteristics {
    pub strength: u8,
    pub intelligence: u8,
    pub willpower: u8,
    pub agility: u8,
    pub speed: u8,
    pub endurance: u8,
    pub personality: u8,
    pub luck: u8,
    pub block: u8,
    pub armorer: u8,
    pub medium_armor: u8,
    pub heavy_armor: u8,
    pub blunt_weapon: u8,
    pub long_blade: u8,
    pub axe: u8,
    pub spear: u8,
    pub athletics: u8,
    pub enchant: u8,
    pub destruction: u8,
    pub alteration: u8,
    pub illusion: u8,
    pub conjuration: u8,
    pub mysticism: u8,
    pub restoration: u8,
    pub alchemy: u8,
    pub unarmored: u8,
    pub security: u8,
    pub sneak: u8,
    pub acrobatics: u8,
    pub light_armor: u8,
    pub short_blade: u8,
    pub marksman: u8,
    pub mercantile: u8,
    pub speechcraft: u8,
    pub hand_to_hand: u8,
    pub faction: u8,
    pub health: i16,
    pub magicka: i16,
    pub fatigue: i16,
}

#[derive(Debug, Clone)]
pub enum NpcCharacteristicsOption {
    None(u16),
    Some(NpcCharacteristics)
}

impl Serialize for NpcCharacteristicsOption {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            match self {
                &NpcCharacteristicsOption::None(padding) => serializer.serialize_u16(padding),
                NpcCharacteristicsOption::Some(c) => c.serialize(serializer)
            }
        } else {
            match self {
                NpcCharacteristicsOption::None(padding) =>
                    serializer.serialize_newtype_variant("NpcCharacteristicsOption", size_of::<u16>() as u32, "None", padding),
                NpcCharacteristicsOption::Some(c) =>
                    serializer.serialize_newtype_variant("NpcCharacteristicsOption", size_of::<NpcCharacteristics>() as u32, "Some", c),
            }
        }
    }
}

struct StructSeqProxyDeserializer<'de, A: de::SeqAccess<'de>> {
    seq: A,
    phantom: PhantomData<&'de ()>
}

impl<'de, A: de::SeqAccess<'de>> Deserializer<'de> for StructSeqProxyDeserializer<'de, A> {
    type Error = A::Error;

    fn deserialize_struct<V>(self, _: &'static str, _: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> {
        visitor.visit_seq(self.seq)
    }

    fn deserialize_seq<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_bool<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i8<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i16<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_f32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_f64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u8<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u16<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    serde_if_integer128! {
        fn deserialize_i128<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

        fn deserialize_u128<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }
    }

    fn deserialize_char<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_str<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_string<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_bytes<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_byte_buf<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_option<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_unit<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_unit_struct<V>(self, _: &'static str, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_newtype_struct<V>(self, _: &'static str, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_map<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_tuple<V>(self, _: usize, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_tuple_struct<V>(self, _: &'static str, _: usize, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_enum<V>(self, _: &'static str, _: &'static [&'static str], _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_identifier<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_ignored_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }
}

struct StructMapProxyDeserializer<'de, A: de::MapAccess<'de>> {
    map: A,
    phantom: PhantomData<&'de ()>
}

impl<'de, A: de::MapAccess<'de>> Deserializer<'de> for StructMapProxyDeserializer<'de, A> {
    type Error = A::Error;

    fn deserialize_struct<V>(self, _: &'static str, _: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> {
        visitor.visit_map(self.map)
    }

    fn deserialize_seq<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_bool<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i8<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i16<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_f32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_f64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u8<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u16<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    serde_if_integer128! {
        fn deserialize_i128<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

        fn deserialize_u128<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }
    }

    fn deserialize_char<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_str<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_string<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_bytes<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_byte_buf<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_option<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_unit<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_unit_struct<V>(self, _: &'static str, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_newtype_struct<V>(self, _: &'static str, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_map<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_tuple<V>(self, _: usize, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_tuple_struct<V>(self, _: &'static str, _: usize, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_enum<V>(self, _: &'static str, _: &'static [&'static str], _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_identifier<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_ignored_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }
}

struct NpcCharacteristicsOptionHRDeserializer;

impl<'de> de::Visitor<'de> for NpcCharacteristicsOptionHRDeserializer {
    type Value = NpcCharacteristicsOption;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "16-bit unsigned integer or NPC characteristics")
    }

    fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E> where E: de::Error {
        Ok(NpcCharacteristicsOption::None(v))
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        let c = NpcCharacteristics::deserialize(StructSeqProxyDeserializer { seq, phantom: PhantomData })?;
        Ok(NpcCharacteristicsOption::Some(c))
    }

    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error> where A: de::MapAccess<'de> {
        let c = NpcCharacteristics::deserialize(StructMapProxyDeserializer { map, phantom: PhantomData })?;
        Ok(NpcCharacteristicsOption::Some(c))
    }
}

struct NpcCharacteristicsOptionNHRDeserializer;

impl<'de> de::Visitor<'de> for NpcCharacteristicsOptionNHRDeserializer {
    type Value = NpcCharacteristicsOption;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "16-bit unsigned integer or NPC characteristics")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error> where A: de::EnumAccess<'de> {
        let (variant_index, variant) = data.variant::<u32>()?;
        match variant_index {
            2 => Ok(NpcCharacteristicsOption::None(variant.newtype_variant()?)),
            42 => Ok(NpcCharacteristicsOption::Some(variant.newtype_variant()?)),
            n => Err(A::Error::invalid_value(Unexpected::Unsigned(n as u64), &self))
        }
    }
}

impl<'de> Deserialize<'de> for NpcCharacteristicsOption {
    fn deserialize<D>(deserializer: D) -> Result<NpcCharacteristicsOption, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            deserializer.deserialize_any(NpcCharacteristicsOptionHRDeserializer)
        } else {
            deserializer.deserialize_enum("NpcCharacteristicsOption", &["None", "Some"], NpcCharacteristicsOptionNHRDeserializer)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Npc12Or52 {
    Npc12(Npc12),
    Npc52(Npc52)
}

impl Serialize for Npc12Or52 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            Npc::from(self.clone()).serialize(serializer)
        } else {
            match self {
                Npc12Or52::Npc12(npc12) =>
                    serializer.serialize_newtype_variant("Npc12Or52", size_of::<Npc12>() as u32, "Npc12", npc12),
                Npc12Or52::Npc52(npc52) =>
                    serializer.serialize_newtype_variant("Npc12Or52", size_of::<Npc52>() as u32, "Npc52", npc52),
            }
        }
    }
}

struct Npc12Or52NHRDeserializer;

impl<'de> de::Visitor<'de> for Npc12Or52NHRDeserializer {
    type Value = Npc12Or52;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NPC")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error> where A: de::EnumAccess<'de> {
        let (variant_index, variant) = data.variant::<u32>()?;
        match variant_index {
            12 => Ok(Npc12Or52::Npc12(variant.newtype_variant()?)),
            52 => Ok(Npc12Or52::Npc52(variant.newtype_variant()?)),
            n => Err(A::Error::invalid_value(Unexpected::Unsigned(n as u64), &self))
        }
    }
}

impl<'de> Deserialize<'de> for Npc12Or52 {
    fn deserialize<D>(deserializer: D) -> Result<Npc12Or52, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            Npc::deserialize(deserializer).map(Npc12Or52::from)
        } else {
            deserializer.deserialize_enum("Npc12Or52", &["Npc12", "Npc52"], Npc12Or52NHRDeserializer)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Npc {
    pub level: u16,
    pub disposition: i8,
    pub reputation: i8,
    pub rank: i8,
    pub gold: i32,
    pub padding: u8,
    pub characteristics: NpcCharacteristicsOption
}

impl From<Npc> for Npc12Or52 {
    fn from(npc: Npc) -> Npc12Or52 {
        match npc.characteristics {
            NpcCharacteristicsOption::Some(characteristics) => Npc12Or52::Npc52(Npc52 {
                level: npc.level, disposition: npc.disposition,
                reputation: npc.reputation, rank: npc.rank,
                padding: npc.padding,
                gold: npc.gold,
                characteristics
            }),
            NpcCharacteristicsOption::None(padding_16) => Npc12Or52::Npc12(Npc12 {
                level: npc.level, disposition: npc.disposition,
                reputation: npc.reputation, rank: npc.rank,
                padding_8: npc.padding, padding_16,
                gold: npc.gold
            })
        }
    }
}

impl From<Npc12> for Npc12Or52 {
    fn from(npc: Npc12) -> Npc12Or52 {
        Npc12Or52::Npc12(npc)
    }
}

impl From<Npc52> for Npc12Or52 {
    fn from(npc: Npc52) -> Npc12Or52 {
        Npc12Or52::Npc52(npc)
    }
}

impl From<Npc12Or52> for Npc {
    fn from(npc: Npc12Or52) -> Npc {
        match npc {
            Npc12Or52::Npc12(npc) => Npc {
                level: npc.level, disposition: npc.disposition, reputation: npc.reputation,
                rank: npc.rank, gold: npc.gold, padding: npc.padding_8,
                characteristics: NpcCharacteristicsOption::None(npc.padding_16)
            },
            Npc12Or52::Npc52(npc) => Npc {
                level: npc.level, disposition: npc.disposition, reputation: npc.reputation,
                rank: npc.rank, gold: npc.gold, padding: npc.padding,
                characteristics: NpcCharacteristicsOption::Some(npc.characteristics)
            }
        }
    }
}

impl From<Npc12> for Npc {
    fn from(npc: Npc12) -> Npc {
        Npc12Or52::from(npc).into()
    }
}

impl From<Npc52> for Npc {
    fn from(npc: Npc52) -> Npc {
        Npc12Or52::from(npc).into()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Npc12 {
    pub level: u16,
    pub disposition: i8,
    pub reputation: i8,
    pub rank: i8,
    pub padding_8: u8,
    pub padding_16: u16,
    pub gold: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Npc52 {
    pub level: u16,
    pub characteristics: NpcCharacteristics,
    pub disposition: i8,
    pub reputation: i8,
    pub rank: i8,
    pub padding: u8,
    pub gold: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Item {
    pub count: i32,
    #[serde(with = "string_32")]
    pub item_id: String,
}

#[derive(Debug, Clone)]
pub enum DialogTypeOption {
    None(u32),
    Some(DialogType)
}

impl Serialize for DialogTypeOption {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            match self {
                &DialogTypeOption::None(padding) => serializer.serialize_u32(padding),
                DialogTypeOption::Some(c) => c.serialize(serializer)
            }
        } else {
            match self {
                DialogTypeOption::None(padding) =>
                    serializer.serialize_newtype_variant("DialogTypeOption", size_of::<u32>() as u32, "None", padding),
                DialogTypeOption::Some(c) =>
                    serializer.serialize_newtype_variant("DialogTypeOption", size_of::<NpcCharacteristics>() as u32, "Some", c),
            }
        }
    }
}

struct DialogTypeOptionHRDeserializer;

impl<'de> de::Visitor<'de> for DialogTypeOptionHRDeserializer {
    type Value = DialogTypeOption;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "32-bit unsigned integer or NPC characteristics")
    }

    fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E> where E: de::Error {
        Ok(DialogTypeOption::None(v))
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        let c = DialogType::deserialize(StructSeqProxyDeserializer { seq, phantom: PhantomData })?;
        Ok(DialogTypeOption::Some(c))
    }

    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error> where A: de::MapAccess<'de> {
        let c = DialogType::deserialize(StructMapProxyDeserializer { map, phantom: PhantomData })?;
        Ok(DialogTypeOption::Some(c))
    }
}

struct DialogTypeOptionNHRDeserializer;

impl<'de> de::Visitor<'de> for DialogTypeOptionNHRDeserializer {
    type Value = DialogTypeOption;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "32-bit unsigned integer or dialog type")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error> where A: de::EnumAccess<'de> {
        let (variant_index, variant) = data.variant::<u32>()?;
        match variant_index {
            4 => Ok(DialogTypeOption::None(variant.newtype_variant()?)),
            1 => Ok(DialogTypeOption::Some(variant.newtype_variant()?)),
            n => Err(A::Error::invalid_value(Unexpected::Unsigned(n as u64), &self))
        }
    }
}

impl<'de> Deserialize<'de> for DialogTypeOption {
    fn deserialize<D>(deserializer: D) -> Result<DialogTypeOption, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            deserializer.deserialize_any(DialogTypeOptionHRDeserializer)
        } else {
            deserializer.deserialize_enum("DialogTypeOption", &["None", "Some"], DialogTypeOptionNHRDeserializer)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Field {
    Binary(Vec<u8>),
    String(String),
    StringZ(StringZ),
    StringList(Vec<String>),
    StringZList(StringZList),
    Item(Item),
    Float(f32),
    Int(i32),
    Short(i16),
    Long(i64),
    Byte(u8),
    Ingredient(Ingredient),
    ScriptMetadata(ScriptMetadata),
    DialogMetadata(DialogTypeOption),
    FileMetadata(FileMetadata),
    SavedNpc(SavedNpc),
    Npc(Npc),
    Effect(Effect),
}

#[cfg(test)]
mod tests {
    use crate::*;
    use num_traits::cast::FromPrimitive;
    use std::str::FromStr;

    #[test]
    fn debug_and_display_tag() {
        assert_eq!("TES3", format!("{}", TES3));
        assert_eq!("TES3", format!("{:?}", TES3));
        assert_eq!(Ok(SCPT), Tag::from_str("SCPT"));
    }

    #[test]
    fn test_file_type() {
        assert_eq!("ESM", format!("{}", FileType::ESM));
        assert_eq!("ESS", format!("{:?}", FileType::ESS));
        assert_eq!(Some(FileType::ESP), FileType::from_u32(0));
        assert_eq!(None, FileType::from_u32(2));
        assert_eq!(32, FileType::ESS as u32);
        assert_eq!(Ok(FileType::ESP), FileType::from_str("ESP"));
    }
}
