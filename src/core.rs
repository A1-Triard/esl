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
use serde::{Serialize, Deserialize, Serializer};
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeMap, SerializeSeq};

use crate::base::*;
use crate::serde::*;

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
    use serde::{Serializer, Deserializer};
    use crate::serde::*;

    pub fn serialize<S>(s: &str, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {
        
        CODE_PAGE.with(|x| serializer.serialize_string(x.get(), Some(32), s))
    }
    
    pub fn deserialize<'de, D>(deserializer: D) -> Result<String, D::Error> where
        D: Deserializer<'de> {

        CODE_PAGE.with(|x| deserializer.deserialize_string_ext(x.get(), 32, true))
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
    use serde::{Serializer, Deserializer};
    use crate::core::*;

    pub fn serialize<S>(lines: &[String], serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {

        CODE_PAGE.with(|x| serializer.serialize_string_list(x.get(), LinebreakStyle::Dos.new_line(), Some(256), lines))
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error> where
        D: Deserializer<'de> {
        
        CODE_PAGE.with(|x| deserializer.deserialize_string_list(x.get(), LinebreakStyle::Dos.new_line(), 256, true))
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Npc {
    pub level: u16,
    pub disposition: i8,
    pub reputation: i8,
    pub rank: i8,
    pub gold: i32,
    pub padding: u8,
    pub characteristics: Either<u16, NpcCharacteristics>
}

impl Npc {
    pub fn variant(&self) -> Either<Npc12, Npc52> {
        match &self.characteristics {
            Right(characteristics) => Right(Npc52 {
                level: self.level, disposition: self.disposition,
                reputation: self.reputation, rank: self.rank,
                padding: self.padding,
                gold: self.gold,
                characteristics: characteristics.clone()
            }),
            &Left(padding_16) => Left(Npc12 {
                level: self.level, disposition: self.disposition,
                reputation: self.reputation, rank: self.rank,
                padding_8: self.padding, padding_16,
                gold: self.gold
            })
        }
    }
}

impl From<Npc12> for Npc {
    fn from(npc: Npc12) -> Npc {
        Npc {
            level: npc.level, disposition: npc.disposition, reputation: npc.reputation,
            rank: npc.rank, gold: npc.gold, padding: npc.padding_8,
            characteristics: Left(npc.padding_16)
        }
    }
}

impl From<Npc52> for Npc {
    fn from(npc: Npc52) -> Npc {
        Npc {
            level: npc.level, disposition: npc.disposition, reputation: npc.reputation,
            rank: npc.rank, gold: npc.gold, padding: npc.padding,
            characteristics: Right(npc.characteristics)
        }
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
    DialogMetadata(Either<u32, DialogType>),
    FileMetadata(FileMetadata),
    SavedNpc(SavedNpc),
    Npc(Npc),
    Effect(Effect),
}

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

        match FieldType::from_tags(self.0, self.1) {
            FieldType::String(p) => if let Field::String(s) = self.2 {
                CODE_PAGE.with(|x| serializer.serialize_string(x.get(), p.either(|_| None, |n| Some(n as usize)), s))
            } else {
                Err(S::Error::custom(&format!("{} {} field should have string type", self.0, self.1)))
            },
            FieldType::StringZ => if let Field::StringZ(s) = self.2 {
                CODE_PAGE.with(|x| serializer.serialize_string_z(x.get(), s))
            } else {
                Err(S::Error::custom(&format!("{} {} field should have zero-terminated string type", self.0, self.1)))
            },
            FieldType::Multiline(_, lb) => if let Field::StringList(s) = self.2 {
                CODE_PAGE.with(|x| serializer.serialize_string_list(x.get(), lb.new_line(), None, s))
            } else {
                Err(S::Error::custom(&format!("{} {} field should have string list type", self.0, self.1)))
            },
            FieldType::StringZList => if let Field::StringZList(s) = self.2 {
                CODE_PAGE.with(|x| serializer.serialize_string_z_list(x.get(), s))
            } else {
                Err(S::Error::custom(&format!("{} {} field should have zero-terminated string list type", self.0, self.1)))
            },
            FieldType::Binary | FieldType::Compressed => if let Field::Binary(v) = self.2 {
                serializer.serialize_bytes(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have binary type", self.0, self.1)))
            },
            FieldType::Item => if let Field::Item(v) = self.2 {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have item type", self.0, self.1)))
            },
            FieldType::Ingredient => if let Field::Ingredient(v) = self.2 {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have ingredient type", self.0, self.1)))
            },
            FieldType::ScriptMetadata => if let Field::ScriptMetadata(v) = self.2 {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have script metadata type", self.0, self.1)))
            },
            FieldType::FileMetadata => if let Field::FileMetadata(v) = self.2 {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have file metadata type", self.0, self.1)))
            },
            FieldType::SavedNpc => if let Field::SavedNpc(v) = self.2 {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have saved npc type", self.0, self.1)))
            },
            FieldType::Effect => if let Field::Effect(v) = self.2 {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have effect type", self.0, self.1)))
            },
            FieldType::Npc => if let Field::Npc(v) = self.2 {
                v.serialize(serializer)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have NPC type", self.0, self.1)))
            },
            FieldType::DialogMetadata => if let &Field::DialogMetadata(v) = self.2 {
                match v {
                    Left(v) => serializer.serialize_u32(v),
                    Right(v) => v.serialize(serializer)
                }
            } else {
                Err(S::Error::custom(&format!("{} {} field should have dialog metadata type", self.0, self.1)))
            },
            FieldType::Float => if let &Field::Float(v) = self.2 {
                serializer.serialize_f32(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have float type", self.0, self.1)))
            },
            FieldType::Int => if let &Field::Int(v) = self.2 {
                serializer.serialize_i32(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have int type", self.0, self.1)))
            },
            FieldType::Short => if let &Field::Short(v) = self.2 {
                serializer.serialize_i16(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have short type", self.0, self.1)))
            },
            FieldType::Long => if let &Field::Long(v) = self.2 {
                serializer.serialize_i64(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have long type", self.0, self.1)))
            },
            FieldType::Byte => if let &Field::Byte(v) = self.2 {
                serializer.serialize_u8(v)
            } else {
                Err(S::Error::custom(&format!("{} {} field should have byte type", self.0, self.1)))
            },
        }
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
