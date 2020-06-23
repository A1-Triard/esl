use std::fmt::{self, Debug, Display};
use std::str::FromStr;
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::de::{self, Unexpected, VariantAccess};
use serde::de::Error as de_Error;
use either::{Either, Left, Right};
use std::ops::{Index, IndexMut};
use std::convert::TryFrom;

use crate::strings::*;
use crate::serde_helpers::*;

pub use crate::tag::*;

include!(concat!(env!("OUT_DIR"), "/tags.rs"));

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub(crate) enum Newline {
    Unix,
    Dos
}

impl Newline {
    pub fn as_str(self) -> &'static str {
        if self == Newline::Unix { "\n" } else { "\r\n" }
    }
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum FileType {
        ESP = 0,
        ESM = 1,
        ESS = 32
    }
}

enum_serde!(FileType, "file type", u32, to_u32, as from_u32, Unsigned, u64);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum DialogType {
        Topic = 0,
        Voice = 1,
        Greeting = 2,
        Persuasion = 3,
        Journal = 4
    }
}

enum_serde!(DialogType, "dialog type", u8, to_u8, as from_u8, Unsigned, u64);

mod dialog_type_u32 {
    use crate::field::DialogType;
    use serde::{Serializer, Deserializer, Serialize, Deserialize};
    use serde::de::Unexpected;
    use serde::de::Error as de_Error;
    use num_traits::cast::{ToPrimitive, FromPrimitive};

    pub fn serialize<S>(&v: &DialogType, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            v.serialize(serializer)
        } else {
            serializer.serialize_u32(v.to_u32().unwrap())
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<DialogType, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            DialogType::deserialize(deserializer)
        } else {
            let d = u32::deserialize(deserializer)?;
            DialogType::from_u32(d).ok_or_else(|| D::Error::invalid_value(Unexpected::Unsigned(d as u64), &"dialog type"))
        }
    }
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug)]
    pub enum EffectRange {
        Self_ = 0,
        Touch = 1,
        Target = 2,
    }
}

impl Display for EffectRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EffectRange::Self_ => write!(f, "Self"),
            EffectRange::Touch => write!(f, "Touch"),
            EffectRange::Target => write!(f, "Target"),
        }
    }
}

impl FromStr for EffectRange {
    type Err = ();
    
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Self" => Ok(EffectRange::Self_),
            "Touch" => Ok(EffectRange::Touch), 
            "Target" => Ok(EffectRange::Target),
            _ => Err(())
        }
    }
}

enum_serde!(EffectRange, "effect range", u32, to_u32, as from_u32, Unsigned, u64);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum FieldType {
    U8List, U8ListZip,
    String(Option<u32>),
    StringZ, StringZList,
    Multiline(Newline),
    F32, I32, I16, I64, U8,
    MarkerU8(u8),
    Ingredient, ScriptMetadata, DialogMetadata, FileMetadata, Npc, NpcState, Effect, Spell,
    Ai, AiWander, AiTravel, AiTarget, AiActivate, NpcFlags, CreatureFlags, Book, ContainerFlags,
    Creature, Light, MiscItem, Apparatus, Weapon, Armor, BipedObject, BodyPart, Clothing, Enchantment,
    Tool, RepairItem, Position, PositionOrCell, Grid, PathGrid, ScriptVars,
    I16List, I32List, F32List, Weather, Color, SoundChance, Potion, Class, Skill, EffectIndex,
    Item, Sound, EffectMetadata, Race, SoundGen, Info, Faction, SkillMetadata, Interior
}

impl FieldType {
    pub fn from_tags(record_tag: Tag, field_tag: Tag) -> FieldType {
        match (record_tag, field_tag) {
            (APPA, AADT) => FieldType::Apparatus,
            (INFO, ACDT) => FieldType::StringZ,
            (_, ACDT) => FieldType::U8ListZip,
            (_, ACSC) => FieldType::U8ListZip,
            (_, ACSL) => FieldType::U8ListZip,
            (_, ACTN) => FieldType::I32,
            (_, AI_A) => FieldType::AiActivate,
            (_, AI_E) => FieldType::AiTarget,
            (_, AI_F) => FieldType::AiTarget,
            (_, AI_T) => FieldType::AiTravel,
            (_, AI_W) => FieldType::AiWander,
            (_, AIDT) => FieldType::Ai,
            (ALCH, ALDT) => FieldType::Potion,
            (CELL, AMBI) => FieldType::Interior,
            (FACT, ANAM) => FieldType::String(None),
            (_, ANAM) => FieldType::StringZ,
            (ARMO, AODT) => FieldType::Armor,
            (REFR, APUD) => FieldType::String(None), // TODO
            (_, ASND) => FieldType::StringZ,
            (_, AVFX) => FieldType::StringZ,
            (BOOK, BKDT) => FieldType::Book,
            (ARMO, BNAM) => FieldType::String(None),
            (BODY, BNAM) => FieldType::String(None),
            (CLOT, BNAM) => FieldType::String(None),
            (INFO, BNAM) => FieldType::Multiline(Newline::Dos),
            (_, BNAM) => FieldType::StringZ,
            (_, BSND) => FieldType::StringZ,
            (_, BVFX) => FieldType::StringZ,
            (BODY, BYDT) => FieldType::BodyPart,
            (_, CHRD) => FieldType::U8ListZip,
            (CLAS, CLDT) => FieldType::Class,
            (ARMO, CNAM) => FieldType::String(None),
            (CLOT, CNAM) => FieldType::String(None),
            (KLST, CNAM) => FieldType::I32,
            (REGN, CNAM) => FieldType::Color,
            (_, CNAM) => FieldType::StringZ,
            (CELL, CNDT) => FieldType::Grid,
            (CONT, CNDT) => FieldType::F32,
            (CELL, CRED) => FieldType::U8ListZip,
            (CELL, CSHN) => FieldType::StringZ,
            (_, CSND) => FieldType::StringZ,
            (CELL, CSTN) => FieldType::StringZ,
            (CLOT, CTDT) => FieldType::Clothing,
            (_, CVFX) => FieldType::StringZ,
            (CELL, DATA) => FieldType::PositionOrCell,
            (DIAL, DATA) => FieldType::DialogMetadata,
            (INFO, DATA) => FieldType::Info,
            (LAND, DATA) => FieldType::I32,
            (LEVC, DATA) => FieldType::I32,
            (LEVI, DATA) => FieldType::I32,
            (LTEX, DATA) => FieldType::StringZ,
            (PGRD, DATA) => FieldType::PathGrid,
            (REFR, DATA) => FieldType::Position,
            (SNDG, DATA) => FieldType::SoundGen,
            (SOUN, DATA) => FieldType::Sound,
            (SSCR, DATA) => FieldType::String(None),
            (TES3, DATA) => FieldType::I64,
            (QUES, DATA) => FieldType::StringZ,
            (_, DELE) => FieldType::I32,
            (BSGN, DESC) => FieldType::StringZ,
            (_, DESC) => FieldType::String(None),
            (_, DNAM) => FieldType::StringZ,
            (_, DODT) => FieldType::Position,
            (ALCH, ENAM) => FieldType::Effect,
            (ENCH, ENAM) => FieldType::Effect,
            (PCDT, ENAM) => FieldType::I64,
            (SPEL, ENAM) => FieldType::Effect,
            (_, ENAM) => FieldType::StringZ,
            (ENCH, ENDT) => FieldType::Enchantment,
            (FACT, FADT) => FieldType::Faction,
            (CELL, FGTN) => FieldType::StringZ,
            (CONT, FLAG) => FieldType::ContainerFlags,
            (CREA, FLAG) => FieldType::CreatureFlags,
            (NPC_, FLAG) => FieldType::NpcFlags,
            (_, FLAG) => FieldType::I32,
            (_, FLTV) => FieldType::F32,
            (GLOB, FNAM) => FieldType::String(None),
            (PCDT, FNAM) => FieldType::U8ListZip,
            (_, FNAM) => FieldType::StringZ,
            (_, FRMR) => FieldType::I32,
            (_, GMDT) => FieldType::U8ListZip,
            (TES3, HEDR) => FieldType::FileMetadata,
            (_, HSND) => FieldType::StringZ,
            (_, HVFX) => FieldType::StringZ,
            (_, INAM) => FieldType::StringZ,
            (ARMO, INDX) => FieldType::BipedObject,
            (CLOT, INDX) => FieldType::BipedObject,
            (MGEF, INDX) => FieldType::EffectIndex,
            (SKIL, INDX) => FieldType::Skill,
            (_, INDX) => FieldType::I32,
            (CELL, INTV) => FieldType::F32,
            (LAND, INTV) => FieldType::Grid,
            (LEVC, INTV) => FieldType::I16,
            (LEVI, INTV) => FieldType::I16,
            (_, INTV) => FieldType::I32,
            (INGR, IRDT) => FieldType::Ingredient,
            (_, ITEX) => FieldType::StringZ,
            (PCDT, KNAM) => FieldType::U8ListZip,
            (_, KNAM) => FieldType::StringZ,
            (LIGH, LHDT) => FieldType::Light,
            (PCDT, LNAM) => FieldType::I64,
            (LOCK, LKDT) => FieldType::Tool,
            (CELL, LSHN) => FieldType::StringZ,
            (CELL, LSTN) => FieldType::StringZ,
            (_, LVCR) => FieldType::U8,
            (FMAP, MAPD) => FieldType::U8ListZip,
            (FMAP, MAPH) => FieldType::I64,
            (TES3, MAST) => FieldType::StringZ,
            (MISC, MCDT) => FieldType::MiscItem,
            (MGEF, MEDT) => FieldType::EffectMetadata,
            (PCDT, MNAM) => FieldType::StringZ,
            (CELL, MNAM) => FieldType::U8,
            (_, MODL) => FieldType::StringZ,
            (CELL, MVRF) => FieldType::I32,
            (CELL, NAM0) => FieldType::I32,
            (PCDT, NAM0) => FieldType::StringZ,
            (SPLM, NAM0) => FieldType::U8,
            (PCDT, NAM1) => FieldType::StringZ,
            (PCDT, NAM2) => FieldType::StringZ,
            (PCDT, NAM3) => FieldType::StringZ,
            (CELL, NAM5) => FieldType::I32,
            (CELL, NAM8) => FieldType::U8ListZip,
            (CELL, NAM9) => FieldType::I32,
            (PCDT, NAM9) => FieldType::I32,
            (GMST, NAME) => FieldType::String(None),
            (INFO, NAME) => FieldType::String(None),
            (JOUR, NAME) => FieldType::Multiline(Newline::Unix),
            (SPLM, NAME) => FieldType::I32,
            (SSCR, NAME) => FieldType::String(None),
            (_, NAME) => FieldType::StringZ,
            (_, ND3D) => FieldType::U8,
            (LEVC, NNAM) => FieldType::U8,
            (LEVI, NNAM) => FieldType::U8,
            (_, NNAM) => FieldType::StringZ,
            (_, NPCO) => FieldType::Item,
            (CREA, NPDT) => FieldType::Creature,
            (SPLM, NPDT) => FieldType::U8ListZip,
            (NPC_, NPDT) => FieldType::Npc,
            (NPCC, NPDT) => FieldType::NpcState,
            (_, NPCS) => FieldType::String(Some(32)),
            (_, ONAM) => FieldType::StringZ,
            (PROB, PBDT) => FieldType::Tool,
            (_, PGRC) => FieldType::U8ListZip,
            (_, PGRP) => FieldType::U8ListZip,
            (PCDT, PNAM) => FieldType::U8ListZip,
            (_, PNAM) => FieldType::StringZ,
            (_, PTEX) => FieldType::StringZ,
            (RACE, RADT) => FieldType::Race,
            (_, RGNN) => FieldType::StringZ,
            (REPA, RIDT) => FieldType::RepairItem,
            (FACT, RNAM) => FieldType::String(Some(32)),
            (SCPT, RNAM) => FieldType::I32,
            (_, RNAM) => FieldType::StringZ,
            (SCPT, SCDT) => FieldType::U8ListZip,
            (SCPT, SCHD) => FieldType::ScriptMetadata,
            (TES3, SCRD) => FieldType::U8ListZip,
            (_, SCRI) => FieldType::StringZ,
            (TES3, SCRS) => FieldType::U8ListZip,
            (_, SCTX) => FieldType::Multiline(Newline::Dos),
            (SCPT, SCVR) => FieldType::StringZList,
            (_, SCVR) => FieldType::String(None),
            (SKIL, SKDT) => FieldType::SkillMetadata,
            (_, SLCS) => FieldType::ScriptVars,
            (_, SLFD) => FieldType::F32List,
            (_, SLLD) => FieldType::I32List,
            (_, SLSD) => FieldType::I16List,
            (PCDT, SNAM) => FieldType::U8ListZip,
            (REGN, SNAM) => FieldType::SoundChance,
            (_, SNAM) => FieldType::StringZ,
            (SPLM, SPDT) => FieldType::U8ListZip,
            (SPEL, SPDT) => FieldType::Spell,
            (_, STPR) => FieldType::U8ListZip,
            (_, STRV) => FieldType::String(None),
            (BOOK, TEXT) => FieldType::Multiline(Newline::Dos),
            (_, TEXT) => FieldType::StringZ,
            (_, TNAM) => FieldType::StringZ,
            (INFO, QSTF) => FieldType::MarkerU8(1),
            (INFO, QSTN) => FieldType::MarkerU8(1),
            (INFO, QSTR) => FieldType::MarkerU8(1),
            (_, VCLR) => FieldType::U8ListZip,
            (_, VHGT) => FieldType::U8ListZip,
            (_, VNML) => FieldType::U8ListZip,
            (_, VTEX) => FieldType::U8ListZip,
            (REGN, WEAT) => FieldType::Weather,
            (_, WHGT) => FieldType::F32,
            (_, WIDX) => FieldType::I64,
            (_, WNAM) => FieldType::U8ListZip,
            (WEAP, WPDT) => FieldType::Weapon,
            (_, XCHG) => FieldType::F32,
            (_, XHLT) => FieldType::I32,
            (_, XIDX) => FieldType::I32,
            (REFR, XNAM) => FieldType::StringZ,
            (SPLM, XNAM) => FieldType::U8,
            (_, XSCL) => FieldType::F32,
            (_, XSOL) => FieldType::StringZ,
            (REFR, YNAM) => FieldType::I32,
            (CELL, ZNAM) => FieldType::U8,
            _ => FieldType::U8List
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[derive(Derivative)]
#[derivative(PartialEq, Eq)]
pub struct Ingredient {
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub weight: f32,
    pub value: u32,
    #[serde(with="effect_index_option_i32")]
    pub effect_1_index: Either<Option<i32>, EffectIndex>,
    #[serde(with="effect_index_option_i32")]
    pub effect_2_index: Either<Option<i32>, EffectIndex>,
    #[serde(with="effect_index_option_i32")]
    pub effect_3_index: Either<Option<i32>, EffectIndex>,
    #[serde(with="effect_index_option_i32")]
    pub effect_4_index: Either<Option<i32>, EffectIndex>,
    #[serde(with="skill_option_i32")]
    pub effect_1_skill: Either<Option<i32>, Skill>,
    #[serde(with="skill_option_i32")]
    pub effect_2_skill: Either<Option<i32>, Skill>,
    #[serde(with="skill_option_i32")]
    pub effect_3_skill: Either<Option<i32>, Skill>,
    #[serde(with="skill_option_i32")]
    pub effect_4_skill: Either<Option<i32>, Skill>,
    #[serde(with="attribute_option_i32")]
    pub effect_1_attribute: Either<Option<i32>, Attribute>,
    #[serde(with="attribute_option_i32")]
    pub effect_2_attribute: Either<Option<i32>, Attribute>,
    #[serde(with="attribute_option_i32")]
    pub effect_3_attribute: Either<Option<i32>, Attribute>,
    #[serde(with="attribute_option_i32")]
    pub effect_4_attribute: Either<Option<i32>, Attribute>,
}

fn eq_f32(a: &f32, b: &f32) -> bool {
    a.to_bits() == b.to_bits()
}

fn eq_f32_list(a: &[f32], b: &[f32]) -> bool {
    if a.len() != b.len() { return false; }
    a.iter().zip(b.iter()).all(|(x, y)| eq_f32(x, y))
}

mod float_32 {
    use serde::{Serializer, Deserializer};
    use crate::serde_helpers::*;

    pub fn serialize<S>(v: &f32, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_f32_as_is(*v, serializer)
    }
    
    pub fn deserialize<'de, D>(deserializer: D) -> Result<f32, D::Error> where D: Deserializer<'de> {
        deserialize_f32_as_is(deserializer)
    }
}

mod option_i8 {
    use serde::{Serializer, Deserializer, Deserialize, Serialize};
    use serde::ser::Error as ser_Error;

    pub fn serialize<S>(&v: &Option<i8>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            v.serialize(serializer)
        } else {
            let v = if let Some(v) = v {
                if v == -1 { return Err(S::Error::custom("-1 is reserved")); }
                v
            } else {
                -1
            };
            serializer.serialize_i8(v)
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<i8>, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            <Option<i8>>::deserialize(deserializer)
        } else {
            let d = i8::deserialize(deserializer)?;
            if d == -1 {
                Ok(None)
            } else {
                Ok(Some(d))
            }
        }
    }
}

mod bool_u32 {
    use serde::{Serializer, Deserializer};
    use crate::serde_helpers::*;

    pub fn serialize<S>(&v: &bool, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_bool_u32(v, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<bool, D::Error> where D: Deserializer<'de> {
        deserialize_bool_u32(deserializer)
    }
}

mod bool_u8 {
    use serde::{Serializer, Deserializer};
    use crate::serde_helpers::*;

    pub fn serialize<S>(&v: &bool, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_bool_u8(v, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<bool, D::Error> where D: Deserializer<'de> {
        deserialize_bool_u8(deserializer)
    }
}

mod bool_either_i16 {
    use serde::{Serializer, Deserializer, Deserialize, Serialize};
    use serde::de::Error as de_Error;
    use either::{Either, Left, Right};
    use serde::de::Unexpected;

    pub fn serialize<S>(&v: &Either<bool, bool>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            v.serialize(serializer)
        } else {
            let v = match v {
                Left(false) => -2,
                Left(true) => -1,
                Right(false) => 0,
                Right(true) => 1
            };
            serializer.serialize_i16(v)
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Either<bool, bool>, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            <Either<bool, bool>>::deserialize(deserializer)
        } else {
            let d = i16::deserialize(deserializer)?;
            match d {
                0 => Ok(Right(false)),
                1 => Ok(Right(true)),
                -1 => Ok(Left(true)),
                -2 => Ok(Left(false)),
                d => Err(D::Error::invalid_value(Unexpected::Signed(d as i64), &"-2, -1, 0, or 1"))
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ScriptVars {
    pub shorts: u32,
    pub longs: u32,
    pub floats: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ScriptMetadata {
    #[serde(with = "string_32")]
    pub name: String,
    pub vars: ScriptVars,
    pub data_size: u32,
    pub var_table_size: u32
}

mod string_32 {
    use serde::{Serializer, Deserializer};
    use crate::serde_helpers::*;

    pub fn serialize<S>(s: &str, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_string_tuple(s, 32, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<String, D::Error> where D: Deserializer<'de> {
        deserialize_string_tuple(32, deserializer)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct FileMetadata {
    pub version: u32,
    #[serde(rename="type")]
    pub file_type: FileType,
    #[serde(with = "string_32")]
    pub author: String,
    #[serde(with = "multiline_256_dos")]
    pub description: Vec<String>,
    pub records: u32
}

mod multiline_256_dos {
    use serde::{Serializer, Deserializer};
    use crate::field::*;

    pub fn serialize<S>(lines: &[String], serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_string_list(lines, Newline::Dos.as_str(), Some(256), serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error> where D: Deserializer<'de> {
        deserialize_string_list(Newline::Dos.as_str(), Some(256), deserializer)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Effect {
    #[serde(with="effect_index_option_i16")]
    pub index: Either<Option<i16>, EffectIndex>,
    #[serde(with="skill_option_i8")]
    pub skill: Either<Option<i8>, Skill>,
    #[serde(with="attribute_option_i8")]
    pub attribute: Either<Option<i8>, Attribute>,
    pub range: EffectRange,
    pub area: i32,
    pub duration: i32,
    pub magnitude_min: i32,
    pub magnitude_max: i32
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct NpcState {
    pub disposition: i16,
    pub reputation: i16,
    pub index: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct NpcStats {
    pub attributes: Attributes<u8>,
    pub skills: Skills,
    pub faction: u8,
    pub health: i16,
    pub magicka: i16,
    pub fatigue: i16,
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
#[serde(rename="NpcStatsOption")]
enum NpcStatsOptionHRSurrogate {
    None(u16),
    Some(NpcStats)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Npc {
    pub level: u16,
    pub disposition: i8,
    pub reputation: i8,
    pub rank: i8,
    pub gold: i32,
    pub padding: u8,
    pub stats: Either<u16, NpcStats>
}

#[derive(Serialize, Deserialize)]
struct NpcHRSurrogate {
    pub level: u16,
    pub disposition: i8,
    pub reputation: i8,
    pub rank: i8,
    pub gold: i32,
    pub padding: u8,
    pub stats: NpcStatsOptionHRSurrogate
}

impl From<Npc> for NpcHRSurrogate {
    fn from(t: Npc) -> Self {
        NpcHRSurrogate {
            level: t.level, disposition: t.disposition, reputation:t.reputation,
            rank: t.rank, gold: t.gold, padding: t.padding,
            stats: t.stats.either(NpcStatsOptionHRSurrogate::None, NpcStatsOptionHRSurrogate::Some)
        }
    }
}

impl From<NpcHRSurrogate> for Npc {
    fn from(t: NpcHRSurrogate) -> Self {
        let stats = match t.stats {
            NpcStatsOptionHRSurrogate::None(x) => Left(x),
            NpcStatsOptionHRSurrogate::Some(x) => Right(x)
        };
        Npc {
            level: t.level, disposition: t.disposition, reputation:t.reputation,
            rank: t.rank, gold: t.gold, padding: t.padding,
            stats
        }
    }
}

impl Serialize for Npc {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            NpcHRSurrogate::from(self.clone()).serialize(serializer)
        } else {
            let surrogate: Either<NpcNHRSurrogate12, NpcNHRSurrogate52> = self.clone().into();
            match surrogate {
                Left(npc12) => serializer.serialize_newtype_variant(
                    name_of!(type Npc), 12, "Npc12", &npc12
                ),
                Right(npc52) => serializer.serialize_newtype_variant(
                    name_of!(type Npc), 52, "Npc52", &npc52
                ),
            }
        }
    }
}

struct NpcNHRDeserializer;

impl<'de> de::Visitor<'de> for NpcNHRDeserializer {
    type Value = Npc;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NPC")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error> where A: de::EnumAccess<'de> {
        let (variant_index, variant) = data.variant::<u32>()?;
        match variant_index {
            12 => Ok(variant.newtype_variant::<NpcNHRSurrogate12>()?.into()),
            52 => Ok(variant.newtype_variant::<NpcNHRSurrogate52>()?.into()),
            n => Err(A::Error::invalid_value(Unexpected::Unsigned(n as u64), &self))
        }
    }
}

impl<'de> Deserialize<'de> for Npc {
    fn deserialize<D>(deserializer: D) -> Result<Npc, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            NpcHRSurrogate::deserialize(deserializer).map(Npc::from)
        } else {
            deserializer.deserialize_enum(name_of!(type Npc), &["Npc12", "Npc52"], NpcNHRDeserializer)
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(rename="Npc12")]
struct NpcNHRSurrogate12 {
    pub level: u16,
    pub disposition: i8,
    pub reputation: i8,
    pub rank: i8,
    pub padding_8: u8,
    pub padding_16: u16,
    pub gold: i32,
}

#[derive(Serialize, Deserialize)]
#[serde(rename="Npc52")]
struct NpcNHRSurrogate52 {
    pub level: u16,
    pub stats: NpcStats,
    pub disposition: i8,
    pub reputation: i8,
    pub rank: i8,
    pub padding: u8,
    pub gold: i32,
}

impl From<Npc> for Either<NpcNHRSurrogate12, NpcNHRSurrogate52> {
    fn from(npc: Npc) -> Either<NpcNHRSurrogate12, NpcNHRSurrogate52> {
        match npc.stats {
            Right(stats) => Right(NpcNHRSurrogate52 {
                level: npc.level, disposition: npc.disposition,
                reputation: npc.reputation, rank: npc.rank,
                padding: npc.padding,
                gold: npc.gold,
                stats
            }),
            Left(padding_16) => Left(NpcNHRSurrogate12 {
                level: npc.level, disposition: npc.disposition,
                reputation: npc.reputation, rank: npc.rank,
                padding_8: npc.padding, padding_16,
                gold: npc.gold
            })
        }
    }
}

impl From<NpcNHRSurrogate52> for Npc {
    fn from(npc: NpcNHRSurrogate52) -> Npc {
        Npc {
            level: npc.level, disposition: npc.disposition, reputation: npc.reputation,
            rank: npc.rank, gold: npc.gold, padding: npc.padding,
            stats: Right(npc.stats)
        }
    }
}

impl From<NpcNHRSurrogate12> for Npc {
    fn from(npc: NpcNHRSurrogate12) -> Npc {
        Npc {
            level: npc.level, disposition: npc.disposition, reputation: npc.reputation,
            rank: npc.rank, gold: npc.gold, padding: npc.padding_8,
            stats: Left(npc.padding_16)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Item {
    pub count: i32,
    #[serde(with = "string_32")]
    pub item_id: String,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum SpellType {
        Spell = 0,
        Ability = 1,
        Blight = 2,
        Disease = 3,
        Curse = 4,
        Power = 5
    }
}

enum_serde!(SpellType, "spell type", u32, to_u32, as from_u32, Unsigned, u64);

pub_bitflags_display!(SpellFlags, u32, AUTO_CALCULATE_COST = 1, PC_START = 2, ALWAYS_SUCCEEDS = 4);

enum_serde!(SpellFlags, "spell flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Spell {
    #[serde(rename="type")]
    pub spell_type: SpellType,
    pub cost: u32,
    pub flags: SpellFlags
}

pub_bitflags_display!(Services, u32,
    WEAPON = 0x00000001,
    ARMOR = 0x00000002,
    CLOTHING = 0x00000004,
    BOOKS = 0x00000008,
    INGREDIENTS = 0x00000010,
    PICKS = 0x00000020,
    PROBES = 0x00000040,
    LIGHTS = 0x00000080,
    APPARATUS = 0x00000100,
    REPAIR_ITEMS = 0x00000200,
    MISCELLANEOUS  = 0x00000400,
    SPELLS = 0x00000800,
    MAGIC_ITEMS = 0x00001000,
    POTIONS = 0x00002000,
    TRAINING = 0x00004000,
    SPELLMAKING = 0x00008000,
    ENCHANTING = 0x00010000,
    REPAIR = 0x00020000,
    _80000 = 0x00080000,
    _200000 = 0x00200000,
    _400000 = 0x00400000,
    _800000 = 0x00800000,
    _1000000 = 0x01000000
);

enum_serde!(Services, "services", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Ai {
    pub hello: u16,
    pub fight: u8,
    pub flee: u8,
    pub alarm: u8,
    pub padding_8: u8,
    pub padding_16: u16,
    pub services: Services
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct AiWander {
    pub distance: u16,
    pub duration: u16,
    pub time_of_day: u8,
    pub idle: [u8; 8],
    #[serde(with="bool_u8")]
    pub repeat: bool,
}

pub_bitflags_display!(AiTravelFlags, u32,
    RESET = 0x000100,
    _1 = 0x000001,
    _800 = 0x000800,
    _1000 = 0x001000,
    _4000 = 0x004000,
    _10000 = 0x010000,
    _20000 = 0x020000,
    _40000 = 0x040000,
    _400000 = 0x400000
);

enum_serde!(AiTravelFlags, "AI travel flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct AiTravel {
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub x: f32,
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub y: f32,
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub z: f32,
    pub flags: AiTravelFlags
}

pub_bitflags_display!(AiTargetFlags, u8,
    _1 = 0x01,
    _2 = 0x02,
    _4 = 0x04,
    _8 = 0x08
);

enum_serde!(AiTargetFlags, "AI target flags", u8, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct AiTarget {
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub x: f32,
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub y: f32,
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub z: f32,
    pub duration: u16,
    #[serde(with = "string_32")]
    pub actor_id: String,
    #[serde(with = "bool_u8")]
    pub reset: bool,
    pub flags: AiTargetFlags
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct AiActivate {
    #[serde(with = "string_32")]
    pub object_id: String,
    #[serde(with = "bool_u8")]
    pub reset: bool
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum Blood {
        Default = 0,
        Skeleton = 4,
        MetalSparks = 8,
    }
}

enum_serde!(Blood, "blood", u8, to_u8, as from_u8, Unsigned, u64);

pub_bitflags_display!(NpcFlags, u8,
    FEMALE = 0x01,
    ESSENTIAL = 0x02,
    RESPAWN = 0x04,
    AUTO_CALCULATE_STATS = 0x10
);

enum_serde!(NpcFlags, "NPC flags", u8, bits, try from_bits, Unsigned, u64, ^0x08);

pub_bitflags_display!(CreatureFlags, u8,
    BIPED = 0x01,
    RESPAWN = 0x02,
    WEAPON_AND_SHIELD = 0x04,
    SWIMS = 0x10,
    FLIES = 0x20,
    WALKS = 0x40,
    ESSENTIAL = 0x80
);

enum_serde!(CreatureFlags, "creature flags", u8, bits, try from_bits, Unsigned, u64, ^0x08);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct FlagsAndBlood<Flags> {
    pub flags: Flags,
    pub blood: Blood,
    pub padding: u16,
}

pub_bitflags_display!(BookFlags, u32,
    SCROLL = 0x01,
    _10 = 0x10
);

enum_serde!(BookFlags, "book flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Book {
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub weight: f32,
    pub value: u32,
    pub flags: BookFlags,
    #[serde(with="skill_option_i32")]
    pub skill: Either<Option<i32>, Skill>,
    pub enchantment: u32
}

pub_bitflags_display!(ContainerFlags, u32,
    ORGANIC = 0x01,
    RESPAWN = 0x02
);

enum_serde!(ContainerFlags, "container flags", u32, bits, try from_bits, Unsigned, u64, ^0x08);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum CreatureType {
        Creature = 0,
        Daedra  = 1,
        Undead = 2,
        Humanoid = 3
    }
}

enum_serde!(CreatureType, "creature type", u32, to_u32, as from_u32, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Creature {
    #[serde(rename="type")]
    pub creature_type: CreatureType,
    pub level: u32,
    pub attributes: Attributes<u32>,
    pub health: u32,
    pub magicka: u32,
    pub fatigue: u32,
    pub soul: u32,
    pub combat: u32,
    pub magic: u32,
    pub stealth: u32,
    pub attack_1_min: u32,
    pub attack_1_max: u32,
    pub attack_2_min: u32,
    pub attack_2_max: u32,
    pub attack_3_min: u32,
    pub attack_3_max: u32,
    pub gold: u32,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum Attribute {
        Strength = 0,
        Intelligence = 1,
        Willpower = 2,
        Agility = 3,
        Speed = 4,
        Endurance = 5,
        Personality = 6,
        Luck = 7,
    }
}

enum_serde!(Attribute, "attribute", u32, to_u32, as from_u32, Unsigned, u64);

mod attribute_option_i8 {
    use serde::{Serializer, Deserializer};
    use either::{Either};
    use crate::field::Attribute;
    use num_traits::cast::{ToPrimitive, FromPrimitive};
    use crate::serde_helpers::*;

    pub fn serialize<S>(&v: &Either<Option<i8>, Attribute>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_option_index("attribute", -1, Attribute::from_i8, |x| x.to_i8().unwrap(), v, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Either<Option<i8>, Attribute>, D::Error> where D: Deserializer<'de> {
        deserialize_option_index(-1, Attribute::from_i8, deserializer)
    }
}

mod attribute_option_i32 {
    use serde::{Serializer, Deserializer};
    use either::{Either};
    use crate::field::Attribute;
    use num_traits::cast::{ToPrimitive, FromPrimitive};
    use crate::serde_helpers::*;

    pub fn serialize<S>(&v: &Either<Option<i32>, Attribute>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_option_index("attribute", -1, Attribute::from_i32, |x| x.to_i32().unwrap(), v, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Either<Option<i32>, Attribute>, D::Error> where D: Deserializer<'de> {
        deserialize_option_index(-1, Attribute::from_i32, deserializer)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Attributes<T> {
    pub strength: T,
    pub intelligence: T,
    pub willpower: T,
    pub agility: T,
    pub speed: T,
    pub endurance: T,
    pub personality: T,
    pub luck: T,
}

impl<T> Index<Attribute> for Attributes<T> {
    type Output = T;

    fn index(&self, index: Attribute) -> &Self::Output {
        match index {
            Attribute::Strength => &self.strength,
            Attribute::Intelligence => &self.intelligence,
            Attribute::Willpower => &self.willpower,
            Attribute::Agility => &self.agility,
            Attribute::Speed => &self.speed,
            Attribute::Endurance => &self.endurance,
            Attribute::Personality => &self.personality,
            Attribute::Luck => &self.luck,
        }
    }
}

impl<T> IndexMut<Attribute> for Attributes<T> {
    fn index_mut(&mut self, index: Attribute) -> &mut Self::Output {
        match index {
            Attribute::Strength => &mut self.strength,
            Attribute::Intelligence => &mut self.intelligence,
            Attribute::Willpower => &mut self.willpower,
            Attribute::Agility => &mut self.agility,
            Attribute::Speed => &mut self.speed,
            Attribute::Endurance => &mut self.endurance,
            Attribute::Personality => &mut self.personality,
            Attribute::Luck => &mut self.luck,
        }
    }
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum Skill {
        Block = 0,
        Armorer = 1,
        MediumArmor = 2,
        HeavyArmor = 3,
        BluntWeapon = 4,
        LongBlade = 5,
        Axe = 6,
        Spear = 7,
        Athletics = 8,
        Enchant = 9,
        Destruction = 10,
        Alteration = 11,
        Illusion = 12,
        Conjuration = 13,
        Mysticism = 14,
        Restoration = 15,
        Alchemy = 16,
        Unarmored = 17,
        Security = 18,
        Sneak = 19,
        Acrobatics = 20,
        LightArmor = 21,
        ShortBlade = 22,
        Marksman = 23,
        Mercantile = 24,
        Speechcraft = 25,
        HandToHand = 26
    }
}

enum_serde!(Skill, "skill", u32, to_u32, as from_u32, Unsigned, u64);

mod skill_option_i32 {
    use serde::{Serializer, Deserializer};
    use either::{Either};
    use crate::field::Skill;
    use num_traits::cast::{ToPrimitive, FromPrimitive};
    use crate::serde_helpers::*;

    pub fn serialize<S>(&v: &Either<Option<i32>, Skill>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_option_index("skill", -1, Skill::from_i32, |x| x.to_i32().unwrap(), v, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Either<Option<i32>, Skill>, D::Error> where D: Deserializer<'de> {
        deserialize_option_index(-1, Skill::from_i32, deserializer)
    }
}

mod skill_option_i8 {
    use serde::{Serializer, Deserializer};
    use either::{Either};
    use crate::field::Skill;
    use num_traits::cast::{ToPrimitive, FromPrimitive};
    use crate::serde_helpers::*;

    pub fn serialize<S>(&v: &Either<Option<i8>, Skill>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_option_index("skill", -1, Skill::from_i8, |x| x.to_i8().unwrap(), v, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Either<Option<i8>, Skill>, D::Error> where D: Deserializer<'de> {
        deserialize_option_index(-1, Skill::from_i8, deserializer)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Skills {
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
}

impl Index<Skill> for Skills {
    type Output = u8;

    fn index(&self, index: Skill) -> &Self::Output {
        match index {
            Skill::Block => &self.block,
            Skill::Armorer => &self.armorer,
            Skill::MediumArmor => &self.medium_armor,
            Skill::HeavyArmor => &self.heavy_armor,
            Skill::BluntWeapon => &self.blunt_weapon,
            Skill::LongBlade => &self.long_blade,
            Skill::Axe => &self.axe,
            Skill::Spear => &self.spear,
            Skill::Athletics => &self.athletics,
            Skill::Enchant => &self.enchant,
            Skill::Destruction => &self.destruction,
            Skill::Alteration => &self.alteration,
            Skill::Illusion => &self.illusion,
            Skill::Conjuration => &self.conjuration,
            Skill::Mysticism => &self.mysticism,
            Skill::Restoration => &self.restoration,
            Skill::Alchemy => &self.alchemy,
            Skill::Unarmored => &self.unarmored,
            Skill::Security => &self.security,
            Skill::Sneak => &self.sneak,
            Skill::Acrobatics => &self.acrobatics,
            Skill::LightArmor => &self.light_armor,
            Skill::ShortBlade => &self.short_blade,
            Skill::Marksman => &self.marksman,
            Skill::Mercantile => &self.mercantile,
            Skill::Speechcraft => &self.speechcraft,
            Skill::HandToHand => &self.hand_to_hand,
        }
    }
}

impl IndexMut<Skill> for Skills {
    fn index_mut(&mut self, index: Skill) -> &mut Self::Output {
        match index {
            Skill::Block => &mut self.block,
            Skill::Armorer => &mut self.armorer,
            Skill::MediumArmor => &mut self.medium_armor,
            Skill::HeavyArmor => &mut self.heavy_armor,
            Skill::BluntWeapon => &mut self.blunt_weapon,
            Skill::LongBlade => &mut self.long_blade,
            Skill::Axe => &mut self.axe,
            Skill::Spear => &mut self.spear,
            Skill::Athletics => &mut self.athletics,
            Skill::Enchant => &mut self.enchant,
            Skill::Destruction => &mut self.destruction,
            Skill::Alteration => &mut self.alteration,
            Skill::Illusion => &mut self.illusion,
            Skill::Conjuration => &mut self.conjuration,
            Skill::Mysticism => &mut self.mysticism,
            Skill::Restoration => &mut self.restoration,
            Skill::Alchemy => &mut self.alchemy,
            Skill::Unarmored => &mut self.unarmored,
            Skill::Security => &mut self.security,
            Skill::Sneak => &mut self.sneak,
            Skill::Acrobatics => &mut self.acrobatics,
            Skill::LightArmor => &mut self.light_armor,
            Skill::ShortBlade => &mut self.short_blade,
            Skill::Marksman => &mut self.marksman,
            Skill::Mercantile => &mut self.mercantile,
            Skill::Speechcraft => &mut self.speechcraft,
            Skill::HandToHand => &mut self.hand_to_hand,
        }
    }
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum School {
        Alteration = 0,
        Conjuration = 1,
        Illusion = 2,
        Destruction = 3,
        Mysticism = 4,
        Restoration = 5,
    }
}

enum_serde!(School, "school", u32, to_u32, as from_u32, Unsigned, u64);

impl From<School> for Skill {
    fn from(s: School) -> Skill {
        match s {
            School::Alteration => Skill::Alteration,
            School::Conjuration => Skill::Conjuration,
            School::Illusion => Skill::Illusion,
            School::Destruction => Skill::Destruction,
            School::Mysticism => Skill::Mysticism,
            School::Restoration => Skill::Restoration
        }
    }
}

impl TryFrom<Skill> for School {
    type Error = ();
    
    fn try_from(s: Skill) -> Result<School, ()> {
        match s {
            Skill::Alteration => Ok(School::Alteration),
            Skill::Conjuration => Ok(School::Conjuration),
            Skill::Illusion => Ok(School::Illusion),
            Skill::Destruction => Ok(School::Destruction),
            Skill::Mysticism => Ok(School::Mysticism),
            Skill::Restoration => Ok(School::Restoration),
            _ => Err(())
        }
    }
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum EffectIndex {
        WaterBreathing = 0, SwiftSwim = 1, WaterWalking = 2, Shield = 3, FireShield = 4, LightningShield = 5,
        FrostShield = 6, Burden = 7, Feather = 8, Jump = 9, Levitate = 10, SlowFall = 11, Lock = 12, Open = 13,
        FireDamage = 14, ShockDamage = 15, FrostDamage = 16, DrainAttribute = 17, DrainHealth = 18,
        DrainSpellpoints = 19, DrainFatigue = 20, DrainSkill = 21, DamageAttribute = 22, DamageHealth = 23,
        DamageMagicka = 24, DamageFatigue = 25, DamageSkill = 26, Poison = 27, WeaknessToFire = 28,
        WeaknessToFrost = 29, WeaknessToShock = 30, WeaknessToMagicka = 31, WeaknessToCommonDisease = 32,
        WeaknessToBlightDisease = 33, WeaknessToCorprusDisease = 34, WeaknessToPoison = 35,
        WeaknessToNormalWeapons = 36, DisintegrateWeapon = 37, DisintegrateArmor = 38, Invisibility = 39,
        Chameleon = 40, Light = 41, Sanctuary = 42, NightEye = 43, Charm = 44, Paralyze = 45, Silence = 46,
        Blind = 47, Sound = 48, CalmHumanoid = 49, CalmCreature = 50, FrenzyHumanoid = 51, FrenzyCreature = 52,
        DemoralizeHumanoid = 53, DemoralizeCreature = 54, RallyHumanoid = 55, RallyCreature = 56, Dispel = 57,
        Soultrap = 58, Telekinesis = 59, Mark = 60, Recall = 61, DivineIntervention = 62, AlmsiviIntervention = 63,
        DetectAnimal = 64, DetectEnchantment = 65, DetectKey = 66, SpellAbsorption = 67, Reflect = 68,
        CureCommonDisease = 69, CureBlightDisease = 70, CureCorprusDisease = 71, CurePoison = 72,
        CureParalyzation = 73, RestoreAttribute = 74, RestoreHealth = 75, RestoreSpellPoints = 76,
        RestoreFatigue = 77, RestoreSkill = 78, FortifyAttribute = 79, FortifyHealth = 80, FortifySpellpoints = 81,
        FortifyFatigue = 82, FortifySkill = 83, FortifyMagickaMultiplier = 84, AbsorbAttribute = 85,
        AbsorbHealth = 86, AbsorbSpellPoints = 87, AbsorbFatigue = 88, AbsorbSkill = 89, ResistFire = 90,
        ResistFrost = 91, ResistShock = 92, ResistMagicka = 93, ResistCommonDisease = 94,
        ResistBlightDisease = 95, ResistCorprusDisease = 96, ResistPoison = 97, ResistNormalWeapons = 98,
        ResistParalysis = 99, RemoveCurse = 100, TurnUndead = 101, SummonScamp = 102, SummonClannfear = 103,
        SummonDaedroth = 104, SummonDremora = 105, SummonAncestralGhost = 106, SummonSkeletalMinion = 107,
        SummonLeastBonewalker = 108, SummonGreaterBonewalker = 109, SummonBonelord = 110,
        SummonWingedTwilight = 111, SummonHunger = 112, SummonGoldensaint = 113, SummonFlameAtronach = 114,
        SummonFrostAtronach = 115, SummonStormAtronach = 116, FortifyAttackBonus = 117, CommandCreatures = 118,
        CommandHumanoids = 119, BoundDagger = 120, BoundLongsword = 121, BoundMace = 122, BoundBattleAxe = 123,
        BoundSpear = 124, BoundLongbow = 125, ExtraSpell = 126, BoundCuirass = 127, BoundHelm = 128,
        BoundBoots = 129, BoundShield = 130, BoundGloves = 131, Corpus = 132, Vampirism = 133,
        SummonCenturionSphere = 134, SunDamage = 135, StuntedMagicka = 136, SummonFabricant = 137,
        SummonCreature01 = 138, SummonCreature02 = 139, SummonCreature03 = 140, SummonCreature04 = 141,
        SummonCreature05 = 142
    }
}

enum_serde!(EffectIndex, "effect index", u32, to_u32, as from_u32, Unsigned, u64);

mod effect_index_option_i32 {
    use serde::{Serializer, Deserializer};
    use either::{Either};
    use crate::field::EffectIndex;
    use num_traits::cast::{ToPrimitive, FromPrimitive};
    use crate::serde_helpers::*;

    pub fn serialize<S>(&v: &Either<Option<i32>, EffectIndex>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_option_index("effect index", -1, EffectIndex::from_i32, |x| x.to_i32().unwrap(), v, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Either<Option<i32>, EffectIndex>, D::Error> where D: Deserializer<'de> {
        deserialize_option_index(-1, EffectIndex::from_i32, deserializer)
    }
}

mod effect_index_option_i16 {
    use serde::{Serializer, Deserializer};
    use either::{Either};
    use crate::field::EffectIndex;
    use num_traits::cast::{ToPrimitive, FromPrimitive};
    use crate::serde_helpers::*;

    pub fn serialize<S>(&v: &Either<Option<i16>, EffectIndex>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_option_index("effect index", -1, EffectIndex::from_i16, |x| x.to_i16().unwrap(), v, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Either<Option<i16>, EffectIndex>, D::Error> where D: Deserializer<'de> {
        deserialize_option_index(-1, EffectIndex::from_i16, deserializer)
    }
}

pub_bitflags_display!(EffectFlags, u32,
    SPELLMAKING = 0x200,
    ENCHANTING = 0x400,
    LIGHT_NEGATIVE = 0x800
);

enum_serde!(EffectFlags, "effect flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct EffectMetadata {
    pub school: School,
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub base_cost: f32,
    pub flags: EffectFlags,
    #[serde(with="color_components")]
    pub color: Color,
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub size_factor: f32,
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub speed: f32,
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub size_cap: f32,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{:02X}{:02X}{:02X}", self.r, self.g, self.b)
    }
}

impl FromStr for Color {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 7{ return Err(()); }
        if &s[0..1] != "#" || &s[1..2] == "+" { return Err(()); }
        let r = u8::from_str_radix(&s[1..3], 16).map_err(|_| ())?;
        let g = u8::from_str_radix(&s[3..5], 16).map_err(|_| ())?;
        let b = u8::from_str_radix(&s[5..7], 16).map_err(|_| ())?;
        Ok(Color { r, g, b })
    }
}

impl Color {
    pub fn to_u32(self) -> u32 {
        (self.r as u32) | ((self.g as u32) << 8) | ((self.b as u32) << 16)
    }
    
    pub fn try_from_u32(u: u32) -> Option<Color> {
        if u & 0xFF000000 != 0 { 
            None
        } else {
            let r = (u & 0xFF) as u8;
            let g = ((u >> 8) & 0xFF) as u8;
            let b = ((u >> 16) & 0xFF) as u8;
            Some(Color { r, g, b })
        }
    }
}

enum_serde!(Color, "RGB color", u32, to_u32(), try try_from_u32, Unsigned, u64);

mod color_components {
    use std::convert::TryInto;
    use serde::{Serializer, Deserializer, Serialize, Deserialize};
    use crate::field::Color;
    use serde::de::Unexpected;
    use serde::de::Error as de_Error;
    use serde::ser::SerializeTuple;

    pub fn serialize<S>(&c: &Color, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            c.serialize(serializer)
        } else {
            let mut serializer = serializer.serialize_tuple(3)?;
            serializer.serialize_element(&(c.r as u32))?;
            serializer.serialize_element(&(c.g as u32))?;
            serializer.serialize_element(&(c.b as u32))?;
            serializer.end()
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Color, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            Color::deserialize(deserializer)
        } else {
            let (r, g, b) = <(u32, u32, u32)>::deserialize(deserializer)?;
            let r = r.try_into().map_err(|_| D::Error::invalid_value(Unexpected::Unsigned(r as u64), &"0 .. 255"))?;
            let g = g.try_into().map_err(|_| D::Error::invalid_value(Unexpected::Unsigned(g as u64), &"0 .. 255"))?;
            let b = b.try_into().map_err(|_| D::Error::invalid_value(Unexpected::Unsigned(b as u64), &"0 .. 255"))?;
            Ok(Color { r, g, b })
        }
    }
}

pub_bitflags_display!(LightFlags, u32,
    DYNAMIC = 0x0001,
    CAN_CARRY = 0x0002,
    NEGATIVE = 0x0004,
    FLICKER = 0x0008,
    FIRE = 0x0010,
    OFF_BY_DEFAULT = 0x0020,
    FLICKER_SLOW = 0x0040,
    PULSE = 0x0080,
    PULSE_SLOW = 0x0100
);

enum_serde!(LightFlags, "light flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Light {
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub weight: f32,
    pub value: u32,
    pub time: i32,
    pub radius: u32,
    pub color: Color,
    pub flags: LightFlags,
}

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct MiscItem {
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub weight: f32,
    pub value: u32,
    #[serde(with = "bool_u32")]
    pub is_key: bool,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum ApparatusType {
        MortarPestle = 0,
        Alembic  = 1,
        Calcinator = 2,
        Retort = 3
    }
}

enum_serde!(ApparatusType, "apparatus type", u32, to_u32, as from_u32, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Apparatus {
    #[serde(rename="type")]
    pub apparatus_type: ApparatusType,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub quality: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub weight: f32,
    pub value: u32,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum ArmorType {
        Helmet = 0,
        Cuirass = 1,
        LeftPauldron = 2,
        RightPauldron = 3,
        Greaves = 4,
        Boots = 5,
        LeftGauntlet = 6,
        RightGauntlet = 7,
        Shield = 8,
        LeftBracer = 9,
        RightBracer = 10
    }
}

enum_serde!(ArmorType, "armor type", u32, to_u32, as from_u32, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Armor {
    #[serde(rename="type")]
    pub armor_type: ArmorType,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub weight: f32,
    pub value: u32,
    pub health: u32,
    pub enchantment: u32,
    pub armor: u32,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum WeaponType {
        ShortBladeOneHand = 0,
        LongBladeOneHand = 1,
        LongBladeTwoClose = 2,
        BluntOneHand = 3,
        BluntTwoClose = 4,
        BluntTwoWide = 5,
        SpearTwoWide = 6,
        AxeOneHand = 7,
        AxeTwoClose = 8,
        MarksmanBow = 9,
        MarksmanCrossbow = 10,
        MarksmanThrown = 11,
        Arrow = 12,
        Bolt = 13
    }
}

enum_serde!(WeaponType, "weapon type", u16, to_u16, as from_u16, Unsigned, u64);

pub_bitflags_display!(WeaponFlags, u32,
    MAGICAL = 0x01,
    SILVER = 0x02
);

enum_serde!(WeaponFlags, "weapon flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Weapon {
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub weight: f32,
    pub value: u32,
    #[serde(rename="type")]
    pub weapon_type: WeaponType,
    pub health: u16,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub speed: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub reach: f32,
    pub enchantment: u16,
    pub chop_min: u8,
    pub chop_max: u8,
    pub slash_min: u8,
    pub slash_max: u8,
    pub thrust_min: u8,
    pub thrust_max: u8,
    pub flags: WeaponFlags,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum BodyPartKind {
        Head = 0,
        Hair = 1,
        Neck = 2,
        Chest = 3,
        Groin = 4,
        Hand = 5,
        Wrist = 6,
        Forearm = 7,
        UpperArm = 8,
        Foot = 9,
        Ankle = 10,
        Knee = 11,
        UpperLeg = 12,
        Clavicle = 13,
        Tail = 14,
    }
}

enum_serde!(BodyPartKind, "body part kind", u8, to_u8, as from_u8, Unsigned, u64);

pub_bitflags_display!(BodyPartFlags, u8,
    FEMALE = 0x01,
    NON_PLAYABLE = 0x02
);

enum_serde!(BodyPartFlags, "body part flags", u8, bits, try from_bits, Unsigned, u64);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum BodyPartType {
        Skin = 0,
        Clothing = 1,
        Armor = 2
    }
}

enum_serde!(BodyPartType, "body part type", u8, to_u8, as from_u8, Unsigned, u64);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum BipedObject {
        Head = 0,
        Hair = 1,
        Neck = 2,
        Cuirass = 3,
        Groin = 4,
        Skirt = 5,
        RightHand = 6,
        LeftHand = 7,
        RightWrist = 8,
        LeftWrist = 9,
        Shield = 10,
        RightForearm = 11,
        LeftForearm = 12,
        RightUpperArm = 13,
        LeftUpperArm = 14,
        RightFoot = 15,
        LeftFoot = 16,
        RightAnkle = 17,
        LeftAnkle = 18,
        RightKnee = 19,
        LeftKnee = 20,
        RightUpperLeg = 21,
        LeftUpperLeg = 22,
        RightPauldron = 23,
        LeftPauldron = 24,
        Weapon = 25,
        Tail = 26,
    }
}

enum_serde!(BipedObject, "biped object", u8, to_u8, as from_u8, Unsigned, u64);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum ClothingType {
        Pants = 0,
        Shoes = 1,
        Shirt = 2,
        Belt = 3,
        Robe = 4,
        RightGlove = 5,
        LeftGlove = 6,
        Skirt = 7,
        Ring = 8,
        Amulet = 9
    }
}

enum_serde!(ClothingType, "clothing type", u32, to_u32, as from_u32, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct BodyPart {
    pub kind: BodyPartKind,
    #[serde(with = "bool_u8")]
    pub vampire: bool,
    pub flags: BodyPartFlags,
    #[serde(rename="type")]
    pub body_part_type: BodyPartType,
}

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Clothing {
    #[serde(rename="type")]
    pub clothing_type: ClothingType,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub weight: f32,
    pub value: u16,
    pub enchantment: u16,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum EnchantmentType {
        CastOnce = 0,
        WhenStrikes = 1,
        WhenUsed = 2,
        ConstantEffect = 3
    }
}

enum_serde!(EnchantmentType, "enchantment type", u32, to_u32, as from_u32, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Enchantment {
    #[serde(rename="type")]
    pub enchantment_type: EnchantmentType,
    pub cost: u32,
    pub charge_amount: u32,
    #[serde(with = "bool_either_i16")]
    pub auto_calculate: Either<bool, bool>,
    pub padding: u16,
}

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Tool {
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub weight: f32,
    pub value: u32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub quality: f32,
    pub uses: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub(crate) struct RepairItem {
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub weight: f32,
    pub value: u32,
    pub uses: u32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub quality: f32,
}

impl From<RepairItem> for Tool {
    fn from(t: RepairItem)-> Tool {
        Tool { weight: t.weight, value: t.value, quality: t.quality, uses: t.uses }
    }
}

impl From<Tool> for RepairItem {
    fn from(t: Tool)-> RepairItem {
        RepairItem { weight: t.weight, value: t.value, quality: t.quality, uses: t.uses }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Position {
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub x: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub y: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub z: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub x_rot: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub y_rot: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub z_rot: f32,
}

pub_bitflags_display!(CellFlags, u32,
    INTERIOR = 0x01,
    HAS_WATER = 0x02,
    ILLEGAL_TO_SLEEP = 0x04,
    BEHAVE_LIKE_EXTERIOR = 0x80,
    _8 = 0x08,
    _10 = 0x10,
    _20 = 0x20,
    _40 = 0x40
);

enum_serde!(CellFlags, "cell flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Cell {
    pub flags: CellFlags,
    pub grid: Grid,
}

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Interior {
    pub ambient: Color,
    pub sunlight: Color,
    pub fog: Color,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub fog_density: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Grid {
    pub x: i32,
    pub y: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct PathGrid {
    pub grid: Grid,
    pub flags: u16,
    pub points: u16,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Weather {
    pub clear: u8,
    pub cloudy: u8,
    pub foggy: u8,
    pub overcast: u8,
    pub rain: u8,
    pub thunder: u8,
    pub ash: u8,
    pub blight: u8,
    pub ex: Option<WeatherEx>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct WeatherEx {
    pub snow: u8,
    pub blizzard: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct SoundChance {
    #[serde(with = "string_32")]
    pub sound_id: String,
    pub chance: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Potion {
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub weight: f32,
    pub value: u32,
    #[serde(with = "bool_u32")]
    pub auto_calculate_value: bool,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum Specialization {
        Combat = 0,
        Magic = 1,
        Stealth = 2,
    }
}

enum_serde!(Specialization, "specialization", u32, to_u32, as from_u32, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Class {
    pub primary_attribute_1: Attribute,
    pub primary_attribute_2: Attribute,
    pub specialization: Specialization,
    pub minor_skill_1: Skill,
    pub major_skill_1: Skill,
    pub minor_skill_2: Skill,
    pub major_skill_2: Skill,
    pub minor_skill_3: Skill,
    pub major_skill_3: Skill,
    pub minor_skill_4: Skill,
    pub major_skill_4: Skill,
    pub minor_skill_5: Skill,
    pub major_skill_5: Skill,
    #[serde(with="bool_u32")]
    pub playable: bool,
    pub auto_calc_services: Services,
}

pub_bitflags_display!(RaceFlags, u32,
    PLAYABLE = 0x01,
    BEAST_RACE = 0x02
);

enum_serde!(RaceFlags, "race flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct RaceAttribute {
    pub male: u32,
    pub female: u32,
}

impl Index<Sex> for RaceAttribute {
    type Output = u32;

    fn index(&self, index: Sex) -> &Self::Output {
        match index {
            Sex::Male => &self.male,
            Sex::Female => &self.female,
        }
    }
}

impl IndexMut<Sex> for RaceAttribute {
    fn index_mut(&mut self, index: Sex) -> &mut Self::Output {
        match index {
            Sex::Male => &mut self.male,
            Sex::Female => &mut self.female,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct RaceParameter {
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub male: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub female: f32,
}

impl Index<Sex> for RaceParameter {
    type Output = f32;

    fn index(&self, index: Sex) -> &Self::Output {
        match index {
            Sex::Male => &self.male,
            Sex::Female => &self.female,
        }
    }
}

impl IndexMut<Sex> for RaceParameter {
    fn index_mut(&mut self, index: Sex) -> &mut Self::Output {
        match index {
            Sex::Male => &mut self.male,
            Sex::Female => &mut self.female,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Race {
    #[serde(with="skill_option_i32")]
    pub skill_1: Either<Option<i32>, Skill>,
    pub skill_1_bonus: u32,
    #[serde(with="skill_option_i32")]
    pub skill_2: Either<Option<i32>, Skill>,
    pub skill_2_bonus: u32,
    #[serde(with="skill_option_i32")]
    pub skill_3: Either<Option<i32>, Skill>,
    pub skill_3_bonus: u32,
    #[serde(with="skill_option_i32")]
    pub skill_4: Either<Option<i32>, Skill>,
    pub skill_4_bonus: u32,
    #[serde(with="skill_option_i32")]
    pub skill_5: Either<Option<i32>, Skill>,
    pub skill_5_bonus: u32,
    #[serde(with="skill_option_i32")]
    pub skill_6: Either<Option<i32>, Skill>,
    pub skill_6_bonus: u32,
    #[serde(with="skill_option_i32")]
    pub skill_7: Either<Option<i32>, Skill>,
    pub skill_7_bonus: u32,
    pub attributes: Attributes<RaceAttribute>,
    pub height: RaceParameter,
    pub weight: RaceParameter,
    pub flags: RaceFlags
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Sound {
    pub volume: u8,
    pub range_min: u8,
    pub range_max: u8,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum SoundGen {
        Left = 0,
        Right = 1,
        SwimLeft = 2,
        SwimRight = 3,
        Moan = 4,
        Roar = 5,
        Scream = 6,
        Land = 7
    }
}

enum_serde!(SoundGen, "sound gen", u32, to_u32, as from_u32, Unsigned, u64);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    pub enum Sex {
        Male = 0,
        Female = 1
    }
}

enum_serde!(Sex, "sex", u8, to_u8, as from_u8, Unsigned, u64);

mod sex_option_i8 {
    use serde::{Serializer, Deserializer};
    use either::{Either};
    use crate::field::Sex;
    use num_traits::cast::{ToPrimitive, FromPrimitive};
    use crate::serde_helpers::*;

    pub fn serialize<S>(&v: &Either<Option<i8>, Sex>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        serialize_option_index("sex", -1, Sex::from_i8, |x| x.to_i8().unwrap(), v, serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Either<Option<i8>, Sex>, D::Error> where D: Deserializer<'de> {
        deserialize_option_index(-1, Sex::from_i8, deserializer)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Info {
    #[serde(with="dialog_type_u32")]
    pub dialog_type: DialogType,
    pub disp_index: u32,
    #[serde(with="option_i8")]
    pub rank: Option<i8>,
    #[serde(with="sex_option_i8")]
    pub sex: Either<Option<i8>, Sex>,
    #[serde(with="option_i8")]
    pub pc_rank: Option<i8>,
    pub padding: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Rank {
    pub attribute_1: u32,
    pub attribute_2: u32,
    pub primary_skill: u32,
    pub favored_skill: u32,
    pub reputation: u32
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Faction {
    pub favored_attribute_1: Attribute,
    pub favored_attribute_2: Attribute,
    pub ranks: [Rank; 10],
    #[serde(with="skill_option_i32")]
    pub favored_skill_1: Either<Option<i32>, Skill>,
    #[serde(with="skill_option_i32")]
    pub favored_skill_2: Either<Option<i32>, Skill>,
    #[serde(with="skill_option_i32")]
    pub favored_skill_3: Either<Option<i32>, Skill>,
    #[serde(with="skill_option_i32")]
    pub favored_skill_4: Either<Option<i32>, Skill>,
    #[serde(with="skill_option_i32")]
    pub favored_skill_5: Either<Option<i32>, Skill>,
    #[serde(with="skill_option_i32")]
    pub favored_skill_6: Either<Option<i32>, Skill>,
    #[serde(with="skill_option_i32")]
    pub favored_skill_7: Either<Option<i32>, Skill>,
    #[serde(with="bool_u32")]
    pub hidden_from_pc: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct SkillMetadata {
    pub governing_attribute: Attribute,
    pub specialization: Specialization,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub use_value_1: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub use_value_2: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub use_value_3: f32,
    #[derivative(PartialEq(compare_with = "eq_f32"))]
    #[serde(with = "float_32")]
    pub use_value_4: f32,
}

macro_rules! define_field {
    ($($variant:ident($(#[derivative(PartialEq(compare_with=$a:literal))])? $from:ty),)*) => {
        #[derive(Debug, Clone)]
        #[derive(Derivative)]
        #[derivative(PartialEq="feature_allow_slow_enum", Eq)]
        pub enum Field {
            None,
            $($variant($(#[derivative(PartialEq(compare_with=$a))])? $from)),*
        }
        
        $(
        impl From<$from> for Field {
            fn from(v: $from) -> Self { Field::$variant(v) }
        }
        )*
    }
}

define_field!(
    Ai(Ai),
    AiActivate(AiActivate),
    AiTarget(AiTarget),
    AiTravel(AiTravel),
    AiWander(AiWander),
    Apparatus(Apparatus),
    Armor(Armor),
    BipedObject(BipedObject),
    BodyPart(BodyPart),
    Book(Book),
    Cell(Cell),
    Class(Class),
    Clothing(Clothing),
    Color(Color),
    ContainerFlags(ContainerFlags),
    Creature(Creature),
    CreatureFlags(FlagsAndBlood<CreatureFlags>),
    DialogType(DialogType),
    Effect(Effect),
    EffectIndex(EffectIndex),
    EffectMetadata(EffectMetadata),
    Enchantment(Enchantment),
    F32(#[derivative(PartialEq(compare_with="eq_f32"))] f32),
    F32List(#[derivative(PartialEq(compare_with="eq_f32_list"))] Vec<f32>),
    Faction(Faction),
    FileMetadata(FileMetadata),
    Grid(Grid),
    I16(i16),
    I16List(Vec<i16>),
    I32(i32),
    I32List(Vec<i32>),
    I64(i64),
    Info(Info),
    Ingredient(Ingredient),
    Interior(Interior),
    Item(Item),
    Light(Light),
    MiscItem(MiscItem),
    Npc(Npc),
    NpcFlags(FlagsAndBlood<NpcFlags>),
    NpcState(NpcState),
    PathGrid(PathGrid),
    Position(Position),
    Potion(Potion),
    Race(Race),
    ScriptMetadata(ScriptMetadata),
    ScriptVars(ScriptVars),
    Skill(Skill),
    SkillMetadata(SkillMetadata),
    Sound(Sound),
    SoundChance(SoundChance),
    SoundGen(SoundGen),
    Spell(Spell),
    String(String),
    StringList(Vec<String>),
    StringZ(StringZ),
    StringZList(StringZList),
    Tool(Tool),
    U8(u8),
    U8List(Vec<u8>),
    Weapon(Weapon),
    Weather(Weather),
);

impl From<()> for Field {
    fn from(_: ()) -> Self { Field::None }
}

fn allow_fit(record_tag: Tag, field_tag: Tag) -> bool {
    match (record_tag, field_tag) {
        (_, AI_A) => true,
        (_, AI_E) => true,
        (_, AI_F) => true,
        (ARMO, BNAM) => true,
        (BODY, BNAM) => true,
        (CLOT, BNAM) => true,
        (INFO, BNAM) => true,
        (ARMO, CNAM) => true,
        (SSCR, DATA) => true,
        (BSGN, DESC) => true,
        (ACTI, FNAM) => true,
        (TES3, HEDR) => true,
        (JOUR, NAME) => true,
        (SSCR, NAME) => true,
        (INFO, NNAM) => true,
        (INFO, PNAM) => true,
        (FACT, RNAM) => true,
        (_, SCTX) => true,
        (REGN, SNAM) => true,
        (BOOK, TEXT) => true,
        _ => false
    }
}

impl Field {
    pub fn fit(&mut self, record_tag: Tag, field_tag: Tag) {
        if !allow_fit(record_tag, field_tag) { return; }
        match FieldType::from_tags(record_tag, field_tag) {
            FieldType::FileMetadata => {
                if let Field::FileMetadata(v) = self {
                    v.author.find('\0').map(|i| v.author.truncate(i));
                    let mut d = v.description.join(Newline::Dos.as_str());
                    d.find('\0').map(|i| d.truncate(i));
                    v.description = d.split(Newline::Dos.as_str()).map(String::from).collect();
                } else {
                    panic!("invalid field type")
                }
            },
            FieldType::String(_) => {
                if let Field::String(v) = self {
                    v.find('\0').map(|i| v.truncate(i));
                } else {
                    panic!("invalid field type")
                }
            },
            FieldType::SoundChance => {
                if let Field::SoundChance(v) = self {
                    v.sound_id.find('\0').map(|i| v.sound_id.truncate(i));
                } else {
                    panic!("invalid field type")
                }
            },
            FieldType::Multiline(newline) => {
                if let Field::StringList(v) = self {
                    let mut s = v.join(newline.as_str());
                    s.find('\0').map(|i| s.truncate(i));
                    *v = s.split(newline.as_str()).map(String::from).collect();
                } else {
                    panic!("invalid field type")
                }
            },
            FieldType::StringZ => {
                if let Field::StringZ(v) = self {
                    v.string.find('\0').map(|i| v.string.truncate(i));
                    v.has_tail_zero = true;
                } else {
                    panic!("invalid field type")
                }
            },
            FieldType::StringZList => {
                if let Field::StringZList(v) = self {
                    v.has_tail_zero = true;
                } else {
                    panic!("invalid field type")
                }
            },
            FieldType::AiTarget => {
                if let Field::AiTarget(v) = self {
                    v.actor_id.find('\0').map(|i| v.actor_id.truncate(i));
                } else {
                    panic!("invalid field type")
                }
            },
            FieldType::AiActivate => {
                if let Field::AiActivate(v) = self {
                    v.object_id.find('\0').map(|i| v.object_id.truncate(i));
                } else {
                    panic!("invalid field type")
                }
            },
            _ => ()
        }
    }
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
    
    #[test]
    fn light_flags_from_str() {
        assert_eq!(
            LightFlags::from_str("DYNAMIC CAN_CARRY FIRE FLICKER_SLOW"),
            Ok(LightFlags::DYNAMIC | LightFlags::CAN_CARRY | LightFlags::FIRE | LightFlags::FLICKER_SLOW)
        );
    }
}
