use std::fmt::{self, Debug, Display};
use std::str::FromStr;
use std::mem::{transmute};
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::de::{self, Unexpected, VariantAccess};
use serde::de::Error as de_Error;
use either::{Either, Left, Right};

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
    #[repr(u32)]
    pub enum FileType {
        ESP = 0,
        ESM = 1,
        ESS = 32
    }
}

enum_serde!(FileType, "file type", u32, to_u32, as from_u32);

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

enum_serde!(DialogType, "dialog type", u8, to_u8, as from_u8);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug)]
    #[repr(u32)]
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

enum_serde!(EffectRange, "effect range", u32, to_u32, as from_u32);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum FieldType {
    Binary,
    String(Option<u32>),
    StringZ,
    Multiline(Newline),
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
    NpcState,
    Effect,
    SpellMetadata,
    Ai,
    AiWander,
    AiTravel,
    AiTarget,
    AiActivate,
    NpcFlags,
    CreatureFlags,
    Book,
    ContainerFlags,
    Creature,
    Light,
    MiscItem,
    Apparatus,
    Weapon,
    Armor,
    BipedObject,
    BodyPart,
    Clothing,
    Enchantment,
    Tool,
    RepairItem,
    Position,
    PositionOrCell,
    Grid,
    PathGrid,
}

impl FieldType {
    pub fn from_tags(record_tag: Tag, field_tag: Tag) -> FieldType {
        match (record_tag, field_tag) {
            (APPA, AADT) => FieldType::Apparatus,
            (INFO, ACDT) => FieldType::String(None),
            (CELL, ACTN) => FieldType::Int,
            (_, AI_A) => FieldType::AiActivate,
            (_, AI_E) => FieldType::AiTarget,
            (_, AI_F) => FieldType::AiTarget,
            (_, AI_T) => FieldType::AiTravel,
            (_, AI_W) => FieldType::AiWander,
            (_, AIDT) => FieldType::Ai,
            (FACT, ANAM) => FieldType::String(None),
            (_, ANAM) => FieldType::StringZ,
            (ARMO, AODT) => FieldType::Armor,
            (_, ASND) => FieldType::StringZ,
            (_, AVFX) => FieldType::StringZ,
            (BOOK, BKDT) => FieldType::Book,
            (ARMO, BNAM) => FieldType::String(None),
            (BODY, BNAM) => FieldType::String(None),
            (CLOT, BNAM) => FieldType::String(None),
            (INFO, BNAM) => FieldType::Multiline(Newline::Dos),
            (PCDT, BNAM) => FieldType::String(None),
            (_, BNAM) => FieldType::StringZ,
            (_, BSND) => FieldType::StringZ,
            (_, BVFX) => FieldType::StringZ,
            (BODY, BYDT) => FieldType::BodyPart,
            (ARMO, CNAM) => FieldType::String(None),
            (CLOT, CNAM) => FieldType::String(None),
            (KLST, CNAM) => FieldType::Int,
            (REGN, CNAM) => FieldType::Int,
            (_, CNAM) => FieldType::StringZ,
            (CONT, CNDT) => FieldType::Float,
            (_, CSND) => FieldType::StringZ,
            (CLOT, CTDT) => FieldType::Clothing,
            (_, CVFX) => FieldType::StringZ,
            (CELL, DATA) => FieldType::PositionOrCell,
            (DIAL, DATA) => FieldType::DialogMetadata,
            (LAND, DATA) => FieldType::Int,
            (LEVC, DATA) => FieldType::Int,
            (LEVI, DATA) => FieldType::Int,
            (LTEX, DATA) => FieldType::StringZ,
            (PGRD, DATA) => FieldType::PathGrid,
            (SSCR, DATA) => FieldType::String(None),
            (TES3, DATA) => FieldType::Long,
            (QUES, DATA) => FieldType::String(None),
            (DIAL, DELE) => FieldType::Int,
            (BSGN, DESC) => FieldType::StringZ,
            (_, DESC) => FieldType::String(None),
            (_, DNAM) => FieldType::StringZ,
            (_, DODT) => FieldType::Position,
            (ALCH, ENAM) => FieldType::Effect,
            (ENCH, ENAM) => FieldType::Effect,
            (PCDT, ENAM) => FieldType::Long,
            (SPEL, ENAM) => FieldType::Effect,
            (_, ENAM) => FieldType::StringZ,
            (ENCH, ENDT) => FieldType::Enchantment,
            (CELL, FGTN) => FieldType::String(None),
            (CONT, FLAG) => FieldType::ContainerFlags,
            (CREA, FLAG) => FieldType::CreatureFlags,
            (NPC_, FLAG) => FieldType::NpcFlags,
            (_, FLAG) => FieldType::Int,
            (_, FLTV) => FieldType::Float,
            (GLOB, FNAM) => FieldType::String(None),
            (PCDT, FNAM) => FieldType::Binary,
            (_, FNAM) => FieldType::StringZ,
            (CELL, FRMR) => FieldType::Int,
            (TES3, HEDR) => FieldType::FileMetadata,
            (_, HSND) => FieldType::StringZ,
            (_, HVFX) => FieldType::StringZ,
            (_, INAM) => FieldType::StringZ,
            (ARMO, INDX) => FieldType::BipedObject,
            (CLOT, INDX) => FieldType::BipedObject,
            (_, INDX) => FieldType::Int,
            (CELL, INTV) => FieldType::Float,
            (LAND, INTV) => FieldType::Grid,
            (LEVC, INTV) => FieldType::Short,
            (LEVI, INTV) => FieldType::Short,
            (_, INTV) => FieldType::Int,
            (INGR, IRDT) => FieldType::Ingredient,
            (_, ITEX) => FieldType::StringZ,
            (PCDT, KNAM) => FieldType::Binary,
            (_, KNAM) => FieldType::StringZ,
            (LIGH, LHDT) => FieldType::Light,
            (PCDT, LNAM) => FieldType::Long,
            (LOCK, LKDT) => FieldType::Tool,
            (CELL, LSHN) => FieldType::String(None),
            (CELL, LSTN) => FieldType::String(None),
            (_, LVCR) => FieldType::Byte,
            (FMAP, MAPD) => FieldType::Compressed,
            (FMAP, MAPH) => FieldType::Long,
            (TES3, MAST) => FieldType::StringZ,
            (MISC, MCDT) => FieldType::MiscItem,
            (PCDT, MNAM) => FieldType::String(None),
            (CELL, MNAM) => FieldType::Byte,
            (_, MODL) => FieldType::StringZ,
            (CELL, NAM0) => FieldType::Int,
            (SPLM, NAM0) => FieldType::Byte,
            (CELL, NAM5) => FieldType::Int,
            (CELL, NAM9) => FieldType::Int,
            (PCDT, NAM9) => FieldType::Int,
            (GMST, NAME) => FieldType::String(None),
            (INFO, NAME) => FieldType::String(None),
            (JOUR, NAME) => FieldType::Multiline(Newline::Unix),
            (SPLM, NAME) => FieldType::Int,
            (SSCR, NAME) => FieldType::String(None),
            (_, NAME) => FieldType::StringZ,
            (_, ND3D) => FieldType::Byte,
            (LEVC, NNAM) => FieldType::Byte,
            (LEVI, NNAM) => FieldType::Byte,
            (_, NNAM) => FieldType::StringZ,
            (_, NPCO) => FieldType::Item,
            (CREA, NPDT) => FieldType::Creature,
            (NPC_, NPDT) => FieldType::Npc,
            (NPCC, NPDT) => FieldType::NpcState,
            (_, NPCS) => FieldType::String(Some(32)),
            (_, ONAM) => FieldType::StringZ,
            (PROB, PBDT) => FieldType::Tool,
            (_, PGRC) => FieldType::Compressed,
            (_, PGRP) => FieldType::Compressed,
            (PCDT, PNAM) => FieldType::Binary,
            (_, PNAM) => FieldType::StringZ,
            (_, PTEX) => FieldType::StringZ,
            (_, RGNN) => FieldType::StringZ,
            (REPA, RIDT) => FieldType::RepairItem,
            (FACT, RNAM) => FieldType::String(Some(32)),
            (SCPT, RNAM) => FieldType::Int,
            (_, RNAM) => FieldType::StringZ,
            (SCPT, SCHD) => FieldType::ScriptMetadata,
            (_, SCRI) => FieldType::StringZ,
            (_, SCTX) => FieldType::Multiline(Newline::Dos),
            (SCPT, SCVR) => FieldType::StringZList,
            (_, SCVR) => FieldType::String(None),
            (PCDT, SNAM) => FieldType::Binary,
            (REGN, SNAM) => FieldType::Binary,
            (_, SNAM) => FieldType::StringZ,
            (SPEL, SPDT) => FieldType::SpellMetadata,
            (_, STRV) => FieldType::String(None),
            (BOOK, TEXT) => FieldType::Multiline(Newline::Dos),
            (_, TEXT) => FieldType::StringZ,
            (_, TNAM) => FieldType::StringZ,
            (_, VCLR) => FieldType::Compressed,
            (_, VHGT) => FieldType::Compressed,
            (_, VNML) => FieldType::Compressed,
            (_, VTEX) => FieldType::Compressed,
            (_, WHGT) => FieldType::Int,
            (_, WIDX) => FieldType::Long,
            (_, WNAM) => FieldType::Compressed,
            (WEAP, WPDT) => FieldType::Weapon,
            (_, XCHG) => FieldType::Float,
            (_, XHLT) => FieldType::Int,
            (_, XIDX) => FieldType::Int,
            (SPLM, XNAM) => FieldType::Byte,
            (_, XSCL) => FieldType::Float,
            (_, XSOL) => FieldType::StringZ,
            (CELL, ZNAM) => FieldType::Byte,
            _ => FieldType::Binary
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
    pub effects: [i32; 4],
    pub skills: [i32; 4],
    pub attributes: [i32; 4]
}

fn eq_f32(a: &f32, b: &f32) -> bool {
    let a: u32 = unsafe { transmute(*a) };
    let b: u32 = unsafe { transmute(*b) };
    a == b
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

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
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
    pub effect_id: i16,
    pub skill: i8,
    pub attribute: i8,
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
    #[repr(u32)]
    pub enum SpellType {
        Spell = 0,
        Ability = 1,
        Blight = 2,
        Disease = 3,
        Curse = 4,
        Power = 5
    }
}

enum_serde!(SpellType, "spell type", u32, to_u32, as from_u32);

pub_bitflags_display!(SpellFlags, u32, AUTO_CALCULATE_COST = 1, PC_START = 2, ALWAYS_SUCCEEDS = 4);

enum_serde!(SpellFlags, "spell flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct SpellMetadata {
    #[serde(rename="type")]
    pub spell_type: SpellType,
    pub cost: u32,
    pub flags: SpellFlags
}

pub_bitflags_display!(AiServices, u32, [
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
    MISCELLANEOUS  = 0x00000400
], [
    POTIONS = 0x00002000,
    SPELLS = 0x00000800,
    MAGIC_ITEMS = 0x00001000,
    TRAINING = 0x00004000,
    SPELLMAKING = 0x00008000,
    ENCHANTING = 0x00010000,
    REPAIR = 0x00020000,
    _80000 = 0x00080000,
    _200000 = 0x00200000,
    _400000 = 0x00400000,
    _800000 = 0x00800000,
    _1000000 = 0x01000000
]);

enum_serde!(AiServices, "AI services", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Ai {
    pub hello: u16,
    pub fight: u8,
    pub flee: u8,
    pub alarm: u8,
    pub padding_8: u8,
    pub padding_16: u16,
    pub services: AiServices
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct AiWander {
    pub distance: u16,
    pub duration: u16,
    pub time_of_day: u8,
    pub idle: [u8; 8],
    pub repeat: u8
}

pub_bitflags_display!(AiTravelFlags, u32,
    RESET = 0x0100
);

enum_serde!(AiTravelFlags, "AI travel flags", u32, bits, try from_bits, Unsigned, u64, ^0x0001);

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
    pub reset: u16
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct AiActivate {
    #[serde(with = "string_32")]
    pub object_id: String,
    pub reset: u8
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(u8)]
    pub enum Blood {
        Default = 0,
        Skeleton = 4,
        MetalSparks = 8,
    }
}

enum_serde!(Blood, "blood", u8, to_u8, as from_u8);

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

#[derive(Debug, Clone, Serialize, Deserialize, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct Book {
    #[derivative(PartialEq(compare_with="eq_f32"))]
    #[serde(with="float_32")]
    pub weight: f32,
    pub value: u32,
    pub scroll: u32,
    pub skill: i32,
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
    #[repr(u32)]
    pub enum CreatureType {
        Creature = 0,
        Daedra  = 1,
        Undead = 2,
        Humanoid = 3
    }
}

enum_serde!(CreatureType, "creature type", u32, to_u32, as from_u32);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Creature {
    #[serde(rename="type")]
    pub creature_type: CreatureType,
    pub level: u32,
    pub strength: u32,
    pub intelligence: u32,
    pub willpower: u32,
    pub agility: u32,
    pub speed: u32,
    pub endurance: u32,
    pub personality: u32,
    pub luck: u32,
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

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub struct Color(pub u32);

impl Color {
    pub fn argb(a: u8, r: u8, g: u8, b: u8) -> Color {
        Color((r as u32) | ((g as u32) << 8) | ((b as u32) << 16) | ((a as u32) << 24))
    }
    pub fn rgb(r: u8, g: u8, b: u8) -> Color {
        Color((r as u32) | ((g as u32) << 8) | ((b as u32) << 16))
    }
    
    pub fn r(self) -> u8 { (self.0 & 0xFF) as u8 }
    pub fn g(self) -> u8 { ((self.0 >> 8) & 0xFF) as u8 }
    pub fn b(self) -> u8 { ((self.0 >> 16) & 0xFF) as u8 }
    pub fn a(self) -> u8 { ((self.0 >> 24) & 0xFF) as u8 }
}

impl Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{:02X}{:02X}{:02X}{:02X}", self.a(), self.r(), self.g(), self.b())
    }
}

impl FromStr for Color {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let with_alpha = match s.len() {
            9 => true,
            7 => false,
            _ => return Err(())
        };
        if &s[0..1] != "#" || &s[1..2] == "+" { return Err(()); }
        let x = u8::from_str_radix(&s[1..3], 16).map_err(|_| ())?;
        let y = u8::from_str_radix(&s[3..5], 16).map_err(|_| ())?;
        let z = u8::from_str_radix(&s[5..7], 16).map_err(|_| ())?;
        if with_alpha {
            let w = u8::from_str_radix(&s[7..9], 16).map_err(|_| ())?;
            Ok(Color::argb(x, y, z, w))
        } else {
            Ok(Color::rgb(x, y, z))
        }
    }
}

enum_serde!(Color, "ARGB color", u32, ());

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
    pub is_key: u32,
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(u32)]
    pub enum ApparatusType {
        MortarPestle = 0,
        Alembic  = 1,
        Calcinator = 2,
        Retort = 3
    }
}

enum_serde!(ApparatusType, "apparatus type", u32, to_u32, as from_u32);

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
    #[repr(u32)]
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

enum_serde!(ArmorType, "armor type", u32, to_u32, as from_u32);

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
    #[repr(u16)]
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

enum_serde!(WeaponType, "weapon type", u16, to_u16, as from_u16);

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
    #[repr(u8)]
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

enum_serde!(BodyPartKind, "body part kind", u8, to_u8, as from_u8);

pub_bitflags_display!(BodyPartFlags, u8,
    FEMALE = 0x01,
    NON_PLAYABLE = 0x02
);

enum_serde!(BodyPartFlags, "body part flags", u8, bits, try from_bits, Unsigned, u64);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(u8)]
    pub enum BodyPartType {
        Skin = 0,
        Clothing = 1,
        Armor = 2
    }
}

enum_serde!(BodyPartType, "body part type", u8, to_u8, as from_u8);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(u8)]
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

enum_serde!(BipedObject, "biped object", u8, to_u8, as from_u8);

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(u32)]
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

enum_serde!(ClothingType, "clothing type", u32, to_u32, as from_u32);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct BodyPart {
    pub kind: BodyPartKind,
    pub vampire: u8,
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
    #[repr(u32)]
    pub enum EnchantmentType {
        CastOnce = 0,
        WhenStrikes = 1,
        WhenUsed = 2,
        ConstantEffect = 3
    }
}

enum_serde!(EnchantmentType, "enchantment type", u32, to_u32, as from_u32);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Enchantment {
    #[serde(rename="type")]
    pub enchantment_type: EnchantmentType,
    pub cost: u32,
    pub charge_amount: u32,
    pub auto_calculate: u32,
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
    _40 = 0x40,
    BEHAVE_LIKE_EXTERIOR = 0x80
);

enum_serde!(CellFlags, "cell flags", u32, bits, try from_bits, Unsigned, u64);

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Cell {
    pub flags: CellFlags,
    pub grid: Grid,
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

macro_rules! define_field {
    ($($variant:ident($(#[derivative(PartialEq(compare_with=$a:literal))])? $from:ty),)*) => {
        #[derive(Debug, Clone)]
        #[derive(Derivative)]
        #[derivative(PartialEq="feature_allow_slow_enum", Eq)]
        pub enum Field {
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
    Binary(Vec<u8>),
    String(String),
    StringZ(StringZ),
    StringList(Vec<String>),
    StringZList(StringZList),
    Item(Item),
    Float(#[derivative(PartialEq(compare_with="eq_f32"))] f32),
    Int(i32),
    Short(i16),
    Long(i64),
    Byte(u8),
    Ingredient(Ingredient),
    ScriptMetadata(ScriptMetadata),
    DialogType(DialogType),
    FileMetadata(FileMetadata),
    NpcState(NpcState),
    Npc(Npc),
    Effect(Effect),
    SpellMetadata(SpellMetadata),
    Ai(Ai),
    AiWander(AiWander),
    AiTravel(AiTravel),
    AiTarget(AiTarget),
    AiActivate(AiActivate),
    NpcFlags(FlagsAndBlood<NpcFlags>),
    CreatureFlags(FlagsAndBlood<CreatureFlags>),
    Book(Book),
    ContainerFlags(ContainerFlags),
    Creature(Creature),
    Light(Light),
    MiscItem(MiscItem),
    Apparatus(Apparatus),
    Armor(Armor),
    Weapon(Weapon),
    BipedObject(BipedObject),
    BodyPart(BodyPart),
    Clothing(Clothing),
    Enchantment(Enchantment),
    Tool(Tool),
    Position(Position),
    Cell(Cell),
    Grid(Grid),
    PathGrid(PathGrid),
);

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
        (SSCR, NAME) => true,
        (_, SCTX) => true,
        (BOOK, TEXT) => true,
        (FACT, RNAM) => true,
        // TODO (JOUR, NAME)
        _ => false
    }
}

impl Field {
    pub fn fit(&mut self, record_tag: Tag, field_tag: Tag) {
        if !allow_fit(record_tag, field_tag) { return; }
        match FieldType::from_tags(record_tag, field_tag) {
            FieldType::String(_) => {
                if let Field::String(v) = self {
                    v.find('\0').map(|i| v.truncate(i));
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
}
