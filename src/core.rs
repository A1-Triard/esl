use either::{Either};
use std::fmt::{self, Display, Debug};
use std::str::{FromStr};
use ::nom::IResult;
use ::nom::branch::alt;
use ::nom::combinator::{value as nom_value, map, opt};
use ::nom::bytes::complete::tag as nom_tag;
use ::nom::multi::{fold_many0};
use ::nom::sequence::{preceded, terminated, pair};
use ::nom::bytes::complete::take_while;

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

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum StringCoerce {
    None,
    CutTailZeros,
    CutTailZerosExceptOne,
}

impl StringCoerce {
    pub fn coerce(self, s: &mut String) {
        if self == StringCoerce::None { return; }
        while !s.is_empty() && s.as_bytes()[s.len() - 1] == 0 {
            s.truncate(s.len() - 1);
        }
        if self == StringCoerce::CutTailZerosExceptOne {
            s.push('\0');
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum LinebreakStyle {
    Unix,
    Dos
}

impl LinebreakStyle {
    pub fn split(self, s: &str) -> std::str::Split<&'static str> {
        s.split(if self == LinebreakStyle::Unix { "\n" } else { "\r\n" })
    }
}

#[derive(Copy, Clone, Debug)]
pub enum FieldType {
    Binary,
    String(StringCoerce),
    Multiline(LinebreakStyle, StringCoerce),
    MultiString,
    Reference,
    FixedString(u32),
    Float,
    Int,
    Short,
    Long,
    Byte,
    Compressed,
    Ingredient,
    ScriptMetadata,
    DialogMetadata,
    DeletionMark,
    FileMetadata,
    Npc,
    SavedNpc,
    Effect,
}

impl FieldType {
    pub fn coerce(self, allow: bool) -> FieldType {
        if allow { return self; }
        match self {
            FieldType::String(_) => FieldType::String(StringCoerce::None),
            FieldType::Multiline(linebreaks, _) => FieldType::Multiline(linebreaks, StringCoerce::None),
            x => x
        }
    }
    
    pub fn from_tags(record_tag: Tag, field_tag: Tag) -> FieldType {
        match (record_tag, field_tag) {
            (INFO, ACDT) => FieldType::String(StringCoerce::None),
            (CELL, ACTN) => FieldType::Int,
            (NPC_, ANAM) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (_, ANAM) => FieldType::String(StringCoerce::None),
            (_, ASND) => FieldType::String(StringCoerce::None),
            (_, AVFX) => FieldType::String(StringCoerce::None),
            (ARMO, BNAM) => FieldType::String(StringCoerce::CutTailZeros),
            (BODY, BNAM) => FieldType::String(StringCoerce::CutTailZeros),
            (CELL, BNAM) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (CLOT, BNAM) => FieldType::String(StringCoerce::CutTailZeros),
            (CONT, BNAM) => FieldType::Multiline(LinebreakStyle::Dos, StringCoerce::CutTailZeros),
            (INFO, BNAM) => FieldType::Multiline(LinebreakStyle::Dos, StringCoerce::CutTailZeros),
            (NPC_, BNAM) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (PCDT, BNAM) => FieldType::String(StringCoerce::None),
            (REGN, BNAM) => FieldType::String(StringCoerce::None),
            (_, BNAM) => FieldType::Multiline(LinebreakStyle::Dos, StringCoerce::None),
            (_, BSND) => FieldType::String(StringCoerce::None),
            (_, BVFX) => FieldType::String(StringCoerce::None),
            (ARMO, CNAM) => FieldType::String(StringCoerce::CutTailZeros),
            (KLST, CNAM) => FieldType::Int,
            (NPC_, CNAM) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (REGN, CNAM) => FieldType::Int,
            (_, CNAM) => FieldType::String(StringCoerce::None),
            (_, CSND) => FieldType::String(StringCoerce::None),
            (_, CVFX) => FieldType::String(StringCoerce::None),
            (DIAL, DATA) => FieldType::DialogMetadata,
            (LAND, DATA) => FieldType::Int,
            (LEVC, DATA) => FieldType::Int,
            (LEVI, DATA) => FieldType::Int,
            (LTEX, DATA) => FieldType::String(StringCoerce::None),
            (SSCR, DATA) => FieldType::String(StringCoerce::CutTailZeros),
            (TES3, DATA) => FieldType::Long,
            (QUES, DATA) => FieldType::String(StringCoerce::None),
            (DIAL, DELE) => FieldType::DeletionMark,
            (_, DESC) => FieldType::String(StringCoerce::None),
            (_, DNAM) => FieldType::String(StringCoerce::None),
            (ALCH, ENAM) => FieldType::Effect,
            (ARMO, ENAM) => FieldType::String(StringCoerce::None),
            (ENCH, ENAM) => FieldType::Effect,
            (PCDT, ENAM) => FieldType::Long,
            (SPEL, ENAM) => FieldType::Effect,
            (CELL, FGTN) => FieldType::String(StringCoerce::None),
            (_, FLAG) => FieldType::Int,
            (_, FLTV) => FieldType::Float,
            (ACTI, FNAM) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (PCDT, FNAM) => FieldType::Binary,
            (RACE, FNAM) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (_, FNAM) => FieldType::String(StringCoerce::None),
            (CELL, FRMR) => FieldType::Int,
            (TES3, HEDR) => FieldType::FileMetadata,
            (_, HSND) => FieldType::String(StringCoerce::None),
            (_, HVFX) => FieldType::String(StringCoerce::None),
            (_, INAM) => FieldType::String(StringCoerce::None),
            (ARMO, INDX) => FieldType::Byte,
            (CLOT, INDX) => FieldType::Byte,
            (_, INDX) => FieldType::Int,
            (LAND, INTV) => FieldType::Long,
            (LEVC, INTV) => FieldType::Short,
            (LEVI, INTV) => FieldType::Short,
            (_, INTV) => FieldType::Int,
            (INGR, IRDT) => FieldType::Ingredient,
            (_, ITEX) => FieldType::String(StringCoerce::None),
            (NPC_, KNAM) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (PCDT, KNAM) => FieldType::Binary,
            (_, KNAM) => FieldType::String(StringCoerce::None),
            (PCDT, LNAM) => FieldType::Long,
            (CELL, LSHN) => FieldType::String(StringCoerce::None),
            (CELL, LSTN) => FieldType::String(StringCoerce::None),
            (_, LVCR) => FieldType::Byte,
            (FMAP, MAPD) => FieldType::Compressed,
            (FMAP, MAPH) => FieldType::Long,
            (TES3, MAST) => FieldType::String(StringCoerce::None),
            (PCDT, MNAM) => FieldType::String(StringCoerce::None),
            (CELL, MNAM) => FieldType::Byte,
            (LIGH, MODL) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (_, MODL) => FieldType::String(StringCoerce::None),
            (CELL, NAM0) => FieldType::Int,
            (SPLM, NAM0) => FieldType::Byte,
            (CELL, NAM5) => FieldType::Int,
            (CELL, NAM9) => FieldType::Int,
            (PCDT, NAM9) => FieldType::Int,
            (CELL, NAME) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (JOUR, NAME) => FieldType::Multiline(LinebreakStyle::Unix, StringCoerce::None),
            (SPLM, NAME) => FieldType::Int,
            (SSCR, NAME) => FieldType::String(StringCoerce::CutTailZeros),
            (_, NAME) => FieldType::String(StringCoerce::None),
            (_, ND3D) => FieldType::Byte,
            (INFO, NNAM) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (LEVC, NNAM) => FieldType::Byte,
            (LEVI, NNAM) => FieldType::Byte,
            (_, NNAM) => FieldType::String(StringCoerce::None),
            (_, NPCO) => FieldType::Reference,
            (NPC_, NPDT) => FieldType::Npc,
            (NPCC, NPDT) => FieldType::SavedNpc,
            (BSGN, NPCS) => FieldType::FixedString(32),
            (NPC_, NPCS) => FieldType::FixedString(32),
            (RACE, NPCS) => FieldType::FixedString(32),
            (_, NPCS) => FieldType::String(StringCoerce::None),
            (_, ONAM) => FieldType::String(StringCoerce::None),
            (INFO, PNAM) => FieldType::String(StringCoerce::CutTailZerosExceptOne),
            (PCDT, PNAM) => FieldType::Binary,
            (_, PNAM) => FieldType::String(StringCoerce::None),
            (_, PTEX) => FieldType::String(StringCoerce::None),
            (_, RGNN) => FieldType::String(StringCoerce::None),
            (FACT, RNAM) => FieldType::FixedString(32),
            (_, RNAM) => FieldType::String(StringCoerce::None),
            (SCPT, SCHD) => FieldType::ScriptMetadata,
            (_, SCRI) => FieldType::String(StringCoerce::None),
            (_, SCTX) => FieldType::Multiline(LinebreakStyle::Dos, StringCoerce::CutTailZeros),
            (SCPT, SCVR) => FieldType::MultiString,
            (_, SCVR) => FieldType::String(StringCoerce::None),
            (CELL, SLSD) => FieldType::Binary,
            (PCDT, SNAM) => FieldType::Binary,
            (REGN, SNAM) => FieldType::Binary,
            (_, SNAM) => FieldType::String(StringCoerce::None),
            (_, STRV) => FieldType::String(StringCoerce::None),
            (ALCH, TEXT) => FieldType::String(StringCoerce::None),
            (BOOK, TEXT) => FieldType::Multiline(LinebreakStyle::Dos, StringCoerce::CutTailZeros),
            (_, TEXT) => FieldType::Multiline(LinebreakStyle::Dos, StringCoerce::None),
            (_, TNAM) => FieldType::String(StringCoerce::None),
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
            (_, XSOL) => FieldType::String(StringCoerce::None),
            (SPLM, XNAM) => FieldType::Byte,
            (CELL, XSCL) => FieldType::Int,
            (CELL, ZNAM) => FieldType::Byte,
            _ => FieldType::Binary
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ingredient {
    pub weight: f32,
    pub value: u32,
    pub effects: [i32; 4],
    pub skills: [i32; 4],
    pub attributes: [i32; 4]
}

#[derive(Debug, Clone)]
pub struct ScriptMetadata {
    pub name: String,
    pub shorts: u32,
    pub longs: u32,
    pub floats: u32,
    pub data_size: u32,
    pub var_table_size: u32
}

#[derive(Debug, Clone)]
pub struct FileMetadata {
    pub version: u32,
    pub file_type: FileType,
    pub author: String,
    pub description: Vec<String>,
    pub records_count: u32
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct SavedNpc {
    pub disposition: i16,
    pub reputation: i16,
    pub index: u32,
}

#[derive(Debug, Clone)]
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
    pub fatigue: i16
}

#[derive(Debug, Clone)]
pub struct Npc {
    pub level: u16,
    pub disposition: i8,
    pub reputation: i8,
    pub rank: i8,
    pub gold: i32,
    pub characteristics: Either<u32, NpcCharacteristics>
}

#[derive(Debug, Clone)]
pub enum Field {
    Binary(Vec<u8>),
    String(String),
    Multiline(Vec<String>),
    MultiString(Vec<String>),
    Reference(i32, String),
    Float(f32),
    Int(i32),
    Short(i16),
    Long(i64),
    Byte(u8),
    Compressed(Vec<u8>),
    Ingredient(Ingredient),
    ScriptMetadata(ScriptMetadata),
    DialogMetadata(Either<u32, DialogType>),
    DeletionMark,
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

#[derive(Debug, Clone)]
pub struct Record {
    pub tag: Tag,
    pub flags: RecordFlags,
    pub fields: Vec<Field>,
}

#[cfg(test)]
mod tests {
    use crate::*;
    use num_traits::cast::FromPrimitive;
    use std::str::FromStr;
    use std::hash::Hash;
    use encoding::types::Encoding;
    use encoding::{DecoderTrap, EncoderTrap};
    use encoding::all::WINDOWS_1251;
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
    
    #[test]
    fn text_decoding() {
        assert_eq!(Ok("\u{402}".into()), WINDOWS_1251.decode(b"\x80", DecoderTrap::Strict));
        assert_eq!(Ok("\u{403}".into()), WINDOWS_1251.decode(b"\x81", DecoderTrap::Strict));
        assert_eq!(Ok("\u{201A}".into()), WINDOWS_1251.decode(b"\x82", DecoderTrap::Strict));
        assert_eq!(Ok("\u{453}".into()), WINDOWS_1251.decode(b"\x83", DecoderTrap::Strict));
        assert_eq!(Ok("\u{201E}".into()), WINDOWS_1251.decode(b"\x84", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2026}".into()), WINDOWS_1251.decode(b"\x85", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2020}".into()), WINDOWS_1251.decode(b"\x86", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2021}".into()), WINDOWS_1251.decode(b"\x87", DecoderTrap::Strict));
        assert_eq!(Ok("\u{20AC}".into()), WINDOWS_1251.decode(b"\x88", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2030}".into()), WINDOWS_1251.decode(b"\x89", DecoderTrap::Strict));
        assert_eq!(Ok("\u{409}".into()), WINDOWS_1251.decode(b"\x8A", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2039}".into()), WINDOWS_1251.decode(b"\x8B", DecoderTrap::Strict));
        assert_eq!(Ok("\u{40A}".into()), WINDOWS_1251.decode(b"\x8C", DecoderTrap::Strict));
        assert_eq!(Ok("\u{40C}".into()), WINDOWS_1251.decode(b"\x8D", DecoderTrap::Strict));
        assert_eq!(Ok("\u{40B}".into()), WINDOWS_1251.decode(b"\x8E", DecoderTrap::Strict));
        assert_eq!(Ok("\u{40F}".into()), WINDOWS_1251.decode(b"\x8F", DecoderTrap::Strict));
        assert_eq!(Ok("\u{452}".into()), WINDOWS_1251.decode(b"\x90", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2018}".into()), WINDOWS_1251.decode(b"\x91", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2019}".into()), WINDOWS_1251.decode(b"\x92", DecoderTrap::Strict));
        assert_eq!(Ok("\u{201C}".into()), WINDOWS_1251.decode(b"\x93", DecoderTrap::Strict));
        assert_eq!(Ok("\u{201D}".into()), WINDOWS_1251.decode(b"\x94", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2022}".into()), WINDOWS_1251.decode(b"\x95", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2013}".into()), WINDOWS_1251.decode(b"\x96", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2014}".into()), WINDOWS_1251.decode(b"\x97", DecoderTrap::Strict));
        assert_eq!(Ok("\u{98}".into()), WINDOWS_1251.decode(b"\x98", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2122}".into()), WINDOWS_1251.decode(b"\x99", DecoderTrap::Strict));
        assert_eq!(Ok("\u{459}".into()), WINDOWS_1251.decode(b"\x9A", DecoderTrap::Strict));
        assert_eq!(Ok("\u{203A}".into()), WINDOWS_1251.decode(b"\x9B", DecoderTrap::Strict));
        assert_eq!(Ok("\u{45A}".into()), WINDOWS_1251.decode(b"\x9C", DecoderTrap::Strict));
        assert_eq!(Ok("\u{45C}".into()), WINDOWS_1251.decode(b"\x9D", DecoderTrap::Strict));
        assert_eq!(Ok("\u{45B}".into()), WINDOWS_1251.decode(b"\x9E", DecoderTrap::Strict));
        assert_eq!(Ok("\u{45F}".into()), WINDOWS_1251.decode(b"\x9F", DecoderTrap::Strict));
        assert_eq!(Ok("\u{A0}".into()), WINDOWS_1251.decode(b"\xA0", DecoderTrap::Strict));
        assert_eq!(Ok("\u{40E}".into()), WINDOWS_1251.decode(b"\xA1", DecoderTrap::Strict));
        assert_eq!(Ok("\u{45E}".into()), WINDOWS_1251.decode(b"\xA2", DecoderTrap::Strict));
        assert_eq!(Ok("\u{408}".into()), WINDOWS_1251.decode(b"\xA3", DecoderTrap::Strict));
        assert_eq!(Ok("\u{A4}".into()), WINDOWS_1251.decode(b"\xA4", DecoderTrap::Strict));
        assert_eq!(Ok("\u{490}".into()), WINDOWS_1251.decode(b"\xA5", DecoderTrap::Strict));
        assert_eq!(Ok("\u{A6}".into()), WINDOWS_1251.decode(b"\xA6", DecoderTrap::Strict));
        assert_eq!(Ok("\u{A7}".into()), WINDOWS_1251.decode(b"\xA7", DecoderTrap::Strict));
        assert_eq!(Ok("\u{401}".into()), WINDOWS_1251.decode(b"\xA8", DecoderTrap::Strict));
        assert_eq!(Ok("\u{A9}".into()), WINDOWS_1251.decode(b"\xA9", DecoderTrap::Strict));
        assert_eq!(Ok("\u{404}".into()), WINDOWS_1251.decode(b"\xAA", DecoderTrap::Strict));
        assert_eq!(Ok("\u{AB}".into()), WINDOWS_1251.decode(b"\xAB", DecoderTrap::Strict));
        assert_eq!(Ok("\u{AC}".into()), WINDOWS_1251.decode(b"\xAC", DecoderTrap::Strict));
        assert_eq!(Ok("\u{AD}".into()), WINDOWS_1251.decode(b"\xAD", DecoderTrap::Strict));
        assert_eq!(Ok("\u{AE}".into()), WINDOWS_1251.decode(b"\xAE", DecoderTrap::Strict));
        assert_eq!(Ok("\u{407}".into()), WINDOWS_1251.decode(b"\xAF", DecoderTrap::Strict));
        assert_eq!(Ok("\u{B0}".into()), WINDOWS_1251.decode(b"\xB0", DecoderTrap::Strict));
        assert_eq!(Ok("\u{B1}".into()), WINDOWS_1251.decode(b"\xB1", DecoderTrap::Strict));
        assert_eq!(Ok("\u{406}".into()), WINDOWS_1251.decode(b"\xB2", DecoderTrap::Strict));
        assert_eq!(Ok("\u{456}".into()), WINDOWS_1251.decode(b"\xB3", DecoderTrap::Strict));
        assert_eq!(Ok("\u{491}".into()), WINDOWS_1251.decode(b"\xB4", DecoderTrap::Strict));
        assert_eq!(Ok("\u{B5}".into()), WINDOWS_1251.decode(b"\xB5", DecoderTrap::Strict));
        assert_eq!(Ok("\u{B6}".into()), WINDOWS_1251.decode(b"\xB6", DecoderTrap::Strict));
        assert_eq!(Ok("\u{B7}".into()), WINDOWS_1251.decode(b"\xB7", DecoderTrap::Strict));
        assert_eq!(Ok("\u{451}".into()), WINDOWS_1251.decode(b"\xB8", DecoderTrap::Strict));
        assert_eq!(Ok("\u{2116}".into()), WINDOWS_1251.decode(b"\xB9", DecoderTrap::Strict));
        assert_eq!(Ok("\u{454}".into()), WINDOWS_1251.decode(b"\xBA", DecoderTrap::Strict));
        assert_eq!(Ok("\u{BB}".into()), WINDOWS_1251.decode(b"\xBB", DecoderTrap::Strict));
        assert_eq!(Ok("\u{458}".into()), WINDOWS_1251.decode(b"\xBC", DecoderTrap::Strict));
        assert_eq!(Ok("\u{405}".into()), WINDOWS_1251.decode(b"\xBD", DecoderTrap::Strict));
        assert_eq!(Ok("\u{455}".into()), WINDOWS_1251.decode(b"\xBE", DecoderTrap::Strict));
        assert_eq!(Ok("\u{457}".into()), WINDOWS_1251.decode(b"\xBF", DecoderTrap::Strict));
    }

    #[test]
    fn text_encoding() {
        assert_eq!(Ok(vec![0x80]), WINDOWS_1251.encode("\u{402}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x81]), WINDOWS_1251.encode("\u{403}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x82]), WINDOWS_1251.encode("\u{201A}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x83]), WINDOWS_1251.encode("\u{453}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x84]), WINDOWS_1251.encode("\u{201E}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x85]), WINDOWS_1251.encode("\u{2026}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x86]), WINDOWS_1251.encode("\u{2020}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x87]), WINDOWS_1251.encode("\u{2021}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x88]), WINDOWS_1251.encode("\u{20AC}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x89]), WINDOWS_1251.encode("\u{2030}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x8A]), WINDOWS_1251.encode("\u{409}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x8B]), WINDOWS_1251.encode("\u{2039}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x8C]), WINDOWS_1251.encode("\u{40A}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x8D]), WINDOWS_1251.encode("\u{40C}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x8E]), WINDOWS_1251.encode("\u{40B}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x8F]), WINDOWS_1251.encode("\u{40F}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x90]), WINDOWS_1251.encode("\u{452}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x91]), WINDOWS_1251.encode("\u{2018}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x92]), WINDOWS_1251.encode("\u{2019}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x93]), WINDOWS_1251.encode("\u{201C}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x94]), WINDOWS_1251.encode("\u{201D}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x95]), WINDOWS_1251.encode("\u{2022}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x96]), WINDOWS_1251.encode("\u{2013}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x97]), WINDOWS_1251.encode("\u{2014}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x98]), WINDOWS_1251.encode("\u{98}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x99]), WINDOWS_1251.encode("\u{2122}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x9A]), WINDOWS_1251.encode("\u{459}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x9B]), WINDOWS_1251.encode("\u{203A}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x9C]), WINDOWS_1251.encode("\u{45A}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x9D]), WINDOWS_1251.encode("\u{45C}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x9E]), WINDOWS_1251.encode("\u{45B}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0x9F]), WINDOWS_1251.encode("\u{45F}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA0]), WINDOWS_1251.encode("\u{A0}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA1]), WINDOWS_1251.encode("\u{40E}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA2]), WINDOWS_1251.encode("\u{45E}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA3]), WINDOWS_1251.encode("\u{408}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA4]), WINDOWS_1251.encode("\u{A4}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA5]), WINDOWS_1251.encode("\u{490}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA6]), WINDOWS_1251.encode("\u{A6}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA7]), WINDOWS_1251.encode("\u{A7}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA8]), WINDOWS_1251.encode("\u{401}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xA9]), WINDOWS_1251.encode("\u{A9}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xAA]), WINDOWS_1251.encode("\u{404}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xAB]), WINDOWS_1251.encode("\u{AB}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xAC]), WINDOWS_1251.encode("\u{AC}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xAD]), WINDOWS_1251.encode("\u{AD}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xAE]), WINDOWS_1251.encode("\u{AE}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xAF]), WINDOWS_1251.encode("\u{407}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB0]), WINDOWS_1251.encode("\u{B0}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB1]), WINDOWS_1251.encode("\u{B1}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB2]), WINDOWS_1251.encode("\u{406}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB3]), WINDOWS_1251.encode("\u{456}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB4]), WINDOWS_1251.encode("\u{491}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB5]), WINDOWS_1251.encode("\u{B5}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB6]), WINDOWS_1251.encode("\u{B6}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB7]), WINDOWS_1251.encode("\u{B7}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB8]), WINDOWS_1251.encode("\u{451}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xB9]), WINDOWS_1251.encode("\u{2116}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xBA]), WINDOWS_1251.encode("\u{454}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xBB]), WINDOWS_1251.encode("\u{BB}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xBC]), WINDOWS_1251.encode("\u{458}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xBD]), WINDOWS_1251.encode("\u{405}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xBE]), WINDOWS_1251.encode("\u{455}", EncoderTrap::Strict));
        assert_eq!(Ok(vec![0xBF]), WINDOWS_1251.encode("\u{457}", EncoderTrap::Strict));
    }
    
    #[test]
    fn string_coerce() {
        let mut a = "a\0\0\0".into();
        StringCoerce::CutTailZerosExceptOne.coerce(&mut a);
        assert_eq!(a, "a\0");
        StringCoerce::CutTailZeros.coerce(&mut a);
        assert_eq!(a, "a");
    }
}
