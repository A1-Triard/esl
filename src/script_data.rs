use crate::field::{eq_f32, float_32};
use crate::code_page::CodePage;
use crate::serde_helpers::HexDump;
use educe::Educe;
use enum_derive_2018::{EnumDisplay, EnumFromStr};
use enumn::N;
use macro_attr_2018::macro_attr;
use nameof::name_of;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::{self, DeserializeSeed};
use serde::de::Error as de_Error;
use serde::ser::SerializeStruct;
use serde::ser::Error as ser_Error;
use serde_serialize_seed::{SerializeSeed, ValueWithSeed};
use std::fmt::{self, Formatter};

macro_attr! {
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, N, EnumDisplay!, EnumFromStr!)]
    #[repr(u8)]
    pub enum VarType {
        Short = b's',
        Float = b'f',
        Long = b'l',
        Mystery = b'V',
    }
}

enum_serde!(VarType, "var type", as u8, Unsigned, u64);

impl VarType {
    fn write(self, res: &mut Vec<u8>) {
        res.push(self as u8);
    }
}
 
#[derive(Debug, Clone, Copy, Educe, Serialize, Deserialize)]
#[educe(Eq, PartialEq)]
#[serde(tag="kind")]
pub enum Float {
    Val {
        #[educe(PartialEq(method="eq_f32"))]
        #[serde(with="float_32")]
        val: f32
    },
    Var { #[serde(rename="type")] var_type: VarType, index: u16 },
}

impl Float {
    fn write(self, res: &mut Vec<u8>) -> Result<(), String> {
        match self {
            Float::Var { var_type, index } => {
                var_type.write(res);
                write_u16(index, res);
                res.push(0);
            },
            Float::Val { val } => {
                let bits = val.to_bits();
                if VarType::n((bits & 0xFF) as u8).is_some() && (bits >> 24) == 0 {
                    return Err("denied float value '{val}/{bits:08X}'".to_string());
                }
                write_u32(bits, res);
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(tag="kind")]
pub enum Var {
    Local { owner: Option<String>, #[serde(rename="type")] var_type: VarType, index: u16 },
    Global { name: String },
}

fn write_char(code_page: CodePage, s: &str, res: &mut Vec<u8>) -> Result<(), String> {
    let bytes = code_page.encode(s).map_err(|e| match e {
        None => format!("the '{s}' string does not correspond to any source byte sequence"),
        Some(c) => format!("the '{c}' char is not representable in {code_page:?} code page")
    })?;
    if bytes.len() != 1 {
        return Err(format!("multiply chars '{s}"));
    }
    res.push(bytes[0]);
    Ok(())
}

fn write_str_raw(
    write_len: impl FnOnce(usize, &mut Vec<u8>) -> Result<(), ()>, code_page: CodePage, s: &str, res: &mut Vec<u8>
) -> Result<(), String> {
    let bytes = code_page.encode(s).map_err(|e| match e {
        None => format!("the '{s}' string does not correspond to any source byte sequence"),
        Some(c) => format!("the '{c}' char is not representable in {code_page:?} code page")
    })?;
    write_len(bytes.len(), res).map_err(|()| format!("too long string '{s}'"))?;
    res.extend_from_slice(&bytes);
    Ok(())
}

fn write_str(code_page: CodePage, s: &str, res: &mut Vec<u8>) -> Result<(), String> {
    write_str_raw(|len, res| {
        let len = len.try_into().map_err(|_| ())?;
        res.push(len);
        Ok(())
    }, code_page, s, res)
}

fn write_int_list(w: &[i16], res: &mut Vec<u8>) -> Result<(), String> {
    let len = w.len().try_into().map_err(|_| "too big word list".to_string())?;
    write_u16(len, res);
    for w in w {
        write_i16(*w, res);
    }
    Ok(())
}

fn write_str_list(code_page: CodePage, s: &[String], res: &mut Vec<u8>) -> Result<(), String> {
    let len = s.len().try_into().map_err(|_| "too big string list".to_string())?;
    res.push(len);
    for s in s {
        write_str(code_page, s, res)?;
    }
    Ok(())
}

fn write_var_list(code_page: CodePage, v: &[Var], res: &mut Vec<u8>) -> Result<(), String> {
    let len = v.len().try_into().map_err(|_| "too big var list".to_string())?;
    res.push(len);
    for v in v {
        v.write(code_page, res)?;
    }
    Ok(())
}

fn write_text(code_page: CodePage, s: &str, res: &mut Vec<u8>) -> Result<(), String> {
    write_str_raw(|len, res| {
        let len = len.try_into().map_err(|_| ())?;
        write_u16(len, res);
        Ok(())
    }, code_page, s, res)
}

fn write_u16(v: u16, res: &mut Vec<u8>) {
    res.push((v & 0xFF) as u8);
    res.push((v >> 8) as u8);
}

fn write_i16(v: i16, res: &mut Vec<u8>) {
    write_u16(v as u16, res);
}

fn write_u32(v: u32, res: &mut Vec<u8>) {
    res.push((v & 0xFF) as u8);
    res.push((v >> 8) as u8);
    res.push((v >> 16) as u8);
    res.push((v >> 24) as u8);
}

impl Var {
    fn write(&self, code_page: CodePage, res: &mut Vec<u8>) -> Result<(), String> {
        match self {
            &Var::Local { ref owner, var_type, index } => {
                if let Some(owner) = owner {
                    res.push(b'r');
                    write_str(code_page, owner, res)?;
                }
                var_type.write(res);
                write_u16(index, res);
            },
            Var::Global { name } => {
                res.push(b'G');
                write_str(code_page, name, res)?;
            }
        }
        Ok(())
    }
}
  
macro_attr! {
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, N, EnumDisplay!, EnumFromStr!)]
    #[repr(u16)]
    pub enum Func {
        End = 0x0101,
        Set = 0x0105,
        If = 0x0106,
        Else = 0x0107,
        ElseIf = 0x0108,
        EndIf = 0x0109,
        SetRef = 0x010C,
        Return = 0x0124,
        EnableRest = 0x013F,
        MessageBox = 0x1000,
        PlaySound = 0x1002,
        StreamMusic = 0x1003,
        Position = 0x1004,
        PositionCell = 0x1005,
        Move = 0x1006,
        Rotate = 0x1007,
        MoveWorld = 0x1008,
        RotateWorld = 0x1009,
        SetPos = 0x100B,
        SetAngle = 0x100D,
        SetAtStart = 0x1010,
        PlayGroup = 0x1014,
        LoopGroup = 0x1015,
        Activate = 0x1017,
        OnActivate = 0x1018,
        StartCombat = 0x1019,
        StopCombat = 0x101A,
        StartScript = 0x101B,
        StopScript = 0x101C,
        AddTopic = 0x1022,
        SetStrength = 0x1024,
        ModStrength = 0x1025,
        SetIntelligence = 0x1027,
        ModIntelligence = 0x1028,
        SetWillpower = 0x102A,
        ModWillpower = 0x102B,
        SetAgility = 0x102D,
        ModAgility = 0x102E,
        SetSpeed = 0x1030,
        ModSpeed = 0x1031,
        SetEndurance = 0x1033,
        ModEndurance = 0x1034,
        SetPersonality = 0x1036,
        ModPersonality = 0x1037,
        SetLuck = 0x1039,
        ModLuck = 0x103A,
        SetBlock = 0x103C,
        ModBlock = 0x103D,
        SetArmorer = 0x103F,
        ModArmorer = 0x1040,
        SetMediumArmor = 0x1042,
        ModMediumArmor = 0x1043,
        SetHeavyArmor = 0x1045,
        ModHeavyArmor = 0x1046,
        SetBluntWeapon = 0x1048,
        ModBluntWeapon = 0x1049,
        SetLongBlade = 0x104B,
        ModLongBlade = 0x104C,
        SetAxe = 0x104E,
        ModAxe = 0x104F,
        SetSpear = 0x1051,
        ModSpear = 0x1052,
        SetAthletics = 0x1054,
        ModAthletics = 0x1055,
        SetEnchant = 0x1057,
        SetDestruction = 0x105A,
        SetAlteration = 0x105D,
        SetIllusion = 0x1060,
        SetConjuration = 0x1063,
        SetMysticism = 0x1066,
        SetRestoration = 0x1069,
        ModRestoration = 0x106A,
        SetAlchemy = 0x106C,
        SetMarksman = 0x1081,
        ModMarksman = 0x1082,
        ModMercantile = 0x1085,
        SetHealth = 0x108D,
        ModHealth = 0x108E,
        SetMagicka = 0x1090,
        ModMagicka = 0x1091,
        SetFatigue = 0x1093,
        ModFatigue = 0x1094,
        ModReputation = 0x1097,
        SetDisposition = 0x1099,
        ModDisposition = 0x109A,
        SetPCCrimeLevel = 0x109C,
        Journal = 0x10CC,
        RaiseRank = 0x10CE,
        PCRaiseRank = 0x10D0,
        PCClearExpelled = 0x10D3,
        AddItem = 0x10D4,
        RemoveItem = 0x10D5,
        ModPCFacRep = 0x10D9,
        Enable = 0x10DA,
        Disable = 0x10DB,
        EnablePlayerControls = 0x10DD,
        DisablePlayerControls = 0x10DE,
        WakeUpPC = 0x10E1,
        EnablePlayerViewSwitch = 0x10E2,
        DisablePlayerViewSwitch = 0x10E3,
        ShowRestMenu = 0x10E5,
        PlaceAtPC = 0x10E6,
        Resurrect = 0x10E7,
        ForceGreeting = 0x10E8,
        RemoveSoulGem = 0x10EC,
        EnableTeleporting = 0x10EE,
        DisableTeleporting = 0x10EF,
        AiEscort = 0x10F4,
        AiFollow = 0x10F6,
        AiTravel = 0x10F8,
        AiWander = 0x10F9,
        SetFight = 0x1100,
        ModFight = 0x1101,
        SetFlee = 0x1103,
        SetHello = 0x1109,
        Drop = 0x110D,
        ModFactionReaction = 0x1111,
        EnableStatsMenu = 0x1117,
        EnableInventoryMenu = 0x1118,
        EnableMapMenu = 0x1119,
        EnableMagicMenu = 0x111A,
        Say = 0x111B,
        AddSpell = 0x111D,
        RemoveSpell = 0x111E,
        Cast = 0x1123,
        ChangeWeather = 0x1124,
        ModRegion = 0x1125,
        EnableNameMenu = 0x1126,
        EnableRaceMenu = 0x1127,
        EnableClassMenu = 0x1128,
        EnableBirthMenu = 0x1129,
        PlaySoundVP = 0x112B,
        PlaySound3D = 0x112C,
        PlaySound3DVP = 0x112D,
        PlayLoopSound3D = 0x112E,
        PlayLoopSound3DVP = 0x112F,
        FadeOut = 0x1130,
        FadeIn = 0x1131,
        ModCurrentHealth = 0x1132,
        ModCurrentFatigue = 0x1134,
        HurtStandingActor = 0x1135,
        Lock = 0x1136,
        Unlock = 0x1137,
        PCJoinFaction = 0x113B,
        EnablePlayerJumping = 0x113F,
        DisablePlayerJumping = 0x1140,
        EnableVanityMode = 0x114B,
        DisableVanityMode = 0x114C,
        PayFine = 0x114F,
        StopSound = 0x1151,
        ShowMap = 0x1152,
        PlayBink = 0x1155,
        EnablePlayerFighting = 0x1159,
        DisablePlayerFighting = 0x115A,
        EnablePlayerMagic = 0x115C,
        DisablePlayerMagic = 0x115D,
        DontSaveObject = 0x115F,
        EnableStatReviewMenu = 0x1160,
        ForceSneak = 0x1163,
        ClearForceSneak = 0x1164,
        Fall = 0x1166,
        MwseJumpShortZero = 0x380C,
        MwsePop = 0x380F,
        MwsePushS = 0x3813,
        MwseGetLocal = 0x3C00,
    }
}

enum_serde!(Func, "func", as u16, Unsigned, u64);

#[derive(Debug, Clone, Eq, PartialEq, Copy, Ord, PartialOrd, Hash)]
pub enum FuncParams {
    None,
    Byte,
    ByteStr,
    CharFloat,
    Float,
    FloatStr,
    Float3Byte,
    Float3IntListByte,
    Float4,
    Float4Str,
    Int,
    IntByte,
    Int2,
    Str,
    StrByte,
    StrByte8,
    StrFloat2,
    StrInt,
    StrIntFloatInt,
    StrIntFloat3Byte,
    StrInt2,
    StrText,
    Str2,
    Str2Int,
    Text,
    TextVarListStrList,
    VarStr,
}

impl Func {
    pub fn params(self) -> FuncParams {
        match self {
            Func::Activate => FuncParams::None,
            Func::AddItem => FuncParams::StrInt,
            Func::AddSpell => FuncParams::Str,
            Func::AddTopic => FuncParams::Str,
            Func::AiEscort => FuncParams::StrIntFloat3Byte,
            Func::AiFollow => FuncParams::StrIntFloat3Byte,
            Func::AiTravel => FuncParams::Float3Byte,
            Func::AiWander => FuncParams::Float3IntListByte,
            Func::Cast => FuncParams::Str2,
            Func::ChangeWeather => FuncParams::StrInt,
            Func::ClearForceSneak => FuncParams::None,
            Func::Disable => FuncParams::None,
            Func::DisablePlayerControls => FuncParams::None,
            Func::DisablePlayerFighting => FuncParams::None,
            Func::DisablePlayerJumping => FuncParams::None,
            Func::DisablePlayerMagic => FuncParams::None,
            Func::DisablePlayerViewSwitch => FuncParams::None,
            Func::DisableTeleporting => FuncParams::None,
            Func::DisableVanityMode => FuncParams::None,
            Func::DontSaveObject => FuncParams::None,
            Func::Drop => FuncParams::StrInt,
            Func::Else => FuncParams::Byte,
            Func::ElseIf => FuncParams::ByteStr,
            Func::Enable => FuncParams::None,
            Func::EnableBirthMenu => FuncParams::None,
            Func::EnableClassMenu => FuncParams::None,
            Func::EnableInventoryMenu => FuncParams::None,
            Func::EnableMagicMenu => FuncParams::None,
            Func::EnableMapMenu => FuncParams::None,
            Func::EnableNameMenu => FuncParams::None,
            Func::EnablePlayerControls => FuncParams::None,
            Func::EnablePlayerFighting => FuncParams::None,
            Func::EnablePlayerJumping => FuncParams::None,
            Func::EnablePlayerMagic => FuncParams::None,
            Func::EnablePlayerViewSwitch => FuncParams::None,
            Func::EnableRaceMenu => FuncParams::None,
            Func::EnableRest => FuncParams::None,
            Func::EnableStatReviewMenu => FuncParams::None,
            Func::EnableStatsMenu => FuncParams::None,
            Func::EnableTeleporting => FuncParams::None,
            Func::EnableVanityMode => FuncParams::None,
            Func::End => FuncParams::None,
            Func::EndIf => FuncParams::None,
            Func::FadeIn => FuncParams::Float,
            Func::FadeOut => FuncParams::Float,
            Func::Fall => FuncParams::None,
            Func::ForceGreeting => FuncParams::None,
            Func::ForceSneak => FuncParams::None,
            Func::HurtStandingActor => FuncParams::Float,
            Func::If => FuncParams::ByteStr,
            Func::Journal => FuncParams::StrInt2,
            Func::Lock => FuncParams::Int,
            Func::LoopGroup => FuncParams::Int2,
            Func::MessageBox => FuncParams::TextVarListStrList,
            Func::ModAgility => FuncParams::Float,
            Func::ModArmorer => FuncParams::Float,
            Func::ModAthletics => FuncParams::Float,
            Func::ModAxe => FuncParams::Float,
            Func::ModBlock => FuncParams::Float,
            Func::ModBluntWeapon => FuncParams::Float,
            Func::ModCurrentFatigue => FuncParams::Float,
            Func::ModCurrentHealth => FuncParams::Float,
            Func::ModDisposition => FuncParams::Float,
            Func::ModEndurance => FuncParams::Float,
            Func::ModFactionReaction => FuncParams::Str2Int,
            Func::ModFatigue => FuncParams::Float,
            Func::ModFight => FuncParams::Float,
            Func::ModHealth => FuncParams::Float,
            Func::ModHeavyArmor => FuncParams::Float,
            Func::ModIntelligence => FuncParams::Float,
            Func::ModLongBlade => FuncParams::Float,
            Func::ModLuck => FuncParams::Float,
            Func::ModMagicka => FuncParams::Float,
            Func::ModMarksman => FuncParams::Float,
            Func::ModMediumArmor => FuncParams::Float,
            Func::ModMercantile => FuncParams::Float,
            Func::ModPCFacRep => FuncParams::FloatStr,
            Func::ModPersonality => FuncParams::Float,
            Func::ModRegion => FuncParams::StrByte8,
            Func::ModReputation => FuncParams::Float,
            Func::ModRestoration => FuncParams::Float,
            Func::ModSpear => FuncParams::Float,
            Func::ModSpeed => FuncParams::Float,
            Func::ModStrength => FuncParams::Float,
            Func::ModWillpower => FuncParams::Float,
            Func::Move => FuncParams::CharFloat,
            Func::MoveWorld => FuncParams::CharFloat,
            Func::MwseGetLocal => FuncParams::None,
            Func::MwseJumpShortZero => FuncParams::Int,
            Func::MwsePop => FuncParams::Int,
            Func::MwsePushS => FuncParams::Int,
            Func::OnActivate => FuncParams::None,
            Func::PayFine => FuncParams::None,
            Func::PCClearExpelled => FuncParams::Str,
            Func::PCJoinFaction => FuncParams::Str,
            Func::PCRaiseRank => FuncParams::Str,
            Func::PlaceAtPC => FuncParams::StrIntFloatInt,
            Func::PlayBink => FuncParams::StrByte,
            Func::PlayGroup => FuncParams::IntByte,
            Func::PlayLoopSound3D => FuncParams::Str,
            Func::PlayLoopSound3DVP => FuncParams::StrFloat2,
            Func::PlaySound => FuncParams::Str,
            Func::PlaySoundVP => FuncParams::StrFloat2,
            Func::PlaySound3D => FuncParams::Str,
            Func::PlaySound3DVP => FuncParams::StrFloat2,
            Func::Position => FuncParams::Float4,
            Func::PositionCell => FuncParams::Float4Str,
            Func::RaiseRank => FuncParams::None,
            Func::RemoveItem => FuncParams::StrInt,
            Func::RemoveSoulGem => FuncParams::Str,
            Func::RemoveSpell => FuncParams::Str,
            Func::Resurrect => FuncParams::None,
            Func::Return => FuncParams::None,
            Func::Rotate => FuncParams::CharFloat,
            Func::RotateWorld => FuncParams::CharFloat,
            Func::Say => FuncParams::StrText,
            Func::Set => FuncParams::VarStr,
            Func::SetAgility => FuncParams::Float,
            Func::SetAlchemy => FuncParams::Float,
            Func::SetAlteration => FuncParams::Float,
            Func::SetAngle => FuncParams::CharFloat,
            Func::SetArmorer => FuncParams::Float,
            Func::SetAthletics => FuncParams::Float,
            Func::SetAtStart => FuncParams::None,
            Func::SetAxe => FuncParams::Float,
            Func::SetBlock => FuncParams::Float,
            Func::SetBluntWeapon => FuncParams::Float,
            Func::SetConjuration => FuncParams::Float,
            Func::SetDestruction => FuncParams::Float,
            Func::SetDisposition => FuncParams::Float,
            Func::SetEnchant => FuncParams::Float,
            Func::SetEndurance => FuncParams::Float,
            Func::SetFatigue => FuncParams::Float,
            Func::SetFight => FuncParams::Float,
            Func::SetFlee => FuncParams::Float,
            Func::SetHealth => FuncParams::Float,
            Func::SetHeavyArmor => FuncParams::Float,
            Func::SetHello => FuncParams::Float,
            Func::SetLongBlade => FuncParams::Float,
            Func::SetLuck => FuncParams::Float,
            Func::SetMagicka => FuncParams::Float,
            Func::SetMarksman => FuncParams::Float,
            Func::SetMysticism => FuncParams::Float,
            Func::SetPCCrimeLevel => FuncParams::Float,
            Func::SetPos => FuncParams::CharFloat,
            Func::SetRef => FuncParams::Str,
            Func::SetRestoration => FuncParams::Float,
            Func::SetSpear => FuncParams::Float,
            Func::SetSpeed => FuncParams::Float,
            Func::SetStrength => FuncParams::Float,
            Func::SetMediumArmor => FuncParams::Float,
            Func::SetIllusion => FuncParams::Float,
            Func::SetIntelligence => FuncParams::Float,
            Func::SetWillpower => FuncParams::Float,
            Func::SetPersonality => FuncParams::Float,
            Func::ShowMap => FuncParams::Str,
            Func::ShowRestMenu => FuncParams::None,
            Func::StartCombat => FuncParams::Str,
            Func::StartScript => FuncParams::Str,
            Func::StopCombat => FuncParams::None,
            Func::StopScript => FuncParams::Str,
            Func::StopSound => FuncParams::Str,
            Func::StreamMusic => FuncParams::Str,
            Func::Unlock => FuncParams::None,
            Func::WakeUpPC => FuncParams::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncArgs {
    None,
    Byte(u8),
    ByteStr(u8, String),
    CharFloat(String, Float),
    Float(Float),
    FloatStr(Float, String),
    Float3Byte([Float; 3], u8),
    Float3IntListByte([Float; 3], Vec<i16>, u8),
    Float4([Float; 4]),
    Float4Str([Float; 4], String),
    Int(i16),
    IntByte(i16, u8),
    Int2([i16; 2]),
    Str(String),
    StrByte(String, u8),
    StrByte8(String, [u8; 8]),
    StrFloat2(String, [Float; 2]),
    StrInt(String, i16),
    StrIntFloatInt(String, i16, Float, i16),
    StrIntFloat3Byte(String, i16, [Float; 3], u8),
    StrInt2(String, [i16; 2]),
    StrText(String, String),
    Str2([String; 2]),
    Str2Int([String; 2], i16),
    Text(String),
    TextVarListStrList(String, Vec<Var>, Vec<String>),
    VarStr(Var, String),
}

#[derive(Clone)]
pub struct FuncArgsSerde {
    pub params: FuncParams
}

impl SerializeSeed for FuncArgsSerde {
    type Value = FuncArgs;

    fn serialize<S: Serializer>(&self, value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if self.params != value.params() {
            return Err(S::Error::custom("stmt args params mismatch"));
        }
        match value {
            FuncArgs::None => ().serialize(serializer),
            FuncArgs::Byte(a1) => a1.serialize(serializer),
            FuncArgs::ByteStr(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::CharFloat(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::Float(a1) => a1.serialize(serializer),
            FuncArgs::FloatStr(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::Float3Byte(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::Float4(a1) => a1.serialize(serializer),
            FuncArgs::Float4Str(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::Float3IntListByte(a1, a2, a3) => (a1, a2, a3).serialize(serializer),
            FuncArgs::Int(a1) => a1.serialize(serializer),
            FuncArgs::IntByte(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::Int2(a1) => a1.serialize(serializer),
            FuncArgs::Str(a1) => a1.serialize(serializer),
            FuncArgs::StrByte(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::StrByte8(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::StrFloat2(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::StrInt(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::StrIntFloatInt(a1, a2, a3, a4) => (a1, a2, a3, a4).serialize(serializer),
            FuncArgs::StrIntFloat3Byte(a1, a2, a3, a4) => (a1, a2, a3, a4).serialize(serializer),
            FuncArgs::StrInt2(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::StrText(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::Str2(a1) => a1.serialize(serializer),
            FuncArgs::Str2Int(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::Text(a1) => a1.serialize(serializer),
            FuncArgs::TextVarListStrList(a1, a2, a3) => (a1, a2, a3).serialize(serializer),
            FuncArgs::VarStr(a1, a2) => (a1, a2).serialize(serializer),
        }
    }
}

impl<'de> DeserializeSeed<'de> for FuncArgsSerde {
    type Value = FuncArgs;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        Ok(match self.params {
            FuncParams::None => { <()>::deserialize(deserializer)?; FuncArgs::None },
            FuncParams::Byte => { let a1 = u8::deserialize(deserializer)?; FuncArgs::Byte(a1) },
            FuncParams::ByteStr => { let (a1, a2) = <(u8, String)>::deserialize(deserializer)?; FuncArgs::ByteStr(a1, a2) },
            FuncParams::CharFloat => {
                let (a1, a2) = <(String, Float)>::deserialize(deserializer)?;
                FuncArgs::CharFloat(a1, a2)
            },
            FuncParams::Float => { let a1 = Float::deserialize(deserializer)?; FuncArgs::Float(a1) },
            FuncParams::FloatStr => { let (a1, a2) = <(Float, String)>::deserialize(deserializer)?; FuncArgs::FloatStr(a1, a2) },
            FuncParams::Float3Byte => {
                let (a1, a2) = <([Float; 3], u8)>::deserialize(deserializer)?;
                FuncArgs::Float3Byte(a1, a2)
            },
            FuncParams::Float3IntListByte => {
                let (a1, a2, a3) = <([Float; 3], Vec<i16>, u8)>::deserialize(deserializer)?;
                FuncArgs::Float3IntListByte(a1, a2, a3)
            },
            FuncParams::Float4 => { let a1 = <[Float; 4]>::deserialize(deserializer)?; FuncArgs::Float4(a1) },
            FuncParams::Float4Str => {
                let (a1, a2) = <([Float; 4], String)>::deserialize(deserializer)?;
                FuncArgs::Float4Str(a1, a2)
            },
            FuncParams::Int => { let a1 = i16::deserialize(deserializer)?; FuncArgs::Int(a1) },
            FuncParams::IntByte => { let (a1, a2) = <(i16, u8)>::deserialize(deserializer)?; FuncArgs::IntByte(a1, a2) },
            FuncParams::Int2 => { let a1 = <[i16; 2]>::deserialize(deserializer)?; FuncArgs::Int2(a1) },
            FuncParams::Str => { let a1 = String::deserialize(deserializer)?; FuncArgs::Str(a1) },
            FuncParams::StrByte => { let (a1, a2) = <(String, u8)>::deserialize(deserializer)?; FuncArgs::StrByte(a1, a2) },
            FuncParams::StrByte8 => { let (a1, a2) = <(String, [u8; 8])>::deserialize(deserializer)?; FuncArgs::StrByte8(a1, a2) },
            FuncParams::StrFloat2 => {
                let (a1, a2) = <(String, [Float; 2])>::deserialize(deserializer)?; FuncArgs::StrFloat2(a1, a2)
            },
            FuncParams::StrInt => { let (a1, a2) = <(String, i16)>::deserialize(deserializer)?; FuncArgs::StrInt(a1, a2) },
            FuncParams::StrIntFloatInt => {
                let (a1, a2, a3, a4) = <(String, i16, Float, i16)>::deserialize(deserializer)?;
                FuncArgs::StrIntFloatInt(a1, a2, a3, a4)
            },
            FuncParams::StrIntFloat3Byte => {
                let (a1, a2, a3, a4) = <(String, i16, [Float; 3], u8)>::deserialize(deserializer)?;
                FuncArgs::StrIntFloat3Byte(a1, a2, a3, a4)
            },
            FuncParams::StrInt2 => {
                let (a1, a2) = <(String, [i16; 2])>::deserialize(deserializer)?; FuncArgs::StrInt2(a1, a2)
            },
            FuncParams::StrText => { let (a1, a2) = <(String, String)>::deserialize(deserializer)?; FuncArgs::StrText(a1, a2) },
            FuncParams::Str2 => { let a1 = <[String; 2]>::deserialize(deserializer)?; FuncArgs::Str2(a1) },
            FuncParams::Str2Int => { let (a1, a2) = <([String; 2], i16)>::deserialize(deserializer)?; FuncArgs::Str2Int(a1, a2) },
            FuncParams::Text => { let a1 = String::deserialize(deserializer)?; FuncArgs::Text(a1) },
            FuncParams::TextVarListStrList => {
                let (a1, a2, a3) = <(String, Vec<Var>, Vec<String>)>::deserialize(deserializer)?;
                FuncArgs::TextVarListStrList(a1, a2, a3)
            },
            FuncParams::VarStr => { let (a1, a2) = <(Var, String)>::deserialize(deserializer)?; FuncArgs::VarStr(a1, a2) },
        })
    }
}

impl FuncArgs {
    pub fn params(&self) -> FuncParams {
        match self {
            FuncArgs::None => FuncParams::None,
            FuncArgs::Byte(..) => FuncParams::Byte,
            FuncArgs::ByteStr(..) => FuncParams::ByteStr,
            FuncArgs::CharFloat(..) => FuncParams::CharFloat,
            FuncArgs::Float(..) => FuncParams::Float,
            FuncArgs::FloatStr(..) => FuncParams::FloatStr,
            FuncArgs::Float3Byte(..) => FuncParams::Float3Byte,
            FuncArgs::Float4(..) => FuncParams::Float4,
            FuncArgs::Float4Str(..) => FuncParams::Float4Str,
            FuncArgs::Float3IntListByte(..) => FuncParams::Float3IntListByte,
            FuncArgs::Int(..) => FuncParams::Int,
            FuncArgs::IntByte(..) => FuncParams::IntByte,
            FuncArgs::Int2(..) => FuncParams::Int2,
            FuncArgs::Str(..) => FuncParams::Str,
            FuncArgs::StrByte(..) => FuncParams::StrByte,
            FuncArgs::StrByte8(..) => FuncParams::StrByte8,
            FuncArgs::StrFloat2(..) => FuncParams::StrFloat2,
            FuncArgs::StrInt(..) => FuncParams::StrInt,
            FuncArgs::StrIntFloatInt(..) => FuncParams::StrIntFloatInt,
            FuncArgs::StrIntFloat3Byte(..) => FuncParams::StrIntFloat3Byte,
            FuncArgs::StrInt2(..) => FuncParams::StrInt2,
            FuncArgs::StrText(..) => FuncParams::StrText,
            FuncArgs::Str2(..) => FuncParams::Str2,
            FuncArgs::Str2Int(..) => FuncParams::Str2Int,
            FuncArgs::Text(..) => FuncParams::Text,
            FuncArgs::TextVarListStrList(..) => FuncParams::TextVarListStrList,
            FuncArgs::VarStr(..) => FuncParams::VarStr,
        }
    }

    fn write(&self, code_page: CodePage, res: &mut Vec<u8>) -> Result<(), String> {
        match self {
            FuncArgs::None => { },
            FuncArgs::Byte(a1) => res.push(*a1),
            FuncArgs::ByteStr(a1, a2) => {
                res.push(*a1);
                write_str(code_page, a2, res)?;
            },
            FuncArgs::CharFloat(a1, a2) => {
                write_char(code_page, a1, res)?;
                a2.write(res)?;
            },
            FuncArgs::Float(a1) => a1.write(res)?,
            FuncArgs::FloatStr(a1, a2) => {
                a1.write(res)?;
                write_str(code_page, a2, res)?;
            },
            FuncArgs::Float3Byte([a1_1, a1_2, a1_3], a2) => {
                a1_1.write(res)?;
                a1_2.write(res)?;
                a1_3.write(res)?;
                res.push(*a2);
            },
            FuncArgs::Float3IntListByte([a1_1, a1_2, a1_3], a2, a3) => {
                a1_1.write(res)?;
                a1_2.write(res)?;
                a1_3.write(res)?;
                write_int_list(a2, res)?;
                res.push(*a3);
            },
            FuncArgs::Float4([a1_1, a1_2, a1_3, a1_4]) => {
                a1_1.write(res)?;
                a1_2.write(res)?;
                a1_3.write(res)?;
                a1_4.write(res)?;
            },
            FuncArgs::Float4Str([a1_1, a1_2, a1_3, a1_4], a2) => {
                a1_1.write(res)?;
                a1_2.write(res)?;
                a1_3.write(res)?;
                a1_4.write(res)?;
                write_str(code_page, a2, res)?;
            },
            FuncArgs::Int(a1) => write_i16(*a1, res),
            FuncArgs::IntByte(a1, a2) => {
                write_i16(*a1, res);
                res.push(*a2);
            },
            FuncArgs::Int2([a1_1, a1_2]) => {
                write_i16(*a1_1, res);
                write_i16(*a1_2, res);
            },
            FuncArgs::Str(a1) => write_str(code_page, a1, res)?,
            FuncArgs::StrByte(a1, a2) => {
                write_str(code_page, a1, res)?;
                res.push(*a2);
            },
            FuncArgs::StrByte8(a1, [a2_1, a2_2, a2_3, a2_4, a2_5, a2_6, a2_7, a2_8]) => {
                write_str(code_page, a1, res)?;
                res.push(*a2_1);
                res.push(*a2_2);
                res.push(*a2_3);
                res.push(*a2_4);
                res.push(*a2_5);
                res.push(*a2_6);
                res.push(*a2_7);
                res.push(*a2_8);
            },
            FuncArgs::StrFloat2(a1, [a2_1, a2_2]) => {
                write_str(code_page, a1, res)?;
                a2_1.write(res)?;
                a2_2.write(res)?;
            },
            FuncArgs::StrInt(a1, a2) => {
                write_str(code_page, a1, res)?;
                write_i16(*a2, res);
            },
            FuncArgs::StrIntFloatInt(a1, a2, a3, a4) => {
                write_str(code_page, a1, res)?;
                write_i16(*a2, res);
                a3.write(res)?;
                write_i16(*a4, res);
            },
            FuncArgs::StrIntFloat3Byte(a1, a2, [a3_1, a3_2, a3_3], a4) => {
                write_str(code_page, a1, res)?;
                write_i16(*a2, res);
                a3_1.write(res)?;
                a3_2.write(res)?;
                a3_3.write(res)?;
                res.push(*a4);
            },
            FuncArgs::StrInt2(a1, [a2_1, a2_2]) => {
                write_str(code_page, a1, res)?;
                write_i16(*a2_1, res);
                write_i16(*a2_2, res);
            },
            FuncArgs::StrText(a1, a2) => {
                write_str(code_page, a1, res)?;
                write_text(code_page, a2, res)?;
            },
            FuncArgs::Str2([a1_1, a1_2]) => {
                write_str(code_page, a1_1, res)?;
                write_str(code_page, a1_2, res)?;
            },
            FuncArgs::Str2Int([a1_1, a1_2], a2) => {
                write_str(code_page, a1_1, res)?;
                write_str(code_page, a1_2, res)?;
                write_i16(*a2, res);
            },
            FuncArgs::Text(a1) => write_text(code_page, a1, res)?,
            FuncArgs::TextVarListStrList(a1, a2, a3) => {
                write_text(code_page, a1, res)?;
                write_var_list(code_page, a2, res)?;
                write_str_list(code_page, a3, res)?;
            },
            FuncArgs::VarStr(a1, a2) => {
                a1.write(code_page, res)?;
                write_str(code_page, a2, res)?;
            },
        }
        Ok(())
    }
}

mod parser {
    use super::*;
    use nom_errors::*;
    use nom_errors::bytes::*;

    fn float(input: &[u8]) -> NomRes<&[u8], Float, (), !> {
        map(le_u32(), |bits| {
            if let Some(var_type) = VarType::n((bits & 0xFF) as u8) {
                if (bits >> 24) != 0 {
                    None
                } else {
                    Some(Float::Var { var_type, index: ((bits >> 8) & 0xFFFF) as u16 })
                }
            } else { None }.unwrap_or(Float::Val { val: f32::from_bits(bits) })
        })(input)
    }

    fn string<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], String, (), !> {
        map(flat_map(le_u8(), |len| take(len.into())), move |x| code_page.decode(x))
    }

    fn str_list<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Vec<String>, (), !> {
        flat_map(le_u8(), move |len| count(string(code_page), len.into()))
    }

    fn int_list(input: &[u8]) -> NomRes<&[u8], Vec<i16>, (), !> {
        flat_map(le_u16(), move |len| count(le_i16(), len.into()))(input)
    }

    fn var_list<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Vec<Var>, (), !> {
        flat_map(le_u8(), move |len| count(var(code_page), len.into()))
    }

    fn text<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], String, (), !> {
        map(flat_map(le_u16(), |len| take(len.into())), move |x| code_page.decode(x))
    }

    fn ch<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], String, (), !> {
        map(take(1), move |x| code_page.decode(x))
    }

    fn var_type(input: &[u8]) -> NomRes<&[u8], VarType, (), !> {
        map_res(le_u8(), |x| VarType::n(x).ok_or(()))(input)
    }

    fn local_var(input: &[u8]) -> NomRes<&[u8], (VarType, u16), (), !> {
        map(seq_2(var_type, le_u16()), |(var_type, index)| (var_type, index))(input)
    }

    fn owner_var<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], (String, VarType, u16), (), !> {
        map(seq_3(tag([b'r']), string(code_page), local_var), |(_, owner, var)| (owner, var.0, var.1))
    }

    fn global_var<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], String, (), !> {
        map(seq_2(tag([b'G']), string(code_page)), |(_, var)| var)
    }

    fn var<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Var, (), !> {
        alt_3(
            map(owner_var(code_page), |(owner, var_type, index)| Var::Local { owner: Some(owner), var_type, index }),
            map(local_var, |(var_type, index)| Var::Local { owner: None, var_type, index }),
            map(global_var(code_page), |name| Var::Global { name })
        )
    }

    fn byte_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(le_u8(), FuncArgs::Byte)(input)
    }

    fn byte_str_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_2(
                le_u8(),
                string(code_page)
            ),
            |(a1, a2)| FuncArgs::ByteStr(a1, a2)
        )
    }

    fn char_float_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(seq_2(ch(code_page), float), |(a1, a2)| FuncArgs::CharFloat(a1, a2))
    }

    fn float_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(float, FuncArgs::Float)(input)
    }

    fn float_str_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_2(float, string(code_page)),
            |(a1, a2)| FuncArgs::FloatStr(a1, a2)
        )
    }

    fn float_3_byte_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(
            seq_4(float, float, float, le_u8()),
            |(a1_1, a1_2, a1_3, a2)| FuncArgs::Float3Byte([a1_1, a1_2, a1_3], a2)
        )(input)
    }

    fn float_3_int_list_byte_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(
            seq_3(seq_3(float, float, float), int_list, le_u8()),
            |((a1_1, a1_2, a1_3), a2, a3)| FuncArgs::Float3IntListByte(
                [a1_1, a1_2, a1_3], a2, a3
            )
        )(input)
    }

    fn float_4_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(
            seq_4(float, float, float, float),
            |(a1_1, a1_2, a1_3, a1_4)| FuncArgs::Float4([a1_1, a1_2, a1_3, a1_4])
        )(input)
    }

    fn float_4_str_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_5(float, float, float, float, string(code_page)),
            |(a1_1, a1_2, a1_3, a1_4, a2)| FuncArgs::Float4Str([a1_1, a1_2, a1_3, a1_4], a2)
        )
    }

    fn int_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(le_i16(), FuncArgs::Int)(input)
    }

    fn int_byte_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(seq_2(le_i16(), le_u8()), |(a1, a2)| FuncArgs::IntByte(a1, a2))(input)
    }

    fn int_2_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(seq_2(le_i16(), le_i16()), |(a1_1, a1_2)| FuncArgs::Int2([a1_1, a1_2]))(input)
    }

    fn str_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(string(code_page), FuncArgs::Str)
    }

    fn str_byte_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_2(
                string(code_page),
                le_u8()
            ),
            |(a1, a2)| FuncArgs::StrByte(a1, a2)
        )
    }

    fn str_byte_8_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_2(
                string(code_page),
                seq_8(le_u8(), le_u8(), le_u8(), le_u8(), le_u8(), le_u8(), le_u8(), le_u8())
            ),
            |(a1, (a2_1, a2_2, a2_3, a2_4, a2_5, a2_6, a2_7, a2_8))| FuncArgs::StrByte8(
                a1, [a2_1, a2_2, a2_3, a2_4, a2_5, a2_6, a2_7, a2_8]
            )
        )
    }

    fn str_float_2_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_2(
                string(code_page),
                seq_2(float, float)
            ),
            |(a1, (a2_1, a2_2))| FuncArgs::StrFloat2(a1, [a2_1, a2_2])
        )
    }

    fn str_int_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(seq_2(string(code_page), le_i16()), |(a1, a2)| FuncArgs::StrInt(a1, a2))
    }

    fn str_int_float_int_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_4(string(code_page), le_i16(), float, le_i16()),
            |(a1, a2, a3, a4)| FuncArgs::StrIntFloatInt(a1, a2, a3, a4)
        )
    }

    fn str_int_float_3_byte_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_6(string(code_page), le_i16(), float, float, float, le_u8()),
            |(a1, a2, a3_1, a3_2, a3_3, a4)| FuncArgs::StrIntFloat3Byte(a1, a2, [a3_1, a3_2, a3_3], a4)
        )
    }

    fn str_int_2_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_3(string(code_page), le_i16(), le_i16()),
            |(a1, a2_1, a2_2)| FuncArgs::StrInt2(a1, [a2_1, a2_2])
        )
    }

    fn str_text_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(seq_2(string(code_page), text(code_page)), |(a1, a2)| FuncArgs::StrText(a1, a2))
    }

    fn str_2_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(seq_2(string(code_page), string(code_page)), |(a1_1, a1_2)| FuncArgs::Str2([a1_1, a1_2]))
    }

    fn str_2_int_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(seq_3(string(code_page), string(code_page), le_i16()), |(a1_1, a1_2, a2)| FuncArgs::Str2Int([a1_1, a1_2], a2))
    }

    fn text_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(text(code_page), FuncArgs::Text)
    }

    fn text_var_list_str_list_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_3(text(code_page), var_list(code_page), str_list(code_page)),
            |(a1, a2, a3)| FuncArgs::TextVarListStrList(a1, a2, a3)
        )
    }

    fn var_str_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_2(
                var(code_page),
                string(code_page)
            ),
            |(a1, a2)| FuncArgs::VarStr(a1, a2)
        )
    }

    fn func_args<'a>(code_page: CodePage, func: Func) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        move |input| {
            match func.params() {
                FuncParams::None => Ok((input, FuncArgs::None)),
                FuncParams::Byte => byte_args(input),
                FuncParams::ByteStr => byte_str_args(code_page)(input),
                FuncParams::CharFloat => char_float_args(code_page)(input),
                FuncParams::Float => float_args(input),
                FuncParams::FloatStr => float_str_args(code_page)(input),
                FuncParams::Float3Byte => float_3_byte_args(input),
                FuncParams::Float3IntListByte => float_3_int_list_byte_args(input),
                FuncParams::Float4 => float_4_args(input),
                FuncParams::Float4Str => float_4_str_args(code_page)(input),
                FuncParams::Int => int_args(input),
                FuncParams::IntByte => int_byte_args(input),
                FuncParams::Int2 => int_2_args(input),
                FuncParams::Str => str_args(code_page)(input),
                FuncParams::StrByte => str_byte_args(code_page)(input),
                FuncParams::StrByte8 => str_byte_8_args(code_page)(input),
                FuncParams::StrFloat2 => str_float_2_args(code_page)(input),
                FuncParams::StrInt => str_int_args(code_page)(input),
                FuncParams::StrIntFloatInt => str_int_float_int_args(code_page)(input),
                FuncParams::StrIntFloat3Byte => str_int_float_3_byte_args(code_page)(input),
                FuncParams::StrInt2 => str_int_2_args(code_page)(input),
                FuncParams::StrText => str_text_args(code_page)(input),
                FuncParams::Str2 => str_2_args(code_page)(input),
                FuncParams::Str2Int => str_2_int_args(code_page)(input),
                FuncParams::Text => text_args(code_page)(input),
                FuncParams::TextVarListStrList => text_var_list_str_list_args(code_page)(input),
                FuncParams::VarStr => var_str_args(code_page)(input),
            }
        }
    }

    fn func(input: &[u8]) -> NomRes<&[u8], Func, (), !> {
        map_res(le_u16(), |func| Func::n(func).ok_or(()))(input)
    }

    pub fn stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Stmt, (), !> {
        flat_map(func, move |func| map(func_args(code_page, func), move |args| Stmt { func, args }))
    }

    pub fn stmts(code_page: CodePage, input: &[u8]) -> (&[u8], Vec<Stmt>) {
        result_from_parser(many0(stmt(code_page))(input)).unwrap_or_else(|x| match x {
            NomErr::Error(x) => x,
            NomErr::Failure(x) => x
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Stmt {
    pub func: Func,
    pub args: FuncArgs,
}

impl Stmt {
    fn write(&self, code_page: CodePage, res: &mut Vec<u8>) -> Result<(), String> {
        write_u16(self.func as u16, res);
        self.args.write(code_page, res)?;
        Ok(())
    }
}

const STMT_FUNC_FIELD: &str = name_of!(func in Stmt);
const STMT_ARGS_FIELD: &str = name_of!(args in Stmt);

const TAGGED_STMT_FIELDS: &[&str] = &[
    STMT_ARGS_FIELD,
];

struct TaggedStmt<'a>(&'a Stmt);

struct TaggedStmtBuf(Stmt);

struct TaggedStmtDe(Func);

impl<'a> Serialize for TaggedStmt<'a> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut serializer = serializer.serialize_struct(name_of!(type Stmt), 1)?;
        serializer.serialize_field(STMT_ARGS_FIELD, &ValueWithSeed(&self.0.args, FuncArgsSerde { params: self.0.func.params() }))?;
        serializer.end()
    }
}

enum TaggedStmtField {
    Args
}

struct TaggedStmtFieldDeVisitor;

impl<'de> de::Visitor<'de> for TaggedStmtFieldDeVisitor {
    type Value = TaggedStmtField;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "stmt field")
    }

    fn visit_str<E: de::Error>(self, value: &str) -> Result<Self::Value, E> {
        match value {
            STMT_ARGS_FIELD => Ok(TaggedStmtField::Args),
            x => Err(E::unknown_field(x, TAGGED_STMT_FIELDS)),
        }
    }
}

impl<'de> de::Deserialize<'de> for TaggedStmtField {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_identifier(TaggedStmtFieldDeVisitor)
    }
}

struct TaggedStmtDeVisitor(Func);

impl<'de> de::Visitor<'de> for TaggedStmtDeVisitor {
    type Value = TaggedStmtBuf;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "stmt")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error> where A: de::MapAccess<'de> {
        let mut args = None;
        while let Some(TaggedStmtField::Args) = map.next_key()? {
            if args.replace(map.next_value_seed(FuncArgsSerde { params: self.0.params() })?).is_some() {
                return Err(A::Error::duplicate_field(STMT_ARGS_FIELD));
            }
        }
        let args = args.ok_or_else(|| A::Error::missing_field(STMT_ARGS_FIELD))?;
        Ok(TaggedStmtBuf(Stmt { func: self.0, args }))
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        let args = seq.next_element_seed(FuncArgsSerde { params: self.0.params() })?
            .ok_or_else(|| A::Error::invalid_length(0, &self))?;
        Ok(TaggedStmtBuf(Stmt { func: self.0, args }))
    }
}

impl<'de> DeserializeSeed<'de> for TaggedStmtDe {
    type Value = TaggedStmtBuf;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        deserializer.deserialize_struct(name_of!(type Stmt), TAGGED_STMT_FIELDS, TaggedStmtDeVisitor(self.0))
    }
}

impl Serialize for Stmt {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serde_tagged::ser::internal::serialize(
            serializer,
            STMT_FUNC_FIELD,
            &self.func,
            &TaggedStmt(self)
        )
    }
}

struct TaggedStmtDeSeedFactory;

impl<'de> serde_tagged::de::SeedFactory<'de, Func> for TaggedStmtDeSeedFactory {
    type Value = TaggedStmtBuf;
    type Seed = TaggedStmtDe;

    fn seed<E: de::Error>(self, tag: Func) -> Result<Self::Seed, E> {
        Ok(TaggedStmtDe(tag))
    }
}

impl<'de> Deserialize<'de> for Stmt {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        serde_tagged::de::internal::deserialize(
            deserializer,
            STMT_FUNC_FIELD,
            TaggedStmtDeSeedFactory,
        ).map(|x| x.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ScriptData {
    pub stmts: Vec<Stmt>,
    pub raw: Vec<u8>,
}

impl ScriptData {
    pub fn to_bytes(&self, code_page: CodePage) -> Result<Vec<u8>, String> {
        let mut bytes = Vec::new();
        for stmt in &self.stmts {
            stmt.write(code_page, &mut bytes)?;
        }
        if parser::stmt(code_page)(&self.raw).is_ok() {
            return Err("known func in raw bytes".into());
        }
        bytes.extend_from_slice(&self.raw);
        Ok(bytes)
    }

    pub fn from_bytes(code_page: CodePage, bytes: &[u8]) -> ScriptData {
        let (bytes, stmts) = parser::stmts(code_page, bytes);
        ScriptData { stmts, raw: bytes.into() }
    }
}

#[derive(Clone)]
pub struct ScriptDataSerde {
    pub code_page: Option<CodePage>,
}

const SCRIPT_DATA_STMTS_FIELD: &str = name_of!(stmts in ScriptData);
const SCRIPT_DATA_RAW_FIELD: &str = name_of!(raw in ScriptData);

const SCRIPT_DATA_FIELDS: &[&str] = &[
    SCRIPT_DATA_STMTS_FIELD,
    SCRIPT_DATA_RAW_FIELD,
];

impl SerializeSeed for ScriptDataSerde {
    type Value = ScriptData;

    fn serialize<S: Serializer>(&self, value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            let mut serializer = serializer.serialize_struct(name_of!(type ScriptData), 2)?;
            serializer.serialize_field(SCRIPT_DATA_STMTS_FIELD, &value.stmts)?;
            serializer.serialize_field(SCRIPT_DATA_RAW_FIELD, &ValueWithSeed(&value.raw[..], HexDump))?;
            serializer.end()
        } else {
            let Some(code_page) = self.code_page else {
                return Err(S::Error::custom("code page required for binary serialization"));
            };
            value.to_bytes(code_page).map_err(S::Error::custom)?.serialize(serializer)
        }
    }
}

enum ScriptDataField {
    Stmts,
    Raw,
}

struct ScriptDataFieldDeVisitor;

impl<'de> de::Visitor<'de> for ScriptDataFieldDeVisitor {
    type Value = ScriptDataField;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "script data field")
    }

    fn visit_str<E: de::Error>(self, value: &str) -> Result<Self::Value, E> {
        match value {
            SCRIPT_DATA_STMTS_FIELD => Ok(ScriptDataField::Stmts),
            SCRIPT_DATA_RAW_FIELD => Ok(ScriptDataField::Raw),
            x => Err(E::unknown_field(x, SCRIPT_DATA_FIELDS)),
        }
    }
}

impl<'de> de::Deserialize<'de> for ScriptDataField {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_identifier(ScriptDataFieldDeVisitor)
    }
}

struct ScriptDataDeVisitor;

impl<'de> de::Visitor<'de> for ScriptDataDeVisitor {
    type Value = ScriptData;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "script data")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error> where A: de::MapAccess<'de> {
        let mut stmts = None;
        let mut raw = None;
        while let Some(field) = map.next_key()? {
            match field {
                ScriptDataField::Stmts =>
                    if stmts.replace(map.next_value()?).is_some() {
                        return Err(A::Error::duplicate_field(SCRIPT_DATA_STMTS_FIELD));
                    },
                ScriptDataField::Raw =>
                    if raw.replace(map.next_value_seed(HexDump)?).is_some() {
                        return Err(A::Error::duplicate_field(SCRIPT_DATA_RAW_FIELD));
                    },
            }
        }
        let stmts = stmts.ok_or_else(|| A::Error::missing_field(SCRIPT_DATA_STMTS_FIELD))?;
        let raw = raw.ok_or_else(|| A::Error::missing_field(SCRIPT_DATA_RAW_FIELD))?;
        Ok(ScriptData { stmts, raw })
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        let stmts = seq.next_element()?
            .ok_or_else(|| A::Error::invalid_length(0, &self))?;
        let raw = seq.next_element_seed(HexDump)?
            .ok_or_else(|| A::Error::invalid_length(1, &self))?;
        Ok(ScriptData { stmts, raw })
    }
}

impl<'de> DeserializeSeed<'de> for ScriptDataSerde {
    type Value = ScriptData;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            deserializer.deserialize_struct(name_of!(type ScriptData), SCRIPT_DATA_FIELDS, ScriptDataDeVisitor)
        } else {
            let Some(code_page) = self.code_page else {
                return Err(D::Error::custom("code page required for binary deserialization"));
            };
            Ok(ScriptData::from_bytes(code_page, &<Vec<u8>>::deserialize(deserializer)?))
        }
    }
}
