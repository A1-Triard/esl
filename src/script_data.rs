#![cfg(esl_script_data)]
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
                if VarType::n((bits & 0xFF) as u8).is_some() {
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
        Some(c) => format!("the '{c}' char is not representable in {code_page:?} func page")
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
        Some(c) => format!("the '{c}' char is not representable in {code_page:?} func page")
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

fn write_word_list(w: &[u16], res: &mut Vec<u8>) -> Result<(), String> {
    let len = w.len().try_into().map_err(|_| "too big word list".to_string())?;
    write_u16(len, res);
    for w in w {
        write_u16(*w, res);
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
        MessageBox = 0x1000,
        PlaySound = 0x1002,
        PositionCell = 0x1005,
        Rotate = 0x1007,
        SetAngle = 0x100D,
        SetAtStart = 0x1010,
        PlayGroup = 0x1014,
        Activate = 0x1017,
        StartCombat = 0x1019,
        StopCombat = 0x101A,
        StartScript = 0x101B,
        StopScript = 0x101C,
        AddTopic = 0x1022,
        ModStrength = 0x1025,
        ModIntelligence = 0x1028,
        ModWillpower = 0x102B,
        ModAgility = 0x102E,
        ModSpeed = 0x1031,
        ModEndurance = 0x1034,
        ModPersonality = 0x1037,
        ModLuck = 0x103A,
        SetMarksman = 0x1081,
        SetHealth = 0x108D,
        ModHealth = 0x108E,
        SetMagicka = 0x1090,
        ModMagicka = 0x1091,
        ModFatigue = 0x1094,
        ModDisposition = 0x109A,
        SetPCCrimeLevel = 0x109C,
        Journal = 0x10CC,
        RaiseRank = 0x10CE,
        AddItem = 0x10D4,
        RemoveItem = 0x10D5,
        Enable = 0x10DA,
        Disable = 0x10DB,
        EnablePlayerControls = 0x10DD,
        DisablePlayerControls = 0x10DE,
        ShowRestMenu = 0x10E5,
        PlaceAtPC = 0x10E6,
        ForceGreeting = 0x10E8,
        DisableTeleporting = 0x10EF,
        AiFollow = 0x10F6,
        AiTravel = 0x10F8,
        AiWander = 0x10F9,
        SetFight = 0x1100,
        SetHello = 0x1109,
        Drop = 0x110D,
        Say = 0x111B,
        Cast = 0x1123,
        EnableNameMenu = 0x1126,
        PlaySound3D = 0x112C,
        PayFine = 0x114F,
        PlayBink = 0x1155,
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
    Float3Byte,
    Float3WordListByte,
    Float4Str,
    Str,
    StrByte,
    StrWord,
    StrWordFloatWord,
    StrWordFloat3Byte,
    StrWordInt,
    StrText,
    Str2,
    Text,
    TextVarListStrList,
    Var,
    VarStr,
    WordByte,
}

impl Func {
    pub fn params(self) -> FuncParams {
        match self {
            Func::Activate => FuncParams::None,
            Func::AddItem => FuncParams::StrWord,
            Func::AddTopic => FuncParams::Str,
            Func::AiFollow => FuncParams::StrWordFloat3Byte,
            Func::AiTravel => FuncParams::Float3Byte,
            Func::AiWander => FuncParams::Float3WordListByte,
            Func::Cast => FuncParams::Str2,
            Func::Disable => FuncParams::None,
            Func::DisablePlayerControls => FuncParams::None,
            Func::DisableTeleporting => FuncParams::None,
            Func::Drop => FuncParams::StrWord,
            Func::Else => FuncParams::Byte,
            Func::ElseIf => FuncParams::ByteStr,
            Func::Enable => FuncParams::None,
            Func::EnableNameMenu => FuncParams::None,
            Func::EnablePlayerControls => FuncParams::None,
            Func::End => FuncParams::None,
            Func::EndIf => FuncParams::None,
            Func::ForceGreeting => FuncParams::None,
            Func::If => FuncParams::ByteStr,
            Func::Journal => FuncParams::StrWordInt,
            Func::MessageBox => FuncParams::TextVarListStrList,
            Func::ModAgility => FuncParams::Float,
            Func::ModDisposition => FuncParams::Float,
            Func::ModEndurance => FuncParams::Float,
            Func::ModFatigue => FuncParams::Float,
            Func::ModHealth => FuncParams::Float,
            Func::ModIntelligence => FuncParams::Float,
            Func::ModLuck => FuncParams::Float,
            Func::ModMagicka => FuncParams::Float,
            Func::ModPersonality => FuncParams::Float,
            Func::ModSpeed => FuncParams::Float,
            Func::ModStrength => FuncParams::Float,
            Func::ModWillpower => FuncParams::Float,
            Func::PayFine => FuncParams::None,
            Func::PlaceAtPC => FuncParams::StrWordFloatWord,
            Func::PlayBink => FuncParams::StrByte,
            Func::PlayGroup => FuncParams::WordByte,
            Func::PlaySound => FuncParams::Str,
            Func::PlaySound3D => FuncParams::Str,
            Func::PositionCell => FuncParams::Float4Str,
            Func::RaiseRank => FuncParams::None,
            Func::RemoveItem => FuncParams::StrWord,
            Func::Return => FuncParams::None,
            Func::Rotate => FuncParams::CharFloat,
            Func::Say => FuncParams::StrText,
            Func::Set => FuncParams::VarStr,
            Func::SetAngle => FuncParams::CharFloat,
            Func::SetAtStart => FuncParams::None,
            Func::SetFight => FuncParams::Float,
            Func::SetHealth => FuncParams::Float,
            Func::SetHello => FuncParams::Float,
            Func::SetMagicka => FuncParams::Float,
            Func::SetMarksman => FuncParams::Float,
            Func::SetPCCrimeLevel => FuncParams::Float,
            Func::SetRef => FuncParams::Str,
            Func::ShowRestMenu => FuncParams::None,
            Func::StartCombat => FuncParams::Str,
            Func::StartScript => FuncParams::Str,
            Func::StopCombat => FuncParams::None,
            Func::StopScript => FuncParams::Str,
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
    Float3Byte([Float; 3], u8),
    Float3WordListByte([Float; 3], Vec<u16>, u8),
    Float4Str([Float; 4], String),
    Str(String),
    StrByte(String, u8),
    StrText(String, String),
    StrWord(String, u16),
    StrWordFloatWord(String, u16, Float, u16),
    StrWordFloat3Byte(String, u16, [Float; 3], u8),
    StrWordInt(String, u16, i16),
    Str2([String; 2]),
    Text(String),
    TextVarListStrList(String, Vec<Var>, Vec<String>),
    Var(Var),
    VarStr(Var, String),
    WordByte(u16, u8),
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
            FuncArgs::Float3Byte(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::Float4Str(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::Float3WordListByte(a1, a2, a3) => (a1, a2, a3).serialize(serializer),
            FuncArgs::Str(a1) => a1.serialize(serializer),
            FuncArgs::StrByte(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::StrText(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::StrWord(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::StrWordFloatWord(a1, a2, a3, a4) => (a1, a2, a3, a4).serialize(serializer),
            FuncArgs::StrWordFloat3Byte(a1, a2, a3, a4) => (a1, a2, a3, a4).serialize(serializer),
            FuncArgs::StrWordInt(a1, a2, a3) => (a1, a2, a3).serialize(serializer),
            FuncArgs::Str2(a1) => a1.serialize(serializer),
            FuncArgs::Text(a1) => a1.serialize(serializer),
            FuncArgs::TextVarListStrList(a1, a2, a3) => (a1, a2, a3).serialize(serializer),
            FuncArgs::Var(a1) => a1.serialize(serializer),
            FuncArgs::VarStr(a1, a2) => (a1, a2).serialize(serializer),
            FuncArgs::WordByte(a1, a2) => (a1, a2).serialize(serializer),
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
            FuncParams::Float3Byte => {
                let (a1, a2) = <([Float; 3], u8)>::deserialize(deserializer)?;
                FuncArgs::Float3Byte(a1, a2)
            },
            FuncParams::Float3WordListByte => {
                let (a1, a2, a3) = <([Float; 3], Vec<u16>, u8)>::deserialize(deserializer)?;
                FuncArgs::Float3WordListByte(a1, a2, a3)
            },
            FuncParams::Float4Str => {
                let (a1, a2) = <([Float; 4], String)>::deserialize(deserializer)?;
                FuncArgs::Float4Str(a1, a2)
            },
            FuncParams::Str => { let a1 = String::deserialize(deserializer)?; FuncArgs::Str(a1) },
            FuncParams::StrByte => { let (a1, a2) = <(String, u8)>::deserialize(deserializer)?; FuncArgs::StrByte(a1, a2) },
            FuncParams::StrText => { let (a1, a2) = <(String, String)>::deserialize(deserializer)?; FuncArgs::StrText(a1, a2) },
            FuncParams::StrWord => { let (a1, a2) = <(String, u16)>::deserialize(deserializer)?; FuncArgs::StrWord(a1, a2) },
            FuncParams::StrWordFloatWord => {
                let (a1, a2, a3, a4) = <(String, u16, Float, u16)>::deserialize(deserializer)?;
                FuncArgs::StrWordFloatWord(a1, a2, a3, a4)
            },
            FuncParams::StrWordFloat3Byte => {
                let (a1, a2, a3, a4) = <(String, u16, [Float; 3], u8)>::deserialize(deserializer)?;
                FuncArgs::StrWordFloat3Byte(a1, a2, a3, a4)
            },
            FuncParams::StrWordInt => {
                let (a1, a2, a3) = <(String, u16, i16)>::deserialize(deserializer)?; FuncArgs::StrWordInt(a1, a2, a3)
            },
            FuncParams::Str2 => { let a1 = <[String; 2]>::deserialize(deserializer)?; FuncArgs::Str2(a1) },
            FuncParams::Text => { let a1 = String::deserialize(deserializer)?; FuncArgs::Text(a1) },
            FuncParams::TextVarListStrList => {
                let (a1, a2, a3) = <(String, Vec<Var>, Vec<String>)>::deserialize(deserializer)?;
                FuncArgs::TextVarListStrList(a1, a2, a3)
            },
            FuncParams::Var => { let a1 = Var::deserialize(deserializer)?; FuncArgs::Var(a1) },
            FuncParams::VarStr => { let (a1, a2) = <(Var, String)>::deserialize(deserializer)?; FuncArgs::VarStr(a1, a2) },
            FuncParams::WordByte => { let (a1, a2) = <(u16, u8)>::deserialize(deserializer)?; FuncArgs::WordByte(a1, a2) },
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
            FuncArgs::Float3Byte(..) => FuncParams::Float3Byte,
            FuncArgs::Float4Str(..) => FuncParams::Float4Str,
            FuncArgs::Float3WordListByte(..) => FuncParams::Float3WordListByte,
            FuncArgs::Str(..) => FuncParams::Str,
            FuncArgs::StrByte(..) => FuncParams::StrByte,
            FuncArgs::StrText(..) => FuncParams::StrText,
            FuncArgs::StrWord(..) => FuncParams::StrWord,
            FuncArgs::StrWordFloatWord(..) => FuncParams::StrWordFloatWord,
            FuncArgs::StrWordFloat3Byte(..) => FuncParams::StrWordFloat3Byte,
            FuncArgs::StrWordInt(..) => FuncParams::StrWordInt,
            FuncArgs::Str2(..) => FuncParams::Str2,
            FuncArgs::Text(..) => FuncParams::Text,
            FuncArgs::TextVarListStrList(..) => FuncParams::TextVarListStrList,
            FuncArgs::Var(..) => FuncParams::Var,
            FuncArgs::VarStr(..) => FuncParams::VarStr,
            FuncArgs::WordByte(..) => FuncParams::WordByte,
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
            FuncArgs::Float3Byte([a1_1, a1_2, a1_3], a2) => {
                a1_1.write(res)?;
                a1_2.write(res)?;
                a1_3.write(res)?;
                res.push(*a2);
            },
            FuncArgs::Float3WordListByte([a1_1, a1_2, a1_3], a2, a3) => {
                a1_1.write(res)?;
                a1_2.write(res)?;
                a1_3.write(res)?;
                write_word_list(a2, res)?;
                res.push(*a3);
            },
            FuncArgs::Float4Str([a1_1, a1_2, a1_3, a1_4], a2) => {
                a1_1.write(res)?;
                a1_2.write(res)?;
                a1_3.write(res)?;
                a1_4.write(res)?;
                write_str(code_page, a2, res)?;
            },
            FuncArgs::Str(a1) => write_str(code_page, a1, res)?,
            FuncArgs::StrByte(a1, a2) => {
                write_str(code_page, a1, res)?;
                res.push(*a2);
            },
            FuncArgs::StrText(a1, a2) => {
                write_str(code_page, a1, res)?;
                write_text(code_page, a2, res)?;
            },
            FuncArgs::StrWord(a1, a2) => {
                write_str(code_page, a1, res)?;
                write_u16(*a2, res);
            },
            FuncArgs::StrWordFloatWord(a1, a2, a3, a4) => {
                write_str(code_page, a1, res)?;
                write_u16(*a2, res);
                a3.write(res)?;
                write_u16(*a4, res);
            },
            FuncArgs::StrWordFloat3Byte(a1, a2, [a3_1, a3_2, a3_3], a4) => {
                write_str(code_page, a1, res)?;
                write_u16(*a2, res);
                a3_1.write(res)?;
                a3_2.write(res)?;
                a3_3.write(res)?;
                res.push(*a4);
            },
            FuncArgs::StrWordInt(a1, a2, a3) => {
                write_str(code_page, a1, res)?;
                write_u16(*a2, res);
                write_i16(*a3, res);
            },
            FuncArgs::Str2([a1_1, a1_2]) => {
                write_str(code_page, a1_1, res)?;
                write_str(code_page, a1_2, res)?;
            },
            FuncArgs::Text(a1) => write_text(code_page, a1, res)?,
            FuncArgs::TextVarListStrList(a1, a2, a3) => {
                write_text(code_page, a1, res)?;
                write_var_list(code_page, &a2, res)?;
                write_str_list(code_page, &a3, res)?;
            },
            FuncArgs::Var(a1) => a1.write(code_page, res)?,
            FuncArgs::VarStr(a1, a2) => {
                a1.write(code_page, res)?;
                write_str(code_page, a2, res)?;
            },
            FuncArgs::WordByte(a1, a2) => {
                write_u16(*a1, res);
                res.push(*a2);
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
        map_res(le_u32(), |bits| {
            if let Some(var_type) = VarType::n((bits & 0xFF) as u8) {
                if (bits >> 24) != 0 { return Err(()); }
                Ok(Float::Var { var_type, index: ((bits >> 8) & 0xFFFF) as u16 })
            } else { 
                Ok(Float::Val { val: f32::from_bits(bits) })
            }
        })(input)
    }

    fn string<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], String, (), !> {
        map(flat_map(le_u8(), |len| take(len.into())), move |x| code_page.decode(x))
    }

    fn str_list<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Vec<String>, (), !> {
        flat_map(le_u8(), move |len| count(string(code_page), len.into()))
    }

    fn word_list(input: &[u8]) -> NomRes<&[u8], Vec<u16>, (), !> {
        flat_map(le_u16(), move |len| count(le_u16(), len.into()))(input)
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

    fn local_var_ty<'a>(var_type: u8) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], u16, (), !> {
        map(seq_2(tag([var_type]), le_u16()), |(_, var)| var)
    }

    fn local_var(input: &[u8]) -> NomRes<&[u8], (VarType, u16), (), !> {
        alt_3(
            map(local_var_ty(b's'), |index| (VarType::Short, index)),
            map(local_var_ty(b'f'), |index| (VarType::Float, index)),
            map(local_var_ty(b'l'), |index| (VarType::Long, index)),
        )(input)
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
        map(le_u8(), |a1| FuncArgs::Byte(a1))(input)
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
        map(float, |a1| FuncArgs::Float(a1))(input)
    }

    fn float_3_byte_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(
            seq_4(float, float, float, le_u8()),
            |(a1_1, a1_2, a1_3, a2)| FuncArgs::Float3Byte([a1_1, a1_2, a1_3], a2)
        )(input)
    }

    fn float_3_word_list_byte_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(
            seq_3(seq_3(float, float, float), word_list, le_u8()),
            |((a1_1, a1_2, a1_3), a2, a3)| FuncArgs::Float3WordListByte(
                [a1_1, a1_2, a1_3], a2, a3
            )
        )(input)
    }

    fn float_4_str_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_5(float, float, float, float, string(code_page)),
            |(a1_1, a1_2, a1_3, a1_4, a2)| FuncArgs::Float4Str([a1_1, a1_2, a1_3, a1_4], a2)
        )
    }

    fn str_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(string(code_page), |a1| FuncArgs::Str(a1))
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

    fn str_text_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(seq_2(string(code_page), text(code_page)), |(a1, a2)| FuncArgs::StrText(a1, a2))
    }

    fn str_word_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(seq_2(string(code_page), le_u16()), |(a1, a2)| FuncArgs::StrWord(a1, a2))
    }

    fn str_word_float_word_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_4(string(code_page), le_u16(), float, le_u16()),
            |(a1, a2, a3, a4)| FuncArgs::StrWordFloatWord(a1, a2, a3, a4)
        )
    }

    fn str_word_float_3_byte_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_6(string(code_page), le_u16(), float, float, float, le_u8()),
            |(a1, a2, a3_1, a3_2, a3_3, a4)| FuncArgs::StrWordFloat3Byte(a1, a2, [a3_1, a3_2, a3_3], a4)
        )
    }

    fn str_word_int_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_3(string(code_page), le_u16(), le_i16()),
            |(a1, a2, a3)| FuncArgs::StrWordInt(a1, a2, a3)
        )
    }

    fn str_2_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(seq_2(string(code_page), string(code_page)), |(a1_1, a1_2)| FuncArgs::Str2([a1_1, a1_2]))
    }

    fn text_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(text(code_page), |a1| FuncArgs::Text(a1))
    }

    fn text_var_list_str_list_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(
            seq_3(text(code_page), var_list(code_page), str_list(code_page)),
            |(a1, a2, a3)| FuncArgs::TextVarListStrList(a1, a2, a3)
        )
    }

    fn var_args<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        map(var(code_page), |a1| FuncArgs::Var(a1))
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

    fn word_byte_args(input: &[u8]) -> NomRes<&[u8], FuncArgs, (), !> {
        map(seq_2(le_u16(), le_u8()), |(a1, a2)| FuncArgs::WordByte(a1, a2))(input)
    }

    fn func_args<'a>(code_page: CodePage, func: Func) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], FuncArgs, (), !> {
        move |input| {
            match func.params() {
                FuncParams::None => Ok((input, FuncArgs::None)),
                FuncParams::Byte => byte_args(input),
                FuncParams::ByteStr => byte_str_args(code_page)(input),
                FuncParams::CharFloat => char_float_args(code_page)(input),
                FuncParams::Float => float_args(input),
                FuncParams::Float3Byte => float_3_byte_args(input),
                FuncParams::Float3WordListByte => float_3_word_list_byte_args(input),
                FuncParams::Float4Str => float_4_str_args(code_page)(input),
                FuncParams::Str => str_args(code_page)(input),
                FuncParams::StrByte => str_byte_args(code_page)(input),
                FuncParams::StrText => str_text_args(code_page)(input),
                FuncParams::StrWord => str_word_args(code_page)(input),
                FuncParams::StrWordFloatWord => str_word_float_word_args(code_page)(input),
                FuncParams::StrWordFloat3Byte => str_word_float_3_byte_args(code_page)(input),
                FuncParams::StrWordInt => str_word_int_args(code_page)(input),
                FuncParams::Str2 => str_2_args(code_page)(input),
                FuncParams::Text => text_args(code_page)(input),
                FuncParams::TextVarListStrList => text_var_list_str_list_args(code_page)(input),
                FuncParams::Var => var_args(code_page)(input),
                FuncParams::VarStr => var_str_args(code_page)(input),
                FuncParams::WordByte => word_byte_args(input),
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

const STMT_FIELDS: &[&'static str] = &[
    STMT_FUNC_FIELD,
    STMT_ARGS_FIELD,
];

impl Serialize for Stmt {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut serializer = serializer.serialize_struct(name_of!(type Stmt), 2)?;
        serializer.serialize_field(STMT_FUNC_FIELD, &self.func)?;
        serializer.serialize_field(STMT_ARGS_FIELD, &ValueWithSeed(&self.args, FuncArgsSerde { params: self.func.params() }))?;
        serializer.end()
    }
}

enum StmtField {
    Func,
    Args,
}

struct StmtFieldDeVisitor;

impl<'de> de::Visitor<'de> for StmtFieldDeVisitor {
    type Value = StmtField;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "stmt field")
    }

    fn visit_str<E: de::Error>(self, value: &str) -> Result<Self::Value, E> {
        match value {
            STMT_FUNC_FIELD => Ok(StmtField::Func),
            STMT_ARGS_FIELD => Ok(StmtField::Args),
            x => Err(E::unknown_field(x, STMT_FIELDS)),
        }
    }
}

impl<'de> de::Deserialize<'de> for StmtField {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_identifier(StmtFieldDeVisitor)
    }
}

struct StmtDeVisitor;

impl<'de> de::Visitor<'de> for StmtDeVisitor {
    type Value = Stmt;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "stmt")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error> where A: de::MapAccess<'de> {
        let mut func: Option<Func> = None;
        let mut args = None;
        while let Some(field) = map.next_key()? {
            match field {
                StmtField::Func =>
                    if func.replace(map.next_value()?).is_some() {
                        return Err(A::Error::duplicate_field(STMT_FUNC_FIELD));
                    },
                StmtField::Args => {
                    let Some(func) = func else {
                        return Err(A::Error::custom("the 'args' field should be prceeded by the 'func' field"));
                    };
                    if args.replace(map.next_value_seed(FuncArgsSerde { params: func.params() })?).is_some() {
                        return Err(A::Error::duplicate_field(STMT_ARGS_FIELD));
                    }
                },
            }
        }
        let func = func.ok_or_else(|| A::Error::missing_field(STMT_FUNC_FIELD))?;
        let args = args.ok_or_else(|| A::Error::missing_field(STMT_ARGS_FIELD))?;
        Ok(Stmt { func, args })
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        let func: Func = seq.next_element()?
            .ok_or_else(|| A::Error::invalid_length(0, &self))?;
        let args = seq.next_element_seed(FuncArgsSerde { params: func.params() })?
            .ok_or_else(|| A::Error::invalid_length(1, &self))?;
        Ok(Stmt { func, args })
    }
}

impl<'de> Deserialize<'de> for Stmt {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_struct(name_of!(type Stmt), STMT_FIELDS, StmtDeVisitor)
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

const SCRIPT_DATA_FIELDS: &[&'static str] = &[
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
                return Err(S::Error::custom("func page required for binary serialization"));
            };
            value.to_bytes(code_page).serialize(serializer)
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
                return Err(D::Error::custom("func page required for binary deserialization"));
            };
            Ok(ScriptData::from_bytes(code_page, &<Vec<u8>>::deserialize(deserializer)?))
        }
    }
}
