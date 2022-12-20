#![cfg(esl_script_data)]

use crate::code_page::CodePage;
use enum_derive_2018::{EnumDisplay, EnumFromStr};
use enumn::N;
use iter_identify_first_last::IteratorIdentifyFirstLastExt;
use macro_attr_2018::macro_attr;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::DeserializeSeed;
use serde::de::Error as de_Error;
use serde::ser::Error as ser_Error;
use serde_serialize_seed::SerializeSeed;

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
 
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(tag="kind")]
pub enum Var {
    Local { #[serde(rename="type")] var_type: VarType, index: u16 },
    Global { name: String },
}

fn write_text(code_page: CodePage, s: &str, res: &mut Vec<u8>) -> Result<(), String> {
    let bytes = code_page.encode(s).map_err(|e| match e {
        None => format!("the '{s}' string does not correspond to any source byte sequence"),
        Some(c) => format!("the '{c}' char is not representable in {code_page:?} code page")
    })?;
    let len = bytes.len().try_into().map_err(|_| format!("too long string '{s}'"))?;
    res.push(len);
    res.extend_from_slice(&bytes);
    Ok(())
}

fn write_u16(v: u16, res: &mut Vec<u8>) {
    res.push((v & 0xFF) as u8);
    res.push((v >> 8) as u8);
}

fn write_i16(v: i16, res: &mut Vec<u8>) {
    write_u16(v as u16, res);
}

impl Var {
    fn write(&self, code_page: CodePage, res: &mut Vec<u8>) -> Result<(), String> {
        match self {
            &Var::Local { var_type, index } => {
                var_type.write(res);
                write_u16(index, res);
            },
            Var::Global { name } => {
                res.push(b'G');
                write_text(code_page, name, res)?;
            }
        }
        Ok(())
    }
}

mod hex_dump {
    use super::*;
    use crate::serde_helpers::HexDump;
    use serde::{Serializer, Deserializer, Serialize};
    use serde_serialize_seed::ValueWithSeed;

    pub fn serialize<S>(v: &Vec<u8>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        ValueWithSeed(&v[..], HexDump).serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error> where D: Deserializer<'de> {
        HexDump.deserialize(deserializer)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(tag="stmt")]
#[repr(u16)]
pub enum Stmt {
    Raw { #[serde(with="hex_dump")] bytes: Vec<u8> } = 0x0000,
    End = 0x0101,
    Set { var: Var, expr: String } = 0x0105,
    If { stmts: u8, cond: String } = 0x0106,
    EndIf = 0x0109,
    SetRef { id: String } = 0x010C,
    Return = 0x0124,
    StopScript { id: String } = 0x101C,
    Journal { id: String, value: u16, extra: i16 } = 0x10CC,
    RaiseRank = 0x10CE,
    AddItem { id: String, count: u16 } = 0x10D4,
    Enable = 0x10DA,
    Disable = 0x10DB,
    Drop { id: String, count: u16 } = 0x110D,
}

impl Stmt {
    fn write(&self, code_page: CodePage, is_last: bool, res: &mut Vec<u8>) -> Result<(), String> {
        match self {
            Stmt::Raw { bytes } => {
                if bytes.is_empty() { return Err("the 'raw bytes' should not be empty".into()); }
                if !is_last { return Err("the 'raw bytes' stmt should be the last".into()); }
                if !parser::is_raw_stmt(code_page, bytes) {
                    return Err("known stmt signature in raw bytes".into());
                }
                res.extend_from_slice(&bytes);
            },
            Stmt::Set { var, expr } => {
                write_u16(0x0105, res);
                var.write(code_page, res)?;
                write_text(code_page, expr, res)?;
            },
            Stmt::If { stmts, cond } => {
                write_u16(0x0106, res);
                res.push(*stmts);
                write_text(code_page, cond, res)?;
            },
            Stmt::EndIf => write_u16(0x0109, res),
            Stmt::End => write_u16(0x0101, res),
            Stmt::Return => write_u16(0x0124, res),
            Stmt::Enable => write_u16(0x10DA, res),
            Stmt::Disable => write_u16(0x10DB, res),
            Stmt::RaiseRank => write_u16(0x10CE, res),
            Stmt::SetRef { id } => {
                write_u16(0x010C, res);
                write_text(code_page, id, res)?;
            },
            Stmt::StopScript { id } => {
                write_u16(0x101C, res);
                write_text(code_page, id, res)?;
            },
            Stmt::AddItem { id, count } => {
                write_u16(0x10D4, res);
                write_text(code_page, id, res)?;
                write_u16(*count, res);
            },
            Stmt::Journal { id, value, extra } => {
                write_u16(0x10CC, res);
                write_text(code_page, id, res)?;
                write_u16(*value, res);
                write_i16(*extra, res);
            },
            Stmt::Drop { id, count } => {
                write_u16(0x110D, res);
                write_text(code_page, id, res)?;
                write_u16(*count, res);
            },
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ScriptData(Vec<Stmt>);

mod parser {
    use super::*;
    use nom_errors::*;
    use nom_errors::bytes::*;

    fn text<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], String, (), !> {
        map(flat_map(le_u8(), |len| take(len.into())), move |x| code_page.decode(x))
    }

    fn local_var<'a>(var_type: u8) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], u16, (), !> {
        map(seq_2(tag([var_type]), le_u16()), |(_, var)| var)
    }

    fn global_var<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], String, (), !> {
        map(seq_2(tag([b'G']), text(code_page)), |(_, var)| var)
    }

    fn var<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Var, (), !> {
        alt_4(
            map(local_var(b's'), |index| Var::Local { var_type: VarType::Short, index }),
            map(local_var(b'f'), |index| Var::Local { var_type: VarType::Float, index }),
            map(local_var(b'l'), |index| Var::Local { var_type: VarType::Long, index }),
            map(global_var(code_page), |name| Var::Global { name })
        )
    }

    fn set_stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Stmt, (), !> {
        map(
            seq_3(
                tag([0x05, 0x01]),
                var(code_page),
                text(code_page)
            ),
            |(_, var, expr)| Stmt::Set { var, expr }
        )
    }

    fn if_stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Stmt, (), !> {
        map(
            seq_3(
                tag([0x06, 0x01]),
                le_u8(),
                text(code_page)
            ),
            |(_, stmts, cond)| Stmt::If { stmts, cond }
        )
    }

    fn end_if_stmt(input: &[u8]) -> NomRes<&[u8], Stmt, (), !> {
        map(tag([0x09, 0x01]), |_| Stmt::EndIf)(input)
    }

    fn return_stmt(input: &[u8]) -> NomRes<&[u8], Stmt, (), !> {
        map(tag([0x24, 0x01]), |_| Stmt::Return)(input)
    }

    fn end_stmt(input: &[u8]) -> NomRes<&[u8], Stmt, (), !> {
        map(tag([0x01, 0x01]), |_| Stmt::End)(input)
    }

    fn enable_stmt(input: &[u8]) -> NomRes<&[u8], Stmt, (), !> {
        map(tag([0xDA, 0x10]), |_| Stmt::Enable)(input)
    }

    fn disable_stmt(input: &[u8]) -> NomRes<&[u8], Stmt, (), !> {
        map(tag([0xDB, 0x10]), |_| Stmt::Disable)(input)
    }

    fn raise_rank_stmt(input: &[u8]) -> NomRes<&[u8], Stmt, (), !> {
        map(tag([0xCE, 0x10]), |_| Stmt::RaiseRank)(input)
    }

    fn set_ref_stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Stmt, (), !> {
        map(seq_2(tag([0x0C, 0x01]), text(code_page)), |(_, id)| Stmt::SetRef { id })
    }

    fn stop_script_stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Stmt, (), !> {
        map(seq_2(tag([0x1C, 0x10]), text(code_page)), |(_, id)| Stmt::StopScript { id })
    }

    fn add_item_stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Stmt, (), !> {
        map(seq_3(tag([0xD4, 0x10]), text(code_page), le_u16()), |(_, id, count)| Stmt::AddItem { id, count })
    }

    fn journal_stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Stmt, (), !> {
        map(
            seq_4(tag([0xCC, 0x10]), text(code_page), le_u16(), le_i16()),
            |(_, id, value, extra)| Stmt::Journal { id, value, extra }
        )
    }

    fn drop_stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Stmt, (), !> {
        map(seq_3(tag([0x0D, 0x11]), text(code_page), le_u16()), |(_, id, count)| Stmt::Drop { id, count })
    }

    fn stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], Stmt, !, !> {
        alt_14(
            set_stmt(code_page),
            if_stmt(code_page),
            end_if_stmt,
            end_stmt,
            return_stmt,
            enable_stmt,
            disable_stmt,
            raise_rank_stmt,
            set_ref_stmt(code_page),
            stop_script_stmt(code_page),
            add_item_stmt(code_page),
            journal_stmt(code_page),
            drop_stmt(code_page),
            map(take_all(), |bytes: &[u8]| Stmt::Raw { bytes: bytes.into() })
        )
    }

    pub fn is_raw_stmt(code_page: CodePage, bytes: &[u8]) -> bool {
        matches!(result_from_parser(stmt(code_page)(bytes)).map_or_else(|e| match e {
            NomErr::Error(e) => e,
            NomErr::Failure(e) => e
        }, |x| x.1), Stmt::Raw { .. })
    }

    pub fn stmts(code_page: CodePage, bytes: &[u8]) -> Vec<Stmt> {
        result_from_parser(many0(stmt(code_page))(bytes)).map_or_else(|e| match e {
            NomErr::Error(e) => e,
            NomErr::Failure(e) => e
        }, |x| x.1)
    }
}

impl ScriptData {
    pub fn new(stmts: Vec<Stmt>) -> Result<ScriptData, String> {
        let this = ScriptData(stmts);
        this.to_bytes(CodePage::Unicode)?;
        Ok(this)
    }

    /// # Safety
    ///
    /// The `stmts` list should be obtained from [`stmts`] / [`stmts_mut`] / [`into_stmts`] method.
    pub unsafe fn new_unchecked(stmts: Vec<Stmt>) -> ScriptData {
        ScriptData(stmts)
    }

    pub fn stmts(&self) -> &[Stmt] { &self.0 }

    pub fn stmts_mut(&mut self) -> &mut [Stmt] { &mut self.0 }

    pub fn into_stmts(self) -> Vec<Stmt> { self.0 }

    pub fn to_bytes(&self, code_page: CodePage) -> Result<Vec<u8>, String> {
        let mut res = Vec::new();
        for (is_last, stmt) in self.0.iter().identify_last() {
            stmt.write(code_page, is_last, &mut res)?;
        }
        Ok(res)
    }

    pub fn from_bytes(code_page: CodePage, bytes: &[u8]) -> Self {
        ScriptData(parser::stmts(code_page, bytes))
    }
}

pub struct ScriptDataSerde {
    pub code_page: Option<CodePage>,
}

impl SerializeSeed for ScriptDataSerde {
    type Value = ScriptData;

    fn serialize<S: Serializer>(&self, value: &Self::Value, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            value.0.serialize(serializer)
        } else {
            let Some(code_page) = self.code_page else {
                return Err(S::Error::custom("code page required for binary serialization"));
            };
            value.to_bytes(code_page).serialize(serializer)
        }
    }
}

impl<'de> DeserializeSeed<'de> for ScriptDataSerde {
    type Value = ScriptData;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            let stmts = <Vec<Stmt>>::deserialize(deserializer)?;
            ScriptData::new(stmts).map_err(D::Error::custom)
        } else {
            let Some(code_page) = self.code_page else {
                return Err(D::Error::custom("code page required for binary serialization"));
            };
            let bytes = <Vec<u8>>::deserialize(deserializer)?;
            Ok(ScriptData::from_bytes(code_page, &bytes))
        }
    }
}
