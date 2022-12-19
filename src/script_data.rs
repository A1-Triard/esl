use crate::code::CodePage;
use enum_derive_2018::{EnumDisplay, EnumFromStr};
use enumn::N;
use iter_identify_first_last::IteratorIdentifyFirstLastExt;
use macro_attr_2018::macro_attr;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::DeserializeSeed;
use serde::de::Error as de_Error;
use serde::ser::Error as ser_Error;

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

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(tag="type")]
pub enum ScriptStmt {
    Raw { bytes: Vec<u8> },
    SetLocal { var_type: VarType, var: u16, expr: String },
    SetGlobal,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ScriptData(Vec<ScriptStmt>);

mod parser {
    use super::*;
    use nom_errors::{NomErr, NomRes, alt_5, flat_map, map, many0, result_from_parser, seq_3};
    use nom_errors::bytes::{le_u8, le_u16, tag, take, take_all};

    fn text<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], String, (), !> {
        map(flat_map(le_u8(), |len| take(len.into())), move |x| code_page.decode(x))
    }

    fn set_local<'a>(code_page: CodePage, var_type: u8) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], (u16, String), (), !> {
        map(
            seq_3(
                tag([0x05, 0x01, var_type]),
                le_u16(),
                text(code_page)
            ),
            |(_, var, expr)| (var, expr)
        )
    }

    fn set_global(input: &[u8]) -> NomRes<&[u8], (), (), !> {
        map(tag([0x05, 0x01, b'G']), |_| ())(input)
    }

    fn stmt<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> NomRes<&'a [u8], ScriptStmt, !, !> {
        alt_5(
            map(set_local(code_page, b's'), |(var, expr)| ScriptStmt::SetLocal { var_type: VarType::Short, var, expr }),
            map(set_local(code_page, b'f'), |(var, expr)| ScriptStmt::SetLocal { var_type: VarType::Float, var, expr }),
            map(set_local(code_page, b'l'), |(var, expr)| ScriptStmt::SetLocal { var_type: VarType::Long, var, expr }),
            map(set_global, |()| ScriptStmt::SetGlobal),
            map(take_all(), |bytes: &[u8]| ScriptStmt::Raw { bytes: bytes.into() })
        )
    }

    pub fn is_raw_stmt(code_page: CodePage, bytes: &[u8]) -> bool {
        matches!(result_from_parser(stmt(code_page)(bytes)).map_or_else(|e| match e {
            NomErr::Error(e) => e,
            NomErr::Failure(e) => e
        }, |x| x.1), ScriptStmt::Raw { .. })
    }

    pub fn stmts(code_page: CodePage, bytes: &[u8]) -> Vec<ScriptStmt> {
        result_from_parser(many0(stmt(code_page))(bytes)).map_or_else(|e| match e {
            NomErr::Error(e) => e,
            NomErr::Failure(e) => e
        }, |x| x.1)
    }
}

impl ScriptData {
    pub fn new(stmts: Vec<ScriptStmt>) -> Result<ScriptData, String> {
        let this = ScriptData(stmts);
        this.to_bytes(CodePage::Unicode)?;
        Ok(this)
    }

    /// # Safety
    ///
    /// The `stmts` list should be obtained from [`stmts`] / [`stmts_mut`] / [`into_stmts`] method.
    pub unsafe fn new_unchecked(stmts: Vec<ScriptStmt>) -> ScriptData {
        ScriptData(stmts)
    }

    pub fn stmts(&self) -> &[ScriptStmt] { &self.0 }

    pub fn stmts_mut(&mut self) -> &mut [ScriptStmt] { &mut self.0 }

    pub fn into_stmts(self) -> Vec<ScriptStmt> { self.0 }

    pub fn to_bytes(&self, code_page: CodePage) -> Result<Vec<u8>, String> {
        let mut res = Vec::new();
        for (is_last, stmt) in self.0.iter().identify_last() {
            match stmt {
                ScriptStmt::Raw { bytes } => {
                    if bytes.is_empty() { return Err("the 'raw bytes' should not be empty".into()); }
                    if !is_last { return Err("the 'raw bytes' stmt should be the last".into()); }
                    if !parser::is_raw_stmt(code_page, bytes) {
                        return Err("known stmt signature in raw bytes".into());
                    }
                    res.extend_from_slice(&bytes);
                },
                &ScriptStmt::SetLocal { var_type, var, ref expr } => {
                    res.push(0x05);
                    res.push(0x01);
                    res.push(var_type as u8);
                    res.push((var & 0xFF) as u8);
                    res.push((var >> 8) as u8);
                    let expr_bytes = code_page.encode(expr).map_err(|e| match e {
                        None => format!("the '{expr}' string does not correspond to any source byte sequence"),
                        Some(c) => format!("the '{c}' char is not representable in {code_page:?} code page")
                    })?;
                    let expr_len = expr.len().try_into().map_err(|_| format!("too long expr 'expr'"))?;
                    res.push(expr_len);
                    res.extend_from_slice(&expr_bytes);
                },
                ScriptStmt::SetGlobal => {
                    res.push(0x05);
                    res.push(0x01);
                    res.push(b'G');
                },
            }
        }
        Ok(res)
    }

    pub fn from_bytes(code_page: CodePage, bytes: &[u8]) -> Self {
        ScriptData(parser::stmts(code_page, bytes))
    }
}

pub struct ScriptDataSerializer<'a> {
    pub code_page: Option<CodePage>,
    pub data: &'a ScriptData,
}

impl<'a> Serialize for ScriptDataSerializer<'a> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            self.data.0.serialize(serializer)
        } else {
            let Some(code_page) = self.code_page else {
                return Err(S::Error::custom("code page required for binary serialization"));
            };
            self.data.to_bytes(code_page).serialize(serializer)
        }
    }
}

pub struct ScriptDataDeserializer {
    pub code_page: Option<CodePage>,
}

impl<'de> DeserializeSeed<'de> for ScriptDataDeserializer {
    type Value = ScriptData;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        if deserializer.is_human_readable() {
            let stmts = <Vec<ScriptStmt>>::deserialize(deserializer)?;
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
