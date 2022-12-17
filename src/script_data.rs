mod from_bytes;
use from_bytes::*;

use std::slice::{self};

#[derive(Debug, Clone)]
pub enum ScriptItem {
    Raw(Vec<u8>),
    SetStmt,
}

enum ScriptItemBytes<'a> {
    Raw(slice::Iter<'a, u8>),
    SetStmt(u8),
}

impl<'a> Iterator for ScriptItemBytes<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        match self {
            ScriptItemBytes::Raw(bytes) => bytes.next().copied(),
            ScriptItemBytes::SetStmt(left) => {
                *left = (*left).checked_sub(1)?;
                match *left {
                    0 => Some(0x01),
                    1 => Some(0x05),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            ScriptItemBytes::Raw(bytes) => bytes.size_hint(),
            &ScriptItemBytes::SetStmt(left) => (left.into(), Some(left.into())),
        }
    }
}

impl ScriptItem {
    pub fn bytes(&self) -> impl Iterator<Item=u8> + '_ {
        match self {
            ScriptItem::Raw(bytes) => ScriptItemBytes::Raw(bytes.iter()),
            ScriptItem::SetStmt => ScriptItemBytes::SetStmt(2),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScriptData(pub Vec<ScriptItem>);
    
impl ScriptData {
    pub fn bytes(&self) -> impl Iterator<Item=u8> + '_ {
        self.0.iter().flat_map(|item| item.bytes())
    }

    pub fn from_bytes(bytes: &[u8]) -> ScriptData {
        script_data_from_bytes(bytes)
    }
}
