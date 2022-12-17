mod from_bytes;
use from_bytes::*;

use std::slice::{self};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::{self};

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ScriptData(Vec<ScriptItem>);
    
impl ScriptData {
    pub fn new(items: Vec<ScriptItem>) -> Option<ScriptData> {
        let this = ScriptData(items);
        let this_from_bytes = Self::from_bytes(&this.bytes().collect::<Vec<_>>());
        if this == this_from_bytes { Some(this) } else { None }
    }

    /// # Safety
    ///
    /// The `items` list should be obtained from [`items`] / [`items_mut`] / [`into_items`] method.
    pub unsafe fn new_unchecked(items: Vec<ScriptItem>) -> ScriptData {
        ScriptData(items)
    }

    pub fn items(&self) -> &[ScriptItem] { &self.0 }

    pub fn items_mut(&mut self) -> &mut [ScriptItem] { &mut self.0 }

    pub fn into_items(self) -> Vec<ScriptItem> { self.0 }

    pub fn bytes(&self) -> impl Iterator<Item=u8> + '_ {
        self.0.iter().flat_map(|item| item.bytes())
    }

    pub fn from_bytes(bytes: &[u8]) -> ScriptData {
        script_data_from_bytes(bytes)
    }
}

impl Serialize for ScriptData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            self.items().serialize(serializer)
        } else {
            self.bytes().collect::<Vec<_>>().serialize(serializer)
        }
    }
}

impl<'de> Deserialize<'de> for ScriptData {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            let items = <Vec<ScriptItem>>::deserialize(deserializer)?;
            Self::new(items).ok_or_else(|| de::Error::custom("invalid script items"))
        } else {
            <Vec<u8>>::deserialize(deserializer).map(|x| Self::from_bytes(&x))
        }
    }
}
