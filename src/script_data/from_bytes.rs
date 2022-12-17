use super::*;
use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::map;
use nom::error::ParseError;
use nom::multi::many0;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Eq, Clone, Copy)]
struct Void(!);

impl PartialEq for Void {
    fn eq(&self, _: &Self) -> bool { self.0 }

    #[allow(clippy::partialeq_ne_impl)]
    fn ne(&self, _: &Self) -> bool { self.0 }
}

impl Ord for Void {
    fn cmp(&self, _: &Self) -> Ordering { self.0 }

    fn max(self, _: Self) -> Self { self.0 }

    fn min(self, _: Self) -> Self { self.0 }

    fn clamp(self, _: Self, _: Self) -> Self { self.0 }
}

impl PartialOrd for Void {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> { self.0 }

    fn lt(&self, _: &Self) -> bool { self.0 }

    fn le(&self, _: &Self) -> bool { self.0 }

    fn gt(&self, _: &Self) -> bool { self.0 }

    fn ge(&self, _: &Self) -> bool { self.0 }
}

impl Debug for Void {
    fn fmt(&self, _: &mut Formatter) -> fmt::Result { self.0 }
}

impl Hash for Void {
    fn hash<H: Hasher>(&self, _: &mut H) {
        self.0
    }
}

impl<I> ParseError<I> for Void {
    fn from_error_kind(_input: I, _kind: nom::error::ErrorKind) -> Self { panic!() }

    fn append(_input: I, _kind: nom::error::ErrorKind, other: Self) -> Self { other.0 }

    fn or(self, other: Self) -> Self { other.0 }

    fn from_char(_input: I, _: char) -> Self { panic!() }
}

fn set_stmt(input: &[u8]) -> IResult<&[u8], ScriptItem, Void> {
    map(tag([0x05u8, 0x01u8]), |_| ScriptItem::SetStmt)(input)
}

fn raw(input: &[u8]) -> IResult<&[u8], ScriptItem, Void> {
    map(take(input.len()), |bytes: &[u8]| ScriptItem::Raw(bytes.into()))(input)
}

fn item(input: &[u8]) -> IResult<&[u8], ScriptItem, Void> {
    alt((set_stmt, raw))(input)
}

fn data(input: &[u8]) -> IResult<&[u8], ScriptData, Void> {
    map(many0(item), ScriptData)(input)
}

pub fn script_data_from_bytes(bytes: &[u8]) -> ScriptData {
    data(bytes).map_or_else(|e| match e {
        nom::Err::Incomplete(_) => unreachable!(),
        nom::Err::Error(e) => e.0,
        nom::Err::Failure(e) => e.0
    }, |x| x.1)
}
