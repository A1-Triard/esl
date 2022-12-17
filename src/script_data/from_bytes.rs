use super::*;
use nom_errors::{NomErr, NomRes, alt_2, map, many0, result_from_parser};
use nom_errors::bytes::{tag, take_all};

fn set_stmt(input: &[u8]) -> NomRes<&[u8], ScriptItem, (), !> {
    map(tag([0x05u8, 0x01u8]), |_| ScriptItem::SetStmt)(input)
}

fn raw(input: &[u8]) -> NomRes<&[u8], ScriptItem, !, !> {
    map(take_all(), |bytes: &[u8]| ScriptItem::Raw(bytes.into()))(input)
}

fn item(input: &[u8]) -> NomRes<&[u8], ScriptItem, !, !> {
    alt_2(set_stmt, raw)(input)
}

fn data(input: &[u8]) -> NomRes<&[u8], ScriptData, !, !> {
    map(many0(item), ScriptData)(input)
}

pub fn script_data_from_bytes(bytes: &[u8]) -> ScriptData {
    result_from_parser(data(bytes)).map_or_else(|e| match e {
        NomErr::Error(e) => e,
        NomErr::Failure(e) => e
    }, |x| x.1)
}
