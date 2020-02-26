use nom::IResult;
use nom::combinator::{map, flat_map, cut};
use nom::sequence::{pair, tuple, preceded};
use nom::number::complete::{le_u32, le_u64, le_i32, le_i16, le_i64, le_u8, le_f32, le_u16, le_i8};
use nom::error::{ParseError, ErrorKind};
use nom::bytes::complete::take;
use encoding::{DecoderTrap};
use num_traits::cast::FromPrimitive;
use nom::multi::many0;
use std::io::{self, Read, Write};
use std::error::Error;
use std::fmt::{self};
use std::mem::replace;
use flate2::write::ZlibEncoder;
use flate2::Compression;
use either::{Right, Left};

use crate::strings::*;
use crate::field::*;
use crate::record::*;
use crate::code::CodePage;

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Clone, Copy)]
enum Void { }

impl Void {
    fn unreachable<T>(self) -> T { unreachable!() }
}

impl<I> ParseError<I> for Void {
    fn from_error_kind(_input: I, _kind: ErrorKind) -> Self { panic!() }

    fn append(_input: I, _kind: ErrorKind, _other: Self) -> Self { unreachable!() }

    fn or(self, _other: Self) -> Self { unreachable!() }

    fn add_context(_input: I, _ctx: &'static str, _other: Self) -> Self { unreachable!() }
}

fn map_err<I: Clone, O, E, X, F>(f: F, m: impl Fn(E, I) -> X)
    -> impl Fn(I) -> IResult<I, O, X> where
    F: Fn(I) -> IResult<I, O, E> {
    
    move |input: I| {
        match f(input.clone()) {
            Err(nom::Err::Error(e)) => Err(nom::Err::Error(m(e, input))),
            Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(m(e, input))),
            Err(nom::Err::Incomplete(n)) => Err(nom::Err::Incomplete(n)),
            Ok(r) => Ok(r)
        }
    }
}

fn no_err<I: Clone, O, X, F>(f: F) -> impl Fn(I) -> IResult<I, O, X> where
    F: Fn(I) -> IResult<I, O, Void> {

    map_err(
        f,
        |x, _| x.unreachable()
    )
}

fn set_err<I: Clone, O, X, F>(f: F, m: impl Fn(I) -> X)
                                 -> impl Fn(I) -> IResult<I, O, X> where
    F: Fn(I) -> IResult<I, O, ()> {

    move |input: I| {
        match f(input.clone()) {
            Err(nom::Err::Error(())) => Err(nom::Err::Error(m(input))),
            Err(nom::Err::Failure(())) => Err(nom::Err::Failure(m(input))),
            Err(nom::Err::Incomplete(n)) => Err(nom::Err::Incomplete(n)),
            Ok(r) => Ok(r)
        }
    }
}

fn map_res<I: Clone, O, E, R, F>(f: F, m: impl Fn(O, I) -> Result<R, nom::Err<E>>)
    -> impl Fn(I) -> IResult<I, R, E> where
    F: Fn(I) -> IResult<I, O, E> {

    move |input: I| {
        match f(input.clone()) {
            Err(nom::Err::Error(e)) => Err(nom::Err::Error(e)),
            Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(e)),
            Err(nom::Err::Incomplete(n)) => Err(nom::Err::Incomplete(n)),
            Ok((i, r)) => m(r, input).map(|x| (i, x)),
        }
    }
}

trait ErrExt<E>: Sized {
    fn into_err(self) -> nom::Err<E>;
    fn unwrap(self) -> E {
        match self.into_err() {
            nom::Err::Error(e) => e,
            nom::Err::Failure(e) => e,
            nom::Err::Incomplete(_) => panic!()
        }
    }
}

impl<E> ErrExt<E> for nom::Err<E> {
    fn into_err(self) -> nom::Err<E> { self }
}

macro_rules! impl_parse_error {
    (<$($lifetimes:lifetime),+>, $input:ty, $name:ident) => {
        impl<$($lifetimes),+> ::nom::error::ParseError<$input> for $name {
            fn from_error_kind(_input: $input, kind: ::nom::error::ErrorKind) -> Self { panic!(format!("{:?}", kind)) }
        
            fn append(_input: $input, _kind: ::nom::error::ErrorKind, _other: Self) -> Self { panic!() }
        
            fn or(self, _other: Self) -> Self { panic!() }
        
            fn add_context(_input: $input, _ctx: &'static str, _other: Self) -> Self { panic!() }
        }
    };
    (<$($lifetime:lifetime),+>, $input:ty, $name:ident<$($name_lt:lifetime),+>) => {
        impl<$($lifetime),+> ::nom::error::ParseError<$input> for $name<$($name_lt),+> {
            fn from_error_kind(_input: $input, kind: ::nom::error::ErrorKind) -> Self { panic!(format!("{:?}", kind)) }
        
            fn append(_input: $input, _kind: ::nom::error::ErrorKind, _other: Self) -> Self { panic!() }
        
            fn or(self, _other: Self) -> Self { panic!() }
        
            fn add_context(_input: $input, _ctx: &'static str, _other: Self) -> Self { panic!() }
        }
    }
}

#[derive(Debug, Clone)]
enum FieldBodyError {
    UnexpectedEndOfField(u32),
    UnknownValue(Unknown, u32),
    UnexpectedFieldSize(u32)
}

impl_parse_error!(<'a>, &'a [u8], FieldBodyError);

fn u8_list_field<E>(input: &[u8]) -> IResult<&[u8], Vec<u8>, E> {
    Ok((&input[input.len() .. ], input.into()))
}

fn u8_list_zip_field<E>(input: &[u8]) -> IResult<&[u8], Vec<u8>, E> {
    let mut encoder = ZlibEncoder::new(Vec::new(), Compression::new(5));
    encoder.write_all(input).unwrap();
    Ok((&input[input.len() .. ], encoder.finish().unwrap()))
}

fn trim_end_nulls(bytes: &[u8]) -> &[u8] {
    let cut_to = bytes.iter().rposition(|&x| x != 0).map_or(0, |i| i + 1);
    &bytes[..cut_to]
}

fn decode_string(code_page: CodePage, bytes: &[u8]) -> String {
    code_page.encoding().decode(bytes, DecoderTrap::Strict).unwrap()
}

fn string_field<E>(code_page: CodePage) -> impl Fn(&[u8]) -> IResult<&[u8], String, E> {
    move |input| {
        Ok((&input[input.len()..], decode_string(code_page, input)))
    }
}

fn string_z_field<E>(code_page: CodePage) -> impl Fn(&[u8]) -> IResult<&[u8], StringZ, E> {
    move |input| {
        Ok((&input[input.len()..], {
            let has_tail_zero = input.last() == Some(&0);
            let input = if has_tail_zero {
                &input[..input.len() - 1]                
            } else {
                input
            };
            StringZ { string: decode_string(code_page, input), has_tail_zero }
        }))
    }
}

fn string_z_list_field<'a, E>(code_page: CodePage) -> impl Fn(&'a [u8]) 
    -> IResult<&'a [u8], StringZList, E> {
    
    no_err(
        map(
            string_z_field(code_page),
            |s| StringZList { vec: s.string.split("\0").map(String::from).collect(), has_tail_zero: s.has_tail_zero }
        )
    )
}

fn string_len<'a>(code_page: CodePage, length: u32) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], String, ()> {
    map(take(length), move |bytes| decode_string(code_page, trim_end_nulls(bytes)))
}

fn file_metadata_field<'a>(code_page: CodePage)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], FileMetadata, FieldBodyError> {

    map(
        tuple((
            set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(300)),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(300)),
                |w, _| FileType::from_u32(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::FileType(w), 4).into()))
            ),
            set_err(
                tuple((
                    string_len(code_page, 32),
                    map(
                        string_len(code_page, 256),
                        |s| s.split(Newline::Dos.as_str()).map(String::from).collect()
                    ),
                    le_u32
                )),
                |_| FieldBodyError::UnexpectedEndOfField(300)
            )
        )),
        |(version, file_type, (author, description, records))| FileMetadata {
            version, file_type, author, description, records
        }
    )
}

fn string_len_field<'a>(code_page: CodePage, length: u32) -> impl Fn(&'a [u8])
    -> IResult<&'a [u8], String, FieldBodyError> {
    
    set_err(string_len(code_page, length), move |_| FieldBodyError::UnexpectedEndOfField(length))
}

fn multiline_field<'a, E>(code_page: CodePage, linebreaks: Newline)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<String>, E> {

    no_err(
        map(
            string_field(code_page),
            move |s| s.split(linebreaks.as_str()).map(String::from).collect()
        )
    )
}

fn item_field<'a>(code_page: CodePage) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Item, FieldBodyError> {
    set_err(
        map(
            pair(
                le_i32,
                string_len(code_page, 32)
            ),
            |(count, item_id)| Item { count, item_id }
        ),
        |_| FieldBodyError::UnexpectedEndOfField(4 + 32)
    )
}

fn i32_field(input: &[u8]) -> IResult<&[u8], i32, FieldBodyError> {
    set_err(le_i32, |_| FieldBodyError::UnexpectedEndOfField(4))(input)
}

fn i32_list_field(input: &[u8]) -> IResult<&[u8], Vec<i32>, FieldBodyError> {
    let m = input.len() % 4;
    if m != 0 {
        return Err(nom::Err::Error(FieldBodyError::UnexpectedEndOfField((input.len() - m + 4) as u32)));
    }
    set_err(many0(le_i32), |_| unreachable!())(input)
}

fn i16_field(input: &[u8]) -> IResult<&[u8], i16, FieldBodyError> {
    set_err(le_i16, |_| FieldBodyError::UnexpectedEndOfField(2))(input)
}

fn i16_list_field(input: &[u8]) -> IResult<&[u8], Vec<i16>, FieldBodyError> {
    let m = input.len() % 2;
    if m != 0 {
        return Err(nom::Err::Error(FieldBodyError::UnexpectedEndOfField((input.len() - m + 2) as u32)));
    }
    set_err(many0(le_i16), |_| unreachable!())(input)
}

fn i64_field(input: &[u8]) -> IResult<&[u8], i64, FieldBodyError> {
    set_err(le_i64, |_| FieldBodyError::UnexpectedEndOfField(8))(input)
}

fn u8_field(input: &[u8]) -> IResult<&[u8], u8, FieldBodyError> {
    set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(1))(input)
}

fn f32_field(input: &[u8]) -> IResult<&[u8], f32, FieldBodyError> {
    set_err(le_f32, |_| FieldBodyError::UnexpectedEndOfField(4))(input)
}

fn f32_list_field(input: &[u8]) -> IResult<&[u8], Vec<f32>, FieldBodyError> {
    let m = input.len() % 4;
    if m != 0 {
        return Err(nom::Err::Error(FieldBodyError::UnexpectedEndOfField((input.len() - m + 4) as u32)));
    }
    set_err(many0(le_f32), |_| unreachable!())(input)
}

fn ingredient_field(input: &[u8]) -> IResult<&[u8], Ingredient, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_f32, le_u32,
                le_i32, le_i32, le_i32, le_i32,
                le_i32, le_i32, le_i32, le_i32,
                le_i32, le_i32, le_i32, le_i32,
            )),
            |_| FieldBodyError::UnexpectedEndOfField(56)
        ),
        |(weight, value, e1, e2, e3, e4, s1, s2, s3, s4, a1, a2, a3, a4)| Ingredient {
            weight,
            value,
            effects: [e1, e2, e3, e4],
            skills: [s1, s2, s3, s4],
            attributes: [a1, a2, a3, a4]
        }
    )(input)
}

fn sound_chance_field<'a>(code_page: CodePage)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], SoundChance, FieldBodyError> {

    map(
        set_err(
            pair(
                string_len(code_page, 32),
                le_u8
            ),
            |_| FieldBodyError::UnexpectedEndOfField(32 + 1)
        ),
        |(sound_id, chance)| SoundChance { sound_id, chance }
    )
}

fn script_metadata_field<'a>(code_page: CodePage)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], ScriptMetadata, FieldBodyError> {

    map(
        set_err(
            tuple((
                string_len(code_page, 32),
                le_u32, le_u32, le_u32,
                le_u32, le_u32,
            )),
            |_| FieldBodyError::UnexpectedEndOfField(32 + 20)
        ),
        |(name, shorts, longs, floats, data_size, var_table_size)| ScriptMetadata {
            name,
            vars: ScriptVars {
                shorts,
                longs,
                floats
            },
            data_size,
            var_table_size
        }
    )
}

fn script_vars_field(input: &[u8]) -> IResult<&[u8], ScriptVars, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_u32, le_u32, le_u32
            )),
            |_| FieldBodyError::UnexpectedEndOfField(12)
        ),
        |(shorts, longs, floats)| ScriptVars {
            shorts,
            longs,
            floats
        }
    )(input)
}

fn weather_field(input: &[u8]) -> IResult<&[u8], Weather, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_u8, le_u8, le_u8, le_u8, le_u8,
                le_u8, le_u8, le_u8
            )),
            |_| FieldBodyError::UnexpectedEndOfField(8)
        ),
        |(clear, cloudy, foggy, overcast, rain, thunder, ash, blight)| Weather {
            clear, cloudy, foggy, overcast, rain, thunder, ash, blight, ex: None
        }
    )(input)
}

fn weather_ex_field(input: &[u8]) -> IResult<&[u8], Weather, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_u8, le_u8, le_u8, le_u8, le_u8,
                le_u8, le_u8, le_u8, le_u8, le_u8
            )),
            |_| FieldBodyError::UnexpectedEndOfField(10)
        ),
        |(clear, cloudy, foggy, overcast, rain, thunder, ash, blight, snow, blizzard)| Weather {
            clear, cloudy, foggy, overcast, rain, thunder, ash, blight,
            ex: Some(WeatherEx { snow, blizzard })
        }
    )(input)
}

fn npc_state_field(input: &[u8]) -> IResult<&[u8], NpcState, FieldBodyError> {
    map(
        set_err(
            tuple((le_i16, le_i16, le_u32)),
            |_| FieldBodyError::UnexpectedEndOfField(8)
        ),
        |(disposition, reputation, index)| NpcState {
            disposition,
            reputation,
            index
        }
    )(input)
}

fn cell_field(input: &[u8]) -> IResult<&[u8], Cell, FieldBodyError> {
    map(
        pair(
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
                |w, _| CellFlags::from_bits(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::CellFlags(w), 0).into()))
            ),
            set_err(
                pair(le_i32, le_i32),
                |_| FieldBodyError::UnexpectedEndOfField(12)
            )
        ),
        |(flags, (x, y))| Cell {
            flags, grid: Grid { x, y }
        }
    )(input)
}

fn path_grid_field(input: &[u8]) -> IResult<&[u8], PathGrid, FieldBodyError> {
    map(
        set_err(
            tuple((le_i32, le_i32, le_u16, le_u16)),
            |_| FieldBodyError::UnexpectedEndOfField(12)
        ),
        |(x, y, flags, points)| PathGrid { grid: Grid { x, y }, flags, points }
    )(input)
}

fn grid_field(input: &[u8]) -> IResult<&[u8], Grid, FieldBodyError> {
    map(
        set_err(
            pair(le_i32, le_i32),
            |_| FieldBodyError::UnexpectedEndOfField(8)
        ),
        |(x, y)| Grid { x, y }
    )(input)
}

fn spell_metadata_field(input: &[u8]) -> IResult<&[u8], SpellMetadata, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
                |w, _| SpellType::from_u32(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::SpellType(w), 0).into()))
            ),
            set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
                |w, _| SpellFlags::from_bits(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::SpellFlags(w), 8).into()))
            ),
        )),
        |(spell_type, cost, flags)| SpellMetadata {
            spell_type, cost, flags
        }
    )(input)
}

fn light_field(input: &[u8]) -> IResult<&[u8], Light, FieldBodyError> {
    map(
        tuple((
            set_err(
                tuple((le_f32, le_u32, le_i32, le_u32)),
                |_| FieldBodyError::UnexpectedEndOfField(24)
            ),
            map(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                Color
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                |w, _| LightFlags::from_bits(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::LightFlags(w), 20).into()))
            )
        )),
        |((weight, value, time, radius), color, flags)| Light {
            weight, value, time, radius, color, flags
        }
    )(input)
}

fn color_field(input: &[u8]) -> IResult<&[u8], Color, FieldBodyError> {
    map(
        set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(4)),
        Color
    )(input)
}

fn misc_item_field(input: &[u8]) -> IResult<&[u8], MiscItem, FieldBodyError> {
    map(
        set_err(
            tuple((le_f32, le_u32, le_u32)),
            |_| FieldBodyError::UnexpectedEndOfField(12)
        ),
        |(weight, value, is_key)| MiscItem {
            weight, value, is_key
        }
    )(input)
}

fn apparatus_field(input: &[u8]) -> IResult<&[u8], Apparatus, FieldBodyError> {
    map(
        pair(
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(16)),
                |w, _| ApparatusType::from_u32(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::ApparatusType(w), 0).into()))
            ),
            set_err(
                tuple((le_f32, le_f32, le_u32)),
                |_| FieldBodyError::UnexpectedEndOfField(16)
            ),
        ),
        |(apparatus_type, (quality, weight, value))| Apparatus {
            apparatus_type, quality, weight, value
        }
    )(input)
}

fn enchantment_field(input: &[u8]) -> IResult<&[u8], Enchantment, FieldBodyError> {
    map(
        pair(
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(16)),
                |w, _| EnchantmentType::from_u32(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::EnchantmentType(w), 0).into()))
            ),
            set_err(
                tuple((le_u32, le_u32, le_u32)),
                |_| FieldBodyError::UnexpectedEndOfField(16)
            ),
        ),
        |(enchantment_type, (cost, charge_amount, auto_calculate))| Enchantment {
            enchantment_type, cost, charge_amount, auto_calculate
        }
    )(input)
}

fn armor_field(input: &[u8]) -> IResult<&[u8], Armor, FieldBodyError> {
    map(
        pair(
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                |w, _| ArmorType::from_u32(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::ArmorType(w), 0).into()))
            ),
            set_err(
                tuple((le_f32, le_u32, le_u32, le_u32, le_u32)),
                |_| FieldBodyError::UnexpectedEndOfField(24)
            ),
        ),
        |(armor_type, (weight, value, health, enchantment, armor))| Armor {
            armor_type, health, weight, value, enchantment, armor
        }
    )(input)
}

fn clothing_field(input: &[u8]) -> IResult<&[u8], Clothing, FieldBodyError> {
    map(
        pair(
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
                |w, _| ClothingType::from_u32(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::ClothingType(w), 0).into()))
            ),
            set_err(
                tuple((le_f32, le_u16, le_u16)),
                |_| FieldBodyError::UnexpectedEndOfField(12)
            ),
        ),
        |(clothing_type, (weight, value, enchantment))| Clothing {
            clothing_type, weight, value, enchantment
        }
    )(input)
}

fn weapon_field(input: &[u8]) -> IResult<&[u8], Weapon, FieldBodyError> {
    map(
        tuple((
            set_err(
                tuple((le_f32, le_u32)),
                |_| FieldBodyError::UnexpectedEndOfField(32)
            ),
            map_res(
                set_err(le_u16, |_| FieldBodyError::UnexpectedEndOfField(32)),
                |w, _| WeaponType::from_u16(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::WeaponType(w), 8).into()))
            ),
            set_err(
                tuple((le_u16, le_f32, le_f32, le_u16, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8)),
                |_| FieldBodyError::UnexpectedEndOfField(32)
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(32)),
                |w, _| WeaponFlags::from_bits(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::WeaponFlags(w), 28).into()))
            )
        )),
        |((weight, value), weapon_type, (health, speed, reach, enchantment, chop_min, chop_max, slash_min, slash_max, thrust_min, thrust_max), flags)| Weapon {
            weight, value, weapon_type, health, speed, reach, enchantment, chop_min, chop_max, slash_min, slash_max, thrust_min, thrust_max, flags
        }
    )(input)
}

fn body_part_field(input: &[u8]) -> IResult<&[u8], BodyPart, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| BodyPartKind::from_u8(b).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::BodyPartKind(b), 0).into()))
            ),
            set_err(
                le_u8,
                |_| FieldBodyError::UnexpectedEndOfField(4)
            ),
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| BodyPartFlags::from_bits(b).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::BodyPartFlags(b), 2).into()))
            ),
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| BodyPartType::from_u8(b).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::BodyPartType(b), 3).into()))
            )
        )),
        |(kind, vampire, flags, body_part_type)| BodyPart {
            kind, vampire, flags, body_part_type
        }
    )(input)
}

fn ai_field(input: &[u8]) -> IResult<&[u8], Ai, FieldBodyError> {
    map(
        pair(
            set_err(
                tuple((le_u16, le_u8, le_u8, le_u8, le_u8, le_u16)),
                |_| FieldBodyError::UnexpectedEndOfField(12)
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
                |w, _| AiServices::from_bits(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::AiServices(w), 8).into()))
            )
        ),
        |((hello, fight, flee, alarm, padding_8, padding_16), services)| Ai {
            hello, fight, flee, alarm, padding_8, padding_16, services
        }
    )(input)
}

fn ai_wander_field(input: &[u8]) -> IResult<&[u8], AiWander, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_u16, le_u16, le_u8,
                le_u8, le_u8, le_u8, le_u8,
                le_u8, le_u8, le_u8, le_u8,
                le_u8
            )),
            |_| FieldBodyError::UnexpectedEndOfField(14)
        ),
        |(distance, duration, time_of_day, idle2, idle3, idle4, idle5, idle6, idle7, idle8, idle9, repeat)| AiWander {
            distance,
            duration,
            time_of_day,
            idle: [idle2, idle3, idle4, idle5, idle6, idle7, idle8, idle9],
            repeat
        }
    )(input)
}

fn ai_travel_field(input: &[u8]) -> IResult<&[u8], AiTravel, FieldBodyError> {
    map(
        pair(
            set_err(
                tuple((
                    le_f32, le_f32, le_f32
                )),
                |_| FieldBodyError::UnexpectedEndOfField(16)
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |w, _| AiTravelFlags::from_bits(w ^ 0x01).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::AiTravelFlags(w), 12).into()))
            )
        ),
        |((x, y, z), flags)| AiTravel {
            x, y, z, flags
        }
    )(input)
}

fn ai_target_field<'a>(code_page: CodePage) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], AiTarget, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_f32, le_f32, le_f32, le_u16,
                string_len(code_page, 32),
                le_u16
            )),
            |_| FieldBodyError::UnexpectedEndOfField(48)
        ),
        |(x, y, z, duration, actor_id, reset)| AiTarget {
            x, y, z, duration, actor_id, reset
        }
    )
}

fn ai_activate_field<'a>(code_page: CodePage) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], AiActivate, FieldBodyError> {
    map(
        set_err(
            pair(
                string_len(code_page, 32),
                le_u8
            ),
            |_| FieldBodyError::UnexpectedEndOfField(33)
        ),
        |(object_id, reset)| AiActivate {
            object_id, reset
        }
    )
}

fn position_field(input: &[u8]) -> IResult<&[u8], Position, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_f32, le_f32, le_f32,
                le_f32, le_f32, le_f32,
            )),
            |_| FieldBodyError::UnexpectedEndOfField(24)
        ),
        |(x, y, z, x_rot, y_rot, z_rot)| Position {
            x, y, z, x_rot, y_rot, z_rot
        }
    )(input)
}

fn npc_flags_field(input: &[u8]) -> IResult<&[u8], FlagsAndBlood<NpcFlags>, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| NpcFlags::from_bits(b ^ 0x08).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::NpcFlags(b), 0).into()))
            ),
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| Blood::from_u8(b).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::Blood(b), 1).into()))
            ),
            set_err(le_u16, |_| FieldBodyError::UnexpectedEndOfField(4)),
        )),
        |(flags, blood, padding)| FlagsAndBlood {
            flags,
            blood,
            padding
        }
    )(input)
}

fn creature_flags_field(input: &[u8]) -> IResult<&[u8], FlagsAndBlood<CreatureFlags>, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| CreatureFlags::from_bits(b ^ 0x08).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::CreatureFlags(b), 0).into()))
            ),
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| Blood::from_u8(b).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::Blood(b), 1).into()))
            ),
            set_err(le_u16, |_| FieldBodyError::UnexpectedEndOfField(4)),
        )),
        |(flags, blood, padding)| FlagsAndBlood {
            flags,
            blood,
            padding
        }
    )(input)
}

fn container_flags_field(input: &[u8]) -> IResult<&[u8], ContainerFlags, FieldBodyError> {
    map_res(
        set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(4)),
        |w, _| ContainerFlags::from_bits(w ^ 0x08).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::ContainerFlags(w), 0).into()))
    )(input)
}

fn biped_object_field(input: &[u8]) -> IResult<&[u8], BipedObject, FieldBodyError> {
    map_res(
        set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(1)),
        |b, _| BipedObject::from_u8(b).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::BipedObject(b), 0).into()))
    )(input)
}

fn book_field(input: &[u8]) -> IResult<&[u8], Book, FieldBodyError> {
    map(
        set_err(
            tuple((le_f32, le_u32, le_u32, le_i32, le_u32)),
            |_| FieldBodyError::UnexpectedEndOfField(20)
        ),
        |(weight, value, scroll, skill, enchantment)| Book {
            weight, value, scroll, skill, enchantment
        }
    )(input)
}

fn tool_field(input: &[u8]) -> IResult<&[u8], Tool, FieldBodyError> {
    map(
        set_err(
            tuple((le_f32, le_u32, le_f32, le_u32)),
            |_| FieldBodyError::UnexpectedEndOfField(16)
        ),
        |(weight, value, quality, uses)| Tool {
            weight, value, quality, uses
        }
    )(input)
}

fn repair_item_field(input: &[u8]) -> IResult<&[u8], RepairItem, FieldBodyError> {
    map(
        set_err(
            tuple((le_f32, le_u32, le_u32, le_f32)),
            |_| FieldBodyError::UnexpectedEndOfField(16)
        ),
        |(weight, value, uses, quality)| RepairItem {
            weight, value, quality, uses
        }
    )(input)
}

fn npc_stats(input: &[u8]) -> IResult<&[u8], NpcStats, ()> {
    map(
        tuple((
            tuple((le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8)),
            tuple((le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8)),
            tuple((le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8)),
            tuple((le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8)),
            tuple((le_i16, le_i16, le_i16))
        )),
        |(
            (strength, intelligence, willpower, agility, speed, endurance, personality, luck, block),
            (armorer, medium_armor, heavy_armor, blunt_weapon, long_blade, axe, spear, athletics, enchant),
            (destruction, alteration, illusion, conjuration, mysticism, restoration, alchemy, unarmored, security), 
            (sneak, acrobatics, light_armor, short_blade, marksman, mercantile, speechcraft, hand_to_hand, faction),
            (health, magicka, fatigue)
        )| NpcStats {
            strength, intelligence, willpower, agility, speed,
            endurance, personality, luck, block,
            armorer, medium_armor, heavy_armor, blunt_weapon,
            long_blade, axe, spear, athletics,
            enchant, destruction, alteration, illusion,
            conjuration, mysticism, restoration, alchemy,
            unarmored, security, sneak, acrobatics, light_armor,
            short_blade, marksman, mercantile, speechcraft,
            hand_to_hand, faction, health, magicka, fatigue
        }
    )(input)
}

fn creature_field(input: &[u8]) -> IResult<&[u8], Creature, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(96)),
                |w, _| CreatureType::from_u32(w).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::CreatureType(w), 0).into()))
            ),
            set_err(
                tuple((
                    tuple((le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32)),
                    tuple((le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32)),
                    tuple((le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32))
                )),
                |_| FieldBodyError::UnexpectedEndOfField(96)
            )
        )),
        |(
            creature_type,
            (
                (level, strength, intelligence, willpower, agility, speed, endurance, personality),
                (luck, health, magicka, fatigue, soul, combat, magic, stealth),
                (attack_1_min, attack_1_max, attack_2_min, attack_2_max, attack_3_min, attack_3_max, gold)
            )
        )| Creature {
            creature_type,
            level, strength, intelligence, willpower, agility, speed, endurance, personality,
            luck, health, magicka, fatigue, soul, combat, magic, stealth,
            attack_1_min, attack_1_max, attack_2_min, attack_2_max, attack_3_min, attack_3_max, gold
        }
    )(input)
}

fn npc_52_field(input: &[u8]) -> IResult<&[u8], Npc, FieldBodyError> {
    map(
        set_err(
            tuple((le_u16, npc_stats, le_i8, le_i8, le_i8, le_u8, le_i32)),
            |_| FieldBodyError::UnexpectedEndOfField(52)
        ),
        |(level, stats, disposition, reputation, rank, padding, gold)| Npc {
            level, disposition, reputation, rank, gold,
            stats: Right(stats), padding
        }.into()
    )(input)
}

fn npc_12_field(input: &[u8]) -> IResult<&[u8], Npc, FieldBodyError> {
    map(
        set_err(
            tuple((le_u16, le_i8, le_i8, le_i8, le_u8, le_u16, le_i32)),
            |_| FieldBodyError::UnexpectedEndOfField(12)
        ),
        |(level, disposition, reputation, rank, padding_8, padding_16, gold)| Npc {
            level, disposition, reputation, rank, gold,
            padding: padding_8, stats: Left(padding_16)
        }.into()
    )(input)
}

fn effect_field(input: &[u8]) -> IResult<&[u8], Effect, FieldBodyError> {
    map(
        tuple((
            set_err(
                tuple((le_i16, le_i8, le_i8)),
                |_| FieldBodyError::UnexpectedEndOfField(24)
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                |d, _| EffectRange::from_u32(d).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::EffectRange(d), 4).into()))
            ),
            set_err(
                tuple((le_i32, le_i32, le_i32, le_i32)),
                |_| FieldBodyError::UnexpectedEndOfField(24)
            ),
        )),
        |(
            (id, skill, attribute),
            range,
            (area, duration, magnitude_min, magnitude_max),
        )| Effect {
            effect_id: id, skill, attribute, range,
            area, duration, magnitude_min, magnitude_max
        }
    )(input)
}

fn dialog_type_field(input: &[u8]) -> IResult<&[u8], DialogType, FieldBodyError> {
    map_res(
        set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(1)),
        |b, _| DialogType::from_u8(b).ok_or(nom::Err::Error(FieldBodyError::UnknownValue(Unknown::DialogType(b), 0).into()))
    )
    (input)
}

fn field_body<'a>(code_page: CodePage, record_tag: Tag, field_tag: Tag, field_size: u32)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Field, FieldBodyError> {

    move |input| {
        let field_type = FieldType::from_tags(record_tag, field_tag);
        match field_type {
            FieldType::U8List => map(u8_list_field, Field::U8List)(input),
            FieldType::U8ListZip => map(u8_list_zip_field, Field::U8List)(input),
            FieldType::Multiline(newline) => map(multiline_field(code_page, newline), Field::StringList)(input),
            FieldType::Item => map(item_field(code_page), Field::Item)(input),
            FieldType::String(Some(len)) => map(string_len_field(code_page, len), Field::String)(input),
            FieldType::String(None) => map(string_field(code_page), Field::String)(input),
            FieldType::StringZ => map(string_z_field(code_page), Field::StringZ)(input),
            FieldType::StringZList => map(string_z_list_field(code_page), Field::StringZList)(input),
            FieldType::FileMetadata => map(file_metadata_field(code_page), Field::FileMetadata)(input),
            FieldType::SpellMetadata => map(spell_metadata_field, Field::SpellMetadata)(input),
            FieldType::Ai => map(ai_field, Field::Ai)(input),
            FieldType::AiWander => map(ai_wander_field, Field::AiWander)(input),
            FieldType::AiTravel => map(ai_travel_field, Field::AiTravel)(input),
            FieldType::AiTarget => map(ai_target_field(code_page), Field::AiTarget)(input),
            FieldType::AiActivate => map(ai_activate_field(code_page), Field::AiActivate)(input),
            FieldType::NpcFlags => map(npc_flags_field, Field::NpcFlags)(input),
            FieldType::CreatureFlags => map(creature_flags_field, Field::CreatureFlags)(input),
            FieldType::Book => map(book_field, Field::Book)(input),
            FieldType::Light => map(light_field, Field::Light)(input),
            FieldType::MiscItem => map(misc_item_field, Field::MiscItem)(input),
            FieldType::Apparatus => map(apparatus_field, Field::Apparatus)(input),
            FieldType::Armor => map(armor_field, Field::Armor)(input),
            FieldType::Weapon => map(weapon_field, Field::Weapon)(input),
            FieldType::Position => map(position_field, Field::Position)(input),
            FieldType::Tool => map(tool_field, Field::Tool)(input),
            FieldType::RepairItem => map(repair_item_field, |x| Field::Tool(x.into()))(input),
            FieldType::BipedObject => map(biped_object_field, Field::BipedObject)(input),
            FieldType::BodyPart => map(body_part_field, Field::BodyPart)(input),
            FieldType::Clothing => map(clothing_field, Field::Clothing)(input),
            FieldType::Enchantment => map(enchantment_field, Field::Enchantment)(input),
            FieldType::Creature => map(creature_field, Field::Creature)(input),
            FieldType::ContainerFlags => map(container_flags_field, Field::ContainerFlags)(input),
            FieldType::Grid => map(grid_field, Field::Grid)(input),
            FieldType::Color => map(color_field, Field::Color)(input),
            FieldType::I32 => map(i32_field, Field::I32)(input),
            FieldType::I16 => map(i16_field, Field::I16)(input),
            FieldType::I64 => map(i64_field, Field::I64)(input),
            FieldType::U8 => map(u8_field, Field::U8)(input),
            FieldType::F32 => map(f32_field, Field::F32)(input),
            FieldType::I32List => map(i32_list_field, Field::I32List)(input),
            FieldType::I16List => map(i16_list_field, Field::I16List)(input),
            FieldType::F32List => map(f32_list_field, Field::F32List)(input),
            FieldType::SoundChance => map(sound_chance_field(code_page), Field::SoundChance)(input),
            FieldType::Ingredient => map(ingredient_field, Field::Ingredient)(input),
            FieldType::ScriptMetadata => map(script_metadata_field(code_page), Field::ScriptMetadata)(input),
            FieldType::ScriptVars => map(script_vars_field, Field::ScriptVars)(input),
            FieldType::NpcState => map(npc_state_field, Field::NpcState)(input),
            FieldType::Npc => match field_size {
                52 => map(npc_52_field, Field::Npc)(input),
                12 => map(npc_12_field, Field::Npc)(input),
                x => Err(nom::Err::Error(FieldBodyError::UnexpectedFieldSize(x))),
            },
            FieldType::PathGrid => map(path_grid_field, Field::PathGrid)(input),
            FieldType::Effect => map(effect_field, Field::Effect)(input),
            FieldType::DialogMetadata => match field_size {
                4 => map(i32_field, Field::I32)(input),
                1 => map(dialog_type_field, Field::DialogType)(input),
                x => Err(nom::Err::Error(FieldBodyError::UnexpectedFieldSize(x))),
            },
            FieldType::PositionOrCell => match field_size {
                24 => map(position_field, Field::Position)(input),
                12 => map(cell_field, Field::Cell)(input),
                x => Err(nom::Err::Error(FieldBodyError::UnexpectedFieldSize(x))),
            },
            FieldType::Weather => match field_size {
                8 => map(weather_field, Field::Weather)(input),
                10 => map(weather_ex_field, Field::Weather)(input),
                x => Err(nom::Err::Error(FieldBodyError::UnexpectedFieldSize(x))),
            },
        }
    }
}

fn tag(input: &[u8]) -> IResult<&[u8], Tag, ()> {
    map(le_u32, Tag::from)(input)
}

#[derive(Debug, Clone)]
enum FieldError {
    UnexpectedEndOfRecord(u32),
    FieldSizeMismatch(Tag, u32, u32),
    UnknownValue(Tag, Unknown, u32),
    UnexpectedFieldSize(Tag, u32)
}

impl_parse_error!(<'a>, &'a [u8], FieldError);

fn field_bytes(input: &[u8]) -> IResult<&[u8], (Tag, u32, &[u8]), FieldError> {
    flat_map(
        set_err(
            pair(tag, le_u32),
            |_| FieldError::UnexpectedEndOfRecord(8)
        ),
        |(field_tag, field_size)| {
            map(
                set_err(take(field_size), move |_| FieldError::UnexpectedEndOfRecord(field_size + 8)),
                move |field_bytes| (field_tag, field_size, field_bytes)
            )
        }
    )(input)
}

fn field<'a>(code_page: CodePage, record_tag: Tag) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], (Tag, Field), FieldError> {
    map_res(
        field_bytes,
        move |(field_tag, field_size, field_bytes), _| {
            let (remaining_field_bytes, field_body) = map_err(
                field_body(code_page, record_tag, field_tag, field_size),
                move |e, _| match e {
                    FieldBodyError::UnexpectedEndOfField(n) => FieldError::FieldSizeMismatch(field_tag, n, field_size),
                    FieldBodyError::UnknownValue(t, v) => FieldError::UnknownValue(field_tag, t, v),
                    FieldBodyError::UnexpectedFieldSize(s) => FieldError::UnexpectedFieldSize(field_tag, s),
                }
            )(field_bytes)?;
            if !remaining_field_bytes.is_empty() {
                return Err(nom::Err::Error(FieldError::FieldSizeMismatch(field_tag, field_size - remaining_field_bytes.len() as u32, field_size)));
            }
            Ok((field_tag, field_body))
        }
    )
}

#[derive(Debug, Clone)]
pub struct UnexpectedEof {
    pub record_offset: u64,
    pub expected_size: u32,
    pub actual_size: u32
}

impl fmt::Display for UnexpectedEof {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "unexpected EOF at {:X}h in record started at {:X}h, {} bytes missing",
            self.record_offset + self.actual_size as u64,
            self.record_offset,
            self.expected_size - self.actual_size
        )
    }
}

impl Error for UnexpectedEof {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub struct InvalidRecordFlags {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub value: u64
}

impl fmt::Display for InvalidRecordFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "invalid record flags value {:016X}h at {:X}h in {} record started at {:X}h",
            self.value,
            self.record_offset + 8,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for InvalidRecordFlags {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub struct RecordSizeMismatch {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub expected_size: u32,
    pub actual_size: u32
}

impl fmt::Display for RecordSizeMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "record size mismatch, expected {:08X}h, found {:08X}h at {:X}h in {} record started at {:X}h",
            self.expected_size,
            self.actual_size,
            self.record_offset + 4,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for RecordSizeMismatch {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub struct FieldSizeMismatch {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub field_offset: u32,
    pub field_tag: Tag,
    pub expected_size: u32,
    pub actual_size: u32
}

impl fmt::Display for FieldSizeMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "field size mismatch, expected {:08X}h, found {:08X}h at {:X}h in {} field started at {:X}h in {} record started at {:X}h",
            self.expected_size,
            self.actual_size,
            self.record_offset + 16 + self.field_offset as u64 + 4,
            self.field_tag,
            self.record_offset + 16 + self.field_offset as u64,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for FieldSizeMismatch {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub struct UnexpectedFieldSize {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub field_offset: u32,
    pub field_tag: Tag,
    pub field_size: u32
}

impl fmt::Display for UnexpectedFieldSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "found {} field at {:X}h with unexpected size {} at {:X}h in {} record started at {:X}h",
            self.field_tag,
            self.record_offset + 16 + self.field_offset as u64,
            self.field_size,
            self.record_offset + 16 + self.field_offset as u64 + 4,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for UnexpectedFieldSize {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub enum Unknown {
    FileType(u32),
    EffectRange(u32),
    DialogType(u8),
    SpellType(u32),
    SpellFlags(u32),
    AiServices(u32),
    NpcFlags(u8),
    CreatureFlags(u8),
    Blood(u8),
    ContainerFlags(u32),
    CreatureType(u32),
    LightFlags(u32),
    ApparatusType(u32),
    WeaponFlags(u32),
    WeaponType(u16),
    ArmorType(u32),
    BodyPartKind(u8),
    BodyPartType(u8),
    BodyPartFlags(u8),
    BipedObject(u8),
    ClothingType(u32),
    AiTravelFlags(u32),
    EnchantmentType(u32),
    CellFlags(u32),
}

impl fmt::Display for Unknown {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unknown::FileType(v) => write!(f, "file type {:08X}h", v),
            Unknown::EffectRange(v) => write!(f, "effect range {}", v),
            Unknown::DialogType(v) => write!(f, "dialog type {}", v),
            Unknown::SpellType(v) => write!(f, "spell type {}", v),
            Unknown::SpellFlags(v) => write!(f, "spell flags {:08X}h", v),
            Unknown::AiServices(v) => write!(f, "AI services {:08X}h", v),
            Unknown::NpcFlags(v) => write!(f, "NPC flags {:02X}h", v),
            Unknown::CreatureFlags(v) => write!(f, "creature flags {:02X}h", v),
            Unknown::Blood(v) => write!(f, "blood {}", v),
            Unknown::ContainerFlags(v) => write!(f, "container flags {:08X}h", v),
            Unknown::CreatureType(v) => write!(f, "creature type {}", v),
            Unknown::LightFlags(v) => write!(f, "light flags {:08X}h", v),
            Unknown::ApparatusType(v) => write!(f, "apparatus type {}", v),
            Unknown::WeaponFlags(v) => write!(f, "weapon flags {:08X}h", v),
            Unknown::WeaponType(v) => write!(f, "weapon type {}", v),
            Unknown::ArmorType(v) => write!(f, "armor type {}", v),
            Unknown::BodyPartKind(v) => write!(f, "body part kind {}", v),
            Unknown::BodyPartType(v) => write!(f, "body part type {}", v),
            Unknown::BodyPartFlags(v) => write!(f, "body part flags {:02X}h", v),
            Unknown::BipedObject(v) => write!(f, "biped object {}", v),
            Unknown::ClothingType(v) => write!(f, "clothing type {}", v),
            Unknown::AiTravelFlags(v) => write!(f, "AI travel flags {:08X}h", v),
            Unknown::EnchantmentType(v) => write!(f, "enchantment type {}", v),
            Unknown::CellFlags(v) => write!(f, "cell flags {:08X}h", v),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnknownValue {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub field_offset: u32,
    pub field_tag: Tag,
    pub value_offset: u32,
    pub value: Unknown,
}

impl fmt::Display for UnknownValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "Unknown {} at {:X}h in {} field started at {:X}h in {} record started at {:X}h",
            self.value,
            self.record_offset + 16 + self.field_offset as u64 + 8 + self.value_offset as u64,
            self.field_tag,
            self.record_offset + 16 + self.field_offset as u64,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for UnknownValue {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub enum RecordError {
    UnexpectedEof(UnexpectedEof),
    InvalidRecordFlags(InvalidRecordFlags),
    RecordSizeMismatch(RecordSizeMismatch),
    FieldSizeMismatch(FieldSizeMismatch),
    UnexpectedFieldSize(UnexpectedFieldSize),
    UnknownValue(UnknownValue),
}

impl fmt::Display for RecordError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordError::UnexpectedEof(x) => x.fmt(f),
            RecordError::InvalidRecordFlags(x) => x.fmt(f),
            RecordError::RecordSizeMismatch(x) => x.fmt(f),
            RecordError::FieldSizeMismatch(x) => x.fmt(f),
            RecordError::UnexpectedFieldSize(x) => x.fmt(f),
            RecordError::UnknownValue(x) => x.fmt(f),
        }
    }
}

impl Error for RecordError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(match self {
            RecordError::UnexpectedEof(x) => x,
            RecordError::InvalidRecordFlags(x) => x,
            RecordError::RecordSizeMismatch(x) => x,
            RecordError::FieldSizeMismatch(x) => x,
            RecordError::UnexpectedFieldSize(x) => x,
            RecordError::UnknownValue(x) => x,
        })
    }
}

impl_parse_error!(<'a>, &'a [u8], RecordError);

fn record_head<'a>(record_offset: u64) -> impl Fn(&'a [u8])
    -> IResult<&'a [u8], (Tag, u32, RecordFlags), RecordError> {

    flat_map(
        set_err(
            pair(tag, le_u32),
            move |input| {
                debug_panic!();
                RecordError::UnexpectedEof(UnexpectedEof { record_offset, expected_size: 16, actual_size: input.len() as u32 })
            }
        ),
        move |(record_tag, record_size)| {
            map_res(
                set_err(
                    le_u64,
                    move |input| {
                        debug_panic!();
                        RecordError::UnexpectedEof(UnexpectedEof { record_offset, expected_size: 16, actual_size: input.len() as u32 + 8 })
                    }
                ),
                move |value, _| {
                    let record_flags = RecordFlags::from_bits(value)
                        .ok_or(nom::Err::Error(RecordError::InvalidRecordFlags(InvalidRecordFlags { record_offset, record_tag, value })))?;
                    Ok((record_tag, record_size, record_flags))
                }
            )
        }
    )
}

fn read_record_head(record_offset: u64, input: &[u8]) -> Result<(Tag, u32, RecordFlags), RecordError> {
    match record_head(record_offset)(input) {
        Ok((rem, res)) => {
            debug_assert!(rem.is_empty());
            Ok(res)
        },
        Err(err) => Err(err.unwrap())
    }
}

#[derive(Debug, Clone)]
struct RecordBodyError<'a>(FieldError, &'a [u8]);

impl_parse_error!(<'a>, &'a [u8], RecordBodyError<'a>);

fn record_body<'a>(code_page: CodePage, record_tag: Tag) 
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<(Tag, Field)>, RecordBodyError<'a>> {
    
    many0(
        preceded(
            |input| {
                if input.is_empty() {
                    Err(nom::Err::Error(RecordBodyError(FieldError::UnexpectedEndOfRecord(0), input))) // error type doesn't matter
                } else {
                    Ok((input, ()))
                }
            },
            cut(map_err(field(code_page, record_tag), |e, input| RecordBodyError(e, input)))
        )
    )
}

fn read_record_body(record_offset: u64, code_page: CodePage,
                    record_tag: Tag, record_size: u32, record_flags: RecordFlags,
                    input: &[u8])
    -> Result<Record, RecordError> {
    
    let (remaining_record_bytes, record_body) = map_err(
        record_body(code_page, record_tag),
        move |e, input| match e {
            RecordBodyError(FieldError::UnexpectedEndOfRecord(n), field) =>
                RecordError::RecordSizeMismatch(RecordSizeMismatch {
                    record_offset, record_tag,
                    expected_size: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32 + n,
                    actual_size: record_size
                }),
            RecordBodyError(FieldError::FieldSizeMismatch(field_tag, expected_size, actual_size), field) =>
                RecordError::FieldSizeMismatch(FieldSizeMismatch {
                    record_offset, record_tag, field_tag, expected_size, actual_size,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32
                }),
            RecordBodyError(FieldError::UnknownValue(field_tag, value, value_offset), field) =>
                RecordError::UnknownValue(UnknownValue {
                    record_offset, value,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32,
                    record_tag, field_tag, value_offset
                }),
            RecordBodyError(FieldError::UnexpectedFieldSize(field_tag, field_size), field) =>
                RecordError::UnexpectedFieldSize(UnexpectedFieldSize {
                    record_offset, field_size,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32,
                    record_tag, field_tag
                }),
        }
    )(input).map_err(|x| x.unwrap())?;
    if !remaining_record_bytes.is_empty() {
        return Err(RecordError::RecordSizeMismatch(RecordSizeMismatch {
            record_offset,
            record_tag,
            expected_size: record_size - remaining_record_bytes.len() as u32,
            actual_size: record_size
        }));
    }
    Ok(Record { tag: record_tag, flags: record_flags, fields: record_body })
}

#[derive(Debug)]
pub struct ReadRecordError {
    record_error: RecordError,
    io_error: Option<io::Error>,
    bytes: Vec<u8>
}

fn is_eof(e: &RecordError) -> bool {
    match e {
        RecordError::UnexpectedEof(_) => true,
        _ => false
    }
}

lazy_static! {
    static ref INVALID_DATA_IO_ERROR: io::Error = io::Error::from(io::ErrorKind::InvalidData);
    static ref UNEXPECTED_EOF_IO_ERROR: io::Error = io::Error::from(io::ErrorKind::UnexpectedEof);
}

impl ReadRecordError {
    pub fn as_record_error(&self) -> &RecordError { &self.record_error }
    
    pub fn as_io_error(&self) -> &io::Error {
        self.io_error.as_ref().unwrap_or_else(|| if is_eof(&self.record_error) {
            &*UNEXPECTED_EOF_IO_ERROR
        } else {
            &*INVALID_DATA_IO_ERROR
        })
    }
    
    pub fn into_io_error(self) -> io::Error {
        let record_error = &self.record_error;
        self.io_error.unwrap_or_else(move || if is_eof(record_error) {
            io::Error::from(io::ErrorKind::UnexpectedEof)
        } else {
            io::Error::from(io::ErrorKind::InvalidData)
        })
    }
    
    pub fn into_record_error(self) -> RecordError { self.record_error }
    
    pub fn as_bytes(&self) -> &[u8] { &self.bytes }

    pub fn into_bytes(self) -> Vec<u8> { self.bytes }
    
    pub fn into_tuple(self) -> (RecordError, io::Error, Vec<u8>) {
        let record_error = &self.record_error;
        let io_error = self.io_error.unwrap_or_else(move || if is_eof(record_error) {
            io::Error::from(io::ErrorKind::UnexpectedEof)
        } else {
            io::Error::from(io::ErrorKind::InvalidData)
        });
        (self.record_error, io_error, self.bytes)
    }
}

impl fmt::Display for ReadRecordError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(io_error) = &self.io_error {
            if io_error.kind() != io::ErrorKind::UnexpectedEof {
                return io_error.fmt(f);
            }
        }
        self.record_error.fmt(f)
    }
}

impl Error for ReadRecordError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(self.io_error.as_ref().map(|x| {
            let x: &(dyn Error + 'static) = x;
            x
        }).unwrap_or(&self.record_error))
    }
}

fn read_and_ignore_interrupts(input: &mut (impl Read + ?Sized), buf: &mut [u8]) -> io::Result<usize> {
    loop {
        match input.read(buf) {
            Ok(read) => return Ok(read),
            Err(e) => {
                if e.kind() != io::ErrorKind::Interrupted {
                    return Err(e)
                }
            }
        }
    }
}

pub struct RecordReader {
    buf: Vec<u8>,
}

impl RecordReader {
    pub fn new() -> Self {
        RecordReader {
            buf: Vec::with_capacity(16)
        }
    }

    fn read_chunk(&mut self, record_offset: u64, input: &mut (impl Read + ?Sized))
                  -> Result<usize, ReadRecordError> {

        read_and_ignore_interrupts(input, &mut self.buf[..])
            .map_err(|io_error| ReadRecordError {
                io_error: Some(io_error),
                record_error: RecordError::UnexpectedEof(UnexpectedEof {
                    record_offset,
                    expected_size: self.buf.len() as u32,
                    actual_size: 0
                }),
                bytes: Vec::new()
            })
    }

    fn fill_buf(&mut self, mut from: usize, record_offset: u64, input: &mut (impl Read + ?Sized))
                -> Result<(), ReadRecordError> {

        while from < self.buf.len() {
            let read = read_and_ignore_interrupts(input, &mut self.buf[from..])
                .map_err(|io_error| ReadRecordError {
                    io_error: Some(io_error),
                    record_error: RecordError::UnexpectedEof(UnexpectedEof {
                        record_offset,
                        expected_size: self.buf.len() as u32,
                        actual_size: from as u32
                    }),
                    bytes: {
                        let mut bytes = replace(&mut self.buf, Vec::with_capacity(16));
                        bytes.truncate(from);
                        bytes
                    }
                })?;
            if read == 0 {
                return Err(ReadRecordError {
                    io_error: None,
                    record_error: RecordError::UnexpectedEof(UnexpectedEof {
                        record_offset,
                        expected_size: self.buf.len() as u32,
                        actual_size: from as u32
                    }),
                    bytes: {
                        let mut bytes = replace(&mut self.buf, Vec::with_capacity(16));
                        bytes.truncate(from);
                        bytes
                    }
                });
            }
            from += read;
        }
        Ok(())
    }

    pub fn read<Input: Read + ?Sized>(&mut self, code_page: CodePage, offset: u64, input: &mut Input)
        -> Result<Option<(Record, u32)>, ReadRecordError> {

        self.buf.resize(16, 0);
        let read = self.read_chunk(offset, input)?;
        if read == 0 { return Ok(None); }
        self.fill_buf(read, offset, input)?;
        let (record_tag, record_size, record_flags) =
            read_record_head(offset, &self.buf[..]).map_err(|record_error| ReadRecordError {
                io_error: None,
                record_error,
                bytes: replace(&mut self.buf, Vec::with_capacity(16))
            })?;
        self.buf.resize(16 + record_size as usize, 0);
        self.fill_buf(16, offset, input)?;
        let record = read_record_body(offset, code_page, record_tag, record_size, record_flags,
                         &self.buf[16..]).map_err(|record_error| ReadRecordError {
            io_error: None,
            record_error,
            bytes: replace(&mut self.buf, Vec::with_capacity(16))
        })?;
        Ok(Some((record, 16 + record_size)))
    }
}

pub struct Records<'a, Input: Read + ?Sized> {
    code_page: CodePage,
    input: &'a mut Input,
    offset: u64,
    reader: RecordReader,
}

impl<'a, Input: Read + ?Sized> Records<'a, Input> {
    pub fn new(code_page: CodePage, offset: u64, input: &'a mut Input) -> Self {
        Records {
            code_page,
            input,
            offset,
            reader: RecordReader::new()
        }
    }
}

impl<'a, Input: Read + ?Sized> Iterator for Records<'a, Input> {
    type Item = Result<Record, ReadRecordError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.reader.read(self.code_page, self.offset, self.input) {
            Ok(None) => None,
            Err(e) => {
                self.offset += e.as_bytes().len() as u64;
                Some(Err(e))
            },
            Ok(Some((record, read))) => {
                self.offset += read as u64;
                Some(Ok(record))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use ::nom;
    use crate::read::*;
    use encoding::all::WINDOWS_1251;
    use encoding::types::Encoding;
    use encoding::EncoderTrap;
    use either::{Right, Left};
    use crate::code::*;

    fn string(s: &str) -> Vec<u8> {
        WINDOWS_1251.encode(s, EncoderTrap::Strict).unwrap()
    }
    
    fn len(n: usize, s: &str) -> String {
        let mut r = String::from(s);
        while r.len() < n {
            r.push('\0');
        }
        r
    }

    #[test]
    fn read_record_flags_empty() {
        let mut input: Vec<u8> = Vec::new();
        input.extend(TES3.dword.to_le_bytes().iter());
        input.extend(0u32.to_le_bytes().iter());
        input.extend(0u64.to_le_bytes().iter());
        let (result, read) =
            RecordReader::new().read(CodePage::English, 0x11, &mut (&input[..])).unwrap().unwrap();
        assert_eq!(read, 16);
        assert_eq!(result.flags, RecordFlags::empty());
        assert_eq!(result.tag, TES3);
        assert!(result.fields.is_empty());
    }

    #[test]
    fn read_record_flags_invalid() {
        let mut input: Vec<u8> = Vec::new();
        input.extend(TES3.dword.to_le_bytes().iter());
        input.extend(0u32.to_le_bytes().iter());
        input.extend(0x70000u64.to_le_bytes().iter());
        let result = RecordReader::new().read(CodePage::English, 0x11, &mut (&input[..]));
        let error = result.err().unwrap();
        if let RecordError::InvalidRecordFlags(error) = error.into_record_error() { 
            assert_eq!(error.value, 0x70000);
            assert_eq!(error.record_offset, 0x11);
            assert_eq!(error.record_tag, TES3);
        } else {
            panic!()
        }
    }

    #[test]
    fn read_deletion_mark_mismatching_size_greater() {
        let mut input: Vec<u8> = Vec::new();
        input.extend(DELE.dword.to_le_bytes().iter());
        input.extend(6u32.to_le_bytes().iter());
        input.extend([0x00, 0x00, 0x00, 0x00, 0x00, 0x00].iter());
        let result = field(CodePage::English, DIAL)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldError::FieldSizeMismatch(DELE, expected, actual)) = error {
            assert_eq!(expected, 4);
            assert_eq!(actual, 6);
        } else {
            panic!()
        }
    }

    #[test]
    fn read_deletion_mark_mismatching_size_field_lesser() {
        let mut input: Vec<u8> = Vec::new();
        input.extend(DELE.dword.to_le_bytes().iter());
        input.extend(2u32.to_le_bytes().iter());
        input.extend([0x00, 0x00, 0x00, 0x00, 0x00, 0x00].iter());
        let result = field(CodePage::English, DIAL)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldError::FieldSizeMismatch(DELE, expected, actual)) = error {
            assert_eq!(expected, 4);
            assert_eq!(actual, 2);
        } else {
            panic!()
        }
    }

    #[test]
    fn read_deletion_mark_mismatching_size_lesser() {
        let mut input: Vec<u8> = Vec::new();
        input.extend(DELE.dword.to_le_bytes().iter());
        input.extend(2u32.to_le_bytes().iter());
        input.extend([0x00, 0x00].iter());
        let result = field(CodePage::English, DIAL)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldError::FieldSizeMismatch(DELE, expected, actual)) = error {
            assert_eq!(expected, 4);
            assert_eq!(actual, 2);
        } else {
            panic!()
        }
    }

    #[test]
    fn read_string_list_field() {
        let input: &'static [u8] = b"123\r\n\xC0\xC1t\r\n\xDA\xDFX\r\n";
        if let (remaining_input, Field::StringList(result)) =
                field_body(CodePage::Russian, INFO, BNAM, input.len() as u32)(input).unwrap() {
            assert_eq!(remaining_input.len(), 0);
            assert_eq!(result.len(), 4);
            assert_eq!(result[0], "123");
            assert_eq!(result[1], "АБt");
            assert_eq!(result[2], "ЪЯX");
            assert_eq!(result[3], "");
        } else {
            panic!()
        }
    }

    #[test]
    fn read_from_vec() {
        let input: Vec<u8> = Vec::new();
        field_body(CodePage::English, TES3, HEDR, input.len() as u32)(&input).err().unwrap();
    }

    #[test]
    fn read_from_vec_if_let() {
        let input: Vec<u8> = Vec::new();
        let res = field_body(CodePage::English, TES3, HEDR, input.len() as u32)(&input);
        if let Ok((_, _)) = res {
            panic!()
        } else { }
    }
    
    #[test]
    fn read_file_metadata() {
        let mut input: Vec<u8> = Vec::new();
        input.extend([0x00, 0x00, 0x00, 0x22].iter());
        input.extend([0x20, 0x00, 0x00, 0x00].iter());
        input.extend(string(&len(32, "author")));
        input.extend(string(&len(256, "description\r\nlines\r\n")));
        input.extend(vec![0x01, 0x02, 0x03, 0x04]);
        let result = field_body(CodePage::English, TES3, HEDR, input.len() as u32)(&input);
        if let (remaining_input, Field::FileMetadata(result)) = result.unwrap() {
            assert_eq!(remaining_input.len(), 0);
            assert_eq!(result.file_type, FileType::ESS);
            assert_eq!(result.author, "author");
            assert_eq!(result.description, &["description", "lines", ""]);
            assert_eq!(result.version, 0x22000000);
        } else {
            panic!()
        }
    }

    #[test]
    fn read_invalid_file_type() {
        let mut input: Vec<u8> = Vec::new();
        input.extend([0x00, 0x00, 0x00, 0x22].iter());
        input.extend([0x00, 0x00, 0x10, 0x00].iter());
        input.extend(string(&len(32, "author")));
        input.extend(string(&len(256, "description")));
        input.extend([0x01, 0x02, 0x03, 0x04].iter());
        let result = field_body(CodePage::English, TES3, HEDR, input.len() as u32)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldBodyError::UnknownValue(Unknown::FileType(val), offset)) = error {
            assert_eq!(val, 0x100000);
            assert_eq!(offset, 4);
        } else {
            panic!()
        }
    }
    
    #[test]
    fn read_file_metadata_eof_in_file_type() {
        let mut input: Vec<u8> = Vec::new();
        input.extend([0x00, 0x00, 0x00, 0x22].iter());
        input.extend([0x20, 0x00, 0x00].iter());
        let result = field_body(CodePage::English, TES3, HEDR, input.len() as u32)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldBodyError::UnexpectedEndOfField(size)) = error {
            assert_eq!(size, 300);
        } else {
            panic!()
        }
    }
    
    #[test]
    fn serialize_ingredient() {
        let ingredient = Ingredient {
            weight: 10.0,
            value: 117,
            effects: [22, 23, 24, 25],
            skills: [-1, 0, 17, 19],
            attributes: [-1, 1, 10, 14]
        };
        let bin: Vec<u8> = serialize(&ingredient, CodePage::English, false).unwrap();
        let res = ingredient_field(&bin).unwrap().1;
        assert_eq!(res.weight, ingredient.weight);
        assert_eq!(res.value, ingredient.value);
        assert_eq!(res.effects[0], ingredient.effects[0]);
        assert_eq!(res.effects[1], ingredient.effects[1]);
        assert_eq!(res.effects[2], ingredient.effects[2]);
        assert_eq!(res.effects[3], ingredient.effects[3]);
        assert_eq!(res.skills[0], ingredient.skills[0]);
        assert_eq!(res.skills[1], ingredient.skills[1]);
        assert_eq!(res.skills[2], ingredient.skills[2]);
        assert_eq!(res.skills[3], ingredient.skills[3]);
        assert_eq!(res.attributes[0], ingredient.attributes[0]);
        assert_eq!(res.attributes[1], ingredient.attributes[1]);
        assert_eq!(res.attributes[2], ingredient.attributes[2]);
        assert_eq!(res.attributes[3], ingredient.attributes[3]);
    }

    #[test]
    fn serialize_script_metadata() {
        let script_metadata = ScriptMetadata {
            name: "ScriptName".into(),
            vars: ScriptVars {
                shorts: 22,
                longs: 3,
                floats: 12
            },
            data_size: 65500,
            var_table_size: 100
        };
        let bin: Vec<u8> = serialize(&script_metadata, CodePage::English, false).unwrap();
        let res = script_metadata_field(CodePage::English)(&bin).unwrap().1;
        assert_eq!(res.name, script_metadata.name);
        assert_eq!(res.vars.shorts, script_metadata.vars.shorts);
        assert_eq!(res.vars.longs, script_metadata.vars.longs);
        assert_eq!(res.vars.floats, script_metadata.vars.floats);
        assert_eq!(res.data_size, script_metadata.data_size);
        assert_eq!(res.var_table_size, script_metadata.var_table_size);
    }

    #[test]
    fn serialize_file_metadata() {
        let file_metadata = FileMetadata {
            version: 42424242,
            file_type: FileType::ESS,
            author: "Some author".into(),
            description: vec!["descr line1".into(), "descr line2".into()],
            records: 1333
        };
        let bin: Vec<u8> = serialize(&file_metadata, CodePage::English, false).unwrap();
        let res = file_metadata_field(CodePage::English)(&bin).unwrap().1;
        assert_eq!(res.version, file_metadata.version);
        assert_eq!(res.file_type, file_metadata.file_type);
        assert_eq!(res.author, file_metadata.author);
        assert_eq!(res.description, file_metadata.description);
        assert_eq!(res.records, file_metadata.records);
    }

    #[test]
    fn serialize_effect() {
        let effect = Effect {
            effect_id: 12700,
            skill: 127,
            attribute: -128,
            range: EffectRange::Touch,
            area: 1333,
            duration: 200,
            magnitude_min: 1,
            magnitude_max: 300
        };
        let bin: Vec<u8> = serialize(&effect, CodePage::English, false).unwrap();
        let res = effect_field(&bin).unwrap().1;
        assert_eq!(res.effect_id, effect.effect_id);
        assert_eq!(res.skill, effect.skill);
        assert_eq!(res.attribute, effect.attribute);
        assert_eq!(res.range, effect.range);
        assert_eq!(res.area, effect.area);
        assert_eq!(res.duration, effect.duration);
        assert_eq!(res.magnitude_min, effect.magnitude_min);
        assert_eq!(res.magnitude_max, effect.magnitude_max);
    }

    #[test]
    fn serialize_npc_state() {
        let npc_state = NpcState {
            disposition: -100,
            reputation: -200,
            index: 129
        };
        let bin: Vec<u8> = serialize(&npc_state, CodePage::English, false).unwrap();
        let res = npc_state_field(&bin).unwrap().1;
        assert_eq!(res.disposition, npc_state.disposition);
        assert_eq!(res.reputation, npc_state.reputation);
        assert_eq!(res.index, npc_state.index);
    }

    #[test]
    fn serialize_npc_stats() {
        let stats = NpcStats {
            strength: 1, intelligence: 2, willpower: 3, agility: 4, speed: 5, endurance: 6,
            personality: 7, luck: 8, block: 9, armorer: 10, medium_armor: 11, heavy_armor: 12,
            blunt_weapon: 13, long_blade: 14, axe: 15, spear: 16, athletics: 17, enchant: 18,
            destruction: 19, alteration: 20, illusion: 21, conjuration: 22, mysticism: 23,
            restoration: 24, alchemy: 25, unarmored: 26, security: 27, sneak: 28, acrobatics: 29,
            light_armor: 30, short_blade: 31, marksman: 32, mercantile: 33, speechcraft: 34,
            hand_to_hand: 35, faction: 36, health: -37, magicka: -38, fatigue: 39
        };
        let bin: Vec<u8> = serialize(&stats, CodePage::English, false).unwrap();
        let res = npc_stats(&bin).unwrap().1;
        assert_eq!(res.strength, stats.strength);
        assert_eq!(res.intelligence, stats.intelligence);
        assert_eq!(res.willpower, stats.willpower);
        assert_eq!(res.agility, stats.agility);
        assert_eq!(res.speed, stats.speed);
        assert_eq!(res.endurance, stats.endurance);
        assert_eq!(res.personality, stats.personality);
        assert_eq!(res.luck, stats.luck);
        assert_eq!(res.block, stats.block);
        assert_eq!(res.armorer, stats.armorer);
        assert_eq!(res.medium_armor, stats.medium_armor);
        assert_eq!(res.heavy_armor, stats.heavy_armor);
        assert_eq!(res.blunt_weapon, stats.blunt_weapon);
        assert_eq!(res.long_blade, stats.long_blade);
        assert_eq!(res.axe, stats.axe);
        assert_eq!(res.spear, stats.spear);
        assert_eq!(res.athletics, stats.athletics);
        assert_eq!(res.enchant, stats.enchant);
        assert_eq!(res.destruction, stats.destruction);
        assert_eq!(res.alteration, stats.alteration);
        assert_eq!(res.illusion, stats.illusion);
        assert_eq!(res.conjuration, stats.conjuration);
        assert_eq!(res.mysticism, stats.mysticism);
        assert_eq!(res.restoration, stats.restoration);
        assert_eq!(res.alchemy, stats.alchemy);
        assert_eq!(res.unarmored, stats.unarmored);
        assert_eq!(res.security, stats.security);
        assert_eq!(res.sneak, stats.sneak);
        assert_eq!(res.acrobatics, stats.acrobatics);
        assert_eq!(res.light_armor, stats.light_armor);
        assert_eq!(res.short_blade, stats.short_blade);
        assert_eq!(res.marksman, stats.marksman);
        assert_eq!(res.mercantile, stats.mercantile);
        assert_eq!(res.speechcraft, stats.speechcraft);
        assert_eq!(res.hand_to_hand, stats.hand_to_hand);
        assert_eq!(res.faction, stats.faction);
        assert_eq!(res.health, stats.health);
        assert_eq!(res.magicka, stats.magicka);
        assert_eq!(res.fatigue, stats.fatigue);
    }

    #[test]
    fn serialize_npc_52() {
        let npc_stats = NpcStats {
            strength: 1, intelligence: 2, willpower: 3, agility: 4, speed: 5, endurance: 6,
            personality: 7, luck: 8, block: 9, armorer: 10, medium_armor: 11, heavy_armor: 12,
            blunt_weapon: 13, long_blade: 14, axe: 15, spear: 16, athletics: 17, enchant: 18,
            destruction: 19, alteration: 20, illusion: 21, conjuration: 22, mysticism: 23,
            restoration: 24, alchemy: 25, unarmored: 26, security: 27, sneak: 28, acrobatics: 29,
            light_armor: 30, short_blade: 31, marksman: 32, mercantile: 33, speechcraft: 34,
            hand_to_hand: 35, faction: 36, health: -37, magicka: -38, fatigue: 39
        };
        let npc = Npc {
            level: 100,
            disposition: -100,
            reputation: -92,
            rank: 33,
            gold: 20000,
            padding: 17,
            stats: Right(npc_stats.clone())
        };
        let bin: Vec<u8> = serialize(&Npc::from(npc.clone()), CodePage::English, true).unwrap();
        let res = npc_52_field(&bin).unwrap().1;
        assert_eq!(res.level, npc.level);
        assert_eq!(res.disposition, npc.disposition);
        assert_eq!(res.reputation, npc.reputation);
        assert_eq!(res.rank, npc.rank);
        assert_eq!(res.gold, npc.gold);
        assert_eq!(res.padding, npc.padding);
        if let Right(stats) = res.stats {
            assert_eq!(stats.enchant, npc_stats.enchant);
        } else {
            panic!()
        }
    }

    #[test]
    fn serialize_npc_12() {
        let npc = Npc {
            level: 100,
            disposition: -100,
            reputation: -92,
            rank: 33,
            gold: 20000,
            padding: 17,
            stats: Left(30001)
        };
        let bin: Vec<u8> = serialize(&Npc::from(npc.clone()), CodePage::English, true).unwrap();
        let res = npc_12_field(&bin).unwrap().1;
        assert_eq!(res.level, npc.level);
        assert_eq!(res.disposition, npc.disposition);
        assert_eq!(res.reputation, npc.reputation);
        assert_eq!(res.rank, npc.rank);
        assert_eq!(res.gold, npc.gold);
        assert_eq!(res.padding, npc.padding);
        if let Left(padding) = res.stats {
            assert_eq!(padding, 30001);
        } else {
            panic!()
        }
    }

    #[test]
    fn serialize_spell() {
        let spell = SpellMetadata {
            flags: SpellFlags::empty(),
            cost: 40,
            spell_type: SpellType::Curse
        };
        let bin: Vec<u8> = serialize(&spell, CodePage::English, false).unwrap();
        let res = spell_metadata_field(&bin).unwrap().1;
        assert_eq!(res, spell);
    }

    #[test]
    fn serialize_item() {
        let item = Item {
            count: -3,
            item_id: "b_item_01 ".into()
        };
        let bin: Vec<u8> = serialize(&item, CodePage::English, false).unwrap();
        let res = item_field(CodePage::English)(&bin).unwrap().1;
        assert_eq!(res.count, item.count);
        assert_eq!(res.item_id, item.item_id);
    }

    #[test]
    fn serialize_record() {
        let record = Record {
            tag: SCPT,
            flags: RecordFlags::PERSIST,
            fields: vec![
                (SCHD, Field::ScriptMetadata(ScriptMetadata {
                    name: "Scr1".into(),
                    vars: ScriptVars { shorts: 1, longs: 2, floats: 3 },
                    data_size: 800, var_table_size: 35
                })),
                (SCTX, Field::StringList(vec![
                    "Begin Scr1".into(),
                    "short i".into(),
                    "End Scr1".into(),
                ]))
            ]
        };
        let bin: Vec<u8> = serialize(&record, CodePage::English, true).unwrap();
        println!("{:?}", bin);
        let mut bin = &bin[..];
        let records = Records::new(CodePage::English, 0, &mut bin);
        let records = records.map(|x| x.unwrap()).collect::<Vec<_>>();
        assert_eq!(records.len(), 1);
        let res = &records[0];
        assert_eq!(res.tag, record.tag);
        assert_eq!(res.flags, record.flags);
        assert_eq!(res.fields.len(), 2);
    }
}
