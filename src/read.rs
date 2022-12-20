use nom::IResult;
use nom::combinator::{map, flat_map, cut};
use nom::sequence::{pair, tuple, preceded};
use nom::number::complete::{le_u32, le_u64, le_i32, le_i16, le_i64, le_u8, le_f32, le_u16, le_i8};
use nom::error::{ParseError, ErrorKind};
use nom::bytes::complete::take;
use nom::multi::many0;
use std::io::{self, Read, Write};
use std::error::Error;
use std::fmt::{self, Display, Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::mem::{replace, transmute};
use flate2::write::ZlibEncoder;
use flate2::Compression;
use either::{Right, Left, Either};
use std::cmp::Ordering;
use std::convert::TryInto;
use std::sync::LazyLock;

use crate::script_data::*;
use crate::strings::*;
use crate::field::*;
use crate::record::*;
use crate::code::CodePage;

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
    fn from_error_kind(_input: I, _kind: ErrorKind) -> Self { panic!() }

    fn append(_input: I, _kind: ErrorKind, other: Self) -> Self { other.0 }

    fn or(self, other: Self) -> Self { other.0 }

    fn from_char(_input: I, _: char) -> Self { panic!() }
}

fn map_err<I: Clone, O, E, X, F>(
    mut f: F,
    m: impl Fn(E, I) -> X
) -> impl FnMut(I) -> IResult<I, O, X> where F: FnMut(I) -> IResult<I, O, E> {
    move |input: I| {
        match f(input.clone()) {
            Err(nom::Err::Error(e)) => Err(nom::Err::Error(m(e, input))),
            Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(m(e, input))),
            Err(nom::Err::Incomplete(n)) => Err(nom::Err::Incomplete(n)),
            Ok(r) => Ok(r)
        }
    }
}

fn set_err<I: Clone, O, X, F>(
    mut f: F,
    m: impl Fn(I) -> X
) -> impl FnMut(I) -> IResult<I, O, X> where F: FnMut(I) -> IResult<I, O, ()> {
    move |input: I| {
        match f(input.clone()) {
            Err(nom::Err::Error(())) => Err(nom::Err::Error(m(input))),
            Err(nom::Err::Failure(())) => Err(nom::Err::Failure(m(input))),
            Err(nom::Err::Incomplete(n)) => Err(nom::Err::Incomplete(n)),
            Ok(r) => Ok(r)
        }
    }
}

fn map_res<I: Clone, O, E, R, F>(
    mut f: F,
    m: impl Fn(O, I) -> Result<R, E>
) -> impl FnMut(I) -> IResult<I, R, E> where F: FnMut(I) -> IResult<I, O, E> {
    move |input: I| {
        match f(input.clone()) {
            Err(e) => Err(e),
            Ok((i, r)) => m(r, input).map(|x| (i, x)).map_err(nom::Err::Failure),
        }
    }
}

fn and_then<I: Clone, O, E, R, F>(
    mut f: F,
    m: impl Fn(O, I) -> Result<R, nom::Err<E>>
) -> impl FnMut(I) -> IResult<I, R, E> where F: FnMut(I) -> IResult<I, O, E> {
    move |input: I| {
        match f(input.clone()) {
            Err(e) => Err(e),
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
            fn from_error_kind(_input: $input, kind: ::nom::error::ErrorKind) -> Self { panic!("{:?}", kind) }
        
            fn append(_input: $input, _kind: ::nom::error::ErrorKind, _other: Self) -> Self { panic!() }
        
            fn or(self, _other: Self) -> Self { panic!() }
        
            fn from_char(_input: $input, _: char) -> Self { panic!() }
        }
    };
    (<$($lifetime:lifetime),+>, $input:ty, $name:ident<$($name_lt:lifetime),+>) => {
        impl<$($lifetime),+> ::nom::error::ParseError<$input> for $name<$($name_lt),+> {
            fn from_error_kind(_input: $input, kind: ::nom::error::ErrorKind) -> Self { panic!("{:?}", kind) }
        
            fn append(_input: $input, _kind: ::nom::error::ErrorKind, _other: Self) -> Self { panic!() }
        
            fn or(self, _other: Self) -> Self { panic!() }
        
            fn from_char(_input: $input, _: char) -> Self { panic!() }
        }
    }
}

#[derive(Debug, Clone)]
enum FieldBodyError {
    UnexpectedEndOfField(u32),
    UnknownValue(Unknown, u32),
    UnexpectedFieldSize(u32),
    InvalidValue(Invalid, u32),
}

impl_parse_error!(<'a>, &'a [u8], FieldBodyError);

fn u8_list_field<E>(input: &[u8]) -> IResult<&[u8], Vec<u8>, E> {
    Ok((&input[input.len() .. ], input.into()))
}

fn script_data_field<E>(code_page: CodePage) -> impl FnMut(&[u8]) -> IResult<&[u8], ScriptData, E> {
    move |input| Ok((&input[input.len() .. ], ScriptData::from_bytes(code_page, input)))
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

fn consume<E>(input: &[u8]) -> IResult<&[u8], &[u8], E> {
    Ok((&input[input.len()..], input))
}

fn string_field<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], String, FieldBodyError> {
    map(
        consume,
        move |input| code_page.decode(input)
    )
}

fn string_z_field<'a>(code_page: CodePage) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], StringZ, FieldBodyError> {
    map(
        consume,
        move |input| {
            let has_tail_zero = input.last() == Some(&0);
            let input = if has_tail_zero {
                &input[..input.len() - 1]                
            } else {
                input
            };
            StringZ { string: code_page.decode(input), has_tail_zero }
        }
    )
}

fn string_z_list_field<'a>(
    code_page: CodePage
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], StringZList, FieldBodyError> {
    map(
        string_z_field(code_page),
        |s| StringZList { vec: s.string.split('\0').map(String::from).collect(), has_tail_zero: s.has_tail_zero }
    )
}

fn short_string<'a>(
    code_page: CodePage,
    length: u32,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], String, ()> {
    map(
        set_err(take(length), |_| ()),
        move |bytes| code_page.decode(trim_end_nulls(bytes))
    )
}

fn file_metadata_field<'a>(
    code_page: CodePage
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], FileMetadata, FieldBodyError> {
    map(
        tuple((
            set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(300)),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(300)),
                |w, _| FileType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::FileType(w), 4))
            ),
            set_err(
                tuple((
                    short_string(code_page, 32),
                    map(
                        short_string(code_page, 256),
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

fn short_string_field<'a>(code_page: CodePage, mode: RecordReadMode, length: u32) -> impl Fn(&'a [u8])
    -> IResult<&'a [u8], String, FieldBodyError> {
    
    move |input| {
        let length = if mode == RecordReadMode::Lenient && input.len() < length as usize { input.len() as u32 } else { length };
        set_err(short_string(code_page, length), move |_| FieldBodyError::UnexpectedEndOfField(length))(input)
    }
}

fn multiline_field<'a>(
    code_page: CodePage,
    linebreaks: Newline
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Vec<String>, FieldBodyError> {
    map(
        string_field(code_page),
        move |s| s.split(linebreaks.as_str()).map(String::from).collect()
    )
}

fn current_time_field(input: &[u8]) -> IResult<&[u8], CurrentTime, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_f32,
                le_u32,
                le_u32,
                le_u32,
            )),
            |_| FieldBodyError::UnexpectedEndOfField(16)
        ),
        |(hour, day, month, year)| CurrentTime { hour, day, month, year }
    )(input)
}

fn time_field(input: &[u8]) -> IResult<&[u8], Time, FieldBodyError> {
    map(
        set_err(
            pair(
                le_f32,
                le_u32,
            ),
            |_| FieldBodyError::UnexpectedEndOfField(8)
        ),
        |(hour, day)| Time { hour, day }
    )(input)
}

fn item_field<'a>(
    code_page: CodePage
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Item, FieldBodyError> {
    set_err(
        map(
            pair(
                le_i32,
                short_string(code_page, 32)
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
        return Err(nom::Err::Failure(FieldBodyError::UnexpectedEndOfField((input.len() - m + 4) as u32)));
    }
    set_err(many0(le_i32), |_| unreachable!())(input)
}

fn i16_field(input: &[u8]) -> IResult<&[u8], i16, FieldBodyError> {
    set_err(le_i16, |_| FieldBodyError::UnexpectedEndOfField(2))(input)
}

fn i16_list_field(input: &[u8]) -> IResult<&[u8], Vec<i16>, FieldBodyError> {
    let m = input.len() % 2;
    if m != 0 {
        return Err(nom::Err::Failure(FieldBodyError::UnexpectedEndOfField((input.len() - m + 2) as u32)));
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
        return Err(nom::Err::Failure(FieldBodyError::UnexpectedEndOfField((input.len() - m + 4) as u32)));
    }
    set_err(many0(le_f32), |_| unreachable!())(input)
}

fn attribute_option_i8(input: &[u8]) -> IResult<&[u8], Either<Option<i8>, Attribute>, ()> {
    map(
        le_i8,
        move |b| if b == -1 { 
            Left(None)
        } else if let Some(a) = b.try_into().ok().and_then(Attribute::n) {
            Right(a)
        } else {
            Left(Some(b))
        }
    )(input)
}

fn attribute_option_i32(input: &[u8]) -> IResult<&[u8], Either<Option<i32>, Attribute>, ()> {
    map(
        le_i32,
        move |b| if b == -1 {
            Left(None)
        } else if let Some(a) = b.try_into().ok().and_then(Attribute::n) {
            Right(a)
        } else {
            Left(Some(b))
        }
    )(input)
}

fn skill_option_i32(input: &[u8]) -> IResult<&[u8], Either<Option<i32>, Skill>, ()> {
    map(
        le_i32,
        move |b| if b == -1 {
            Left(None)
        } else if let Some(a) = b.try_into().ok().and_then(Skill::n) {
            Right(a)
        } else {
            Left(Some(b))
        }
    )(input)
}

fn skill_option_i8(input: &[u8]) -> IResult<&[u8], Either<Option<i8>, Skill>, ()> {
    map(
        le_i8,
        move |b| if b == -1 {
            Left(None)
        } else if let Some(a) = b.try_into().ok().and_then(Skill::n) {
            Right(a)
        } else {
            Left(Some(b))
        }
    )(input)
}

fn effect_index_option_i16(input: &[u8]) -> IResult<&[u8], Either<Option<i16>, EffectIndex>, ()> {
    map(
        le_i16,
        move |b| if b == -1 {
            Left(None)
        } else if let Some(a) = b.try_into().ok().and_then(EffectIndex::n) {
            Right(a)
        } else {
            Left(Some(b))
        }
    )(input)
}

fn effect_index_option_i32(input: &[u8]) -> IResult<&[u8], Either<Option<i32>, EffectIndex>, ()> {
    map(
        le_i32,
        move |b| if b == -1 {
            Left(None)
        } else if let Some(a) = b.try_into().ok().and_then(EffectIndex::n) {
            Right(a)
        } else {
            Left(Some(b))
        }
    )(input)
}

fn sex_option_i8(input: &[u8]) -> IResult<&[u8], Either<Option<i8>, Sex>, ()> {
    map(
        le_i8,
        move |b| if b == -1 {
            Left(None)
        } else if let Some(a) = b.try_into().ok().and_then(Sex::n) {
            Right(a)
        } else {
            Left(Some(b))
        }
    )(input)
}

fn option_i8(input: &[u8]) -> IResult<&[u8], Option<i8>, ()> {
    map(
        le_i8,
        move |b| if b == -1 {
            None
        } else {
            Some(b)
        }
    )(input)
}

fn ingredient_field(input: &[u8]) -> IResult<&[u8], Ingredient, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_f32, le_u32,
                tuple((effect_index_option_i32, effect_index_option_i32, effect_index_option_i32, effect_index_option_i32)),
                tuple((skill_option_i32, skill_option_i32, skill_option_i32, skill_option_i32)),
                tuple((attribute_option_i32, attribute_option_i32, attribute_option_i32, attribute_option_i32))
            )),
            |_| FieldBodyError::UnexpectedEndOfField(56)
        ),
        |(
            weight, value,
            (effect_1_index, effect_2_index, effect_3_index, effect_4_index),
            (effect_1_skill, effect_2_skill, effect_3_skill, effect_4_skill),
            (effect_1_attribute, effect_2_attribute, effect_3_attribute, effect_4_attribute)
        )| Ingredient {
            weight, value,
            effect_1_index, effect_2_index, effect_3_index, effect_4_index,
            effect_1_skill, effect_2_skill, effect_3_skill, effect_4_skill,
            effect_1_attribute, effect_2_attribute, effect_3_attribute, effect_4_attribute
        }
    )(input)
}

fn sound_chance_field<'a>(
    code_page: CodePage
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], SoundChance, FieldBodyError> {
    map(
        set_err(
            pair(
                short_string(code_page, 32),
                le_u8
            ),
            |_| FieldBodyError::UnexpectedEndOfField(32 + 1)
        ),
        |(sound_id, chance)| SoundChance { sound_id, chance }
    )
}

fn script_metadata_field<'a>(
    code_page: CodePage
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], ScriptMetadata, FieldBodyError> {
    map(
        set_err(
            tuple((
                short_string(code_page, 32),
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
            tuple((le_u32, le_u32, le_u32)),
            |_| FieldBodyError::UnexpectedEndOfField(12)
        ),
        |(shorts, longs, floats)| ScriptVars {
            shorts,
            longs,
            floats
        }
    )(input)
}

fn info_field(input: &[u8]) -> IResult<&[u8], Info, FieldBodyError> {
    map(
        pair(
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
                |w, _| w.try_into().ok().and_then(DialogType::n).ok_or(
                    FieldBodyError::UnknownValue(Unknown::DialogType(w), 0)
                )
            ),
            set_err(
                tuple((le_u32, option_i8, sex_option_i8, option_i8, le_u8)),
                |_| FieldBodyError::UnexpectedEndOfField(12)
            )
        ),
        |(dialog_type, (disp_index, rank, sex, pc_rank, padding))| Info {
            dialog_type, disp_index, rank, sex, pc_rank, padding
        }
    )(input)
}

fn rank(input: &[u8]) -> IResult<&[u8], Rank, ()> {
    map(
        tuple((le_u32, le_u32, le_u32, le_u32, le_u32)),
        |(attribute_1, attribute_2, primary_skill, favored_skill, reputation)| Rank {
            attribute_1, attribute_2, primary_skill, favored_skill, reputation
        }
    )(input)
}

fn faction_field(input: &[u8]) -> IResult<&[u8], Faction, FieldBodyError> {
    map(
        tuple((
            attribute(240, 0), attribute(240, 4),
            set_err(
                pair(
                    tuple((rank, rank, rank, rank, rank, rank, rank, rank, rank, rank)),
                    tuple((
                        skill_option_i32, skill_option_i32, skill_option_i32, skill_option_i32,
                        skill_option_i32, skill_option_i32, skill_option_i32
                    ))
                ),
                |_| FieldBodyError::UnexpectedEndOfField(240)
            ),
            bool_u32(240, 236)
        )),
        |(
            favored_attribute_1, favored_attribute_2,
            (
                (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9),
                (
                    favored_skill_1, favored_skill_2, favored_skill_3, favored_skill_4,
                    favored_skill_5, favored_skill_6, favored_skill_7
                )
            ),
            hidden_from_pc
        )| Faction {
            favored_attribute_1, favored_attribute_2,
            favored_skill_1, favored_skill_2, favored_skill_3, favored_skill_4,
            favored_skill_5, favored_skill_6, favored_skill_7, hidden_from_pc,
            ranks: [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9]
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
                |w, _| CellFlags::from_bits(w).ok_or(
                    FieldBodyError::UnknownValue(Unknown::CellFlags(w), 0)
                )
            ),
            set_err(
                pair(le_i32, le_i32),
                |_| FieldBodyError::UnexpectedEndOfField(12)
            )
        ),
        |(flags, (x, y))| {
            let position = if flags.contains(CellFlags::INTERIOR) {
                CellPosition::Interior {
                    x: unsafe { transmute(x) },
                    y: unsafe { transmute(y) }
                }
            } else {
                CellPosition::Exterior { x, y }
            };
            Cell { flags, position }
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

fn spell_field(input: &[u8]) -> IResult<&[u8], Spell, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
                |w, _| SpellType::n(w).ok_or(
                    FieldBodyError::UnknownValue(Unknown::SpellType(w), 0)
                )
            ),
            set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(12)),
                |w, _| SpellFlags::from_bits(w).ok_or(
                    FieldBodyError::UnknownValue(Unknown::SpellFlags(w), 8)
                )
            ),
        )),
        |(spell_type, cost, flags)| Spell {
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
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                |w, _| Color::try_from_u32(w).ok_or(
                    FieldBodyError::InvalidValue(Invalid::Color(w), 16)
                )
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                |w, _| LightFlags::from_bits(w).ok_or(
                    FieldBodyError::UnknownValue(Unknown::LightFlags(w), 20)
                )
            )
        )),
        |((weight, value, time, radius), color, flags)| Light {
            weight, value, time, radius, color, flags
        }
    )(input)
}

fn interior_field(input: &[u8]) -> IResult<&[u8], Interior, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(16)),
                |w, _| Color::try_from_u32(w).ok_or(
                    FieldBodyError::InvalidValue(Invalid::Color(w), 0)
                )
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(16)),
                |w, _| Color::try_from_u32(w).ok_or(
                    FieldBodyError::InvalidValue(Invalid::Color(w), 4)
                )
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(16)),
                |w, _| Color::try_from_u32(w).ok_or(
                    FieldBodyError::InvalidValue(Invalid::Color(w), 8)
                )
            ),
            set_err(le_f32, |_| FieldBodyError::UnexpectedEndOfField(16))
        )),
        |(ambient, sunlight, fog, fog_density)| Interior {
            ambient, sunlight, fog, fog_density
        }
    )(input)
}

fn sound_field(input: &[u8]) -> IResult<&[u8], Sound, FieldBodyError> {
    map(
        set_err(
            tuple((le_u8, le_u8, le_u8)),
            |_| FieldBodyError::UnexpectedEndOfField(3)
        ),
        |(volume, range_min, range_max)| Sound {
            volume, range_min, range_max
        }
    )(input)
}

fn color_component<'a>(
    field_size: u32,
    offset: u32
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], u8, FieldBodyError> {
    map_res(
        set_err(le_u32, move |_| FieldBodyError::UnexpectedEndOfField(field_size)),
        move |w, _| w.try_into().map_err(|_| FieldBodyError::InvalidValue(Invalid::ColorComponent(w), offset))
    )
}

fn effect_metadata_field(input: &[u8]) -> IResult<&[u8], EffectMetadata, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(36)),
                |w, _| School::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::School(w), 0))
            ),
            set_err(le_f32, |_| FieldBodyError::UnexpectedEndOfField(36)),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(36)),
                |w, _| EffectFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::EffectFlags(w), 8))
            ),
            color_component(36, 12),
            color_component(36, 16),
            color_component(36, 20),
            set_err(tuple((le_f32, le_f32, le_f32)), |_| FieldBodyError::UnexpectedEndOfField(36))
        )),
        |(school, base_cost, flags, r, g, b, (size_factor, speed, size_cap))| EffectMetadata {
            school, base_cost, flags, color: Color { r, g, b }, size_factor, speed, size_cap
        }
    )(input)
}

fn color_field(input: &[u8]) -> IResult<&[u8], Color, FieldBodyError> {
    map_res(
        set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
        |w, _| Color::try_from_u32(w).ok_or(FieldBodyError::InvalidValue(Invalid::Color(w), 0))
    )(input)
}

fn misc_item_field(input: &[u8]) -> IResult<&[u8], MiscItem, FieldBodyError> {
    map(
        pair(
            set_err(
                pair(le_f32, le_u32),
                |_| FieldBodyError::UnexpectedEndOfField(12)
            ),
            bool_u32(12, 8)
        ),
        |((weight, value), is_key)| MiscItem {
            weight, value, is_key
        }
    )(input)
}

fn apparatus_field(input: &[u8]) -> IResult<&[u8], Apparatus, FieldBodyError> {
    map(
        pair(
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(16)),
                |w, _| ApparatusType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::ApparatusType(w), 0))
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
        tuple((
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(16)),
                |w, _| EnchantmentType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::EnchantmentType(w), 0))
            ),
            set_err(
                pair(le_u32, le_u32),
                |_| FieldBodyError::UnexpectedEndOfField(16)
            ),
            map_res(
                set_err(le_i16, |_| FieldBodyError::UnexpectedEndOfField(16)),
                |w, _| match w {
                    0 => Ok(Right(false)),
                    1 => Ok(Right(true)),
                    -1 => Ok(Left(true)),
                    -2 => Ok(Left(false)),
                    w => Err(FieldBodyError::UnknownValue(Unknown::EnchantmentAutoCalculate(w), 12))
                }
            ),
            set_err(le_u16, |_| FieldBodyError::UnexpectedEndOfField(16))
        )),
        |(enchantment_type, (cost, charge_amount), auto_calculate, padding)| Enchantment {
            enchantment_type, cost, charge_amount, auto_calculate, padding
        }
    )(input)
}

fn armor_field(input: &[u8]) -> IResult<&[u8], Armor, FieldBodyError> {
    map(
        pair(
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                |w, _| ArmorType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::ArmorType(w), 0))
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
                |w, _| ClothingType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::ClothingType(w), 0))
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
                |w, _| WeaponType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::WeaponType(w), 8))
            ),
            set_err(
                tuple((le_u16, le_f32, le_f32, le_u16, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8)),
                |_| FieldBodyError::UnexpectedEndOfField(32)
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(32)),
                |w, _| WeaponFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::WeaponFlags(w), 28))
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
                |b, _| BodyPartKind::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::BodyPartKind(b), 0))
            ),
            bool_u8(4, 1),
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| BodyPartFlags::from_bits(b).ok_or(FieldBodyError::UnknownValue(Unknown::BodyPartFlags(b), 2))
            ),
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| BodyPartType::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::BodyPartType(b), 3))
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
                |w, _| Services::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::AiServices(w), 8))
            )
        ),
        |((hello, fight, flee, alarm, padding_8, padding_16), services)| Ai {
            hello, fight, flee, alarm, padding_8, padding_16, services
        }
    )(input)
}

fn ai_wander_field(input: &[u8]) -> IResult<&[u8], AiWander, FieldBodyError> {
    map(
        pair(
            set_err(
                tuple((
                    le_u16, le_u16, le_u8,
                    le_u8, le_u8, le_u8, le_u8,
                    le_u8, le_u8, le_u8, le_u8
                )),
                |_| FieldBodyError::UnexpectedEndOfField(14)
            ),
            bool_u8(14, 13)
        ),
        |((distance, duration, time_of_day, idle2, idle3, idle4, idle5, idle6, idle7, idle8, idle9), repeat)| AiWander {
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
                |w, _| AiTravelFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::AiTravelFlags(w), 12))
            )
        ),
        |((x, y, z), flags)| AiTravel {
            pos: Pos { x, y, z }, flags
        }
    )(input)
}

fn ai_target_field<'a>(
    code_page: CodePage
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], AiTarget, FieldBodyError> {
    map(
        tuple((
            set_err(
                tuple((
                    le_f32, le_f32, le_f32, le_u16,
                    short_string(code_page, 32)
                )),
                |_| FieldBodyError::UnexpectedEndOfField(48)
            ),
            bool_u8(48, 46),
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(48)),
                |b, _| AiTargetFlags::from_bits(b).ok_or(
                    FieldBodyError::UnknownValue(Unknown::AiTargetFlags(b), 46)
                )
            )
        )),
        |((x, y, z, duration, actor_id), reset, flags)| AiTarget {
            pos: Pos { x, y, z }, duration, actor_id, reset, flags
        }
    )
}

fn bool_u8<'a>(
    field_size: u32,
    offset: u32
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], bool, FieldBodyError> {
    map_res(
        set_err(le_u8, move |_| FieldBodyError::UnexpectedEndOfField(field_size)),
        move |b, _| match b {
            0 => Ok(false),
            1 => Ok(true),
            b => Err(FieldBodyError::InvalidValue(Invalid::Bool(b as u32), offset))
        }
    )
}

fn bool_u32<'a>(
    field_size: u32,
    offset: u32
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], bool, FieldBodyError> {
    map_res(
        set_err(le_u32, move |_| FieldBodyError::UnexpectedEndOfField(field_size)),
        move |w, _| match w {
            0 => Ok(false),
            1 => Ok(true),
            w => Err(FieldBodyError::InvalidValue(Invalid::Bool(w), offset))
        }
    )
}

fn none_u8_field<'a>(
    none: u8
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], (), FieldBodyError> {
    map_res(
        set_err(le_u8, move |_| FieldBodyError::UnexpectedEndOfField(1)),
        move |b, _| if b == none {
            Ok(())
        } else {
            Err(FieldBodyError::InvalidValue(Invalid::MarkerU8(b), 0))
        }
    )
}

fn ai_activate_field<'a>(
    code_page: CodePage
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], AiActivate, FieldBodyError> {
    map(
        pair(
            set_err(short_string(code_page, 32), |_| FieldBodyError::UnexpectedEndOfField(33)),
            bool_u8(33, 32)
        ),
        |(object_id, reset)| AiActivate {
            object_id, reset
        }
    )
}

fn attribute<'a>(
    field_size: u32,
    offset: u32
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Attribute, FieldBodyError> {
    map_res(
        set_err(le_u32, move |_| FieldBodyError::UnexpectedEndOfField(field_size)),
        move |w, _| Attribute::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::Attribute(w), offset))
    )
}

fn skill<'a>(
    field_size: u32,
    offset: u32
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Skill, FieldBodyError> {
    map_res(
        set_err(le_u32, move |_| FieldBodyError::UnexpectedEndOfField(field_size)),
        move |w, _| Skill::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::Skill(w), offset))
    )
}

fn skill_field(input: &[u8]) -> IResult<&[u8], Skill, FieldBodyError> {
    skill(4, 0)(input)
}

fn effect_arg_field(input: &[u8]) -> IResult<&[u8], EffectArg, FieldBodyError> {
    map(
        set_err(le_u32, move |_| FieldBodyError::UnexpectedEndOfField(4)),
        EffectArg::from
    )(input)
}

fn effect_index_field(input: &[u8]) -> IResult<&[u8], EffectIndex, FieldBodyError> {
    map_res(
        set_err(le_u32, move |_| FieldBodyError::UnexpectedEndOfField(4)),
        move |w, _| EffectIndex::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::EffectIndex(w), 0))
    )(input)
}

fn tag_field(input: &[u8]) -> IResult<&[u8], Tag, FieldBodyError> {
    map(
        set_err(le_u32, move |_| FieldBodyError::UnexpectedEndOfField(4)),
        Tag::from
    )(input)
}

fn sound_gen_field(input: &[u8]) -> IResult<&[u8], SoundGen, FieldBodyError> {
    map_res(
        set_err(le_u32, move |_| FieldBodyError::UnexpectedEndOfField(4)),
        move |w, _| SoundGen::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::SoundGen(w), 0))
    )(input)
}

fn class_field(input: &[u8]) -> IResult<&[u8], Class, FieldBodyError> {
    map(
        tuple((
            attribute(60, 0),
            attribute(60, 4),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(60)),
                |w, _| Specialization::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::Specialization(w), 8))
            ),
            pair(
                tuple((
                    skill(60, 12), skill(60, 16), skill(60, 20), skill(60, 24), skill(60, 28),
                    skill(60, 32), skill(60, 36), skill(60, 40), skill(60, 44), skill(60, 48),
                )),
                bool_u32(60, 52)
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(60)),
                |w, _| Services::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::AiServices(w), 56))
            )
        )),
        |(
            primary_attribute_1, primary_attribute_2, specialization,
            (
                (minor_skill_1, major_skill_1, minor_skill_2, major_skill_2, minor_skill_3, major_skill_3, minor_skill_4, major_skill_4, minor_skill_5, major_skill_5),
                playable
            ),
            auto_calc_services
        )| Class {
            primary_attribute_1, primary_attribute_2, specialization,
            minor_skill_1, minor_skill_2, minor_skill_3, minor_skill_4, minor_skill_5,
            major_skill_1, major_skill_2, major_skill_3, major_skill_4, major_skill_5,
            playable, auto_calc_services
        }
    )(input)
}

fn skill_metadata_field(input: &[u8]) -> IResult<&[u8], SkillMetadata, FieldBodyError> {
    map(
        tuple((
            attribute(24, 0),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                |w, _| Specialization::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::Specialization(w), 4))
            ),
            set_err(
                tuple((le_f32, le_f32, le_f32, le_f32)),
                |_| FieldBodyError::UnexpectedEndOfField(24)
            )
        )),
        |(
            governing_attribute, specialization,
            (use_value_1, use_value_2, use_value_3, use_value_4)
        )| SkillMetadata {
            governing_attribute, specialization, use_value_1, use_value_2, use_value_3, use_value_4
        }
    )(input)
}

fn pos_field(input: &[u8]) -> IResult<&[u8], Pos, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_f32, le_f32, le_f32,
            )),
            |_| FieldBodyError::UnexpectedEndOfField(12)
        ),
        |(x, y, z)| Pos { x, y, z }
    )(input)
}

fn pos_rot_field(input: &[u8]) -> IResult<&[u8], PosRot, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_f32, le_f32, le_f32,
                le_f32, le_f32, le_f32,
            )),
            |_| FieldBodyError::UnexpectedEndOfField(24)
        ),
        |(pos_x, pos_y, pos_z, rot_x, rot_y, rot_z)| PosRot {
            pos: Pos { x: pos_x, y: pos_y, z: pos_z },
            rot: Rot { x: rot_x, y: rot_y, z: rot_z }
        }
    )(input)
}

fn attributes_field(input: &[u8]) -> IResult<&[u8], Attributes<u32>, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_u32, le_u32, le_u32, le_u32,
                le_u32, le_u32, le_u32, le_u32,
            )),
            |_| FieldBodyError::UnexpectedEndOfField(32)
        ),
        |(
            strength, intelligence, willpower, agility,
            speed, endurance, personality, luck,
        )| Attributes {
            strength, intelligence, willpower, agility,
            speed, endurance, personality, luck,
        }
    )(input)
}

fn skills_field(input: &[u8]) -> IResult<&[u8], Skills<u32>, FieldBodyError> {
    map(
        set_err(
            tuple((
                tuple((le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32)),
                tuple((le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32)),
                tuple((le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32)),
            )),
            |_| FieldBodyError::UnexpectedEndOfField(108)
        ),
        |(
            (block, armorer, medium_armor, heavy_armor, blunt_weapon, long_blade, axe, spear, athletics, enchant),
            (destruction, alteration, illusion, conjuration, mysticism, restoration, alchemy, unarmored, security), 
            (sneak, acrobatics, light_armor, short_blade, marksman, mercantile, speechcraft, hand_to_hand),
        )| Skills {
            block, armorer, medium_armor, heavy_armor, blunt_weapon, long_blade, axe, spear,
            athletics, enchant, destruction, alteration, illusion, conjuration, mysticism,
            restoration, alchemy, unarmored, security, sneak, acrobatics, light_armor,
            short_blade, marksman, mercantile, speechcraft, hand_to_hand
        }
    )(input)
}

fn race_field(input: &[u8]) -> IResult<&[u8], Race, FieldBodyError> {
    map(
        pair(
            set_err(
                pair(
                    tuple((
                        skill_option_i32, le_u32,
                        skill_option_i32, le_u32,
                        skill_option_i32, le_u32,
                        skill_option_i32, le_u32,
                        skill_option_i32, le_u32,
                        skill_option_i32, le_u32,
                        skill_option_i32, le_u32
                    )),
                    tuple((
                        le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32,
                        le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32, le_u32,
                        le_f32, le_f32, le_f32, le_f32
                    ))
                ),
                |_| FieldBodyError::UnexpectedEndOfField(140)
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(140)),
                |w, _| RaceFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::RaceFlags(w), 136))
            )
        ),
        |(
            (
                (
                    skill_1, skill_1_bonus, skill_2, skill_2_bonus, skill_3, skill_3_bonus, skill_4, skill_4_bonus,
                    skill_5, skill_5_bonus, skill_6, skill_6_bonus, skill_7, skill_7_bonus
                ),
                (
                    strength_m, strength_f, intelligence_m, intelligence_f, willpower_m, willpower_f, agility_m, agility_f,
                    speed_m, speed_f, endurance_m, endurance_f, personality_m, personality_f, luck_m, luck_f,
                    height_m, height_f, weight_m, weight_f
                )
            ),
            flags
        )| Race {
            skill_1, skill_1_bonus, skill_2, skill_2_bonus, skill_3, skill_3_bonus, skill_4, skill_4_bonus,
            skill_5, skill_5_bonus, skill_6, skill_6_bonus, skill_7, skill_7_bonus, flags,
            height: RaceParameter { male: height_m, female: height_f },
            weight: RaceParameter { male: weight_m, female: weight_f },
            attributes: Attributes {
                strength: RaceAttribute { male: strength_m, female: strength_f },
                intelligence: RaceAttribute { male: intelligence_m, female: intelligence_f },
                willpower: RaceAttribute { male: willpower_m, female: willpower_f },
                agility: RaceAttribute { male: agility_m, female: agility_f },
                speed: RaceAttribute { male: speed_m, female: speed_f },
                endurance: RaceAttribute { male: endurance_m, female: endurance_f },
                personality: RaceAttribute { male: personality_m, female: personality_f },
                luck: RaceAttribute { male: luck_m, female: luck_f }
            }
        }
    )(input)
}

fn npc_flags_field(input: &[u8]) -> IResult<&[u8], FlagsAndBlood<NpcFlags>, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| NpcFlags::from_bits(b ^ 0x08).ok_or(FieldBodyError::UnknownValue(Unknown::NpcFlags(b), 0))
            ),
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| Blood::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::Blood(b), 1))
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
                |b, _| CreatureFlags::from_bits(b ^ 0x08).ok_or(FieldBodyError::UnknownValue(Unknown::CreatureFlags(b), 0))
            ),
            map_res(
                set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(4)),
                |b, _| Blood::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::Blood(b), 1))
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
        |w, _| ContainerFlags::from_bits(w ^ 0x08).ok_or(FieldBodyError::UnknownValue(Unknown::ContainerFlags(w), 0))
    )(input)
}

fn biped_object_field(input: &[u8]) -> IResult<&[u8], BipedObject, FieldBodyError> {
    map_res(
        set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(1)),
        |b, _| BipedObject::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::BipedObject(b), 0))
    )(input)
}

fn book_field(input: &[u8]) -> IResult<&[u8], Book, FieldBodyError> {
    map(
        tuple((
            set_err(pair(le_f32, le_u32), |_| FieldBodyError::UnexpectedEndOfField(20)),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(20)),
                |w, _| BookFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::BookFlags(w), 8))
            ),
            set_err(pair(skill_option_i32, le_u32), |_| FieldBodyError::UnexpectedEndOfField(20))
        )),
        |((weight, value), flags, (skill, enchantment))| Book {
            weight, value, flags, skill, enchantment
        }
    )(input)
}

fn potion_field(input: &[u8]) -> IResult<&[u8], Potion, FieldBodyError> {
    map(
        pair(
            set_err(
                pair(le_f32, le_u32),
                |_| FieldBodyError::UnexpectedEndOfField(12)
            ),
            bool_u32(12, 8)
        ),
        |((weight, value), auto_calculate_value)| Potion {
            weight, value, auto_calculate_value
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
            attributes: Attributes {
                strength, intelligence, willpower,
                agility, speed, endurance,
                personality, luck
            },
            skills: Skills {
                block, armorer, medium_armor, heavy_armor, blunt_weapon, long_blade, axe, spear,
                athletics, enchant, destruction, alteration, illusion, conjuration, mysticism,
                restoration, alchemy, unarmored, security, sneak, acrobatics, light_armor,
                short_blade, marksman, mercantile, speechcraft, hand_to_hand
            },
            faction, health, magicka, fatigue
        }
    )(input)
}

fn creature_field(input: &[u8]) -> IResult<&[u8], Creature, FieldBodyError> {
    map(
        tuple((
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(96)),
                |w, _| CreatureType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::CreatureType(w), 0))
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
            creature_type, level,
            attributes: Attributes {
                strength, intelligence, willpower,
                agility, speed, endurance,
                personality, luck
            },
            health, magicka, fatigue, soul, combat, magic, stealth,
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
        }
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
        }
    )(input)
}

fn effect_field(input: &[u8]) -> IResult<&[u8], Effect, FieldBodyError> {
    map(
        tuple((
            set_err(
                tuple((effect_index_option_i16, skill_option_i8, attribute_option_i8)),
                |_| FieldBodyError::UnexpectedEndOfField(24)
            ),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                |d, _| EffectRange::n(d).ok_or(FieldBodyError::UnknownValue(Unknown::EffectRange(d), 4))
            ),
            set_err(
                tuple((le_i32, le_i32, le_i32, le_i32)),
                |_| FieldBodyError::UnexpectedEndOfField(24)
            ),
        )),
        |(
            (index, skill, attribute),
            range,
            (area, duration, magnitude_min, magnitude_max),
        )| Effect {
            index, skill, attribute, range,
            area, duration, magnitude_min, magnitude_max
        }
    )(input)
}

fn dialog_type_field(input: &[u8]) -> IResult<&[u8], DialogType, FieldBodyError> {
    map_res(
        set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(1)),
        |b, _| DialogType::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::DialogType(b as u32), 0))
    )
    (input)
}

fn field_body<'a>(
    code_page: CodePage,
    mode: RecordReadMode,
    record_tag: Tag,
    prev_tag: Tag,
    field_tag: Tag,
    field_size: u32
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Field, FieldBodyError> {
    move |input| {
        let field_type = FieldType::from_tags(record_tag, prev_tag, field_tag);
        match field_type {
            FieldType::U8List => map(u8_list_field, Field::U8List)(input),
            FieldType::ScriptData => map(script_data_field(code_page), Field::ScriptData)(input),
            FieldType::U8ListZip => map(u8_list_zip_field, Field::U8List)(input),
            FieldType::Multiline(newline) => map(multiline_field(code_page, newline), Field::StringList)(input),
            FieldType::Item => map(item_field(code_page), Field::Item)(input),
            FieldType::CurrentTime => map(current_time_field, Field::CurrentTime)(input),
            FieldType::Time => map(time_field, Field::Time)(input),
            FieldType::String(Some(len)) => map(short_string_field(code_page, mode, len), Field::String)(input),
            FieldType::String(None) => map(string_field(code_page), Field::String)(input),
            FieldType::StringZ => map(string_z_field(code_page), Field::StringZ)(input),
            FieldType::StringZList => map(string_z_list_field(code_page), Field::StringZList)(input),
            FieldType::FileMetadata => map(file_metadata_field(code_page), Field::FileMetadata)(input),
            FieldType::Spell => map(spell_field, Field::Spell)(input),
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
            FieldType::Faction => map(faction_field, Field::Faction)(input),
            FieldType::Armor => map(armor_field, Field::Armor)(input),
            FieldType::Weapon => map(weapon_field, Field::Weapon)(input),
            FieldType::Pos => map(pos_field, Field::Pos)(input),
            FieldType::PosRot => map(pos_rot_field, Field::PosRot)(input),
            FieldType::Skill => map(skill_field, Field::Skill)(input),
            FieldType::EffectArg => map(effect_arg_field, Field::EffectArg)(input),
            FieldType::EffectIndex => map(effect_index_field, Field::EffectIndex)(input),
            FieldType::Tag => map(tag_field, Field::Tag)(input),
            FieldType::EffectMetadata => map(effect_metadata_field, Field::EffectMetadata)(input),
            FieldType::Tool => map(tool_field, Field::Tool)(input),
            FieldType::RepairItem => map(repair_item_field, |x| Field::Tool(x.into()))(input),
            FieldType::BipedObject => map(biped_object_field, Field::BipedObject)(input),
            FieldType::BodyPart => map(body_part_field, Field::BodyPart)(input),
            FieldType::Clothing => map(clothing_field, Field::Clothing)(input),
            FieldType::Race => map(race_field, Field::Race)(input),
            FieldType::Enchantment => map(enchantment_field, Field::Enchantment)(input),
            FieldType::Creature => map(creature_field, Field::Creature)(input),
            FieldType::ContainerFlags => map(container_flags_field, Field::ContainerFlags)(input),
            FieldType::Grid => map(grid_field, Field::Grid)(input),
            FieldType::Color => map(color_field, Field::Color)(input),
            FieldType::Interior => map(interior_field, Field::Interior)(input),
            FieldType::Sound => map(sound_field, Field::Sound)(input),
            FieldType::SoundGen => map(sound_gen_field, Field::SoundGen)(input),
            FieldType::Info => map(info_field, Field::Info)(input),
            FieldType::SkillMetadata => map(skill_metadata_field, Field::SkillMetadata)(input),
            FieldType::Potion => map(potion_field, Field::Potion)(input),
            FieldType::I32 => map(i32_field, Field::I32)(input),
            FieldType::I16 => map(i16_field, Field::I16)(input),
            FieldType::I64 => map(i64_field, Field::I64)(input),
            FieldType::U8 => map(u8_field, Field::U8)(input),
            FieldType::Bool8 => map(bool_u8(1, 0), Field::Bool)(input),
            FieldType::Bool32 => map(bool_u32(4, 0), Field::Bool)(input),
            FieldType::MarkerU8(none) => map(none_u8_field(none), |()| Field::None)(input),
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
                x => Err(nom::Err::Failure(FieldBodyError::UnexpectedFieldSize(x))),
            },
            FieldType::PathGrid => map(path_grid_field, Field::PathGrid)(input),
            FieldType::Class => map(class_field, Field::Class)(input),
            FieldType::Attributes => map(attributes_field, Field::Attributes)(input),
            FieldType::Skills => map(skills_field, Field::Skills)(input),
            FieldType::Effect => map(effect_field, Field::Effect)(input),
            FieldType::DialogMetadata => match field_size {
                4 => map(i32_field, Field::I32)(input),
                1 => map(dialog_type_field, Field::DialogType)(input),
                x => Err(nom::Err::Failure(FieldBodyError::UnexpectedFieldSize(x))),
            },
            FieldType::PosRotOrCell => match field_size {
                24 => map(pos_rot_field, Field::PosRot)(input),
                12 => map(cell_field, Field::Cell)(input),
                x => Err(nom::Err::Failure(FieldBodyError::UnexpectedFieldSize(x))),
            },
            FieldType::Weather => match field_size {
                8 => map(weather_field, Field::Weather)(input),
                10 => map(weather_ex_field, Field::Weather)(input),
                x => Err(nom::Err::Failure(FieldBodyError::UnexpectedFieldSize(x))),
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
    UnexpectedFieldSize(Tag, u32),
    InvalidValue(Tag, Invalid, u32),
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

fn field<'a>(
    code_page: CodePage,
    mode: RecordReadMode,
    record_tag: Tag,
    prev_tag: Tag,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], (Tag, Field), FieldError> {
    map_res(
        and_then(
            field_bytes,
            move |(field_tag, field_size, field_bytes), _| {
                let (remaining_field_bytes, field_body) = map_err(
                    field_body(code_page, mode, record_tag, prev_tag, field_tag, field_size),
                    move |e, _| match e {
                        FieldBodyError::UnexpectedEndOfField(n) => FieldError::FieldSizeMismatch(field_tag, n, field_size),
                        FieldBodyError::UnknownValue(v, o) => FieldError::UnknownValue(field_tag, v, o),
                        FieldBodyError::InvalidValue(v, o) => FieldError::InvalidValue(field_tag, v, o),
                        FieldBodyError::UnexpectedFieldSize(s) => FieldError::UnexpectedFieldSize(field_tag, s),
                    }
                )(field_bytes)?;
                Ok((field_tag, field_size, remaining_field_bytes, field_body))
            }
        ),
        move |(field_tag, field_size, remaining_field_bytes, field_body), _| {
            if !remaining_field_bytes.is_empty() {
                return Err(
                    FieldError::FieldSizeMismatch(field_tag, field_size - remaining_field_bytes.len() as u32, field_size)
                );
            }
            Ok((field_tag, field_body))
        }
    )
}

#[derive(Debug, Clone)]
pub struct UnknownRecordFlags {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub value: u64
}

impl Display for UnknownRecordFlags {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f, "invalid record flags value {:016X}h at {:X}h in {} record started at {:X}h",
            self.value,
            self.record_offset + 8,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for UnknownRecordFlags {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub struct RecordSizeMismatch {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub expected_size: u32,
    pub actual_size: u32
}

impl Display for RecordSizeMismatch {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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

impl Display for FieldSizeMismatch {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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

impl Display for UnexpectedFieldSize {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
    AiServices(u32),
    AiTargetFlags(u8),
    AiTravelFlags(u32),
    ApparatusType(u32),
    ArmorType(u32),
    Attribute(u32),
    BipedObject(u8),
    Blood(u8),
    BodyPartFlags(u8),
    BodyPartKind(u8),
    BodyPartType(u8),
    BookFlags(u32),
    CellFlags(u32),
    ClothingType(u32),
    ContainerFlags(u32),
    CreatureFlags(u8),
    CreatureType(u32),
    DialogType(u32),
    EffectFlags(u32),
    EffectIndex(u32),
    EffectRange(u32),
    EnchantmentAutoCalculate(i16),
    EnchantmentType(u32),
    FileType(u32),
    LightFlags(u32),
    NpcFlags(u8),
    RaceFlags(u32),
    School(u32),
    Skill(u32),
    SoundGen(u32),
    Specialization(u32),
    SpellFlags(u32),
    SpellType(u32),
    WeaponFlags(u32),
    WeaponType(u16),
}

impl Display for Unknown {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Unknown::FileType(v) => write!(f, "file type {v:08X}h"),
            Unknown::EffectRange(v) => write!(f, "effect range {v}"),
            Unknown::DialogType(v) => write!(f, "dialog type {v}"),
            Unknown::SpellType(v) => write!(f, "spell type {v}"),
            Unknown::SpellFlags(v) => write!(f, "spell flags {v:08X}h"),
            Unknown::AiServices(v) => write!(f, "AI services {v:08X}h"),
            Unknown::NpcFlags(v) => write!(f, "NPC flags {v:02X}h"),
            Unknown::CreatureFlags(v) => write!(f, "creature flags {v:02X}h"),
            Unknown::Blood(v) => write!(f, "blood {v}"),
            Unknown::ContainerFlags(v) => write!(f, "container flags {v:08X}h"),
            Unknown::CreatureType(v) => write!(f, "creature type {v}"),
            Unknown::LightFlags(v) => write!(f, "light flags {v:08X}h"),
            Unknown::ApparatusType(v) => write!(f, "apparatus type {v}"),
            Unknown::WeaponFlags(v) => write!(f, "weapon flags {v:08X}h"),
            Unknown::WeaponType(v) => write!(f, "weapon type {v}"),
            Unknown::ArmorType(v) => write!(f, "armor type {v}"),
            Unknown::BodyPartKind(v) => write!(f, "body part kind {v}"),
            Unknown::BodyPartType(v) => write!(f, "body part type {v}"),
            Unknown::BodyPartFlags(v) => write!(f, "body part flags {v:02X}h"),
            Unknown::BipedObject(v) => write!(f, "biped object {v}"),
            Unknown::ClothingType(v) => write!(f, "clothing type {v}"),
            Unknown::AiTravelFlags(v) => write!(f, "AI travel flags {v:08X}h"),
            Unknown::AiTargetFlags(v) => write!(f, "AI travel flags {v:02X}h"),
            Unknown::EnchantmentType(v) => write!(f, "enchantment type {v}"),
            Unknown::EnchantmentAutoCalculate(v) => write!(f, "enchantment 'Auto Calculate' value {v}"),
            Unknown::CellFlags(v) => write!(f, "cell flags {v:08X}h"),
            Unknown::EffectFlags(v) => write!(f, "effect flags {v:08X}h"),
            Unknown::RaceFlags(v) => write!(f, "race flags {v:08X}h"),
            Unknown::Specialization(v) => write!(f, "specialization {v}"),
            Unknown::Attribute(v) => write!(f, "attribute {v}"),
            Unknown::Skill(v) => write!(f, "skill {v}"),
            Unknown::School(v) => write!(f, "school {v}"),
            Unknown::SoundGen(v) => write!(f, "sound gen {v}"),
            Unknown::EffectIndex(v) => write!(f, "effect index {v}"),
            Unknown::BookFlags(v) => write!(f, "book flags {v:08X}h"),
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

impl Display for UnknownValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
pub enum Invalid {
    Bool(u32),
    Color(u32),
    ColorComponent(u32),
    MarkerU8(u8)
}

impl Display for Invalid {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Invalid::Color(v) => write!(f, "RGB color {v:08X}h"),
            Invalid::ColorComponent(v) => write!(f, "RGB color component {v}"),
            Invalid::Bool(v) => write!(f, "boolean {v}"),
            Invalid::MarkerU8(v) => write!(f, "one-byte marker {v}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InvalidValue {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub field_offset: u32,
    pub field_tag: Tag,
    pub value_offset: u32,
    pub value: Invalid,
}

impl Display for InvalidValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f, "Invalid {} at {:X}h in {} field started at {:X}h in {} record started at {:X}h",
            self.value,
            self.record_offset + 16 + self.field_offset as u64 + 8 + self.value_offset as u64,
            self.field_tag,
            self.record_offset + 16 + self.field_offset as u64,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for InvalidValue {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub enum RecordError {
    FieldSizeMismatch(FieldSizeMismatch),
    InvalidValue(InvalidValue),
    RecordSizeMismatch(RecordSizeMismatch),
    UnexpectedFieldSize(UnexpectedFieldSize),
    UnknownRecordFlags(UnknownRecordFlags),
    UnknownValue(UnknownValue),
}

impl RecordError {
    pub fn record_tag(&self) -> Tag {
        match self {
            RecordError::UnknownRecordFlags(x) => x.record_tag,
            RecordError::RecordSizeMismatch(x) => x.record_tag,
            RecordError::FieldSizeMismatch(x) => x.record_tag,
            RecordError::UnexpectedFieldSize(x) => x.record_tag,
            RecordError::UnknownValue(x) => x.record_tag,
            RecordError::InvalidValue(x) => x.record_tag,
        }
    }

    pub fn record_offset(&self) -> u64 {
        match self {
            RecordError::UnknownRecordFlags(x) => x.record_offset,
            RecordError::RecordSizeMismatch(x) => x.record_offset,
            RecordError::FieldSizeMismatch(x) => x.record_offset,
            RecordError::UnexpectedFieldSize(x) => x.record_offset,
            RecordError::UnknownValue(x) => x.record_offset,
            RecordError::InvalidValue(x) => x.record_offset,
        }
    }
}

impl Display for RecordError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            RecordError::UnknownRecordFlags(x) => Display::fmt(x, f),
            RecordError::RecordSizeMismatch(x) => Display::fmt(x, f),
            RecordError::FieldSizeMismatch(x) => Display::fmt(x, f),
            RecordError::UnexpectedFieldSize(x) => Display::fmt(x, f),
            RecordError::UnknownValue(x) => Display::fmt(x, f),
            RecordError::InvalidValue(x) => Display::fmt(x, f),
        }
    }
}

impl Error for RecordError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(match self {
            RecordError::UnknownRecordFlags(x) => x,
            RecordError::RecordSizeMismatch(x) => x,
            RecordError::FieldSizeMismatch(x) => x,
            RecordError::UnexpectedFieldSize(x) => x,
            RecordError::UnknownValue(x) => x,
            RecordError::InvalidValue(x) => x,
        })
    }
}

impl_parse_error!(<'a>, &'a [u8], RecordError);

fn record_head(input: &[u8]) -> IResult<&[u8], (Tag, u32, u64), RecordError> {
    set_err(tuple((tag, le_u32, le_u64)), |_| { panic!(); })(input)
}

fn read_record_head(input: &[u8]) -> Result<(Tag, u32, u64), RecordError> {
    match record_head(input) {
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

fn sliding_many0<I, O, E, F, G, S: Copy, X>(f: G, x: X, s: S) -> impl FnMut(I) -> IResult<I, Vec<O>, E> where
    I: Clone + nom::InputLength,
    F: nom::Parser<I, O, E>,
    G: Fn(S) -> F,
    E: ParseError<I>,
    X: Fn(&O) -> S,
{
    move |mut i: I| {
        let mut acc = Vec::with_capacity(4);
        let mut s = s;
        loop {
            let len = i.input_len();
            match f(s).parse(i.clone()) {
                Err(nom::Err::Error(_)) => return Ok((i, acc)),
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    // infinite loop check: the parser must always consume
                    if i1.input_len() == len {
                        return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many0)));
                    }
                    i = i1;
                    s = x(&o);
                    acc.push(o);
                }
            }
        }
    }
}

fn record_body<'a>(
    code_page: CodePage,
    mode: RecordReadMode,
    record_tag: Tag
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Vec<(Tag, Field)>, RecordBodyError<'a>> {
    sliding_many0(
        move |prev_tag: Tag| preceded(
            |input: &'a [u8]| {
                if input.is_empty() {
                    // error type doesn't matter
                    Err(nom::Err::Error(RecordBodyError(FieldError::UnexpectedEndOfRecord(0), input)))
                } else {
                    Ok((input, ()))
                }
            },
            cut(map_err(field(code_page, mode, record_tag, prev_tag), RecordBodyError))
        ),
        |s: &(Tag, Field)| s.0,
        META
    )
}

fn read_record_body(record_offset: u64, code_page: CodePage, mode: RecordReadMode,
                    record_tag: Tag, record_size: u32, record_flags: RecordFlags,
                    input: &[u8])
    -> Result<Record, RecordError> {
    
    let (remaining_record_bytes, record_body) = map_err(
        record_body(code_page, mode, record_tag),
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
            RecordBodyError(FieldError::InvalidValue(field_tag, value, value_offset), field) =>
                RecordError::InvalidValue(InvalidValue {
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
    source: Either<RecordError, io::Error>,
    bytes: Vec<u8>
}

static INVALID_DATA_IO_ERROR: LazyLock<io::Error> = LazyLock::new(|| io::Error::from(io::ErrorKind::InvalidData));

impl ReadRecordError {
    pub fn as_io_error(&self) -> &io::Error {
        self.source.as_ref().right_or_else(|_| &INVALID_DATA_IO_ERROR)
    }
    
    pub fn into_io_error(self) -> io::Error {
        self.source.right_or_else(|_| io::Error::from(io::ErrorKind::InvalidData))
    }

    pub fn as_bytes(&self) -> &[u8] { &self.bytes }

    pub fn into_bytes(self) -> Vec<u8> { self.bytes }

    pub fn into_tuple(self) -> (Either<RecordError, io::Error>, Vec<u8>) {
        (self.source, self.bytes)
    }

    pub fn source(&self) -> Either<&RecordError, &io::Error> { self.source.as_ref() }

    pub fn into_source(self) -> Either<RecordError, io::Error> { self.source }
}

impl From<ReadRecordError> for io::Error {
    fn from(e: ReadRecordError) -> io::Error { e.into_io_error() }
}

impl Display for ReadRecordError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.source {
            Left(record_error) => Display::fmt(record_error, f),
            Right(io_error) => Display::fmt(io_error, f),
        }
    }
}

impl Error for ReadRecordError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(match &self.source {
            Left(record_error) => record_error,
            Right(io_error) => io_error,
        })
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum RecordReadMode {
    Strict,
    Lenient
}

pub struct RecordReader {
    buf: Vec<u8>,
}

#[allow(clippy::new_without_default)]
impl RecordReader {
    pub fn new() -> Self {
        RecordReader {
            buf: Vec::with_capacity(16)
        }
    }

    fn read_chunk(&mut self, input: &mut (impl Read + ?Sized)) -> Result<usize, ReadRecordError> {
        read_and_ignore_interrupts(input, &mut self.buf[..])
            .map_err(|io_error| ReadRecordError {
                source: Right(io_error),
                bytes: Vec::new()
            })
    }

    fn fill_buf(&mut self, mut from: usize, input: &mut (impl Read + ?Sized))
        -> Result<(), ReadRecordError> {

        while from < self.buf.len() {
            let read = read_and_ignore_interrupts(input, &mut self.buf[from..])
                .map_err(|io_error| ReadRecordError {
                    source: Right(io_error),
                    bytes: {
                        let mut bytes = replace(&mut self.buf, Vec::with_capacity(16));
                        bytes.truncate(from);
                        bytes
                    }
                })?;
            if read == 0 {
                return Err(ReadRecordError {
                    source: Right(io::Error::from(io::ErrorKind::UnexpectedEof)),
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

    pub fn read<Input: Read + ?Sized>(&mut self, code_page: CodePage, mode: RecordReadMode, offset: u64, input: &mut Input)
        -> Result<Option<(Record, u32)>, ReadRecordError> {

        self.buf.resize(16, 0);
        let read = self.read_chunk(input)?;
        if read == 0 { return Ok(None); }
        self.fill_buf(read, input)?;
        let (record_tag, record_size, record_flags) =
            read_record_head(&self.buf[..]).map_err(|record_error| ReadRecordError {
                source: Left(record_error),
                bytes: replace(&mut self.buf, Vec::with_capacity(16))
            })?;
        self.buf.resize(16 + record_size as usize, 0);
        self.fill_buf(16, input)?;
        let record_flags = RecordFlags::from_bits(record_flags)
            .ok_or_else(|| ReadRecordError {
                source: Left(RecordError::UnknownRecordFlags(UnknownRecordFlags {
                    record_offset: offset,
                    record_tag,
                    value: record_flags
                })),
                bytes: replace(&mut self.buf, Vec::with_capacity(16))
            })?;
        let record = read_record_body(
            offset, code_page, mode, record_tag, record_size, record_flags,
            &self.buf[16..]).map_err(|record_error| ReadRecordError {
                source: Left(record_error),
                bytes: replace(&mut self.buf, Vec::with_capacity(16))
            }
        )?;
        Ok(Some((record, 16 + record_size)))
    }
}

pub struct Records<'a, Input: Read + ?Sized> {
    code_page: CodePage,
    mode: RecordReadMode,
    input: &'a mut Input,
    offset: u64,
    reader: RecordReader,
}

impl<'a, Input: Read + ?Sized> Records<'a, Input> {
    pub fn new(code_page: CodePage, mode: RecordReadMode, offset: u64, input: &'a mut Input) -> Self {
        Records {
            code_page,
            mode,
            input,
            offset,
            reader: RecordReader::new()
        }
    }
}

impl<'a, Input: Read + ?Sized> Iterator for Records<'a, Input> {
    type Item = Result<Record, ReadRecordError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.reader.read(self.code_page, self.mode, self.offset, self.input) {
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
    use serde_serialize_seed::ValueWithSeed;
    use crate::code::{self};

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
            RecordReader::new().read(CodePage::English, RecordReadMode::Strict, 0x11, &mut (&input[..])).unwrap().unwrap();
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
        let result = RecordReader::new().read(CodePage::English, RecordReadMode::Strict, 0x11, &mut (&input[..]));
        let error = result.err().unwrap();
        if let Left(RecordError::UnknownRecordFlags(error)) = error.into_source() { 
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
        let result = field(CodePage::English, RecordReadMode::Strict, DIAL, META)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Failure(FieldError::FieldSizeMismatch(DELE, expected, actual)) = error {
            assert_eq!(expected, 4);
            assert_eq!(actual, 6);
        } else {
            panic!("{:?}", error)
        }
    }

    #[test]
    fn read_deletion_mark_mismatching_size_field_lesser() {
        let mut input: Vec<u8> = Vec::new();
        input.extend(DELE.dword.to_le_bytes().iter());
        input.extend(2u32.to_le_bytes().iter());
        input.extend([0x00, 0x00, 0x00, 0x00, 0x00, 0x00].iter());
        let result = field(CodePage::English, RecordReadMode::Strict, DIAL, META)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldError::FieldSizeMismatch(DELE, expected, actual)) = error {
            assert_eq!(expected, 4);
            assert_eq!(actual, 2);
        } else {
            panic!("{:?}", error)
        }
    }

    #[test]
    fn read_deletion_mark_mismatching_size_lesser() {
        let mut input: Vec<u8> = Vec::new();
        input.extend(DELE.dword.to_le_bytes().iter());
        input.extend(2u32.to_le_bytes().iter());
        input.extend([0x00, 0x00].iter());
        let result = field(CodePage::English, RecordReadMode::Strict, DIAL, META)(&input);
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
                field_body(CodePage::Russian, RecordReadMode::Strict, INFO, META, BNAM, input.len() as u32)(input).unwrap() {
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
        field_body(CodePage::English, RecordReadMode::Strict, TES3, META, HEDR, input.len() as u32)(&input).err().unwrap();
    }

    #[test]
    fn read_from_vec_if_let() {
        let input: Vec<u8> = Vec::new();
        let res = field_body(CodePage::English, RecordReadMode::Strict, TES3, META, HEDR, input.len() as u32)(&input);
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
        let result = field_body(CodePage::English, RecordReadMode::Strict, TES3, META, HEDR, input.len() as u32)(&input);
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
        let result = field_body(CodePage::English, RecordReadMode::Strict, TES3, META, HEDR, input.len() as u32)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Failure(FieldBodyError::UnknownValue(Unknown::FileType(val), offset)) = error {
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
        let result = field_body(CodePage::English, RecordReadMode::Strict, TES3, META, HEDR, input.len() as u32)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldBodyError::UnexpectedEndOfField(size)) = error {
            assert_eq!(size, 300);
        } else {
            panic!("{:?}", error)
        }
    }
    
    #[test]
    fn serialize_ingredient() {
        let ingredient = Ingredient {
            weight: 10.0,
            value: 117,
            effect_1_index: Right(EffectIndex::AbsorbFatigue), effect_2_index: Right(EffectIndex::Charm),
            effect_3_index: Right(EffectIndex::SummonClannfear), effect_4_index: Right(EffectIndex::Chameleon),
            effect_1_skill: Left(None), effect_2_skill: Right(Skill::Armorer),
            effect_3_skill: Right(Skill::Axe), effect_4_skill: Right(Skill::Block),
            effect_1_attribute: Left(None), effect_2_attribute: Right(Attribute::Luck),
            effect_3_attribute: Right(Attribute::Endurance), effect_4_attribute: Right(Attribute::Strength)
        };
        let bin: Vec<u8> = code::serialize(&ingredient, CodePage::English, false).unwrap();
        let res = ingredient_field(&bin).unwrap().1;
        assert_eq!(res, ingredient);
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
        let bin: Vec<u8> = code::serialize(&ValueWithSeed(&script_metadata, ScriptMetadataSerde { code_page: Some(CodePage::English) }), CodePage::English, false).unwrap();
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
        let bin: Vec<u8> = code::serialize(&ValueWithSeed(&file_metadata, FileMetadataSerde { code_page: Some(CodePage::English) }), CodePage::English, false).unwrap();
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
            index: Right(EffectIndex::Recall),
            skill: Right(Skill::Enchant),
            attribute: Right(Attribute::Agility),
            range: EffectRange::Touch,
            area: 1333,
            duration: 200,
            magnitude_min: 1,
            magnitude_max: 300
        };
        let bin: Vec<u8> = code::serialize(&effect, CodePage::English, false).unwrap();
        let res = effect_field(&bin).unwrap().1;
        assert_eq!(res.index, effect.index);
        assert_eq!(res.skill, effect.skill);
        assert_eq!(res.attribute, effect.attribute);
        assert_eq!(res.range, effect.range);
        assert_eq!(res.area, effect.area);
        assert_eq!(res.duration, effect.duration);
        assert_eq!(res.magnitude_min, effect.magnitude_min);
        assert_eq!(res.magnitude_max, effect.magnitude_max);
    }

    #[test]
    fn serialize_effect_undefined_values() {
        let effect = Effect {
            index: Left(None),
            skill: Left(Some(-10)),
            attribute: Left(None),
            range: EffectRange::Touch,
            area: 1333,
            duration: 200,
            magnitude_min: 1,
            magnitude_max: 300
        };
        let bin: Vec<u8> = code::serialize(&effect, CodePage::English, false).unwrap();
        let res = effect_field(&bin).unwrap().1;
        assert_eq!(res.index, effect.index);
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
        let bin: Vec<u8> = code::serialize(&npc_state, CodePage::English, false).unwrap();
        let res = npc_state_field(&bin).unwrap().1;
        assert_eq!(res.disposition, npc_state.disposition);
        assert_eq!(res.reputation, npc_state.reputation);
        assert_eq!(res.index, npc_state.index);
    }

    #[test]
    fn serialize_npc_stats() {
        let stats = NpcStats {
            attributes: Attributes {
                strength: 1, intelligence: 2, willpower: 3, agility: 4,
                speed: 5, endurance: 6, personality: 7, luck: 8
            },
            skills: Skills {
                block: 9, armorer: 10, medium_armor: 11, heavy_armor: 12,
                blunt_weapon: 13, long_blade: 14, axe: 15, spear: 16, athletics: 17, enchant: 18,
                destruction: 19, alteration: 20, illusion: 21, conjuration: 22, mysticism: 23,
                restoration: 24, alchemy: 25, unarmored: 26, security: 27, sneak: 28, acrobatics: 29,
                light_armor: 30, short_blade: 31, marksman: 32, mercantile: 33, speechcraft: 34,
                hand_to_hand: 35
            },
            faction: 36, health: -37, magicka: -38, fatigue: 39
        };
        let bin: Vec<u8> = code::serialize(&stats, CodePage::English, false).unwrap();
        let res = npc_stats(&bin).unwrap().1;
        assert_eq!(res, stats);
    }

    #[test]
    fn serialize_npc_52() {
        let npc_stats = NpcStats {
            attributes: Attributes {
                strength: 1, intelligence: 2, willpower: 3, agility: 4,
                speed: 5, endurance: 6, personality: 7, luck: 8    
            },
            skills: Skills {
                block: 9, armorer: 10, medium_armor: 11, heavy_armor: 12, blunt_weapon: 13, long_blade: 14,
                axe: 15, spear: 16, athletics: 17, enchant: 18, destruction: 19, alteration: 20,
                illusion: 21, conjuration: 22, mysticism: 23, restoration: 24, alchemy: 25, unarmored: 26,
                security: 27, sneak: 28, acrobatics: 29, light_armor: 30, short_blade: 31,
                marksman: 32, mercantile: 33, speechcraft: 34, hand_to_hand: 35
            },
            faction: 36, health: -37, magicka: -38, fatigue: 39
        };
        let npc = Npc {
            level: 100,
            disposition: -100,
            reputation: -92,
            rank: 33,
            gold: 20000,
            padding: 17,
            stats: Right(npc_stats)
        };
        let bin: Vec<u8> = code::serialize(&npc, CodePage::English, true).unwrap();
        let res = npc_52_field(&bin).unwrap().1;
        assert_eq!(res, npc);
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
        let bin: Vec<u8> = code::serialize(&npc, CodePage::English, true).unwrap();
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
        let spell = Spell {
            flags: SpellFlags::empty(),
            cost: 40,
            spell_type: SpellType::Curse
        };
        let bin: Vec<u8> = code::serialize(&spell, CodePage::English, false).unwrap();
        let res = spell_field(&bin).unwrap().1;
        assert_eq!(res, spell);
    }

    #[test]
    fn serialize_item() {
        let item = Item {
            count: -3,
            item_id: "b_item_01 ".into()
        };
        let bin: Vec<u8> = code::serialize(&ValueWithSeed(&item, ItemSerde { code_page: Some(CodePage::English) }), CodePage::English, false).unwrap();
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
        let bin: Vec<u8> = code::serialize(&ValueWithSeed(&record, RecordSerde { code_page: Some(CodePage::English) }), CodePage::English, true).unwrap();
        println!("{:?}", bin);
        let mut bin = &bin[..];
        let records = Records::new(CodePage::English, RecordReadMode::Strict, 0, &mut bin);
        let records = records.map(|x| x.unwrap()).collect::<Vec<_>>();
        assert_eq!(records.len(), 1);
        let res = &records[0];
        assert_eq!(res.tag, record.tag);
        assert_eq!(res.flags, record.flags);
        assert_eq!(res.fields.len(), 2);
    }

    #[test]
    fn lenient_mode() {
        let mut input: Vec<u8> = Vec::new();
        input.extend(CREA.dword.to_le_bytes().iter());
        input.extend(18u32.to_le_bytes().iter());
        input.extend(0u64.to_le_bytes().iter());
        input.extend(NPCS.dword.to_le_bytes().iter());
        input.extend(10u32.to_le_bytes().iter());
        input.extend(string(&len(10, "spell")));
        let mut input = &input[..];
        let records = Records::new(CodePage::English, RecordReadMode::Lenient, 0, &mut input);
        let records = records.map(|x| x.unwrap()).collect::<Vec<_>>();
        assert_eq!(records.len(), 1);
        let res = &records[0];
        assert_eq!(res.tag, CREA);
        assert_eq!(res.fields.len(), 1);
        assert_eq!(res.fields[0].0, NPCS);
        if let Field::String(s) = &res.fields[0].1 {
            assert_eq!(s, "spell")
        } else {
            panic!()
        }
    }
}
