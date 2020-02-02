use crate::core::*;

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
use either::Either;

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
    UnknownFileType(u32),
    NonZeroDeletionMark(u32),
    NonZeroNpcPaddingByte(u8),
    UnexpectedNpcFieldSize(u32),
    InvalidEffectRange(i32),
    InvalidDialogType(u8),
    UnexpectedDialogMetadataFieldSize(u32),
}

impl_parse_error!(<'a>, &'a [u8], FieldBodyError);

fn binary_field<E>(input: &[u8]) -> IResult<&[u8], Vec<u8>, E> {
    Ok((&input[input.len() .. ], input.into()))
}

fn compressed_field<E>(input: &[u8]) -> IResult<&[u8], Vec<u8>, E> {
    let mut encoder = ZlibEncoder::new(Vec::new(), Compression::new(5));
    encoder.write_all(input).unwrap();
    Ok((&input[input.len() .. ], encoder.finish().unwrap()))
}

fn trim_end_nulls(mut bytes: &[u8]) -> &[u8] {
    while !bytes.is_empty() && bytes[bytes.len() - 1] == 0 {
        bytes = &bytes[..bytes.len() - 1];
    }
    bytes
}

fn decode_string(code_page: CodePage, bytes: &[u8]) -> String {
    code_page.encoding().decode(bytes, DecoderTrap::Strict).unwrap()
}

fn string_field<E>(code_page: CodePage, trim_tail_zeros: bool) -> impl Fn(&[u8]) -> IResult<&[u8], String, E> {
    move |input| {
        Ok((&input[input.len()..], {
            let input = if trim_tail_zeros {
                trim_end_nulls(input)
            } else {
                input
            };
            decode_string(code_page, input)
        }))
    }
}

fn string_z_field<E>(code_page: CodePage, allow_coerce: bool) -> impl Fn(&[u8]) -> IResult<&[u8], StringZ, E> {
    move |input| {
        Ok((&input[input.len()..], {
            let (input, has_tail_zero) = if allow_coerce {
                (trim_end_nulls(input), true)
            } else if !input.is_empty() && input[input.len() - 1] == 0 {
                (&input[..input.len() - 1], true)                
            } else {
                (input, false)
            };
            StringZ { str: decode_string(code_page, input), has_tail_zero }
        }))
    }
}

fn fixed_string<'a>(code_page: CodePage, length: u32) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], String, ()> {
    map(take(length), move |bytes| decode_string(code_page, trim_end_nulls(bytes)))
}

fn file_metadata_field<'a>(code_page: CodePage)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], FileMetadata, FieldBodyError> {

    map(
        tuple((
            set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(300)),
            map_res(
                set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(300)),
                |w, _| FileType::from_u32(w).ok_or(nom::Err::Error(FieldBodyError::UnknownFileType(w)))
            ),
            set_err(
                tuple((
                    fixed_string(code_page, 32),
                    map(
                        fixed_string(code_page, 256), 
                        |s| s.split(LinebreakStyle::Dos.new_line()).map(String::from).collect()
                    ),
                    le_u32
                )),
                |_| FieldBodyError::UnexpectedEndOfField(300)
            )
        )),
        |(version, file_type, (author, description, records_count))| FileMetadata {
            version, file_type, author, description, records_count
        }
    )
}

fn fixed_string_field<'a>(code_page: CodePage, length: u32) -> impl Fn(&'a [u8])
    -> IResult<&'a [u8], String, FieldBodyError> {
    
    set_err(fixed_string(code_page, length), move |_| FieldBodyError::UnexpectedEndOfField(length))
}

fn multiline_field<'a, E>(code_page: CodePage, linebreaks: LinebreakStyle, trim_tail_zeros: bool)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<String>, E> {

    no_err(
        map(
            string_field(code_page, trim_tail_zeros),
            move |s| s.split(linebreaks.new_line()).map(String::from).collect()
        )
    )
}

fn multi_string_field<'a, E>(code_page: CodePage) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<String>, E> {
    no_err(
        map(string_field(code_page, false), |s| s.split("\0").map(String::from).collect())
    )
}

fn reference_field<'a>(code_page: CodePage) 
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], (i32, String), FieldBodyError> {

    set_err(
        pair(
            le_i32,
            fixed_string(code_page, 32)
        ),
        |_| FieldBodyError::UnexpectedEndOfField(4 + 32)
    )
}

fn deletion_mark_field(input: &[u8]) -> IResult<&[u8], (), FieldBodyError> {
    map_res(
        set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(4)),
        |d, _| if d == 0 { Ok(()) } else { Err(nom::Err::Error(FieldBodyError::NonZeroDeletionMark(d))) }
    )(input)
}

fn int_field(input: &[u8]) -> IResult<&[u8], i32, FieldBodyError> {
    set_err(le_i32, |_| FieldBodyError::UnexpectedEndOfField(4))(input)
}

fn short_field(input: &[u8]) -> IResult<&[u8], i16, FieldBodyError> {
    set_err(le_i16, |_| FieldBodyError::UnexpectedEndOfField(2))(input)
}

fn long_field(input: &[u8]) -> IResult<&[u8], i64, FieldBodyError> {
    set_err(le_i64, |_| FieldBodyError::UnexpectedEndOfField(8))(input)
}

fn byte_field(input: &[u8]) -> IResult<&[u8], u8, FieldBodyError> {
    set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(1))(input)
}

fn float_field(input: &[u8]) -> IResult<&[u8], f32, FieldBodyError> {
    set_err(le_f32, |_| FieldBodyError::UnexpectedEndOfField(4))(input)
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

fn script_metadata_field<'a>(code_page: CodePage)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], ScriptMetadata, FieldBodyError> {

    map(
        set_err(
            tuple((
                fixed_string(code_page, 32),
                le_u32, le_u32, le_u32,
                le_u32, le_u32,
            )),
            |_| FieldBodyError::UnexpectedEndOfField(32 + 20)
        ),
        |(name, shorts, longs, floats, data_size, var_table_size)| ScriptMetadata {
            name,
            shorts,
            longs,
            floats,
            data_size,
            var_table_size
        }
    )
}

fn saved_npc_field(input: &[u8]) -> IResult<&[u8], SavedNpc, FieldBodyError> {
    map(
        set_err(
            tuple((
                le_i16,
                le_i16,
                le_u32
            )),
            |_| FieldBodyError::UnexpectedEndOfField(8)
        ),
        |(disposition, reputation, index)| SavedNpc {
            disposition,
            reputation,
            index
        }
    )(input)
}

fn npc_characteristics(input: &[u8]) -> IResult<&[u8], NpcCharacteristics, ()> {
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
        )| NpcCharacteristics {
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

fn npc_52_field(input: &[u8]) -> IResult<&[u8], Npc, FieldBodyError> {
    map_res(
        set_err(
            tuple((le_u16, npc_characteristics, le_i8, le_i8, le_i8, le_u8, le_i32)),
            |_| FieldBodyError::UnexpectedEndOfField(52)
        ),
        |(level, characteristics, disposition, reputation, rank, padding, gold), _| {
            if padding != 0 {
                Err(nom::Err::Error(FieldBodyError::NonZeroNpcPaddingByte(padding)))
            } else {
                Ok(Npc {
                    level, disposition, reputation, rank, gold,
                    characteristics: Either::Right(characteristics)
                })
            }
        }
    )(input)
}

fn npc_12_field(input: &[u8]) -> IResult<&[u8], Npc, FieldBodyError> {
    map(
        set_err(
            tuple((le_u16, le_i8, le_i8, le_i8, le_u8, le_u16, le_i32)),
            |_| FieldBodyError::UnexpectedEndOfField(12)
        ),
        |(level, disposition, reputation, rank, u1, u2, gold)| Npc {
            level, disposition, reputation, rank, gold,
            characteristics: Either::Left((u1 as u32) << 16 | (u2 as u32))
        }
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
                set_err(le_i32, |_| FieldBodyError::UnexpectedEndOfField(24)),
                |d, _| EffectRange::from_i32(d).ok_or(nom::Err::Error(FieldBodyError::InvalidEffectRange(d)))
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
            id, skill, attribute, range,
            area, duration, magnitude_min, magnitude_max
        }
    )(input)
}

fn dialog_metadata_1_field(input: &[u8]) -> IResult<&[u8], Either<u32, DialogType>, FieldBodyError> {
    map(
        map_res(
            set_err(le_u8, |_| FieldBodyError::UnexpectedEndOfField(1)),
            |d, _| DialogType::from_u8(d).ok_or(nom::Err::Error(FieldBodyError::InvalidDialogType(d)))
        ),
        Either::Right
    )(input)
}

fn dialog_metadata_4_field(input: &[u8]) -> IResult<&[u8], Either<u32, DialogType>, FieldBodyError> {
    map(
        set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(4)),
        Either::Left
    )(input)
}

fn field_body<'a>(code_page: CodePage, allow_coerce: bool, record_tag: Tag, field_tag: Tag, field_size: u32)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Field, FieldBodyError> {

    move |input| {
        let field_type = FieldType::from_tags(record_tag, field_tag);
        match field_type {
            FieldType::Binary => map(binary_field, Field::Binary)(input),
            FieldType::Compressed => map(compressed_field, Field::Compressed)(input),
            FieldType::Multiline { linebreaks, trim_tail_zeros } =>
                map(multiline_field(code_page, linebreaks, trim_tail_zeros && allow_coerce), Field::Multiline)(input),
            FieldType::Reference =>
                map(reference_field(code_page), |(count, name)| Field::Reference(count, name))(input),
            FieldType::FixedString(len) => map(fixed_string_field(code_page, len), Field::String)(input),
            FieldType::String { trim_tail_zeros } =>
                map(string_field(code_page, trim_tail_zeros && allow_coerce), Field::String)(input),
            FieldType::StringZ => map(string_z_field(code_page, allow_coerce), Field::StringZ)(input),
            FieldType::MultiString => map(multi_string_field(code_page), Field::MultiString)(input),
            FieldType::FileMetadata => map(file_metadata_field(code_page), Field::FileMetadata)(input),
            FieldType::DeletionMark => map(deletion_mark_field, |()| Field::DeletionMark)(input),
            FieldType::Int => map(int_field, Field::Int)(input),
            FieldType::Short => map(short_field, Field::Short)(input),
            FieldType::Long => map(long_field, Field::Long)(input),
            FieldType::Byte => map(byte_field, Field::Byte)(input),
            FieldType::Float => map(float_field, Field::Float)(input),
            FieldType::Ingredient => map(ingredient_field, Field::Ingredient)(input),
            FieldType::ScriptMetadata => map(script_metadata_field(code_page), Field::ScriptMetadata)(input),
            FieldType::SavedNpc => map(saved_npc_field, Field::SavedNpc)(input),
            FieldType::Npc => match field_size {
                52 => map(npc_52_field, Field::Npc)(input),
                12 => map(npc_12_field, Field::Npc)(input),
                x => Err(nom::Err::Error(FieldBodyError::UnexpectedNpcFieldSize(x))),
            },
            FieldType::Effect => map(effect_field, Field::Effect)(input),
            FieldType::DialogMetadata => match field_size {
                4 => map(dialog_metadata_4_field, Field::DialogMetadata)(input),
                1 => map(dialog_metadata_1_field, Field::DialogMetadata)(input),
                x => Err(nom::Err::Error(FieldBodyError::UnexpectedDialogMetadataFieldSize(x))),
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
    UnknownFileType(u32),
    NonZeroDeletionMark(u32),
    NonZeroNpcPaddingByte(u8),
    UnexpectedNpcFieldSize(u32),
    InvalidEffectRange(i32),
    InvalidDialogType(u8),
    UnexpectedDialogMetadataFieldSize(u32),
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

fn field<'a>(code_page: CodePage, allow_coerce: bool, record_tag: Tag)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Field, FieldError> {
    
    map_res(
        field_bytes,
        move |(field_tag, field_size, field_bytes), _| {
            let (remaining_field_bytes, field_body) = map_err(
                field_body(code_page, allow_coerce, record_tag, field_tag, field_size),
                move |e, _| match e {
                    FieldBodyError::UnexpectedEndOfField(n) => FieldError::FieldSizeMismatch(field_tag, n, field_size),
                    FieldBodyError::UnknownFileType(d) => FieldError::UnknownFileType(d),
                    FieldBodyError::NonZeroDeletionMark(d) => FieldError::NonZeroDeletionMark(d),
                    FieldBodyError::NonZeroNpcPaddingByte(d) => FieldError::NonZeroNpcPaddingByte(d),
                    FieldBodyError::UnexpectedNpcFieldSize(d) => FieldError::UnexpectedNpcFieldSize(d),
                    FieldBodyError::InvalidEffectRange(d) => FieldError::InvalidEffectRange(d),
                    FieldBodyError::InvalidDialogType(d) => FieldError::InvalidDialogType(d),
                    FieldBodyError::UnexpectedDialogMetadataFieldSize(d) => FieldError::UnexpectedDialogMetadataFieldSize(d),
                }
            )(field_bytes)?;
            if !remaining_field_bytes.is_empty() {
                return Err(nom::Err::Error(FieldError::FieldSizeMismatch(field_tag, field_size - remaining_field_bytes.len() as u32, field_size)));
            }
            Ok(field_body)
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
            f, "record size mismatch, expected {:04X}h, found {:04X}h at {:X}h in {} record started at {:X}h",
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
            f, "field size mismatch, expected {:04X}h, found {:04X}h at {:X}h in {} field started at {:X}h in {} record started at {:X}h",
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
pub struct UnknownFileType {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub field_offset: u32,
    pub field_tag: Tag,
    pub value_offset: u32,
    pub value: u32
}

impl fmt::Display for UnknownFileType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "unknown file type {:04X}h at {:X}h in {} field started at {:X}h in {} record started at {:X}h",
            self.value,
            self.record_offset + 16 + self.field_offset as u64 + 8 + self.value_offset as u64,
            self.field_tag,
            self.record_offset + 16 + self.field_offset as u64,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for UnknownFileType {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub struct NonZeroDeletionMark {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub field_offset: u32,
    pub field_tag: Tag,
    pub value: u32
}

impl fmt::Display for NonZeroDeletionMark {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "found non-zero deletion mark value {:04X}h at {:X}h in {} field started at {:X}h in {} record started at {:X}h",
            self.value,
            self.record_offset + 16 + self.field_offset as u64 + 8,
            self.field_tag,
            self.record_offset + 16 + self.field_offset as u64,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for NonZeroNpcPaddingByte {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub struct NonZeroNpcPaddingByte {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub field_offset: u32,
    pub field_tag: Tag,
    pub value_offset: u32,
    pub value: u8
}

impl fmt::Display for NonZeroNpcPaddingByte {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "found non-zero padding byte {:01X}h at {:X}h in {} field started at {:X}h in {} record started at {:X}h",
            self.value,
            self.record_offset + 16 + self.field_offset as u64 + 8 + self.value_offset as u64,
            self.field_tag,
            self.record_offset + 16 + self.field_offset as u64,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for NonZeroDeletionMark {
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
pub struct InvalidEffectRange {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub field_offset: u32,
    pub field_tag: Tag,
    pub value_offset: u32,
    pub value: i32,
}

impl fmt::Display for InvalidEffectRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "invalid effect range {} at {:X}h in {} field started at {:X}h in {} record started at {:X}h",
            self.value,
            self.record_offset + 16 + self.field_offset as u64 + 8 + self.value_offset as u64,
            self.field_tag,
            self.record_offset + 16 + self.field_offset as u64,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for InvalidEffectRange {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub struct InvalidDialogType {
    pub record_offset: u64,
    pub record_tag: Tag,
    pub field_offset: u32,
    pub field_tag: Tag,
    pub value_offset: u32,
    pub value: u8
}

impl fmt::Display for InvalidDialogType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "invalid dialog type {} at {:X}h in {} field started at {:X}h in {} record started at {:X}h",
            self.value,
            self.record_offset + 16 + self.field_offset as u64 + 8 + self.value_offset as u64,
            self.field_tag,
            self.record_offset + 16 + self.field_offset as u64,
            self.record_tag,
            self.record_offset
        )
    }
}

impl Error for InvalidDialogType {
    fn source(&self) -> Option<&(dyn Error + 'static)> { None }
}

#[derive(Debug, Clone)]
pub enum RecordError {
    UnexpectedEof(UnexpectedEof),
    InvalidRecordFlags(InvalidRecordFlags),
    RecordSizeMismatch(RecordSizeMismatch),
    FieldSizeMismatch(FieldSizeMismatch),
    UnknownFileType(UnknownFileType),
    NonZeroDeletionMark(NonZeroDeletionMark),
    NonZeroNpcPaddingByte(NonZeroNpcPaddingByte),
    UnexpectedFieldSize(UnexpectedFieldSize),
    InvalidEffectRange(InvalidEffectRange),
    InvalidDialogType(InvalidDialogType),
}

impl fmt::Display for RecordError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordError::UnexpectedEof(x) => x.fmt(f),
            RecordError::InvalidRecordFlags(x) => x.fmt(f),
            RecordError::RecordSizeMismatch(x) => x.fmt(f),
            RecordError::FieldSizeMismatch(x) => x.fmt(f),
            RecordError::UnknownFileType(x) => x.fmt(f),
            RecordError::NonZeroDeletionMark(x) => x.fmt(f),
            RecordError::NonZeroNpcPaddingByte(x) => x.fmt(f),
            RecordError::UnexpectedFieldSize(x) => x.fmt(f),
            RecordError::InvalidEffectRange(x) => x.fmt(f),
            RecordError::InvalidDialogType(x) => x.fmt(f),
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
            RecordError::UnknownFileType(x) => x,
            RecordError::NonZeroDeletionMark(x) => x,
            RecordError::NonZeroNpcPaddingByte(x) => x,
            RecordError::UnexpectedFieldSize(x) => x,
            RecordError::InvalidEffectRange(x) => x,
            RecordError::InvalidDialogType(x) => x,
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

fn record_body<'a>(code_page: CodePage, allow_coerce: bool, record_tag: Tag) 
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<Field>, RecordBodyError<'a>> {
    
    many0(
        preceded(
            |input| {
                if input.is_empty() {
                    Err(nom::Err::Error(RecordBodyError(FieldError::UnexpectedEndOfRecord(0), input))) // error type doesn't matter
                } else {
                    Ok((input, ()))
                }
            },
            cut(map_err(field(code_page, allow_coerce, record_tag), |e, input| RecordBodyError(e, input)))
        )
    )
}

fn read_record_body(record_offset: u64, code_page: CodePage, allow_coerce: bool,
                    record_tag: Tag, record_size: u32, record_flags: RecordFlags,
                    input: &[u8])
    -> Result<Record, RecordError> {
    
    let (remaining_record_bytes, record_body) = map_err(
        record_body(code_page, allow_coerce, record_tag),
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
            RecordBodyError(FieldError::UnknownFileType(value), field) =>
                RecordError::UnknownFileType(UnknownFileType {
                    record_offset, value,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32,
                    record_tag: TES3, field_tag: HEDR, value_offset: 4
                }),
            RecordBodyError(FieldError::NonZeroDeletionMark(value), field) =>
                RecordError::NonZeroDeletionMark(NonZeroDeletionMark {
                    record_offset, value,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32,
                    record_tag: DIAL, field_tag: DELE
                }),
            RecordBodyError(FieldError::NonZeroNpcPaddingByte(value), field) =>
                RecordError::NonZeroNpcPaddingByte(NonZeroNpcPaddingByte {
                    record_offset, value,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32,
                    record_tag: NPC_, field_tag: NPDT, value_offset: 47
                }),
            RecordBodyError(FieldError::UnexpectedNpcFieldSize(field_size), field) =>
                RecordError::UnexpectedFieldSize(UnexpectedFieldSize {
                    record_offset, field_size,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32,
                    record_tag: NPC_, field_tag: NPDT
                }),
            RecordBodyError(FieldError::InvalidEffectRange(value), field) =>
                RecordError::InvalidEffectRange(InvalidEffectRange {
                    record_offset, record_tag, value,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32,
                    field_tag: ENAM, value_offset: 4
                }),
            RecordBodyError(FieldError::InvalidDialogType(value), field) =>
                RecordError::InvalidDialogType(InvalidDialogType {
                    record_offset, value,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32,
                    record_tag: DIAL, field_tag: DATA, value_offset: 0
                }),
            RecordBodyError(FieldError::UnexpectedDialogMetadataFieldSize(field_size), field) =>
                RecordError::UnexpectedFieldSize(UnexpectedFieldSize {
                    record_offset,  field_size,
                    field_offset: unsafe { field.as_ptr().offset_from(input.as_ptr()) } as u32,
                    record_tag: DIAL, field_tag: DATA
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

    pub fn read<Input: Read + ?Sized>(&mut self,
                                      code_page: CodePage, allow_coerce: bool,
                                      offset: u64, input: &mut Input)
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
        let record = read_record_body(offset, code_page, allow_coerce, record_tag, record_size, record_flags,
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
    allow_coerce: bool,
    reader: RecordReader,
}

impl<'a, Input: Read + ?Sized> Records<'a, Input> {
    pub fn new(code_page: CodePage, allow_coerce: bool, offset: u64, input: &'a mut Input) -> Self {
        Records {
            code_page,
            input,
            allow_coerce,
            offset,
            reader: RecordReader::new()
        }
    }
}

impl<'a, Input: Read + ?Sized> Iterator for Records<'a, Input> {
    type Item = Result<Record, ReadRecordError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.reader.read(self.code_page, self.allow_coerce, self.offset, self.input) {
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
            RecordReader::new().read(CodePage::English, false, 0x11, &mut (&input[..])).unwrap().unwrap();
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
        let result = RecordReader::new().read(CodePage::English, false, 0x11, &mut (&input[..]));
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
        let result = field(CodePage::English, true, DIAL)(&input);
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
        let result = field(CodePage::English, true, DIAL)(&input);
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
        let result = field(CodePage::English, true, DIAL)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldError::FieldSizeMismatch(DELE, expected, actual)) = error {
            assert_eq!(expected, 4);
            assert_eq!(actual, 2);
        } else {
            panic!()
        }
    }

    #[test]
    fn read_deletion_mark_non_zero() {
        let mut input: Vec<u8> = Vec::new();
        input.extend(DELE.dword.to_le_bytes().iter());
        input.extend(4u32.to_le_bytes().iter());
        input.extend([0x01, 0x00, 0x00, 0x00].iter());
        let result = field(CodePage::English, true, DIAL)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldError::NonZeroDeletionMark(d)) = error {
            assert_eq!(d, 1);
        } else {
            panic!()
        }
    }

    #[test]
    fn read_multiline_field() {
        let input: &'static [u8] = b"123\r\n\xC0\xC1t\r\n\xDA\xDFX\r\n";
        if let (remaining_input, Field::Multiline(result)) =
                field_body(CodePage::Russian, true, CONT, BNAM, input.len() as u32)(input).unwrap() {
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
        field_body(CodePage::English, true, TES3, HEDR, input.len() as u32)(&input).err().unwrap();
    }

    #[test]
    fn read_from_vec_if_let() {
        let input: Vec<u8> = Vec::new();
        let res = field_body(CodePage::English, true, TES3, HEDR, input.len() as u32)(&input);
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
        let result = field_body(CodePage::English, true, TES3, HEDR, input.len() as u32)(&input);
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
        let result = field_body(CodePage::English, true, TES3, HEDR, input.len() as u32)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldBodyError::UnknownFileType(val)) = error {
            assert_eq!(val, 0x100000);
        } else {
            panic!()
        }
    }
    
    #[test]
    fn read_file_metadata_eof_in_file_type() {
        let mut input: Vec<u8> = Vec::new();
        input.extend([0x00, 0x00, 0x00, 0x22].iter());
        input.extend([0x20, 0x00, 0x00].iter());
        let result = field_body(CodePage::English, true, TES3, HEDR, input.len() as u32)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldBodyError::UnexpectedEndOfField(size)) = error {
            assert_eq!(size, 300);
        } else {
            panic!()
        }
    }
}
