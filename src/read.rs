use crate::code_page::CodePage;
use crate::script_data::*;
use crate::strings::*;
use crate::field::*;
use crate::record::*;
use either::{Right, Left, Either};
use flate2::write::ZlibEncoder;
use flate2::Compression;
use mynom::{Parser, u8, u16_le, u32_le, parser, accumulate_until_eof, consume, take, f32_le, i32_le, i16_le};
use mynom::{error, i8, UnexpectedEof, i64_le, u64_le, f64_le};
use std::convert::TryInto;
use std::error::Error;
use std::fmt::{self, Display, Debug, Formatter};
use std::hash::Hash;
use std::io::{self, Read, Write};
use std::mem::replace;
use std::sync::LazyLock;

#[derive(Debug, Clone)]
enum FieldBodyError {
    UnexpectedEndOfField(u32),
    UnknownValue(Unknown, u32),
    UnexpectedFieldSize(u32),
    InvalidValue(Invalid, u32),
}

fn u8_list_field<'p, E>() -> impl Parser<'p, Result=Vec<u8>, Error=E> {
    consume().map(|x| x.into()).map_err(|x| x)
}

fn script_data_field<'p, E>(code_page: CodePage) -> impl Parser<'p, Result=ScriptData, Error=E> {
    consume().map(move |x| ScriptData::from_bytes(code_page, x)).map_err(|x| x)
}

fn u8_list_zip_field<'p, E>() -> impl Parser<'p, Result=Vec<u8>, Error=E> {
    consume().map(|input| {
        let mut encoder = ZlibEncoder::new(Vec::new(), Compression::new(5));
        encoder.write_all(input).unwrap();
        encoder.finish().unwrap()
    }).map_err(|x| x)
}

fn trim_end_nulls(bytes: &[u8]) -> &[u8] {
    let cut_to = bytes.iter().rposition(|&x| x != 0).map_or(0, |i| i + 1);
    &bytes[..cut_to]
}

fn string_field<'p>(code_page: CodePage) -> impl Parser<'p, Result=String, Error=FieldBodyError> {
    consume().map(move |x| code_page.decode(x)).map_err(|x| x)
}

fn string_z_field<'p>(code_page: CodePage) -> impl Parser<'p, Result=StringZ, Error=FieldBodyError> {
    consume().map(move |input| {
        let has_tail_zero = input.last() == Some(&0);
        let input = if has_tail_zero {
            &input[..input.len() - 1]
        } else {
            input
        };
        StringZ { string: code_page.decode(input), has_tail_zero }
    }).map_err(|x| x)
}

fn string_z_list_field<'p>(
    code_page: CodePage
) -> impl Parser<'p, Result=StringZList, Error=FieldBodyError> {
    string_z_field(code_page)
        .map(|s| StringZList {
            vec: s.string.split('\0').map(String::from).collect(), has_tail_zero: s.has_tail_zero
        })
}

fn short_string<'p>(
    code_page: CodePage,
    length: u32,
) -> impl Parser<'p, Result=String, Error=UnexpectedEof> {
    take(usize::try_from(length).expect("OOM")).map(
        move |bytes| code_page.decode(trim_end_nulls(bytes))
    )
}

fn file_metadata_20_field<'p>() -> impl Parser<'p, Result=FileMetadata, Error=FieldBodyError> {
    (
        u32_le().map_err(|_| FieldBodyError::UnexpectedEndOfField(20)),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(20))
            .map_res(|w| FileType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::FileType(w), 4))),
        (u32_le(), u32_le(), u32_le()).map_err(|_| FieldBodyError::UnexpectedEndOfField(20))
    ).map(
        |(version, file_type, (author, description, records))| FileMetadata {
            version, file_type, author: Left(author), description: Left(description), records
        }
    )
}

fn file_metadata_300_field<'p>(
    code_page: CodePage
) -> impl Parser<'p, Result=FileMetadata, Error=FieldBodyError> {
    (
        u32_le().map_err(|_| FieldBodyError::UnexpectedEndOfField(300)),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(300))
            .map_res(|w| FileType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::FileType(w), 4))),
        (
            short_string(code_page, 32),
            short_string(code_page, 256)
                .map(|s| s.split(Newline::Dos.as_str()).map(String::from).collect()),
            u32_le()
        ).map_err(|_| FieldBodyError::UnexpectedEndOfField(300))
    ).map(
        |(version, file_type, (author, description, records))| FileMetadata {
            version, file_type, author: Right(author), description: Right(description), records
        }
    )
}

fn short_string_field<'p>(
    code_page: CodePage, mode: RecordReadMode, length: u32
) -> impl Parser<'p, Result=String, Error=FieldBodyError> {
    consume().peek().map_err(|x| x).and_then(move |input| {
        let length = if mode == RecordReadMode::Lenient && input.len() < length as usize {
            input.len() as u32
        } else {
            length
        };
        short_string(code_page, length).map_err(move |_| FieldBodyError::UnexpectedEndOfField(length))
    })
}

fn multiline_field<'p>(
    code_page: CodePage,
    linebreaks: Newline
) -> impl Parser<'p, Result=Vec<String>, Error=FieldBodyError> {
    string_field(code_page).map(
        move |s| s.split(linebreaks.as_str()).map(String::from).collect()
    )
}

fn current_time_field<'p>() -> impl Parser<'p, Result=CurrentTime, Error=FieldBodyError> {
    (
        f32_le(),
        u32_le(),
        u32_le(),
        u32_le(),
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
        .map(|(hour, day, month, year)| CurrentTime { hour, day, month, year })
}

fn time_field<'p>() -> impl Parser<'p, Result=Time, Error=FieldBodyError> {
    (
        f32_le(),
        u32_le(),
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(8))
        .map(|(hour, day)| Time { hour, day })
}

fn item_field<'p>(
    code_page: CodePage
) -> impl Parser<'p, Result=Item, Error=FieldBodyError> {
    (
        i32_le(),
        short_string(code_page, 32),
    )
        .map(|(count, item_id)| Item { count, item_id })
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(4 + 32))
}

fn i32_field<'p>() -> impl Parser<'p, Result=i32, Error=FieldBodyError> {
    i32_le().map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
}

fn i32_list_field<'p>() -> impl Parser<'p, Result=Vec<i32>, Error=FieldBodyError> {
    consume().peek().map_err(|x| x).and_then(|input| {
        let m = input.len() % 4;
        if m != 0 {
            let e = (input.len() - m + 4) as u32;
            return Left(error(move || FieldBodyError::UnexpectedEndOfField(e)));
        }
        Right(i32_le().repeat_until_eof(Vec::new, |mut v, i| { v.push(i); v }).map_err(|_| unreachable!()))
    }).map(|x| x.either(|l| l, |r| r))
}

fn i16_field<'p>() -> impl Parser<'p, Result=i16, Error=FieldBodyError> {
    i16_le().map_err(|_| FieldBodyError::UnexpectedEndOfField(2))
}

fn i16_list_field<'p>() -> impl Parser<'p, Result=Vec<i16>, Error=FieldBodyError> {
    consume().peek().map_err(|x| x).and_then(|input| {
        let m = input.len() % 2;
        if m != 0 {
            let e = (input.len() - m + 2) as u32;
            return Left(error(move || FieldBodyError::UnexpectedEndOfField(e)));
        }
        Right(i16_le().repeat_until_eof(Vec::new, |mut v, i| { v.push(i); v }).map_err(|_| unreachable!()))
    }).map(|x| x.either(|l| l, |r| r))
}

fn i64_field<'p>() -> impl Parser<'p, Result=i64, Error=FieldBodyError> {
    i64_le().map_err(|_| FieldBodyError::UnexpectedEndOfField(8))
}

fn u8_field<'p>() -> impl Parser<'p, Result=u8, Error=FieldBodyError> {
    u8().map_err(|_| FieldBodyError::UnexpectedEndOfField(1))
}

fn f32_field<'p>() -> impl Parser<'p, Result=f32, Error=FieldBodyError> {
    f32_le().map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
}

fn f64_field<'p>() -> impl Parser<'p, Result=f64, Error=FieldBodyError> {
    f64_le().map_err(|_| FieldBodyError::UnexpectedEndOfField(8))
}

fn f32_list_field<'p>() -> impl Parser<'p, Result=Vec<f32>, Error=FieldBodyError> {
    consume().peek().map_err(|x| x).and_then(|input| {
        let m = input.len() % 4;
        if m != 0 {
            let e = (input.len() - m + 4) as u32;
            return Left(error(move || FieldBodyError::UnexpectedEndOfField(e)));
        }
        Right(f32_le().repeat_until_eof(Vec::new, |mut v, f| { v.push(f); v }).map_err(|_| unreachable!()))
    }).map(|x| x.either(|l| l, |r| r))
}

fn attribute_option_i8<'p>() -> impl Parser<'p, Result=Either<Option<i8>, Attribute>, Error=UnexpectedEof> {
    i8().map(move |b| if b == -1 { 
        Left(None)
    } else if let Some(a) = b.try_into().ok().and_then(Attribute::n) {
        Right(a)
    } else {
        Left(Some(b))
    })
}

fn attribute_option_i32<'p>() -> impl Parser<'p, Result=Either<Option<i32>, Attribute>, Error=UnexpectedEof> {
    i32_le().map(move |b| if b == -1 {
        Left(None)
    } else if let Some(a) = b.try_into().ok().and_then(Attribute::n) {
        Right(a)
    } else {
        Left(Some(b))
    })
}

fn skill_option_i32<'p>() -> impl Parser<'p, Result=Either<Option<i32>, Skill>, Error=UnexpectedEof> {
    i32_le().map(move |b| if b == -1 {
        Left(None)
    } else if let Some(a) = b.try_into().ok().and_then(Skill::n) {
        Right(a)
    } else {
        Left(Some(b))
    })
}

fn skill_option_i8<'p>() -> impl Parser<'p, Result=Either<Option<i8>, Skill>, Error=UnexpectedEof> {
    i8().map(move |b| if b == -1 {
        Left(None)
    } else if let Some(a) = b.try_into().ok().and_then(Skill::n) {
        Right(a)
    } else {
        Left(Some(b))
    })
}

fn effect_index_option_i16<'p>() -> impl Parser<'p, Result=Either<Option<i16>, EffectIndex>, Error=UnexpectedEof> {
    i16_le().map(move |b| if b == -1 {
        Left(None)
    } else if let Some(a) = b.try_into().ok().and_then(EffectIndex::n) {
        Right(a)
    } else {
        Left(Some(b))
    })
}

fn effect_index_option_i32<'p>() -> impl Parser<'p, Result=Either<Option<i32>, EffectIndex>, Error=UnexpectedEof> {
    i32_le().map(move |b| if b == -1 {
        Left(None)
    } else if let Some(a) = b.try_into().ok().and_then(EffectIndex::n) {
        Right(a)
    } else {
        Left(Some(b))
    })
}

fn sex_option_i8<'p>() -> impl Parser<'p, Result=Either<Option<i8>, Sex>, Error=UnexpectedEof> {
    i8().map(move |b| if b == -1 {
        Left(None)
    } else if let Some(a) = b.try_into().ok().and_then(Sex::n) {
        Right(a)
    } else {
        Left(Some(b))
    })
}

fn option_i8<'p>() -> impl Parser<'p, Result=Option<i8>, Error=UnexpectedEof> {
    i8().map(move |b| if b == -1 {
        None
    } else {
        Some(b)
    })
}

fn ingredient_field<'p>() -> impl Parser<'p, Result=Ingredient, Error=FieldBodyError> {
    (
        f32_le(), u32_le(),
        (effect_index_option_i32(), effect_index_option_i32(), effect_index_option_i32(), effect_index_option_i32()),
        (skill_option_i32(), skill_option_i32(), skill_option_i32(), skill_option_i32()),
        (attribute_option_i32(), attribute_option_i32(), attribute_option_i32(), attribute_option_i32())
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(56))
        .map(
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
        )
}

fn sound_chance_field<'p>(
    code_page: CodePage
) -> impl Parser<'p, Result=SoundChance, Error=FieldBodyError> {
    (
        short_string(code_page, 32),
        u8()
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(32 + 1))
        .map(|(sound_id, chance)| SoundChance { sound_id, chance })
}

fn script_metadata_field<'p>(
    code_page: CodePage
) -> impl Parser<'p, Result=ScriptMetadata, Error=FieldBodyError> {
    (
        short_string(code_page, 32),
        u32_le(), u32_le(), u32_le(),
        u32_le(), u32_le(),
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(32 + 20))
        .map(
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

fn script_vars_field<'p>() -> impl Parser<'p, Result=ScriptVars, Error=FieldBodyError> {
    (u32_le(), u32_le(), u32_le())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
        .map(
            |(shorts, longs, floats)| ScriptVars {
                shorts,
                longs,
                floats
            }
        )
}

fn info_field<'p>() -> impl Parser<'p, Result=Info, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
            .map_res(
                |w| w.try_into().ok().and_then(DialogType::n).ok_or(
                    FieldBodyError::UnknownValue(Unknown::DialogType(w), 0)
                )
            ),
        (u32_le(), option_i8(), sex_option_i8(), option_i8(), u8())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
    )
        .map(
            |(dialog_type, (disp_index, rank, sex, pc_rank, padding))| Info {
                dialog_type, disp_index, rank, sex, pc_rank, padding
            }
        )
}

fn rank<'p>() -> impl Parser<'p, Result=Rank, Error=UnexpectedEof> {
    (u32_le(), u32_le(), u32_le(), u32_le(), u32_le())
        .map(
            |(attribute_1, attribute_2, primary_skill, favored_skill, reputation)| Rank {
                attribute_1, attribute_2, primary_skill, favored_skill, reputation
            }
        )
}

fn faction_field<'p>() -> impl Parser<'p, Result=Faction, Error=FieldBodyError> {
    (
        attribute(240, 0), attribute(240, 4),
        (
            (rank(), rank(), rank(), rank(), rank(), rank(), rank(), rank(), rank(), rank()),
            (
                skill_option_i32(), skill_option_i32(), skill_option_i32(), skill_option_i32(),
                skill_option_i32(), skill_option_i32(), skill_option_i32()
            )
        )
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(240)),
        bool_u32(240, 236)
    )
        .map(
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
        )
}

fn weather_field<'p>() -> impl Parser<'p, Result=Weather, Error=FieldBodyError> {
    (
        u8(), u8(), u8(), u8(), u8(),
        u8(), u8(), u8()
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(8))
        .map(
            |(clear, cloudy, foggy, overcast, rain, thunder, ash, blight)| Weather {
                clear, cloudy, foggy, overcast, rain, thunder, ash, blight, ex: None
            }
        )
}

fn weather_ex_field<'p>() -> impl Parser<'p, Result=Weather, Error=FieldBodyError> {
    (
        u8(), u8(), u8(), u8(), u8(),
        u8(), u8(), u8(), u8(), u8()
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(10))
        .map(
            |(clear, cloudy, foggy, overcast, rain, thunder, ash, blight, snow, blizzard)| Weather {
                clear, cloudy, foggy, overcast, rain, thunder, ash, blight,
                ex: Some(WeatherEx { snow, blizzard })
            }
        )
}

fn npc_state_field<'p>() -> impl Parser<'p, Result=NpcState, Error=FieldBodyError> {
    (i16_le(), i16_le(), u32_le())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(8))
        .map(
            |(disposition, reputation, index)| NpcState {
                disposition,
                reputation,
                index
            }
        )
}

fn cell_field<'p>() -> impl Parser<'p, Result=Cell, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
            .map_res(
                |w| CellFlags::from_bits(w).ok_or(
                    FieldBodyError::UnknownValue(Unknown::CellFlags(w), 0)
                )
            ),
        (i32_le(), i32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
    )
        .map(
            |(flags, (x, y))| {
                let position = if flags.contains(CellFlags::INTERIOR) {
                    CellPosition::Interior {
                        x: f32::from_bits(x.cast_unsigned()),
                        y: f32::from_bits(y.cast_unsigned())
                    }
                } else {
                    CellPosition::Exterior { x, y }
                };
                Cell { flags, position }
            }
        )
}

fn path_grid_field<'p>() -> impl Parser<'p, Result=PathGrid, Error=FieldBodyError> {
    (i32_le(), i32_le(), u16_le(), u16_le())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
        .map(|(x, y, flags, points)| PathGrid { grid: Grid { x, y }, flags, points })
}

fn grid_field<'p>() -> impl Parser<'p, Result=Grid, Error=FieldBodyError> {
    (i32_le(), i32_le())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(8))
        .map(|(x, y)| Grid { x, y })
}

fn spell_field<'p>() -> impl Parser<'p, Result=Spell, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
            .map_res(
                |w| SpellType::n(w).ok_or(
                    FieldBodyError::UnknownValue(Unknown::SpellType(w), 0)
                )
            ),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12)),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
            .map_res(
                |w| SpellFlags::from_bits(w).ok_or(
                    FieldBodyError::UnknownValue(Unknown::SpellFlags(w), 8)
                )
            ),
    )
        .map(
            |(spell_type, cost, flags)| Spell {
                spell_type, cost, flags
            }
        )
}

fn light_field<'p>() -> impl Parser<'p, Result=Light, Error=FieldBodyError> {
    (
        (f32_le(), u32_le(), i32_le(), u32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24)),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24))
            .map_res(
                |w| Color::try_from_u32(w).ok_or(
                    FieldBodyError::InvalidValue(Invalid::Color(w), 16)
                )
            ),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24))
            .map_res(
                |w| LightFlags::from_bits(w).ok_or(
                    FieldBodyError::UnknownValue(Unknown::LightFlags(w), 20)
                )
            )
    )
        .map(
            |((weight, value, time, radius), color, flags)| Light {
                weight, value, time, radius, color, flags
            }
        )
}

fn interior_field<'p>() -> impl Parser<'p, Result=Interior, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
            .map_res(
                |w| Color::try_from_u32(w).ok_or(
                    FieldBodyError::InvalidValue(Invalid::Color(w), 0)
                )
            ),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
            .map_res(
                |w| Color::try_from_u32(w).ok_or(
                    FieldBodyError::InvalidValue(Invalid::Color(w), 4)
                )
            ),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
            .map_res(
                |w| Color::try_from_u32(w).ok_or(
                    FieldBodyError::InvalidValue(Invalid::Color(w), 8)
                )
            ),
        f32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
    )
        .map(
            |(ambient, sunlight, fog, fog_density)| Interior {
                ambient, sunlight, fog, fog_density
            }
        )
}

fn sound_field<'p>() -> impl Parser<'p, Result=Sound, Error=FieldBodyError> {
    (u8(), u8(), u8())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(3))
        .map(
            |(volume, range_min, range_max)| Sound {
                volume, range_min, range_max
            }
        )
}

fn color_component<'p>(
    field_size: u32,
    offset: u32
) -> impl Parser<'p, Result=u8, Error=FieldBodyError> {
    u32_le()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(field_size))
        .map_res(move |w|
            w.try_into().map_err(|_| FieldBodyError::InvalidValue(Invalid::ColorComponent(w), offset))
        )
}

fn effect_metadata_field<'p>() -> impl Parser<'p, Result=EffectMetadata, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(36))
            .map_res(|w| School::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::School(w), 0))),
        f32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(36)),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(36))
            .map_res(|w|
                EffectFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::EffectFlags(w), 8))
            ),
        color_component(36, 12),
        color_component(36, 16),
        color_component(36, 20),
        (f32_le(), f32_le(), f32_le()).map_err(|_| FieldBodyError::UnexpectedEndOfField(36))
    )
        .map(
            |(school, base_cost, flags, r, g, b, (size_factor, speed, size_cap))| EffectMetadata {
                school, base_cost, flags, color: Color { r, g, b }, size_factor, speed, size_cap
            }
        )
}

fn color_field<'p>() -> impl Parser<'p, Result=Color, Error=FieldBodyError> {
    u32_le()
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(24))
        .map_res(|w| Color::try_from_u32(w).ok_or(FieldBodyError::InvalidValue(Invalid::Color(w), 0)))
}

fn misc_item_field<'p>() -> impl Parser<'p, Result=MiscItem, Error=FieldBodyError> {
    (
        (f32_le(), u32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12)),
        bool_u32(12, 8)
    )
        .map(
            |((weight, value), is_key)| MiscItem {
                weight, value, is_key
            }
        )
}

fn apparatus_field<'p>() -> impl Parser<'p, Result=Apparatus, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
            .map_res(|w| ApparatusType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::ApparatusType(w), 0))),
        (f32_le(), f32_le(), u32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
    )
        .map(
            |(apparatus_type, (quality, weight, value))| Apparatus {
                apparatus_type, quality, weight, value
            }
        )
}

fn enchantment_field<'p>() -> impl Parser<'p, Result=Enchantment, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
            .map_res(|w|
                EnchantmentType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::EnchantmentType(w), 0))
            ),
        (u32_le(), u32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16)),
        i16_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
            .map_res(|w| match w {
                0 => Ok(Right(false)),
                1 => Ok(Right(true)),
                -1 => Ok(Left(true)),
                -2 => Ok(Left(false)),
                w => Err(FieldBodyError::UnknownValue(Unknown::EnchantmentAutoCalculate(w), 12))
            }),
        u16_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
    )
        .map(
            |(enchantment_type, (cost, charge_amount), auto_calculate, padding)| Enchantment {
                enchantment_type, cost, charge_amount, auto_calculate, padding
            }
        )
}

fn armor_field<'p>() -> impl Parser<'p, Result=Armor, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24))
            .map_res(|w| ArmorType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::ArmorType(w), 0)))
            ,
        (f32_le(), u32_le(), u32_le(), u32_le(), u32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24))
            ,
    
    )
        .map(|(armor_type, (weight, value, health, enchantment, armor))| Armor {
            armor_type, health, weight, value, enchantment, armor
        })
}

fn clothing_field<'p>() -> impl Parser<'p, Result=Clothing, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
            .map_res(|w| ClothingType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::ClothingType(w), 0)))
            ,
        (f32_le(), u16_le(), u16_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
            ,
    )
        .map(|(clothing_type, (weight, value, enchantment))| Clothing {
            clothing_type, weight, value, enchantment
        })
}

fn weapon_field<'p>() -> impl Parser<'p, Result=Weapon, Error=FieldBodyError> {
    (
        (f32_le(), u32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(32))
            ,
        u16_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(32))
            .map_res(|w| WeaponType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::WeaponType(w), 8)))
            ,
        (u16_le(), f32_le(), f32_le(), u16_le(), u8(), u8(), u8(), u8(), u8(), u8())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(32))
            ,
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(32))
            .map_res(|w|
                WeaponFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::WeaponFlags(w), 28))
            )
            ,
    )
        .map(|(
            (weight, value),
            weapon_type,
            (
                health,
                speed,
                reach,
                enchantment,
                chop_min,
                chop_max,
                slash_min,
                slash_max,
                thrust_min,
                thrust_max,
            ),
            flags
        )| Weapon {
            weight, value, weapon_type, health, speed, reach, enchantment,
            chop_min, chop_max, slash_min, slash_max, thrust_min, thrust_max, flags
        })
}

fn body_part_field<'p>() -> impl Parser<'p, Result=BodyPart, Error=FieldBodyError> {
    (
        u8()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
            .map_res(|b| BodyPartKind::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::BodyPartKind(b), 0)))
            ,
        bool_u8(4, 1),
        u8()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
            .map_res(|b|
                BodyPartFlags::from_bits(b).ok_or(FieldBodyError::UnknownValue(Unknown::BodyPartFlags(b), 2))
            )
            ,
        u8()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
            .map_res(|b| BodyPartType::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::BodyPartType(b), 3)))
            ,
    )
        .map(|(kind, vampire, flags, body_part_type)| BodyPart {
            kind, vampire, flags, body_part_type
        })
}

fn ai_field<'p>() -> impl Parser<'p, Result=Ai, Error=FieldBodyError> {
    (
        (u16_le(), u8(), u8(), u8(), u8(), u16_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
            ,
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
            .map_res(|w| Services::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::AiServices(w), 8)))
            ,
    )
        .map(|((hello, fight, flee, alarm, padding_8, padding_16), services)| Ai {
            hello, fight, flee, alarm, padding_8, padding_16, services
        })
}

fn ai_wander_field<'p>() -> impl Parser<'p, Result=AiWander, Error=FieldBodyError> {
    (
        (
            u16_le(), u16_le(), u8(),
            u8(), u8(), u8(), u8(),
            u8(), u8(), u8(), u8()
        )
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(14))
            ,
        bool_u8(14, 13)
    )
        .map(|
            (
                (distance, duration, time_of_day, idle2, idle3, idle4, idle5, idle6, idle7, idle8, idle9),
                repeat,
            )
        | AiWander {
            distance,
            duration,
            time_of_day,
            idle: [idle2, idle3, idle4, idle5, idle6, idle7, idle8, idle9],
            repeat
        })
}

fn ai_travel_field<'p>() -> impl Parser<'p, Result=AiTravel, Error=FieldBodyError> {
    (
        (f32_le(), f32_le(), f32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
            ,
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
            .map_res(|w|
                AiTravelFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::AiTravelFlags(w), 12))
            )
            ,
    )
        .map(|((x, y, z), flags)| AiTravel {
            pos: Pos { x, y, z }, flags
        })
}

fn ai_target_field<'p>(code_page: CodePage) -> impl Parser<'p, Result=AiTarget, Error=FieldBodyError> {
    (
        (
            f32_le(), f32_le(), f32_le(), u16_le(),
            short_string(code_page, 32)
        )
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(48))
            ,
        bool_u8(48, 46),
        u8()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(48))
            .map_res(|b| AiTargetFlags::from_bits(b).ok_or(
                FieldBodyError::UnknownValue(Unknown::AiTargetFlags(b), 46)
            ))
            ,
    )
        .map(|((x, y, z, duration, actor_id), reset, flags)| AiTarget {
            pos: Pos { x, y, z }, duration, actor_id, reset, flags
        })
}

fn bool_u8<'p>(
    field_size: u32,
    offset: u32
) -> impl Parser<'p, Result=bool, Error=FieldBodyError> {
    u8()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(field_size))
        .map_res(move |b| match b {
            0 => Ok(false),
            1 => Ok(true),
            b => Err(FieldBodyError::InvalidValue(Invalid::Bool(b as u32), offset))
        })
}

fn bool_u32<'p>(
    field_size: u32,
    offset: u32
) -> impl Parser<'p, Result=bool, Error=FieldBodyError> {
    u32_le()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(field_size))
        .map_res(move |w| match w {
            0 => Ok(false),
            1 => Ok(true),
            w => Err(FieldBodyError::InvalidValue(Invalid::Bool(w), offset))
        })
}

fn none_u8_field<'p>(
    none: u8
) -> impl Parser<'p, Result=(), Error=FieldBodyError> {
    u8()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(1))
        .map_res(move |b| if b == none {
            Ok(())
        } else {
            Err(FieldBodyError::InvalidValue(Invalid::MarkerU8(b), 0))
        })
}

fn ai_activate_field<'p>(
    code_page: CodePage
) -> impl Parser<'p, Result=AiActivate, Error=FieldBodyError> {
    (
        short_string(code_page, 32).map_err(|_| FieldBodyError::UnexpectedEndOfField(33)),
        bool_u8(33, 32)
    )
        .map(|(object_id, reset)| AiActivate {
            object_id, reset
        })
}

fn attribute<'p>(
    field_size: u32,
    offset: u32
) -> impl Parser<'p, Result=Attribute, Error=FieldBodyError> {
    u32_le()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(field_size))
        .map_res(move |w| Attribute::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::Attribute(w), offset)))
}

fn skill<'p>(
    field_size: u32,
    offset: u32
) -> impl Parser<'p, Result=Skill, Error=FieldBodyError> {
    u32_le()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(field_size))
        .map_res(move |w| Skill::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::Skill(w), offset)))
}

fn skill_field<'p>() -> impl Parser<'p, Result=Skill, Error=FieldBodyError> {
    skill(4, 0)
}

fn effect_arg_field<'p>() -> impl Parser<'p, Result=EffectArg, Error=FieldBodyError> {
    u32_le()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(4))
        .map(EffectArg::from)
}

fn effect_index_field<'p>() -> impl Parser<'p, Result=EffectIndex, Error=FieldBodyError> {
    u32_le()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(4))
        .map_res(move |w| EffectIndex::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::EffectIndex(w), 0)))
}

fn tag_field<'p>() -> impl Parser<'p, Result=Tag, Error=FieldBodyError> {
    u32_le()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(4))
        .map(Tag::from)
}

fn sound_gen_field<'p>() -> impl Parser<'p, Result=SoundGen, Error=FieldBodyError> {
    u32_le()
        .map_err(move |_| FieldBodyError::UnexpectedEndOfField(4))
        .map_res(move |w| SoundGen::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::SoundGen(w), 0)))
}

fn class_field<'p>() -> impl Parser<'p, Result=Class, Error=FieldBodyError> {
    (
        attribute(60, 0),
        attribute(60, 4),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(60))
            .map_res(|w|
                Specialization::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::Specialization(w), 8))
            )
            ,
        (
            (
                skill(60, 12), skill(60, 16), skill(60, 20), skill(60, 24), skill(60, 28),
                skill(60, 32), skill(60, 36), skill(60, 40), skill(60, 44), skill(60, 48),
            ),
            bool_u32(60, 52)
        ),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(60))
            .map_res(|w|
                Services::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::AiServices(w), 56))
            )
            ,
    )
        .map(|(
            primary_attribute_1, primary_attribute_2, specialization,
            (
                (
                    minor_skill_1,
                    major_skill_1,
                    minor_skill_2,
                    major_skill_2,
                    minor_skill_3,
                    major_skill_3,
                    minor_skill_4,
                    major_skill_4,
                    minor_skill_5,
                    major_skill_5,
                ),
                playable
            ),
            auto_calc_services
        )| Class {
            primary_attribute_1, primary_attribute_2, specialization,
            minor_skill_1, minor_skill_2, minor_skill_3, minor_skill_4, minor_skill_5,
            major_skill_1, major_skill_2, major_skill_3, major_skill_4, major_skill_5,
            playable, auto_calc_services
        })
}

fn skill_metadata_field<'p>() -> impl Parser<'p, Result=SkillMetadata, Error=FieldBodyError> {
    (
        attribute(24, 0),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24))
            .map_res(
                |w| Specialization::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::Specialization(w), 4))
            ),
        (f32_le(), f32_le(), f32_le(), f32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24)),
    )
        .map(|(
            governing_attribute, specialization,
            (use_value_1, use_value_2, use_value_3, use_value_4)
        )| SkillMetadata {
            governing_attribute, specialization, use_value_1, use_value_2, use_value_3, use_value_4
        })
}

fn pos_field<'p>() -> impl Parser<'p, Result=Pos, Error=FieldBodyError> {
    (f32_le(), f32_le(), f32_le())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
        .map(|(x, y, z)| Pos { x, y, z })
}

fn pos_rot_field<'p>() -> impl Parser<'p, Result=PosRot, Error=FieldBodyError> {
    (
        f32_le(), f32_le(), f32_le(),
        f32_le(), f32_le(), f32_le(),
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(24))
        .map(|(pos_x, pos_y, pos_z, rot_x, rot_y, rot_z)| PosRot {
            pos: Pos { x: pos_x, y: pos_y, z: pos_z },
            rot: Rot { x: rot_x, y: rot_y, z: rot_z }
        })
}

fn attributes_field<'p>() -> impl Parser<'p, Result=Attributes<u32>, Error=FieldBodyError> {
    (
        u32_le(), u32_le(), u32_le(), u32_le(),
        u32_le(), u32_le(), u32_le(), u32_le(),
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(32))
        .map(|(
            strength, intelligence, willpower, agility,
            speed, endurance, personality, luck,
        )| Attributes {
            strength, intelligence, willpower, agility,
            speed, endurance, personality, luck,
        })
}

fn skills_field<'p>() -> impl Parser<'p, Result=Skills<u32>, Error=FieldBodyError> {
    (
        (u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le()),
        (u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le()),
        (u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le()),
    )
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(108))
        .map(|(
            (block, armorer, medium_armor, heavy_armor, blunt_weapon, long_blade, axe, spear, athletics, enchant),
            (destruction, alteration, illusion, conjuration, mysticism, restoration, alchemy, unarmored, security), 
            (sneak, acrobatics, light_armor, short_blade, marksman, mercantile, speechcraft, hand_to_hand),
        )| Skills {
            block, armorer, medium_armor, heavy_armor, blunt_weapon, long_blade, axe, spear,
            athletics, enchant, destruction, alteration, illusion, conjuration, mysticism,
            restoration, alchemy, unarmored, security, sneak, acrobatics, light_armor,
            short_blade, marksman, mercantile, speechcraft, hand_to_hand
        })
}

fn race_field<'p>() -> impl Parser<'p, Result=Race, Error=FieldBodyError> {
    (
        (
            (
                skill_option_i32(), u32_le(),
                skill_option_i32(), u32_le(),
                skill_option_i32(), u32_le(),
                skill_option_i32(), u32_le(),
                skill_option_i32(), u32_le(),
                skill_option_i32(), u32_le(),
                skill_option_i32(), u32_le()
            ),
            (
                u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(),
                u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(),
                f32_le(), f32_le(), f32_le(), f32_le()
            ),
        )
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(140)),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(140))
            .map_res(
                |w| RaceFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::RaceFlags(w), 136))
            ),
    )
        .map(|(
            (
                (
                    skill_1, skill_1_bonus, skill_2, skill_2_bonus, skill_3, skill_3_bonus, skill_4, skill_4_bonus,
                    skill_5, skill_5_bonus, skill_6, skill_6_bonus, skill_7, skill_7_bonus
                ),
                (
                    strength_m, strength_f, intelligence_m, intelligence_f,
                    willpower_m, willpower_f, agility_m, agility_f,
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
        })
}

fn npc_flags_field<'p>() -> impl Parser<'p, Result=FlagsAndBlood<NpcFlags>, Error=FieldBodyError> {
    (
        u8()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
            .map_res(
                |b| NpcFlags::from_bits(b ^ 0x08).ok_or(FieldBodyError::UnknownValue(Unknown::NpcFlags(b), 0))
            ),
        u8()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
            .map_res(
                |b| Blood::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::Blood(b), 1))
            ),
        u16_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4)),
    )
        .map(
            |(flags, blood, padding)| FlagsAndBlood {
                flags,
                blood,
                padding
            }
        )
}

fn creature_flags_field<'p>() -> impl Parser<'p, Result=FlagsAndBlood<CreatureFlags>, Error=FieldBodyError> {
    (
        u8()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
            .map_res(|b|
                CreatureFlags::from_bits(b ^ 0x08)
                    .ok_or(FieldBodyError::UnknownValue(Unknown::CreatureFlags(b), 0))
            ),
        u8()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
            .map_res(|b| Blood::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::Blood(b), 1))),
        u16_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(4)),
    )
        .map(
            |(flags, blood, padding)| FlagsAndBlood {
                flags,
                blood,
                padding
            }
        )
}

fn container_flags_field<'p>() -> impl Parser<'p, Result=ContainerFlags, Error=FieldBodyError> {
    u32_le()
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(4))
        .map_res(|w|
            ContainerFlags::from_bits(w ^ 0x08)
                .ok_or(FieldBodyError::UnknownValue(Unknown::ContainerFlags(w), 0))
        )
}

fn biped_object_field<'p>() -> impl Parser<'p, Result=BipedObject, Error=FieldBodyError> {
    u8()
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(1))
        .map_res(|b| BipedObject::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::BipedObject(b), 0)))
}

fn book_field<'p>() -> impl Parser<'p, Result=Book, Error=FieldBodyError> {
    (
        (f32_le(), u32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(20)),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(20))
            .map_res(
                |w| BookFlags::from_bits(w).ok_or(FieldBodyError::UnknownValue(Unknown::BookFlags(w), 8))
            ),
        (skill_option_i32(), u32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(20)),
    )
        .map(
            |((weight, value), flags, (skill, enchantment))| Book {
                weight, value, flags, skill, enchantment
            }
        )
}

fn potion_field<'p>() -> impl Parser<'p, Result=Potion, Error=FieldBodyError> {
    (
        (f32_le(), u32_le()).map_err(|_| FieldBodyError::UnexpectedEndOfField(12)),
        bool_u32(12, 8),
    )
        .map(
            |((weight, value), auto_calculate_value)| Potion {
                weight, value, auto_calculate_value
            }
        )
}

fn tool_field<'p>() -> impl Parser<'p, Result=Tool, Error=FieldBodyError> {
    (f32_le(), u32_le(), f32_le(), u32_le())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
        .map(
            |(weight, value, quality, uses)| Tool {
                weight, value, quality, uses
            }
        )
}

fn repair_item_field<'p>() -> impl Parser<'p, Result=RepairItem, Error=FieldBodyError> {
    (f32_le(), u32_le(), u32_le(), f32_le())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(16))
        .map(
            |(weight, value, uses, quality)| RepairItem {
                weight, value, quality, uses
            }
        )
}

fn npc_stats<'p>() -> impl Parser<'p, Result=NpcStats, Error=UnexpectedEof> {
    (
        (u8(), u8(), u8(), u8(), u8(), u8(), u8(), u8(), u8()),
        (u8(), u8(), u8(), u8(), u8(), u8(), u8(), u8(), u8()),
        (u8(), u8(), u8(), u8(), u8(), u8(), u8(), u8(), u8()),
        (u8(), u8(), u8(), u8(), u8(), u8(), u8(), u8(), u8()),
        (i16_le(), i16_le(), i16_le())
    )
        .map(|(
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
        })
}

fn creature_field<'p>() -> impl Parser<'p, Result=Creature, Error=FieldBodyError> {
    (
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(96))
            .map_res(|w| CreatureType::n(w).ok_or(FieldBodyError::UnknownValue(Unknown::CreatureType(w), 0))),
        (
            (u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le()),
            (u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le()),
            (u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le(), u32_le())
        )
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(96)),
    )
        .map(|(
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
        })
}

fn npc_52_field<'p>() -> impl Parser<'p, Result=Npc, Error=FieldBodyError> {
    (u16_le(), npc_stats(), i8(), i8(), i8(), u8(), i32_le())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(52))
        .map(
            |(level, stats, disposition, reputation, rank, padding, gold)| Npc {
                level, disposition, reputation, rank, gold,
                stats: Right(stats), padding
            }
        )
}

fn npc_12_field<'p>() -> impl Parser<'p, Result=Npc, Error=FieldBodyError> {
    (u16_le(), i8(), i8(), i8(), u8(), u16_le(), i32_le())
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(12))
        .map(
            |(level, disposition, reputation, rank, padding_8, padding_16, gold)| Npc {
                level, disposition, reputation, rank, gold,
                padding: padding_8, stats: Left(padding_16)
            }
        )
}

fn effect_field<'p>() -> impl Parser<'p, Result=Effect, Error=FieldBodyError> {
    (
        (effect_index_option_i16(), skill_option_i8(), attribute_option_i8())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24)),
        u32_le()
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24))
            .map_res(|d| EffectRange::n(d).ok_or(FieldBodyError::UnknownValue(Unknown::EffectRange(d), 4))),
        (i32_le(), i32_le(), i32_le(), i32_le())
            .map_err(|_| FieldBodyError::UnexpectedEndOfField(24)),
    )
        .map(
            |(
                (index, skill, attribute),
                range,
                (area, duration, magnitude_min, magnitude_max),
            )| Effect {
                index, skill, attribute, range,
                area, duration, magnitude_min, magnitude_max
            }
        )
}

fn dialog_type_field<'p>() -> impl Parser<'p, Result=DialogType, Error=FieldBodyError> {
    u8()
        .map_err(|_| FieldBodyError::UnexpectedEndOfField(1))
        .map_res(|b| DialogType::n(b).ok_or(FieldBodyError::UnknownValue(Unknown::DialogType(b as u32), 0)))
}

fn field_body<'p>(
    code_page: CodePage,
    mode: RecordReadMode,
    record_tag: Tag,
    prev_tag: Tag,
    field_tag: Tag,
    field_size: u32,
    omwsave: bool,
) -> impl Parser<'p, Result=Field, Error=FieldBodyError> {
    parser(move |input| {
        let field_type = FieldType::from_tags(record_tag, prev_tag, field_tag, omwsave);
        match field_type {
            FieldType::U8List => u8_list_field().map(Field::U8List).parse(input),
            FieldType::ScriptData => script_data_field(code_page).map(Field::ScriptData).parse(input),
            FieldType::U8ListZip => u8_list_zip_field().map(Field::U8List).parse(input),
            FieldType::Multiline(newline) => multiline_field(code_page, newline).map(Field::StringList).parse(input),
            FieldType::Item => item_field(code_page).map(Field::Item).parse(input),
            FieldType::CurrentTime => current_time_field().map(Field::CurrentTime).parse(input),
            FieldType::Time => time_field().map(Field::Time).parse(input),
            FieldType::String(Some(len)) => short_string_field(code_page, mode, len).map(Field::String).parse(input),
            FieldType::String(None) => string_field(code_page).map(Field::String).parse(input),
            FieldType::StringZ => string_z_field(code_page).map(Field::StringZ).parse(input),
            FieldType::StringZList => string_z_list_field(code_page).map(Field::StringZList).parse(input),
            FieldType::FileMetadata => match field_size {
                20 => file_metadata_20_field().map(Field::FileMetadata).parse(input),
                300 => file_metadata_300_field(code_page).map(Field::FileMetadata).parse(input),
                x => Err(FieldBodyError::UnexpectedFieldSize(x)),
            },
            FieldType::Spell => spell_field().map(Field::Spell).parse(input),
            FieldType::Ai => ai_field().map(Field::Ai).parse(input),
            FieldType::AiWander => ai_wander_field().map(Field::AiWander).parse(input),
            FieldType::AiTravel => ai_travel_field().map(Field::AiTravel).parse(input),
            FieldType::AiTarget => ai_target_field(code_page).map(Field::AiTarget).parse(input),
            FieldType::AiActivate => ai_activate_field(code_page).map(Field::AiActivate).parse(input),
            FieldType::NpcFlags => npc_flags_field().map(Field::NpcFlags).parse(input),
            FieldType::CreatureFlags => creature_flags_field().map(Field::CreatureFlags).parse(input),
            FieldType::Book => book_field().map(Field::Book).parse(input),
            FieldType::Light => light_field().map(Field::Light).parse(input),
            FieldType::MiscItem => misc_item_field().map(Field::MiscItem).parse(input),
            FieldType::Apparatus => apparatus_field().map(Field::Apparatus).parse(input),
            FieldType::Faction => faction_field().map(Field::Faction).parse(input),
            FieldType::Armor => armor_field().map(Field::Armor).parse(input),
            FieldType::Weapon => weapon_field().map(Field::Weapon).parse(input),
            FieldType::Pos => pos_field().map(Field::Pos).parse(input),
            FieldType::PosRot => pos_rot_field().map(Field::PosRot).parse(input),
            FieldType::Skill => skill_field().map(Field::Skill).parse(input),
            FieldType::EffectArg => effect_arg_field().map(Field::EffectArg).parse(input),
            FieldType::EffectIndex => effect_index_field().map(Field::EffectIndex).parse(input),
            FieldType::Tag => tag_field().map(Field::Tag).parse(input),
            FieldType::EffectMetadata => effect_metadata_field().map(Field::EffectMetadata).parse(input),
            FieldType::Tool => tool_field().map(Field::Tool).parse(input),
            FieldType::RepairItem => repair_item_field().map(|x| Field::Tool(x.into())).parse(input),
            FieldType::BipedObject => biped_object_field().map(Field::BipedObject).parse(input),
            FieldType::BodyPart => body_part_field().map(Field::BodyPart).parse(input),
            FieldType::Clothing => clothing_field().map(Field::Clothing).parse(input),
            FieldType::Race => race_field().map(Field::Race).parse(input),
            FieldType::Enchantment => enchantment_field().map(Field::Enchantment).parse(input),
            FieldType::Creature => creature_field().map(Field::Creature).parse(input),
            FieldType::ContainerFlags => container_flags_field().map(Field::ContainerFlags).parse(input),
            FieldType::Grid => grid_field().map(Field::Grid).parse(input),
            FieldType::Color => color_field().map(Field::Color).parse(input),
            FieldType::Interior => interior_field().map(Field::Interior).parse(input),
            FieldType::Sound => sound_field().map(Field::Sound).parse(input),
            FieldType::SoundGen => sound_gen_field().map(Field::SoundGen).parse(input),
            FieldType::Info => info_field().map(Field::Info).parse(input),
            FieldType::SkillMetadata => skill_metadata_field().map(Field::SkillMetadata).parse(input),
            FieldType::Potion => potion_field().map(Field::Potion).parse(input),
            FieldType::I32 => i32_field().map(Field::I32).parse(input),
            FieldType::I16 => i16_field().map(Field::I16).parse(input),
            FieldType::I64 => i64_field().map(Field::I64).parse(input),
            FieldType::U8 => u8_field().map(Field::U8).parse(input),
            FieldType::Bool8 => bool_u8(1, 0).map(Field::Bool).parse(input),
            FieldType::Bool32 => bool_u32(4, 0).map(Field::Bool).parse(input),
            FieldType::MarkerU8(none) => none_u8_field(none).map(|()| Field::None).parse(input),
            FieldType::F32 => f32_field().map(Field::F32).parse(input),
            FieldType::F64 => f64_field().map(Field::F64).parse(input),
            FieldType::I32List => i32_list_field().map(Field::I32List).parse(input),
            FieldType::I16List => i16_list_field().map(Field::I16List).parse(input),
            FieldType::F32List => f32_list_field().map(Field::F32List).parse(input),
            FieldType::SoundChance => sound_chance_field(code_page).map(Field::SoundChance).parse(input),
            FieldType::Ingredient => ingredient_field().map(Field::Ingredient).parse(input),
            FieldType::ScriptMetadata => script_metadata_field(code_page).map(Field::ScriptMetadata).parse(input),
            FieldType::ScriptVars => script_vars_field().map(Field::ScriptVars).parse(input),
            FieldType::NpcState => npc_state_field().map(Field::NpcState).parse(input),
            FieldType::Npc => match field_size {
                52 => npc_52_field().map(Field::Npc).parse(input),
                12 => npc_12_field().map(Field::Npc).parse(input),
                x => Err(FieldBodyError::UnexpectedFieldSize(x)),
            },
            FieldType::PathGrid => path_grid_field().map(Field::PathGrid).parse(input),
            FieldType::Class => class_field().map(Field::Class).parse(input),
            FieldType::Attributes => attributes_field().map(Field::Attributes).parse(input),
            FieldType::Skills => skills_field().map(Field::Skills).parse(input),
            FieldType::Effect => effect_field().map(Field::Effect).parse(input),
            FieldType::DialogMetadata => match field_size {
                4 => i32_field().map(Field::I32).parse(input),
                1 => dialog_type_field().map(Field::DialogType).parse(input),
                x => Err(FieldBodyError::UnexpectedFieldSize(x)),
            },
            FieldType::PosRotOrCell => match field_size {
                24 => pos_rot_field().map(Field::PosRot).parse(input),
                12 => cell_field().map(Field::Cell).parse(input),
                x => Err(FieldBodyError::UnexpectedFieldSize(x)),
            },
            FieldType::Weather => match field_size {
                8 => weather_field().map(Field::Weather).parse(input),
                10 => weather_ex_field().map(Field::Weather).parse(input),
                x => Err(FieldBodyError::UnexpectedFieldSize(x)),
            },
        }
    })
}

fn tag<'p>() -> impl Parser<'p, Result=Tag, Error=UnexpectedEof> {
    u32_le().map(Tag::from)
}

#[derive(Debug, Clone)]
enum FieldError {
    UnexpectedEndOfRecord(u32),
    FieldSizeMismatch(Tag, u32, u32),
    UnknownValue(Tag, Unknown, u32),
    UnexpectedFieldSize(Tag, u32),
    InvalidValue(Tag, Invalid, u32),
}

fn field_bytes<'p>() -> impl Parser<'p, Result=(Tag, u32, &'p [u8]), Error=FieldError> {
    (tag(), u32_le())
        .map_err(|_| FieldError::UnexpectedEndOfRecord(8))
        .and_then(|(field_tag, field_size)| {
            take(usize::try_from(field_size).expect("OOM"))
                .map_err(move |_| FieldError::UnexpectedEndOfRecord(field_size + 8))
                .map(move |field_bytes| (field_tag, field_size, field_bytes))
        })
}

fn field_end<'p>(field_tag: Tag, field_size: u32) -> impl Parser<'p, Result=(), Error=FieldError> {
    parser(move |input| {
        if !input.is_empty() {
            return Err(
                FieldError::FieldSizeMismatch(field_tag, field_size - input.len() as u32, field_size)
            );
        }
        Ok(((), input))
    })
}

fn field<'p>(
    code_page: CodePage,
    mode: RecordReadMode,
    record_tag: Tag,
    prev_tag: Tag,
    omwsave: bool,
) -> impl Parser<'p, Result=(Tag, Field), Error=FieldError> {
    field_bytes()
        .map_parser(move |(field_tag, field_size, field_bytes)| (
            (
                field_body(code_page, mode, record_tag, prev_tag, field_tag, field_size, omwsave)
                    .map_err(
                        move |e| match e {
                            FieldBodyError::UnexpectedEndOfField(n) =>
                                FieldError::FieldSizeMismatch(field_tag, n, field_size),
                            FieldBodyError::UnknownValue(v, o) => FieldError::UnknownValue(field_tag, v, o),
                            FieldBodyError::InvalidValue(v, o) => FieldError::InvalidValue(field_tag, v, o),
                            FieldBodyError::UnexpectedFieldSize(s) =>
                                FieldError::UnexpectedFieldSize(field_tag, s),
                        }
                    ),
                field_end(field_tag, field_size),
            ).map(move |(field, _)| (field_tag, field)),
            field_bytes
        ))
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

fn record_head<'p>() -> impl Parser<'p, Result=(Tag, u32, u64), Error=RecordError> {
    (tag(), u32_le(), u64_le()).map_err(|_| panic!())
}

fn read_record_head(input: &[u8]) -> Result<(Tag, u32, u64), RecordError> {
    match record_head().parse(input) {
        Ok((res, rem)) => {
            debug_assert!(rem.is_empty());
            Ok(res)
        },
        Err(err) => Err(err)
    }
}

#[derive(Debug, Clone)]
struct RecordBodyError<'p>(FieldError, &'p [u8]);

fn record_body<'p>(
    code_page: CodePage,
    mode: RecordReadMode,
    record_tag: Tag,
    omwsave: bool,
) -> impl Parser<'p, Result=Vec<(Tag, Field)>, Error=RecordBodyError<'p>> {
    accumulate_until_eof(
        move |(v, prev_tag)| (
            consume().peek().map_err(|x| x).and_then(move |f|
                field(code_page, mode, record_tag, prev_tag, omwsave).map_err(|x| RecordBodyError(x, f))
            ),
            (v, prev_tag),
        ),
        || (Vec::new(), META),
        |(mut v, _), (tag, field)| {
            v.push((tag, field));
            (v, tag)
        }
    ).map(|(v, _)| v)
}

fn read_record_body(record_offset: u64, code_page: CodePage, mode: RecordReadMode,
                    record_tag: Tag, record_size: u32, record_flags: RecordFlags, omwsave: bool,
                    input: &[u8])
    -> Result<Record, RecordError> {
    
    let (record_body, remaining_record_bytes) =
        consume().peek().map_err(|x| x).and_then(|input|
            record_body(code_page, mode, record_tag, omwsave).map_err(
                move |e| match e {
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
            )
        ).parse(input)?
    ;
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

    pub fn read<Input: Read + ?Sized>(&mut self, code_page: CodePage, mode: RecordReadMode, omwsave: bool, offset: u64, input: &mut Input)
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
            offset, code_page, mode, record_tag, record_size, record_flags, omwsave,
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
    omwsave: bool,
    offset: u64,
    reader: RecordReader,
}

impl<'a, Input: Read + ?Sized> Records<'a, Input> {
    pub fn new(code_page: CodePage, mode: RecordReadMode, omwsave: bool, offset: u64, input: &'a mut Input) -> Self {
        Records {
            code_page,
            mode,
            input,
            omwsave,
            offset,
            reader: RecordReader::new()
        }
    }
}

impl<'a, Input: Read + ?Sized> Iterator for Records<'a, Input> {
    type Item = Result<Record, ReadRecordError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.reader.read(self.code_page, self.mode, self.omwsave, self.offset, self.input) {
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
            RecordReader::new().read(CodePage::English, RecordReadMode::Strict, false, 0x11, &mut (&input[..])).unwrap().unwrap();
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
        let result = RecordReader::new().read(CodePage::English, RecordReadMode::Strict, false, 0x11, &mut (&input[..]));
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
        let result = field(CodePage::English, RecordReadMode::Strict, DIAL, META, false).parse(&input);
        let error = result.err().unwrap();
        if let FieldError::FieldSizeMismatch(DELE, expected, actual) = error {
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
        let result = field(CodePage::English, RecordReadMode::Strict, DIAL, META, false).parse(&input);
        let error = result.err().unwrap();
        if let FieldError::FieldSizeMismatch(DELE, expected, actual) = error {
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
        let result = field(CodePage::English, RecordReadMode::Strict, DIAL, META, false).parse(&input);
        let error = result.err().unwrap();
        if let FieldError::FieldSizeMismatch(DELE, expected, actual) = error {
            assert_eq!(expected, 4);
            assert_eq!(actual, 2);
        } else {
            panic!()
        }
    }

    #[test]
    fn read_string_list_field() {
        let input: &'static [u8] = b"123\r\n\xC0\xC1t\r\n\xDA\xDFX\r\n";
        if
            let (Field::StringList(result), remaining_input)
                = field_body(
                    CodePage::Russian, RecordReadMode::Strict, INFO, META, BNAM, input.len() as u32, false
                ).parse(input).unwrap()
        {
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
        field_body(CodePage::English, RecordReadMode::Strict, TES3, META, HEDR, input.len() as u32, false).parse(&input).err().unwrap();
    }

    #[test]
    fn read_from_vec_if_let() {
        let input: Vec<u8> = Vec::new();
        let res = field_body(CodePage::English, RecordReadMode::Strict, TES3, META, HEDR, input.len() as u32, false).parse(&input);
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
        let result = field_body(
            CodePage::English, RecordReadMode::Strict, TES3, META, HEDR, input.len() as u32, false
        ).parse(&input);
        if let (Field::FileMetadata(result), remaining_input) = result.unwrap() {
            assert_eq!(remaining_input.len(), 0);
            assert_eq!(result.file_type, FileType::ESS);
            assert_eq!(result.author.right().unwrap(), "author");
            assert_eq!(result.description.right().unwrap(), &["description", "lines", ""]);
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
        let result = field_body(CodePage::English, RecordReadMode::Strict, TES3, META, HEDR, input.len() as u32, false).parse(&input);
        let error = result.err().unwrap();
        if let FieldBodyError::UnknownValue(Unknown::FileType(val), offset) = error {
            assert_eq!(val, 0x100000);
            assert_eq!(offset, 4);
        } else {
            panic!()
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
        let bin: Vec<u8> = code::serialize(&ingredient, false).unwrap();
        let res = ingredient_field().parse(&bin).unwrap().0;
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
        let bin: Vec<u8> = code::serialize(
            &ValueWithSeed(&script_metadata, ScriptMetadataSerde { code_page: Some(CodePage::English) }), false
        ).unwrap();
        let res = script_metadata_field(CodePage::English).parse(&bin).unwrap().0;
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
            author: Right("Some author".into()),
            description: Right(vec!["descr line1".into(), "descr line2".into()]),
            records: 1333
        };
        let bin: Vec<u8> = code::serialize(
            &ValueWithSeed(&file_metadata, FileMetadataSerde { code_page: Some(CodePage::English) }), true
        ).unwrap();
        let res = file_metadata_300_field(CodePage::English).parse(&bin).unwrap().0;
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
        let bin: Vec<u8> = code::serialize(&effect, false).unwrap();
        let res = effect_field().parse(&bin).unwrap().0;
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
        let bin: Vec<u8> = code::serialize(&effect, false).unwrap();
        let res = effect_field().parse(&bin).unwrap().0;
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
        let bin: Vec<u8> = code::serialize(&npc_state, false).unwrap();
        let res = npc_state_field().parse(&bin).unwrap().0;
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
        let bin: Vec<u8> = code::serialize(&stats, false).unwrap();
        let res = npc_stats().parse(&bin).unwrap().0;
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
        let bin: Vec<u8> = code::serialize(&npc, true).unwrap();
        let res = npc_52_field().parse(&bin).unwrap().0;
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
        let bin: Vec<u8> = code::serialize(&npc, true).unwrap();
        let res = npc_12_field().parse(&bin).unwrap().0;
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
        let bin: Vec<u8> = code::serialize(&spell, false).unwrap();
        let res = spell_field().parse(&bin).unwrap().0;
        assert_eq!(res, spell);
    }

    #[test]
    fn serialize_item() {
        let item = Item {
            count: -3,
            item_id: "b_item_01 ".into()
        };
        let bin: Vec<u8> = code::serialize(&ValueWithSeed(&item, ItemSerde { code_page: Some(CodePage::English) }), false)
            .unwrap();
        let res = item_field(CodePage::English).parse(&bin).unwrap().0;
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
        let bin: Vec<u8> = code::serialize(&ValueWithSeed(&record, RecordSerde { code_page: Some(CodePage::English), omwsave: false }), true)
            .unwrap();
        println!("{:?}", bin);
        let mut bin = &bin[..];
        let records = Records::new(CodePage::English, RecordReadMode::Strict, false, 0, &mut bin);
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
        let records = Records::new(CodePage::English, RecordReadMode::Lenient, false, 0, &mut input);
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
