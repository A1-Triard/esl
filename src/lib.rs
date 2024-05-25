#![feature(const_trait_impl)]
#![feature(effects)]
#![feature(iterator_try_collect)]
#![feature(lazy_cell)]
#![feature(never_type)]
#![feature(stmt_expr_attributes)]
#![feature(type_alias_impl_trait)]

#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::let_unit_value)]
#![allow(clippy::non_canonical_partial_ord_impl)]
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::match_ref_pats)]
#![allow(clippy::transmute_float_to_int)]
#![allow(clippy::transmute_int_to_float)]
#![allow(clippy::needless_doctest_main)]

#![recursion_limit="512"]

#[doc=include_str!("../README.md")]
type _DocTestReadme = ();

#[macro_use]
mod tag;

pub use crate::tag::*;

#[macro_use]
mod bitflags_ext;

mod code_page;
pub use code_page::*;

pub mod script_data;

mod field;

pub use crate::field::*;

mod record;

pub use crate::record::*;

pub mod read;

mod strings;

pub use crate::strings::*;

pub mod code;

mod serde_helpers;

#[cfg(test)]
mod tests {
    use crate::*;
    use crate::code::{self};
    use crate::read::*;
    use byteorder::{WriteBytesExt, LittleEndian};
    use either::Right;
    use iter_identify_first_last::IteratorIdentifyFirstLastExt;
    use quickcheck_macros::quickcheck;
    use serde::de::DeserializeSeed;
    use serde_serialize_seed::{ValueWithSeed, VecSerde};
    use std::iter::Iterator;
    use std::str::FromStr;
    use std::mem::transmute;

    #[quickcheck]
    fn tag_from_str_is_display_inversion(dword: u32) -> bool {
        Tag::from_str(&Tag { dword }.to_string()) == Ok(Tag { dword })
    }

    fn test_author() -> &'static [u8] {
        b"test author\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    }
    
    fn test_description() -> Vec<u8> {
        let mut v = Vec::new();
        v.extend_from_slice(b"test description");
        v.extend_from_slice(b"\r\nAAA\r\n\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v.extend_from_slice(b"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0");
        v
    }
    
    fn test_file_1_bytes() -> Vec<u8> {
        let mut v = Vec::new();
        v.extend_from_slice(b"TES3");
        v.write_u32::<LittleEndian>(346).unwrap();
        v.write_u64::<LittleEndian>(0).unwrap();
        v.extend_from_slice(b"HEDR");
        v.write_u32::<LittleEndian>(300).unwrap();
        v.write_u32::<LittleEndian>(7).unwrap();
        v.write_u32::<LittleEndian>(32).unwrap();
        v.extend_from_slice(test_author());
        v.extend_from_slice(&test_description());
        v.write_u32::<LittleEndian>(1).unwrap();
        v.extend_from_slice(b"MAST");
        v.write_u32::<LittleEndian>(14).unwrap();
        v.extend_from_slice(b"Mo__o_in_.esm\0");
        v.extend_from_slice(b"DATA");
        v.write_u32::<LittleEndian>(8).unwrap();
        v.write_u64::<LittleEndian>(137).unwrap();
        v.extend_from_slice(b"CLOH");
        v.write_u32::<LittleEndian>(29).unwrap();
        v.write_u64::<LittleEndian>(0).unwrap();
        v.extend_from_slice(b"NAMF");
        v.write_u32::<LittleEndian>(8).unwrap();
        v.extend_from_slice(b"namename");
        v.extend_from_slice(b"IDID");
        v.write_u32::<LittleEndian>(5).unwrap();
        v.extend_from_slice(b"idid\0");
        v
    }

    fn test_file1() -> Vec<Record> {
        vec![
            Record {
                tag: TES3,
                flags: RecordFlags::empty(),
                fields: vec![
                    (HEDR, Field::FileMetadata(FileMetadata {
                        version: 7,
                        file_type: FileType::ESS,
                        author: Right("test author".into()),
                        description: Right(vec!["test description".into(), "AAA".into(), "".into()]),
                        records: 1
                    })),
                    (MAST, Field::StringZ("Mo__o_in_.esm".into())),
                    (DATA, Field::I64(137))
                ]
            },
            Record {
                tag: Tag::from_str("CLOH").unwrap(),
                flags: RecordFlags::empty(),
                fields: vec![
                    (Tag::from_str("NAMF").unwrap(), Field::U8List(b"namename".iter().copied().collect())),
                    (Tag::from_str("IDID").unwrap(), Field::U8List(b"idid\0".iter().copied().collect())),
                ]
            }
        ]
    }

    fn serialize_file(file: &Vec<Record>, isolated: bool) -> Result<Vec<u8>, code::ser::Error> {
        let mut res = Vec::new();
        for (is_last, record) in file.iter().identify_last() {
            code::serialize_into_vec(
                &ValueWithSeed(record, RecordSerde { code_page: Some(CodePage::Russian) }), &mut res, isolated && is_last
            )?;
        }
        Ok(res)
    }
    
    #[test]
    fn read_file_1() {
        let mut records = &test_file_1_bytes()[..];
        let records = Records::new(CodePage::English, RecordReadMode::Strict, 0, &mut records);
        let records = records.map(|x| x.unwrap()).collect::<Vec<_>>();
        assert_eq!(records, test_file1());
    }

    #[test]
    fn serialize_file_1() {
        let bytes = serialize_file(&test_file1(), true).unwrap();
        assert_eq!(bytes, test_file_1_bytes());
    }

    fn deserialize_record(bytes: &mut &[u8], isolated: bool) -> Result<Record, code::de::Error> {
        code::deserialize_from_slice_seed(RecordSerde { code_page: Some(CodePage::Russian) }, bytes, isolated)
    }

    #[test]
    fn deserialize_file_1() {
        let mut records = &test_file_1_bytes()[..];
        let record1 = deserialize_record(&mut records, false).unwrap();
        let record2 = deserialize_record(&mut records, false).unwrap();
        assert!(records.is_empty());
        assert_eq!(vec![record1, record2], test_file1());
    }
  
    fn test_file_2_bytes() -> Vec<u8> {
        let mut v = Vec::new();
        v.extend_from_slice(b"TES3");
        v.write_u32::<LittleEndian>(346).unwrap();
        v.write_u64::<LittleEndian>(0).unwrap();
        v.extend_from_slice(b"HEDR");
        v.write_u32::<LittleEndian>(300).unwrap();
        v.write_u32::<LittleEndian>(7).unwrap();
        v.write_u32::<LittleEndian>(32).unwrap();
        v.extend_from_slice(test_author());
        v.extend_from_slice(&vec![0; 256]);
        v.write_u32::<LittleEndian>(1).unwrap();
        v.extend_from_slice(b"MAST");
        v.write_u32::<LittleEndian>(14).unwrap();
        v.extend_from_slice(b"Mo__o_in_.esm\0");
        v.extend_from_slice(b"DATA");
        v.write_u32::<LittleEndian>(8).unwrap();
        v.write_u64::<LittleEndian>(137).unwrap();
        v.extend_from_slice(b"CLOH");
        v.write_u32::<LittleEndian>(29).unwrap();
        v.write_u64::<LittleEndian>(0).unwrap();
        v.extend_from_slice(b"NAMF");
        v.write_u32::<LittleEndian>(8).unwrap();
        v.extend_from_slice(b"namename");
        v.extend_from_slice(b"IDID");
        v.write_u32::<LittleEndian>(5).unwrap();
        v.extend_from_slice(b"idid\0");
        v
    }

    fn test_file2() -> Vec<Record> {
        vec![
            Record {
                tag: TES3,
                flags: RecordFlags::empty(),
                fields: vec![
                    (HEDR, Field::FileMetadata(FileMetadata {
                        version: 7,
                        file_type: FileType::ESS,
                        author: Right("test author".into()),
                        description: Right(vec!["".to_string()]),
                        records: 1
                    })),
                    (MAST, Field::StringZ("Mo__o_in_.esm".into())),
                    (DATA, Field::I64(137))
                ]
            },
            Record {
                tag: Tag::from_str("CLOH").unwrap(),
                flags: RecordFlags::empty(),
                fields: vec![
                    (Tag::from_str("NAMF").unwrap(), Field::U8List(b"namename".iter().copied().collect())),
                    (Tag::from_str("IDID").unwrap(), Field::U8List(b"idid\0".iter().copied().collect())),
                ]
            }
        ]
    }
    
    #[test]
    fn read_file_2() {
        let mut records = &test_file_2_bytes()[..];
        let records = Records::new(CodePage::English, RecordReadMode::Strict, 0, &mut records);
        let records = records.map(|x| x.unwrap()).collect::<Vec<_>>();
        assert_eq!(records, test_file2());
    }

    #[test]
    fn serialize_file_2() {
        let bytes = serialize_file(&test_file2(), true).unwrap();
        assert_eq!(bytes, test_file_2_bytes());
    }

    #[test]
    fn deserialize_file_2() {
        let mut records = &test_file_2_bytes()[..];
        let record1 = deserialize_record(&mut records, false).unwrap();
        let record2 = deserialize_record(&mut records, false).unwrap();
        assert!(records.is_empty());
        assert_eq!(vec![record1, record2], test_file2());
    }

    fn test_file_without_description() -> Vec<Record> {
        vec![
            Record {
                tag: TES3,
                flags: RecordFlags::empty(),
                fields: vec![
                    (HEDR, Field::FileMetadata(FileMetadata {
                        version: 7,
                        file_type: FileType::ESS,
                        author: Right("test author".into()),
                        description: Right(Vec::new()),
                        records: 1
                    })),
                    (MAST, Field::StringZ("Mo__o_in_.esm".into())),
                    (DATA, Field::I64(137))
                ]
            },
            Record {
                tag: Tag::from_str("CLOH").unwrap(),
                flags: RecordFlags::empty(),
                fields: vec![
                    (Tag::from_str("NAMF").unwrap(), Field::U8List(b"namename".iter().copied().collect())),
                    (Tag::from_str("IDID").unwrap(), Field::U8List(b"idid\0".iter().copied().collect())),
                ]
            }
        ]
    }

    #[test]
    fn serialize_file_without_description() {
        let err = serialize_file(&test_file_without_description(), true).err().unwrap();
        let msg = match err {
            code::ser::Error::Custom(s) => s,
            _ => panic!("unexpected error '{}'", err)
        };
        assert_eq!(
            msg,
            "empty string list cannot be serialized bacause it is indistinguishable from list with one empty string"
        );
    }

    #[test]
    fn race_fnam() {
        let mut bytes = &[
            0x52, 0x41, 0x43, 0x45, 0x21, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x4E, 0x41, 0x4D, 0x45, 0x09, 0x00, 0x00, 0x00, 0x52, 0x65, 0x64, 0x67, 0x75, 0x61, 0x72, 0x64,
            0x00, 0x46, 0x4E, 0x41, 0x4D, 0x08, 0x00, 0x00, 0x00, 0xD0, 0xE5, 0xE4, 0xE3, 0xE0, 0xF0, 0xE4,
            0x00
        ][..];
        let mut records = Records::new(CodePage::Russian, RecordReadMode::Strict, 0, &mut bytes);
        let record = records.next().unwrap().unwrap();
        assert_eq!(record.fields[1].1, Field::StringZ(StringZ::from("Редгард")));
        let yaml = serde_yaml::to_string(&ValueWithSeed(&record, RecordSerde { code_page: None })).unwrap();
        assert!(!yaml.contains('^'));
        assert!(!yaml.contains("\\u"));
    }
    
    #[test]
    fn cell() {
        let record = Record {
            tag: CELL,
            flags: RecordFlags::empty(),
            fields: vec![
                (NAME, Field::StringZ("".into())),
                (DATA, Field::Cell(Cell { flags:CellFlags::HAS_WATER, position: CellPosition::Exterior { x: 20, y: 4 } })),
                (RGNN, Field::StringZ("Coast Region".into())),
                (FRMR, Field::I32(347140)),
                (NAME, Field::StringZ("crab".into())),
                (DATA, Field::PosRot(PosRot {
                    pos: Pos { x: 165898.0, y: 38710.484, z: 198.8868, },
                    rot: Rot { x: 0.0, y: 0.0, z: 0.0 }
                }))
            ]
        };
        let bytes = code::serialize(&ValueWithSeed(&record, RecordSerde { code_page: Some(CodePage::English) }), false).unwrap();
        let read = {
            let mut bytes = &bytes[..];
            let mut records = Records::new(CodePage::Russian, RecordReadMode::Strict, 0, &mut bytes);
            let read = records.next().unwrap().unwrap();
            assert!(records.next().is_none());
            read
        };
        assert_eq!(record, read);
        let deserialized: Record = code::deserialize_seed(RecordSerde { code_page: Some(CodePage::Russian) }, &bytes, false)
            .unwrap();
        assert_eq!(record, deserialized);
    }

    #[test]
    fn effect_metadata() {
        let record = Record {
            tag: MGEF,
            flags: RecordFlags::empty(),
            fields: vec![(MEDT, Field::EffectMetadata(EffectMetadata {
                school: School::Destruction,
                base_cost: 100.0,
                flags: EffectFlags::SPELLMAKING | EffectFlags::LIGHT_NEGATIVE,
                color: Color { r: 1, g: 2, b: 255 },
                size_factor: 1.0,
                size_cap: 50.0,
                speed: 1.0
            }))]
        };
        let bytes = code::serialize(&ValueWithSeed(&record, RecordSerde { code_page: Some(CodePage::English) }), false).unwrap();
        let read = {
            let mut bytes = &bytes[..];
            let mut records = Records::new(CodePage::Russian, RecordReadMode::Strict, 0, &mut bytes);
            let read = records.next().unwrap().unwrap();
            assert!(records.next().is_none());
            read
        };
        assert_eq!(record, read);
        let deserialized: Record = code::deserialize_seed(RecordSerde { code_page: Some(CodePage::Russian) }, &bytes, false)
            .unwrap();
        assert_eq!(record, deserialized);
    }

    #[test]
    fn journal_zeros() {
        let record = Record {
            tag: JOUR,
            flags: RecordFlags::empty(),
            fields: vec![(NAME, Field::StringList(vec![
                "\0\0\0".into(),
            ]))]
        };
        let bytes = code::serialize(&ValueWithSeed(&record, RecordSerde { code_page: Some(CodePage::English) }), false).unwrap();
        let read = {
            let mut bytes = &bytes[..];
            let mut records = Records::new(CodePage::English, RecordReadMode::Strict, 0, &mut bytes);
            let read = records.next().unwrap().unwrap();
            assert!(records.next().is_none());
            read
        };
        assert_eq!(record, read);
        let deserialized = code::deserialize_seed(RecordSerde { code_page: Some(CodePage::Russian) }, &bytes, false).unwrap();
        assert_eq!(record, deserialized);
    }

    #[allow(clippy::transmute_int_to_float)]
    #[test]
    fn deserialize_float_field() {
        let yaml = "\
- GMST:
  - FLTV: 3.0
  - FLTV: .nan
  - FLTV: nanFFEEEEEE
  - FLTV: 0.1
  - FLTV: -0.0
";
        let res: Vec<Record> = VecSerde(RecordSerde { code_page: None }).deserialize(serde_yaml::Deserializer::from_str(yaml)).unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(res[0].fields.len(), 5);
        assert_eq!(res[0].fields[0].1, Field::F32(3.0));
        let standard_nan: f32 = unsafe { transmute(0xFFFFFFFFu32) };
        let custom_nan: f32 = unsafe { transmute(0xFFEEEEEEu32) };
        assert!(standard_nan.is_nan());
        assert!(custom_nan.is_nan());
        assert_ne!(Field::F32(standard_nan), Field::F32(custom_nan));
        assert_eq!(res[0].fields[1].1, Field::F32(standard_nan));
        assert_eq!(res[0].fields[2].1, Field::F32(custom_nan));
        assert_eq!(res[0].fields[3].1, Field::F32(0.1));
        assert_eq!(res[0].fields[4].1, Field::F32(0.0_f32.copysign(-1.0)));
        let res_yaml = serde_yaml::to_string(&ValueWithSeed(&res[..], VecSerde(RecordSerde { code_page: None }))).unwrap();
        assert_eq!(res_yaml, yaml);
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn deserialize_light_field() {
        let yaml = "\
- LIGH:
  - LHDT:
      weight: 0.1
      value: 3
      time: 0
      radius: 128
      color: '#F58C28'
      flags: DYNAMIC CAN_CARRY FIRE FLICKER_SLOW
";
        let res: Vec<Record> = VecSerde(RecordSerde { code_page: None }).deserialize(serde_yaml::Deserializer::from_str(yaml)).unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(res[0].fields.len(), 1);
        if let Field::Light(field) = &res[0].fields[0].1 {
            assert_eq!(field.weight, 0.1);
        } else {
            panic!()
        }
        let res_yaml = serde_yaml::to_string(&ValueWithSeed(&res[..], VecSerde(RecordSerde { code_page: None }))).unwrap();
        assert_eq!(res_yaml, yaml);
    }
}
