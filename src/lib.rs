#![feature(ptr_offset_from)]
#![feature(type_alias_impl_trait)]
#![feature(stmt_expr_attributes)]
#![deny(warnings)]
#![recursion_limit="512"]

#[macro_use]
extern crate enum_derive;
#[macro_use]
extern crate enum_primitive_derive;
#[macro_use]
extern crate macro_attr;
#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate debug_panic;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate serde;
#[macro_use]
extern crate derivative;
#[macro_use]
extern crate nameof;

#[macro_use]
mod tag;

pub use crate::tag::*;

#[macro_use]
mod bitflags_ext;

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
    use crate::read::*;
    use crate::code::{self, CodePage};
    use byteorder::{WriteBytesExt, LittleEndian};
    use std::iter::Iterator;
    use std::str::FromStr;
    use std::mem::transmute;

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
                        author: "test author".into(),
                        description: vec!["test description".into(), "AAA".into(), "".into()],
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
                    (Tag::from_str("NAMF").unwrap(), Field::U8List(b"namename".iter().map(|&x| x).collect())),
                    (Tag::from_str("IDID").unwrap(), Field::U8List(b"idid\0".iter().map(|&x| x).collect())),
                ]
            }
        ]
    }
    
    #[test]
    fn read_file_1() {
        let mut records = &test_file_1_bytes()[..];
        let records = Records::new(CodePage::English, 0, &mut records);
        let records = records.map(|x| x.unwrap()).collect::<Vec<_>>();
        assert_eq!(records, test_file1());
    }

    #[test]
    fn serialize_file_1() {
        let bytes = code::serialize(&test_file1(), CodePage::Russian, true).unwrap();
        assert_eq!(bytes, test_file_1_bytes());
    }

    #[test]
    fn deserialize_file_1() {
        let mut records = &test_file_1_bytes()[..];
        let record1: Record = code::deserialize_from_slice(&mut records, CodePage::Russian, false).unwrap();
        let record2: Record = code::deserialize_from_slice(&mut records, CodePage::Russian, false).unwrap();
        assert!(records.is_empty());
        assert_eq!(vec![record1, record2], test_file1());
    }
  
    #[test]
    fn race_fnam() {
        let mut bytes = &[
            0x52, 0x41, 0x43, 0x45, 0x21, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x4E, 0x41, 0x4D, 0x45, 0x09, 0x00, 0x00, 0x00, 0x52, 0x65, 0x64, 0x67, 0x75, 0x61, 0x72, 0x64,
            0x00, 0x46, 0x4E, 0x41, 0x4D, 0x08, 0x00, 0x00, 0x00, 0xD0, 0xE5, 0xE4, 0xE3, 0xE0, 0xF0, 0xE4,
            0x00
        ][..];
        let mut records = Records::new(CodePage::Russian, 0, &mut bytes);
        let record = records.next().unwrap().unwrap();
        assert_eq!(record.fields[1].1, Field::StringZ(StringZ::from("Редгард")));
        let yaml = serde_yaml::to_string(&record).unwrap();
        assert!(!yaml.contains("^"));
        assert!(!yaml.contains("\\u"));
    }
    
    #[test]
    fn cell() {
        let record = Record {
            tag: CELL,
            flags: RecordFlags::empty(),
            fields: vec![
                (NAME, Field::StringZ("".into())),
                (DATA, Field::Cell(Cell { flags:CellFlags::HAS_WATER, grid: Grid { x: 20, y: 4} })),
                (RGNN, Field::StringZ("Coast Region".into())),
                (FRMR, Field::I32(347140)),
                (NAME, Field::StringZ("crab".into())),
                (DATA, Field::Position(Position { x: 165898.0, y: 38710.484375, z: 198.8867950439453, x_rot: 0.0, y_rot: 0.0, z_rot: 0.0 }))
            ]
        };
        let bytes = code::serialize(&record, CodePage::English, false).unwrap();
        let read = {
            let mut bytes = &bytes[..];
            let mut records = Records::new(CodePage::Russian, 0, &mut bytes);
            let read = records.next().unwrap().unwrap();
            assert!(records.next().is_none());
            read
        };
        assert_eq!(record, read);
        let deserialized: Record = code::deserialize(&bytes, CodePage::Russian, false).unwrap();
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
        let bytes = code::serialize(&record, CodePage::English, false).unwrap();
        let read = {
            let mut bytes = &bytes[..];
            let mut records = Records::new(CodePage::Russian, 0, &mut bytes);
            let read = records.next().unwrap().unwrap();
            assert!(records.next().is_none());
            read
        };
        assert_eq!(record, read);
        let deserialized: Record = code::deserialize(&bytes, CodePage::Russian, false).unwrap();
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
        let bytes = code::serialize(&record, CodePage::English, false).unwrap();
        let read = {
            let mut bytes = &bytes[..];
            let mut records = Records::new(CodePage::English, 0, &mut bytes);
            let read = records.next().unwrap().unwrap();
            assert!(records.next().is_none());
            read
        };
        assert_eq!(record, read);
        let deserialized: Record = code::deserialize(&bytes, CodePage::Russian, false).unwrap();
        assert_eq!(record, deserialized);
    }

    #[test]
    fn deseralize_float_field() {
        let yaml = "\
- GMST:
  - FLTV: 3.0
  - FLTV: .nan
  - FLTV: nanFFEEEEEE
        ";
        let res: Vec<Record> = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(res[0].fields.len(), 3);
        assert_eq!(res[0].fields[0].1, Field::F32(3.0));
        let standard_nan: f32 = unsafe { transmute(0xFFFFFFFFu32) };
        let custom_nan: f32 = unsafe { transmute(0xFFEEEEEEu32) };
        assert!(standard_nan.is_nan());
        assert!(custom_nan.is_nan());
        assert_ne!(Field::F32(standard_nan), Field::F32(custom_nan));
        assert_eq!(res[0].fields[1].1, Field::F32(standard_nan));
        assert_eq!(res[0].fields[2].1, Field::F32(custom_nan));
    }

    #[test]
    fn deserialize_light_field() {
        let yaml = "\
- LIGH:
    - LHDT:
        weight: 0.1
        value: 3
        time: 0
        radius: 128
        color: \"#F58C28\"
        flags: DYNAMIC CAN_CARRY FIRE FLICKER_SLOW
        ";
        let res: Vec<Record> = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(res[0].fields.len(), 1);
        if let Field::Light(field) = &res[0].fields[0].1 {
            assert_eq!(field.weight, 0.1);
        } else {
            panic!()
        }
        let res_yaml = serde_yaml::to_string(&res).unwrap();
        assert_eq!(res_yaml, format!("---\n{}", yaml).trim());
    }
}
