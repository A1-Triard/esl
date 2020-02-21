#![feature(ptr_offset_from)]
#![feature(type_alias_impl_trait)]
#![deny(warnings)]

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
                        records_count: 1
                    })),
                    (MAST, Field::StringZ("Mo__o_in_.esm".into())),
                    (DATA, Field::Long(137))
                ]
            },
            Record {
                tag: Tag::from_str("CLOH").unwrap(),
                flags: RecordFlags::empty(),
                fields: vec![
                    (Tag::from_str("NAMF").unwrap(), Field::Binary(b"namename".iter().map(|&x| x).collect())),
                    (Tag::from_str("IDID").unwrap(), Field::Binary(b"idid\0".iter().map(|&x| x).collect())),
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
}