#![deny(warnings)]

#[macro_use]
extern crate enum_derive;
#[macro_use]
extern crate enum_primitive_derive;
#[macro_use]
extern crate macro_attr;

mod tag;

pub use tag::*;

include!(concat!(env!("OUT_DIR"), "/tags.rs"));

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(u32)]
    pub enum FileType {
        ESP = 0,
        ESM = 1,
        ESS = 32
    }
}

pub use FileType::*;

#[cfg(test)]
mod tests {
    use crate::*;
    use num_traits::cast::FromPrimitive;

    #[test]
    fn debug_and_display_tag() {
        assert_eq!("TES3", format!("{}", TES3));
        assert_eq!("TES3", format!("{:?}", TES3));
    }

    #[test]
    fn test_file_type() {
        assert_eq!("ESM", format!("{}", ESM));
        assert_eq!("ESS", format!("{:?}", ESS));
        assert_eq!(Some(ESP), FileType::from_u32(0));
        assert_eq!(None, FileType::from_u32(2));
        assert_eq!(32, ESS as u32);
    }
}
