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
    pub enum Tes3 {
        ESP = 0,
        ESM = 1,
        ESS = 32
    }
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(u8)]
    pub enum DialogType {
        Topic = 0,
        Voice = 1,
        Greeting = 2,
        Persuasion = 3,
        Journal = 4
    }
}

macro_attr! {
    #[derive(Primitive)]
    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
    #[derive(Debug, EnumDisplay!, EnumFromStr!)]
    #[repr(i32)]
    pub enum EffectType {
        Oneself = 0,
        Touch = 1,
        Target = 2,
    }
}

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
        assert_eq!("ESM", format!("{}", Tes3::ESM));
        assert_eq!("ESS", format!("{:?}", Tes3::ESS));
        assert_eq!(Some(Tes3::ESP), Tes3::from_u32(0));
        assert_eq!(None, Tes3::from_u32(2));
        assert_eq!(32, Tes3::ESS as u32);
    }
}
