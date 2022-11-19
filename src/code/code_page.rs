use encoding::{Encoding};
use encoding::all::{WINDOWS_1251, WINDOWS_1252};
use macro_attr_2018::macro_attr;
use enum_derive_2018::IterVariants;

macro_attr! {
    #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[derive(IterVariants!(CodePageVariants))]
    pub enum CodePage {
        English,
        Russian,
        Unicode,
    }
}

impl CodePage {
    pub fn encoding(self) -> Option<&'static dyn Encoding> {
        match self {
            CodePage::English => Some(WINDOWS_1252),
            CodePage::Russian => Some(WINDOWS_1251),
            CodePage::Unicode => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::code::*;
    use encoding::{DecoderTrap, EncoderTrap};

    #[test]
    fn all_code_pages_are_single_byte_encodings() {
        for encoding in CodePage::iter_variants().filter_map(|x| x.encoding()) {
            for byte in 0u8 ..= 255 {
                let c = encoding.decode(&[byte], DecoderTrap::Strict).unwrap();
                let b = encoding.encode(&c, EncoderTrap::Strict).unwrap();
                assert_eq!(b.len(), 1);
                assert_eq!(b[0], byte);
            }
            for byte in 0u8 .. 128 {
                let c = encoding.decode(&[byte], DecoderTrap::Strict).unwrap();
                assert_eq!(c.len(), 1);
                assert_eq!(byte as u32, c.as_bytes()[0] as u32);
            }
        }
    }
}
