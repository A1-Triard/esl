use std::fmt::{Debug};
use encoding::types::Encoding;
use encoding::all::{WINDOWS_1251, WINDOWS_1252};

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum LinebreakStyle {
    Unix,
    Dos
}

impl LinebreakStyle {
    pub fn new_line(self) -> &'static str {
        if self == LinebreakStyle::Unix { "\n" } else { "\r\n" }
    }
}

#[derive(Clone, Debug, PartialOrd, PartialEq, Ord, Eq, Hash)]
pub struct StringZ {
    pub str: String,
    pub has_tail_zero: bool
}

impl Default for StringZ {
    fn default() -> StringZ { StringZ { str: String::default(), has_tail_zero: true } }
}

impl<T: Into<String>> From<T> for StringZ {
    fn from(t: T) -> StringZ { StringZ { str: t.into(), has_tail_zero: true } }
}

#[derive(Clone, Debug, PartialOrd, PartialEq, Ord, Eq, Hash)]
pub struct StringZList {
    pub vec: Vec<String>,
    pub has_tail_zero: bool
}

impl Default for StringZList {
    fn default() -> StringZList { StringZList { vec: Vec::default(), has_tail_zero: true } }
}

impl<T: Into<Vec<String>>> From<T> for StringZList {
    fn from(t: T) -> StringZList { StringZList { vec: t.into(), has_tail_zero: true } }
}

macro_attr! {
    #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[derive(IterVariants!(CodePageVariants))]
    pub enum CodePage {
        English,
        Russian,
    }
}

impl CodePage {
    pub fn encoding(self) -> &'static dyn Encoding {
        match self {
            CodePage::English => WINDOWS_1252,
            CodePage::Russian => WINDOWS_1251,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use encoding::{DecoderTrap, EncoderTrap};

    #[test]
    fn string_into_string_z() {
        let z = Field::StringZ(String::from("Y").into());
        if let Field::StringZ(z) = z {
            assert_eq!(z.str, "Y");
        } else {
            panic!()
        }
        let z = Field::StringZ("Y".into());
        if let Field::StringZ(z) = z {
            assert_eq!(z.str, "Y");
        } else {
            panic!()
        }
    }

    #[test]
    fn all_code_pages_are_single_byte_encodings() {
        for code_page in CodePage::iter_variants() {
            let encoding = code_page.encoding();
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
