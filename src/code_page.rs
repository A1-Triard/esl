use encoding::{DecoderTrap, EncoderTrap, Encoding};
use encoding::all::{WINDOWS_1251, WINDOWS_1252};
use enum_derive_2018::IterVariants;
use macro_attr_2018::macro_attr;
use std::fmt::Write as fmt_Write;
use utf8_chars::BufReadCharsExt;

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
    fn encoding(self) -> Option<&'static dyn Encoding> {
        match self {
            CodePage::English => Some(WINDOWS_1252),
            CodePage::Russian => Some(WINDOWS_1251),
            CodePage::Unicode => None,
        }
    }

    pub fn decode(self, mut bytes: &[u8]) -> String {
        if let Some(encoding) = self.encoding() {
            let s = encoding.decode(bytes, DecoderTrap::Strict).unwrap();
            s.replace('\u{25ca}', "\u{25ca}\u{25ca}")
        } else {
            let mut s = String::with_capacity(bytes.len()); // at least
            for c in (&mut bytes).chars_raw() {
                match c {
                    Ok('\u{25ca}') => s.push_str("\u{25ca}\u{25ca}"),
                    Ok(c) => s.push(c),
                    Err(e) => e.as_bytes().iter().for_each(|&b| write!(s, "\u{25ca}{b:02X}").unwrap()),
                }
            }
            s
        }
    }

    pub fn encode(self, s: &str) -> Result<Vec<u8>, Option<char>> {
        let bytes = if let Some(encoding) = self.encoding() {
            let s = s.replace("\u{25ca}\u{25ca}", "\u{25ca}");
            encoding
                .encode(&s, EncoderTrap::Strict)
                .map_err(|e| Some(e.chars().next().unwrap()))?
        } else {
            let mut bytes = Vec::with_capacity(s.len());
            let mut chars = s.char_indices();
            while let Some((i, c)) = chars.next() {
                if c == '\u{25ca}' {
                    let (j, d) = chars.next().ok_or(None)?;
                    if d == '\u{25ca}' {
                        bytes.push(0xE2);
                        bytes.push(0x97);
                        bytes.push(0x8A);
                    } else {
                        let (k, e) = chars.next().ok_or(None)?;
                        bytes.push(
                            u8::from_str_radix(&s[j .. k + e.len_utf8()], 16)
                                .map_err(|_| None)?
                        );
                    }
                } else {
                    bytes.extend_from_slice(s[i .. i + c.len_utf8()].as_bytes());
                }
            }
            bytes
        };
        if self.decode(&bytes) != s { return Err(None); }
        Ok(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use encoding::{DecoderTrap, EncoderTrap};
    use quickcheck::TestResult;
    use quickcheck_macros::quickcheck;

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

    #[quickcheck]
    fn decode_encode(b: Vec<u8>) -> bool {
        CodePage::iter_variants().all(|x| matches!(x.encode(&x.decode(&b)), Ok(x) if x == b))
    }

    #[quickcheck]
    fn encode_decode_unicode(s: String) -> TestResult {
        let Ok(b) = CodePage::Unicode.encode(&s) else { return TestResult::discard(); };
        TestResult::from_bool(CodePage::Unicode.decode(&b) == s)
    }

    #[test]
    fn unicode_can_represent_all_code_pages() {
        for encoding in CodePage::iter_variants().filter_map(|x| x.encoding()) {
            for byte in 0u8 ..= 255 {
                let c = encoding.decode(&[byte], DecoderTrap::Strict).unwrap();
                assert!(CodePage::Unicode.encode(&c).is_ok());
            }
        }
    }
}
