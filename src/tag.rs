use std::char::{self};
use std::fmt::{self, Debug, Display};
use std::str::{FromStr};

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
pub struct Tag {
    pub dword: u32
}

impl Tag {
    pub const fn from(dword: u32) -> Tag {
        Tag { dword }
    }
}

impl From<u32> for Tag {
    fn from(dword: u32) -> Tag {
        Tag { dword }
    }
}

impl Debug for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c0 = char::from(self.dword as u8);
        let c1 = char::from((self.dword >> 8) as u8);
        let c2 = char::from((self.dword >> 16) as u8);
        let c3 = char::from((self.dword >> 24) as u8);
        write!(f, "{}{}{}{}", c3, c2, c1, c0)
    }
}

impl FromStr for Tag {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut dword = 0;
        let mut i = 0;
        for byte in s.bytes() {
            if i == 4 { return Err(()); }
            dword = (dword << 8) | (byte as u32);
            i += 1;
        }
        if i != 4 { return Err(()); }
        Ok(Tag { dword })
    }
}
