use std::char::{self};
use std::fmt::{self, Debug, Display};
//use std::str::{FromStr};

include!(concat!(env!("OUT_DIR"), "/Mark.rs"));

pub use Mark::*;

impl Debug for Mark {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Mark {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = *self as u32;
        let c0 = char::from(value as u8);
        let c1 = char::from((value >> 8) as u8);
        let c2 = char::from((value >> 16) as u8);
        let c3 = char::from((value >> 24) as u8);
        write!(f, "{}{}{}{}", c3, c2, c1, c0)
    }
}

/*
impl FromStr for Mark {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        
    }
}
*/

#[cfg(test)]
mod tests {
    use crate::*;
    
    #[test]
    fn debug_and_display_mark() {
        assert_eq!("TES3", format!("{}", TES3));
        assert_eq!("TES3", format!("{:?}", TES3));
    }
}
