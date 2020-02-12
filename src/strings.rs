use std::fmt::{Debug};
use serde::{Serialize, Deserialize};

#[derive(Clone, Debug, PartialOrd, PartialEq, Ord, Eq, Hash)]
#[derive(Serialize,Deserialize)]
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
#[derive(Serialize,Deserialize)]
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

#[cfg(test)]
mod tests {
    use crate::*;

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
}
