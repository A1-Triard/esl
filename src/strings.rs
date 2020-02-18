use std::fmt::{self, Debug};
use std::iter::{self};
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use crate::serde::ser::Error as ser_Error;
use crate::serde::de::{self};

#[derive(Clone, Debug, PartialOrd, PartialEq, Ord, Eq, Hash)]
pub struct StringZ {
    pub string: String,
    pub has_tail_zero: bool
}

impl Default for StringZ {
    fn default() -> StringZ { StringZ { string: String::default(), has_tail_zero: true } }
}

impl<T: Into<String>> From<T> for StringZ {
    fn from(t: T) -> StringZ { StringZ { string: t.into(), has_tail_zero: true } }
}

impl Serialize for StringZ {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            let mut carets = self.string.len() - self.string.rfind(|x| x != '^').map_or(0, |i| 1 + i);
            if !self.has_tail_zero {
                carets += 1;
            }
            let mut s = String::with_capacity(self.string.len() + carets);
            s.push_str(&self.string);
            s.extend(iter::repeat('^').take(carets));
            serializer.serialize_str(&s)
        } else {
            if !self.has_tail_zero && self.string.as_bytes().last().map_or(false, |&x| x == 0) {
                return Err(S::Error::custom("zero-terminated string value has tail zero"));
            }
            let mut s = String::with_capacity(self.string.len() + if self.has_tail_zero { 1 } else { 0 });
            s.push_str(&self.string);
            if self.has_tail_zero {
                s.push('\0');
            }
            serializer.serialize_str(&s)
        }
    }
}

struct StringZDeserializer {
    is_human_readable: bool
}

impl<'de> de::Visitor<'de> for StringZDeserializer {
    type Value = StringZ;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "string")
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
        self.visit_string(v.into())
    }
    
    fn visit_string<E: de::Error>(self, mut string: String) -> Result<Self::Value, E> {
        if self.is_human_readable {
            let carets = string.len() - string.rfind(|x| x != '^').map_or(0, |i| 1 + i);
            let has_tail_zero = carets % 2 == 1;
            let carets = (carets + 1) / 2;
            string.truncate(string.len() - carets);
            Ok(StringZ { string, has_tail_zero })
        } else {
            let has_tail_zero = string.as_bytes().last().map_or(false, |&x| x == 0);
            if has_tail_zero {
                string.truncate(string.len() - 1);
            }
            Ok(StringZ { string, has_tail_zero })
        }
    }
}

impl<'de> Deserialize<'de> for StringZ {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
        let is_human_readable = deserializer.is_human_readable();
        deserializer.deserialize_string(StringZDeserializer { is_human_readable })
    }
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
            assert_eq!(z.string, "Y");
        } else {
            panic!()
        }
        let z = Field::StringZ("Y".into());
        if let Field::StringZ(z) = z {
            assert_eq!(z.string, "Y");
        } else {
            panic!()
        }
    }
}
