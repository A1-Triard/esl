use std::fmt::{self, Debug, Formatter};
use std::iter::{self};
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::ser::SerializeSeq;
use serde::ser::Error as ser_Error;
use serde::de::{self};

pub fn string_from_utf8_like(mut bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len()); // at least
    loop {
        let Some((&b, n)) = bytes.split_first() else { break; };
        bytes = n;
        let c = if b <= 0x7F { // valid one-byte UTF-8
            b.into() // 0x000000 ..= 0x00007F
        } else if b >= 0xC0 && b <= 0xDF { // two-byte UTF-8 lead byte
            if let Some((&t, n)) = bytes.split_first() {
                bytes = n;
                if t >= 0x80 && t <= 0xBF { // two-byte UTF-8 second byte
                    let c = (u32::from(b & 0x1F) << 6) | u32::from(t & 0x3F);
                    if c <= 0x7F {
                        0x100000u32 | c // 0x100000 ..= 0x10007F
                    } else { // valid two-byte UTF-8
                        c // 0x000080 ..= 0x0007FF
                    }
                } else {
                    0x100000u32 | (u32::from(b) << 8) | u32::from(t)
                    // 0x10C000 ..= 0x10DFFF,
                    // last byte in 0x00 ..= 0x7F or 0xC0 ..= 0xFF
                }
            } else { // last char
                0x100000u32 | u32::from(b) // 0x1000C0 ..= 0x1000DF
            }
        } else {
            0x100000u32 | u32::from(b) // 0x100080 ..= 0x1000BF U 0x1000E0 ..= 0x1000FF
        };
        s.push(char::from_u32(c).unwrap());
    }
    s
}

pub fn string_to_utf8_like(s: &str) -> Option<Vec<u8>> {
    let mut bytes = Vec::with_capacity(s.len());
    let mut stop = false;
    for c in s.chars().map(u32::from) {
        if stop { return None; }
        match c {
            0x000000 ..= 0x00007F =>
                bytes.push(c as u8),
            0x000080 ..= 0x0007FF => {
                let h = 0xC0 | ((c & 0x7C0) >> 6) as u8;
                let t = 0x80 | (c & 0x3F) as u8;
                bytes.push(h);
                bytes.push(t);
            },
            0x100000 ..= 0x10007F => {
                let h = 0xC0 | ((c & 0x7C0) >> 6) as u8;
                let t = 0x80 | (c & 0x3F) as u8;
                bytes.push(h);
                bytes.push(t);
            },
            0x1000C0 ..= 0x1000DF => {
                stop = true;
                bytes.push((c & 0xFF) as u8);
            },
            0x100080 ..= 0x1000FF =>
                bytes.push((c & 0xFF) as u8),
            0x10C000 ..= 0x10DFFF => {
                let h = ((c & 0xFF00) >> 8) as u8;
                let t = (c & 0xFF) as u8;
                if (0x80 ..= 0xBF).contains(&t) { return None; }
                bytes.push(h);
                bytes.push(t);
            },
            _ => return None,
        }
    }
    Some(bytes)
}

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
            let mut carets = self.string.len() - self.string.rfind(|x| x != '^')
                .map_or(0, |i| i + self.string[i..].chars().next().unwrap().len_utf8());
            if !self.has_tail_zero {
                carets += 1;
            }
            let mut s = String::with_capacity(self.string.len() + carets);
            s.push_str(&self.string);
            s.extend(iter::repeat('^').take(carets));
            serializer.serialize_str(&s)
        } else {
            if !self.has_tail_zero && self.string.as_bytes().last() == Some(&0) {
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

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "string")
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
        self.visit_string(v.into())
    }
    
    fn visit_string<E: de::Error>(self, mut string: String) -> Result<Self::Value, E> {
        if self.is_human_readable {
            let carets = string.len() - string.rfind(|x| x != '^').map_or(0, |i| i + string[i..].chars().next().unwrap().len_utf8());
            let has_tail_zero = carets % 2 == 0;
            let carets = (carets + 1) / 2;
            string.truncate(string.len() - carets);
            Ok(StringZ { string, has_tail_zero })
        } else {
            let has_tail_zero = string.as_bytes().last() == Some(&0);
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

impl Serialize for StringZList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        if serializer.is_human_readable() {
            let mut carets = self.vec.len() - self.vec.iter().rposition(|x| x != "^").map_or(0, |i| i + 1);
            if !self.has_tail_zero {
                carets += 1;
            }
            let mut serializer = serializer.serialize_seq(Some(self.vec.len() + carets))?;
            for s in &self.vec {
                serializer.serialize_element(s)?;
            }
            for _ in 0..carets {
                serializer.serialize_element("^")?;
            }
            serializer.end()
        } else {
            let mut capacity = 0;
            for s in &self.vec {
                if s.contains('\0') {
                    return Err(S::Error::custom("zero-terminated string list item contains zero byte"));
                }
                capacity += s.len() + 1;
            }
            let mut text = String::with_capacity(capacity);
            for s in &self.vec {
                text.push_str(s);
                text.push('\0');
            }
            if !self.has_tail_zero {
                text.truncate(text.len() - 1);
            }
            serializer.serialize_str(&text)
        }
    }
}

struct StringZListHRDeserializer;

impl<'de> de::Visitor<'de> for StringZListHRDeserializer {
    type Value = StringZList;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "string sequence")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        let mut vec: Vec<String> = seq.size_hint().map_or_else(Vec::new, Vec::with_capacity);
        while let Some(line) = seq.next_element()? {
            vec.push(line);
        }
        let carets = vec.len() - vec.iter().rposition(|x| x != "^").map_or(0, |i| i + 1);
        let has_tail_zero = carets % 2 == 0;
        let carets = (carets + 1) / 2;
        vec.truncate(vec.len() - carets);
        Ok(StringZList { vec, has_tail_zero })
    }
}

struct StringZListNHRDeserializer;

impl<'de> de::Visitor<'de> for StringZListNHRDeserializer {
    type Value = StringZList;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "string")
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
        let has_tail_zero = v.as_bytes().last() == Some(&0);
        let v = if has_tail_zero { &v[.. v.len() - 1] } else { v };
        Ok(StringZList { vec: v.split('\0').map(|x| x.into()).collect(), has_tail_zero })
    }
}

impl<'de> Deserialize<'de> for StringZList {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
        if deserializer.is_human_readable() {
            deserializer.deserialize_seq(StringZListHRDeserializer)
        } else {
            deserializer.deserialize_str(StringZListNHRDeserializer)
        }
    }
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
