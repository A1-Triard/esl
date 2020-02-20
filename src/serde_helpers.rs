use std::fmt::{self};
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::de::{self};
use serde::de::Error as de_Error;
use std::marker::PhantomData;
use serde::ser::Error as ser_Error;
use serde::ser::{SerializeTuple, SerializeSeq};

pub fn serialize_string_tuple<S>(s: &str, len: usize, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if serializer.is_human_readable() {
        serializer.serialize_str(s)
    } else {
        if s.as_bytes().last().map_or(false, |&x| x == 0) {
            return Err(S::Error::custom("string tuple value has tail zero"));
        }
        let mut serializer = serializer.serialize_tuple(len)?;
        let mut s_len = 0;
        for c in s.chars() {
            serializer.serialize_element(&c)?;
            s_len += 1;
        }
        if s_len > len {
            return Err(S::Error::custom(&format!("string length is above {} chars", len)));
        }
        for _ in s_len .. len {
            serializer.serialize_element(&'\0')?;
        }
        serializer.end()
    }
}

struct StringNHRDeserializer { len: usize }

impl<'de> de::Visitor<'de> for StringNHRDeserializer {
    type Value = String;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} character string", self.len)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        if let Some(n) = seq.size_hint() {
            if n != self.len {
                return Err(A::Error::invalid_length(n, &self));
            }
        }
        let mut string: String = String::with_capacity(self.len);
        let mut string_len = 0;
        while let Some(c) = seq.next_element()? {
            string.push(c);
            string_len += 1;
        }
        if string_len != self.len {
            Err(A::Error::invalid_length(string_len, &self))
        } else {
            let cut_to = string.rfind(|c| c != '\0').map_or(0, |n| 1 + n);
            string.truncate(cut_to);
            Ok(string)
        }
    }
}

pub fn deserialize_string_tuple<'de, D>(len: usize, deserializer: D) -> Result<String, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        String::deserialize(deserializer)
    } else {
        deserializer.deserialize_tuple(len, StringNHRDeserializer { len })
    }
}

pub fn serialize_string_list<S>(lines: &[String], separator: &str, len: Option<usize>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    if serializer.is_human_readable() {
        lines.serialize(serializer)
    } else {
        let mut capacity = 0;
        for line in lines {
            if line.contains(separator) {
                return Err(S::Error::custom("string list item contains separator"));
            }
            capacity += line.len() + separator.len();
        }
        let mut text = String::with_capacity(capacity);
        for line in lines.iter() {
            text.push_str(line);
            text.push_str(separator);
        }
        text.truncate(text.len() - separator.len());
        if text.as_bytes().last().map_or(false, |&x| x == 0) {
            return Err(S::Error::custom("string list has tail zero"));
        }
        if let Some(len) = len {
            let mut serializer = serializer.serialize_tuple(len)?;
            let mut text_len = 0;
            for c in text.chars() {
                serializer.serialize_element(&c)?;
                text_len += 1;
            }
            if text_len > len {
                return Err(S::Error::custom(&format!("string list total length is above {} chars", len)));
            }
            for _ in text_len .. len {
                serializer.serialize_element(&'\0')?;
            }
            serializer.end()
        } else {
            let text_len = text.chars().count();
            let mut serializer = serializer.serialize_seq(Some(text_len))?;
            for c in text.chars() {
                serializer.serialize_element(&c)?;
            }
            serializer.end()
        }
    }
}

struct StringListNHRDeserializer<'a> { len: Option<usize>, separator: &'a str }

impl<'a, 'de> de::Visitor<'de> for StringListNHRDeserializer<'a> {
    type Value = Vec<String>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(len) = self.len {
            write!(f, "{} character string", len)
        } else {
            write!(f, "string")
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: de::SeqAccess<'de> {
        let mut size_hint = seq.size_hint();
        if let Some(len) = self.len {
            if let Some(n) = size_hint {
                if n != len {
                    return Err(A::Error::invalid_length(n, &self));
                }
            } else {
                size_hint = Some(len);
            }
        }
        let mut string: String = size_hint.map_or_else(String::new, String::with_capacity);
        let mut string_len = 0;
        while let Some(c) = seq.next_element()? {
            string.push(c);
            string_len += 1;
        }
        if let Some(len) = self.len {
            if string_len != len {
                return Err(A::Error::invalid_length(string_len, &self));
            } else {
                let cut_to = string.rfind(|c| c != '\0').map_or(0, |n| 1 + n);
                string.truncate(cut_to);
            }
        }
        Ok(string.split(self.separator).map(|x| x.into()).collect())
    }
}

pub fn deserialize_string_list<'de, D>(separator: &str, len: Option<usize>, deserializer: D) -> Result<Vec<String>, D::Error> where D: Deserializer<'de> {
    if deserializer.is_human_readable() {
        <Vec<String>>::deserialize(deserializer)
    } else {
        if let Some(len) = len {
            deserializer.deserialize_tuple(len, StringListNHRDeserializer { len: Some(len), separator })
        } else {
            deserializer.deserialize_seq(StringListNHRDeserializer { len: None, separator })
        }
    }
}

pub struct StructSeqProxyDeserializer<'de, A: de::SeqAccess<'de>> {
    seq: A,
    phantom: PhantomData<&'de ()>
}

impl<'de, A: de::SeqAccess<'de>> StructSeqProxyDeserializer<'de, A> {
    pub fn new(seq: A) -> Self { StructSeqProxyDeserializer { seq, phantom: PhantomData } }
}

impl<'de, A: de::SeqAccess<'de>> Deserializer<'de> for StructSeqProxyDeserializer<'de, A> {
    type Error = A::Error;

    fn deserialize_struct<V>(self, _: &'static str, _: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> {
        visitor.visit_seq(self.seq)
    }

    fn deserialize_seq<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_bool<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i8<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i16<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_f32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_f64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u8<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u16<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    serde_if_integer128! {
        fn deserialize_i128<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

        fn deserialize_u128<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }
    }

    fn deserialize_char<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_str<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_string<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_bytes<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_byte_buf<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_option<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_unit<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_unit_struct<V>(self, _: &'static str, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_newtype_struct<V>(self, _: &'static str, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_map<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_tuple<V>(self, _: usize, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_tuple_struct<V>(self, _: &'static str, _: usize, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_enum<V>(self, _: &'static str, _: &'static [&'static str], _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_identifier<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_ignored_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }
}

pub struct StructMapProxyDeserializer<'de, A: de::MapAccess<'de>> {
    map: A,
    phantom: PhantomData<&'de ()>
}

impl<'de, A: de::MapAccess<'de>> StructMapProxyDeserializer<'de, A> {
    pub fn new(map: A) -> Self { StructMapProxyDeserializer { map, phantom: PhantomData } }
}

impl<'de, A: de::MapAccess<'de>> Deserializer<'de> for StructMapProxyDeserializer<'de, A> {
    type Error = A::Error;

    fn deserialize_struct<V>(self, _: &'static str, _: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> {
        visitor.visit_map(self.map)
    }

    fn deserialize_seq<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_bool<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i8<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i16<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_i64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_f32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_f64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u8<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u16<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u32<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_u64<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    serde_if_integer128! {
        fn deserialize_i128<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

        fn deserialize_u128<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }
    }

    fn deserialize_char<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_str<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_string<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_bytes<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_byte_buf<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_option<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_unit<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_unit_struct<V>(self, _: &'static str, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_newtype_struct<V>(self, _: &'static str, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_map<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_tuple<V>(self, _: usize, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_tuple_struct<V>(self, _: &'static str, _: usize, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_enum<V>(self, _: &'static str, _: &'static [&'static str], _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_identifier<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }

    fn deserialize_ignored_any<V>(self, _: V) -> Result<V::Value, Self::Error> where V: de::Visitor<'de> { unreachable!() }
}
