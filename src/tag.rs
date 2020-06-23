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
        write!(f, "{}{}{}{}", c0, c1, c2, c3)
    }
}

impl FromStr for Tag {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut dword = 0;
        let mut i = 0;
        for byte in s.bytes() {
            if i == 4 { return Err(()); }
            dword |= (byte as u32) << (8 * i);
            i += 1;
        }
        if i != 4 { return Err(()); }
        Ok(Tag { dword })
    }
}

macro_rules! enum_serde {
    ($name:ty, $exp:literal, $bits:ty, $to:ident, as $from:ident, $signed:ident, $big:ident) => {
        impl ::serde::Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> where
                S: ::serde::Serializer {
        
                if serializer.is_human_readable() {
                    serializer.serialize_str(&format!("{}", self))
                } else {
                    <$bits as ::serde::Serialize>::serialize(
                        &::num_traits::cast::ToPrimitive::$to(self).unwrap(),
                        serializer
                    )
                }
            }
        }
        
        impl<'de> ::serde::Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error> where
                D: ::serde::Deserializer<'de> {
        
                struct HRDeserializer;
                
                impl<'de> ::serde::de::Visitor<'de> for HRDeserializer {
                    type Value = $name;
                
                    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, $exp) }
                
                    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> where E: ::serde::de::Error {
                        ::std::str::FromStr::from_str(s).map_err(|_| E::invalid_value(::serde::de::Unexpected::Str(s), &self))
                    }
                }
        
                if deserializer.is_human_readable() {
                    deserializer.deserialize_str(HRDeserializer)
                } else {
                    let b = <$bits as ::serde::Deserialize>::deserialize(deserializer)?;
                    ::num_traits::cast::FromPrimitive::$from(b).ok_or_else(|| <D::Error as ::serde::de::Error>::invalid_value(::serde::de::Unexpected::$signed(b as $big), &$exp))
                }
            }
        }
    };
    ($name:ty, $exp:literal, $bits:ty, $to:ident, $from:ident) => {
        impl ::serde::Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> where
                S: ::serde::Serializer {
        
                if serializer.is_human_readable() {
                    serializer.serialize_str(&format!("{}", self))
                } else {
                    <$bits as ::serde::Serialize>::serialize(&self.$to, serializer)
                }
            }
        }
        
        impl<'de> ::serde::Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error> where
                D: ::serde::Deserializer<'de> {
        
                struct HRDeserializer;
                
                impl<'de> ::serde::de::Visitor<'de> for HRDeserializer {
                    type Value = $name;
                
                    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, $exp) }
                
                    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> where E: ::serde::de::Error {
                        ::std::str::FromStr::from_str(s).map_err(|_| E::invalid_value(::serde::de::Unexpected::Str(s), &self))
                    }
                }
        
                if deserializer.is_human_readable() {
                    deserializer.deserialize_str(HRDeserializer)
                } else {
                    let b = <$bits as ::serde::Deserialize>::deserialize(deserializer)?;
                    Ok(<$name>::$from(b))
                }
            }
        }
    };
    ($name:ty, $exp:literal, $bits:ty, $to:ident, try $from:ident, $signed:ident, $big:ident $(, ^$xor:literal)?) => {
        impl ::serde::Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> where
                S: ::serde::Serializer {
        
                if serializer.is_human_readable() {
                    serializer.serialize_str(&format!("{}", self))
                } else {
                    <$bits as ::serde::Serialize>::serialize(&(self.$to $(^ $xor)?), serializer)
                }
            }
        }
        
        impl<'de> ::serde::Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error> where
                D: ::serde::Deserializer<'de> {
        
                struct HRDeserializer;
                
                impl<'de> ::serde::de::Visitor<'de> for HRDeserializer {
                    type Value = $name;
                
                    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, $exp) }
                
                    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> where E: ::serde::de::Error {
                        ::std::str::FromStr::from_str(s).map_err(|_| E::invalid_value(::serde::de::Unexpected::Str(s), &self))
                    }
                }
        
                if deserializer.is_human_readable() {
                    deserializer.deserialize_str(HRDeserializer)
                } else {
                    let b = <$bits as ::serde::Deserialize>::deserialize(deserializer)? $(^ $xor)?;
                    let v = <$name>::$from(b).ok_or_else(|| <D::Error as ::serde::de::Error>::invalid_value(::serde::de::Unexpected::$signed(b as $big), &$exp))?;
                    Ok(v)
                }
            }
        }
    };
    ($name:ty, $exp:literal, $bits:ty, $to:ident (), try $from:ident, $signed:ident, $big:ident $(, ^$xor:literal)?) => {
        impl ::serde::Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> where
                S: ::serde::Serializer {
        
                if serializer.is_human_readable() {
                    serializer.serialize_str(&format!("{}", self))
                } else {
                    <$bits as ::serde::Serialize>::serialize(&(self.$to() $(^ $xor)?), serializer)
                }
            }
        }
        
        impl<'de> ::serde::Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error> where
                D: ::serde::Deserializer<'de> {
        
                struct HRDeserializer;
                
                impl<'de> ::serde::de::Visitor<'de> for HRDeserializer {
                    type Value = $name;
                
                    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, $exp) }
                
                    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> where E: ::serde::de::Error {
                        ::std::str::FromStr::from_str(s).map_err(|_| E::invalid_value(::serde::de::Unexpected::Str(s), &self))
                    }
                }
        
                if deserializer.is_human_readable() {
                    deserializer.deserialize_str(HRDeserializer)
                } else {
                    let b = <$bits as ::serde::Deserialize>::deserialize(deserializer)? $(^ $xor)?;
                    let v = <$name>::$from(b).ok_or_else(|| <D::Error as ::serde::de::Error>::invalid_value(::serde::de::Unexpected::$signed(b as $big), &$exp))?;
                    Ok(v)
                }
            }
        }
    }
}

enum_serde!(Tag, "four-byte tag", u32, dword, from);
