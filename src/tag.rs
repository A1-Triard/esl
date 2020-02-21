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
            dword = dword | (byte as u32) << (8 * i);
            i += 1;
        }
        if i != 4 { return Err(()); }
        Ok(Tag { dword })
    }
}

macro_rules! enum_serde {
    ([$name:ident, $de:ident, $exp:literal, $bits:ty, $from:ident, $to:ident,
    $visit:ident, $serialize:ident, $deserialize:ident, $signed:ident, $big:ident]) => {
        impl ::serde::Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> where
                S: ::serde::Serializer {
        
                if serializer.is_human_readable() {
                    serializer.serialize_str(&format!("{}", self))
                } else {
                    serializer.$serialize(::num_traits::cast::ToPrimitive::$to(self).unwrap())
                }
            }
        }
        
        struct $de {
            is_human_readable: bool
        }
        
        impl<'de> ::serde::de::Visitor<'de> for $de {
            type Value = $name;
        
            fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, $exp)
            }
        
            fn $visit<E: ::serde::de::Error>(self, v: $bits) -> ::std::result::Result<Self::Value, E> {
                if self.is_human_readable {
                    Err(E::invalid_type(::serde::de::Unexpected::$signed(v as $big), &self))
                } else {
                    Ok(::num_traits::cast::FromPrimitive::$from(v).unwrap())
                }
            }
        
            fn visit_str<E: ::serde::de::Error>(self, v: &str) -> ::std::result::Result<Self::Value, E> {
                if self.is_human_readable {
                    ::std::str::FromStr::from_str(v).map_err(|_| E::invalid_value(::serde::de::Unexpected::Str(v), &self))
                } else {
                    Err(E::invalid_type(::serde::de::Unexpected::Str(v), &self))
                }
            }
        }
        
        impl<'de> ::serde::Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error> where
                D: ::serde::Deserializer<'de> {
        
                if deserializer.is_human_readable() {
                    deserializer.deserialize_str($de { is_human_readable: true })
                } else {
                    deserializer.$deserialize($de { is_human_readable: false })
                }
            }
        }
    };
    (<$name:ident, $de:ident, $exp:literal, $bits:ty, $from:ident, $to:ident,
    $visit:ident, $serialize:ident, $deserialize:ident, $signed:ident, $big:ident>) => {
        impl ::serde::Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> where
                S: ::serde::Serializer {
        
                if serializer.is_human_readable() {
                    serializer.serialize_str(&format!("{}", self))
                } else {
                    serializer.$serialize(self.$to)
                }
            }
        }
        
        struct $de {
            is_human_readable: bool
        }
        
        impl<'de> ::serde::de::Visitor<'de> for $de {
            type Value = $name;
        
            fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, $exp)
            }
        
            fn $visit<E: ::serde::de::Error>(self, v: $bits) -> ::std::result::Result<Self::Value, E> {
                if self.is_human_readable {
                    Err(E::invalid_type(::serde::de::Unexpected::$signed(v as $big), &self))
                } else {
                    Ok($name::$from(v))
                }
            }
        
            fn visit_str<E: ::serde::de::Error>(self, v: &str) -> ::std::result::Result<Self::Value, E> {
                if self.is_human_readable {
                    ::std::str::FromStr::from_str(v).map_err(|_| E::invalid_value(::serde::de::Unexpected::Str(v), &self))
                } else {
                    Err(E::invalid_type(::serde::de::Unexpected::Str(v), &self))
                }
            }
        }
        
        impl<'de> ::serde::Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error> where
                D: ::serde::Deserializer<'de> {
        
                if deserializer.is_human_readable() {
                    deserializer.deserialize_str($de { is_human_readable: true })
                } else {
                    deserializer.$deserialize($de { is_human_readable: false })
                }
            }
        }
    };
    ({$name:ident, $de:ident, $exp:literal, $bits:ty, $from:ident, $to:ident,
    $visit:ident, $serialize:ident, $deserialize:ident, $signed:ident, $big:ident}) => {
        impl ::serde::Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> where
                S: ::serde::Serializer {
        
                if serializer.is_human_readable() {
                    serializer.serialize_str(&format!("{}", self))
                } else {
                    serializer.$serialize(self.$to)
                }
            }
        }
        
        struct $de {
            is_human_readable: bool
        }
        
        impl<'de> ::serde::de::Visitor<'de> for $de {
            type Value = $name;
        
            fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, $exp)
            }
        
            fn $visit<E: ::serde::de::Error>(self, v: $bits) -> ::std::result::Result<Self::Value, E> {
                if self.is_human_readable {
                    Err(E::invalid_type(::serde::de::Unexpected::$signed(v as $big), &self))
                } else {
                    $name::$from(v).ok_or_else(|| E::invalid_value(::serde::de::Unexpected::$signed(v as $big), &self))
                }
            }
        
            fn visit_str<E: ::serde::de::Error>(self, v: &str) -> ::std::result::Result<Self::Value, E> {
                if self.is_human_readable {
                    ::std::str::FromStr::from_str(v).map_err(|_| E::invalid_value(::serde::de::Unexpected::Str(v), &self))
                } else {
                    Err(E::invalid_type(::serde::de::Unexpected::Str(v), &self))
                }
            }
        }
        
        impl<'de> ::serde::Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> ::std::result::Result<Self, D::Error> where
                D: ::serde::Deserializer<'de> {
        
                if deserializer.is_human_readable() {
                    deserializer.deserialize_str($de { is_human_readable: true })
                } else {
                    deserializer.$deserialize($de { is_human_readable: false })
                }
            }
        }
    }
}

enum_serde!(<
    Tag, TagDeserializer, "four-byte tag",
    u32, from, dword, visit_u32, serialize_u32, deserialize_u32,
    Unsigned, u64
>);
