macro_rules! pub_bitflags_display {
    ($flags:ident, $ty:ty, $name:ident = $value:literal) => {
        bitflags! {
            #[derive(Default)]
            pub struct $flags: $ty {
                const $name = $value;
            }
        }
        
        impl ::std::fmt::Display for $flags {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                if self.contains($flags::$name) {
                    write!(f, stringify!($name))?;
                }
                Ok(())
            }
        }
        
        impl ::std::str::FromStr for $flags {
            type Err = ();
        
            fn from_str(s: &str) -> Result<$flags, Self::Err> {
                fn flag(input: &str) -> ::nom::IResult<&str, $flags, ()> {
                    ::nom::combinator::value($flags::$name, ::nom::bytes::complete::tag(stringify!($name)))
                    (input)
                }

                let (unconsumed, flags_value) = ::nom::combinator::map(::nom::combinator::opt(
                    flag,
                ), |m| m.unwrap_or($flags::empty()))(s).map_err(|_: ::nom::Err<()>| ())?;
                if !unconsumed.is_empty() { return Err(()); }
                Ok(flags_value)
            }
        }
    };
    ($flags:ident, $ty:ty, $($name:ident = $value:literal),+) => {
        bitflags! {
            #[derive(Default)]
            pub struct $flags: $ty {
                $(const $name = $value;)*
            }
        }
        
        impl ::std::fmt::Display for $flags {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                let mut start = true;
                $(
                    if self.contains($flags::$name) {
                        if start {
                            #[allow(unused_assignments)]
                            start = false;
                        } else {
                            write!(f, " ")?;
                        }
                        write!(f, stringify!($name))?;
                    }
                )*
                Ok(())
            }
        }
        
        impl ::std::str::FromStr for $flags {
            type Err = ();
        
            fn from_str(s: &str) -> Result<$flags, Self::Err> {
                fn flag(input: &str) -> ::nom::IResult<&str, $flags, ()> {
                    ::nom::branch::alt((
                        $(
                            ::nom::combinator::value($flags::$name, ::nom::bytes::complete::tag(stringify!($name)))
                        ),*
                    ))(input)
                }

                let (unconsumed, flags_value) = ::nom::combinator::map(::nom::combinator::opt(::nom::combinator::map(::nom::sequence::pair(
                    flag,
                    ::nom::multi::fold_many0(::nom::sequence::preceded(::nom::bytes::complete::take_while(char::is_whitespace), flag), $flags::empty(), |a, v| a | v)
                ), |(a, b)| a | b)), |m| m.unwrap_or($flags::empty()))(s).map_err(|_: ::nom::Err<()>| ())?;
                if !unconsumed.is_empty() { return Err(()); }
                Ok(flags_value)
            }
        }
    };
    ($flags:ident, $ty:ty, $([$($name:ident = $value:literal),+]),+) => {
        bitflags! {
            #[derive(Default)]
            pub struct $flags: $ty {
                $($(const $name = $value;)+)+
            }
        }
        
        impl ::std::fmt::Display for $flags {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                let mut start = true;
                $($(
                    if self.contains($flags::$name) {
                        if start {
                            #[allow(unused_assignments)]
                            start = false;
                        } else {
                            write!(f, " ")?;
                        }
                        write!(f, stringify!($name))?;
                    }
                )+)+
                Ok(())
            }
        }
        
        impl ::std::str::FromStr for $flags {
            type Err = ();
        
            fn from_str(s: &str) -> Result<$flags, Self::Err> {
                fn flag(input: &str) -> ::nom::IResult<&str, $flags, ()> {
                    ::nom::branch::alt((
                        $(::nom::branch::alt((
                            $(
                                ::nom::combinator::value($flags::$name, ::nom::bytes::complete::tag(stringify!($name)))
                            ),+
                        ))),+
                    ))(input)
                }

                let (unconsumed, flags_value) = ::nom::combinator::map(::nom::combinator::opt(::nom::combinator::map(::nom::sequence::pair(
                    flag,
                    ::nom::multi::fold_many0(::nom::sequence::preceded(::nom::bytes::complete::take_while(char::is_whitespace), flag), $flags::empty(), |a, v| a | v)
                ), |(a, b)| a | b)), |m| m.unwrap_or($flags::empty()))(s).map_err(|_: ::nom::Err<()>| ())?;
                if !unconsumed.is_empty() { return Err(()); }
                Ok(flags_value)
            }
        }
    }
}

