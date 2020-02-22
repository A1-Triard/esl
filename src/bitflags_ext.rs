use ::nom::IResult;
use ::nom::combinator::{value as nom_value};
use ::nom::bytes::complete::tag as nom_tag;
use ::nom::sequence::{preceded, terminated};
use ::nom::bytes::complete::take_while;

pub fn pipe(input: &str) -> IResult<&str, (), ()> {
    nom_value((),
        terminated(preceded(take_while(char::is_whitespace), nom_tag("|")), take_while(char::is_whitespace))
    )(input)
}

macro_rules! pub_bitflags_display {
    ($flags:ident, $ty:ty, $($name:ident = $value:literal),*) => {
        bitflags! {
            pub struct $flags: $ty {
                $(const $name = $value;)*
            }
        }
        
        impl ::std::fmt::Display for $flags {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                ::std::fmt::Debug::fmt(self, f)
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
                    ::nom::multi::fold_many0(::nom::sequence::preceded(crate::bitflags_ext::pipe, flag), $flags::empty(), |a, v| a | v)
                ), |(a, b)| a | b)), |m| m.unwrap_or($flags::empty()))(s).map_err(|_: ::nom::Err<()>| ())?;
                if !unconsumed.is_empty() { return Err(()); }
                Ok(flags_value)
            }
        }
    }
}

