macro_rules! pub_bitflags_display {
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
                let mut flags = $flags::empty();
                for f in s.split(char::is_whitespace) {
                    match f {
                        "" => { },
                        $(
                            stringify!($name) => flags |= $flags::$name,
                        )*
                        _ => return Err(())
                    }
                }
                Ok(flags)
            }
        }
    }
}

