macro_rules! bitflags_ext {
    (
        struct $flags:ident : $ty:ty {
            $($(
                $name:ident = $value:expr
            ),+ $(,)?)?
        }
    ) => {
        bitflags_ext! {
            @impl () $flags : $ty [$($($name = $value),+)?]
        }
    };
    (
        pub $(($($vis:tt)+))? struct $flags:ident : $ty:ty {
            $($(
                $name:ident = $value:expr
            ),+ $(,)?)?
        }
    ) => {
        bitflags_ext! {
            @impl (pub $(($($vis)+))?) $flags : $ty [$($($name = $value),+)?]
        }
    };
    (
        @impl ($($vis:tt)*) $flags:ident : $ty:ty [$($name:ident = $value:expr),*]
    ) => {
        bitflags::bitflags! {
            #[derive(Default)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            $($vis)* struct $flags: $ty {
                $(const $name = $value;)*
            }
        }

        impl ::std::fmt::Display for $flags {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
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
