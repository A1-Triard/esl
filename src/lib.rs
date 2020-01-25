#![deny(warnings)]
mod tag;

pub use tag::*;

include!(concat!(env!("OUT_DIR"), "/tags.rs"));

#[cfg(test)]
mod tests {
    use crate::*;
    
    #[test]
    fn debug_and_display_mark() {
        assert_eq!("TES3", format!("{}", TES3));
        assert_eq!("TES3", format!("{:?}", TES3));
    }
}
