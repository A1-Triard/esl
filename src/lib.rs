#![feature(ptr_offset_from)]
#![feature(type_alias_impl_trait)]
#![deny(warnings)]

#[macro_use]
extern crate enum_derive;
#[macro_use]
extern crate enum_primitive_derive;
#[macro_use]
extern crate macro_attr;
#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate debug_panic;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate serde;

#[macro_use]
mod tag;

pub use crate::tag::*;

mod field;

pub use crate::field::*;

mod record;

pub use crate::record::*;

pub mod read;

mod strings_serde;
mod field_serde;

pub mod serde_ {
    pub use crate::strings_serde::*;
    pub use crate::field_serde::*;
}

mod strings;

pub use crate::strings::*;

pub mod code;
pub mod code_vec;