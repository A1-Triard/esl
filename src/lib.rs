#![feature(ptr_offset_from)]
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
mod tag;

pub use crate::tag::*;

mod core;

pub use crate::core::*;

pub mod read;
