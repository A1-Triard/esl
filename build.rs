use std::env;
use std::fs::File;
use std::io::{Write, BufReader, BufRead};
use std::path::Path;

fn mark_value(mark: &str) -> u32 {
    mark.bytes().fold(0, |value, byte| (value << 8) | (byte as u32))
}

const MARK: &'static str = "Mark";

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let src_path = Path::new("src/marks.list");
    let dest_path = Path::new(&out_dir).join(&format!("{}.rs", MARK));
    let mut dest = File::create(&dest_path).unwrap();
    dest.write_all(b"#[allow(non_camel_case_types)]
#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u32)]
").unwrap();
    writeln!(dest, "pub enum {} {{", MARK).unwrap();
    let src = BufReader::new(File::open(&src_path).unwrap());
    for (name, value) in src.lines().map(|x| x.unwrap()).map(|m| { let value = mark_value(&m); (m, value) }) {
        write!(dest, "    {}", &name).unwrap();
        dest.write_all(b" = ").unwrap();
        writeln!(dest, "{},", value).unwrap();
    }
    dest.write_all(b"}
").unwrap();
}