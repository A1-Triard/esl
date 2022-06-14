![maintenance: actively developed](https://img.shields.io/badge/maintenance-actively--developed-brightgreen.svg)

# esl

A library for reading, writing and processing ESM/ESP/ESS files.

```rust
use esl::code::CodePage;
use esl::read::{RecordReadMode, Records};
use std::fs::File;
use std::io::{BufReader, BufWriter};

fn main() {
    if let Ok(input) = File::open("Morrowind.esm") {
        let mut input = BufReader::new(input);
        let records = Records::new(CodePage::Russian, RecordReadMode::Strict, 0, &mut input);
        let records = records.map(|x| {
            match x {
                Ok(mut x) => {
                    x.fit();
                    x
                },
                Err(e) => panic!("{}", e)
            }
        }).collect::<Vec<_>>();
        let output = File::create("Morrowind.esm.yaml").unwrap();
        serde_yaml::to_writer(BufWriter::new(output), &records).unwrap();
    }
}
```
