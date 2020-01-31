use crate::core::*;

use nom::IResult;
use nom::combinator::{map};
use nom::sequence::{pair, tuple};
use nom::number::complete::{le_u32, /*le_u64,*/ le_i32};
use nom::error::{ParseError, ErrorKind};
use nom::bytes::complete::take;
use encoding::types::Encoding;
use encoding::{DecoderTrap};
use encoding::all::WINDOWS_1251;
use num_traits::cast::FromPrimitive;

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Clone, Copy)]
enum Void { }

impl Void {
    fn unreachable<T>(self) -> T { unreachable!() }
}

impl<I> ParseError<I> for Void {
    fn from_error_kind(_input: I, _kind: ErrorKind) -> Self { panic!() }

    fn append(_input: I, _kind: ErrorKind, _other: Self) -> Self { unreachable!() }

    fn or(self, _other: Self) -> Self { unreachable!() }

    fn add_context(_input: I, _ctx: &'static str, _other: Self) -> Self { unreachable!() }
}

fn map_err<I: Clone, O, E, X, F>(f: F, m: impl Fn(E) -> X)
    -> impl Fn(I) -> IResult<I, O, X> where
    F: Fn(I) -> IResult<I, O, E> {
    
    move |input: I| {
        match f(input) {
            Err(nom::Err::Error(e)) => Err(nom::Err::Error(m(e))),
            Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(m(e))),
            Err(nom::Err::Incomplete(n)) => Err(nom::Err::Incomplete(n)),
            Ok(r) => Ok(r)
        }
    }
}

fn set_err<I: Clone, O, X, F>(f: F, m: impl Fn(I) -> X)
                                 -> impl Fn(I) -> IResult<I, O, X> where
    F: Fn(I) -> IResult<I, O, ()> {

    move |input: I| {
        match f(input.clone()) {
            Err(nom::Err::Error(())) => Err(nom::Err::Error(m(input))),
            Err(nom::Err::Failure(())) => Err(nom::Err::Failure(m(input))),
            Err(nom::Err::Incomplete(n)) => Err(nom::Err::Incomplete(n)),
            Ok(r) => Ok(r)
        }
    }
}

fn map_res<I: Clone, O, E, R, F>(f: F, m: impl Fn(O, I) -> Result<R, nom::Err<E>>)
    -> impl Fn(I) -> IResult<I, R, E> where
    F: Fn(I) -> IResult<I, O, E>,
    E: ParseError<I> {

    move |input: I| {
        match f(input.clone()) {
            Err(nom::Err::Error(e)) => Err(nom::Err::Error(e)),
            Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(e)),
            Err(nom::Err::Incomplete(n)) => Err(nom::Err::Incomplete(n)),
            Ok((i, r)) => m(r, input).map(|x| (i, x)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FieldBodyError<'a> {
    UnexpectedEndOfField(u32),
    UnknownFileType(&'a [u8], u32),
}

impl<'a> ParseError<&'a [u8]> for FieldBodyError<'a> {
    fn from_error_kind(_input: &'a [u8], _kind: ErrorKind) -> Self { panic!() }

    fn append(_input: &'a [u8], _kind: ErrorKind, _other: Self) -> Self { panic!() }

    fn or(self, _other: Self) -> Self { panic!() }

    fn add_context(_input: &'a [u8], _ctx: &'static str, _other: Self) -> Self { panic!() }
}

fn binary_field<E>(input: &[u8]) -> IResult<&[u8], Vec<u8>, E> {
    Ok((&input[input.len() .. ], input.into()))
}

fn decode_string(coerce: StringCoerce, bytes: &[u8]) -> String {
    let mut s = WINDOWS_1251.decode(bytes, DecoderTrap::Strict).unwrap();
    coerce.coerce(&mut s);
    s
}

fn string_field<E>(coerce: StringCoerce) -> impl Fn(&[u8]) -> IResult<&[u8], String, E> {
    move |input| {
        Ok((&input[input.len()..], decode_string(coerce, input)))
    }
}

fn fixed_string<'a>(length: u32) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], String, ()> {
    map(take(length), |bytes| decode_string(StringCoerce::CutTailZeros, bytes))
}

fn file_metadata_field(input: &[u8]) -> IResult<&[u8], FileMetadata, FieldBodyError> {
    map(
        tuple((
            set_err(le_u32, |_| FieldBodyError::UnexpectedEndOfField(300)),
            map_res(le_u32, |w, input| FileType::from_u32(w).ok_or(nom::Err::Error(FieldBodyError::UnknownFileType(input, w)))),
            set_err(
                tuple((
                    fixed_string(32),
                    map(fixed_string(256), |s| LinebreakStyle::Dos.split(&s).map(String::from).collect()),
                    map(le_u32, |_| ())
                )),
                |_| FieldBodyError::UnexpectedEndOfField(300)
            )
        )),
        |(version, file_type, (author, description, ()))| FileMetadata { version, file_type, author, description}
    )(input)
}

fn fixed_string_field<'a>(length: u32) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], String, FieldBodyError> {
    set_err(fixed_string(length), move |_| FieldBodyError::UnexpectedEndOfField(length))
}

fn multiline_field<'a, E>(linebreaks: LinebreakStyle, coerce: StringCoerce)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<String>, E> {

    map_err(
        map(string_field::<Void>(coerce), move |s| linebreaks.split(&s).map(String::from).collect()),
        |x| x.unreachable()
    )
}

fn multi_string_field<E>(input: &[u8]) -> IResult<&[u8], Vec<String>, E> {
    map_err(
        map(string_field::<Void>(StringCoerce::None), |s| s.split("\0").map(String::from).collect()),
        |x| x.unreachable()
    )(input)
}

fn reference_field(input: &[u8]) -> IResult<&[u8], (i32, String), FieldBodyError> {
    set_err(
        pair(
            le_i32,
            fixed_string(32)
        ),
        |_| FieldBodyError::UnexpectedEndOfField(4 + 32)
    )(input)
}

pub fn field_body<'a>(allow_coerce: bool, record_tag: Tag, field_tag: Tag, _field_size: u32)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Field, FieldBodyError> {

    move |input| {
        let field_type = FieldType::from_tags(record_tag, field_tag).coerce(allow_coerce);
        match field_type {
            FieldType::Multiline(linebreaks, coerce) =>
                map(multiline_field(linebreaks, coerce), Field::Multiline)(input),
            FieldType::Reference =>
                map(reference_field, |(count, name)| Field::Reference(count, name))(input),
            FieldType::FixedString(len) =>
                map(fixed_string_field(len), Field::String)(input),
            FieldType::String(coerce) =>
                map(string_field(coerce), Field::String)(input),
            FieldType::MultiString =>
                map(multi_string_field, Field::MultiString)(input),
            FieldType::FileMetadata =>
                map(file_metadata_field, Field::FileMetadata)(input),
            _ =>
                map(binary_field, Field::Binary)(input)
        }
    }
}

/*
fn tag(input: &[u8]) -> IResult<&[u8], Tag, ()> {
    map(le_u32, Tag::from)(input)
}

pub fn field_head<'a>(record_start: &'a [u8], prev_fields_size: u32)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], (Tag, u32), Error> {

    map_err(
        pair(tag, le_u32),
        move |(), _| Error::new(record_start).descript(ErrorDescription::UnexpectedEndOfRecord(prev_fields_size + 8))
    )
}

pub fn field<'a>(allow_coerce: bool, record_tag: Tag, record_start: &'a [u8], prev_fields_size: u32)
    -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Field, Error> {
    
    
}
*/
    /*
/*
pub fn record_flags(input: &[u8]) -> IResult<&[u8], RecordFlags, Error> {
    map_res(
        map_err(le_u64, |e: Error, _| e.descript(ErrorDescription::UnexpectedEndOfRecord(8))),
        |d, input| RecordFlags::from_bits(d).ok_or(nom::Err::Error(Error::new(input).descript(ErrorDescription::InvalidRecordFlags(d))))
    )(input) 
}
*/
fieldBody :: Bool -> T3Sign -> T3Sign -> Word32 -> Get T3Error T3Field
fieldBody adjust record_sign s field_size =
f (t3FieldType record_sign s)
where
f (T3FixedString z) = T3StringField s . (`T.snoc` '\0') <$> fixedStringField z ?>> T3UnexpectedEndOfField <$> bytesRead
f (T3String a) = T3StringField s <$> (if adjust then a else id) <$> stringField ?>> T3UnexpectedEndOfField <$> bytesRead
f (T3Multiline use_unix_newlines a) = T3MultilineField s <$> multilineField use_unix_newlines (if adjust then a else id) ?>> T3UnexpectedEndOfField <$> bytesRead
f T3MultiString = T3MultiStringField s <$> multiStringField ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Ref = (\(z, n) -> T3RefField s z n) <$> refField ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Binary = T3BinaryField s <$> binaryField ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Float = T3FloatField s <$> floatField ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Int = T3IntField s <$> getInt32le ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Short = T3ShortField s <$> getInt16le ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Long = T3LongField s <$> getInt64le ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Byte = T3ByteField s <$> getWord8 ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Compressed = T3CompressedField s <$> compressedField ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Ingredient = T3IngredientField s <$> ingredientField ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Script = T3ScriptField s <$> scriptField ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Dial = T3DialField s <$> dialField field_size
f T3None = (const $ T3NoneField s) <$> expect 0 (getWord32le ?>> T3UnexpectedEndOfField <$> bytesRead)
f T3Header = T3HeaderField s <$> fileHeaderData
f T3EssNpc = T3EssNpcField s <$> essNpcData ?>> T3UnexpectedEndOfField <$> bytesRead
f T3Npc = T3NpcField s <$> npcData field_size
f T3Effect = T3EffectField s <$> effectField
*/

#[cfg(test)]
mod tests {
    use crate::*;
    use ::nom;
    use crate::nom::*;
    use encoding::all::WINDOWS_1251;
    use encoding::types::Encoding;
    use encoding::EncoderTrap;

    fn string(s: &str) -> Vec<u8> {
        WINDOWS_1251.encode(s, EncoderTrap::Strict).unwrap()
    }
    
    fn len(n: usize, s: &str) -> String {
        let mut r = String::from(s);
        while r.len() < n {
            r.push('\0');
        }
        r
    }

    /*
    #[test]
    fn read_record_flags_empty() {
        let input: &'static [u8] = &[0, 0, 0, 0, 0, 0, 0, 0];
        let (remaining_input, result) = record_flags(input).unwrap();
        assert_eq!(remaining_input.len(), 0);
        assert_eq!(result, RecordFlags::empty());
    }

    #[test]
    fn read_record_flags_invalid() {
        let input: &'static [u8] = &[0, 0, 0, 0, 7, 0, 0, 0];
        if let ::nom::Err::Error(e) = record_flags(input).err().unwrap() {
            assert_eq!(e.remaining_input.len(), 8);
            if let Either::Right(ErrorDescription::InvalidRecordFlags(d)) = e.description {
                assert_eq!(d, 0x700000000);
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }

    #[test]
    fn read_record_flags_unexpected_eof() {
        let input: &'static [u8] = &[0, 0, 0, 0];
        if let ::nom::Err::Error(e) = record_flags(input).err().unwrap() {
            if let Either::Right(ErrorDescription::UnexpectedEndOfRecord(needed)) = e.description {
                assert_eq!(e.remaining_input.len(), 4);
                assert_eq!(needed, 8);
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
*/
    
    #[test]
    fn read_multiline_field() {
        let input: &'static [u8] = b"123\r\n\xC0\xC1t\r\n\xDA\xDFX\r\n";
        if let (remaining_input, Field::Multiline(result)) = field_body(true, CONT, BNAM, input.len() as u32)(input).unwrap() {
            assert_eq!(remaining_input.len(), 0);
            assert_eq!(result.len(), 4);
            assert_eq!(result[0], "123");
            assert_eq!(result[1], "АБt");
            assert_eq!(result[2], "ЪЯX");
            assert_eq!(result[3], "");
        } else {
            panic!()
        }
    }

    #[test]
    fn read_from_vec() {
        let input: Vec<u8> = Vec::new();
        field_body(true, TES3, HEDR, input.len() as u32)(&input).err().unwrap();
    }

    #[test]
    fn read_from_vec_if_let() {
        let input: Vec<u8> = Vec::new();
        let res = field_body(true, TES3, HEDR, input.len() as u32)(&input);
        if let Ok((_, _)) = res {
            panic!()
        } else { }
    }
    
    #[test]
    fn read_file_metadata() {
        let mut input: Vec<u8> = Vec::new();
        input.append(&mut vec![0x00, 0x00, 0x00, 0x22]);
        input.append(&mut vec![0x20, 0x00, 0x00, 0x00]);
        input.append(&mut string(&len(32, "author")));
        input.append(&mut string(&len(256, "description\r\nlines\r\n")));
        input.append(&mut vec![0x01, 0x02, 0x03, 0x04]);
        let result = field_body(true, TES3, HEDR, input.len() as u32)(&input);
        if let (remaining_input, Field::FileMetadata(result)) = result.unwrap() {
            assert_eq!(remaining_input.len(), 0);
            assert_eq!(result.file_type, FileType::ESS);
            assert_eq!(result.author, "author");
            assert_eq!(result.description, &["description", "lines", ""]);
            assert_eq!(result.version, 0x22000000);
        } else {
            panic!()
        }
    }

    #[test]
    fn read_invalid_file_type() {
        let mut input: Vec<u8> = Vec::new();
        input.append(&mut vec![0x00, 0x00, 0x00, 0x22]);
        input.append(&mut vec![0x00, 0x00, 0x10, 0x00]);
        input.append(&mut string(&len(32, "author")));
        input.append(&mut string(&len(256, "description")));
        input.append(&mut vec![0x01, 0x02, 0x03, 0x04]);
        let result = field_body(true, TES3, HEDR, input.len() as u32)(&input);
        let error = result.err().unwrap();
        if let nom::Err::Error(FieldBodyError::UnknownFileType(pos, val)) = error {
            assert_eq!(unsafe { pos.as_ptr().offset_from(input.as_ptr()) }, 4);
            assert_eq!(val, 0x100000);
        } else {
            panic!()
        }
    }
}