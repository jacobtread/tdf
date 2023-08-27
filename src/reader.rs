//! Buffer reading wrapper to provided a way to easily read data from
//! packet buffers provides easy functions for all the different tdf types

use super::{
    error::{DecodeError, DecodeResult},
    tag::{Tag, Tagged, TdfType},
    types::{TdfDeserialize, TdfDeserializeOwned, TdfTyped},
};
use crate::types::{
    blob::skip_blob,
    float::skip_f32,
    list::skip_list,
    map::{deserialize_map_header, skip_map},
    tagged_union::{skip_tagged_union, TAGGED_UNSET_KEY},
    var_int::skip_var_int,
    Blob, ObjectId, ObjectType, VarIntList,
};

/// Buffered readable implementation. Allows reading through the
/// underlying slice using a cursor and with a position that can
/// be saved using the marker. Provides functions for reading
/// certain data types in the Blaze format
pub struct TdfDeserializer<'a> {
    /// The underlying buffer to read from
    pub buffer: &'a [u8],
    /// The cursor position on the buffer. The cursor should not be set
    /// to any arbitry values should only be set to previously know values
    pub cursor: usize,
}

impl<'de> TdfDeserializer<'de> {
    /// Creates a new reader over the provided slice of bytes with
    /// the default cursor position at zero
    pub fn new(buffer: &'de [u8]) -> Self {
        Self { buffer, cursor: 0 }
    }

    /// Takes a single byte from the underlying buffer moving
    /// the cursor over by one. Will return UnexpectedEof error
    /// if there are no bytes left
    pub fn read_byte(&mut self) -> DecodeResult<u8> {
        self.expect_length(1)?;
        let byte: u8 = self.buffer[self.cursor];
        self.cursor += 1;
        Ok(byte)
    }

    pub fn read_bytes<const S: usize>(&mut self) -> DecodeResult<[u8; S]> {
        // Ensure we have the required number of bytes
        self.expect_length(S)?;
        // Alocate and copy the bytes from the buffer
        let mut bytes: [u8; S] = [0u8; S];
        bytes.copy_from_slice(&self.buffer[self.cursor..self.cursor + S]);
        // Move the cursor
        self.cursor += S;
        Ok(bytes)
    }

    /// Takes a slice of the provided length from the portion of the
    /// buffer that is after the cursor position
    ///
    /// `length` The length of the slice to take
    pub fn read_slice(&mut self, length: usize) -> DecodeResult<&'de [u8]> {
        self.expect_length(length)?;
        let slice: &[u8] = &self.buffer[self.cursor..self.cursor + length];
        self.cursor += length;
        Ok(slice)
    }

    /// Attempts to ensure the next length exists past the cursor
    /// will return an UnexpectedEof if there is not enough bytes
    ///
    /// `length` The length to expect
    fn expect_length(&self, length: usize) -> DecodeResult<()> {
        if self.cursor + length > self.buffer.len() {
            Err(DecodeError::UnexpectedEof {
                cursor: self.cursor,
                wanted: length,
                remaining: self.len(),
            })
        } else {
            Ok(())
        }
    }

    /// Returns the remaining length left after the cursor
    pub fn len(&self) -> usize {
        self.buffer.len() - self.cursor
    }

    /// Returns if there is nothing left after the cursor
    pub fn is_empty(&self) -> bool {
        self.cursor >= self.buffer.len()
    }

    /// Decodes tags from the reader until the tag with the provided tag name
    /// is found. If the tag type doesn't match the `expected_type` then an
    /// error will be returned.
    ///
    /// `tag` The tag name to read until
    /// `ty`  The expected type of the tag
    pub fn until_tag(&mut self, tag: &[u8], ty: TdfType) -> DecodeResult<()> {
        let tag = Tag::from(tag);
        loop {
            let next_tag = match Tagged::deserialize_owned(self) {
                Ok(value) => value,
                Err(DecodeError::UnexpectedEof { .. }) => {
                    return Err(DecodeError::MissingTag { tag, ty })
                }
                Err(err) => return Err(err),
            };

            if next_tag.tag != tag {
                self.skip_type(&next_tag.ty)?;
                continue;
            }

            if next_tag.ty.ne(&ty) {
                return Err(DecodeError::InvalidTagType {
                    tag,
                    expected: ty,
                    actual: next_tag.ty,
                });
            }

            return Ok(());
        }
    }

    /// Attempting version of decode_until that returns true if the value was decoded up to
    /// otherwise returns false. Marks the reader position before reading and resets the position
    /// if the tag was not found
    ///
    /// `tag` The tag name to read until
    /// `ty`  The expected type of the tag
    pub fn try_until_tag(&mut self, tag: &[u8], ty: TdfType) -> bool {
        let tag = Tag::from(tag);
        let start = self.cursor;

        while let Ok(next_tag) = Tagged::deserialize_owned(self) {
            if next_tag.tag != tag {
                if self.skip_type(&next_tag.ty).is_err() {
                    break;
                } else {
                    continue;
                }
            }

            if next_tag.ty.ne(&ty) {
                break;
            }

            return true;
        }
        self.cursor = start;
        false
    }

    /// Reads the provided tag from the buffer discarding values until it
    /// reaches the correct value.
    ///
    /// `tag` The tag name to read
    pub fn tag<C>(&mut self, tag: &[u8]) -> DecodeResult<C>
    where
        C: TdfDeserialize<'de> + TdfTyped,
    {
        self.until_tag(tag, C::TYPE)?;
        C::deserialize(self)
    }

    /// Reads the provided tag from the buffer discarding values until it
    /// reaches the correct value. If the tag is missing the cursor is reset
    /// back to where it was
    ///
    /// `tag` The tag name to read
    pub fn try_tag<C>(&mut self, tag: &[u8]) -> DecodeResult<Option<C>>
    where
        C: TdfDeserialize<'de> + TdfTyped,
    {
        let start = self.cursor;
        match self.tag(tag) {
            Ok(value) => Ok(Some(value)),
            Err(DecodeError::MissingTag { .. }) => {
                self.cursor = start;
                Ok(None)
            }
            Err(err) => Err(err),
        }
    }

    /// Reads the next TdfType value checking that it
    /// matches the provided type and returns an invalid
    /// type error if the type doesn't match
    pub fn expect_type(&mut self, ty: TdfType) -> DecodeResult<()> {
        let value_type = TdfType::deserialize_owned(self)?;

        if value_type != ty {
            Err(DecodeError::InvalidType {
                expected: ty,
                actual: value_type,
            })
        } else {
            Ok(())
        }
    }

    /// Skips the provided length in bytes on the underlying
    /// buffer returning an error if there is not enough space
    pub fn skip_length(&mut self, length: usize) -> DecodeResult<()> {
        self.expect_length(length)?;
        self.cursor += length;
        Ok(())
    }

    /// Skips the starting 2 value at the beggining of the group
    /// if it exists
    pub fn skip_group_2(&mut self) -> DecodeResult<()> {
        let first = self.read_byte()?;
        if first != 2 {
            self.cursor -= 1;
        }
        Ok(())
    }

    /// Skips an entire group if one exists
    pub fn skip_group(&mut self) -> DecodeResult<()> {
        self.skip_group_2()?;
        while self.cursor < self.buffer.len() {
            let byte: u8 = self.buffer[self.cursor];
            if byte == 0 {
                self.cursor += 1;
                break;
            }
            self.skip()?;
        }
        Ok(())
    }

    /// Skips the next tag value
    pub fn skip(&mut self) -> DecodeResult<()> {
        let tag = Tagged::deserialize_owned(self)?;
        self.skip_type(&tag.ty)
    }

    /// Skips a data type
    ///
    /// `ty` The type of data to skip
    pub fn skip_type(&mut self, ty: &TdfType) -> DecodeResult<()> {
        match ty {
            TdfType::VarInt => skip_var_int(self)?,
            TdfType::String | TdfType::Blob => skip_blob(self)?,
            TdfType::Group => self.skip_group()?,
            TdfType::List => skip_list(self)?,
            TdfType::Map => skip_map(self)?,
            TdfType::TaggedUnion => skip_tagged_union(self)?,
            TdfType::VarIntList => VarIntList::skip(self)?,
            TdfType::ObjectType => ObjectType::skip(self)?,
            TdfType::ObjectId => ObjectId::skip(self)?,
            TdfType::Float => skip_f32(self)?,
            TdfType::U12 => {
                self.skip_length(8)?;
                skip_blob(self)?; // string
            }
        }
        Ok(())
    }

    pub fn stringify(&mut self, out: &mut String) -> DecodeResult<()> {
        while self.cursor < self.buffer.len() {
            if let Err(err) = self.stringify_tag(out, 1) {
                let remaining = &self.buffer[self.cursor..];
                out.push_str(&format!(
                    "... cause: {:?}, remaining {} {:?}",
                    err,
                    remaining.len(),
                    remaining,
                ));
                break;
            }
        }
        Ok(())
    }

    pub fn stringify_tag(&mut self, out: &mut String, indent: usize) -> DecodeResult<()> {
        let tag = Tagged::deserialize_owned(self)?;
        out.push_str(&"  ".repeat(indent));
        out.push_str(&format!("\"{}\": ", &tag.tag));
        match self.stringify_type(out, indent, &tag.ty) {
            Ok(_) => {
                out.push_str(",\n");
                Ok(())
            }
            Err(err) => {
                out.push_str("...");
                Err(err)
            }
        }
    }

    pub fn stringify_type(
        &mut self,
        out: &mut String,
        indent: usize,
        ty: &TdfType,
    ) -> DecodeResult<()> {
        match ty {
            TdfType::VarInt => {
                let value = usize::deserialize_owned(self)?;
                out.push_str(&value.to_string());
            }
            TdfType::String => {
                let value = String::deserialize_owned(self)?;
                out.push('"');
                out.push_str(&value);
                out.push('"');
            }
            TdfType::Blob => {
                let value = Blob::deserialize_raw(self)?;
                let length = value.len();
                out.push_str("Blob [");
                for (i, value) in value.iter().enumerate() {
                    out.push_str(&format!("0x{:X}", value));
                    if i < length - 1 {
                        out.push_str(", ");
                    }
                }
                out.push(']');
            }
            TdfType::Group => {
                out.push_str("{\n");
                let mut is_two: bool = false;
                while self.cursor < self.buffer.len() {
                    let byte: u8 = self.buffer[self.cursor];
                    if byte == 0 {
                        self.cursor += 1;
                        break;
                    }
                    if byte == 2 {
                        is_two = true;
                        self.cursor += 1;
                    }
                    self.stringify_tag(out, indent + 1)?;
                }
                out.push_str(&"  ".repeat(indent));
                out.push('}');
                if is_two {
                    out.push_str(" (2)");
                }
            }
            TdfType::List => {
                let value_type: TdfType = TdfType::deserialize_owned(self)?;
                let length: usize = usize::deserialize_owned(self)?;
                let expand = matches!(value_type, TdfType::Map | TdfType::Group);
                out.push('[');
                if expand {
                    out.push('\n');
                }

                for i in 0..length {
                    if expand {
                        out.push_str(&"  ".repeat(indent + 1));
                    }
                    self.stringify_type(out, indent + 1, &value_type)?;
                    if i < length - 1 {
                        out.push_str(", ");
                    }
                    if expand {
                        out.push('\n');
                    }
                }
                if expand {
                    out.push_str(&"  ".repeat(indent));
                }
                out.push(']');
            }
            TdfType::Map => {
                let (key_ty, value_ty, length) = deserialize_map_header(self)?;
                out.push_str(&format!("Map<{:?}, {:?}, {}>", key_ty, value_ty, length));
                out.push_str("{\n");

                let start = self.cursor;

                let mut proc = || -> DecodeResult<()> {
                    for i in 0..length {
                        out.push_str(&"  ".repeat(indent + 1));
                        self.stringify_type(out, indent + 1, &key_ty)?;
                        out.push_str(": ");
                        self.stringify_type(out, indent + 1, &value_ty)?;
                        if i < length - 1 {
                            out.push(',');
                        }
                        out.push('\n')
                    }
                    Ok(())
                };

                if let Err(err) = proc() {
                    out.push_str(&format!("Err: {}", err));
                    out.push_str(&format!("Full Bytes: {:?}", &self.buffer[start..]));
                }

                out.push_str(&"  ".repeat(indent));
                out.push('}');
            }
            TdfType::TaggedUnion => {
                let ty = self.read_byte()?;
                if ty == TAGGED_UNSET_KEY {
                    out.push_str("Union(Unset)")
                } else {
                    let tag = Tagged::deserialize_owned(self)?;
                    out.push_str(&format!("Union(\"{}\", {}, ", &tag.tag, ty));
                    self.stringify_type(out, indent + 1, &tag.ty)?;
                    out.push(')')
                }
            }
            TdfType::VarIntList => {
                let length: usize = usize::deserialize_owned(self)?;
                out.push_str("VarList [");
                for i in 0..length {
                    let value = usize::deserialize_owned(self)?;
                    out.push_str(&value.to_string());
                    if i < length - 1 {
                        out.push_str(", ");
                    }
                }
                out.push(']');
            }
            TdfType::ObjectType => {
                let value = ObjectType::deserialize_owned(self)?;
                out.push_str(&format!("{:?}", value));
            }
            TdfType::ObjectId => {
                let value = ObjectId::deserialize_owned(self)?;
                out.push_str(&format!("{:?}", value));
            }
            TdfType::Float => {
                let value = f32::deserialize_owned(self)?;
                out.push_str(&value.to_string());
            }
            TdfType::U12 => {
                let bytes = self.read_slice(8)?;
                out.push_str(&format!("{:?} + ", bytes));
                let value = String::deserialize_owned(self)?;
                out.push_str(&format!("\"{}\"", value));
            }
        };
        Ok(())
    }

    pub fn until_list(&mut self, tag: &[u8], value_type: TdfType) -> DecodeResult<usize> {
        self.until_tag(tag, TdfType::List)?;
        let list_type = TdfType::deserialize_owned(self)?;
        if list_type != value_type {
            return Err(DecodeError::InvalidType {
                expected: value_type,
                actual: list_type,
            });
        }
        let count = usize::deserialize_owned(self)?;
        Ok(count)
    }

    pub fn until_map(
        &mut self,
        tag: &[u8],
        key_type: TdfType,
        value_type: TdfType,
    ) -> DecodeResult<usize> {
        self.until_tag(tag, TdfType::Map)?;

        let (key_ty, value_ty, length) = deserialize_map_header(self)?;

        if key_ty != key_type {
            return Err(DecodeError::InvalidType {
                expected: key_type,
                actual: key_ty,
            });
        }

        if value_ty != value_type {
            return Err(DecodeError::InvalidType {
                expected: value_type,
                actual: value_ty,
            });
        }

        Ok(length)
    }
}
