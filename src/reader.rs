//! Buffer reading wrapper to provided a way to easily read data from
//! packet buffers provides easy functions for all the different tdf types

use super::{
    codec::{TdfDeserialize, TdfDeserializeOwned, TdfTyped},
    error::{DecodeError, DecodeResult},
    tag::{Tag, Tagged, TdfType},
};
use crate::types::{map::deserialize_map_header, Blob, ObjectId, ObjectType, TaggedUnion, TdfMap};

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
            let next_tag = match self.read_tag() {
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

        while let Ok(next_tag) = self.read_tag() {
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

    /// Reads a tag from the underlying buffer
    pub fn read_tag(&mut self) -> DecodeResult<Tagged> {
        let input: [u8; 4] = self.read_bytes()?;
        let ty: TdfType = TdfType::try_from(input[3])?;
        let mut output: [u8; 4] = [0, 0, 0, 0];

        output[0] |= (input[0] & 0x80) >> 1;
        output[0] |= (input[0] & 0x40) >> 2;
        output[0] |= (input[0] & 0x30) >> 2;
        output[0] |= (input[0] & 0x0C) >> 2;

        output[1] |= (input[0] & 0x02) << 5;
        output[1] |= (input[0] & 0x01) << 4;
        output[1] |= (input[1] & 0xF0) >> 4;

        output[2] |= (input[1] & 0x08) << 3;
        output[2] |= (input[1] & 0x04) << 2;
        output[2] |= (input[1] & 0x03) << 2;
        output[2] |= (input[2] & 0xC0) >> 6;

        output[3] |= (input[2] & 0x20) << 1;
        output[3] |= input[2] & 0x1F;

        let tag = Tag(output);

        Ok(Tagged { tag, ty })
    }

    /// Skips the 4 bytes required for a 32 bit float value
    pub fn skip_f32(&mut self) -> DecodeResult<()> {
        self.expect_length(4)?;
        self.cursor += 4;
        Ok(())
    }

    /// Skips the next string value
    pub fn skip_blob(&mut self) -> DecodeResult<()> {
        let length: usize = usize::deserialize_owned(self)?;
        self.expect_length(length)?;
        self.cursor += length;
        Ok(())
    }

    /// Skips the next var int value
    pub fn skip_var_int(&mut self) -> DecodeResult<()> {
        let mut byte = self.read_byte()?;
        while byte >= 0x80 {
            byte = self.read_byte()?;
        }
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

    /// Skips a list of items
    pub fn skip_list(&mut self) -> DecodeResult<()> {
        let ty: TdfType = TdfType::deserialize_owned(self)?;
        let length: usize = usize::deserialize_owned(self)?;
        for _ in 0..length {
            self.skip_type(&ty)?;
        }
        Ok(())
    }

    /// Skips a map
    pub fn skip_map(&mut self) -> DecodeResult<()> {
        let (key_ty, value_ty, length) = deserialize_map_header(self)?;
        for _ in 0..length {
            self.skip_type(&key_ty)?;
            self.skip_type(&value_ty)?;
        }
        Ok(())
    }

    /// Skips a union value
    pub fn skip_union(&mut self) -> DecodeResult<()> {
        let ty = self.read_byte()?;
        if ty != TaggedUnion::<()>::UNSET_KEY {
            self.skip()?;
        }
        Ok(())
    }

    /// Skips a var int list
    pub fn skip_var_int_list(&mut self) -> DecodeResult<()> {
        let length: usize = usize::deserialize_owned(self)?;
        for _ in 0..length {
            self.skip_var_int()?;
        }
        Ok(())
    }

    /// Skips the next tag value
    pub fn skip(&mut self) -> DecodeResult<()> {
        let tag = self.read_tag()?;
        self.skip_type(&tag.ty)
    }

    /// Skips a data type
    ///
    /// `ty` The type of data to skip
    pub fn skip_type(&mut self, ty: &TdfType) -> DecodeResult<()> {
        match ty {
            TdfType::VarInt => self.skip_var_int()?,
            TdfType::String | TdfType::Blob => self.skip_blob()?,
            TdfType::Group => self.skip_group()?,
            TdfType::List => self.skip_list()?,
            TdfType::Map => self.skip_map()?,
            TdfType::TaggedUnion => self.skip_union()?,
            TdfType::VarIntList => self.skip_var_int_list()?,
            TdfType::ObjectType => {
                self.skip_var_int()?;
                self.skip_var_int()?;
            }
            TdfType::ObjectId => {
                self.skip_var_int()?;
                self.skip_var_int()?;
                self.skip_var_int()?;
            }
            TdfType::Float => self.skip_f32()?,
            TdfType::U12 => {
                self.read_slice(8)?;
                self.skip_blob()?; // string
            }
        }
        Ok(())
    }

    /// Decodes all the contents within the reader into a string
    /// representation
    ///
    /// `out` The string output to append to
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

    /// Decodes and converts the next tag into
    /// a string representation
    ///
    /// `out`    The string output to append to
    /// `indent` The current indent level
    pub fn stringify_tag(&mut self, out: &mut String, indent: usize) -> DecodeResult<()> {
        let tag = self.read_tag()?;
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

    /// Decodes and converts the next value of the provided type
    /// into a string representation
    ///
    /// `out`    The string output to append to
    /// `indent` The current indent level
    /// `ty`     The type
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
                if ty == TaggedUnion::<()>::UNSET_KEY {
                    out.push_str("Union(Unset)")
                } else {
                    let tag = self.read_tag()?;
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

    /// Reads until the next list values selection for the provided
    /// tag. Will read the value type and the length returning
    /// the length.
    ///
    /// `tag`        The tag to read
    /// `value_type` The expected value type
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

    /// Reads until the next map values selection for the provided
    /// tag. Will read the key value types and the length returning
    /// the length.
    ///
    /// `tag`        The tag to read
    /// `key_type`   The expected key type
    /// `value_type` The expected value type
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

/// Majority of reading tests are merged into the writing tests
#[cfg(test)]
mod test {
    use super::TdfDeserializer;

    /// Tests reading a byte from the reader
    #[test]
    fn test_read_byte() {
        for value in 0..255 {
            let buffer = &[value];
            let mut reader = TdfDeserializer::new(buffer);
            let read_value = reader.read_byte().unwrap();
            assert_eq!(value, read_value);
        }
    }
}
