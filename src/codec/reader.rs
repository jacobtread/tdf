//! Buffer reading wrapper to provided a way to easily read data from
//! packet buffers provides easy functions for all the different tdf types

use crate::{
    codec::{Decodable, ValueType},
    error::{DecodeError, DecodeResult},
    prelude::{Blob, ObjectId, ObjectType, SkipType, TdfMapHeader, Union, VarIntList, U12},
    tag::{Tag, Tagged, TdfType},
    types::TdfMap,
};

/// Buffered readable implementation. Allows reading through the
/// underlying slice using a cursor and with a position that can
/// be saved using the marker. Provides functions for reading
/// certain data types in the Blaze format
pub struct TdfReader<'a> {
    /// The underlying buffer to read from
    pub buffer: &'a [u8],
    /// The cursor position on the buffer. The cursor should not be set
    /// to any arbitry values should only be set to previously know values
    pub cursor: usize,
}

impl<'a> TdfReader<'a> {
    /// Creates a new reader over the provided slice of bytes with
    /// the default cursor position at zero
    pub fn new(buffer: &'a [u8]) -> Self {
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

    pub fn read_bytes<const N: usize>(&mut self) -> DecodeResult<[u8; N]> {
        // Ensure we have the required number of bytes
        self.expect_length(N)?;
        // Alocate and copy the bytes from the buffer
        let mut bytes: [u8; N] = [0; N];
        bytes.copy_from_slice(&self.buffer[self.cursor..self.cursor + N]);
        // Move the cursor
        self.cursor += N;
        Ok(bytes)
    }

    /// Takes a slice of the provided length from the portion of the
    /// buffer that is after the cursor position
    ///
    /// `length` The length of the slice to take
    pub fn read_slice(&mut self, length: usize) -> DecodeResult<&[u8]> {
        // Ensure we have the required number of bytes
        if self.cursor + length > self.buffer.len() {
            return Err(DecodeError::UnexpectedEof {
                cursor: self.cursor,
                wanted: length,
                remaining: self.len(),
            });
        }
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

    /// Reads a blob from the buffer. The blob is a slice prefixed
    /// by a length value
    pub fn read_blob(&mut self) -> DecodeResult<&[u8]> {
        let length: usize = usize::decode(self)?;
        let bytes: &[u8] = self.read_slice(length)?;
        Ok(bytes)
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
            let next_tag = match Tagged::decode(self) {
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

        while let Ok(next_tag) = Tagged::decode(self) {
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
    pub fn tag<C: Decodable + ValueType>(&mut self, tag: &[u8]) -> DecodeResult<C> {
        self.until_tag(tag, C::TYPE)?;
        C::decode(self)
    }

    /// Reads the provided tag from the buffer discarding values until it
    /// reaches the correct value. If the tag is missing the cursor is reset
    /// back to where it was
    ///
    /// `tag` The tag name to read
    pub fn try_tag<C: Decodable + ValueType>(&mut self, tag: &[u8]) -> DecodeResult<Option<C>> {
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

    /// Skips the next tag value
    pub fn skip(&mut self) -> DecodeResult<()> {
        let tag = Tagged::decode(self)?;
        self.skip_type(&tag.ty)
    }

    /// Skips a data type
    ///
    /// `ty` The type of data to skip
    pub fn skip_type(&mut self, ty: &TdfType) -> DecodeResult<()> {
        match ty {
            TdfType::VarInt => u64::skip(self)?,
            TdfType::String | TdfType::Blob => Blob::skip(self)?,
            TdfType::Group => self.skip_group()?,
            TdfType::List => Vec::<SkipType>::skip(self)?,
            TdfType::Map => TdfMap::<SkipType, SkipType>::skip(self)?,
            TdfType::TaggedUnion => Union::<SkipType>::skip(self)?,
            TdfType::VarIntList => VarIntList::skip(self)?,
            TdfType::ObjectType => ObjectType::skip(self)?,
            TdfType::ObjectId => ObjectId::skip(self)?,
            TdfType::Float => f32::skip(self)?,
            TdfType::U12 => U12::skip(self)?,
        }
        Ok(())
    }

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

    /// Skips a list of items
    pub fn skip_list(&mut self) -> DecodeResult<()> {
        let ty: TdfType = TdfType::decode(self)?;
        let length: usize = usize::decode(self)?;
        for _ in 0..length {
            self.skip_type(&ty)?;
        }
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
        let list_type = TdfType::decode(self)?;
        if list_type != value_type {
            return Err(DecodeError::InvalidType {
                expected: value_type,
                actual: list_type,
            });
        }
        let count = usize::decode(self)?;
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

        let header = TdfMapHeader::decode(self)?;

        if header.key != key_type {
            return Err(DecodeError::InvalidType {
                expected: key_type,
                actual: header.key,
            });
        }

        if header.value != value_type {
            return Err(DecodeError::InvalidType {
                expected: value_type,
                actual: header.value,
            });
        }

        Ok(header.length)
    }
}
