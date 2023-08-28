//! Buffer reading wrapper to provided a way to easily read data from
//! packet buffers provides easy functions for all the different tdf types

use super::{
    error::{DecodeError, DecodeResult},
    tag::{Tag, Tagged, TdfType},
    types::{TdfDeserialize, TdfDeserializeOwned, TdfTyped},
};
use crate::{tag::RawTag, types::map::deserialize_map_header};

/// Buffered readable implementation. Allows reading through the
/// underlying slice using a cursor and with a position that can
/// be saved using the marker. Provides functions for reading
/// certain data types in the Blaze format
pub struct TdfDeserializer<'de> {
    /// The underlying buffer to read from
    pub buffer: &'de [u8],
    /// The cursor position on the buffer. The cursor should not be set
    /// to any arbitry values should only be set to previously know values
    pub cursor: usize,
}

impl<'de> TdfDeserializer<'de> {
    pub fn new(buffer: &'de [u8]) -> Self {
        Self { buffer, cursor: 0 }
    }

    /// Reads all tags through the buffer until it encounters
    /// a matching tag returning the type of the tag or None
    fn internal_until_tag(&mut self, tag: &Tag) -> DecodeResult<Option<TdfType>> {
        while !self.is_empty() {
            let tagged = Tagged::deserialize_owned(self)?;

            // Tag matches return it
            if tagged.tag.eq(tag) {
                return Ok(Some(tagged.ty));
            }

            // Skip the tag and keep reading
            tagged.ty.skip(self)?;
        }

        Ok(None)
    }

    pub fn until_tag(&mut self, tag: RawTag, ty: TdfType) -> DecodeResult<()> {
        let tag = Tag::from(tag);
        let tag_ty = self
            .internal_until_tag(&tag)?
            .ok_or(DecodeError::MissingTag { tag, ty })?;

        if tag_ty != ty {
            return Err(DecodeError::InvalidTagType {
                tag,
                expected: ty,
                actual: tag_ty,
            });
        }

        Err(DecodeError::MissingTag { tag, ty })
    }

    pub fn try_until_tag(&mut self, tag: RawTag, ty: TdfType) -> DecodeResult<bool> {
        let tag = Tag::from(tag);
        let start = self.cursor;
        let exists = self
            .internal_until_tag(&tag)?
            .is_some_and(|value| value.eq(&ty));

        if !exists {
            self.cursor = start;
        }

        Ok(exists)
    }

    /// Reads the provided tag from the buffer discarding values until it
    /// reaches the correct value.
    ///
    /// `tag` The tag name to read
    pub fn tag<C>(&mut self, tag: RawTag) -> DecodeResult<C>
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
    pub fn try_tag<C>(&mut self, tag: RawTag) -> DecodeResult<Option<C>>
    where
        C: TdfDeserialize<'de> + TdfTyped,
    {
        let exists = self.try_until_tag(tag, C::TYPE)?;
        Ok(if exists {
            let value = C::deserialize(self)?;
            Some(value)
        } else {
            None
        })
    }

    pub fn until_list(&mut self, tag: RawTag, value_type: TdfType) -> DecodeResult<usize> {
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
        tag: RawTag,
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

    pub fn read_byte(&mut self) -> DecodeResult<u8> {
        self.expect_length(1)?;
        let byte: u8 = self.buffer[self.cursor];
        self.cursor += 1;
        Ok(byte)
    }

    pub fn read_bytes(&mut self, length: usize) -> DecodeResult<&'de [u8]> {
        self.expect_length(length)?;
        let slice: &[u8] = &self.buffer[self.cursor..self.cursor + length];
        self.cursor += length;
        Ok(slice)
    }

    /// Moves the cursor back 1 byte
    pub fn move_cursor_back(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
    }

    pub fn read_fixed<const S: usize>(&mut self) -> DecodeResult<[u8; S]> {
        let slice = self.read_bytes(S)?;

        // Copy the bytes into the new fixed size array
        let mut bytes: [u8; S] = [0u8; S];
        bytes.copy_from_slice(slice);

        Ok(bytes)
    }

    /// Skips the provided length in bytes on the underlying
    /// buffer returning an error if there is not enough space
    pub fn skip_length(&mut self, length: usize) -> DecodeResult<()> {
        self.expect_length(length)?;
        self.cursor += length;
        Ok(())
    }

    /// Skips the next tag value
    pub fn skip_tag(&mut self) -> DecodeResult<()> {
        let tag = Tagged::deserialize_owned(self)?;
        tag.ty.skip(self)
    }

    /// Returns the remaining length left after the cursor
    pub fn len(&self) -> usize {
        self.buffer.len() - self.cursor
    }

    /// Returns if there is nothing left after the cursor
    pub fn is_empty(&self) -> bool {
        self.cursor >= self.buffer.len()
    }
}
