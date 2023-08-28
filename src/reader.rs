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
    group::GroupSlice,
    list::skip_list,
    map::{deserialize_map_header, skip_map},
    tagged_union::skip_tagged_union,
    var_int::skip_var_int,
    ObjectId, ObjectType, VarIntList,
};

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

    /// Skips the next tag value
    pub fn skip_tag(&mut self) -> DecodeResult<()> {
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
            TdfType::Group => GroupSlice::skip(self)?,
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

    /// Returns the remaining length left after the cursor
    pub fn len(&self) -> usize {
        self.buffer.len() - self.cursor
    }

    /// Returns if there is nothing left after the cursor
    pub fn is_empty(&self) -> bool {
        self.cursor >= self.buffer.len()
    }
}
