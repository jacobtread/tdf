use super::{
    error::{DecodeError, DecodeResult},
    tag::{Tag, Tagged, TdfType},
    types::{TdfDeserialize, TdfDeserializeOwned, TdfTyped},
};
use crate::{
    tag::RawTag,
    types::{group::GroupSlice, map::deserialize_map_header},
};

pub struct TdfDeserializer<'de> {
    pub(crate) buffer: &'de [u8],
    pub(crate) cursor: usize,
    /// Group indentation counter, used to count depth for tag searching
    pub(crate) group: u8,
}

impl<'de> TdfDeserializer<'de> {
    pub fn new(buffer: &'de [u8]) -> Self {
        Self {
            buffer,
            cursor: 0,
            group: 0,
        }
    }

    pub fn until_tag(&mut self, tag: RawTag, ty: TdfType) -> DecodeResult<()> {
        let tag = Tag::from(tag);

        while !self.is_empty() {
            // Handle reaching the end of a group
            if self.group > 0 {
                let is_end = GroupSlice::deserialize_group_end(self)?;
                if is_end {
                    self.group -= 1;
                    break;
                }
            }

            let tagged = Tagged::deserialize_owned(self)?;

            // Skip tags that don't match
            if tagged.tag.ne(&tag) {
                tagged.ty.skip(self)?;
                continue;
            }

            // Handle mismatched types
            if tagged.ty != ty {
                return Err(DecodeError::InvalidTagType {
                    tag,
                    expected: ty,
                    actual: tagged.ty,
                });
            }

            return Ok(());
        }

        // Reached end of buffer without finding the tag
        Err(DecodeError::MissingTag { tag, ty })
    }

    pub fn try_until_tag(&mut self, tag: RawTag, ty: TdfType) -> DecodeResult<bool> {
        // Preserve initial state
        let start = self.cursor;
        let start_group = self.group;

        let exists = match self.until_tag(tag, ty) {
            Ok(_) => true,
            Err(DecodeError::MissingTag { .. }) => false,
            Err(err) => return Err(err),
        };

        // Reset to initial state
        if !exists {
            self.cursor = start;
            self.group = start_group;
        }

        Ok(exists)
    }

    pub fn tag<V>(&mut self, tag: RawTag) -> DecodeResult<V>
    where
        V: TdfDeserialize<'de> + TdfTyped,
    {
        self.until_tag(tag, V::TYPE)?;
        V::deserialize(self)
    }

    pub fn try_tag<V>(&mut self, tag: RawTag) -> DecodeResult<Option<V>>
    where
        V: TdfDeserialize<'de> + TdfTyped,
    {
        let exists = self.try_until_tag(tag, V::TYPE)?;
        Ok(if exists {
            let value = V::deserialize(self)?;
            Some(value)
        } else {
            None
        })
    }

    /// Attempts to find a group with the provided tag then runs the
    /// provided `action` on the group contents.
    #[inline]
    pub fn group<A, R>(&mut self, tag: RawTag, mut action: A) -> DecodeResult<R>
    where
        A: FnMut(bool, &mut Self) -> DecodeResult<R>,
    {
        self.until_tag(tag, TdfType::Group)?;
        self.group += 1;
        let is_two = GroupSlice::deserialize_prefix_two(self)?;

        let value = action(is_two, self)?;

        // Deserialize any remaining group content
        GroupSlice::deserialize_content_skip(self)?;

        self.group -= 1;
        Ok(value)
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

    pub(crate) fn read_byte(&mut self) -> DecodeResult<u8> {
        self.expect_length(1)?;
        let byte: u8 = self.buffer[self.cursor];
        self.cursor += 1;
        Ok(byte)
    }

    pub(crate) fn read_bytes(&mut self, length: usize) -> DecodeResult<&'de [u8]> {
        self.expect_length(length)?;
        let slice: &[u8] = &self.buffer[self.cursor..self.cursor + length];
        self.cursor += length;
        Ok(slice)
    }

    /// Moves the cursor back 1 byte
    pub(crate) fn move_cursor_back(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
    }

    pub(crate) fn read_fixed<const S: usize>(&mut self) -> DecodeResult<[u8; S]> {
        let slice = self.read_bytes(S)?;

        // Copy the bytes into the new fixed size array
        let mut bytes: [u8; S] = [0u8; S];
        bytes.copy_from_slice(slice);

        Ok(bytes)
    }

    /// Skips the provided length in bytes on the underlying
    /// buffer returning an error if there is not enough space
    pub(crate) fn skip_length(&mut self, length: usize) -> DecodeResult<()> {
        self.expect_length(length)?;
        self.cursor += length;
        Ok(())
    }

    /// Skips the next tag value
    pub(crate) fn skip_tag(&mut self) -> DecodeResult<()> {
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
