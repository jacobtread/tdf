//! Deserialization and reading for the Tdf format
//!
//! This module provides a reader [TdfDeserializer] structure for reading
//! tags from a buffer.
//!
//! ## Creating a deserializer
//!
//! You can create a deserializer using [TdfDeserializer::new] on a slice of bytes
//!
//! ```
//! use tdf::reader::TdfDeserializer;
//!
//! let buffer = &[/* Example byte slice buffer */];
//! let mut r = TdfDeserializer::new(buffer);
//!
//! ```
//!
//! ## Basic Reading
//!
//! Unlike the [TdfSerializer](crate::writer::TdfSerializer) this deserializer doesn't provide
//! functions for deserializing specific types. Instead there is just one [tag](TdfDeserializer::tag)
//! function (and the other associated variants)
//!
//! ### Tag reading
//!
//! To read a deserializable value from the buffer you can use the [tag](TdfDeserializer::tag) function
//!
//! ```
//! use tdf::reader::TdfDeserializer;
//!
//! let buffer = &[/* Example byte slice buffer */];
//! let mut r = TdfDeserializer::new(buffer);
//!
//! let my_value: u32 = r.tag(b"TEST").unwrap();
//!
//! ```
//!
//! > **Note**
//! > Type annotates are important when deserializing values
//!
//! [tag](TdfDeserializer::tag) Will return an error if the specific tag is
//! not present in the buffer, see [try_tag](TdfDeserializer::try_tag) for tags
//! that might not be present
//!   
//! ### Group reading
//!
//! To read within groups yo can use the [group](TdfDeserializer::group) function
//!
//! ```
//! use tdf::reader::TdfDeserializer;
//!
//! let buffer = &[/* Example byte slice buffer */];
//! let mut r = TdfDeserializer::new(buffer);
//!
//! r.group(b"TEST", |_, r| {
//!     let tag: u32 = r.tag(b"INNR")?;
//!
//!     Ok(())
//! })
//! .unwrap();
//!
//! ```
//!
//! The group function takes in an action function which will be executed within
//! the reading context of the group (You can read values from the group but not values outside)
//!
//! The first argument to the group function is a boolean which indicates whether the group
//! was prefixed with a (2)
//!
//! The group function will return any values that the action function returns. The action
//! function accepts [FnMut] so it's also allowed to modify surrounding variables:
//!
//! ```
//! use tdf::reader::TdfDeserializer;
//!
//! let buffer = &[/* Example byte slice buffer */];
//! let mut r = TdfDeserializer::new(buffer);
//!
//! // Returning a value from the group
//! let value = r.group(b"TEST", |_, r| {
//!     let tag: u32 = r.tag(b"INNR")?;
//!
//!     Ok(tag)
//! })
//! .unwrap();
//!
//! // Returning multiple values from the group
//! let (a, b, c) = r.group(b"TEST", |_, r| {
//!     let a: u32 = r.tag(b"A")?;
//!     let b: u32 = r.tag(b"B")?;
//!     let c: u32 = r.tag(b"C")?;
//!
//!     Ok((a, b, c))
//! })
//! .unwrap();
//!
//! let mut my_var = 1;
//!
//! // Modifying variables from the group
//! r.group(b"TEST", |_, r| {
//!     my_var = r.tag(b"A")?;
//!
//!     Ok(())
//! })
//! .unwrap();
//! ```
//!
//! ### Optional tag reading
//!
//! Sometimes tags aren't always present in the serialized message, to handle tags that might not
//! always show up you can use the [try_tag](TdfDeserializer::try_tag) function
//!
//! ```
//! use tdf::reader::TdfDeserializer;
//!
//! let buffer = &[/* Example byte slice buffer */];
//! let mut r = TdfDeserializer::new(buffer);
//!
//! let my_value: Option<u32> = r.try_tag(b"TEST").unwrap();
//!
//! ```
//!
//! ## Complex Reading
//!
//! ### Reading complex lists
//!
//! When reading complex list elements instead of reading all the values you may want
//! to just read one value or do some complex operation. To do this you can instead
//! read the header of a list and then manually implement the value reading. You can
//! do this using [until_list](TdfDeserializer::until_list)
//!
//! ```
//! use tdf::reader::TdfDeserializer;
//!
//! let buffer = &[/* Example byte slice buffer */];
//! let mut r = TdfDeserializer::new(buffer);
//!
//! let length: usize = r.until_list(b"LIST").unwrap();
//!
//! for i in 0..length {
//!     // Read complex items
//! }
//!
//! // Dont forget to read ALL of the items or an error will occur
//! ```
//!
//! ### Reading complex maps
//!
//! When reading complex map elements instead of reading all the key values you may want
//! to just read one key value or do some complex operation. To do this you can instead
//! read the header of a map and then manually implement the value reading. You can
//! do this using [until_map](TdfDeserializer::until_map)
//!
//! ```
//! use tdf::reader::TdfDeserializer;
//!
//! let buffer = &[/* Example byte slice buffer */];
//! let mut r = TdfDeserializer::new(buffer);
//!
//! let (key_ty, value_ty, length): usize = r.until_map(b"LIST").unwrap();
//!
//! for i in 0..length {
//!     // Read key
//!     // Read value
//! }
//!
//! // Dont forget to read ALL of the items or an error will occur
//! ```
//!
//! The above until_map function doesn't check the types for the key and value instead it
//! provides them to you. If you would like to have the types be a specific type you can use
//! the [until_map_typed](TdfDeserializer::until_map_typed) function.
//!
//! ```
//! use tdf::reader::TdfDeserializer;
//! use tdf::tag::TdfType;
//!
//! let buffer = &[/* Example byte slice buffer */];
//! let mut r = TdfDeserializer::new(buffer);
//!
//! // Specify the key value types must be strings
//! let length: usize = r.until_map_typed(b"LIST", TdfType::String, TdfType::String).unwrap();
//!
//! for i in 0..length {
//!     // Read key
//!     // Read value
//! }
//!
//! // Dont forget to read ALL of the items or an error will occur
//! ```
//!
//! ### Until tag
//!
//! > **Warning**
//! > You shouldn't use until_tag or try_until_tag unless you have a clear understanding of the structure you're reading
//! > see the attached warning below for the reason
//!
//! For complex tags like TaggedUnions or if you just want to move the cursor to just after
//! a tag you can use the [until_tag](TdfDeserializer::until_tag) or the [try_until_tag](TdfDeserializer::try_until_tag)
//! to attempt to find that tag
//!
//! > `try_until_tag` will attempt to find the tag and if it fails to find the tag it will reset
//! > the cursor position and return false instead
//!
//! ```
//! use tdf::reader::TdfDeserializer;
//!
//! let buffer = &[/* Example byte slice buffer */];
//! let mut r = TdfDeserializer::new(buffer);
//!
//! r.until_tag(b"TEST").unwrap();
//! /* Operate on TEST */
//!
//! let exists = r.try_until_tag(b"BIN").unwrap();
//! if exists {
//!    /* Tag exists, operate on it */
//! }
//! ```
//!
//! > **Warning**
//! > When manully reading up to tags ensure that you correctly read all of the tag bytes
//! > or skip the value using [TdfType::skip] (If you have already started reading the structure this may not function correctly)
//! > if you don't completely read the tag structure the buffer will be unable to correctly finish reading

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
    /// Buffer storing the bytes to be deserialized
    pub(crate) buffer: &'de [u8],
    /// Cursor representing the current offset within the buffer
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
            if tagged.tag != tag {
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

    #[inline]
    pub fn until_map(&mut self, tag: RawTag) -> DecodeResult<(TdfType, TdfType, usize)> {
        self.until_tag(tag, TdfType::Map)?;
        deserialize_map_header(self)
    }

    pub fn until_map_typed(
        &mut self,
        tag: RawTag,
        key_type: TdfType,
        value_type: TdfType,
    ) -> DecodeResult<(TdfType, TdfType, usize)> {
        self.until_tag(tag, TdfType::Map)?;
        let (key_ty, value_ty, length) = self.until_map(tag)?;

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

        Ok((key_ty, value_ty, length))
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

    pub(crate) fn skip_length(&mut self, length: usize) -> DecodeResult<()> {
        self.expect_length(length)?;
        self.cursor += length;
        Ok(())
    }

    pub(crate) fn skip_tag(&mut self) -> DecodeResult<()> {
        let tag = Tagged::deserialize_owned(self)?;
        tag.ty.skip(self)
    }

    pub fn len(&self) -> usize {
        self.buffer.len() - self.cursor
    }

    pub fn is_empty(&self) -> bool {
        self.cursor >= self.buffer.len()
    }
}

#[cfg(test)]
mod test {
    use crate::writer::TdfSerializer;

    use super::TdfDeserializer;

    #[test]
    fn test_nested_struct() {
        // Create a nested structure to read
        let w = {
            let mut w = Vec::new();
            w.group(b"OUTR", |w| {
                w.tag_u32(b"VALU", 12);
                w.group(b"INNR", |w| {
                    w.tag_u32(b"GOAL", 34);
                });

                // Value shouldn't be reached
                w.tag_u32(b"NOPA", 50);
            });

            // Value shouldn't be reached
            w.tag_u32(b"NOPB", 24);

            w
        };

        let mut r = TdfDeserializer::new(&w);

        // NOPA shouldn't be accessible from this depth
        assert_eq!(r.try_tag::<u32>(b"NOPA").unwrap(), None);

        r.group(b"OUTR", |_, r| {
            let tes2: u32 = r.tag(b"VALU")?;
            assert_eq!(tes2, 12);

            r.group(b"INNR", |_, r| {
                let goal: u32 = r.tag(b"GOAL")?;
                assert_eq!(goal, 34);

                assert_eq!(r.try_tag::<u32>(b"NOPA").unwrap(), None);
                Ok(())
            })?;

            // NOPB shouldn't be accessible from this depth
            assert_eq!(r.try_tag::<u32>(b"NOPB").unwrap(), None);

            Ok(())
        })
        .unwrap();

        let nop2: u32 = r.tag(b"NOPB").unwrap();
        assert_eq!(nop2, 24);
    }
}
