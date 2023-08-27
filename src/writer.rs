//! Writer buffer implementation for writing different kinds of tdf values
//! to byte form without creating a new structure [`TdfWriter`]

use crate::{codec::TdfSerializeOwned, types::TaggedUnion};

use super::{
    codec::{TdfSerialize, TdfTyped},
    tag::TdfType,
};

/// Writer implementation for writing values to an underlying buffer
/// this writer implementation provides functions for writing certain
/// data types in their Blaze format
#[derive(Default)]
pub struct TdfSerializer {
    /// The buffer that will be written to
    pub buffer: Vec<u8>,
}

impl TdfSerializer {
    /// Writes a single byte to the underlying buffer. This just
    /// appends the byte to the buffer.
    ///
    /// `value` The value to write
    #[inline]
    pub fn write_byte(&mut self, value: u8) {
        self.buffer.push(value)
    }

    /// Extends the underlying buffer with the provided slice
    /// value.
    ///
    /// `value` The slice value to write
    #[inline]
    pub fn write_slice(&mut self, value: &[u8]) {
        self.buffer.extend_from_slice(value);
    }

    /// Writes the value type byte of the provided TdfType
    ///
    /// `ty` The type to write
    #[inline]
    pub fn write_type(&mut self, ty: TdfType) {
        self.write_byte(ty as u8);
    }

    /// Writes a tag vvalue to the underlying buffer
    ///
    /// `tag`        The tag bytes to write
    /// `value_type` The value type for the tag
    pub fn tag(&mut self, tag: &[u8], value_type: TdfType) {
        let mut output: [u8; 4] = [0, 0, 0, value_type as u8];
        let length: usize = tag.len();
        if length > 0 {
            output[0] |= (tag[0] & 0x40) << 1;
            output[0] |= (tag[0] & 0x10) << 2;
            output[0] |= (tag[0] & 0x0F) << 2;
        }
        if length > 1 {
            output[0] |= (tag[1] & 0x40) >> 5;
            output[0] |= (tag[1] & 0x10) >> 4;
            output[1] |= (tag[1] & 0x0F) << 4;
        }
        if length > 2 {
            output[1] |= (tag[2] & 0x40) >> 3;
            output[1] |= (tag[2] & 0x10) >> 2;
            output[1] |= (tag[2] & 0x0C) >> 2;
            output[2] |= (tag[2] & 0x03) << 6;
        }
        if length > 3 {
            output[2] |= (tag[3] & 0x40) >> 1;
            output[2] |= tag[3] & 0x1F;
        }
        self.buffer.extend_from_slice(&output);
    }

    /// Writes a new tag to the buffer with a boolean as the
    /// tag value.
    ///
    /// `tag`   The tag to write
    /// `value` The tag value boolean
    pub fn tag_bool(&mut self, tag: &[u8], value: bool) {
        self.tag(tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    /// Writes a new tag where the value is a VarInt that is
    /// simply zero so the encoding can skip all the var int
    /// logic and directly write zero
    ///
    /// `tag` The tag to write
    pub fn tag_zero(&mut self, tag: &[u8]) {
        self.tag(tag, TdfType::VarInt);
        self.write_byte(0);
    }

    /// Writes a new tag where the value is a u8 value using
    /// the var int encoding
    ///
    /// `tag`   The tag to write
    /// `value` The value to write
    pub fn tag_u8(&mut self, tag: &[u8], value: u8) {
        self.tag(tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    /// Writes a new tag where the value is a u16 value using
    /// the var int encoding
    ///
    /// `tag`   The tag to write
    /// `value` The value to write
    pub fn tag_u16(&mut self, tag: &[u8], value: u16) {
        self.tag(tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    /// Writes a new tag where the value is a u32 value using
    /// the var int encoding
    ///
    /// `tag`   The tag to write
    /// `value` The value to write
    pub fn tag_u32(&mut self, tag: &[u8], value: u32) {
        self.tag(tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    /// Writes a new tag where the value is a u64 value using
    /// the var int encoding
    ///
    /// `tag`   The tag to write
    /// `value` The value to write
    pub fn tag_u64(&mut self, tag: &[u8], value: u64) {
        self.tag(tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    /// Writes a new tag where the value is a usize value using
    /// the var int encoding
    ///
    /// `tag`   The tag to write
    /// `value` The value to write
    pub fn tag_usize(&mut self, tag: &[u8], value: usize) {
        self.tag(tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    /// Writes a new tag where the value is an empty string
    ///
    /// `tag` The tag to write
    pub fn tag_str_empty(&mut self, tag: &[u8]) {
        self.tag(tag, TdfType::String);
        self.write_empty_str();
    }

    /// Writes a new tag where the value is an empty blob.
    /// Empty blobs are simply encoded with a zero length
    ///
    /// `tag` The tag to write
    pub fn tag_empty_blob(&mut self, tag: &[u8]) {
        self.tag(tag, TdfType::Blob);
        self.buffer.push(0);
    }

    /// Writes a new tag where the value is a string.
    ///
    /// `tag`   The tag to write
    /// `value` The value to write
    pub fn tag_str(&mut self, tag: &[u8], value: &str) {
        self.tag(tag, TdfType::String);
        self.write_str(value)
    }

    /// Writes a new tag indicating the start of a new group
    ///
    /// `tag` The tag to write
    pub fn tag_group(&mut self, tag: &[u8]) {
        self.tag(tag, TdfType::Group);
    }

    /// Writes the zero value that indicates the end of a group
    pub fn tag_group_end(&mut self) {
        self.buffer.push(0);
    }

    /// Writes a group opening tag and then completes the group function
    /// and closes the group tag
    ///
    /// `tag` The tag to write
    /// `gr`  The group closure
    #[inline]
    pub fn group<F>(&mut self, tag: &[u8], gr: F)
    where
        F: FnOnce(&mut Self),
    {
        self.tag_group(tag);
        gr(self);
        self.tag_group_end();
    }

    /// Writes a new tag indicating that a list is begining and writes the list
    /// type and length
    ///
    /// `tag`    The tag to write
    /// `ty`     The type of items being written after
    /// `length` The number of items that will be written
    pub fn tag_list_start(&mut self, tag: &[u8], ty: TdfType, length: usize) {
        self.tag(tag, TdfType::List);
        self.write_type(ty);
        length.serialize_owned(self);
    }

    /// Writes a new tag indicating that a union with the provided key is
    /// starting
    ///
    /// `tag` The tag to write
    /// `key` The key to write
    pub fn tag_union_start(&mut self, tag: &[u8], key: u8) {
        self.tag(tag, TdfType::TaggedUnion);
        self.buffer.push(key);
    }

    /// Writes a new union tag with its value
    ///
    /// `tag`       The tag to write
    /// `key`       The key of the union
    /// `value_tag` The tag for the value
    /// `value`     The value to write
    pub fn tag_union_value<C: TdfSerialize + TdfTyped>(
        &mut self,
        tag: &[u8],
        key: u8,
        value_tag: &[u8],
        value: &C,
    ) {
        self.tag_union_start(tag, key);
        self.tag(value_tag, C::TYPE);
        value.serialize(self);
    }

    /// Writes a new tag indicating a union with no value
    ///
    /// `tag` The tag to write
    pub fn tag_union_unset(&mut self, tag: &[u8]) {
        self.tag_union_start(tag, TaggedUnion::<()>::UNSET_KEY);
    }

    /// Writes a tag and its value where the value implements ValueType
    ///
    /// `tag`   The tag to write
    /// `value` The value to write
    pub fn tag_value<C: TdfSerialize + TdfTyped>(&mut self, tag: &[u8], value: &C) {
        self.tag(tag, C::TYPE);
        value.serialize(self);
    }

    /// Writes a tag for indiciating a list with no contents
    ///
    /// `tag` The tag to write
    /// `ty`  The type of the empty list
    pub fn tag_list_empty(&mut self, tag: &[u8], ty: TdfType) {
        self.tag(tag, TdfType::List);
        self.write_type(ty);
        self.buffer.push(0);
    }

    /// Slices are already borrowed so they confuse the `tag_value` type using this
    /// function instead makes them work
    pub fn tag_slice_list<C: TdfSerialize + TdfTyped>(&mut self, tag: &[u8], value: &[C]) {
        self.tag(tag, TdfType::List);
        value.serialize(self);
    }

    /// Writes a tag for indiciating a var int list with no contents
    ///
    /// `tag` The tag to write
    pub fn tag_var_int_list_empty(&mut self, tag: &[u8]) {
        self.tag(tag, TdfType::VarIntList);
        self.buffer.push(0);
    }

    /// Writes a tag indicating that a map will be written for the
    /// provided types and length
    ///
    /// `tag`    The tag to write
    /// `key`    The key tdf type
    /// `value`  The value tdf type
    /// `length` The total number of entires that will be written
    pub fn tag_map_start(&mut self, tag: &[u8], key: TdfType, value: TdfType, length: usize) {
        self.tag(tag, TdfType::Map);
        self.write_type(key);
        self.write_type(value);
        length.serialize_owned(self);
    }

    /// Writes a list of tuples as a map of key value paris
    ///
    /// `tag`    The tag to write
    /// `values` The tuples of key value pairs to write
    pub fn tag_map_tuples<K, V>(&mut self, tag: &[u8], values: &[(K, V)])
    where
        K: TdfSerialize + TdfTyped,
        V: TdfSerialize + TdfTyped,
    {
        self.tag_map_start(tag, K::TYPE, V::TYPE, values.len());
        for (key, value) in values {
            key.serialize(self);
            value.serialize(self);
        }
    }

    pub fn tag_object_type(&mut self, tag: &[u8], component: u16, ty: u16) {
        self.tag(tag, TdfType::ObjectType);
        component.serialize_owned(self);
        ty.serialize_owned(self);
    }

    pub fn tag_object_id(&mut self, tag: &[u8], component: u16, ty: u16, id: u64) {
        self.tag(tag, TdfType::ObjectId);
        component.serialize_owned(self);
        ty.serialize_owned(self);
        id.serialize_owned(self);
    }

    /// Writes an empty string. This is simply two bytes a 1 and a 0 which
    /// indicate a string consisting of only a null terminator
    pub fn write_empty_str(&mut self) {
        self.buffer.extend_from_slice(&[1, 0])
    }

    /// Writes a string to the underlying buffer. The bytes
    /// are encoded an a null terminator is appended to the
    /// end then the size and bytes are written to the buffer
    ///
    /// `value` The string value to write
    pub fn write_str(&mut self, value: &str) {
        let mut bytes = value.as_bytes().to_vec();
        match bytes.last() {
            // Ignore if already null terminated
            Some(0) => {}
            // Null terminate
            _ => bytes.push(0),
        }

        bytes.len().serialize_owned(self);
        self.write_slice(&bytes);
    }

    /// Writes the header for a map in order to begin writing map values
    ///
    /// `key_type`   The type of the map keys
    /// `value_type` The type of the map values
    /// `length`     The total number of items that will be written
    pub fn write_map_header(&mut self, key_type: TdfType, value_type: TdfType, length: usize) {
        self.write_type(key_type);
        self.write_type(value_type);
        length.serialize_owned(self);
    }

    /// Clears the contents of the underlying buffer
    pub fn clear(&mut self) {
        self.buffer.clear();
    }
}

/// Implementation for converting tdf writer into its underlying buffer with from
impl From<TdfSerializer> for Vec<u8> {
    fn from(value: TdfSerializer) -> Self {
        value.buffer
    }
}
