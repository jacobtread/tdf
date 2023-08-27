//! Writer buffer implementation for writing different kinds of tdf values
//! to byte form without creating a new structure [`TdfWriter`]

use crate::{
    codec::TdfSerializeOwned,
    tag::{RawTag, Tagged},
    types::{
        list::serialize_list_header, map::serialize_map_header, string::write_empty_str, Blob,
        TaggedUnion,
    },
};

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
    /// Serializes the provided tag and value from a
    /// reference.
    ///
    /// This function should be used when serializing structures
    /// for strings, and slices use [TdfSerializer::tag_alt] and for primitives
    /// types use [TdfSerializer::tag_owned]
    ///
    /// TODO: Update code example to include a structure instead
    ///
    /// ```
    /// use tdf::writer::TdfSerializer;
    ///
    /// let mut w = TdfSerializer::default();
    /// w.tag_ref(b"TEST", &1);
    ///
    /// ```
    ///
    /// # Arguments
    /// * tag - The tag to use for this field
    /// * value - The value to serialize
    pub fn tag_ref<V>(&mut self, tag: RawTag, value: &V)
    where
        V: TdfSerialize + TdfTyped,
    {
        Tagged::serialize_raw(self, tag, V::TYPE);
        value.serialize(self);
    }

    /// Serializes the provided tag and value
    ///
    /// This function should be used when serializing strings, and slices
    /// use [TdfSerializer::tag+ref] for structures and for primitives
    /// types use [TdfSerializer::tag_owned] (or the respective tag_{type} function)
    ///
    /// ```
    /// use tdf::writer::TdfSerializer;
    ///
    /// let mut w = TdfSerializer::default();
    /// let test_slice: &[u8] = &[0,5,12,23,255];
    ///
    /// w.tag_alt(b"TEST", "Example value");
    /// w.tag_alt(b"TEST", test_slice);
    /// ```
    ///
    /// # Arguments
    /// * tag - The tag to use for this field
    /// * value - The value to serialize
    pub fn tag_alt<V>(&mut self, tag: RawTag, value: V)
    where
        V: TdfSerialize + TdfTyped,
    {
        Tagged::serialize_raw(self, tag, V::TYPE);
        value.serialize(self);
    }

    /// Tags a value using its serialize owned value. This extra
    /// tagging value prevents extra copying that the normal
    /// serialize does for primitives
    ///
    /// This function should be used for writing primitive values such
    /// as boolean, integers, and floats:
    ///
    /// Alternatively when hard coding values you can use the tag_{ty}
    /// functions to tag a specific value type
    ///
    /// ```
    /// use tdf::writer::TdfSerializer;
    ///
    /// let mut w = TdfSerializer::default();
    ///
    /// w.tag_owned(b"TEST", false);
    /// w.tag_owned(b"TEST", 123u8);
    /// w.tag_owned(b"TEST", 123u64);
    /// ```
    ///
    /// # Arguments
    /// * tag - The tag to use for this field
    /// * value - The owned value to serialize
    pub fn tag_owned<V>(&mut self, tag: RawTag, value: V)
    where
        V: TdfSerializeOwned + TdfTyped,
    {
        Tagged::serialize_raw(self, tag, V::TYPE);
        value.serialize_owned(self);
    }

    // Primitive integer tagging

    pub fn tag_zero(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::VarInt);
        self.write_byte(0);
    }

    #[inline]
    pub fn tag_bool(&mut self, tag: RawTag, value: bool) {
        self.tag_owned(tag, value);
    }

    #[inline]
    pub fn tag_u8(&mut self, tag: RawTag, value: u8) {
        self.tag_owned(tag, value);
    }

    #[inline]
    pub fn tag_u16(&mut self, tag: RawTag, value: u16) {
        self.tag_owned(tag, value);
    }

    #[inline]
    pub fn tag_u32(&mut self, tag: RawTag, value: u32) {
        self.tag_owned(tag, value);
    }

    #[inline]
    pub fn tag_u64(&mut self, tag: RawTag, value: u64) {
        self.tag_owned(tag, value);
    }

    #[inline]
    pub fn tag_usize(&mut self, tag: RawTag, value: usize) {
        self.tag_owned(tag, value);
    }

    // String tagging

    pub fn tag_str_empty(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::String);
        write_empty_str(self);
    }

    #[inline]
    pub fn tag_str(&mut self, tag: RawTag, value: &str) {
        self.tag_alt(tag, value);
    }

    // Blob tagging

    pub fn tag_empty_blob(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::Blob);
        self.write_byte(0);
    }

    pub fn tag_blob(&mut self, tag: RawTag, blob: &[u8]) {
        Tagged::serialize_raw(self, tag, TdfType::Blob);
        Blob::serialize_raw(self, blob);
    }

    // Group tagging

    #[inline]
    pub fn tag_group(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::Group);
    }

    #[inline]
    pub fn tag_group_end(&mut self) {
        self.write_byte(0);
    }

    #[inline]
    pub fn group<F>(&mut self, tag: RawTag, gr: F)
    where
        F: FnOnce(&mut Self),
    {
        self.tag_group(tag);
        gr(self);
        self.tag_group_end();
    }

    // List tagging

    pub fn tag_list_start(&mut self, tag: RawTag, ty: TdfType, length: usize) {
        Tagged::serialize_raw(self, tag, TdfType::List);
        serialize_list_header(self, ty, length);
    }

    #[inline]
    pub fn tag_list_empty(&mut self, tag: RawTag, ty: TdfType) {
        self.tag_list_start(tag, ty, 0);
    }

    #[inline]
    pub fn tag_list_slice<V>(&mut self, tag: RawTag, value: &[V])
    where
        V: TdfSerialize + TdfTyped,
    {
        self.tag_alt(tag, value);
    }

    // Union tagging

    pub fn tag_union_start(&mut self, tag: RawTag, key: u8) {
        Tagged::serialize_raw(self, tag, TdfType::TaggedUnion);
        self.write_byte(key);
    }

    #[inline]
    pub fn tag_union_value<C>(&mut self, tag: RawTag, key: u8, value_tag: RawTag, value: &C)
    where
        C: TdfSerialize + TdfTyped,
    {
        self.tag_union_start(tag, key);
        self.tag_ref(value_tag, value);
    }

    #[inline]
    pub fn tag_union_unset(&mut self, tag: RawTag) {
        self.tag_union_start(tag, TaggedUnion::<()>::UNSET_KEY);
    }

    // Var int list tagging

    pub fn tag_var_int_list_empty(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::VarIntList);
        self.write_byte(0);
    }

    // Map tagging

    pub fn tag_map_start(&mut self, tag: RawTag, key: TdfType, value: TdfType, length: usize) {
        Tagged::serialize_raw(self, tag, TdfType::Map);
        serialize_map_header(self, key, value, length);
    }

    pub fn tag_map_tuples<K, V>(&mut self, tag: RawTag, values: &[(K, V)])
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

    #[inline]
    pub(crate) fn write_byte(&mut self, value: u8) {
        self.buffer.push(value)
    }

    #[inline]
    pub(crate) fn write_slice(&mut self, value: &[u8]) {
        self.buffer.extend_from_slice(value);
    }

    #[inline]
    pub fn clear(&mut self) {
        self.buffer.clear();
    }
}

impl From<TdfSerializer> for Vec<u8> {
    fn from(value: TdfSerializer) -> Self {
        value.buffer
    }
}
