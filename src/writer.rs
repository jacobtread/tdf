//! Serialization and writing for the Tdf format.
//!
//! This module provides a writer [TdfSerializer] structure for writing tags
//! and raw/special values to a buffer for serialization.
//!
//! ## Tagging
//!
//! ### Structures
//!
//! When writing tagged structures you provide the by reference to the
//! [TdfSerializer::tag_ref] function:
//!
//! ```
//! use tdf::prelude::*;
//!
//! #[derive(TdfSerialize, TdfTyped)]
//! #[tdf(group)]
//! struct Example {
//!     #[tdf(tag = "TEST")]
//!     field: u32
//! }
//!
//! let mut w = Vec::new();
//! let test = Example { field: 12 };
//! w.tag_ref(b"TEST", &test);
//!
//! ```
//!
//! ### Tagging strings, and slices
//! When tagging strings and slices you should use the [TdfSerializer::tag_alt]
//! function:
//!
//! ```
//! use tdf::writer::TdfSerializer;
//!
//! let mut w = Vec::new();
//!
//! w.tag_owned(b"TEST", false);
//! w.tag_owned(b"TEST", 123u8);
//! w.tag_owned(b"TEST", 123u64);
//! ```
//!
//! ### Tagging primitives / owned values
//!
//! When tagging values like primitives you should use [TdfSerializer::tag_owned]
//! or one the the [Dedicated Tagging](#dedicated-tagging) functions provided
//!
//! ```
//! use tdf::writer::TdfSerializer;
//!
//! let mut w = Vec::new();
//!
//! w.tag_owned(b"TEST", false);
//! w.tag_owned(b"TEST", 123u8);
//! w.tag_owned(b"TEST", 123u64);
//! ```
//!
//! > When you are hardcoding values like the above `123u8` and `123u64` number values you
//! > should instead use the [Dedicated Tagging](#dedicated-tagging) functions so that you
//! > can omit the type
//!
//! ## Dedicated Tagging
//!
//! Below are dedicated tagging functions for specific value types or circumstances. These
//! are most useful when hardcoding in primitve values or typed values without having to
//! specify the type
//!
//! ### Integer Functions
//!
//! * [tag_zero](TdfSerializer::tag_zero) - Special function for writing a zero integer value
//! * [tag_bool](TdfSerializer::tag_bool) - Function for tagging boolean values
//! * [tag_u8](TdfSerializer::tag_u8) - Function for tagging unsigned 8 bit integers
//! * [tag_u16](TdfSerializer::tag_u16) - Function for tagging unsigned 16 bit integers
//! * [tag_u32](TdfSerializer::tag_u32) - Function for tagging unsigned 32 bit integers
//! * [tag_u64](TdfSerializer::tag_u64) - Function for tagging unsigned 64 bit integers
//! * [tag_usize](TdfSerializer::tag_usize) - Function for tagging usize integers
//!
//! ### String Functions
//!
//! * [tag_str_empty](TdfSerializer::tag_str_empty) Special function for writing an empty string
//! * [tag_str](TdfSerializer::tag_str) Function for tagging a string slice
//!
//! ### Blob Functions
//!
//! * [tag_blob_empty](TdfSerializer::tag_blob_empty) Special function for tagging an empty blob value
//! * [tag_blob](TdfSerializer::tag_blob) Function for tagging blob values directly from a byte slice
//!
//! ### List Functions
//!
//! Tagging functions for writing list types
//!
//! * [tag_list_start](TdfSerializer::tag_list_start) - Special function for writing a list header (Used to manually write list impl for complex types)
//! * [tag_list_empty](TdfSerializer::tag_list_empty) - Special function for writing an empty list
//! * [tag_list_slice](TdfSerializer::tag_list_slice) - Special function for writing a list using a slice of serializable elements
//! * [tag_list_slice_ref](TdfSerializer::tag_list_slice_ref) - Special function for writing a list using a slice of references to serializable elements
//!
//! #### List Iterator Functions
//!
//! * [tag_list_iter](TdfSerializer::tag_list_iter) - Special function for writing a list using a iterator
//! * [tag_list_iter_ref](TdfSerializer::tag_list_iter_ref) - Special function for writing a list using a iterator of references
//! * [tag_list_iter_owned](TdfSerializer::tag_list_iter_owned) - Special function for writing a list using a iterator of owned values (i.e primitives)
//!
//! > **Note**
//! > Iterators use by these functions must implement [ExactSizeIterator] otherwise the size cannot be
//! > written for the list header
//!
//! ### Map Functions
//!
//! Tagging functions for map types
//!
//! * [tag_map_start](TdfSerializer::tag_map_empty) - Special function for writing an empty map
//! * [tag_map_start](TdfSerializer::tag_map_start) - Special function for writing a map header (Used to manually write list impl for complex types)
//! * [tag_map_tuples](TdfSerializer::tag_map_tuples) - Special function for writing a map from a slice of key value pairs
//!
//! #### Map Iterator Functions
//!
//! * [tag_map_iter](TdfSerializer::tag_map_iter) - Special function for writing a map using a iterator of key value pairs
//! * [tag_map_iter_ref](TdfSerializer::tag_map_iter_ref) - Special function for writing a map using a iterator of references to key value pairs
//! * [tag_map_iter_ref_ref](TdfSerializer::tag_map_iter_ref_ref) - Special function for writing a map using a iterator of reference to key value reference pairs
//! * [tag_map_iter_owned](TdfSerializer::tag_map_iter_owned) - Special function for writing a map using a iterator of owned key value pairs
//!
//! > **Note**
//! > Iterators use by these functions must implement [ExactSizeIterator] otherwise the size cannot be
//! > written for the map header
//!
//! ### Var Int Lists
//!
//! * [tag_var_int_list_empty](TdfSerializer::tag_var_int_list_empty) - Special function for writing an empty var int list
//! * [tag_var_int_list](TdfSerializer::tag_var_int_list) - Special function for writing a var int list from a slice
//!
//! ### Tagged Unions
//!
//! * [tag_union_start](TdfSerializer::tag_union_start) - Special function for writing a tagged union header (Used to manually write list impl for complex types)
//! * [tag_union_value](TdfSerializer::tag_union_value) - Special function for writing a tagged union
//! * [tag_union_unset](TdfSerializer::tag_union_unset) - Special function for writing a tagged union with an unset value

use crate::{
    tag::{RawTag, Tagged, TdfType},
    types::{
        list::serialize_list_header, map::serialize_map_header, string::write_empty_str,
        tagged_union::TAGGED_UNSET_KEY, Blob,
    },
    types::{TdfSerialize, TdfSerializeOwned, TdfTyped},
};

/// [TdfSerializer] provides functions for writing tag values to a buffer for serialization.
/// See [module documentation](crate::writer) for usage
pub trait TdfSerializer: Sized {
    /// Internal function for writing a single byte to the serializer
    fn write_byte(&mut self, value: u8);

    /// Internal function for writing a slice of bytes to the serializer
    fn write_slice(&mut self, value: &[u8]);

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
    /// let mut w = Vec::new();
    /// w.tag_ref(b"TEST", &1);
    ///
    /// ```
    ///
    /// # Arguments
    /// * tag - The tag to use for this field
    /// * value - The value to serialize
    fn tag_ref<V>(&mut self, tag: RawTag, value: &V)
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
    /// let mut w = Vec::new();
    /// let test_slice: &[u8] = &[0,5,12,23,255];
    ///
    /// w.tag_alt(b"TEST", "Example value");
    /// w.tag_alt(b"TEST", test_slice);
    /// ```
    ///
    /// # Arguments
    /// * tag - The tag to use for this field
    /// * value - The value to serialize
    fn tag_alt<V>(&mut self, tag: RawTag, value: V)
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
    /// let mut w = Vec::new();
    ///
    /// w.tag_owned(b"TEST", false);
    /// w.tag_owned(b"TEST", 123u8);
    /// w.tag_owned(b"TEST", 123u64);
    /// ```
    ///
    /// # Arguments
    /// * tag - The tag to use for this field
    /// * value - The owned value to serialize
    fn tag_owned<V>(&mut self, tag: RawTag, value: V)
    where
        V: TdfSerializeOwned + TdfTyped,
    {
        Tagged::serialize_raw(self, tag, V::TYPE);
        value.serialize_owned(self);
    }

    // Primitive integer tagging

    /// Tags a zero value
    fn tag_zero(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::VarInt);
        self.write_byte(0);
    }

    /// Tags a boolean value
    #[inline(always)]
    fn tag_bool(&mut self, tag: RawTag, value: bool) {
        self.tag_owned(tag, value);
    }

    /// Tags a u8 value
    #[inline(always)]
    fn tag_u8(&mut self, tag: RawTag, value: u8) {
        self.tag_owned(tag, value);
    }

    /// Tags a u16 value
    #[inline(always)]
    fn tag_u16(&mut self, tag: RawTag, value: u16) {
        self.tag_owned(tag, value);
    }

    /// Tags a u32 value
    #[inline(always)]
    fn tag_u32(&mut self, tag: RawTag, value: u32) {
        self.tag_owned(tag, value);
    }

    /// Tags a u64 value
    #[inline(always)]
    fn tag_u64(&mut self, tag: RawTag, value: u64) {
        self.tag_owned(tag, value);
    }

    /// Tags a usize value
    #[inline(always)]
    fn tag_usize(&mut self, tag: RawTag, value: usize) {
        self.tag_owned(tag, value);
    }

    // String tagging

    /// Tags an empty string value
    fn tag_str_empty(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::String);
        write_empty_str(self);
    }

    /// Tags a string value
    #[inline(always)]
    fn tag_str(&mut self, tag: RawTag, value: &str) {
        self.tag_alt(tag, value);
    }

    // Blob tagging

    /// Tags an empty blob
    fn tag_blob_empty(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::Blob);
        self.write_byte(0);
    }

    /// Tags a blob value from a slice
    fn tag_blob(&mut self, tag: RawTag, blob: &[u8]) {
        Tagged::serialize_raw(self, tag, TdfType::Blob);
        Blob::serialize_raw(self, blob);
    }

    // Group tagging

    /// Tags the start of a group
    #[inline]
    fn tag_group(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::Group);
    }

    /// Tags an empty group
    #[inline]
    fn tag_group_empty(&mut self, tag: RawTag) {
        self.tag_group(tag);
        self.tag_group_end();
    }

    /// Tags the end of a group
    #[inline]
    fn tag_group_end(&mut self) {
        self.write_byte(0);
    }

    /// Tags a group value running the `gr` function within
    /// the context of the group closing the group after
    #[inline]
    fn group<F>(&mut self, tag: RawTag, gr: F)
    where
        F: FnOnce(&mut Self),
    {
        self.tag_group(tag);
        gr(self);
        self.tag_group_end();
    }

    /// Runs the provided `gr` function as the body
    /// of a group then write the group end
    ///
    /// Useful for manually writing group lists
    #[inline]
    fn group_body<F>(&mut self, gr: F)
    where
        F: FnOnce(&mut Self),
    {
        gr(self);
        self.tag_group_end();
    }

    // List tagging

    /// Tags the start of a list for manual list writing
    fn tag_list_start(&mut self, tag: RawTag, ty: TdfType, length: usize) {
        Tagged::serialize_raw(self, tag, TdfType::List);
        serialize_list_header(self, ty, length);
    }

    /// Tags an empty list
    #[inline]
    fn tag_list_empty(&mut self, tag: RawTag, ty: TdfType) {
        self.tag_list_start(tag, ty, 0);
    }

    /// Tags a list from a slice value
    #[inline]
    fn tag_list_slice<V>(&mut self, tag: RawTag, value: &[V])
    where
        V: TdfSerialize + TdfTyped,
    {
        self.tag_alt(tag, value);
    }

    /// Tags a list from a slice of references
    #[inline]
    fn tag_list_slice_ref<V>(&mut self, tag: RawTag, value: &[&V])
    where
        V: TdfSerialize + TdfTyped,
    {
        self.tag_list_start(tag, V::TYPE, value.len());
        value.iter().for_each(|value| value.serialize(self));
    }

    /// Tags a list from an [ExactSizeIterator]
    #[inline]
    fn tag_list_iter<I, V>(&mut self, tag: RawTag, iter: I)
    where
        I: Iterator<Item = V> + ExactSizeIterator,
        V: TdfSerialize + TdfTyped,
    {
        self.tag_list_start(tag, V::TYPE, iter.len());
        iter.for_each(|value| value.serialize(self));
    }

    /// Tag list iter but for ref values
    #[inline]
    fn tag_list_iter_ref<'i, I, V>(&mut self, tag: RawTag, iter: I)
    where
        I: Iterator<Item = &'i V> + ExactSizeIterator,
        V: TdfSerialize + TdfTyped + 'i,
    {
        self.tag_list_start(tag, V::TYPE, iter.len());
        iter.for_each(|value| value.serialize(self));
    }

    /// Tag list iter but for owned values
    #[inline]
    fn tag_list_iter_owned<'i, I, V>(&mut self, tag: RawTag, iter: I)
    where
        I: Iterator<Item = V> + ExactSizeIterator,
        V: TdfSerializeOwned + TdfTyped + 'i,
    {
        self.tag_list_start(tag, V::TYPE, iter.len());
        iter.for_each(|value| value.serialize_owned(self));
    }

    // Var int list tagging

    /// Tags an empty var int list
    fn tag_var_int_list_empty(&mut self, tag: RawTag) {
        Tagged::serialize_raw(self, tag, TdfType::VarIntList);
        self.write_byte(0);
    }

    /// Tags a var int list from a slice of u64s
    fn tag_var_int_list(&mut self, tag: RawTag, values: &[u64]) {
        Tagged::serialize_raw(self, tag, TdfType::VarIntList);
        values.len().serialize_owned(self);
        values
            .iter()
            .copied()
            .for_each(|value| value.serialize_owned(self));
    }

    // Map tagging

    /// Tags the start of a map
    fn tag_map_start(&mut self, tag: RawTag, key: TdfType, value: TdfType, length: usize) {
        Tagged::serialize_raw(self, tag, TdfType::Map);
        serialize_map_header(self, key, value, length);
    }

    /// Tags the start of an empty map
    fn tag_map_empty(&mut self, tag: RawTag, key: TdfType, value: TdfType) {
        Tagged::serialize_raw(self, tag, TdfType::Map);
        serialize_map_header(self, key, value, 0);
    }

    /// Tags a map from a slice of tuple values
    #[inline]
    fn tag_map_tuples<K, V>(&mut self, tag: RawTag, values: &[(K, V)])
    where
        K: TdfSerialize + TdfTyped,
        V: TdfSerialize + TdfTyped,
    {
        self.tag_map_iter_ref(tag, values.iter())
    }

    /// Tags a map from an [ExactSizeIterator]
    fn tag_map_iter<I, K, V>(&mut self, tag: RawTag, iter: I)
    where
        I: Iterator<Item = (K, V)> + ExactSizeIterator,
        K: TdfSerialize + TdfTyped,
        V: TdfSerialize + TdfTyped,
    {
        self.tag_map_start(tag, K::TYPE, V::TYPE, iter.len());
        iter.for_each(|(key, value)| {
            key.serialize(self);
            value.serialize(self);
        });
    }

    /// Tags a map from an [ExactSizeIterator] of references to tuples
    fn tag_map_iter_ref<'i, I, K, V>(&mut self, tag: RawTag, iter: I)
    where
        I: Iterator<Item = &'i (K, V)> + ExactSizeIterator,
        K: TdfSerialize + TdfTyped + 'i,
        V: TdfSerialize + TdfTyped + 'i,
    {
        self.tag_map_start(tag, K::TYPE, V::TYPE, iter.len());
        iter.for_each(|(key, value)| {
            key.serialize(self);
            value.serialize(self);
        });
    }

    /// Tags a map from an [ExactSizeIterator] of references to tuples of
    /// references
    fn tag_map_iter_ref_ref<'i, I, K, V>(&mut self, tag: RawTag, iter: I)
    where
        I: Iterator<Item = &'i (&'i K, &'i V)> + ExactSizeIterator,
        K: TdfSerialize + TdfTyped + 'i,
        V: TdfSerialize + TdfTyped + 'i,
    {
        self.tag_map_start(tag, K::TYPE, V::TYPE, iter.len());
        iter.for_each(|(key, value)| {
            key.serialize(self);
            value.serialize(self);
        });
    }

    /// Tags a map from an [ExactSizeIterator] of owned values
    fn tag_map_iter_owned<I, K, V>(&mut self, tag: RawTag, iter: I)
    where
        I: Iterator<Item = (K, V)> + ExactSizeIterator,
        K: TdfSerializeOwned + TdfTyped,
        V: TdfSerializeOwned + TdfTyped,
    {
        self.tag_map_start(tag, K::TYPE, V::TYPE, iter.len());
        iter.for_each(|(key, value)| {
            key.serialize_owned(self);
            value.serialize_owned(self);
        });
    }

    // Union tagging

    /// Tags the start of a union value
    fn tag_union_start(&mut self, tag: RawTag, key: u8) {
        Tagged::serialize_raw(self, tag, TdfType::TaggedUnion);
        self.write_byte(key);
    }

    /// Tags a union and its value
    #[inline]
    fn tag_union_value<V>(&mut self, tag: RawTag, key: u8, value_tag: RawTag, value: &V)
    where
        V: TdfSerialize + TdfTyped,
    {
        self.tag_union_start(tag, key);
        self.tag_ref(value_tag, value);
    }

    /// Tags an unset union
    #[inline]
    fn tag_union_unset(&mut self, tag: RawTag) {
        self.tag_union_start(tag, TAGGED_UNSET_KEY);
    }

    // Buffer functions
}

impl TdfSerializer for Vec<u8> {
    #[inline]
    fn write_byte(&mut self, value: u8) {
        self.push(value);
    }

    #[inline]
    fn write_slice(&mut self, value: &[u8]) {
        self.extend_from_slice(value);
    }
}

#[cfg(feature = "bytes")]
mod bytes {
    use bytes::{BufMut, BytesMut};

    use super::TdfSerializer;

    impl TdfSerializer for BytesMut {
        #[inline]
        fn write_byte(&mut self, value: u8) {
            self.put_u8(value);
        }

        #[inline]
        fn write_slice(&mut self, value: &[u8]) {
            self.put_slice(value);
        }
    }
}
