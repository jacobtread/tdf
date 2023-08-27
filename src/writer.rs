//! Writer buffer implementation for writing different kinds of tdf values
//! to byte form without creating a new structure [`TdfWriter`]

use crate::{
    codec::TdfSerializeOwned,
    tag::Tagged,
    types::{string::write_empty_str, TaggedUnion},
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
    #[inline]
    pub fn write_byte(&mut self, value: u8) {
        self.buffer.push(value)
    }

    #[inline]
    pub fn write_slice(&mut self, value: &[u8]) {
        self.buffer.extend_from_slice(value);
    }

    #[inline]
    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    pub fn tag_bool(&mut self, tag: &[u8], value: bool) {
        Tagged::serialize_raw(self, tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    pub fn tag_zero(&mut self, tag: &[u8]) {
        Tagged::serialize_raw(self, tag, TdfType::VarInt);
        self.write_byte(0);
    }

    pub fn tag_u8(&mut self, tag: &[u8], value: u8) {
        Tagged::serialize_raw(self, tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    pub fn tag_u16(&mut self, tag: &[u8], value: u16) {
        Tagged::serialize_raw(self, tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    pub fn tag_u32(&mut self, tag: &[u8], value: u32) {
        Tagged::serialize_raw(self, tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    pub fn tag_u64(&mut self, tag: &[u8], value: u64) {
        Tagged::serialize_raw(self, tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    pub fn tag_usize(&mut self, tag: &[u8], value: usize) {
        Tagged::serialize_raw(self, tag, TdfType::VarInt);
        value.serialize_owned(self);
    }

    pub fn tag_str_empty(&mut self, tag: &[u8]) {
        Tagged::serialize_raw(self, tag, TdfType::String);
        write_empty_str(self);
    }

    pub fn tag_empty_blob(&mut self, tag: &[u8]) {
        Tagged::serialize_raw(self, tag, TdfType::Blob);
        self.write_byte(0);
    }

    pub fn tag_str(&mut self, tag: &[u8], value: &str) {
        Tagged::serialize_raw(self, tag, TdfType::String);
        value.serialize(self);
    }

    pub fn tag_group(&mut self, tag: &[u8]) {
        Tagged::serialize_raw(self, tag, TdfType::Group);
    }

    #[inline]
    pub fn tag_group_end(&mut self) {
        self.write_byte(0);
    }

    #[inline]
    pub fn group<F>(&mut self, tag: &[u8], gr: F)
    where
        F: FnOnce(&mut Self),
    {
        self.tag_group(tag);
        gr(self);
        self.tag_group_end();
    }

    pub fn tag_list_start(&mut self, tag: &[u8], ty: TdfType, length: usize) {
        Tagged::serialize_raw(self, tag, TdfType::List);
        ty.serialize_owned(self);
        length.serialize_owned(self);
    }

    pub fn tag_union_start(&mut self, tag: &[u8], key: u8) {
        Tagged::serialize_raw(self, tag, TdfType::TaggedUnion);
        self.write_byte(key);
    }

    pub fn tag_union_value<C: TdfSerialize + TdfTyped>(
        &mut self,
        tag: &[u8],
        key: u8,
        value_tag: &[u8],
        value: &C,
    ) {
        self.tag_union_start(tag, key);
        Tagged::serialize_raw(self, value_tag, C::TYPE);
        value.serialize(self);
    }

    pub fn tag_union_unset(&mut self, tag: &[u8]) {
        self.tag_union_start(tag, TaggedUnion::<()>::UNSET_KEY);
    }

    pub fn tag<V>(&mut self, tag: &[u8], value: &V)
    where
        V: TdfSerialize + TdfTyped,
    {
        Tagged::serialize_raw(self, tag, V::TYPE);
        value.serialize(self);
    }

    pub fn tag_list_empty(&mut self, tag: &[u8], ty: TdfType) {
        Tagged::serialize_raw(self, tag, TdfType::List);
        ty.serialize_owned(self);
        self.write_byte(0);
    }

    pub fn tag_slice_list<C: TdfSerialize + TdfTyped>(&mut self, tag: &[u8], value: &[C]) {
        Tagged::serialize_raw(self, tag, TdfType::List);
        value.serialize(self);
    }

    pub fn tag_var_int_list_empty(&mut self, tag: &[u8]) {
        Tagged::serialize_raw(self, tag, TdfType::VarIntList);
        self.write_byte(0);
    }

    pub fn tag_map_start(&mut self, tag: &[u8], key: TdfType, value: TdfType, length: usize) {
        Tagged::serialize_raw(self, tag, TdfType::Map);
        key.serialize_owned(self);
        value.serialize_owned(self);
        length.serialize_owned(self);
    }

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
}
