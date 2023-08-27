//! Types implementation for custom types used while encoding values
//! with Blaze packets

use crate::codec::TdfDeserializeOwned;

use super::{
    codec::{TdfDeserialize, TdfSerialize, TdfTyped},
    error::{DecodeError, DecodeResult},
    reader::TdfReader,
    tag::TdfType,
    writer::TdfSerializer,
};

use std::fmt::Debug;

pub use map::TdfMap;
pub use tagged_union::TaggedUnion;
pub use var_int_list::VarIntList;

pub mod var_int_list {
    use crate::{
        codec::{TdfDeserializeOwned, TdfSerialize, TdfTyped},
        error::DecodeResult,
        reader::TdfReader,
        tag::TdfType,
        writer::TdfSerializer,
    };

    /// Wrapper type for a list of variable-length integers.
    /// Represented using a Vec of u64 values
    #[derive(Debug, PartialEq, Eq, Default, Clone)]
    pub struct VarIntList(pub Vec<u64>);

    impl VarIntList {
        /// Creates a new VarIntList
        pub fn new() -> Self {
            Self(Vec::default())
        }

        /// Consumes self returning the underlying
        /// Vec storing the variable-length integer values
        pub fn into_inner(self) -> Vec<u64> {
            self.0
        }
    }

    impl From<Vec<u64>> for VarIntList {
        fn from(value: Vec<u64>) -> Self {
            Self(value)
        }
    }

    impl AsRef<[u64]> for VarIntList {
        fn as_ref(&self) -> &[u64] {
            self.0.as_ref()
        }
    }

    impl TdfSerialize for VarIntList {
        fn serialize(&self, w: &mut TdfSerializer) {
            w.write_usize(self.0.len());
            self.0.iter().copied().for_each(|value| w.write_u64(value));
        }
    }

    impl TdfDeserializeOwned for VarIntList {
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            let length = reader.read_usize()?;
            let mut out = Vec::with_capacity(length);
            for _ in 0..length {
                out.push(reader.read_u64()?);
            }
            Ok(VarIntList(out))
        }
    }

    impl TdfTyped for VarIntList {
        const TYPE: TdfType = TdfType::VarIntList;
    }
}

pub mod tagged_union {
    use crate::{
        codec::{TdfDeserialize, TdfSerialize, TdfTyped},
        error::{DecodeError, DecodeResult},
        reader::TdfReader,
        tag::{Tag, TdfType},
        writer::TdfSerializer,
    };

    /// Representation of a tagged union
    #[derive(Debug, PartialEq, Eq)]
    pub enum TaggedUnion<Value> {
        /// Set variant of a union value
        Set { key: u8, tag: Tag, value: Value },
        /// Unset variant of a union value
        Unset,
    }

    impl<Value> TaggedUnion<Value> {
        /// Key used by tagged unions that have no set value
        pub const UNSET_KEY: u8 = 0x7F;

        /// Checks if the union is of set type
        pub fn is_set(&self) -> bool {
            matches!(self, Self::Set { .. })
        }

        /// Checks if the union is of unset type
        pub fn is_unset(&self) -> bool {
            matches!(self, Self::Unset)
        }

        /// Unwraps the underlying value stored in this tagged
        /// union. Will panic if the tagged union is unset
        pub fn unwrap(self) -> Value {
            match self {
                Self::Unset => panic!("Attempted to unwrap unset union"),
                Self::Set { value, .. } => value,
            }
        }
    }

    impl<Value> From<TaggedUnion<Value>> for Option<Value> {
        fn from(value: TaggedUnion<Value>) -> Self {
            match value {
                TaggedUnion::Set { value, .. } => Some(value),
                TaggedUnion::Unset => None,
            }
        }
    }

    impl<Value> TdfTyped for TaggedUnion<Value> {
        const TYPE: TdfType = TdfType::Union;
    }

    impl<Value> TdfSerialize for TaggedUnion<Value>
    where
        Value: TdfSerialize + TdfTyped,
    {
        fn serialize(&self, output: &mut TdfSerializer) {
            match self {
                TaggedUnion::Set { key, tag, value } => {
                    output.write_byte(*key);
                    output.tag(&tag.0, Value::TYPE);
                    value.serialize(output);
                }
                TaggedUnion::Unset => output.write_byte(Self::UNSET_KEY),
            }
        }
    }

    impl<'de, Value> TdfDeserialize<'de> for TaggedUnion<Value>
    where
        Value: TdfDeserialize<'de> + TdfTyped,
    {
        fn deserialize(reader: &mut TdfReader<'de>) -> DecodeResult<Self> {
            let key = reader.read_byte()?;
            if key == Self::UNSET_KEY {
                return Ok(TaggedUnion::Unset);
            }
            let tag = reader.read_tag()?;
            let expected_type = Value::TYPE;
            let actual_type = tag.ty;
            if actual_type != expected_type {
                return Err(DecodeError::InvalidType {
                    expected: expected_type,
                    actual: actual_type,
                });
            }
            let value = Value::deserialize(reader)?;

            Ok(TaggedUnion::Set {
                key,
                tag: tag.tag,
                value,
            })
        }
    }
}

pub mod map {
    use std::{
        borrow::Borrow,
        fmt::Debug,
        ops::{Bound, Index, IndexMut, RangeBounds},
    };

    use crate::{
        codec::{TdfDeserialize, TdfSerialize, TdfTyped},
        error::DecodeResult,
        reader::TdfReader,
        tag::TdfType,
        writer::TdfSerializer,
    };

    /// [TdfMap] is the data structure used for serializing maps in the
    /// Tdf format. This map implementation uses Rusts nightly
    /// [SortedMap](https://doc.rust-lang.org/stable/nightly-rustc/rustc_data_structures/sorted_map/struct.SortedMap.html)
    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TdfMap<K, V> {
        data: Vec<(K, V)>,
    }

    impl<K, V> Default for TdfMap<K, V> {
        #[inline]
        fn default() -> TdfMap<K, V> {
            TdfMap { data: Vec::new() }
        }
    }

    impl<K, V> TdfMap<K, V> {
        #[inline]
        pub const fn new() -> TdfMap<K, V> {
            TdfMap { data: Vec::new() }
        }

        #[inline]
        pub fn with_capacity(cap: usize) -> TdfMap<K, V> {
            TdfMap {
                data: Vec::with_capacity(cap),
            }
        }
    }

    impl<K: Ord, V> TdfMap<K, V> {
        /// Construct a `SortedMap` from a presorted set of elements. This is faster
        /// than creating an empty map and then inserting the elements individually.
        ///
        /// It is up to the caller to make sure that the elements are sorted by key
        /// and that there are no duplicates.
        #[inline]
        pub fn from_presorted_elements(elements: Vec<(K, V)>) -> TdfMap<K, V> {
            TdfMap { data: elements }
        }

        #[inline]
        pub fn insert(&mut self, key: K, mut value: V) -> Option<V> {
            match self.lookup_index_for(&key) {
                Ok(index) => {
                    let slot = unsafe { self.data.get_unchecked_mut(index) };
                    std::mem::swap(&mut slot.1, &mut value);
                    Some(value)
                }
                Err(index) => {
                    self.data.insert(index, (key, value));
                    None
                }
            }
        }

        #[inline]
        pub fn remove(&mut self, key: &K) -> Option<V> {
            match self.lookup_index_for(key) {
                Ok(index) => Some(self.data.remove(index).1),
                Err(_) => None,
            }
        }

        #[inline]
        pub fn get<Q>(&self, key: &Q) -> Option<&V>
        where
            K: Borrow<Q>,
            Q: Ord + ?Sized,
        {
            match self.lookup_index_for(key) {
                Ok(index) => unsafe { Some(&self.data.get_unchecked(index).1) },
                Err(_) => None,
            }
        }

        #[inline]
        pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
        where
            K: Borrow<Q>,
            Q: Ord + ?Sized,
        {
            match self.lookup_index_for(key) {
                Ok(index) => unsafe { Some(&mut self.data.get_unchecked_mut(index).1) },
                Err(_) => None,
            }
        }

        /// Gets a mutable reference to the value in the entry, or insert a new one.
        #[inline]
        pub fn get_mut_or_insert_default(&mut self, key: K) -> &mut V
        where
            K: Eq,
            V: Default,
        {
            let index = match self.lookup_index_for(&key) {
                Ok(index) => index,
                Err(index) => {
                    self.data.insert(index, (key, V::default()));
                    index
                }
            };
            unsafe { &mut self.data.get_unchecked_mut(index).1 }
        }

        #[inline]
        pub fn clear(&mut self) {
            self.data.clear();
        }

        /// Iterate over elements, sorted by key
        #[inline]
        pub fn iter(&self) -> std::slice::Iter<'_, (K, V)> {
            self.data.iter()
        }

        /// Iterate over the keys, sorted
        #[inline]
        pub fn keys(&self) -> impl Iterator<Item = &K> + ExactSizeIterator + DoubleEndedIterator {
            self.data.iter().map(|(k, _)| k)
        }

        /// Iterate over values, sorted by key
        #[inline]
        pub fn values(&self) -> impl Iterator<Item = &V> + ExactSizeIterator + DoubleEndedIterator {
            self.data.iter().map(|(_, v)| v)
        }

        #[inline]
        pub fn len(&self) -> usize {
            self.data.len()
        }

        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }

        #[inline]
        pub fn range<R>(&self, range: R) -> &[(K, V)]
        where
            R: RangeBounds<K>,
        {
            let (start, end) = self.range_slice_indices(range);
            &self.data[start..end]
        }

        #[inline]
        pub fn remove_range<R>(&mut self, range: R)
        where
            R: RangeBounds<K>,
        {
            let (start, end) = self.range_slice_indices(range);
            self.data.splice(start..end, std::iter::empty());
        }

        /// Mutate all keys with the given function `f`. This mutation must not
        /// change the sort-order of keys.
        #[inline]
        pub fn offset_keys<F>(&mut self, f: F)
        where
            F: Fn(&mut K),
        {
            self.data.iter_mut().map(|(k, _)| k).for_each(f);
        }

        /// Inserts a presorted range of elements into the map. If the range can be
        /// inserted as a whole in between to existing elements of the map, this
        /// will be faster than inserting the elements individually.
        ///
        /// It is up to the caller to make sure that the elements are sorted by key
        /// and that there are no duplicates.
        #[inline]
        pub fn insert_presorted(&mut self, elements: Vec<(K, V)>) {
            if elements.is_empty() {
                return;
            }

            let start_index = self.lookup_index_for(&elements[0].0);

            let elements = match start_index {
                Ok(index) => {
                    let mut elements = elements.into_iter();
                    self.data[index] = elements.next().unwrap();
                    elements
                }
                Err(index) => {
                    if index == self.data.len() || elements.last().unwrap().0 < self.data[index].0 {
                        // We can copy the whole range without having to mix with
                        // existing elements.
                        self.data.splice(index..index, elements.into_iter());
                        return;
                    }

                    let mut elements = elements.into_iter();
                    self.data.insert(index, elements.next().unwrap());
                    elements
                }
            };

            // Insert the rest
            for (k, v) in elements {
                self.insert(k, v);
            }
        }

        /// Looks up the key in `self.data` via `slice::binary_search()`.
        #[inline(always)]
        fn lookup_index_for<Q>(&self, key: &Q) -> Result<usize, usize>
        where
            K: Borrow<Q>,
            Q: Ord + ?Sized,
        {
            self.data.binary_search_by(|(x, _)| x.borrow().cmp(key))
        }

        #[inline]
        fn range_slice_indices<R>(&self, range: R) -> (usize, usize)
        where
            R: RangeBounds<K>,
        {
            let start = match range.start_bound() {
                Bound::Included(k) => match self.lookup_index_for(k) {
                    Ok(index) | Err(index) => index,
                },
                Bound::Excluded(k) => match self.lookup_index_for(k) {
                    Ok(index) => index + 1,
                    Err(index) => index,
                },
                Bound::Unbounded => 0,
            };

            let end = match range.end_bound() {
                Bound::Included(k) => match self.lookup_index_for(k) {
                    Ok(index) => index + 1,
                    Err(index) => index,
                },
                Bound::Excluded(k) => match self.lookup_index_for(k) {
                    Ok(index) | Err(index) => index,
                },
                Bound::Unbounded => self.data.len(),
            };

            (start, end)
        }

        #[inline]
        pub fn contains_key<Q>(&self, key: &Q) -> bool
        where
            K: Borrow<Q>,
            Q: Ord + ?Sized,
        {
            self.get(key).is_some()
        }
    }

    impl<K: Ord, V> IntoIterator for TdfMap<K, V> {
        type Item = (K, V);
        type IntoIter = std::vec::IntoIter<(K, V)>;

        fn into_iter(self) -> Self::IntoIter {
            self.data.into_iter()
        }
    }

    impl<'a, K, Q, V> Index<&'a Q> for TdfMap<K, V>
    where
        K: Ord + Borrow<Q>,
        Q: Ord + ?Sized,
    {
        type Output = V;

        fn index(&self, key: &Q) -> &Self::Output {
            self.get(key).expect("no entry found for key")
        }
    }

    impl<'a, K, Q, V> IndexMut<&'a Q> for TdfMap<K, V>
    where
        K: Ord + Borrow<Q>,
        Q: Ord + ?Sized,
    {
        fn index_mut(&mut self, key: &Q) -> &mut Self::Output {
            self.get_mut(key).expect("no entry found for key")
        }
    }

    impl<K: Ord, V> FromIterator<(K, V)> for TdfMap<K, V> {
        fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
            let mut data: Vec<(K, V)> = iter.into_iter().collect();

            data.sort_unstable_by(|(k1, _), (k2, _)| k1.cmp(k2));
            data.dedup_by(|(k1, _), (k2, _)| k1 == k2);

            TdfMap { data }
        }
    }

    impl<K: Debug, V: Debug> Debug for TdfMap<K, V> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_map()
                .entries(self.data.iter().map(|(a, b)| (a, b)))
                .finish()
        }
    }

    impl<K, V> TdfSerialize for TdfMap<K, V>
    where
        K: TdfSerialize + TdfTyped + Ord,
        V: TdfSerialize + TdfTyped,
    {
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_map_header(K::TYPE, V::TYPE, self.len());

            self.data.iter().for_each(|(key, value)| {
                key.serialize(output);
                value.serialize(output);
            });
        }
    }

    impl<'de, K, V> TdfDeserialize<'de> for TdfMap<K, V>
    where
        K: TdfDeserialize<'de> + TdfTyped + Ord,
        V: TdfDeserialize<'de> + TdfTyped,
    {
        #[inline]
        fn deserialize(reader: &mut TdfReader<'de>) -> DecodeResult<Self> {
            reader.read_map()
        }
    }

    impl<K, V> TdfTyped for TdfMap<K, V> {
        const TYPE: TdfType = TdfType::Map;
    }
}

pub mod float {
    use crate::{
        codec::{TdfDeserializeOwned, TdfSerialize, TdfTyped},
        error::DecodeResult,
        reader::TdfReader,
        tag::TdfType,
        writer::TdfSerializer,
    };

    impl TdfSerialize for f32 {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_f32(*self)
        }
    }

    impl TdfDeserializeOwned for f32 {
        #[inline]
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            reader.read_f32()
        }
    }

    impl TdfTyped for f32 {
        const TYPE: TdfType = TdfType::Float;
    }
}

pub mod var_int {
    use crate::{
        codec::{TdfDeserializeOwned, TdfSerialize, TdfTyped},
        error::DecodeResult,
        reader::TdfReader,
        tag::TdfType,
        writer::TdfSerializer,
    };

    impl TdfSerialize for bool {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_bool(*self)
        }
    }

    impl TdfDeserializeOwned for bool {
        #[inline]
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            reader.read_bool()
        }
    }

    impl TdfTyped for bool {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt u8

    impl TdfDeserializeOwned for u8 {
        #[inline]
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            reader.read_u8()
        }
    }

    impl TdfSerialize for u8 {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_u8(*self)
        }
    }

    impl TdfTyped for u8 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt i8

    impl TdfDeserializeOwned for i8 {
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            let value = reader.read_u8()?;
            Ok(value as i8)
        }
    }

    impl TdfSerialize for i8 {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_u8(*self as u8)
        }
    }

    impl TdfTyped for i8 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt u16

    impl TdfDeserializeOwned for u16 {
        #[inline]
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            reader.read_u16()
        }
    }

    impl TdfSerialize for u16 {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_u16(*self)
        }
    }

    impl TdfTyped for u16 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    impl TdfDeserializeOwned for i16 {
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            let value = reader.read_u16()?;
            Ok(value as i16)
        }
    }

    // VarInt i16

    impl TdfSerialize for i16 {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_u16(*self as u16)
        }
    }

    impl TdfTyped for i16 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    impl TdfDeserializeOwned for u32 {
        #[inline]
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            reader.read_u32()
        }
    }

    // VarInt u32

    impl TdfSerialize for u32 {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_u32(*self)
        }
    }

    impl TdfTyped for u32 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt i32

    impl TdfDeserializeOwned for i32 {
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            let value = reader.read_u32()?;
            Ok(value as i32)
        }
    }

    impl TdfSerialize for i32 {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_u32(*self as u32)
        }
    }

    impl TdfTyped for i32 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt u64

    impl TdfDeserializeOwned for u64 {
        #[inline]
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            reader.read_u64()
        }
    }

    impl TdfSerialize for u64 {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_u64(*self)
        }
    }

    impl TdfTyped for u64 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt i64

    impl TdfDeserializeOwned for i64 {
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            let value = reader.read_u64()?;
            Ok(value as i64)
        }
    }

    impl TdfSerialize for i64 {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_u64(*self as u64)
        }
    }

    impl TdfTyped for i64 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt usize

    impl TdfDeserializeOwned for usize {
        #[inline]
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            reader.read_usize()
        }
    }

    impl TdfSerialize for usize {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_usize(*self)
        }
    }

    impl TdfTyped for usize {
        const TYPE: TdfType = TdfType::VarInt;
    }

    impl TdfDeserializeOwned for isize {
        fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
            let value = reader.read_usize()?;
            Ok(value as isize)
        }
    }

    // VarInt isize

    impl TdfSerialize for isize {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_usize(*self as usize)
        }
    }

    impl TdfTyped for isize {
        const TYPE: TdfType = TdfType::VarInt;
    }
}

pub mod string {
    use crate::{
        codec::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfTyped},
        error::DecodeResult,
        reader::TdfReader,
        tag::TdfType,
        writer::TdfSerializer,
    };

    // str slice types

    impl<'de> TdfDeserialize<'de> for &'de str {
        #[inline]
        fn deserialize(r: &mut crate::reader::TdfReader<'de>) -> DecodeResult<Self> {
            r.read_str()
        }
    }

    impl TdfSerialize for &str {
        #[inline]
        fn serialize(&self, output: &mut TdfSerializer) {
            output.write_str(self)
        }
    }

    impl TdfTyped for &str {
        const TYPE: TdfType = TdfType::String;
    }

    // Owned String types

    impl TdfDeserializeOwned for String {
        #[inline]
        fn deserialize_owned(r: &mut TdfReader) -> DecodeResult<Self> {
            r.read_string()
        }
    }

    impl TdfSerialize for String {
        #[inline]
        fn serialize(&self, w: &mut TdfSerializer) {
            w.write_str(self);
        }
    }

    impl TdfTyped for String {
        const TYPE: TdfType = TdfType::String;
    }
}

/// Blob structure wrapping a vec of bytes. This implementation is
/// to differenciate between a list of VarInts and a Blob of straight
/// bytes
#[derive(Default, Debug, Clone)]
pub struct Blob<'de>(pub &'de [u8]);

impl TdfSerialize for Blob<'_> {
    fn serialize(&self, output: &mut TdfSerializer) {
        output.write_usize(self.0.len());
        output.write_slice(&self.0);
    }
}

impl<'de> TdfDeserialize<'de> for Blob<'de> {
    fn deserialize(reader: &mut TdfReader<'de>) -> DecodeResult<Self> {
        let length = reader.read_usize()?;
        let bytes = reader.read_slice(length)?;
        Ok(Blob(bytes))
    }
}

impl TdfTyped for Blob<'_> {
    const TYPE: TdfType = TdfType::Blob;
}

/// Vec List encoding for encodable items items are required
/// to have the ValueType trait in order to write the list header
impl<C> TdfSerialize for Vec<C>
where
    C: TdfSerialize + TdfTyped,
{
    fn serialize(&self, writer: &mut TdfSerializer) {
        writer.write_type(C::TYPE);
        writer.write_usize(self.len());
        for value in self {
            value.serialize(writer);
        }
    }
}

/// Support for encoding slices of encodable items as lists
impl<C> TdfSerialize for &[C]
where
    C: TdfSerialize + TdfTyped,
{
    fn serialize(&self, writer: &mut TdfSerializer) {
        writer.write_type(C::TYPE);
        writer.write_usize(self.len());
        for value in self.iter() {
            value.serialize(writer);
        }
    }
}

impl<C> TdfTyped for &[C]
where
    C: TdfSerialize + TdfTyped,
{
    const TYPE: TdfType = TdfType::List;
}

impl<'de, C> TdfDeserialize<'de> for Vec<C>
where
    C: TdfDeserialize<'de> + TdfTyped,
{
    fn deserialize(reader: &mut TdfReader<'de>) -> DecodeResult<Self> {
        let value_type: TdfType = reader.read_type()?;
        let expected_type = C::TYPE;
        if value_type != expected_type {
            return Err(DecodeError::InvalidType {
                expected: expected_type,
                actual: value_type,
            });
        }

        let length = reader.read_usize()?;
        let mut values = Vec::with_capacity(length);
        for _ in 0..length {
            values.push(C::deserialize(reader)?);
        }
        Ok(values)
    }
}

impl<C> TdfTyped for Vec<C> {
    const TYPE: TdfType = TdfType::List;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectType {
    /// Component for the object type
    pub component: u16,
    /// The object type
    pub ty: u16,
}

impl TdfSerialize for ObjectType {
    fn serialize(&self, w: &mut TdfSerializer) {
        w.write_u16(self.component);
        w.write_u16(self.ty);
    }
}

impl TdfDeserializeOwned for ObjectType {
    fn deserialize_owned(r: &mut TdfReader) -> DecodeResult<Self> {
        let component = r.read_u16()?;
        let ty = r.read_u16()?;
        Ok(Self { component, ty })
    }
}

impl TdfTyped for ObjectType {
    const TYPE: TdfType = TdfType::ObjectType;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectId {
    /// The object type
    pub ty: ObjectType,
    /// The object ID
    pub id: u64,
}

impl TdfSerialize for ObjectId {
    fn serialize(&self, w: &mut TdfSerializer) {
        self.ty.serialize(w);
        w.write_u64(self.id);
    }
}

impl TdfDeserializeOwned for ObjectId {
    fn deserialize_owned(r: &mut TdfReader) -> DecodeResult<Self> {
        let ty = ObjectType::deserialize_owned(r)?;
        let id = r.read_u64()?;
        Ok(Self { ty, id })
    }
}

impl TdfTyped for ObjectId {
    const TYPE: TdfType = TdfType::ObjectId;
}

#[cfg(test)]
mod test {

    use std::time::Instant;

    use crate::types::TdfMap;

    // /// Tests ordering a map
    // #[test]
    // fn test_map_ord() {
    //     let mut map = TdfMap::<String, String>::new();

    //     // Expected order:
    //     // TdfMap {
    //     //   "key1": "ABC"
    //     //   "key11": "ABC"
    //     //   "key17": "ABC"
    //     //   "key2": "ABC"
    //     //   "key24": "ABC"
    //     //   "key4": "ABC"
    //     // }

    //     let i = Instant::now();
    //     // Input order
    //     map.insert("key1", "ABC");
    //     map.insert("key2", "ABC");
    //     map.insert("key4", "ABC");
    //     map.insert("key24", "ABC");
    //     map.insert("key11", "ABC");
    //     map.insert("key17", "ABC");

    //     map.order();
    //     let el = i.elapsed();
    //     println!("Full order time: {:?}", el);

    //     assert_eq!(map.entries[0].key, "key1");
    //     assert_eq!(map.entries[1].key, "key11");
    //     assert_eq!(map.entries[2].key, "key17");
    //     assert_eq!(map.entries[3].key, "key2");
    //     assert_eq!(map.entries[4].key, "key24");
    //     assert_eq!(map.entries[5].key, "key4");
    // }

    // /// Tests extending an existing map
    // #[test]
    // fn test_map_extend() {
    //     let mut mapa = TdfMap::<String, String>::new();

    //     mapa.insert("key1", "ABC");
    //     mapa.insert("key2", "ABC");
    //     mapa.insert("key4", "ABC");
    //     mapa.insert("key24", "ABC");
    //     mapa.insert("key11", "ABC");
    //     mapa.insert("key17", "ABC");

    //     let mut mapb = TdfMap::<String, String>::new();

    //     mapb.insert("key1", "DDD");
    //     mapb.insert("key2", "ABC");
    //     mapb.insert("key4", "DDD");
    //     mapb.insert("abc", "ABC");

    //     mapa.extend(mapb);
    //     println!("{mapa:?}")
    // }

    // /// Tests inserting into a map
    // #[test]
    // fn test_map_insert() {
    //     let mut map = TdfMap::<String, String>::new();
    //     map.insert("Test", "Abc");

    //     let value = map.get("Test");

    //     assert_eq!(value.unwrap(), "Abc");

    //     println!("{value:?}")
    // }
}
