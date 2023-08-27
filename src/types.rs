//! Types implementation for custom types used while encoding values
//! with Blaze packets

pub use blob::Blob;
pub use map::TdfMap;
pub use object_id::ObjectId;
pub use object_type::ObjectType;
pub use tagged_union::TaggedUnion;
pub use u12::U12;
pub use var_int_list::VarIntList;

pub mod var_int {
    use crate::{
        codec::{TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped},
        error::DecodeResult,
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };

    /// Macro for implementing variable-length integer serialization
    /// for a specific integer type
    ///
    /// # Arguments
    /// * value - The value to use for serializing
    /// * w - The writer to use to write the output
    macro_rules! impl_serialize_int {
        ($value:ident, $w:ident) => {{
            if $value < 0x40 {
                $w.write_byte($value as u8);
                return;
            }

            $w.write_byte((($value & 0x3f) | 0x80) as u8);

            let mut value = $value >> 6;
            while value >= 0x80 {
                $w.write_byte(((value & 0x7f) | 0x80) as u8);
                value >>= 7;
            }

            $w.write_byte(value as u8)
        }};
    }

    /// Macro for implementing variable-length integer deserialization
    /// for a specific integer type
    ///
    /// # Arguments
    /// * ty - The type that is being deserialized
    /// * r - The reader to read bytes from
    macro_rules! impl_deserialize_int {
        ($ty:ty, $r:ident) => {{
            let mut byte: u8 = $r.read_byte()?;
            let mut result: $ty = (byte & 0x3F) as $ty;
            let mut shift = 6;
            while byte >= 0x80 {
                byte = $r.read_byte()?;
                result |= ((byte & 0x7f) as $ty).wrapping_shl(shift);
                shift += 7;
            }

            Ok(result)
        }};
    }

    impl TdfDeserializeOwned for bool {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u8::deserialize_owned(r)?;
            Ok(value == 1)
        }
    }

    impl TdfSerializeOwned for bool {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            w.write_byte(self as u8)
        }
    }

    impl TdfTyped for bool {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt u8

    impl TdfDeserializeOwned for u8 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(u8, r)
        }
    }

    impl TdfSerializeOwned for u8 {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            impl_serialize_int!(self, w)
        }
    }

    impl TdfTyped for u8 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt i8

    impl TdfDeserializeOwned for i8 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u8::deserialize_owned(r)?;
            Ok(value as i8)
        }
    }

    impl TdfSerializeOwned for i8 {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            (self as u8).serialize_owned(w)
        }
    }

    impl TdfTyped for i8 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt u16

    impl TdfDeserializeOwned for u16 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(u16, r)
        }
    }

    impl TdfSerializeOwned for u16 {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            impl_serialize_int!(self, w)
        }
    }

    impl TdfTyped for u16 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    impl TdfDeserializeOwned for i16 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u16::deserialize_owned(r)?;
            Ok(value as i16)
        }
    }

    // VarInt i16

    impl TdfSerializeOwned for i16 {
        #[inline]
        fn serialize_owned(self, w: &mut TdfSerializer) {
            (self as u16).serialize_owned(w);
        }
    }

    impl TdfTyped for i16 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    impl TdfDeserializeOwned for u32 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(u32, r)
        }
    }

    // VarInt u32

    impl TdfSerializeOwned for u32 {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            impl_serialize_int!(self, w)
        }
    }

    impl TdfTyped for u32 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt i32

    impl TdfDeserializeOwned for i32 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u32::deserialize_owned(r)?;
            Ok(value as i32)
        }
    }

    impl TdfSerializeOwned for i32 {
        #[inline]
        fn serialize_owned(self, w: &mut TdfSerializer) {
            (self as u32).serialize_owned(w);
        }
    }

    impl TdfTyped for i32 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt u64

    impl TdfDeserializeOwned for u64 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(u64, r)
        }
    }

    impl TdfSerializeOwned for u64 {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            impl_serialize_int!(self, w);
        }
    }

    impl TdfTyped for u64 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt i64

    impl TdfDeserializeOwned for i64 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u64::deserialize_owned(r)?;
            Ok(value as i64)
        }
    }

    impl TdfSerializeOwned for i64 {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            (self as u64).serialize_owned(w);
        }
    }

    impl TdfTyped for i64 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt usize

    impl TdfDeserializeOwned for usize {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(usize, r)
        }
    }

    impl TdfSerializeOwned for usize {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            impl_serialize_int!(self, w);
        }
    }

    impl TdfTyped for usize {
        const TYPE: TdfType = TdfType::VarInt;
    }

    impl TdfDeserializeOwned for isize {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = usize::deserialize_owned(r)?;
            Ok(value as isize)
        }
    }

    // VarInt isize

    impl TdfSerializeOwned for isize {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            (self as usize).serialize_owned(w);
        }
    }

    impl TdfTyped for isize {
        const TYPE: TdfType = TdfType::VarInt;
    }

    #[cfg(test)]
    mod test {
        use crate::{
            codec::{TdfDeserializeOwned, TdfSerialize},
            reader::TdfDeserializer,
            writer::TdfSerializer,
        };

        #[test]
        fn test_u64_encoding() {
            let mut w = TdfSerializer::default();

            let values: &[(u64, &[u8])] = &[
                (5, &[5]),
                (128, &[128, 2]),
                (256, &[128, 4]),
                (512, &[128, 8]),
                (321, &[129, 5]),
                (2048, &[128, 32]),
                (4096, &[128, 64]),
                (4632, &[152, 72]),
                (8192, &[128, 128, 1]),
                (16384, &[128, 128, 2]),
                (32768, &[128, 128, 4]),
                (65536, &[128, 128, 8]),
                (4294967296, &[128, 128, 128, 128, 32]),
                (
                    9223372036854776000,
                    &[128, 131, 128, 128, 128, 128, 128, 128, 128, 2],
                ),
            ];

            for (value, expected) in values {
                value.serialize(&mut w);
                assert_eq!(&w.buffer, expected);

                let mut r = TdfDeserializer::new(&w.buffer);
                let read_value = u64::deserialize_owned(&mut r).unwrap();
                assert_eq!(read_value, *value);

                w.clear();
            }
        }
    }
}

pub mod string {
    use super::Blob;
    use crate::{
        codec::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped},
        error::{DecodeError, DecodeResult},
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };
    use std::borrow::Cow;

    // str slice types

    impl<'de> TdfDeserialize<'de> for &'de str {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let bytes: &'de [u8] = Blob::deserialize_raw(r)?;
            let text: &'de str =
                std::str::from_utf8(bytes).map_err(DecodeError::InvalidUtf8Value)?;

            if text.is_empty() {
                return Err(DecodeError::Other("String value has zero length"));
            }

            Ok(&text[..text.len() - 1])
        }
    }

    impl TdfSerialize for &str {
        fn serialize(&self, w: &mut TdfSerializer) {
            let bytes = self.as_bytes();
            let needs_terminator = !matches!(bytes.last(), Some(0));
            let mut length = bytes.len();
            if needs_terminator {
                length += 1;
            }

            length.serialize_owned(w);
            w.write_slice(bytes);

            if needs_terminator {
                w.write_byte(0);
            }
        }
    }

    impl TdfTyped for &str {
        const TYPE: TdfType = TdfType::String;
    }

    // Owned String types

    impl TdfDeserializeOwned for String {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let bytes: &[u8] = Blob::deserialize_raw(r)?;
            let text: Cow<str> = String::from_utf8_lossy(bytes);
            let mut text: String = text.to_string();
            // Remove null terminator
            text.pop();
            Ok(text)
        }
    }

    impl TdfSerialize for String {
        #[inline]
        fn serialize(&self, w: &mut TdfSerializer) {
            self.as_str().serialize(w);
        }
    }

    impl TdfTyped for String {
        const TYPE: TdfType = TdfType::String;
    }
}

pub mod blob {
    use crate::{
        codec::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped},
        error::DecodeResult,
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };

    /// [Blob] is a structure representing a variable length chunk
    /// of bytes with the length deliminated by a variable-length integer
    /// followed by the chunk of bytes
    ///
    /// The [String] structure uses the same underlying logic but wraps
    /// extra utf8 parsing on top
    #[derive(Default, Debug, Clone, PartialEq, Eq)]
    pub struct Blob<'de>(pub &'de [u8]);

    impl AsRef<[u8]> for Blob<'_> {
        fn as_ref(&self) -> &[u8] {
            self.0
        }
    }

    impl Blob<'_> {
        pub fn serialize_raw(w: &mut TdfSerializer, slice: &[u8]) {
            slice.len().serialize_owned(w);
            w.write_slice(slice);
        }

        pub fn deserialize_raw<'de>(r: &mut TdfDeserializer<'de>) -> DecodeResult<&'de [u8]> {
            let length = usize::deserialize_owned(r)?;
            let bytes = r.read_slice(length)?;
            Ok(bytes)
        }
    }

    impl<'de> TdfDeserialize<'de> for Blob<'de> {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let bytes = Self::deserialize_raw(r)?;
            Ok(Blob(bytes))
        }
    }

    impl TdfSerialize for Blob<'_> {
        #[inline]
        fn serialize(&self, w: &mut TdfSerializer) {
            Self::serialize_raw(w, self.0)
        }
    }

    impl TdfTyped for Blob<'_> {
        const TYPE: TdfType = TdfType::Blob;
    }
}

pub mod list {
    use crate::{
        codec::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped},
        error::DecodeResult,
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };

    impl<'de, C> TdfDeserialize<'de> for Vec<C>
    where
        C: TdfDeserialize<'de> + TdfTyped,
    {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            r.expect_type(C::TYPE)?;

            let length = usize::deserialize_owned(r)?;
            let mut values = Vec::with_capacity(length);

            for _ in 0..length {
                values.push(C::deserialize(r)?);
            }
            Ok(values)
        }
    }

    impl<V> TdfSerialize for Vec<V>
    where
        V: TdfSerialize + TdfTyped,
    {
        #[inline]
        fn serialize(&self, w: &mut TdfSerializer) {
            self.as_slice().serialize(w)
        }
    }

    impl<V> TdfTyped for Vec<V> {
        const TYPE: TdfType = TdfType::List;
    }

    impl<V> TdfSerialize for &[V]
    where
        V: TdfSerialize + TdfTyped,
    {
        fn serialize(&self, w: &mut TdfSerializer) {
            w.write_type(V::TYPE);
            self.len().serialize_owned(w);
            self.iter().for_each(|value| value.serialize(w));
        }
    }

    impl<C> TdfTyped for &[C]
    where
        C: TdfSerialize + TdfTyped,
    {
        const TYPE: TdfType = TdfType::List;
    }
}

pub mod map {
    use std::{
        borrow::Borrow,
        fmt::Debug,
        ops::{Bound, Index, IndexMut, RangeBounds},
    };

    use crate::{
        codec::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfTyped},
        error::{DecodeError, DecodeResult},
        reader::TdfDeserializer,
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

    pub fn deserialize_map_header(
        r: &mut TdfDeserializer,
    ) -> DecodeResult<(TdfType, TdfType, usize)> {
        let key_type: TdfType = TdfType::deserialize_owned(r)?;
        let value_type: TdfType = TdfType::deserialize_owned(r)?;
        let length = usize::deserialize_owned(r)?;
        Ok((key_type, value_type, length))
    }

    impl<'de, K, V> TdfDeserialize<'de> for TdfMap<K, V>
    where
        K: TdfDeserialize<'de> + TdfTyped + Ord,
        V: TdfDeserialize<'de> + TdfTyped,
    {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let (key_ty, value_ty, length) = deserialize_map_header(r)?;

            if key_ty != K::TYPE {
                return Err(DecodeError::InvalidType {
                    expected: K::TYPE,
                    actual: key_ty,
                });
            }
            if value_ty != V::TYPE {
                return Err(DecodeError::InvalidType {
                    expected: V::TYPE,
                    actual: value_ty,
                });
            }

            let mut map: TdfMap<K, V> = TdfMap::with_capacity(length);
            for _ in 0..length {
                let key: K = K::deserialize(r)?;
                let value: V = V::deserialize(r)?;
                map.insert(key, value);
            }
            Ok(map)
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

    impl<K, V> TdfTyped for TdfMap<K, V> {
        const TYPE: TdfType = TdfType::Map;
    }
}

pub mod tagged_union {
    use crate::{
        codec::{TdfDeserialize, TdfSerialize, TdfTyped},
        error::{DecodeError, DecodeResult},
        reader::TdfDeserializer,
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
        const TYPE: TdfType = TdfType::TaggedUnion;
    }

    impl<'de, Value> TdfDeserialize<'de> for TaggedUnion<Value>
    where
        Value: TdfDeserialize<'de> + TdfTyped,
    {
        fn deserialize(reader: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
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
}

pub mod var_int_list {
    use crate::{
        codec::{TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped},
        error::DecodeResult,
        reader::TdfDeserializer,
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

    impl TdfDeserializeOwned for VarIntList {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let length = usize::deserialize_owned(r)?;
            let mut out = Vec::with_capacity(length);
            for _ in 0..length {
                out.push(u64::deserialize_owned(r)?);
            }
            Ok(VarIntList(out))
        }
    }

    impl TdfSerialize for VarIntList {
        fn serialize(&self, w: &mut TdfSerializer) {
            self.0.len().serialize_owned(w);
            self.0
                .iter()
                .copied()
                .for_each(|value| value.serialize_owned(w));
        }
    }

    impl TdfTyped for VarIntList {
        const TYPE: TdfType = TdfType::VarIntList;
    }
}

pub mod object_type {
    use crate::{
        codec::{TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped},
        error::DecodeResult,
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };

    /// [ObjectType] structure represents a type of object
    /// within the blaze system, this type consists of a
    /// component value and a type value
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct ObjectType {
        /// The component type for this object
        pub component: u16,
        /// The actual object type
        pub ty: u16,
    }

    impl ObjectType {
        /// Create a new [ObjectType] from its component and type
        pub fn new(component: u16, ty: u16) -> Self {
            Self { component, ty }
        }
    }

    impl TdfDeserializeOwned for ObjectType {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let component = u16::deserialize_owned(r)?;
            let ty = u16::deserialize_owned(r)?;
            Ok(Self { component, ty })
        }
    }

    impl TdfSerialize for ObjectType {
        fn serialize(&self, w: &mut TdfSerializer) {
            self.component.serialize_owned(w);
            self.ty.serialize_owned(w);
        }
    }

    impl TdfTyped for ObjectType {
        const TYPE: TdfType = TdfType::ObjectType;
    }

    #[cfg(test)]
    mod test {
        use crate::{
            codec::{TdfDeserialize, TdfSerialize},
            reader::TdfDeserializer,
            writer::TdfSerializer,
        };

        use super::ObjectType;

        /// Tests that object types can be correctly serialized
        #[test]
        fn test_encode_object_id() {
            let mut w = TdfSerializer::default();
            let object_type = ObjectType {
                component: 30722,
                ty: 1,
            };
            object_type.serialize(&mut w);

            // Check deserialize works correctly
            let mut r = TdfDeserializer::new(&w.buffer);
            let value = ObjectType::deserialize(&mut r).unwrap();
            assert_eq!(object_type, value);

            // Check serialized bytes match expected
            let expected = &[
                130, 224, 3, // Component
                1, // Type
            ];

            assert_eq!(&r.buffer, expected)
        }
    }
}

pub mod object_id {
    use super::object_type::ObjectType;
    use crate::{
        codec::{TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped},
        error::DecodeResult,
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };

    /// [ObjectId] structure represents an ID within the blaze system that
    /// is also tied to a specific [ObjectType]
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct ObjectId {
        /// The type of object the ID represents
        pub ty: ObjectType,
        /// The actual ID of the object
        pub id: u64,
    }

    impl ObjectId {
        /// Create a new [ObjectId] from its type and id
        pub fn new(ty: ObjectType, id: u64) -> Self {
            Self { ty, id }
        }

        /// Create a new [ObjectId] from its component, type, and id
        pub fn new_raw(component: u16, ty: u16, id: u64) -> Self {
            Self {
                ty: ObjectType { component, ty },
                id,
            }
        }
    }

    impl TdfDeserializeOwned for ObjectId {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let ty = ObjectType::deserialize_owned(r)?;
            let id = u64::deserialize_owned(r)?;
            Ok(Self { ty, id })
        }
    }

    impl TdfSerialize for ObjectId {
        fn serialize(&self, w: &mut TdfSerializer) {
            self.ty.serialize(w);
            self.id.serialize_owned(w);
        }
    }

    impl TdfTyped for ObjectId {
        const TYPE: TdfType = TdfType::ObjectId;
    }

    #[cfg(test)]
    mod test {
        use crate::{
            codec::{TdfDeserialize, TdfSerialize},
            reader::TdfDeserializer,
            types::object_type::ObjectType,
            writer::TdfSerializer,
        };

        use super::ObjectId;

        /// Tests that object IDs can be correctly serialized
        #[test]
        fn test_encode_object_id() {
            let mut w = TdfSerializer::default();
            let object_id = ObjectId {
                ty: ObjectType {
                    component: 30722,
                    ty: 1,
                },
                id: 0x0000012,
            };
            object_id.serialize(&mut w);

            // Check deserialize works correctly
            let mut r = TdfDeserializer::new(&w.buffer);
            let value = ObjectId::deserialize(&mut r).unwrap();
            assert_eq!(object_id.ty, value.ty);
            assert_eq!(object_id.id, value.id);

            // Check serialized bytes match expected
            let expected = &[
                130, 224, 3,  // Component
                1,  // Type
                18, // ID
            ];

            assert_eq!(&r.buffer, expected)
        }
    }
}

pub mod float {

    use crate::{
        codec::{TdfDeserializeOwned, TdfSerializeOwned, TdfTyped},
        error::DecodeResult,
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };

    impl TdfDeserializeOwned for f32 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let bytes: [u8; 4] = r.read_bytes()?;
            Ok(f32::from_be_bytes(bytes))
        }
    }

    impl TdfSerializeOwned for f32 {
        fn serialize_owned(self, w: &mut TdfSerializer) {
            let bytes: [u8; 4] = self.to_be_bytes();
            w.write_slice(&bytes);
        }
    }

    impl TdfTyped for f32 {
        const TYPE: TdfType = TdfType::Float;
    }

    #[cfg(test)]
    mod test {
        use crate::{
            codec::TdfDeserializeOwned, codec::TdfSerialize, reader::TdfDeserializer,
            writer::TdfSerializer,
        };

        /// Tests f32 encoding and decoding
        #[test]
        fn test_float_encoding() {
            let data: &[(f32, [u8; 4])] = &[
                (123.0, [66, 246, 0, 0]),
                (254.0, [67, 126, 0, 0]),
                (1.0, [63, 128, 0, 0]),
                (-3.0, [192, 64, 0, 0]),
            ];

            let mut w = TdfSerializer::default();
            for (value, expected) in data {
                // Check serialized buffer matches expected bytes
                value.serialize(&mut w);
                assert_eq!(&w.buffer, expected);

                // Check that the deserialize works correctly
                let mut r = TdfDeserializer::new(&w.buffer);
                let read_value = f32::deserialize_owned(&mut r).unwrap();
                assert_eq!(read_value, *value);

                // Reset writer for next iteration
                w.clear();
            }
        }
    }
}

pub mod u12 {
    use crate::{
        codec::{TdfDeserialize, TdfSerialize, TdfTyped},
        error::DecodeResult,
        reader::TdfDeserializer,
        tag::TdfType,
    };

    /// [U12] The type/name for this structure is not yet known
    /// but is represented using 8 bytes of data and a string value
    pub struct U12<'de> {
        /// The leading byte value (Encoding not yet known)
        pub data: [u8; 8],
        /// Associated string value
        pub value: &'de str,
    }

    impl<'de> TdfDeserialize<'de> for U12<'de> {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let data: [u8; 8] = r.read_bytes()?;
            let value: &str = <&str>::deserialize(r)?;
            Ok(Self { data, value })
        }
    }

    impl TdfSerialize for U12<'_> {
        fn serialize(&self, w: &mut crate::writer::TdfSerializer) {
            w.write_slice(&self.data);
            self.value.serialize(w);
        }
    }

    impl TdfTyped for U12<'_> {
        const TYPE: TdfType = TdfType::U12;
    }
}
