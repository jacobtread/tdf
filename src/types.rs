//! Types implementation for custom types used while encoding values
//! with Blaze packets

use crate::codec::TdfDeserializeOwned;

use super::{
    codec::{TdfDeserialize, TdfSerialize, TdfTyped},
    error::{DecodeError, DecodeResult},
    reader::TdfReader,
    tag::{Tag, TdfType},
    writer::TdfWriter,
};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::{slice, vec};

/// List of Var ints
#[derive(Debug, PartialEq, Eq, Default)]
pub struct VarIntList(pub Vec<u64>);

impl VarIntList {
    /// Creates a new VarIntList
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Creates a new VarIntList with no capacity
    pub fn empty() -> Self {
        Self(Vec::with_capacity(0))
    }

    /// Creates a new VarIntList with the provided
    /// capacity
    ///
    /// `capacity` The capacity for the underlying list
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    pub fn into_inner(self) -> Vec<u64> {
        self.0
    }
}

impl AsRef<[u64]> for VarIntList {
    fn as_ref(&self) -> &[u64] {
        self.0.as_ref()
    }
}

impl TdfSerialize for VarIntList {
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_usize(self.0.len());
        self.0
            .iter()
            .copied()
            .for_each(|value| output.write_u64(value));
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

/// Type that can be unset or contain a pair of key
/// values
#[derive(Debug, PartialEq, Eq)]
pub enum Union<C> {
    /// Set variant of a union value
    Set { key: u8, tag: Tag, value: C },
    /// Unset variant of a union value
    Unset,
}

impl<C> Union<C> {
    /// Creates a new union with a unset value
    pub fn unset() -> Self {
        Self::Unset
    }

    /// Creates a new set union value with the provided
    /// key tag and value
    pub fn set(key: u8, tag: &[u8], value: C) -> Self {
        Self::Set {
            key,
            tag: tag.into(),
            value,
        }
    }

    /// Checks if the union is of set type
    pub fn is_set(&self) -> bool {
        matches!(self, Self::Set { .. })
    }

    /// Checks if the union is of unset type
    pub fn is_unset(&self) -> bool {
        matches!(self, Self::Unset)
    }

    /// Unwraps the underlying value stored in this union panicing if the
    /// value is unset
    pub fn unwrap(self) -> C {
        match self {
            Self::Unset => panic!("Attempted to unwrap union with no value"),
            Self::Set { value, .. } => value,
        }
    }
}

impl<C> From<Union<C>> for Option<C> {
    fn from(value: Union<C>) -> Self {
        match value {
            Union::Set { value, .. } => Some(value),
            Union::Unset => None,
        }
    }
}

impl<C> TdfTyped for Union<C> {
    const TYPE: TdfType = TdfType::Union;
}

impl<C> TdfSerialize for Union<C>
where
    C: TdfSerialize + TdfTyped,
{
    fn serialize(&self, output: &mut TdfWriter) {
        match self {
            Union::Set { key, tag, value } => {
                output.write_byte(*key);
                output.tag(&tag.0, C::TYPE);
                value.serialize(output);
            }
            Union::Unset => output.write_byte(UNION_UNSET),
        }
    }
}

impl<'de, C> TdfDeserialize<'de> for Union<C>
where
    C: TdfDeserialize<'de> + TdfTyped,
{
    fn deserialize(reader: &mut TdfReader) -> DecodeResult<Self> {
        let key = reader.read_byte()?;
        if key == UNION_UNSET {
            return Ok(Union::Unset);
        }
        let tag = reader.read_tag()?;
        let expected_type = C::TYPE;
        let actual_type = tag.ty;
        if actual_type != expected_type {
            return Err(DecodeError::InvalidType {
                expected: expected_type,
                actual: actual_type,
            });
        }
        let value = C::deserialize(reader)?;

        Ok(Union::Set {
            key,
            tag: tag.tag,
            value,
        })
    }
}

/// Key value for unions that are unset
pub const UNION_UNSET: u8 = 0x7F;

/// Trait implemented by VarInt types
pub trait VarInt: PartialEq + Eq + Debug + TdfSerialize + for<'a> TdfDeserialize<'a> {}

/// Macro for implementing the var int trait in bulk easily
macro_rules! impl_var_int {
    ($($ty:ty),*) => { $(impl VarInt for $ty {})* };
}

impl_var_int!(u8, i8, u16, i16, u32, i32, u64, i64, usize, isize);

/// Structure for maps used in the protocol. These maps have a special
/// order that is usually required and they retain the order of insertion
/// because it uses two vecs as the underlying structure
pub struct TdfMap<K, V> {
    /// The entries stored in this map
    entries: Vec<MapEntry<K, V>>,
}

/// Entry within a TdfMap storing a key value pair
struct MapEntry<K, V> {
    /// Entry key
    key: K,
    /// Entry value
    value: V,
}

impl<K, V> Clone for MapEntry<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            key: self.key.clone(),
            value: self.value.clone(),
        }
    }
}

impl<K, V> Default for TdfMap<K, V> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

impl<K, V> Clone for TdfMap<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            entries: self.entries.clone(),
        }
    }
}

impl<K, V> Debug for TdfMap<K, V>
where
    K: Debug,
    V: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("TdfMap {")?;
        for (key, value) in self.iter() {
            writeln!(f, "  {key:?}: {value:?}")?;
        }
        f.write_str("}")
    }
}

impl<K, V> TdfMap<K, V> {
    /// Constructor implemention just uses the underlying default
    /// implemenation
    pub fn new() -> Self {
        Self::default()
    }

    /// Function for creating a new TdfMap where the underlying
    /// lists have an initial capacity
    ///
    /// `capacity` The capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            entries: Vec::with_capacity(capacity),
        }
    }

    /// Returns the length of the underlying lists
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns if the underlying lists are empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Creates a new iterator over the underlying items
    /// in the map
    pub fn iter(&self) -> MapEntryIter<'_, K, V> {
        MapEntryIter {
            inner: self.entries.iter(),
        }
    }

    /// Returns the key and value stored at the provided index
    /// will return None if there is nothing at the provided index
    pub fn index(&self, index: usize) -> Option<(&'_ K, &'_ V)> {
        let entry = self.entries.get(index)?;
        Some((&entry.key, &entry.value))
    }

    /// Inserts a new key value pair into the underlying structure.
    ///
    /// This function does NOT maintain order of the entires, use
    /// `insert_ordered` instead for maintaining the order
    ///
    /// `key`   The entry key
    /// `value` The entry value
    pub fn insert<A: Into<K>, B: Into<V>>(&mut self, key: A, value: B) {
        self.entries.push(MapEntry {
            key: key.into(),
            value: value.into(),
        });
    }

    /// Removes the last key and value returning them or None
    /// if there are no entries
    pub fn pop(&mut self) -> Option<(K, V)> {
        let entry = self.entries.pop()?;
        Some((entry.key, entry.value))
    }

    /// Removes all entries from the underlying list
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}

impl<K, V> TdfMap<K, V>
where
    K: PartialOrd + Ord,
{
    /// Orders this map based on its keys by ordering keys that
    /// are greater further up in the map
    ///
    /// This function is quite slow compared to using `insert_ordered`
    /// for all the inserted entries. This is only for if you inserted
    /// with `insert` instead
    pub fn order(&mut self) {
        let entries = &mut self.entries;
        let length = entries.len();
        // If empty or 1 item no need to order
        if length <= 1 {
            return;
        }

        entries.sort_by(|a, b| a.key.cmp(&b.key));
    }
}

impl<K, V> TdfMap<K, V>
where
    K: PartialEq + Eq,
{
    /// Extends this map with the contents of another map. Any keys that already
    /// exist in the map will be replaced with the keys from the other map
    /// and any keys not present will be inserted
    ///
    /// `other` The map to extend with
    pub fn extend(&mut self, other: TdfMap<K, V>) {
        for MapEntry { key, value } in other.entries {
            let key_index: Option<usize> = self.entries.iter().position(|value| key.eq(&value.key));
            if let Some(index) = key_index {
                self.entries[index].value = value;
            } else {
                self.insert(key, value);
            }
        }
    }

    /// Returns the index of the provided key or None if
    /// the key was not present
    ///
    /// `key` The key to find the index of
    fn index_of_key<Q: ?Sized>(&self, key: &Q) -> Option<usize>
    where
        K: Borrow<Q>,
        Q: Eq,
    {
        for index in 0..self.entries.len() {
            let entry_at = &self.entries[index];
            let key_at = entry_at.key.borrow();
            if key_at.eq(key) {
                return Some(index);
            }
        }
        None
    }

    /// Removes a value by its key and returns the entry
    /// that was present at that position.
    ///
    /// `key` The key to remove
    pub fn remove(&mut self, key: &K) -> Option<(K, V)> {
        let index = self.index_of_key(key)?;
        let entry = self.entries.remove(index);
        Some((entry.key, entry.value))
    }

    /// Returns the value stored at the provided key if
    /// its present or None.
    ///
    /// `key` The key to retrieve the value for
    #[inline]
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Eq,
    {
        let index = self.index_of_key(key)?;
        let entry = self.entries.get(index)?;
        Some(&entry.value)
    }

    /// Returns a mutable borrow to the value stored at the
    /// provided key if its present or None.
    ///
    /// `key` The key to retrieve the value for
    #[inline]
    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Eq,
    {
        let index = self.index_of_key(key)?;
        let entry = self.entries.get_mut(index)?;

        Some(&mut entry.value)
    }

    /// Takes the value stored at the provided key out of
    /// the map taking ownership this also removes the key.
    pub fn get_owned<Q: ?Sized>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Eq,
    {
        let index = self.index_of_key(key)?;
        let entry = self.entries.remove(index);
        Some(entry.value)
    }
}

/// Iterator implementation for iterating over TdfMap
pub struct MapEntryIter<'a, K, V> {
    /// The underlying map entry iterator
    inner: slice::Iter<'a, MapEntry<K, V>>,
}

impl<'a, K, V> Iterator for MapEntryIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;

        Some((&next.key, &next.value))
    }
}

/// Iterator type sitting ontop of the map entries to
/// produce unions of the key values from the vec of
/// map entries
pub struct OwnedMapEntryIter<K, V> {
    /// The underlying entry iterator
    inner: vec::IntoIter<MapEntry<K, V>>,
}

impl<K, V> Iterator for OwnedMapEntryIter<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        let MapEntry { key, value } = self.inner.next()?;
        Some((key, value))
    }
}

/// Into iterator implementation for owned map
impl<K, V> IntoIterator for TdfMap<K, V> {
    type Item = (K, V);
    type IntoIter = OwnedMapEntryIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        OwnedMapEntryIter {
            inner: self.entries.into_iter(),
        }
    }
}

/// Into iterator implementation for borrowed map
impl<'a, K, V> IntoIterator for &'a TdfMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = MapEntryIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        MapEntryIter {
            inner: self.entries.iter(),
        }
    }
}

impl<K, V, B: Into<K>, A: Into<V>> FromIterator<(B, A)> for TdfMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (B, A)>>(iter: T) -> Self {
        let entries: Vec<MapEntry<K, V>> = iter
            .into_iter()
            .map(|(key, value)| MapEntry {
                key: key.into(),
                value: value.into(),
            })
            .collect();
        Self { entries }
    }
}

impl<K, V> TdfSerialize for TdfMap<K, V>
where
    K: TdfSerialize + TdfTyped,
    V: TdfSerialize + TdfTyped,
{
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_map_header(K::TYPE, V::TYPE, self.len());

        for MapEntry { key, value } in &self.entries {
            key.serialize(output);
            value.serialize(output);
        }
    }
}

impl<'de, K, V> TdfDeserialize<'de> for TdfMap<K, V>
where
    K: TdfDeserialize<'de> + TdfTyped,
    V: TdfDeserialize<'de> + TdfTyped,
{
    #[inline]
    fn deserialize(reader: &mut TdfReader) -> DecodeResult<Self> {
        reader.read_map()
    }
}

impl<K, V> TdfTyped for TdfMap<K, V> {
    const TYPE: TdfType = TdfType::Map;
}

/// Implementation for converting a HashMap to a TdfMap by taking
/// all its keys and values and building lists for the TdfMap
impl<K, V> From<HashMap<K, V>> for TdfMap<K, V> {
    fn from(map: HashMap<K, V>) -> Self {
        let mut entries: Vec<MapEntry<K, V>> = Vec::with_capacity(map.len());

        for (key, value) in map.into_iter() {
            entries.push(MapEntry { key, value });
        }

        Self { entries }
    }
}

impl TdfSerialize for f32 {
    #[inline]
    fn serialize(&self, output: &mut TdfWriter) {
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

impl TdfSerialize for bool {
    #[inline]
    fn serialize(&self, output: &mut TdfWriter) {
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

/// Macro for forwarding the encode and decodes of a type to
/// another types encoder and decoder
///
/// `$a` The type to forward
/// `$b` The type to forward to
macro_rules! forward_codec {
    ($a:ident, $b:ident) => {
        impl<'de> TdfDeserialize<'de> for $a {
            #[inline]
            fn deserialize(
                reader: &mut $crate::reader::TdfReader,
            ) -> $crate::error::DecodeResult<Self> {
                Ok($b::deserialize(reader)? as $a)
            }
        }

        impl TdfSerialize for $a {
            #[inline]
            fn serialize(&self, output: &mut TdfWriter) {
                $b::serialize(&(*self as $b), output)
            }
        }

        impl $crate::codec::TdfTyped for $a {
            const TYPE: TdfType = $b::TYPE;
        }
    };
}

// Encoding for u8 values

impl TdfSerialize for u8 {
    #[inline]
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_u8(*self)
    }
}

impl TdfDeserializeOwned for u8 {
    #[inline]
    fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
        reader.read_u8()
    }
}

impl TdfSerialize for u16 {
    #[inline]
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_u16(*self)
    }
}

impl TdfDeserializeOwned for u16 {
    #[inline]
    fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
        reader.read_u16()
    }
}

impl TdfSerialize for u32 {
    #[inline]
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_u32(*self)
    }
}

impl TdfDeserializeOwned for u32 {
    #[inline]
    fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
        reader.read_u32()
    }
}

impl TdfSerialize for u64 {
    #[inline]
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_u64(*self)
    }
}

impl TdfDeserializeOwned for u64 {
    #[inline]
    fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
        reader.read_u64()
    }
}

impl TdfSerialize for usize {
    #[inline]
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_usize(*self)
    }
}

impl TdfDeserializeOwned for usize {
    #[inline]
    fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
        reader.read_usize()
    }
}

impl TdfTyped for u8 {
    const TYPE: TdfType = TdfType::VarInt;
}
impl TdfTyped for u16 {
    const TYPE: TdfType = TdfType::VarInt;
}
impl TdfTyped for u32 {
    const TYPE: TdfType = TdfType::VarInt;
}
impl TdfTyped for u64 {
    const TYPE: TdfType = TdfType::VarInt;
}
impl TdfTyped for usize {
    const TYPE: TdfType = TdfType::VarInt;
}

forward_codec!(i8, u8);
forward_codec!(i16, u16);
forward_codec!(i32, u32);
forward_codec!(i64, u64);
forward_codec!(isize, usize);

impl TdfSerialize for &str {
    #[inline]
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_str(self)
    }
}

impl TdfTyped for &str {
    const TYPE: TdfType = TdfType::String;
}

impl TdfSerialize for String {
    #[inline]
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_str(self);
    }
}

impl TdfDeserializeOwned for String {
    #[inline]
    fn deserialize_owned(reader: &mut TdfReader) -> DecodeResult<Self> {
        reader.read_string()
    }
}

impl TdfTyped for String {
    const TYPE: TdfType = TdfType::String;
}

/// Blob structure wrapping a vec of bytes. This implementation is
/// to differenciate between a list of VarInts and a Blob of straight
/// bytes
#[derive(Default, Debug, Clone)]
pub struct Blob<'de>(pub &'de [u8]);

impl TdfSerialize for Blob<'_> {
    fn serialize(&self, output: &mut TdfWriter) {
        output.write_usize(self.0.len());
        output.write_slice(&self.0);
    }
}

impl<'de> TdfDeserialize<'de> for Blob<'de> {
    fn deserialize(reader: &mut TdfReader) -> DecodeResult<Self> {
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
    fn serialize(&self, writer: &mut TdfWriter) {
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
    fn serialize(&self, writer: &mut TdfWriter) {
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
    fn deserialize(reader: &mut TdfReader) -> DecodeResult<Self> {
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
    fn serialize(&self, w: &mut TdfWriter) {
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
    fn serialize(&self, w: &mut TdfWriter) {
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

    /// Tests ordering a map
    #[test]
    fn test_map_ord() {
        let mut map = TdfMap::<String, String>::new();

        // Expected order:
        // TdfMap {
        //   "key1": "ABC"
        //   "key11": "ABC"
        //   "key17": "ABC"
        //   "key2": "ABC"
        //   "key24": "ABC"
        //   "key4": "ABC"
        // }

        let i = Instant::now();
        // Input order
        map.insert("key1", "ABC");
        map.insert("key2", "ABC");
        map.insert("key4", "ABC");
        map.insert("key24", "ABC");
        map.insert("key11", "ABC");
        map.insert("key17", "ABC");

        map.order();
        let el = i.elapsed();
        println!("Full order time: {:?}", el);

        assert_eq!(map.entries[0].key, "key1");
        assert_eq!(map.entries[1].key, "key11");
        assert_eq!(map.entries[2].key, "key17");
        assert_eq!(map.entries[3].key, "key2");
        assert_eq!(map.entries[4].key, "key24");
        assert_eq!(map.entries[5].key, "key4");
    }

    /// Tests extending an existing map
    #[test]
    fn test_map_extend() {
        let mut mapa = TdfMap::<String, String>::new();

        mapa.insert("key1", "ABC");
        mapa.insert("key2", "ABC");
        mapa.insert("key4", "ABC");
        mapa.insert("key24", "ABC");
        mapa.insert("key11", "ABC");
        mapa.insert("key17", "ABC");

        let mut mapb = TdfMap::<String, String>::new();

        mapb.insert("key1", "DDD");
        mapb.insert("key2", "ABC");
        mapb.insert("key4", "DDD");
        mapb.insert("abc", "ABC");

        mapa.extend(mapb);
        println!("{mapa:?}")
    }

    /// Tests inserting into a map
    #[test]
    fn test_map_insert() {
        let mut map = TdfMap::<String, String>::new();
        map.insert("Test", "Abc");

        let value = map.get("Test");

        assert_eq!(value.unwrap(), "Abc");

        println!("{value:?}")
    }
}
