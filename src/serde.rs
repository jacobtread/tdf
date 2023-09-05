//! This module contains the serde Serialize implementations for the
//! structures created by Pocket Relay

use crate::{ObjectId, ObjectType, U12};

use super::types::{Blob, TdfMap, VarIntList};
use serde::ser::{SerializeMap, SerializeStruct};
use serde::Serialize;

impl<K, V> Serialize for TdfMap<K, V>
where
    K: Serialize + Ord,
    V: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.len()))?;

        for (k, v) in self.iter() {
            map.serialize_entry(k, v)?;
        }
        map.end()
    }
}

impl Serialize for VarIntList {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl Serialize for Blob<'_> {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl Serialize for ObjectType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut stru = serializer.serialize_struct("ObjectType", 2)?;
        stru.serialize_field("component", &self.component)?;
        stru.serialize_field("ty", &self.ty)?;
        stru.end()
    }
}

impl Serialize for ObjectId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut stru = serializer.serialize_struct("ObjectId", 2)?;
        stru.serialize_field("ty", &self.ty)?;
        stru.serialize_field("id", &self.id)?;
        stru.end()
    }
}

impl Serialize for U12 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut stru = serializer.serialize_struct("U12", 2)?;
        stru.serialize_field("data", &self.data)?;
        stru.serialize_field("value", &self.value)?;
        stru.end()
    }
}
