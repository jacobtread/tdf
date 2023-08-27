//! Traits for implementing encoding ([`Encodable`]) and decoding ([`Decodable`])
//! for different types and [`ValueType`] trait for specifying the Tdf type of a type

use crate::writer::TdfSerializer;

use super::{error::DecodeResult, reader::TdfDeserializer, tag::TdfType};

pub trait TdfDeserialize<'de>: Sized {
    fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self>;
}

pub trait TdfDeserializeOwned: Sized {
    fn deserialize_owned(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self>;
}

impl<T> TdfDeserialize<'_> for T
where
    T: TdfDeserializeOwned,
{
    #[inline]
    fn deserialize(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
        Self::deserialize_owned(r)
    }
}

pub trait TdfSerialize: Sized {
    fn serialize<S: TdfSerializer>(&self, w: &mut S);

    fn serialize_vec(&self) -> Vec<u8> {
        let mut output = Vec::new();
        self.serialize(&mut output);
        output
    }
}

pub trait TdfSerializeOwned: Sized {
    fn serialize_owned<S: TdfSerializer>(self, w: &mut S);
}

impl<T> TdfSerialize for T
where
    T: TdfSerializeOwned + Copy,
{
    #[inline]
    fn serialize<S: TdfSerializer>(&self, w: &mut S) {
        (*self).serialize_owned(w)
    }
}

/// Associated trait for types that can be encoded/decoded
/// as a specific [TdfType] rather than just a generic
/// encoding and decoding
pub trait TdfTyped {
    /// The [TdfType] this value is represented as
    const TYPE: TdfType;
}
