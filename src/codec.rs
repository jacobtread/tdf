//! Traits for implementing encoding ([`Encodable`]) and decoding ([`Decodable`])
//! for different types and [`ValueType`] trait for specifying the Tdf type of a type

use super::{error::DecodeResult, reader::TdfReader, tag::TdfType, writer::TdfSerializer};

pub trait TdfDeserialize<'de>: Sized {
    fn deserialize(r: &mut TdfReader<'de>) -> DecodeResult<Self>;
}

pub trait TdfDeserializeOwned: Sized {
    fn deserialize_owned(r: &mut TdfReader<'_>) -> DecodeResult<Self>;
}

impl<T> TdfDeserialize<'_> for T
where
    T: TdfDeserializeOwned,
{
    #[inline]
    fn deserialize(r: &mut TdfReader<'_>) -> DecodeResult<Self> {
        Self::deserialize_owned(r)
    }
}

pub trait TdfSerialize: Sized {
    fn serialize(&self, w: &mut TdfSerializer);

    fn serialize_bytes(&self) -> Vec<u8> {
        let mut output = TdfSerializer::default();
        self.serialize(&mut output);
        output.into()
    }
}

/// Associated trait for types that can be encoded/decoded
/// as a specific [TdfType] rather than just a generic
/// encoding and decoding
pub trait TdfTyped {
    /// The [TdfType] this value is represented as
    const TYPE: TdfType;
}
