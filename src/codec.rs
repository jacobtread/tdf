//! Traits for implementing encoding ([`Encodable`]) and decoding ([`Decodable`])
//! for different types and [`ValueType`] trait for specifying the Tdf type of a type

use super::{error::DecodeResult, reader::TdfReader, tag::TdfType, writer::TdfWriter};

pub trait Decodable: Sized {
    fn decode(r: &mut TdfReader) -> DecodeResult<Self>;
}

pub trait Encodable: Sized {
    fn encode(&self, w: &mut TdfWriter);

    fn encode_bytes(&self) -> Vec<u8> {
        let mut output = TdfWriter::default();
        self.encode(&mut output);
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
