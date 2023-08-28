pub mod error;
pub mod reader;

pub mod stringify;
pub mod tag;
pub mod types;
pub mod writer;

pub mod prelude;

// Type additions for serde serialization
#[cfg(feature = "serde")]
pub mod serde;

// Re-exports
pub use error::{DecodeError, DecodeResult};
pub use reader::TdfDeserializer;
pub use stringify::{StringifyError, TdfStringifier};
pub use tag::{RawTag, Tag, Tagged, TdfType};
pub use types::serialize_vec;
pub use types::{
    Blob, GroupSlice, ObjectId, ObjectType, TaggedUnion, TdfDeserialize, TdfDeserializeOwned,
    TdfMap, TdfSerialize, TdfSerializeOwned, TdfTyped, VarIntList, U12,
};
pub use writer::TdfSerializer;

use tdf_derive::{TdfDeserialize, TdfSerialize};

use crate as tdf;

#[derive(TdfSerialize)]
pub struct Example {
    #[tdf(tag = b"ABCD")]
    pub example: u32,
}

#[derive(TdfSerialize, TdfDeserialize)]
pub struct ExampleWithLifetime<'a> {
    #[tdf(tag = b"ABCD")]
    pub example: &'a str,
}

#[derive(TdfSerialize)]
pub struct ExampleWithLifetimeGeneric<'a, T>
where
    T: TdfSerialize + TdfTyped,
{
    #[tdf(tag = b"ABCD")]
    pub example: &'a str,
    #[tdf(tag = b"TEST")]
    pub alt: T,
}

#[derive(TdfSerialize, TdfDeserialize)]
#[tdf(repr = u8)]
#[repr(u8)]
pub enum TestEnum {
    Value = 0x1,
    Test = 0x2,
}
