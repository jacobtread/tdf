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

use tdf_derive::{TdfDeserialize, TdfSerialize, TdfTyped};

use crate as tdf;

#[derive(TdfSerialize, TdfTyped)]
#[tdf(group)]
pub struct Example {
    #[tdf(tag = b"ABCD")]
    pub example: u32,
}

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
#[tdf(group)]
pub struct ExampleWithLifetime<'a> {
    #[tdf(tag = b"ABCD")]
    pub example: &'a str,
}

#[derive(TdfSerialize, TdfTyped)]
#[tdf(group)]
pub struct ExampleWithLifetimeGeneric<'a, T>
where
    T: TdfSerialize + TdfTyped,
{
    #[tdf(tag = b"ABCD")]
    pub example: &'a str,
    #[tdf(tag = b"TEST")]
    pub alt: T,
}

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
#[repr(u8)]
pub enum TestEnum {
    Value = 0x1,
    Test = 0x2,
}

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
#[repr(u16)]
pub enum TestEnumFallback {
    Value = 0x1,
    Test = 0x2,
    #[tdf(default)]
    Unknown = 0x3,
}

#[derive(TdfSerialize, TdfDeserialize, TdfTyped)]
#[tdf(tagged)]
pub enum TestTaggedEnum {
    #[tdf(key = 0x1, tag = b"TEST", prefix_two)]
    Value {
        #[tdf(tag = b"VALU", skip)]
        key: String,
    },
    #[tdf(key = 0x2, tag = b"TEST")]
    Value2(String),
    #[tdf(default)]
    Test,
    #[tdf(unset)]
    Test2,
}
